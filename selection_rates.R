# Selection rates for the 2025 Draft ADP

# Author: Geoff Mayhew
# Start Date: 2024-Aug-07

#======================================================================================================================#
# Preparation ----
#======================================================================================================================#

adp_year <- 2025

#===================#
## Load Packages ----
#===================#

library(data.table)         # Data wrangling
library(ggplot2)            # Plotting
library(sf)                 # Spatial analyses
library(dplyr)              # For piping and handling sf objects
library(FMAtools)           # For connectivity to Analytical Services Program's Shared Google Drive
library(readxl)             # For read_xlsx
library(odbc)               # For database connectivity
# library(grid)               # For unit.pmax  to get widths of grobs so that plots have matching dimensions
# library(gridExtra)          # For arrangeGrob to combine plots
# library(flextable)          # For print-ready tables
# library(officer)            # For additional flextable formatting options such as fp_border

#=====================================#
## Connect to Shared Google Drive ----
#=====================================#

#' TODO upload/download using the Gdrive when the source data and outputs are available.

#===============#
## Load data ----
#===============#

#' TODO *loading the most recent version of Valhalla used for the 2023 Annual Report.* Use the outputs of get_data.R
#' when it is prepared. Download from Projects/ADP/ folder
gdrive_download("source_data/2_AR_data.Rdata", gdrive_set_dribble("Projects/AnnRpt-Deployment-Chapter/"))
AR_data_objects <- load("source_data/2_AR_data.Rdata")
# Remove everything except for work.data
rm(list = setdiff(ls(), c("work.data")))

#' TODO *Do this in get_data*
# Get count of GOA-only EM EFP Vessels for the cost_params. 
trawl_em_goa_v_count <- length(na.omit(unlist(unname(
  setDT(readxl::read_xlsx("source_data/2024 EM EFP Vessel List_NMFS.xlsx", col_names = F))[-c(1:3), 7]))))

# Load the ADFG statistical area shapefile.
stat_area_sf <- st_read(
  dsn = "source_data/ADFG_Stat_Area_shapefile/PVG_Statewide_2001_Present_GCS_WGS1984.shp", quiet = T) %>%
  select(STAT_AREA) %>%
  st_transform(crs = 3467)

# Load the Alaska map sf objects
load("source_data/ak_shp.rdata")      # shp_land, shp_nmfs, and shp_centroids added to global

#====================#
## Load Functions ----
#====================#

source("common_functions/open_channel.R")
source("common_functions/allocation_functions.R")
#' TODO *model_trip_duration should be used by get_data.R, but until it is ready, using it here.*
source("common_functions/model_trip_duration.R")

#===============#
## Data Prep ----
#===============#

# Wrangle the Valhalla data set for spatiotemporal analyses
pc_effort_st <- spatiotemp_data_prep(work.data) 

#' Grab the most recent 1-year of data (trimming off 2 weeks from the last trip)
valhalla_date <- as.Date(max(work.data$TRIP_TARGET_DATE)) - 14
pc_prev_year_trips <- pc_effort_st[
][, .(MIN_TRIP_TARGET_DATE = min(TRIP_TARGET_DATE)), keyby = .(TRIP_ID)
][between(MIN_TRIP_TARGET_DATE, valhalla_date - 365, valhalla_date), TRIP_ID]
pc_effort_st <- pc_effort_st[TRIP_ID %in% pc_prev_year_trips]
range(pc_effort_st$TRIP_TARGET_DATE)

#' TODO *Assigning strata to 2024 definitions. Do this in get_data.R for 2025.* 
pc_effort_st[, STRATA := fcase(
  STRATA %in% c("EM_HAL", "EM_POT"), "EM_FIXED",
  STRATA %in% c("OB_HAL", "OB_POT"), "OB_FIXED",
  STRATA == "EM_TRW_EFP", "EM_TRW",
  STRATA == "OB_TRW", "OB_TRW",
  STRATA == "ZERO", "ZERO"
)][, STRATA := ifelse(STRATA != "ZERO", paste0(STRATA, "-", BSAI_GOA), STRATA)]

#' TODO *Assign DAYS column. Do this in get_data.R!*
#' Get actual days observed, otherwise apply model.
channel <- open_channel()
#' Match observed ODDS and Valhalla records. This creates `mod_dat` object
td_mod <- model_trip_duration(work.data, use_mod = "DAYS ~ RAW + ADP * AGENCY_GEAR_CODE", channel = channel)
# Occasionally we have multiple ODDS records assigned to the same Valhalla trip. In such cases, sum the sea days.
actual_ob_days <- unique(mod_dat[, .(TRIP_ID, ODDS_SEQ, DAYS)])[, .(DAYS = sum(DAYS)), keyby = .(TRIP_ID)]
# Merge in actual days observed
pc_effort_st[, DAYS := actual_ob_days[pc_effort_st, DAYS, on = c(TRIP_ID = "wd_TRIP_ID")]]
# For other observer strata trips that were not observed, use the modeled estimates
ob_trips_predict_days <- unique(pc_effort_st[STRATA %like% "OB" & is.na(DAYS), .(
  ADP = as.factor(ADP), AGENCY_GEAR_CODE,
  RAW = as.numeric(max(LANDING_DATE, TRIP_TARGET_DATE) - min(TRIP_TARGET_DATE, LANDING_DATE), units = "days")
), keyby = .(TRIP_ID)])
# Apply the model, rounding to the nearest half day
ob_trips_predict_days[, DAYS := round(predict(td_mod$TD_MOD, newdata = ob_trips_predict_days)/0.5) * 0.5]
# For trips with multiple gear types, take the average 
ob_trips_predict_days <- ob_trips_predict_days[, .(DAYS = mean(DAYS)), keyby = .(TRIP_ID)]
# Merge in predictions
pc_effort_st[, MOD_DAYS := ob_trips_predict_days[pc_effort_st, DAYS, on = .(TRIP_ID)]]
pc_effort_st[, DAYS := fcase(!is.na(DAYS), DAYS, !is.na(MOD_DAYS), MOD_DAYS)][, MOD_DAYS := NULL]
# For non-observer strata, simply use the end - start + 1 method
pc_effort_st[
  is.na(DAYS), DAYS := as.numeric(
    1 + max(TRIP_TARGET_DATE, LANDING_DATE) - min(TRIP_TARGET_DATE, LANDING_DATE), units = "days"
  ), by = .(TRIP_ID
)]
if(nrow(pc_effort_st[is.na(DAYS)])) stop("Some records are still missing DAYS")

# Re-assign the ADP Year
pc_effort_st[, ADP := adp_year]
setkey(pc_effort_st, STRATA)

#====================#
## Box Parameters ----
#====================#

box_params <- list(
  space = c(2e5, 2e5),
  time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = "STRATA", ps_cols = "GEAR",
  stat_area_sf = stat_area_sf
)

#=======================#
## Monitoring  Costs ----
#=======================#

#' TODO *These will need to be updated!*

# Derived from: https://docs.google.com/spreadsheets/d/1kcdLjq2Ck4XJBYP0EhrQpuknRgtQFt01LN3xUaCg7cI/edit?usp=sharing
# Monitoring cost models are applyed by ob_cost(), emfg_cost(), and emtrw_cost() functions
# Set all parameters involving monitoring costs

cost_params <- list(
  
  OB = list(
    day_rate_intercept          = 1870.4466666667,    # To calculate the sea day cost
    day_rate_slope              =   -0.2263733333,    # To calculate the sea day cost
    travel_day_rate             =  423.7867560407     # Expected cost of travel per day
  ),
  
  #' TODO Cost per vessel should replace equipment install costs with equipment replacement costs? 
  #' Or will Murkowski funds cover this? If so, cost per vessel will largely be from maintenance.
  EMFG = list(
    emfg_v                      = 177,   #' Size of fixed-gear EM vessel pool. 177 in 2024. [UPDATE FOR 2025]
    cost_per_vessel             = 5679.9002264045,
    cost_per_review_day         =  150.3237858616
  ),
  
  # TODO EMTRW will be a carve-off and needs entirely new estimates
  EMTRW = list(
    emtrw_goa_v                 =   trawl_em_goa_v_count,  # Number of EM_TRW vessels that fish exclusively in the GOA. 
    trip_to_plant_factor        =    3.8059361492,    # Used to predict plant days from trips
    amortized_equipment_per_VY  = 4100.7124800000,    # Per (Vessel x Year) amortized EM equipment install costs for GOA-only vessels
    equipment_upkeep_per_VY     = 4746.0398955882,    # Per (Vessel x Year) EM equipment maintenance cost for GOA-only vessels
    review_day_rate             =   27.9918948996,    # Per sea day cost for EM compliance review
    plant_day_rate              =  908.2225000000     # Per plant day cost for shoreside monitoring by observers
  )
)


#' *Use this with ob_cost_new. This has updated values f*
cost_params_new <- list(
  
  OB = list(
    sea_day_min = 1200,
    sea_day_rate_gua = 1695.58,
    sea_day_rate_opt = 877.73,
    travel_day_rate = 435.5971
  ),
  
  #' TODO Cost per vessel should replace equipment install costs with equipment replacement costs? 
  #' Or will Murkowski funds cover this? If so, cost per vessel will largely be from maintenance.
  EMFG = list(
    emfg_v                      = 177,   #' Size of fixed-gear EM vessel pool. 177 in 2024. [UPDATE FOR 2025]
    cost_per_vessel             = 5679.9002264045,
    cost_per_review_day         =  150.3237858616
  ),
  
  # TODO EMTRW will be a carve-off and needs entirely new estimates
  EMTRW = list(
    emtrw_goa_v                 =   trawl_em_goa_v_count,  # Number of EM_TRW vessels that fish exclusively in the GOA. 
    trip_to_plant_factor        =    3.8059361492,    # Used to predict plant days from trips
    amortized_equipment_per_VY  = 4100.7124800000,    # Per (Vessel x Year) amortized EM equipment install costs for GOA-only vessels
    equipment_upkeep_per_VY     = 4746.0398955882,    # Per (Vessel x Year) EM equipment maintenance cost for GOA-only vessels
    review_day_rate             =   27.9918948996,    # Per sea day cost for EM compliance review
    plant_day_rate              =  908.2225000000     # Per plant day cost for shoreside monitoring by observers
  )
)

#' [Set the budget(s) to evaluate]
#' budget_lst <- list(4.8e6 + 1.019e6)        *UPDATE THIS* [Budget of $5,819,000 used in the 2024 ADP]
budget_lst <- list(4.0e6)   #' *Bleak estimate given fee revenues minus some trawl EM costs and no additional funds*

#========================#
## Trawl EM Carve-off ----
#========================#

#' TODO *Estimate the total costs of trawl EM for 2025, then subtract from the estimated budget*

#====================================#
## Determine Trip Selection Rates ----
#====================================#

#=========================#
### Bootstrap sampling ----
#=========================#

#' TODO  Use the outputs of effort_prediction.R, specifically 'effort_strata[ADP == 2024]', to predict the total number of
#' trips to sample for each stratum

#' TODO *For now just use number in dataset as a placeholder*
sample_N <- pc_effort_st[, .(N = uniqueN(TRIP_ID)), keyby = .(STRATA)]
boot_lst <- bootstrap_allo(pc_effort_st, sample_N, box_params, cost_params_new, budget_lst, bootstrap_iter = 1)

#' [Using a $4.5M budget]
# boot_lst <- bootstrap_allo(pc_effort_st, sample_N, box_params, cost_params_new, list(4.5e6), bootstrap_iter = 1)

#' [Using $5.0M budget]
# boot_lst <- bootstrap_allo(pc_effort_st, sample_N, box_params, cost_params_new, list(5.0e6), bootstrap_iter = 1)

#' [Using $5.82M budget (2024 ADP)]
# boot_lst <- bootstrap_allo(pc_effort_st, sample_N, box_params, cost_params_new, list(5.82e6), bootstrap_iter = 1)

#' TODO Save the outputs of the bootstrapping and allocation
if(F) save(boot_lst, file = "results/swor_boot_lst.rdata")

# Extract the results from each iteration
boot_dt <- rbindlist(lapply(boot_lst, "[[", "rates"), idcol = "BOOT_ITER")

ggplot(boot_dt, aes(x = STRATA, y = SAMPLE_RATE)) + geom_violin(draw_quantiles = 0.5) + 
  facet_wrap(.~ STRATA, scales = "free") + stat_summary(geom = "point", fun = mean)

ggplot(boot_dt, aes(x = STRATA, y = SAMPLE_RATE)) + geom_violin(draw_quantiles = 0.5) + 
  stat_summary(geom = "point", fun = mean) + labs(x = "Stratum", y = "Sample Rate") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + geom_hline(yintercept = 0)

# Calculate the rates to use as the mean across iterations. Also calculate average proximity (PROX) and cv_scaling metrics
rates_adp <- boot_dt[, lapply(.SD, mean), .SDcols = c("SAMPLE_RATE", "n", "PROX", "CV_SCALING", "INDEX"), keyby = .(ADP, STRATA, STRATA_N)]


# Monitoring Costs ----

# How much did we allocate to at-sea observers and fixed-gear EM?
mtd <- unique(pc_effort_st[, .(ADP, STRATA, TRIP_ID, DAYS)])[, .(TRP_DUR = mean(DAYS)), keyby = .(ADP, STRATA)]
rates_mtd_adp <- mtd[rates_adp, on = .(ADP, STRATA)]
rates_mtd_adp[, MON_RATE := SAMPLE_RATE]

rates_adp
ob_cost(rates_mtd_adp, cost_params)
emfg_cost(rates_mtd_adp, cost_params)

#=======================================================================================================================#

#' [$4.0M Budget with new values and ob_cost_new()]
 
#  ADP        STRATA STRATA_N SAMPLE_RATE        n      PROX CV_SCALING  INDEX
# 2025 EM_FIXED-BSAI       47      0.5703  26.8041 0.8758692 0.12661410 0.765
# 2025  EM_FIXED-GOA      834      0.1388 115.7592 0.8372885 0.08625301 0.765
# 2025 OB_FIXED-BSAI      329      0.1984  65.2736 0.8603124 0.11081794 0.765
# 2025  OB_FIXED-GOA     2036      0.0654 133.1544 0.8348581 0.08377899 0.765
# 2025   OB_TRW-BSAI      123      0.1846  22.7058 0.9438439 0.18950338 0.765
# 2025    OB_TRW-GOA      534      0.0663  35.4042 0.9133382 0.16239636 0.765

# OB_TOTAL   OB_CPD  OB_DAYS
#  2829634 1968.897 1437.167

# EMFG_TOTAL EMFG_CPD EMFG_DAYS EMFG_BASE
#    1130945 1353.542  835.5445   1005342

#' [$4.0M Budget with old values] 

#  ADP        STRATA STRATA_N SAMPLE_RATE        n      PROX CV_SCALING  INDEX
# 2025 EM_FIXED-BSAI       47      0.5758  27.0626 0.8783970 0.12519892 0.7684
# 2025  EM_FIXED-GOA      834      0.1407 117.3438 0.8402983 0.08557410 0.7684
# 2025 OB_FIXED-BSAI      329      0.2016  66.3264 0.8631470 0.10971526 0.7684
# 2025  OB_FIXED-GOA     2036      0.0664 135.1904 0.8381565 0.08310124 0.7684
# 2025   OB_TRW-BSAI      123      0.1882  23.1486 0.9454848 0.18726739 0.7684
# 2025    OB_TRW-GOA      534      0.0675  36.0450 0.9155853 0.16084290 0.7684

# OB_TOTAL   OB_CPD  OB_DAYS
#  2867444 1963.673 1460.245

# EMFG_TOTAL EMFG_CPD EMFG_DAYS EMFG_BASE
#    1132539 1338.462  846.1493   1005342

#=======================================================================================================================#

#' [$4.5M Budget with new values]

#   ADP        STRATA STRATA_N SAMPLE_RATE        n      PROX CV_SCALING  INDEX
# 2025 EM_FIXED-BSAI       47      0.6478  30.4466 0.9084280 0.10755368 0.8107
# 2025  EM_FIXED-GOA      834      0.1694 141.2796 0.8779806 0.07667543 0.8107
# 2025 OB_FIXED-BSAI      329      0.2487  81.8223 0.8966051 0.09582324 0.8107
# 2025  OB_FIXED-GOA     2036      0.0806 164.1016 0.8764112 0.07485070 0.8107
# 2025   OB_TRW-BSAI      123      0.2437  29.9751 0.9637543 0.15884251 0.8107
# 2025    OB_TRW-GOA      534      0.0871  46.5114 0.9428764 0.14009799 0.8107

# OB_TOTAL   OB_CPD  OB_DAYS
#  3394148 1887.071 1798.633

# EMFG_TOTAL EMFG_CPD EMFG_DAYS EMFG_BASE
#    1156018  1153.32  1002.339   1005342

#' [$4.5M Budget (old)]
#  ADP        STRATA STRATA_N SAMPLE_RATE        n      PROX CV_SCALING  INDEX
# 2025 EM_FIXED-BSAI       47      0.6417  30.1599 0.9060866 0.10899547 0.8073
# 2025  EM_FIXED-GOA      834      0.1668 139.1112 0.8750857 0.07739156 0.8073
# 2025 OB_FIXED-BSAI      329      0.2443  80.3747 0.8940255 0.09696501 0.8073
# 2025  OB_FIXED-GOA     2036      0.0792 161.2512 0.8732474 0.07556683 0.8073
# 2025   OB_TRW-BSAI      123      0.2383  29.3109 0.9624164 0.16120459 0.8073
# 2025    OB_TRW-GOA      534      0.0851  45.4434 0.9407278 0.14188987 0.8073

# OB_TOTAL  OB_CPD  OB_DAYS
#  3345267 1894.51 1765.769

# EMFG_TOTAL EMFG_CPD EMFG_DAYS EMFG_BASE
#    1153914 1167.525  988.3419   1005342

#=======================================================================================================================#

#' [$5.0M Budget] *new*

#  ADP        STRATA STRATA_N SAMPLE_RATE        n      PROX CV_SCALING  INDEX
# 2025 EM_FIXED-BSAI       47      0.7078  33.2666 0.9297243 0.09372078 0.8426
# 2025  EM_FIXED-GOA      834      0.1990 165.9660 0.9054919 0.06947150 0.8426
# 2025 OB_FIXED-BSAI      329      0.2985  98.2065 0.9203622 0.08451693 0.8426
# 2025  OB_FIXED-GOA     2036      0.0956 194.6416 0.9042377 0.06816516 0.8426
# 2025   OB_TRW-BSAI      123      0.3056  37.5888 0.9751549 0.13591758 0.8426
# 2025    OB_TRW-GOA      534      0.1098  58.6332 0.9610388 0.12321739 0.8426

# OB_TOTAL   OB_CPD  OB_DAYS
#  3901142 1804.963 2161.341

# EMFG_TOTAL EMFG_CPD EMFG_DAYS EMFG_BASE
#    1179459 1018.286  1158.279   1005342

#' [$5.0M Budget] (old)

#  ADP        STRATA STRATA_N SAMPLE_RATE        n      PROX CV_SCALING  INDEX
# 2025 EM_FIXED-BSAI       47      0.6990  32.8530 0.9267865 0.09571847 0.8381
# 2025  EM_FIXED-GOA      834      0.1943 162.0462 0.9017090 0.07051269 0.8381
# 2025 OB_FIXED-BSAI      329      0.2905  95.5745 0.9171144 0.08615990 0.8381
# 2025  OB_FIXED-GOA     2036      0.0932 189.7552 0.9004291 0.06912878 0.8381
# 2025   OB_TRW-BSAI      123      0.2954  36.3342 0.9736663 0.13925587 0.8381
# 2025    OB_TRW-GOA      534      0.1060  56.6040 0.9586355 0.12567393 0.8381

# OB_TOTAL   OB_CPD  OB_DAYS
#  3823328 1818.218 2102.788

# EMFG_TOTAL EMFG_CPD EMFG_DAYS EMFG_BASE
#    1175777  1037.04  1133.781   1005342



#=======================================================================================================================#

#' [$5.82M Budget] *new* 2024 Final ADP Budget

#  ADP        STRATA STRATA_N SAMPLE_RATE        n      PROX CV_SCALING  INDEX
# 2025 EM_FIXED-BSAI       47      0.7796  36.6412 0.9515896 0.07755699 0.8778
# 2025  EM_FIXED-GOA      834      0.2457 204.9138 0.9345233 0.06067174 0.8778
# 2025 OB_FIXED-BSAI      329      0.3772 124.0988 0.9447299 0.07084195 0.8778
# 2025  OB_FIXED-GOA     2036      0.1203 244.9308 0.9337055 0.05993018 0.8778
# 2025   OB_TRW-BSAI      123      0.4062  49.9626 0.9852073 0.10901784 0.8778
# 2025    OB_TRW-GOA      534      0.1515  80.9010 0.9779782 0.10241156 0.8778

# OB_TOTAL   OB_CPD  OB_DAYS
#  4605379 1669.938 2757.815

# EMFG_TOTAL EMFG_CPD EMFG_DAYS EMFG_BASE
#    1215202 870.4554  1396.054   1005342

#' [$5.82M Budget] old() 2024 Final ADP Budget

#  ADP        STRATA STRATA_N SAMPLE_RATE        n      PROX CV_SCALING  INDEX
# 2025 EM_FIXED-BSAI       47      0.7794  36.6318 0.9515335 0.07760213 0.8777
# 2025  EM_FIXED-GOA      834      0.2455 204.7470 0.9344255 0.06070449 0.8777
# 2025 OB_FIXED-BSAI      329      0.3769 124.0001 0.9446569 0.07088721 0.8777
# 2025  OB_FIXED-GOA     2036      0.1203 244.9308 0.9337055 0.05993018 0.8777
# 2025   OB_TRW-BSAI      123      0.4058  49.9134 0.9851790 0.10910829 0.8777
# 2025    OB_TRW-GOA      534      0.1513  80.7942 0.9779250 0.10249130 0.8777

# OB_TOTAL   OB_CPD  OB_DAYS
#  4604049 1670.225 2756.544

# EMFG_TOTAL EMFG_CPD EMFG_DAYS EMFG_BASE
#    1215055  870.961  1395.074   1005342

