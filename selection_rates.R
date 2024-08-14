# Selection rates for the 2025 Draft ADP

# Author: Geoff Mayhew
# Start Date: 2024-Aug-07

#======================================================================================================================#
# Preparation ----
#======================================================================================================================#

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
pc_effort_st[, ADP := 2025]
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
# Monitoring cost models are applied by ob_cost(), emfg_cost(), and emtrw_cost() functions
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
boot_lst <- bootstrap_allo(pc_effort_st, sample_N, box_params, cost_params, budget_lst, bootstrap_iter = 1)

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
rates_adp

#' Ooof... With a $4M budget, only 6.5% in GOA , 18-20% in BSAI for observers. Although, the cost function for observer
#' costs is hopefully currently overestimating costs at this scale, but we'll see...