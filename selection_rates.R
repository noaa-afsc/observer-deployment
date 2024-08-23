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

#===============#
## Load data ----
#===============#

#' Load the outputs of `get_data.R`
gdrive_download(
  local_path = "source_data/2025_Draft_ADP_data.rdata",
  gdrive_dribble = gdrive_set_dribble("Projects/ADP/source_data/")
)
(load("source_data/2025_Draft_ADP_data.rdata"))        #  "work.data"  "trips_melt" "efrt"       "PartialCPs" "full_efrt"  "max_date"   "fg_em"    

#' Load `cost_params`, the output of `monitoring_costs.R``
gdrive_download(
  local_path = "source_data/cost_params_2025.Rdata", 
  gdrive_dribble = gdrive_set_dribble("Projects/ADP/Monitoring Costs - CONFIDENTIAL/")
)
(load("source_data/cost_params_2025.Rdata"))


# *** TODO REMOVE THIS SECTION? ----
#' #' TODO *loading the most recent version of Valhalla used for the 2023 Annual Report.* Use the outputs of get_data.R
#' #' when it is prepared. Download from Projects/ADP/ folder
#' gdrive_download("source_data/2_AR_data.Rdata", gdrive_set_dribble("Projects/AnnRpt-Deployment-Chapter/"))
#' AR_data_objects <- load("source_data/2_AR_data.Rdata")
#' # Remove everything except for work.data
#' rm(list = setdiff(ls(), c("work.data")))

#' #' TODO *Do this in get_data*
#' # Get count of GOA-only EM EFP Vessels for the cost_params. 
#' trawl_em_goa_v_count <- length(na.omit(unlist(unname(
#'   setDT(readxl::read_xlsx("source_data/2024 EM EFP Vessel List_NMFS.xlsx", col_names = F))[-c(1:3), 7]))))

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

#' Wrangle the Valhalla data set for spatiotemporal analyses using at least 2 full years
pc_effort_sub <- work.data[ADP >= adp_year - 2]
#' Use the STRATA_NEW column to define stratum and CVG_NEW to define coverage category
pc_effort_sub[
][, STRATA := STRATA_NEW
][, COVERAGE_TYPE := CVG_NEW] 

#' Simplify the dataset
pc_effort_st <- spatiotemp_data_prep(pc_effort_sub) 

#' Grab the most recent 1-year of data (trimming off 2 weeks from the last trip)
valhalla_date <- max_date - 14
pc_prev_year_trips <- pc_effort_st[
][, .(MIN_TRIP_TARGET_DATE = min(TRIP_TARGET_DATE)), keyby = .(TRIP_ID)
][between(MIN_TRIP_TARGET_DATE, valhalla_date - 365, valhalla_date), TRIP_ID]
pc_effort_st <- pc_effort_st[TRIP_ID %in% pc_prev_year_trips]
range(pc_effort_st$TRIP_TARGET_DATE)

#' TODO *Assigning strata to 2024/2025 definitions. Do this in get_data.R for 2025.* 
unique(pc_effort_st$STRATA) 
#' [get_data.R:  EM_FIXED_GOA   EM_FIXED_BSAI   EM_TRW_GOA   TRW   HAL   POT   ZERO]
#' [2025_strata: EM_FIXED-GOA   EM_FIXED-BSAI   EM_TRW-GOA   OB_TRW-GOA   OB_TRW-BSAI   OB_FIXED-GOA   OB_FIXED-BSAI   ZERO]
#' TODO The NEW_STRATA labels in get_data.R are still different from how we used them in 2024 or will for 2025
#' Rename STRATA to reflect monitoring method only
pc_effort_st[
][STRATA == "EM_FIXED_GOA", STRATA := "EM_FIXED"
][STRATA == "EM_FIXED_BSAI", STRATA := "EM_FIXED"
][STRATA == "EM_TRW_GOA", STRATA := "EM_TRW"
][STRATA %in% c("HAL", "POT"), STRATA := "FIXED"
][STRATA %in% c("FIXED", "TRW"), STRATA := paste0("OB_", STRATA)
#' Now apply FMP, separating monitoring method and FMP with a hyphen. Using BSAI_GOA, which determines FMP based on 
#' which had the most total retained catch
][, STRATA := ifelse(STRATA != "ZERO", paste0(STRATA, "-", BSAI_GOA), STRATA)]
unique(pc_effort_st$STRATA)

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
# Modify the trip dates to match the ADP Year
pc_effort_st[, TRIP_TARGET_DATE := TRIP_TARGET_DATE + (adp_year - year(TRIP_TARGET_DATE)) * 365]
pc_effort_st[, LANDING_DATE := LANDING_DATE + (adp_year - year(LANDING_DATE)) * 365]
setkey(pc_effort_st, STRATA)

#' Save and Upload pc_effort_st to the shared google drive 
save(pc_effort_st, file = paste0("source_data/pc_effort_st", "_", adp_year, ".Rdata"))
gdrive_upload(
  local_path = paste0("source_data/pc_effort_st", "_", adp_year, ".Rdata"),
  gdrive_dribble = gdrive_set_dribble("Projects/ADP/Output/")
)

#' Using `fg_em`, add the number of fixed-gear EM vessels to the `cost_params` list
cost_params$EMFG$emfg_v <- uniqueN(fg_em$PERMIT)

#====================#
## Box Parameters ----
#====================#

box_params <- list(
  space = c(2e5, 2e5),
  time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = "STRATA", ps_cols = "GEAR",
  stat_area_sf = stat_area_sf
)

#============#
## Budget ----
#============#

#' [Set the budget(s) to evaluate]
budget_lst <- list(4.4e6)   #' *Approximated Budget. Fee revenues collected in 2023*

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

# Extract the cost summary used in the allocation
cost_summary <- attr(boot_lst[[1]]$rates, "cost_summary")
cost_summary[, .(OB_TOTAL, EMFG_TOTAL, EMTRW_TOTAL)]
#' With a $4.4M budget, 800K carved off for EM_TRW, only 2.62M for at-sea observers and 0.98M for fixed-gear EM


#' Simulate sampling to create a distribution of expected costs, where variance is caused only by the random selection

#======================================================================================================================#
# Outputs ----
#======================================================================================================================#

budget_lst[[1]]   #' Total Budget
rates_adp         #' Allocated Rates, excluding EM_TRW
cost_summary

rates_adp %>% flextable::flextable()


# Generate EM_TRW summary (use bootstrapped results in Final ADP
em_trw_Nn <- pc_effort_st[STRATA == "EM_TRW-GOA", .(N = uniqueN(TRIP_ID)), by = .(STRATA)]
em_trw_Nn[, RATE := 0.33][, n := round(N * RATE)]

# Generate Zero pool summary (use bootstrapped results in Final ADP
pc_effort_st[STRATA == "ZERO", .(N = uniqueN(TRIP_ID)), by = .(STRATA)]


# Generate full coverage summary
full_efrt.smry <- pc_effort_sub[COVERAGE_TYPE == "FULL", .(START = min(TRIP_TARGET_DATE, LANDING_DATE, na.rm = T), END = max(TRIP_TARGET_DATE, LANDING_DATE, na.rm = T)), keyby = .(STRATA, TRIP_ID)]
full_efrt.smry[
][START >= (max_date - 14) - 365 & START < (max_date - 14)
][, .(N = uniqueN(TRIP_ID)), keyby = .(STRATA)]
