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
pc_effort_st[is.na(DAYS), DAYS := as.numeric(
  1 + max(TRIP_TARGET_DATE, LANDING_DATE) - min(TRIP_TARGET_DATE, LANDING_DATE), units = "days"
), by = .(TRIP_ID)]
if(nrow(pc_effort_st[is.na(DAYS)])) stop("Some records are still missing DAYS")

# Re-assign the ADP Year
pc_effort_st[, ADP := 2025]

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


#========================#
## Trawl EM Carve-off ----
#========================#

#' TODO *Estimate the total costs of trawl EM for 2025, then subtract from the estimated budget*


#================#
## Parameters ----
#================#

#'budget_lst <- list(4.8e6 + 1.019e6)  #' TODO *UPDATE THIS* [Budget of $5,819,000 used in the 2024 ADP]
budget_lst <- list(4.0e6)   #' *Bleak estimate given fee revenues minus some trawl EM costs and no additional funds*

#' Number of bootstrap iterations. Typically do 1K each. 
#' TODO *Until we have an effort prediction, just use 1 bootstrap iteration, using actual N as our 'prediction'*
bootstrap_iter <- 1
#bootstrap_iter <- 1000
#sim_iter <- 1000   #' Simulations of trip selection will be used later for creating distribution of cost estimates


#=======================================#
## Generate Bootstrapped Populations ----
#=======================================#

#==============================#
### Sample with replacement ----
#==============================#

#' TODO  Use the outputs of effort_prediction.R, specifically 'effort_strata[ADP == 2024]', to predict the total number of
#' trips to sample for each stratum

# sample_N <- copy(effort_strata[ADP == 2024])[
# ][, STRATA := gsub("_BSAI", "-BSAI", STRATA)
# ][, STRATA := gsub("_GOA", "-GOA", STRATA)
# ][, STRATA := gsub("_EFP", "", STRATA)
# ][!(STRATA %like% "EM|ZERO"), STRATA := paste0("OB_", STRATA)
# ][, N := round(TOTAL_TRIPS)]
# sample_N <- sample_N[, .(STRATA, N)]
#' TODO *For now just use number in dataset as a placeholder*
sample_N <- pc_effort_st[, .(N = uniqueN(TRIP_ID)), keyby = .(STRATA)]

setkey(pc_effort_st, STRATA)
pc_effort_lst <- split(pc_effort_st, by = "STRATA")

# Make sure names and ordering are the same
if(!identical(names(pc_effort_lst), sample_N$STRATA)) stop("Stratum names/order are not the same!")

# Initialize bootstrap list
swor_boot_lst <- vector(mode = "list", length = bootstrap_iter)

# Run the bootstrap loop. Takes ~4.5 hours to complete 1000 bootstrap iterations.
#' TODO *Make the bootstrapping allocation a function.*
set.seed(12345)
for(k in seq_len(bootstrap_iter)) {
  # k <- 1
  cat(k, ", ")
  
  # Bootstrap using adp_strata_N to sample each stratum's population size size
  swor_bootstrap.effort <- rbindlist(Map(
    function(prior, strata_N) {
      # prior <- pc_effort_lst[[5]]; strata_N <- sample_N$N[5]
      
      # Create vector of TRIP_IDs
      trip_ids <- unique(prior$TRIP_ID)
      # How many times does prior effort go into future effort?
      prior_vs_future <- floor(strata_N / length(trip_ids))
      # What number of trips should be sampled without replacement?
      swr_n <- strata_N - (length(trip_ids) * prior_vs_future)
      # Create dt of trip_ids
      sampled_trip_ids <- data.table(
        TRIP_ID = c(
          # Repeat trip_id prior_vs_future times
          rep(trip_ids, times = prior_vs_future), 
          # Sample without replacement using swr_n
          sample(trip_ids, size = swr_n, replace = F)
        )
      )
      sampled_trip_ids[, I := .I]
      # Bring in each trip's data
      bootstrap_sample <- prior[sampled_trip_ids, on = .(TRIP_ID), allow.cartesian = T]
    }, 
    prior = pc_effort_lst,
    strata_N = sample_N$N
  ))
  
  # Re-assign trip_id so that we can differentiate trips sampled multiple times
  swor_bootstrap.effort[, TRIP_ID := .GRP, keyby = .(ADP, STRATA, BSAI_GOA, TRIP_ID, I)]
  if(uniqueN(swor_bootstrap.effort$TRIP_ID) != sum(sample_N$N)) stop("Count of TRIP_IDs doesn't match!")
  
  # [2024 only] Apply the Trawl EM EFP opt-in probability, move those that 'opt-out' into OB_TRW-GOA
  # em_trw_id <- unique(swor_bootstrap.effort[STRATA == "EM_TRW-GOA", TRIP_ID ])
  # # Randomly sample vessels as opting in (TRUE) or out (FALSE) of the EFP. We will use the 'expected' number of trips
  # # opting out rather than allowing it to be stochastic so that STRATA_N does not vary between iterations.
  # trw_em_opt_out_N <- round(sample_N[STRATA == "EM_TRW-GOA", N] * efp_prob[COVERAGE_TYPE == "PARTIAL", 1 - EFP_PROB])
  # trw_em_opt_out_id <- sample(em_trw_id, size = trw_em_opt_out_N)
  # swor_bootstrap.effort[TRIP_ID %in% trw_em_opt_out_id, ':=' (POOL = "OB", STRATA = "OB_TRW-GOA")]
  
  # Define boxes of bootstrapped effort
  swor_bootstrap.box <- define_boxes(
    swor_bootstrap.effort, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
    year_col = "ADP", stratum_cols = c("STRATA"), geom = F, ps_cols = c("GEAR"))
  # Calculate each stratum's mean trip duration of bootstrapped effort
  swor_bootstrap.allo_lst <- list(effort = unique(swor_bootstrap.effort[, .(ADP, STRATA, BSAI_GOA, TRIP_ID, DAYS)])[
  ][, .(STRATA_N = uniqueN(TRIP_ID), TRP_DUR = mean(DAYS)), keyby = .(ADP, STRATA)])
  
  # Calculate rates afforded with the specified budget
  #' [NOTE] The `allo_prox()` function currently excludes the "EM_TRW" strata by default.
  swor_bootstrap.rates <- allo_prox(
    swor_bootstrap.box, swor_bootstrap.allo_lst, cost_params, budget_lst[[1]], max_budget = 7e6, index_interval = 0.0001
  )
  # Capture results of iteration
  swor_boot_lst[[k]] <- list(
    rates = swor_bootstrap.rates,
    strata_N = sample_N
  )
  
}

# Save the outputs of the bootstrapping and allocation
if(F) save(swor_boot_lst, file = "results/swor_boot_lst.rdata")

# Extract the results from each iteration
boot_dt <- rbindlist(lapply(swor_boot_lst, "[[", "rates"), idcol = "BOOT_ITER")


#=============================#
# Determine Rates for 2024 ----
#=============================#

ggplot(boot_dt, aes(x = STRATA, y = SAMPLE_RATE)) + geom_violin(draw_quantiles = 0.5) + 
  facet_wrap(.~ STRATA, scales = "free") + stat_summary(geom = "point", fun = mean)

ggplot(boot_dt, aes(x = STRATA, y = SAMPLE_RATE)) + geom_violin(draw_quantiles = 0.5) + 
  stat_summary(geom = "point", fun = mean) + labs(x = "Stratum", y = "Sample Rate") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + geom_hline(yintercept = 0)

# Calculate the rates to use as the mean across iterations. Also calculate average proximity (ISPN) and cv_scaling metrics
rates_adp <- boot_dt[, lapply(.SD, mean), .SDcols = c("SAMPLE_RATE", "n", "ISPN", "CV_SCALING", "INDEX"), keyby = .(ADP, STRATA, STRATA_N)]
rates_adp
# Ooof... With a $4M budget, only 6.5% in GOA , 18-20% in BSAI for observers.

