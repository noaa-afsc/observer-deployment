# Selection rates for the 2025 Draft ADP

# Author: Geoff Mayhew
# Start Date: 2024-Aug-07

#======================================================================================================================#
# Preparation ----
#======================================================================================================================#

adp_year <- 2025
adp_ver  <- "Final"  #' `Draft` or `Final`, which is case-sensitive!

#===================#
## Load Packages ----
#===================#

library(data.table)         # Data wrangling
library(ggplot2)            # Plotting
library(sf)                 # Spatial analyses
library(ggh4x)              # For facets with nested labels
library(waffle)             # For waffle plots of coverage/tonnage summaries
library(dplyr)              # For piping and handling sf objects
library(FMAtools)           # For connectivity to Analytical Services Program's Shared Google Drive
library(readxl)             # For read_xlsx
library(odbc)               # For database connectivity
library(flextable)          # For print-ready tables
library(officer)            # For additional flextable formatting options such as fp_border

#===============#
## Load data ----
#===============#

#' Load the outputs of `get_data.R`

gdrive_download(
  local_path = "source_data/2025_Final_ADP_data.rdata",
  gdrive_dribble = gdrive_set_dribble("Projects/ADP/source_data/")
)
(load("source_data/2025_Final_ADP_data.rdata"))

#' Load `cost_params`, the output of `monitoring_costs.R`. 2025 Draft used ver = 3. 
gdrive_download( 
  local_path = "source_data/cost_params_2025.Rdata", 
  gdrive_dribble = gdrive_set_dribble("Projects/ADP/Monitoring Costs - CONFIDENTIAL/")
)
(load("source_data/cost_params_2025.Rdata"))

#' Using `fg_em`, add the number of fixed-gear EM vessels to the `cost_params` list
cost_params$EMFG$emfg_v <- uniqueN(fg_em[FLAG %in% c("A", "NONE"), PERMIT])

#' Load `effort_glm`, the output of `effort_prediction.R` This is done for the final draft only.
if(adp_ver == "Final") {
  gdrive_download( 
    local_path = "source_data/effort_prediction_2025.rdata", 
    gdrive_dribble = gdrive_set_dribble("Projects/ADP/source_data/")
  )
  (load("source_data/effort_prediction_2025.rdata"))
}

# Load the ADFG statistical area shapefile.
stat_area_sf <- st_read(
  dsn = "source_data/ADFG_Stat_Area_shapefile/PVG_Statewide_2001_Present_GCS_WGS1984.shp", quiet = T) %>%
  select(STAT_AREA) %>%
  st_transform(crs = 3467)

#' [TODO: Move this to allocation_functions?]
# Load the Alaska map sf objects
load("source_data/ak_shp.rdata")      # shp_land, shp_nmfs, and shp_centroids added to global

#====================#
## Load Functions ----
#====================#

# load allocation functions
source("common_functions/allocation_functions.R")

format_dollar <- function(x, digits) paste0("$", formatC(x, digits = digits, big.mark = ",", format = "f"))

# Setting for flextable outputs
set_flextable_defaults(font.family = "Times New Roman", font.size = 10) 

#===============#
## Data Prep ----
#===============#

#' Wrangle the Valhalla data set for spatiotemporal analyses 
#' [TODO: This object is used to create the full coverage summary too, so name it something other than 'pc_...']
pc_effort_sub <- work.data.recent |>
  # Use at least 2 full years. We'll trim this down to 1 year after a bit of wrangling
  _[ADP >= adp_year - 2
    # Use the STRATA_NEW column to define stratum and CVG_NEW to define coverage category
  ][, STRATA := STRATA_NEW
  ][, COVERAGE_TYPE := CVG_NEW][]

#' Simplify the dataset
pc_effort_st <- spatiotemp_data_prep(pc_effort_sub) 

#' Grab the most recent 1-year of data (trimming off 2 weeks from the last trip)
valhalla_date <- max_date - 14
pc_prev_year_trips <- pc_effort_st |>
  _[, .(MIN_TRIP_TARGET_DATE = min(TRIP_TARGET_DATE)), keyby = .(TRIP_ID)
  ][between(MIN_TRIP_TARGET_DATE, valhalla_date - 365, valhalla_date), TRIP_ID]
pc_effort_st <- pc_effort_st[TRIP_ID %in% pc_prev_year_trips]
range(pc_effort_st$TRIP_TARGET_DATE)

# Re-assign the ADP Year. This tacks the Oct-Dec trips of current year - 1 onto the Jan-Oct trips of the current year.
pc_effort_st |>
  _[, ADP := adp_year
    # Modify the trip dates to match the ADP Year
  ][, TRIP_TARGET_DATE := TRIP_TARGET_DATE + (adp_year - year(TRIP_TARGET_DATE)) * 365
  ][, LANDING_DATE := LANDING_DATE + (adp_year - year(LANDING_DATE)) * 365]
setkey(pc_effort_st, STRATA)

#' Save and Upload pc_effort_st to the shared google drive 
save(pc_effort_st, file = paste0("source_data/pc_effort_st", "_", adp_year, ".Rdata"))
gdrive_upload(
  local_path = paste0("source_data/pc_effort_st", "_", adp_year, ".Rdata"),
  gdrive_dribble = gdrive_set_dribble("Projects/ADP/Output/")
)
#' [NOTE:] *Re-run monitoring_costs.R if pc_effort_st is changed!*

#====================#
## Box Parameters ----
#====================#

#' These parameters define our spatiotemporal boxes, how they neighbor one another, and which columns are used to 
#' identify the ADP year, strata, and post-strata.

box_params <- list(
  space = c(2e5, 2e5),
  time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = "STRATA", ps_cols = "GEAR",
  stat_area_sf = stat_area_sf
)

#============#
## Budget ----
#============#

#' Set the budget(s) to evaluate. The allocation functions were originally built to handle multiple budgets for the 
#' 2024 ADP (as a list). Can't be sure if they still can, but either way, the budget should be defined as a list.
budget_lst <- list(4.19e6)   #' [2025FinalADP: Reduced the $4.4M budget from the draft to $4.19M given reduced revenue]

#======================================================================================================================#
# Sampling Rates ----
#======================================================================================================================#

#' In the Draft, no bootstrap is performed, simply sampling all trips in `pc_effort_st` 1 time.
#' [TODO: Final draft resampling] In the Final, use the outputs of effort_prediction.R, specifically 'effort_strata[ADP == 2024]', to predict 
#' the total number of trips to sample for each stratum

#' For the final ADP, use the outputs of `effort_prediction.R` as our predicted fishing effort. In the draft, we 
#' simply assume fishing effort will be the same number of trips as fished in the previous 365 days. Also, set the 
#' number of effort resampling iterations
if (adp_ver == "Draft") {
  #' *Draft*
  resamp_iter <- 1
  sample_N <- list(pc_effort_st[, .(N = uniqueN(TRIP_ID)), keyby = .(ADP, STRATA)])
  
} else if(adp_ver == "Final") {
  #' *Final*
  
  resamp_iter <- 1000   # Use 1000 for full Final draft run. 10 is fine for testing.
  
  # Vary the number of trips in each stratum based on the effort prediction's confidence intervals.
  set.seed(12345)
  setDT(effort_prediction)
  effort_prediction[, STRATA := as.character(STRATA)]
  sample_N <- unname(split(
    rbindlist(
      # For each stratum, sample a number of trips based on a distribution of our fishing effort estimate
      lapply(split(effort_prediction, by = "STRATA"), function(x) {
        cbind(
          x[, .(I = 1:resamp_iter, ADP = as.integer(ADP), STRATA)],
          N = as.integer(round(exp(rnorm(n = resamp_iter, mean = x[["mean"]], sd = x[["sd"]])))))
      })
    ) |>
      # Order by resampling iteration, ADP, and stratum 
      setorder(I, ADP, STRATA),
    # Split by resampling iteration so each list element represents one iteration of partial coverage fishing effort
    by = "I", keep.by = F))
}

#=================================#
## Partial Coverage Allocation ----
#=================================#

gc()
boot_lst <- bootstrap_allo(pc_effort_st, sample_N, box_params, cost_params, budget_lst)

#' *If desired, manually save the outputs of the bootstrapping and allocation. It will by saved in the final outputs later.*
if(F) save(boot_lst, file = paste0("results/", adp_year, "_", adp_ver, "_ADP", "resample_results.rdata"))

# Extract the results from each iteration
boot_dt <- rbindlist(lapply(boot_lst, "[[", "rates"), idcol = "BOOT_ITER")

# If bootstrapping multiple populations, view the distribution of allocated rates
if(uniqueN(boot_dt$BOOT_ITER) > 1) {
  # Distribution of Proximity Allocation Index
  ggplot(unique(boot_dt[, .(BOOT_ITER, INDEX)]), aes(x = INDEX)) + geom_histogram()
  
  # Distribution of sampling rates
  ggplot(boot_dt, aes(x = STRATA, y = SAMPLE_RATE)) + geom_boxplot() + 
    stat_summary(geom = "point", fun = mean) + labs(x = "Stratum", y = "Sample Rate") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + geom_hline(yintercept = 0)
}

# Calculate the rates to use as the mean across iterations. Also calculate average proximity (PROX) and cv_scaling metrics
#' [NOTE:] Here we are calculating the mean of STRATA_N, SAMPLE_RATE, and n across resampling iterations, but they will
#' no longer be mathematically related. The only important thing we will be using here is the SAMPLE_RATE
rates_adp <- boot_dt[, lapply(.SD, mean), .SDcols = c("STRATA_N", "SAMPLE_RATE", "n", "PROX", "CV_SCALING", "INDEX"), keyby = .(ADP, STRATA)]


#===========================#
## Full Coverage Summary ---- 
#===========================#

# Generate full coverage summary so summary tables can reference it
#' [TODO: Apply the EM trawl CV-Tender Offloading matching here?]
full_efrt.smry <- pc_effort_sub[COVERAGE_TYPE == "FULL", .(
  START = min(TRIP_TARGET_DATE, LANDING_DATE, na.rm = T), 
  END = max(TRIP_TARGET_DATE, LANDING_DATE, na.rm = T)),
  keyby = .(STRATA, VESSEL_ID, TRIP_ID)
][START >= (max_date - 14) - 365 & START < (max_date - 14)]

#======================================================================================================================#
# Cost Simulation ----
#======================================================================================================================#

#' Now that rates are determined, simulate sampling to create a distribution of expected costs, where variance is caused 
#' only by the random selection of trips in the at-sea observer and fixed-gear EM strata.

#' [DRAFT:] For the Draft ADP, we will not simulate fishing effort, so `pc_effort_st` can be used as-is.
#' [FINAL:] For the Final ADP, we will bootstrap fishing effort using outputs of `effort_prediction.R`

#' [TODO: better to make a copy of cost_params here rather than modifying the original]
if( adp_ver == "Draft") {

  # Set number of ODDS simulations for cost estimtes
  odds_iter <- 10000
  # Initialize results list
  cost_boot_lst <- vector(mode = "list", length = odds_iter)
  # Identify trips and their strata
  cost_boot_trips_dt <- unique(pc_effort_st[, .(ADP, STRATA, TRIP_ID)])
  
  #' Hard-code the EM_TRW sampling rate, which is 1/3
  rates_em_trw_goa <- cost_boot_trips_dt[STRATA == "EM_TRW-GOA", .(STRATA_N = uniqueN(TRIP_ID)), keyby = .(ADP, STRATA)]
  cost_boot_rates <- rbind(rates_adp, rates_em_trw_goa, fill = T)
  cost_boot_rates[STRATA == "EM_TRW-GOA", SAMPLE_RATE := 0.3333]
  cost_boot_rates[, MON_RATE := SAMPLE_RATE]
  
  # Begin trip simulation
  set.seed(12345)
  for(i in seq_len(odds_iter)) {
    
    if(i == 1) cat(paste0(odds_iter, " iterations:\n"))
    if( (i %% (odds_iter/10)) == 0 ) cat(paste0(i, ", "))
    
    cost_params$EMTRW$emtrw_sea_days <- pc_effort_st |>
      _[STRATA == "EM_TRW-GOA", .(TRIP_ID, DAYS)] |> 
      unique() |>
      _[, sum(DAYS)]
    
    cost_boot_trips_dt.iter <- copy(cost_boot_trips_dt)
    # Merge in sampling rates. 
    cost_boot_trips_dt.iter <- cost_boot_trips_dt.iter[rates_adp[, .(ADP, STRATA, SAMPLE_RATE)], on = .(ADP, STRATA) ]
    # Assign random numbers
    cost_boot_trips_dt.iter[, RN := runif(.N)]
    # Subset the sampled trips, then subset pc_effort_st
    cost_boot_trips_dt.n <- pc_effort_st[cost_boot_trips_dt.iter[RN < SAMPLE_RATE, ], on = .(ADP, STRATA, TRIP_ID)]
    
    #' Calculate cost of each stratum. Just need a day summary 
    em_fg_d <- unique(cost_boot_trips_dt.n[STRATA %like% "EM_FIXED", .(ADP, STRATA, TRIP_ID, DAYS)])[
    ][, .(d = sum(DAYS)), keyby = .(ADP, STRATA)]
    
    cost_boot_lst[[i]] <- cbind(
      ob_cost_new(cost_boot_rates, cost_params, allo_sub = cost_boot_trips_dt.n, sim = T),
      emfg_cost(em_fg_d, cost_params, sim = T),
      emtrw_cost_new(cost_params)
    )
    
  }
  cost_dt <- rbindlist(cost_boot_lst, idcol = "ODDS_iter")
  cost_dt.med <- cost_dt[, median(OB_TOTAL + EMFG_TOTAL + EMTRW_TOTAL)]
  cost_dt.95 <- cost_dt[, list(quantile(OB_TOTAL + EMFG_TOTAL + EMTRW_TOTAL, c(0.025, .975)))]
  
} else if( adp_ver == "Final") {
  
  odds_iter <- 100
  
  set.seed(12345)
  
  # Subset what is needed from the ADP rates
  rates <- rates_adp[, .(ADP, STRATA, SAMPLE_RATE)]
  cost_lst <- vector(mode = "list", length = length(boot_lst))
  
  for(i in seq_along(boot_lst)) {
    
    # Return progress every 10%
    if( i %% ((length(boot_lst)/10))  == 0 ) cat(paste0(i, ", "))
    
    x <- lapply(boot_lst, "[[", "resample")[[i]]
    
    #' *Rebuild the fishing effort using the resampling results from allocation*
    resample_effort <- pc_effort_st[, .(ADP, POOL, STRATA, wd_TRIP_ID, DAYS, TRIP_TARGET_DATE, LANDING_DATE)] |>
      unique() |>
      _[x, on = .(wd_TRIP_ID)
      ][, TRIP_ID := .GRP, keyby = .(ADP, STRATA, wd_TRIP_ID, I)][]
    
    # Calculate cost of EM_TRW-GOA. This will vary only by number of days, not via ODDS.
    emtrw_goa_days <- unique(resample_effort[STRATA == "EM_TRW-GOA", .(TRIP_ID, DAYS)])[, sum(DAYS)]
    
    # Merge in sampling rates.
    cost_boot_trips_dt.iter <- copy(resample_effort)[rates, on = .(ADP, STRATA) ]
    
    cost_params$EMTRW$emtrw_sea_days <- resample_effort |>
      _[STRATA == "EM_TRW-GOA", .(TRIP_ID, DAYS)] |> 
      unique() |>
      _[, sum(DAYS)]
    
    #' *Simulate ODDS selection and calculate the realized costs*
    cost_lst[[i]] <- rbindlist(lapply(1:odds_iter, function(y) {
      
      # Assign random numbers, determine which trips were selected, and them subset from the fishing effort
      cost_boot_trips_dt.trip_id <- unique(cost_boot_trips_dt.iter[, .(TRIP_ID, SAMPLE_RATE)])[, RN := runif(.N)][]
      cost_boot_trips_dt.trip_id_sel <- cost_boot_trips_dt.trip_id[RN < SAMPLE_RATE, -"SAMPLE_RATE"]
      cost_boot_trips_dt.n <- cost_boot_trips_dt.iter[cost_boot_trips_dt.trip_id_sel, on = .(TRIP_ID)]
      
      #' Calculate number of sea days sampled in the fixed-gear EM strata
      em_fg_d <- cost_boot_trips_dt.n[STRATA %like% "EM_FIXED", .(ADP, STRATA, TRIP_ID, DAYS)] |>
        unique() |>
        _[, .(d = sum(DAYS)), keyby = .(ADP, STRATA)]
      
      cbind(
        ob_cost_new(rates, cost_params, allo_sub = cost_boot_trips_dt.n, sim = T),
        emfg_cost(em_fg_d, cost_params, sim = T),
        emtrw_cost_new(cost_params)
      )
      
    }), idcol = "ODDS_iter")
    
  }
  
  cost_dt <- rbindlist(cost_lst, idcol = "RESAMP_iter")[, TOTAL_COST := OB_TOTAL + EMFG_TOTAL + EMTRW_TOTAL]
  
}

 if(adp_ver == "Final"){
  # Are the costs normally distributed and centered on the budget?
  ggplot(cost_dt, aes(x = TOTAL_COST)) + geom_histogram() + geom_vline(xintercept = mean(cost_dt$TOTAL_COST))
  
  if(odds_iter >= 1) {
    ggplot(cost_dt[RESAMP_iter <= 50], aes(x = TOTAL_COST)) + geom_density(aes(fill = as.character(RESAMP_iter)), alpha = 0.25) +
      theme(legend.position = "none") +
      geom_vline(xintercept = mean(cost_dt$TOTAL_COST))
  }
  
  cost_dt.med <- cost_dt[, median(OB_TOTAL + EMFG_TOTAL + EMTRW_TOTAL)]
  cost_dt.95 <- cost_dt[, list(quantile(OB_TOTAL + EMFG_TOTAL + EMTRW_TOTAL, c(0.025, .975)))]
}


#====================================#
## Figure B-2. Cost Distribution  ----
#====================================#

label_y_pos <- 7500
figure_b2 <- ggplot(cost_dt, aes(x = OB_TOTAL + EMFG_TOTAL + EMTRW_TOTAL)) + 
  geom_histogram(fill = "gray65", bins = 30) + 
  geom_vline(aes(xintercept = budget_lst[[1]]), color = "purple", linetype = 2, linewidth = 1.1) +
  geom_text(
    x = budget_lst[[1]], y = label_y_pos, label = paste("Budget:", format_dollar(budget_lst[[1]], 0)), 
    angle = 90, hjust = 1, vjust = 2, color = "purple", check_overlap = T) + 
  geom_vline(aes(xintercept = cost_dt.med), color = "blue", linetype = 2, linewidth = 1) + 
  geom_text(
    x = cost_dt.med, y = label_y_pos, label = paste("Median:", format_dollar(cost_dt.med, 0)), 
    angle = 90, hjust = 1, vjust = -1, color = "blue", check_overlap = T) + 
  geom_vline(data = cost_dt.95, aes(xintercept = V1), color = "red", linetype = 2, linewidth = 1) + 
  geom_text(
    data = cost_dt.95, aes(x = V1, y = label_y_pos, label = format_dollar(V1, 0)), 
    angle = 90, hjust = 1, vjust = c(1.8, -1), color = "red") + 
  scale_x_continuous(labels = function(x) formatC(x / 1e6, digits = 1, format = "f")) + 
  labs(x = "Total cost (millions)", y = "Number of ODDS iterations with this outcome") +
  theme(axis.text = element_text(color = "black")) # Make text black so it's easier to read

# Note that this includes the cost of EM_TRW which in this simulation, had no variability.

#======================================================================================================================#
# Monitoring Costs ----
#======================================================================================================================#

# Extract the cost summaries used in the sampling simulations.
if(adp_ver == "Draft") {
  #' [DRAFT: Grabbing attributes from the only iteration]
  cost_summary <- attr(boot_lst[[1]]$rates, "cost_summary")
  # Get totals, rounding to the nearest $1K
  cost_totals <- round(
    unlist(cost_summary[, .(OB_TOTAL, EMFG_TOTAL, EMTRW_TOTAL, TOTAL = sum(OB_TOTAL, EMFG_TOTAL, EMTRW_TOTAL))]), -3
  )
  
} else if(adp_ver == "Final") {
  #' [FINAL: Average across attributes from all iterations]
  cost_summary <- lapply(lapply(boot_lst, "[[", "rates"), function(x) attr(x, "cost_summary"))
  # Get totals, rounding to the nearest $1K
  cost_totals <- rbindlist(cost_summary)[, .(
    OB_TOTAL = mean(OB_TOTAL), EMFG_TOTAL = mean(EMFG_TOTAL), EMTRW_TOTAL = mean(EMTRW_TOTAL),
    TOTAL = sum(OB_TOTAL, EMFG_TOTAL, EMTRW_TOTAL) / .N)] |>
    round(-3) |> unlist()
}


#===================================#
## Table B-1. Budget and Vessels ----
#===================================#

#' Cost summary


#' Naming the monitoring method 'strata' here for the sake of keeping the same header as the vessel counts below. The 
#' final table's column names will be named manually
cost_totals.pc <- data.table("STRATA" = names(cost_totals), data.table("value" = format_dollar(cost_totals, 0)))
# Rename the pools
cost_totals.pc[, STRATA := fcase(
  STRATA == "OB_TOTAL", "At-sea Observer",
  STRATA == "EMFG_TOTAL", "EM Fixed-Gear",
  STRATA == "EMTRW_TOTAL", "EM Trawl GOA",
  STRATA == "TOTAL", "Total"
)]
cost_totals.pc <- rbind(data.table(STRATA = "Partial Coverage Monitoring Budget ($)"), cost_totals.pc, fill = T)

# Number of vessels
vessel_totals.pc <- pc_effort_st[, .(value = uniqueN(PERMIT)), keyby = .(STRATA)]
vessel_totals.pc[
][, STRATA := gsub("-", " ", STRATA)
][, STRATA := sub("_FIXED", " Fixed-gear", STRATA)
][, STRATA := sub("_TRW", " Trawl", STRATA)
][, STRATA := sub("^OB", "At-sea Observer", STRATA)
][, STRATA := sub("ZERO", "Zero", STRATA)]
setorder(vessel_totals.pc, STRATA)
vessel_totals.pc <- rbind(data.table(STRATA = "Vessels Participating (Partial Coverage)"), vessel_totals.pc, fill = T)

vessel_totals.fc <- full_efrt.smry[, .(value = uniqueN(VESSEL_ID)), keyby = .(STRATA)]
vessel_totals.fc[, STRATA := fcase(
  STRATA == "EM_TRW-BSAI", "EM Trawl BSAI",
  STRATA == "FULL", "Full Coverage"
)]
setorder(vessel_totals.fc, -STRATA)
vessel_totals.fc <- rbind(data.table(STRATA = "Vessels Participating (Full Coverage)"), vessel_totals.fc, fill = T)

table_b1 <- rbind(cost_totals.pc, vessel_totals.pc, vessel_totals.fc)

if(adp_ver == "Draft") {
  table_b1.flex <- table_b1 %>% 
    flextable() %>%
    compose(i = ~ !is.na(value), value = as_paragraph("\t", .), use_dot = T) %>%
    set_header_labels(values = c("", paste0("Draft ", adp_year, " ADP"))) %>%
    align(j = 2, align = "right", part = "all") %>%
    bold(i = ~ is.na(value)) %>%
    bold(part = "header") %>%
    hline(i = ~ STRATA == "") %>%
    hline(i = 5) %>%
    autofit()
} else if(adp_ver == "Final") {
  
  table_b1.final <- copy(table_b1)
  
  #' Load the Draft outputs and re-assign them
  draft_tables_name <- paste0("results/", adp_year, "_Draft_ADP_tables.rdata")
  gdrive_download(
    local_path =  draft_tables_name,
    gdrive_dribble = gdrive_set_dribble("Projects/ADP/Output")
  )
  (load(draft_tables_name))
  table_b1.draft <- copy(table_b1)
  table_b2.draft <- copy(table_b2)
  table_b3.draft <- copy(table_b3)
  rm(table_b1, table_b2, table_b3)
  
  #' [2025Final: After creating these objects for the Draft we rearranged in the document, apply those changes here]
  
  table_b1.groups <- data.table(group = c(rep("costs", 5), rep("vessels", 12)))
  table_b1.draft <- cbind(table_b1.groups, table_b1.draft)
  table_b1.final <- cbind(table_b1.groups, table_b1.final)
  table_b1 <- table_b1.draft[table_b1.final, on = .(group, STRATA)]
  table_b1[, group := NULL]
  
  table_b1.flex <- table_b1 %>% 
    flextable() %>%
    compose(i = ~ !is.na(value), value = as_paragraph("\t", .), use_dot = T) %>%
    set_header_labels(values = c("", paste0("Draft ", adp_year, " ADP"), paste0("Final ", adp_year, " ADP"))) %>%
    align(j = 2, align = "right", part = "all") %>%
    bold(i = ~ is.na(value)) %>%
    bold(part = "header") %>%
    hline(i = ~ STRATA == "") %>%
    hline(i = 5) %>%
    autofit()
  
}

#======================================================================================================================#
# Sampling Summaries ----
#======================================================================================================================#

#===================================#
## Table B-2. Allocation Indices ----
#===================================#

# Proximity allocation indices
table_b2 <- copy(rates_adp)[, -"ADP"]
# Change 
table_b2[, SAMPLE_RATE := round(SAMPLE_RATE * 100, 2)]
# 
table_b2[
][, STRATA := gsub("-", " ", STRATA)
][, STRATA := sub("_FIXED", " Fixed-gear", STRATA)
][, STRATA := sub("_TRW", " Trawl", STRATA)
][, STRATA := sub("^OB", "At-sea Observer", STRATA)
][, STRATA := sub("ZERO", "Zero", STRATA)]
setorder(table_b2, STRATA)
setnames(table_b2, new = c("Stratum", "N", "r", "n", "T", "F", "D"))
table_b2.flex <- table_b2 %>% 
  flextable() %>%
  compose(i = 1, j = 1, part = "header", value = as_paragraph(., " (", as_i("h"), ")" ), use_dot = T) %>%
  compose(i = 1, j = c(2, 3:7), part = "header", value = as_paragraph(as_i(.), as_sub(as_i("h") )), use_dot = T) %>%
  mk_par(i = 1, j = 5, part = "header", value = as_paragraph(as_i("T\U0302"), as_sub(as_i("h")))) %>%
  mk_par(i = 1, j = 7, part = "header", value = as_paragraph(as_i("D\U0302"), as_sub(as_i("h")))) %>%
  colformat_num(j = 2, big.mark = ",", part = "body") %>%
  colformat_double(j = 4, digits = 2) %>%
  colformat_double(j = 5:7, digits = 4) %>%
  add_header_row(top = F, values = c(paste0("Draft ", adp_year, " ADP"), ""), colwidths = c(1, 6)) %>%
  bold(i = 2, j = 1, part = "header") %>%
  autofit()


#=====================================#
## Table B-3. Rates Trips and Days ----
#=====================================#

#' TODO Would be nice to capture trips and days monitored by stratum in the trip selection simulations!
#' Note that days monitored 'd' for EM_TRW strata reflects the 100% at-sea compliance monitoring rate. 
#' 'n' is a reflection of the shoreside monitoring rate and should be considered as an estimate for the number of deliveries

rates_trips_days.pc <- unique(pc_effort_st[, .(POOL, STRATA, TRIP_ID, DAYS)])[, .(STRATA_N = uniqueN(TRIP_ID), MTD = mean(DAYS)), keyby = .(POOL, STRATA)]
# Merge in monitoring rates and stratum trip counts
rates_trips_days.pc <- rates_adp[, .(STRATA, SAMPLE_RATE, n)][rates_trips_days.pc, on = .(STRATA)]
# Hard-code the EM_TRW-GOA and ZERO pool selection rates
rates_trips_days.pc[
][STRATA == "EM_TRW-GOA", SAMPLE_RATE := 0.3333
][STRATA == "ZERO", SAMPLE_RATE := 0
][is.na(n), n := STRATA_N * SAMPLE_RATE]
# Estimated days monitored based on trips sampled 'n' and mean trip duration 'MTD'.
# Also, dh (days) for EM_TRW-GOA were modified to reflect a 100% compliance monitoring rate at sea, using N.
rates_trips_days.pc[, d := ifelse(STRATA == "EM_TRW-GOA", STRATA_N * MTD, n * MTD)][, MTD := NULL]
# Refine pool labels
rates_trips_days.pc[, POOL := fcase(
  POOL == "OB", "At-sea Observer",
  POOL == "ZE", "Zero",
  STRATA %like% "EM_FIXED", "Fixed-gear EM",
  STRATA %like% "EM_TRW", "Trawl EM")]
# Refine strata names
rates_trips_days.pc[
][, STRATA := gsub("-", " ", STRATA)
][, STRATA := sub("_FIXED", " Fixed-gear", STRATA)
][, STRATA := sub("_TRW", " Trawl", STRATA)
][, STRATA := sub("^OB", "At-sea Observer", STRATA)
][, STRATA := sub("ZERO", "Zero", STRATA)]
# Convert sample rate to percentage
rates_trips_days.pc[, SAMPLE_RATE := SAMPLE_RATE * 100]
# Round off estimates
round_cols <- c("STRATA_N", "n", "d")
rates_trips_days.pc[, (round_cols) := lapply(.SD, round), .SDcols = round_cols]
rates_trips_days.pc[, SAMPLE_RATE := round(SAMPLE_RATE, 2)]
setnames(rates_trips_days.pc, new = c("Stratum", "r", "n", "Pool", "N", "d"))
setcolorder(rates_trips_days.pc, c("Pool", "Stratum", "N", "n", "d", "r"))
# Get list of strata 
rates_trips_days.pc.strata <- unique(rates_trips_days.pc$Stratum)
# Create totals by POOL when there is more than one stratum
rates_trips_days.pc.total <- rates_trips_days.pc |>
  _[, .(Stratum = "Total", N = sum(N), n = sum(n), d = sum(d), r = round(sum(n)/sum(N) * 100, 2), STRATA_COUNT = .N), 
    keyby = .(Pool)
  ][STRATA_COUNT > 1
  ][, STRATA_COUNT := NULL][]
# Add totals in
rates_trips_days.pc <- rbind(
  rates_trips_days.pc,
  rates_trips_days.pc.total
)
# Make Stratum column a factor and add Total as the final level
rates_trips_days.pc[, Stratum := factor(Stratum, levels = c(rates_trips_days.pc.strata, "Total") )]
setorder(rates_trips_days.pc, Pool, Stratum)

# Add full coverage in as well.
rates_trips_days.fc <- full_efrt.smry[, .(
  Pool = "Full Coverage", r = 100, N = uniqueN(TRIP_ID), n = uniqueN(TRIP_ID),
  d = sum(as.numeric(END - START, units = "days") + 1)), 
  keyby = .(STRATA)]
# Refine strata names
rates_trips_days.fc[, STRATA := fcase(
  STRATA == "EM_TRW-BSAI", "EM Trawl BSAI",
  STRATA == "FULL", "Full"
)]
# Create totals by POOL
rates_trips_days.fc.total <- rates_trips_days.fc[, .(
  STRATA = "Total", N = sum(N), n = sum(n), d = sum(d), r = round(sum(n)/sum(N) * 100, 2)
), keyby = .(Pool)]
rates_trips_days.fc.strata <- rev(unique(rates_trips_days.fc$STRATA))
setorder(rates_trips_days.fc, -STRATA)
# Add totals in
rates_trips_days.fc <- rbind(
  rates_trips_days.fc,
  rates_trips_days.fc.total
)
setnames(rates_trips_days.fc , new = c("Stratum", "Pool", "r", "N", "n", "d"))
setcolorder(rates_trips_days.fc, c("Pool", "Stratum", "N", "n", "d", "r"))

table_b3 <- rbind(
  rates_trips_days.pc,
  rates_trips_days.fc
)
if(adp_ver == "Draft") {

  # Now that totals are made for pool, remove pool column
  table_b3.flex <- table_b3[, -"Pool"] %>% 
    flextable() %>%
    autofit() %>%
    compose(i = 1, j = 1, part = "header", value = as_paragraph(., " (", as_i("h"), ")" ), use_dot = T) %>%
    compose(i = 1, j = 2:4, part = "header", value = as_paragraph(as_i(.), as_sub(as_i("h") )), use_dot = T) %>%
    compose(i = 1, j = 5, part = "header", value = as_paragraph(as_i(.), as_sub(as_i("h") ), " (%)"), use_dot = T) %>%
    bold(i = ~ Stratum == "Total", part = "body") %>%
    bold(i = c(9,10), j = 2:5, part = "body") %>%
    hline(i = ~ Stratum %in% c("Total", "EM Trawl GOA", "Zero")) %>%
    compose(i = ~ Stratum == "Total", j = 1, value = as_paragraph("\t", .), use_dot = T) %>%
    add_header_row(top = F, values = c(paste0("Draft ", adp_year, " ADP"), ""), colwidths = c(1, 4)) %>%
    bold(i = 2, j = 1, part = "header") %>%
    fix_border_issues()
  # Note that the rates for EM_TRW-GOA are for shoreside monitoring only and that 'n' is an approximation

} else if(adp_ver == "Final") {
  
  #' [TODO: Add new columns for total # of offloads and sampled offloads? 'O' and 'o'? for EM_TRW strata?]
  table_b3.final <- copy(table_b3)
  table_b3 <- rbind(
    table_b3.draft, 
    rbind(data.table(Stratum = paste(adp_ver, adp_year, "ADP")), table_b3.final, fill = T))
  
  table_b3.flex <- table_b3[, -"Pool"] %>% 
    flextable() %>%
    autofit() %>%
    compose(i = 1, j = 1, part = "header", value = as_paragraph(., " (", as_i("h"), ")" ), use_dot = T) %>%
    compose(i = 1, j = 2:4, part = "header", value = as_paragraph(as_i(.), as_sub(as_i("h") )), use_dot = T) %>%
    compose(i = 1, j = 5, part = "header", value = as_paragraph(as_i(.), as_sub(as_i("h") ), " (%)"), use_dot = T) %>%
    bold(i = ~ Stratum == "Total", part = "body") %>%
    bold(i = c(9,10), j = 2:5, part = "body") %>%
    hline(i = ~ Stratum %in% c("Total", "EM Trawl GOA", "Zero")) %>%
    hline(i = nrow(table_b3.draft) + 1) %>%
    compose(i = ~ Stratum == "Total", j = 1, value = as_paragraph("\t", .), use_dot = T) %>%
    add_header_row(top = F, values = c(paste0("Draft ", adp_year, " ADP"), ""), colwidths = c(1, 4)) %>%
    bold(i = 2, j = 1, part = "header") %>%
    bold(i = nrow(table_b3.draft) + 1, j = 1, part = "body") %>%
    padding(i = ~ Stratum != paste(adp_ver, adp_year, "ADP"), part = "body", padding.top = 2, padding.bottom = 2) %>%
    fix_border_issues()
}


#======================================================================================================================#
# Monitoring Summary ----
#======================================================================================================================#

#' [TODO: Do I still need this? This was used for the 2025 Draft.]

#' Here we will summarize the monitoring of the entire AK. Used in the PCFMAC presentation of the 2025 Draft ADP.

#' *Full coverage*
# Grab all full coverage trips in the last year
mon_catch.fc <- work.data.recent |>
  _[, -c("STRATA")
    # Subset, grabbing only full coverage trips from the most recent year
  ][full_efrt.smry, on = .(VESSEL_ID, TRIP_ID)
    # Broadly categorize gear types by fixed and trawl.
  ][, FIXED_TRW := ifelse(AGENCY_GEAR_CODE %in% c("PTR", "NPT"), "TRW", "FIXED")
    # Categorize monitoring method
  ][, MON := ifelse(STRATA %like% "EM_", "EM", "OB")
    # Sum up retained catch by monitoring method, FMP, and gear
  ][, .(RET_CATCH = sum(WEIGHT_POSTED[SOURCE_TABLE == "Y"], na.rm = T)), keyby = .(MON, FMP, FIXED_TRW)
  ][, MON_CATCH := RET_CATCH
  ][, PERC_MON := MON_CATCH / RET_CATCH * 100][]

#' *Partial Coverage*

mon_catch.pc <- work.data.recent |>
  _[, -"STRATA"
    # Subset only partial coverage sector trips
  ][unique(pc_effort_st[, .(wd_TRIP_ID, STRATA)]), on = c(TRIP_ID = "wd_TRIP_ID")
    # Remove any program management codes that are state-managed
  ][!(MANAGEMENT_PROGRAM_CODE %in% c("SMO", "SMPC", "SMS"))
    # Broadly categorize gear types by fixed and trawl.
  ][, FIXED_TRW := ifelse(AGENCY_GEAR_CODE %in% c("PTR", "NPT"), "TRW", "FIXED")
    # Sum retained catch by stratum, FMP and gear type category
  ][, .(
    RET_CATCH = sum(WEIGHT_POSTED[SOURCE_TABLE == "Y"], na.rm = T)
  ), keyby = .(FMP, FIXED_TRW, STRATA)]
# Merge partial coverage sampling rates in
mon_catch.pc <- mon_catch.pc |>
  _[, SAMPLE_RATE := rates_adp[mon_catch.pc, SAMPLE_RATE, on = .(STRATA)]
  ][STRATA == "ZERO", SAMPLE_RATE := 0
  ][STRATA == "EM_TRW-GOA", SAMPLE_RATE := 1
    # Define Monitoring category
  ][, MON := fcase(STRATA %like% "EM", "EM", STRATA %like% "OB", "OB", STRATA %like% "ZERO", "ZERO")
    # Sum up retained catch by monitoring method, FMP, and gear
  ][, .(RET_CATCH = sum(RET_CATCH), MON_CATCH = sum(RET_CATCH * SAMPLE_RATE)), keyby = .(MON, FMP, FIXED_TRW)
  ][, PERC_MON  := MON_CATCH / RET_CATCH * 100][]

mon_catch <- rbind(
  cbind(SECTOR = "FULL", mon_catch.fc),
  cbind(SECTOR = "PARTIAL", mon_catch.pc)
)[
  # Calculate tonnage of unmonitored catch
][, UNMON_CATCH := RET_CATCH - MON_CATCH][]

# Convert the summary to long form
mon_catch.long <- melt(
  mon_catch, 
  id.vars = c("SECTOR", "FMP", "FIXED_TRW", "MON"),
  measure.vars = c("UNMON_CATCH", "MON_CATCH")
) |>
  # Divide tonnage into 800 units. With 800, each box represents .25% of catch, each column represents 5%
  _[, unit_count := round(value/sum(value) * 800)
    # Define groupings based on sector, monitoring method, and whether catch was expected to be monitored.
  ][, GROUP := fcase(
    SECTOR == "FULL" & variable == "MON_CATCH" & MON == "OB", "Full coverage - Observer",
    SECTOR == "FULL" & variable == "MON_CATCH" & MON == "EM", "Full coverage - EM",
    SECTOR == "PARTIAL" & MON == "ZERO", "No selection",
    SECTOR == "PARTIAL" & variable == "UNMON_CATCH", "Partial coverage - Unmonitored",
    SECTOR == "PARTIAL" & variable == "MON_CATCH" & MON == "OB", "Partial coverage - Observer",
    SECTOR == "PARTIAL" & variable == "MON_CATCH" & MON == "EM", "Partial coverage - EM"
  )
  # Re-label variables for plotting
  ][, FIXED_TRW := ifelse(FIXED_TRW == "FIXED", "Fixed", "Trawl")
  ][, GROUP := factor(GROUP, levels = c(
    "Full coverage - Observer", "Full coverage - EM",
    "Partial coverage - Observer", "Partial coverage - EM", "Partial coverage - Unmonitored", "No selection"))
  ][, FMP := factor(FMP, levels = c("BSAI", "GOA"))
  ][, FIXED_TRW := factor(FIXED_TRW, levels = c("Trawl", "Fixed"))][]
setorder(mon_catch.long, GROUP, FMP, FIXED_TRW)

## Figure for PCFMAC - Waffle plot of tonnage monitored ----
figure_ppt_monitoring_summary <- ggplot(mon_catch.long, aes(values = unit_count, fill = GROUP)) + 
  facet_nested(. ~ FMP + FIXED_TRW, scales = "free_x", space = "free_x") + 
  geom_waffle(color = "white", n_rows = 40) +   #' with 20 rows and 400 units (each box = 0.25%), makes each full column represent 20/400 = 5% or 20 * .25% = 5%
  #theme_minimal() + 
  theme_enhance_waffle() +
  theme(
    axis.ticks = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "gray85", fill = NA),
    strip.background = element_rect(color = "gray85"),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = T)) + 
  scale_fill_manual(values = c("deepskyblue", "dodgerblue",  "magenta", "purple", "pink", "gray")) + 
  labs(fill = "Expected monitoring") 

# Proportion of tonnage monitored
mon_catch.long[, sum(value[variable == "MON_CATCH"]) / sum(value)] # 96% of fisheries are monitored
mon_catch.long[, sum(value[variable == "MON_CATCH"]) / sum(value), keyby = .(FMP)] # 99% in the BSAI and 72% in the GOA
mon_catch.long[, sum(value), keyby = .(SECTOR)][, .(SECTOR, Tonnage = V1, Percent = 100 * V1 / sum(V1))] # 10% of fisheries is in partial coverage

# Excluding EM TRW, we are monitoring 13% of tonnage in partial coverage
mon_catch.long[!(FIXED_TRW != "TRW" & MON == "EM") & SECTOR == "PARTIAL", sum(value[variable == "MON_CATCH"]) / sum(value)]

# Percent monitored by FMP and Gear (both full and partial coverage)
mon_catch.long[, .(PERC_MON = 100 * sum(value[variable == "MON_CATCH"]) / sum(value)), keyby = .(FMP, FIXED_TRW)]
# Percent monitored by FMP and Gear in partial coverage only
mon_catch.long[SECTOR == "PARTIAL", .(PERC_MON = 100 * sum(value[variable == "MON_CATCH"]) / sum(value)), keyby = .(FMP, FIXED_TRW)]


#======================================================================================================================#
# Outputs ----
#======================================================================================================================#

#' *Allocation Objects*
#' Upload all allocation inputs and outputs to the shared google drive.
results_name <- paste0("results/", adp_year, "_", adp_ver, "_ADP_results.rdata")
save(
  ## Allocation inputs
  pc_effort_st, budget_lst, box_params, 
  ## Allocation products
  boot_lst, rates_adp,      
  cost_summary, cost_dt,
  ## Raw tables
  table_b1, table_b2, table_b3,
  ## Location
  file = results_name
)
gdrive_upload(
  local_path = results_name,
  gdrive_dribble = gdrive_set_dribble("Projects/ADP/Output")
)

#' *Tables*
#' Save table outputs for `tables.Rmd` to knit to docx format
tables_name <- paste0("results/", adp_year, "_", adp_ver, "_ADP_tables.rdata")
save(
  table_b1, table_b1.flex,
  table_b2, table_b2.flex,
  table_b3, table_b3.flex,
  file = tables_name
)
gdrive_upload(
  local_path = tables_name,
  gdrive_dribble = gdrive_set_dribble("Projects/ADP/Output")
)

#' *Figures* 

#'   *Figure B-2.* Summary of `cost_iter` outcomes of simulated sampling in ODDS showing the total costs of the 
#' partial coverage monitoring program expected for 2025. Vertical lines depict the available budget (purple line), 
#' median expected cost (blue line), and 95% confidence limits (red lines).
ggsave(filename = "output_figures/figure_b2.png", plot = figure_b2, width = 5, height = 5, units = "in")

#'   *Figure for PCFMAC* Waffle plot of tonnage monitored
ggsave("output_figures/figure_ppt_monitoring_summary.png" , figure_ppt_monitoring_summary, width = 8, height = 5, units = "in")
