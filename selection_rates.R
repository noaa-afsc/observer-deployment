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
library(flextable)          # For print-ready tables
library(officer)            # For additional flextable formatting options such as fp_border

#===============#
## Load data ----
#===============#

#' Load the outputs of `get_data.R`
gdrive_download(
  local_path = "source_data/2025_Draft_ADP_data.rdata",
  gdrive_dribble = gdrive_set_dribble("Projects/ADP/source_data/")
)
(load("source_data/2025_Draft_ADP_data.rdata"))

#' Load `cost_params`, the output of `monitoring_costs.R``
gdrive_download( 
  local_path = "source_data/cost_params_2025.Rdata", 
  gdrive_dribble = gdrive_set_dribble("Projects/ADP/Monitoring Costs - CONFIDENTIAL/")
)
(load("source_data/cost_params_2025.Rdata"))

#' Using `fg_em`, add the number of fixed-gear EM vessels to the `cost_params` list
cost_params$EMFG$emfg_v <- uniqueN(fg_em$PERMIT)

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

format_dollar <- function(x, digits) paste0("$", formatC(x, digits = digits, big.mark = ",", format = "f"))

# Setting for flextable outputs
set_flextable_defaults(font.family = "Times New Roman", font.size = 10) 

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
budget_lst <- list(4.4e6)   #' *2024 Draft ADP Budget*

#======================================================================================================================#
# Sampling Rates ----
#======================================================================================================================#

#' [DRAFT:] No bootstrap is performed, simply sampling all trips in `pc_effort_st` 1 time.
#' [FINAL: TODO]  Use the outputs of effort_prediction.R, specifically 'effort_strata[ADP == 2024]', to predict 
#' the total number of trips to sample for each stratum

#=================================#
## Partial Coverage Allocation ----
#=================================#

sample_N <- pc_effort_st[, .(N = uniqueN(TRIP_ID)), keyby = .(STRATA)]
boot_lst <- bootstrap_allo(pc_effort_st, sample_N, box_params, cost_params, budget_lst, bootstrap_iter = 1)

#' TODO Save the outputs of the bootstrapping and allocation
if(F) save(boot_lst, file = "results/swor_boot_lst.rdata")

# Extract the results from each iteration
boot_dt <- rbindlist(lapply(boot_lst, "[[", "rates"), idcol = "BOOT_ITER")

# If bootstrapping multiple populations, view the distribution of allocated rates
if(uniqueN(boot_dt$BOOT_ITER) > 1) {
  
  ggplot(boot_dt, aes(x = STRATA, y = SAMPLE_RATE)) + geom_violin(draw_quantiles = 0.5) + 
    facet_wrap(.~ STRATA, scales = "free") + stat_summary(geom = "point", fun = mean)
  
  ggplot(boot_dt, aes(x = STRATA, y = SAMPLE_RATE)) + geom_violin(draw_quantiles = 0.5) + 
    stat_summary(geom = "point", fun = mean) + labs(x = "Stratum", y = "Sample Rate") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + geom_hline(yintercept = 0)
}

# Calculate the rates to use as the mean across iterations. Also calculate average proximity (PROX) and cv_scaling metrics
rates_adp <- boot_dt[, lapply(.SD, mean), .SDcols = c("SAMPLE_RATE", "n", "PROX", "CV_SCALING", "INDEX"), keyby = .(ADP, STRATA, STRATA_N)]


#===========================#
## Full Coverage Summary ---- 
#===========================#

# Generate full coverage summary so summary tables can reference it
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

# Set number of bootstrapping simulations for cost estimtes
cost_boot_iter <- 10000
# Initialize results list
cost_boot_lst <- vector(mode = "list", length = cost_boot_iter)
# Identify trips and their strata
cost_boot_trips_dt <- unique(pc_effort_st[, .(ADP, STRATA, TRIP_ID)])

#' Hard-code the EM_TRW sampling rate, which is 1/3
rates_em_trw_goa <- cost_boot_trips_dt[STRATA == "EM_TRW-GOA", .(STRATA_N = uniqueN(TRIP_ID)), keyby = .(ADP, STRATA)]
cost_boot_rates <- rbind(rates_adp, rates_em_trw_goa, fill = T)
cost_boot_rates[STRATA == "EM_TRW-GOA", SAMPLE_RATE := 0.3333]
cost_boot_rates[, MON_RATE := SAMPLE_RATE]

# Begin trip simulation
set.seed(12345)
for(i in seq_len(cost_boot_iter)) {
  
  if(i == 1) cat(paste0(cost_boot_iter, " iterations:\n"))
  if( (i %% (cost_boot_iter/10)) == 0 ) cat(paste0(i, ", "))
  
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
    ob_cost_new(cost_boot_rates, cost_params = cost_params, allo_sub = cost_boot_trips_dt.n, sim = T),
    emfg_cost(em_fg_d, cost_params = cost_params, sim = T),
    emtrw_cost_new(cost_boot_trips_dt.n, cost_params = cost_params)
  )
  
}
cost_boot_dt <- rbindlist(cost_boot_lst, idcol = "iter")
cost_boot_dt.med <- cost_boot_dt[, median(OB_TOTAL + EMFG_TOTAL + EMTRW_TOTAL)]
cost_boot_dt.95 <- cost_boot_dt[, list(quantile(OB_TOTAL + EMFG_TOTAL + EMTRW_TOTAL, c(0.025, .975)))]

#====================================#
## Figure B-2. Cost Distribution  ----
#====================================#
figure_b2 <- ggplot(cost_boot_dt, aes(x = OB_TOTAL + EMFG_TOTAL + EMTRW_TOTAL)) + 
  geom_histogram(fill = "gray65", bins = 30) + 
  geom_vline(aes(xintercept = budget_lst[[1]]), color = "purple", linetype = 2, linewidth = 1.1) +
  geom_text(
    x = budget_lst[[1]], y = cost_boot_iter / 20, label = paste("Budget:", format_dollar(budget_lst[[1]], 0)), 
    angle = 90, hjust = 1, vjust = 2, color = "purple", check_overlap = T) + 
  geom_vline(aes(xintercept = cost_boot_dt.med), color = "blue", linetype = 2, linewidth = 1) + 
  geom_text(
    x = cost_boot_dt.med, y = cost_boot_iter / 20, label = paste("Median:", format_dollar(cost_boot_dt.med, 0)), 
    angle = 90, hjust = 1, vjust = -1, color = "blue", check_overlap = T) + 
  geom_vline(data = cost_boot_dt.95, aes(xintercept = V1), color = "red", linetype = 2, linewidth = 1) + 
  geom_text(
    data = cost_boot_dt.95, aes(x = V1, y = cost_boot_iter / 20, label = format_dollar(V1, 0)), 
    angle = 90, hjust = 1, vjust = c(1.8, -1), color = "red") + 
  scale_x_continuous(labels = function(x) formatC(x / 1e6, digits = 1, format = "f")) + 
  labs(x = "Total cost (millions)", y = "Number of ODDS iterations with this outcome") +
  theme(axis.text = element_text(color = "black")) # Make text black so it's easier to read

# Note that this includes the cost of EM_TRW which in this simulation, had no variability.

#======================================================================================================================#
# Monitoring Costs ----
#======================================================================================================================#

# Extract the cost summary used in the allocation.
#' [DRAFT: Grabbing attributes from the only iteration]
cost_summary <- attr(boot_lst[[1]]$rates, "cost_summary")
#' [FINAL: Average across attributes from all iterations]

#===================================#
## Table B-1. Budget and Vessels ----
#===================================#

#' Cost summary

# Get totals, rounding to the nearest $1K
cost_totals <- round(
  unlist(cost_summary[, .(TOTAL = sum(OB_TOTAL, EMFG_TOTAL, EMTRW_TOTAL), OB_TOTAL, EMFG_TOTAL, EMTRW_TOTAL)]), -3
)
#' Naming the monitoring method 'strata' here for the sake of keeping the same header as the vessel counts below. The 
#' final table's column names will be named manually
cost_totals.pc <- data.table("STRATA" = names(cost_totals), data.table("value" = format_dollar(cost_totals, 0)))
# Rename the pools
cost_totals.pc[, STRATA := fcase(
  STRATA == "TOTAL", "Total",
  STRATA == "OB_TOTAL", "At-sea Observer",
  STRATA == "EMFG_TOTAL", "EM Fixed-Gear",
  STRATA == "EMTRW_TOTAL", "EM Trawl GOA"
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
  STRATA == "EM_TRW_BSAI", "EM Trawl BSAI",
  STRATA == "FULL", "Full Coverage"
)]
setorder(vessel_totals.fc, -STRATA)
vessel_totals.fc <- rbind(data.table(STRATA = "Vessels Participating (Full Coverage)"), vessel_totals.fc, fill = T)

table_b1 <- rbind(cost_totals.pc, vessel_totals.pc, vessel_totals.fc)
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
setnames(rates_trips_days.pc, new = c("Stratum", "r", "n", "Pool", "N", "d"))
setcolorder(rates_trips_days.pc, c("Pool", "Stratum", "N", "n", "d", "r"))
# Get list of strata 
rates_trips_days.pc.strata <- unique(rates_trips_days.pc$Stratum)
# Create totals by POOL when there is more than one stratum
rates_trips_days.pc.total <- rates_trips_days.pc[, .(
  Stratum = "Total", N = sum(N), n = sum(n), d = sum(d), r = round(sum(n)/sum(N) * 100, 2), STRATA_COUNT = .N
), keyby = .(Pool)][STRATA_COUNT > 1][, STRATA_COUNT := NULL]
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
  STRATA == "EM_TRW_BSAI", "EM Trawl BSAI",
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

# Now that totals are made for pool, remove pool column
table_b3.flex <- table_b3[, -"Pool"] %>% 
  flextable() %>%
  autofit() %>%
  compose(i = 1, j = 1, part = "header", value = as_paragraph(., " (", as_i("h"), ")" ), use_dot = T) %>%
  compose(i = 1, j = 2:4, part = "header", value = as_paragraph(as_i(.), as_sub(as_i("h") )), use_dot = T) %>%
  compose(i = 1, j = 5, part = "header", value = as_paragraph(as_i(.), as_sub(as_i("h") ), " (%)"), use_dot = T) %>%
  bold(i = ~ Stratum == "Total", part = "body") %>%
  bold(i = c(9,10), j = 2:5, part = "body") %>%
  hline(i = ~ Stratum == "Total") %>%
  hline(i = c(9, 10)) %>%
  compose(i = ~ Stratum == "Total", j = 1, value = as_paragraph("\t", .), use_dot = T) %>%
  add_header_row(top = F, values = c(paste0("Draft ", adp_year, " ADP"), ""), colwidths = c(1, 4)) %>%
  bold(i = 2, j = 1, part = "header") %>%
  fix_border_issues()
# Note that the rates for EM_TRW-GOA are for shoreside monitoring only and that 'n' is an approximation


#======================================================================================================================#
# Monitoring Summary ----
#======================================================================================================================#

#' Here we will summarize the monitoring of the entire AK 

# Grab all full coverage trips in the last year
coverage_summary.fc <- work.data[, -c("STRATA")][full_efrt.smry, on = .(VESSEL_ID, TRIP_ID)]
coverage_summary.fc[, FIXED_TRW := ifelse(AGENCY_GEAR_CODE %in% c("PTR", "NPT"), "TRW", "FIXED")]
coverage_summary.fc <- coverage_summary.fc[, .(
  RET_CATCH = sum(WEIGHT_POSTED[SOURCE_TABLE == "Y"], na.rm = T)
  ), keyby = .(VESSEL_ID, TRIP_ID, FMP, FIXED_TRW, MANAGEMENT_PROGRAM_CODE, STRATA)]
coverage_summary.fc[, N := .N, by =.(TRIP_ID)]

# Calculate proportion of catch retained under each managemnt program code
coverage_summary.fc[, PROP := RET_CATCH / sum(RET_CATCH), by = .(TRIP_ID)]
coverage_summary.fc[PROP != 1]
coverage_summary.fc[STRATA == "EM_TRW_BSAI", MANAGEMENT_PROGRAM_CODE := "AFA/EM_TRW"]

# Full coverage total
ggplot(coverage_summary.fc, aes(x = FIXED_TRW, y = RET_CATCH, fill = MANAGEMENT_PROGRAM_CODE)) + 
  facet_grid(FMP ~ .) + 
  geom_col()


coverage_summary.pc <- pc_effort_sub[COVERAGE_TYPE == "PARTIAL"]
# Remove state-managed fisheries
coverage_summary.pc <- coverage_summary.pc[!(MANAGEMENT_PROGRAM_CODE %in% c("SMO", "SMPC", "SMS"))]

coverage_summary.pc[, FIXED_TRW := ifelse(AGENCY_GEAR_CODE %in% c("PTR", "NPT"), "TRW", "FIXED")]
coverage_summary.pc <- coverage_summary.pc[, .(
  RET_CATCH = sum(WEIGHT_POSTED[SOURCE_TABLE == "Y"], na.rm = T)
), keyby = .(VESSEL_ID, TRIP_ID, FMP, FIXED_TRW, MANAGEMENT_PROGRAM_CODE, STRATA)]
coverage_summary.pc[, N := .N, by =.(TRIP_ID)]
coverage_summary.pc[, PROP := RET_CATCH / sum(RET_CATCH), by = .(TRIP_ID)]
coverage_summary.pc[PROP != 1]
coverage_summary.pc[STRATA == "EM_TRW_GOA", MANAGEMENT_PROGRAM_CODE := "AFA/EM_TRW"]
ggplot(coverage_summary.pc, aes(x = FIXED_TRW, y = RET_CATCH, fill = MANAGEMENT_PROGRAM_CODE)) + 
  facet_grid(FMP ~ .) + 
  geom_col()

coverage_summary <- rbind(
  cbind(SECTOR = "FULL", coverage_summary.fc),
  cbind(SECTOR = "PARTIAL", coverage_summary.pc)
)
coverage_summary.total <- coverage_summary[, .(
  RET_CATCH = sum(RET_CATCH)
), keyby = .(SECTOR, FMP, FIXED_TRW, MANAGEMENT_PROGRAM_CODE)
]

ggplot(coverage_summary.total , aes(x = FIXED_TRW, y = RET_CATCH, fill = MANAGEMENT_PROGRAM_CODE)) + 
  facet_grid(FMP ~ SECTOR, scales = "free_y", space = "free_y") + 
  geom_col(color = "black")

## Proportions monitored by sector and FMP ----
coverage_summary.pc.strata <- unique(pc_effort_st[, .(wd_TRIP_ID, STRATA)])
coverage_summary.pc.strata <- work.data[, -"STRATA"][coverage_summary.pc.strata, on = c(TRIP_ID = "wd_TRIP_ID")]
# Remove state-managed fisheries
coverage_summary.pc.strata <- coverage_summary.pc.strata[!(MANAGEMENT_PROGRAM_CODE %in% c("SMO", "SMPC", "SMS"))]

coverage_summary.pc.strata[, FIXED_TRW := ifelse(AGENCY_GEAR_CODE %in% c("PTR", "NPT"), "TRW", "FIXED")]
coverage_summary.pc.strata <- coverage_summary.pc.strata[, .(
  RET_CATCH = sum(WEIGHT_POSTED[SOURCE_TABLE == "Y"], na.rm = T)
), keyby = .(VESSEL_ID, TRIP_ID, FMP, FIXED_TRW, MANAGEMENT_PROGRAM_CODE, STRATA)]
# Merge partial coverage sampling rates in
coverage_summary.pc.strata[, SAMPLE_RATE := rates_adp[coverage_summary.pc.strata, SAMPLE_RATE, on = .(STRATA)]]
coverage_summary.pc.strata[STRATA == "ZERO", SAMPLE_RATE := 0]
coverage_summary.pc.strata[STRATA == "EM_TRW-GOA", SAMPLE_RATE := 1]
a1 <- coverage_summary.pc.strata[, .(RET_CATCH = sum(RET_CATCH), MON_CATCH = sum(RET_CATCH * SAMPLE_RATE)), keyby = .(FMP, FIXED_TRW)]
a1[, PERC_MON  := MON_CATCH / RET_CATCH * 100]
a1  # 76% of GOA TRW effort is expected to be monitored


a2 <- coverage_summary.fc[, .(RET_CATCH = sum(RET_CATCH), MON_CATCH = sum(RET_CATCH)), keyby = .(FMP, FIXED_TRW)]
a2[, PERC_MON  := MON_CATCH / RET_CATCH * 100]

a3 <- rbind(
  cbind(SECTOR = "PARTIAL", a1),
  cbind(SECTOR = "FULL", a2)
)
setorder(a3, SECTOR, FMP, FIXED_TRW)
a3[, TOTAL_PERC := sum(MON_CATCH) / sum(RET_CATCH) * 100]  # 96% of tonnage monitored
a3[, FMP_PERC := sum(MON_CATCH) / sum(RET_CATCH) * 100, by = .(FMP)]
a3[, FMP_GEAR_PERC :=  sum(MON_CATCH) / sum(RET_CATCH) * 100, by = .(FMP, FIXED_TRW)]
a3[, UNMON_CATCH := RET_CATCH - MON_CATCH]

a4 <- melt(a3, id.vars = c("SECTOR", "FMP", "FIXED_TRW"), measure.vars = c("UNMON_CATCH", "MON_CATCH"))
ggplot(a4, aes(x = SECTOR, y = value, fill = variable)) + 
  facet_grid(FMP ~ FIXED_TRW, scales = "free_y", space = "free_y") + 
  geom_col(color = "black") + 
  theme(legend.position = "bottom")

#install.packages("waffle")
library(waffle)


a4[, value2 := value/sum(value)*400]  # with 500, each box represents .25% of catch
a4[, value3 := round(value2)]
a4[, GROUP := fcase(
  SECTOR == "FULL" & variable == "MON_CATCH", "Full coverage",
  SECTOR == "PARTIAL" & variable == "UNMON_CATCH", "Unmonitored",
  SECTOR == "PARTIAL" & variable == "MON_CATCH", "Partial coverage"
)]
# TODO Add EM flag to this?
a4[, GROUP := factor(GROUP, levels = c("Full coverage", "Partial coverage", "Unmonitored"))]
a4[, FMP := factor(FMP, levels = c("BSAI", "GOA"))]
a4[, FIXED_TRW := factor(FIXED_TRW, levels = c("TRW", "FIXED"))]
setorder(a4, GROUP, FMP, FIXED_TRW)
ggplot(a4, aes(values = value3, fill = GROUP)) + 
  facet_grid(. ~ FMP + FIXED_TRW, scales = "free_x", space = "free_x") + 
  geom_waffle(color = "white", n_rows = 20) +   #' with 20 rows and 400 units (each box = 0.25%), makes each full column represent 20/400 = 5% or 20 * .25% = 5%
  theme_minimal() + 
  theme_enhance_waffle() +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("blue", "dodgerblue", "pink")) + 
  labs(fill = "Expected monitoring")
# NOTE If I split up this figure into portions monitored by EM, GOA-FIXED would have its one box of partial monitoring split by both observers and EM.
# remember, these summaries are by tonnage!



#' TODO Try to add trips in here. Split trips by gear/FMP based on retained catch.
coverage_summary.fc[, MON := ifelse(STRATA %like% "EM_", "EM", "OB")]
mon_catch.fc <- coverage_summary.fc[, .(RET_CATCH = sum(RET_CATCH), MON_CATCH = sum(RET_CATCH)), keyby = .(MON, FMP, FIXED_TRW)]
mon_catch.fc[, PERC_MON  := MON_CATCH / RET_CATCH * 100]

coverage_summary.pc.strata[, MON := fcase(
  STRATA %like% "EM", "EM",
  STRATA %like% "OB", "OB",
  STRATA %like% "ZERO", "ZERO"
)]
mon_catch.pc <- coverage_summary.pc.strata[, .(RET_CATCH = sum(RET_CATCH), MON_CATCH = sum(RET_CATCH * SAMPLE_RATE)), keyby = .(MON, FMP, FIXED_TRW)]
mon_catch.pc[, PERC_MON  := MON_CATCH / RET_CATCH * 100]

a3 <- rbind(
  cbind(SECTOR = "FULL", mon_catch.fc),
  cbind(SECTOR = "PARTIAL", mon_catch.pc)
)

setorder(a3, SECTOR, FMP, FIXED_TRW, MON)
# a3[, TOTAL_PERC := sum(MON_CATCH) / sum(RET_CATCH) * 100]  # 96% of tonnage monitored
# a3[, FMP_PERC := sum(MON_CATCH) / sum(RET_CATCH) * 100, by = .(FMP)]
# a3[, FMP_GEAR_PERC :=  sum(MON_CATCH) / sum(RET_CATCH) * 100, by = .(FMP, FIXED_TRW)]
a3[, UNMON_CATCH := RET_CATCH - MON_CATCH]
a4 <- melt(a3, id.vars = c("SECTOR", "FMP", "FIXED_TRW", "MON"), measure.vars = c("UNMON_CATCH", "MON_CATCH"))

a4[, value2 := value/sum(value)*800]  # with 500, each box represents .25% of catch
a4[, value3 := round(value2)]
a4[, GROUP := fcase(
  SECTOR == "FULL" & variable == "MON_CATCH" & MON == "OB", "Full coverage - Observer",
  SECTOR == "FULL" & variable == "MON_CATCH" & MON == "EM", "Full coverage - EM",
  SECTOR == "PARTIAL" & MON == "ZERO", "No selection",
  SECTOR == "PARTIAL" & variable == "UNMON_CATCH", "Partial coverage - Unmonitored",
  SECTOR == "PARTIAL" & variable == "MON_CATCH" & MON == "OB", "Partial coverage - Observer",
  SECTOR == "PARTIAL" & variable == "MON_CATCH" & MON == "EM", "Partial coverage - EM"
)]
a4[, FIXED_TRW := ifelse(FIXED_TRW == "FIXED", "Fixed", "Trawl")]
a4[, GROUP := factor(GROUP, levels = c(
  "Full coverage - Observer", "Full coverage - EM",
  "Partial coverage - Observer", "Partial coverage - EM", "Partial coverage - Unmonitored", "No selection"))]
a4[, FMP := factor(FMP, levels = c("BSAI", "GOA"))]
a4[, FIXED_TRW := factor(FIXED_TRW, levels = c("Trawl", "Fixed"))]
setorder(a4, GROUP, FMP, FIXED_TRW)

library(ggh4x)
figure_ppt_monitoring_summary <- ggplot(a4, aes(values = value3, fill = GROUP)) + 
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

ggsave("output_figures/figure_ppt_monitoring_summary.png" , figure_ppt_monitoring_summary, width = 8, height = 5, units = "in")

# Proportion of tonnage monitored
a4[, sum(value[variable == "MON_CATCH"]) / sum(value)] # 96% of fisheries are monitored
a4[, sum(value[variable == "MON_CATCH"]) / sum(value), keyby = .(FMP)] # 99% in the BSAI and 72% in the GOA
a4[, sum(value), keyby = .(SECTOR)][, .(SECTOR, Tonnage = V1, Percent = 100 * V1 / sum(V1))] # 10% of fisheries is in partial coverage

# Excluding EM TRW, we are monitoring 13% of tonnage in partial coverage
a4[!(FIXED_TRW != "TRW" & MON == "EM") & SECTOR == "PARTIAL", sum(value[variable == "MON_CATCH"]) / sum(value)]

a4[, .(PERC_MON = 100 * sum(value[variable == "MON_CATCH"]) / sum(value)), keyby = .(FMP, FIXED_TRW)]
a4[SECTOR == "PARTIAL", .(PERC_MON = 100 * sum(value[variable == "MON_CATCH"]) / sum(value)), keyby = .(FMP, FIXED_TRW)]

coverage_summary.fc[FMP == "BSAI" & FIXED_TRW == "FIXED"]

#' TODO Can you make the same figure but make it number of trips?


#' *======================================*

# Most trips with multiple records have multiple management program codes (e.g., both CDQ and IFQ, both CDQ and OA)
coverage_summary.fc[N > 1, table(MANAGEMENT_PROGRAM_CODE)]

#' [NOTE: this summary has more N than because trips are being split by management program code, gear, etc]
coverage_summary.fc[, .(N = uniqueN(TRIP_ID), RET_CATCH = sum(RET_CATCH)), keyby = .(FMP, GEAR, MANAGEMENT_PROGRAM_CODE, PROCESSING_SECTOR, STRATA)]




coverage_summary.fc[, .(N = uniqueN(TRIP_ID), RET_CATCH = sum(RET_CATCH)), keyby = .(FMP, FIXED_TRW, MANAGEMENT_PROGRAM_CODE, STRATA)]

# For each Trip, compute percentage of retained catch in each Management Program Code
coverage_summary.fc[, .(PROP = RET_CATCH / sum(RET_CATCH)), keyby = .(TRIP_ID)]


# Virtually all BSAI Fixed gear effort is CP cod fishing


# Can mix RPP with OA
coverage_summary.fc[TRIP_ID %in% coverage_summary.fc[N > 1 & MANAGEMENT_PROGRAM_CODE == "RPP", TRIP_ID]]  




# Can Mix AFA with PCTC and CDW and A80
coverage_summary.fc[TRIP_ID %in% coverage_summary.fc[N > 1 & MANAGEMENT_PROGRAM_CODE == "AFA", TRIP_ID], table(MANAGEMENT_PROGRAM_CODE)]  

coverage_summary.fc[TRIP_ID %in% coverage_summary.fc[N > 1 & MANAGEMENT_PROGRAM_CODE == "A80", TRIP_ID], table(MANAGEMENT_PROGRAM_CODE)]  

coverage_summary.fc[N == 1 & MANAGEMENT_PROGRAM_CODE == "OA", table(GEAR)]

coverage_summary.fc[N == 1 & MANAGEMENT_PROGRAM_CODE == "OA" & GEAR == "TRW"]
work.data[TRIP_ID == 2406523, table(TRIP_TARGET_CODE)]
work.data[TRIP_ID == 2406528, table(TRIP_TARGET_CODE)]
work.data[TRIP_ID == 2654541, table(TRIP_TARGET_CODE)]
work.data[TRIP_ID == 2756214, table(TRIP_TARGET_CODE)]

#======================================================================================================================#
# Outputs ----
#======================================================================================================================#

#' *Allocation Objects*
#' Upload all allocation inputs and outputs to the shared google drive.
save(
  ## Allocation inputs
  pc_effort_st, budget_lst, box_params, 
  ## Allocation products
  boot_lst, rates_adp,      
  cost_summary, cost_boot_dt, 
  table_b1, table_b2, table_b3,
  file = paste0("results/draft_adp_", adp_year, "_results.rdata")
)  # was 297mb, box_params is the largest thing thank to the spatial data? weird..

#' *Tables*
#' Save table outputs for `tables.Rmd` to knit to docx format
save(
  table_b1, table_b1.flex,
  table_b2, table_b2.flex,
  table_b3, table_b3.flex,
  file = "tables.rdata"
)

#' *Figures* 

#' *Figure B-2.* Summary of `cost_boot_iter` outcomes of simulated sampling in ODDS showing the total costs of the 
#' partial coverage monitoring program expected for 2025. Vertical lines depict the available budget (purple line), 
#' median expected cost (blue line), and 95% confidence limits (red lines).
ggsave(filename = "output_figures/figure_b2.png", plot = figure_b2, width = 5, height = 5, units = "in")
