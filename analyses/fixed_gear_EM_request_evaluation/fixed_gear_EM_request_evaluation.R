# Fixed-gear EM Requesting Vessels Evaluation
# Author : Geoffrey Mayhew
# Date : 2023-Nov-02

# This script reviews all vessels requesting to join the fixed-gear EM vessel pool. It ranks requesting vessels in terms
# of their potential for producing data gaps by virtue of moving from the observer pool to the EM pool, as wells as the 
# cost-efficiency of each vessel in terms of how many trips they have taken in the past. 

#======================================================================================================================#
# Data Prep ----
#======================================================================================================================#

#===================#
## Load packages ----
#===================#

library(data.table)
library(ggplot2)
library(readxl)      # For reading xlsx spreadsheets
library(gridExtra)
library(magrittr)    # For Piping
library(sf)          # For spatial statistics
library(ggh4x)       # For facet_nested

#===============#
## Load Data ----
#===============#

# Load spreadsheet from Glenn Campbell (sent 2023-Nov-1)
# https://docs.google.com/spreadsheets/d/1CB0UJ63f7sQsyubzrRS_6ZNTyO682svB/edit?usp=share_link&ouid=112928343270640187258&rtpof=true&sd=true
fgem_request_dt <- setDT(read_xlsx("source_data/EM_fixed_gear_requests.xlsx"))
fgem_opt_in <- fgem_request_dt[`Type of Request` == "Request EM for 2024"]
fgem_opt_out <- fgem_request_dt[`Type of Request` == "Opt out of EM for 2024"]

# Load prepped data for 2024 Final ADP
# https://drive.google.com/file/d/1Iet_Fh_8u06UcGwCrZGGWTARpvAAjzky/view?usp=share_link
# TODO data_prep.R has not yet been run on the 2023-Nov-03 version of VALHALLA
load("analyses/allocation_evaluation/data_prep_final.rdata")

# Prepare trips_melt object
# trips_melt has metrics and trip duration for all trips >= 2015. . 
# Add ADP year, STRATA, and DAYS of each TRIP_ID to trips_melt
trips_melt_add_data <- unique(pc_effort_dt[STRATA != "ZERO", .(ADP, TRIP_ID, STRATA, DAYS)])
trips_melt <- trips_melt_add_data[trips_melt, on = .(TRIP_ID)]
setcolorder(trips_melt, c("ADP", "STRATA", "TRIP_ID", "DAYS", "Metric", "Value"))
setkey(trips_melt, ADP, STRATA, TRIP_ID, DAYS, Metric, Value)
# Error-check
if(nrow(trips_melt[, .N, by = .(TRIP_ID)][N != uniqueN(trips_melt$Metric)])) stop("At least one trip has number of metrics != 3!")
# Remove records missing data
trips_melt <- trips_melt[!is.na(ADP)]

# Load the ADFG statistical area shapefile. '../' is needed for Rmarkdown to climb into parent folders.
stat_area_sf <- st_read(
  dsn = "source_data/ADFG_Stat_Area_shapefile/PVG_Statewide_2001_Present_GCS_WGS1984.shp", quiet = T) %>%
  select(STAT_AREA) %>%
  st_transform(crs = 3467)

# Load the Alaska map sf objects
load("source_data/ak_shp.rdata")      # shp_land, shp_nmfs, and shp_centroids added to global

# Load VALHALLA for fishing histories
load("source_data/2024_Final_ADP_data.rdata")

#====================#
## Load Functions ----
#====================#

# FIXME using a version of functions.R that is on the repo under effort_prediction_explroation branch
source("C:/Users/geoff.mayhew/Desktop/2024_adp_functions_temp.R", echo = FALSE)
# source("analyses/allocation_evaluation/functions.R")

#======================================================================================================================#
# Fixed Gear EM Evaluation ----
#======================================================================================================================#

## Fishing History
fishing_history <- unique(work.data[
  VESSEL_ID %in% fgem_opt_in$`Vessel Permit/Vessel ID`,
  .(VESSEL_ID = as.numeric(VESSEL_ID), ADP, TRIP_ID, STRATA, STRATA_NEW, COVERAGE_TYPE, CVG_NEW, TRIP_TARGET_CODE, REPORTING_AREA_CODE, AGENCY_GEAR_CODE)])
table(fishing_history$STRATA_NEW)  # According to 2024 stratum definitions, all fish in the GOA, some fished zero coverage, one as TRW_GOA??

# Any vessels with no fishing history?
setdiff( fgem_opt_in$`Vessel Permit/Vessel ID`, unique(fishing_history$VESSEL_ID))
# 5735 has no fishing history since 2013!

fishing_history[STRATA_NEW == "TRW_GOA"] 
# VESSEL_ID 2348, ALEUT_MISTRESS, fished a TRW trip in 2017?
work.data[VESSEL_ID == 2348 & AGENCY_GEAR_CODE == "NPT"] # TRIP_ID == 4844370, fished one GOA COD tender trip out of King Cove

# Exclude any trips that wouldn't be counted under fixed-gear EM
fishing_history <- fishing_history[(STRATA_NEW %like% c("FIXED_GOA"))]

# Count trips by each vessel and year that would fall under fixed-gear EM
fishing_history_years <- fishing_history[order(ADP), unique(ADP)]
fishing_history_N <- dcast(fishing_history[, .(N = uniqueN(TRIP_ID)), keyby = .(VESSEL_ID, ADP)], VESSEL_ID ~ ADP, value.var = "N", fill = 0)
fishing_history_N[, TOTAL := rowSums(fishing_history_N[, -"VESSEL_ID"])]
fishing_history_N[, EARLIEST := fishing_history_years[min(which(lapply(.SD, function(x) x > 0) == T))], by = VESSEL_ID, .SDcols = as.character(fishing_history_years)]
fishing_history_N[, LATEST := fishing_history_years[max(which(lapply(.SD, function(x) x > 0) == T))], by = VESSEL_ID, .SDcols = as.character(fishing_history_years)]
fishing_history_N[, VESSEL_NAME := fgem_opt_in[fishing_history_N, `Vessel Name`, on = c(`Vessel Permit/Vessel ID` = "VESSEL_ID")]]
fishing_history_N[, LOA := fgem_opt_in[fishing_history_N, `Vessel Length`, on = c(`Vessel Permit/Vessel ID` = "VESSEL_ID")]]
fishing_history_N[, TPY := round(TOTAL / ((2023 - EARLIEST) + 1), 3)]
# Only 0 Vessels fall under this definition, with a fishing history dating back to 2013
fishing_history_N[order(-TOTAL)]
### 3 Vessels only have a history dating back to 2022 at earliest
fishing_history_N[EARLIEST == 2022]  # Both vessels fish at between 4 to 16.5 trips per year
### 2 vessels didn't fish last year, ALEUT MISTRESS (hasn't fished since either 2014)and THREE_PEARLS (hasnt fished since 2019), , and never took more than 1 trip per year
fishing_history_N[LATEST != 2023]

# Several vessels have no fishing history that would count under fixed-gear EM
no_history <- setdiff(fgem_opt_in$`Vessel Permit/Vessel ID`, fishing_history$VESSEL_ID)  
fgem_opt_in[`Vessel Permit/Vessel ID` %in% no_history]  
# CRYSTSAL STAR, EMERALD ISLE, and THREE PERALS have no fishing history aside from ZERO 

# These vessels are listed as 39 ft length overall in VALHALLA/work.data
unique(work.data[VESSEL_ID %in% no_history, .(VESSEL_ID, TRIP_ID, ADP, STRATA, STRATA_NEW, MANAGEMENT_PROGRAM_CODE, LENGTH_OVERALL)])[order(VESSEL_ID)]
fgem_opt_in[`Vessel Permit/Vessel ID` %in% no_history]  # but have lengths of 41, 48, and 44 in the spreadsheet?

#======================================================================================================================#
# Evaluate Interspersion ---- 
#======================================================================================================================#

# -- Assume selection rates of 15% for all strata. Althoug we 'move' vessels from OB to EM, we don't calculate the 
# change to the afforded selection rates from proximity allocation (e.g., OB rates would go up and FG-EM would go down, presumably)
# -- pc_effort_dt is only made for 2015 at earliest

# Redefine STRATA combining HAL and POT into FIXED
fixed.pc_effort <- unique(copy(pc_effort_dt)[STRATA %like% "HAL|POT", STRATA := paste0(POOL, "_", "FIXED")])

# FIXME for some very early years, BSAI_GOA has AI and BS separated
fixed.pc_effort <- unique(fixed.pc_effort[BSAI_GOA %in% c("AI", "BS"), BSAI_GOA := "BSAI"])

# FIXME Also, haven't re-run data_prep.R, so the vessels that were originally labeled as ZERO due to LOA discrepancy need to be converted
fixed.pc_effort[PERMIT %in% c(33881, 35836, 3735) & STRATA == "ZERO", STRATA := "OB_FIXED"]

# Initialize data.table with 15% selection rates for all strata
selection_15 <- unique(fixed.pc_effort[STRATA != "ZERO", .(ADP, STRATA, BSAI_GOA)])[
][, STRATUM_COL := paste0(STRATA, "-", BSAI_GOA)
][, SAMPLE_RATE := 0.15][]

# First, move all vessels opting OUT of fixed-gear EM back into the observer pool
dcast(
  fixed.pc_effort[PERMIT %in% fgem_opt_out$`Vessel Permit/Vessel ID`, .(N = uniqueN(TRIP_ID)), keyby = .(PERMIT, ADP, STRATA, BSAI_GOA)],
  PERMIT + STRATA + BSAI_GOA ~ ADP, value.var = "N", fill = 0) 
# 3297 actually fished a fair amount, probably was a reasonably cost-effective fixed-gear EM vessel?
# 792 fished somewhat consistently, actually fished 10 trips in 2022 but only 1 so far in 2013.
# 3102 hasn't fished since 2015. 32413 hasn't fished since 2018.
fixed.pc_effort[PERMIT %in% fgem_opt_out$`Vessel Permit/Vessel ID`, ':=' (POOL = "OB", STRATA = gsub("EM_", "OB_", STRATA))]

#=============#
## Initial ----
#=============#

# Specify years of fishing effort to include in review (more takes longer!)
# year_vec <- 2021:2023    # for testing (3 years)
year_vec <- 2018:2023  # for full run (6 years)

# Make things faster by cropping out all TRW trips
system.time(init.box <- define_boxes_3(
  fixed.pc_effort[ADP %in% year_vec & !(STRATA %like% "TRW")], space = c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), dmn_cols = c("GEAR"), geom = T ))

# Define acceptor/donor lists for interspersion 
# init.box$dmn$strata_dt
fixed_fmp.OB.acceptor_donor_lst <- c(
  rep(list(3:4), times = 2),                # 1-2:   EM Fixed Gear
  rep(list(3:4), times = 2),                # 3-4:   OB Fixed Gear
  rep(list(3:4), times = 2)                 # 5-6:   ZERO           
)
fixed_fmp.nonOB.acceptor_donor_lst <- c(
  rep(list(1:2), times = 2),               # 1-2: Fixed-gear EM to itself
  rep(list(NULL), times = 4)               # 3-6: No other donors
)

init.ispn <- dmn_interspersion_figs(
  box_def = init.box,
  selection_rates = selection_15,
  ob_adl = fixed_fmp.OB.acceptor_donor_lst,
  nonob_adl = fixed_fmp.nonOB.acceptor_donor_lst
)

# Results separated by year, pool, gear type, and optionally, BSAI_GOA
init.ispn$DMN_INSP_OB_SMRY$OVERALL     # for OB interspersion to OB, EM, and ZERO
init.ispn$DMN_INSP_OB_SMRY$BSAI_GOA
init.ispn$DMN_INSP_NONOB_SMRY$OVERALL  # for fixed-gear EM interspersion, probably don't care about this as much
init.ispn$DMN_INSP_NONOB_SMRY$BSAI_GOA

#==============================#
## Loop through each vessel ----
#==============================#

fgem_vessel_vec <- fgem_opt_in$`Vessel Permit/Vessel ID`
prior.effort <- copy(fixed.pc_effort[ADP %in% year_vec  & !(STRATA %like% "TRW")])
prior.ispn <- copy(init.ispn)
add_fgem_vec <- c()
fgem_eval_vec <- list()

while(length(fgem_vessel_vec) > 0) {
  
  # Count the number of OB_FIXED trips by each vessel and year
  trip_counts <- prior.effort[PERMIT %in% fgem_vessel_vec & STRATA %like% "OB_FIXED", .(N = uniqueN(TRIP_ID)), keyby = .(PERMIT, ADP)]
  
  # identify vessels with no fishing history
  no_fishing_history <- setdiff(fgem_vessel_vec, unique(trip_counts$PERMIT))
  if( length(no_fishing_history) > 0 ) {
    cat("Vessels", paste0(no_fishing_history, collapse = ", "), "have no fishing history and will be dropped.\n\n")
    fgem_vessel_vec <- setdiff(fgem_vessel_vec, no_fishing_history)
  }
  
  # Initialize results list
  loop_res <- vector(mode = "list", length = length(fgem_vessel_vec))
  
  # For each vessel in the vector requesting EM that hasn't been added
  for(i in seq_along(fgem_vessel_vec)) {
    # i <- 1
    
    cat("Vessel:", fgem_vessel_vec[i], ":")
    
    # Copy the most recent stable fishing effort object
    temp.effort <- copy(prior.effort)
    
    # Move the vessel out of OB-FIXED and into FIXED_EM pool
    trips_to_convert <- temp.effort[PERMIT %in% fgem_vessel_vec[i] & STRATA == "OB_FIXED", unique(TRIP_ID)]

    # Convert all OB_FIXED trips into EM_FIXED
    temp.effort[TRIP_ID %in% trips_to_convert, STRATA := "EM_FIXED"]

    # Define boxes
    temp.box <- define_boxes_3(
      temp.effort, space = c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
      year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), dmn_cols = c("GEAR"), geom = T )

    # Calculate interspersion
    temp.ispn <- dmn_interspersion_figs(
      box_def = temp.box,
      selection_rates = selection_15,
      ob_adl = fixed_fmp.OB.acceptor_donor_lst,
      nonob_adl = fixed_fmp.nonOB.acceptor_donor_lst
    )

    
    # Capture interspersion results in list
    loop_res[[i]] <- list(ISPN = temp.ispn, EFFORT = temp.effort)

  }

  # Compare interspersion values to prior.ispn
  ispn_diff <- lapply(loop_res, function(x) {
    # x <- loop_res[[1]]
    
    x1 <- x$ISPN
    # Calculate differences in overall interspersion indices
    overall <- copy(x1$DMN_INSP_OB_SMRY$OVERALL)
    overall[, PRIOR := prior.ispn$DMN_INSP_OB_SMRY$OVERALL[overall, .(POOL_DMN_INTERSPERSION), on = .(GEAR, ADP, POOL)]]
    overall[, DIFF := POOL_DMN_INTERSPERSION - PRIOR]
    
    # Calculate differences in FMP-specific interspersion indices
    bsai_goa <- copy(x1$DMN_INSP_OB_SMRY$BSAI_GOA)
    bsai_goa[, PRIOR := prior.ispn$DMN_INSP_OB_SMRY$BSAI_GOA[bsai_goa, POOL_DMN_INTERSPERSION, on = .(GEAR, BSAI_GOA, ADP, POOL)]]
    bsai_goa[, DIFF := POOL_DMN_INTERSPERSION - PRIOR]
    
    list(OVERALL = overall, BSAI_GOA = bsai_goa)
    
  })
  
  # Sum the differences by pool across years
  ispn_diff_sum <- lapply(ispn_diff, function(x) {
    # x <- ispn_diff[[1]]
    list(
      # y <- x$BSAI_GOA 
      OB = lapply(x, function(y) sum(y[POOL == "OB", DIFF])),
      EM = lapply(x, function(y) sum(y[POOL == "EM", DIFF])),
      ZE = lapply(x, function(y) sum(y[POOL == "ZERO", DIFF]))
    )
  })
  names(ispn_diff_sum) <- fgem_vessel_vec
  
  # We will use changes to the OB pool, summing the changes to both BSAI and FMP, divided by the total number of trips taken
  ob_bsai_goa <- unlist(lapply(lapply(ispn_diff_sum, "[[", "OB"), "[[", "BSAI_GOA"))
  em_bsai_goa <- unlist(lapply(lapply(ispn_diff_sum, "[[", "EM"), "[[", "BSAI_GOA"))
  ze_bsai_goa <- unlist(lapply(lapply(ispn_diff_sum, "[[", "ZE"), "[[", "BSAI_GOA"))

  trip_counts_sub <- trip_counts[PERMIT %in% as.numeric(names( ob_bsai_goa)), .(N = sum(N)) , keyby = .(PERMIT)] # 3717 has the most trips
  ob_overall_dt <- data.table(PERMIT = as.integer(names(ob_bsai_goa)), OB_DIFF =  as.numeric(ob_bsai_goa))
  ob_overall_dt[, N := trip_counts_sub[ob_overall_dt, N, on = .(PERMIT)]]
  # calculate Ratio of total change to interspersion divided by number of trips taken
  ob_overall_dt[, RATIO := OB_DIFF / N]
  loop_metrics <- ob_overall_dt[order(-RATIO)]  
  
  # Select a 'winner', the one with the highest RATIO (least negative change per trip)
  add_to_fgem <- ob_overall_dt[which.max(RATIO), PERMIT]
  add_fgem_vec <- c(add_fgem_vec, add_to_fgem)
  cat("\n+ Adding vessel :", add_to_fgem, "to fixed-gear EM pool.\n\n")
  
  # TODO for first run, when add_fgem_vec is length 0, save INSP_CHANGE object for all vessels as init.insp_change
  
  # Reassign prior.ispn, prior.effort, and fgem_vessel_vec
  prior.effort <- loop_res[[which(fgem_vessel_vec == add_to_fgem)]]$EFFORT
  prior.ispn <- loop_res[[which(fgem_vessel_vec == add_to_fgem)]]$ISPN
  fgem_vessel_vec <- setdiff(fgem_vessel_vec, add_to_fgem)
  
  fgem_eval_vec <- c(fgem_eval_vec, list(list(
    PERMIT = add_to_fgem,
    METRICS = loop_metrics,
    INSP_CHANGE = c(
      OB = ob_bsai_goa[[as.character(add_to_fgem)]], 
      EM = em_bsai_goa[[as.character(add_to_fgem)]], 
      ZE = ze_bsai_goa[[as.character(add_to_fgem)]])
  )))
  
}

# SAVE OUTPUTS
# In Final ADP Outputs folder: https://drive.google.com/file/d/1OKcgtJN1Vb6iFFj2pSGw_Cpt3oeWQU7a/view?usp=drive_link
# save(
#   fgem_request_dt, fgem_opt_in, fgem_opt_out,
#   fishing_history, fishing_history_N, add_fgem_vec, fgem_eval_vec,
#   file = "analyses/fixed_gear_EM_request_evaluation/fgem_eval_results.rdata")

# 6-year Vector:
# 4383 33881 5608 2084 4387 3717 35836 4659 (with 2348, 5735 with no history in 2018-2023)

# TODO Make scorecard, combining fishing_history_N and results of loop

fishing_history_N

eval_dt <- data.table(VESSEL_ID = sapply(fgem_eval_vec, "[[", "PERMIT"))[, RANK := .I][]
eval_dt <- eval_dt[data.table(VESSEL_ID = fgem_opt_in$`Vessel Permit/Vessel ID`), on = .(VESSEL_ID)]
eval_dt <- fishing_history_N[eval_dt, on = .(VESSEL_ID)]
eval_dt <- rbindlist(lapply(fgem_eval_vec, function(x) x$METRICS[1, ]))[, .(VESSEL_ID = PERMIT, OB_DIFF, EVAL_N = N, RATIO)][eval_dt, on = .(VESSEL_ID)]
eval_dt[VESSEL_ID == 5735, VESSEL_NAME := "EMERALD_ISLE"]
eval_dt[order(VESSEL_ID)]
setorder(eval_dt, RANK)
setcolorder(eval_dt, c("VESSEL_ID", "VESSEL_NAME", as.character(2013:2023), "TOTAL", "EARLIEST", "LATEST", "TPY", "EVAL_N", "OB_DIFF", "RATIO", "RANK"))
# TODO ADD LENGTH OVERALL?
eval_dt_melt <- suppressWarnings(melt(
  eval_dt, id.vars = c("VESSEL_ID", "VESSEL_NAME"), 
  measure.vars = c("TOTAL", "TPY", "EVAL_N", "OB_DIFF", "RATIO", "RANK")))
# higher values is better, except for RANK!
eval_dt_melt[, FILL_RATIO := (value - min(value, na.rm = T)) / diff(range(value, na.rm = T)), by = variable]
eval_dt_melt[variable == "RANK", FILL_RATIO := -FILL_RATIO + 1]

# eval_dt_melt[variable == "RANK", FILL_RATIO := (1/ (1-FILL_RATIO))/10]


### How many POT, HAL, or mixed trips? ----
trips_gear <- unique(fishing_history[, .(ADP, AGENCY_GEAR_CODE, TRIP_ID, VESSEL_ID)])
uniqueN(trips_gear$TRIP_ID) # trips 406 total
trips_gear <- trips_gear[, .(
  HAL = .SD[all(AGENCY_GEAR_CODE == "HAL"), uniqueN(TRIP_ID)],
  MIXED = .SD[any(AGENCY_GEAR_CODE == "HAL") & any(AGENCY_GEAR_CODE == "POT"), uniqueN(TRIP_ID)],
  POT = .SD[all(AGENCY_GEAR_CODE == "POT"), uniqueN(TRIP_ID)]
  ), by = .(ADP, VESSEL_ID)]
trips_gear[, TOTAL := HAL + MIXED + POT]
sum(trips_gear$TOTAL) # should equal same amount as before (406)
dcast(trips_gear, VESSEL_ID ~ ADP, value.var = c("HAL", "MIXED", "POT"))
trips_gear_melt <- melt(trips_gear[, -"TOTAL"], id.vars = c("VESSEL_ID", "ADP"))[value != 0]
plot_trips_gear <- ggplot(trips_gear_melt, aes(x = ADP, y = variable, fill = value, label = value)) + 
  facet_grid(as.character(VESSEL_ID) ~ ., scales = "free_y", space = "free_y") + 
  geom_tile(aes(color = variable), linewidth = 1) + geom_text() + 
  scale_fill_viridis_c() + 
  scale_color_manual(values = c(HAL = "red", MIXED = "purple", POT = "blue"), guide = "none") + 
  labs(x = "Year", y = "Gear Used", fill = "Trips")


fishing_history_N[order(VESSEL_ID)]

plot_fgem <- ggplot(eval_dt_melt, aes(x = paste0("(", VESSEL_ID, ") ", VESSEL_NAME), y= value, fill = FILL_RATIO)) + 
  facet_grid(. ~ variable, scales = "free_x") + 
  geom_col(color = "black") + geom_text(aes(label = round(value, 6))) + 
  scale_fill_viridis_c() + scale_x_discrete(limits = rev) + coord_flip() +
  labs(fill = "Relative Value", y = "Value", x = "Vessel") + 
  theme(legend.position = "bottom")



# Changes to other pools (additive)
a <- as.data.table(do.call(rbind, lapply(fgem_eval_vec, "[[", "INSP_CHANGE")))
a[, PERMIT := sapply(fgem_eval_vec, "[[", "PERMIT")]
sapply(fgem_eval_vec, "[[", "PERMIT")
# 35836 has more of an impact to OB than to ZE and EM

a_melt <- melt(a, id.vars = c("PERMIT"))
a_melt[, FILL_RATIO := (value - min(value, na.rm = T)) / diff(range(value, na.rm = T)), by = variable]

# This is total change, not scaled by # trips
ggplot(a_melt, aes(x = as.character(PERMIT), y = value, fill = FILL_RATIO)) + facet_grid(. ~ variable, scales = "free") + geom_col(color = "black") + 
  coord_flip() + scale_fill_viridis_c() + labs(x = "Vessel", y = "Value")

# this is the first cut. As more vessels are added, impacts to OB become a little greater
first_cut <- fgem_eval_vec[[1]]$METRICS[, .(PERMIT, N)]
first_cut <- first_cut[a, on = .(PERMIT)]
metric_cols <- c("OB", "EM", "ZE")
first_cut[, paste0(metric_cols, "_RATIO") := lapply(.SD, function(x) x / N), by = .(PERMIT), .SDcols = metric_cols]
first_cut_melt <- melt(first_cut, id.vars = c("PERMIT", "N"))
first_cut_melt[, FILL_RATIO := (value - min(value, na.rm = T)) / diff(range(value, na.rm = T)), by = variable]
#first_cut_melt[, variable := factor(variable, levels = c("OB", "OB_RATIO", "EM", "EM_RATIO", "ZE", "ZE_RATIO"))]
ob_em_ze_metrics <- ggplot(first_cut_melt, aes(x = as.character(PERMIT), y = value, fill = FILL_RATIO)) + 
  facet_grid(. ~ variable, scales = "free") + geom_col(color = "black") + 
  coord_flip() + scale_fill_viridis_c() + labs(x = "Vessel", y = "Value") + 
  labs(x = "Vessel", y = "Value", fill = "Relative value") + 
  geom_hline(yintercept = 0) +
  scale_x_discrete(limits = rev)
ob_em_ze_metrics
init.ispn$DMN_INSP_OB_SMRY$BSAI_GOA  # Note that we have interspersion split by HAL and POT, so we are summing impacts to both of those idependently
# 3717 had on overall impact of 0.01 on FMP-level interspersion
#  4659 had the worst per-trip impacts to all OB, EM, and ZE, with only 4.6 trips per year

# Of the FGEM vessels that fish, what is the average # of trips taken per year?
fgem_tpy <- fixed.pc_effort[STRATA %like% "EM_FIXED", .(N = uniqueN(TRIP_ID)),  keyby = .(ADP, PERMIT)]
fgem_tpy_mean_median <- fgem_tpy[, .(MEDIAN = median(N), MEAN = mean(N)), keyby = .(ADP)]
plot_fgem_tpy <- ggplot(fgem_tpy, aes(x = N)) + geom_histogram(bins = 30) + facet_grid(ADP ~ .) + 
  geom_vline(data = fgem_tpy_mean_median, aes(xintercept = MEAN), color = "red") + 
  geom_text(data = fgem_tpy_mean_median, aes(x = MEAN, y = 30, label = round(MEAN,2)), color = "red", angle = 90, vjust = 1, hjust = 1, size = 3) + 
  geom_vline(data = fgem_tpy_mean_median, aes(xintercept = MEDIAN)) + 
  geom_text(data = fgem_tpy_mean_median, aes(x = MEDIAN, y = 30, label = round(MEDIAN,2)), angle = 90, vjust = 0, hjust = 1, size = 3) +
  labs(x = "Trips per year", y = "Count of vessels", subtitle = "Using 2023 list of FGEM vessels. Note that 2023 is data to date.")
# Red is mean, black is median
# Trips per year does fluctuate 


#=============#
## Summary ----
#=============#
fishing_history_N[order(VESSEL_ID)]
plot_fgem # Summary of results from history and interspersion analysis (impacts to OB-OB)
ob_em_ze_metrics # First cut, look at impacts to OB, EM, and ZE interspersion, total and per trip (i.e. total divided by N_EVAL)
plot_fgem_tpy # For reference, current list of FG_EM vessels fished 7.76 trips per year on average, median of 6.

### Save Objects for RMD ----
save(fishing_history_N, plot_fgem, ob_em_ze_metrics, plot_fgem_tpy, plot_trips_gear, file = "analyses/fixed_gear_EM_request_evaluation/fgem_results.rdata")
# Saved to Final ADP Outputs folder: https://drive.google.com/file/d/1gbLfqw1ncifSuoHtTsIBDAcTdLSVkHvC/view?usp=drive_link

#  2084  COMMANDER -      [Good candidate] Fishes 8.5 trips per year and had fairly low impact per trip
#  2348  ALEUT MISTRESS - [Bad] No recent fishing effort, last fished in 2014. Probably not cost effective?
# 33881  THREE PEARLS -   [Bad] Hasn't fished since 2019 (no fishing in last 3 years), and only fished 2-1 trips per year. 
# 35836  CRYSTAL STAR -   [Bad?] Fishes 5.5 trips per year but had the 2nd highest impact per rip  -- CHECK WHY 
#  3717  CARLYNN -        [Okay?] Fishes 18.455 trips per year, had most overall impact, but 6th greatest impact per trip.
#  4383  SOUTHEASTERN -   [Bad?] Fishes only 2.5 trips per year (2-4 consistently), with lowest impact per trip.
#  4387  TANYA M -        [Okay?] Fishes a lot (16.5 trips per year), but with some impacts to data. 
#  4659  SUNDANCER -      [Bad?] fishes 4.6 trips per year but had the highest impact per trip -- [CHECK WHY]
#  5608  GARNET -         [Okay?] Fishes only 4 trips per year, but more in more recent years, with low impact per trip. 
#  5735  EMERALD ISLE -   [Bad?] No fishing history whatsoever in VALHALLA

# Relative to all of FGEM vessels, knowing the percentile would be informative (putting them back into OB to compare the difference)

#======================================================================================================================#
# Incomplete ----
#======================================================================================================================#

# The below is not done - was working towards a way to visualize where/when fishing effort from one vessel occurs in 
# relation to the rest of fishing effort.

#=============#
## Mapping ----
#=============#

init.ispn$DMN_INSP_OB$RAW
init.ispn$DMN_INSP_OB$geom  # geometry to apply to cells

a1 <- copy(init.ispn$DMN_INSP_OB$RAW)
a1 <- merge(init.ispn$DMN_INSP_OB$geom, a1, on = "HEX_ID") %>% filter(STRATA %like% "FIXED")

init.box$og_data  # I Don't have VESSEL_ID but I do have TRIP_ID. Can see which BOX_ID they were in

fgem_request_box_id <- lapply(fgem_opt_in$`Vessel Permit/Vessel ID`, function(x) {
  init.box$og_data[TRIP_ID %in% unique(fixed.pc_effort[PERMIT == x, TRIP_ID])  ]
})
names(fgem_request_box_id) <- fgem_opt_in$`Vessel Permit/Vessel ID`
fgem_request_box_id <- rbindlist(fgem_request_box_id, idcol = "PERMIT")
fgem_request_box_id <- merge(init.ispn$DMN_INSP_OB$geom, fgem_request_box_id, on = "HEX_ID") %>% filter(STRATA %like% "FIXED")

# Kind of sloppy, but shows where each vessel fished. This is overplotting across all time (years split by weeks!)
ggplot(a1 %>% filter(STRATA == "OB_FIXED")) +
  geom_sf(data = ak_low_res %>% st_set_crs(st_crs(init.ispn$DMN_INSP_OB$geom))) + 
  geom_sf(data = fgem_request_box_id, fill = "red", alpha = 0.2) +
  geom_sf(fill = NA) + facet_wrap(~PERMIT, nrow = 2)

# Faceting by year
ggplot(a1 %>% filter(STRATA == "OB_FIXED")) +
  geom_sf(data = ak_low_res %>% st_set_crs(st_crs(init.ispn$DMN_INSP_OB$geom))) + 
  geom_sf(data = fgem_request_box_id, fill = "red", alpha = 0.2) +
  geom_sf(fill = NA) + facet_grid(ADP~PERMIT)

# Get yearly totals

a2 <- a1 %>% filter(STRATA == "OB_FIXED") %>% group_by(ADP, HEX_ID, geometry) %>% summarize(BOX_DMN_w = sum(BOX_DMN_w)) 
a3 <- fgem_request_box_id %>% group_by(ADP, PERMIT, HEX_ID, geometry) %>% summarize(BOX_N = n()) # number of weeks fished in that cell

ggplot(a2) +
  geom_sf(data = ak_low_res %>% st_set_crs(st_crs(init.ispn$DMN_INSP_OB$geom))) + 
  geom_sf(aes(fill = BOX_DMN_w)) + facet_grid(ADP~PERMIT) + 
  scale_fill_viridis_c(trans = "sqrt") + 
  geom_sf(data = a3, aes(color = BOX_N), linewidth = 1, fill = NA) + 
  scale_color_gradient(low = "red4", high = "orange")
# TODO Making individual plots for each stratum with fixed color scales should be nice? Could flip though each vessel

ggplot(a2) +
  geom_sf(data = ak_low_res %>% st_set_crs(st_crs(init.ispn$DMN_INSP_OB$geom))) + 
  geom_sf(aes(fill = BOX_DMN_w)) + facet_grid(ADP~PERMIT) + 
  scale_fill_viridis_c(trans = "log") +   # LOG OR SQRT?
  geom_sf(data = a3 %>% filter(PERMIT == 4659), aes(color = BOX_N), linewidth = 1, fill = NA) + 
  scale_color_gradient(low = "red4", high = "orange")
# 4659 is supposed to have the highest impact per trip. Seems like it generally fishes in HEX_IDs with high effort, but if those 
# boxes are high effort because of its trips or if neighboring cells are have fewer trips, may not 
# May need to summarize over time? Quarter Perhaps?

# time is split into weeks 1 to 53. Could split into 'months, rep(c(4,5), times = 6), or quarters sum(c(13,14,13,14))
a2 <- a1 %>% filter(STRATA == "OB_FIXED") %>% 
  mutate(MONTH = cut(TIME, cumsum(c(0, rep(c(4,5), times = 6))), labels = F)) %>%
  group_by(ADP, HEX_ID, MONTH, geometry) %>% summarize(BOX_DMN_w = sum(BOX_DMN_w)) 
a3 <- fgem_request_box_id %>%
  mutate(MONTH = cut(TIME, cumsum(c(0, rep(c(4,5), times = 6))), labels = F)) %>%
  group_by(ADP, PERMIT, HEX_ID, geometry, MONTH) %>% summarize(BOX_N = n()) # number of weeks fished in that cell
ggplot(a2) +
  geom_sf(data = ak_low_res %>% st_set_crs(st_crs(init.ispn$DMN_INSP_OB$geom))) + 
  geom_sf(aes(fill = BOX_DMN_w)) + facet_grid(MONTH ~ PERMIT + ADP) + 
  scale_fill_viridis_c(trans = "log") +   # LOG OR SQRT?
  geom_sf(data = a3 %>% filter(PERMIT == 4659), aes(color = BOX_N), linewidth = 1, fill = NA) + 
  scale_color_gradient(low = "red4", high = "orange")

a4 <- merge(a3, a2 %>% st_drop_geometry(), by = c("ADP", "HEX_ID", "MONTH"))
ggplot(a4 %>% filter(PERMIT == 4659)) +
  geom_sf(data = ak_low_res %>% st_set_crs(st_crs(init.ispn$DMN_INSP_OB$geom))) + 
  geom_sf(aes(fill = BOX_DMN_w, color = BOX_N), linewidth = 1) + facet_grid(MONTH ~ PERMIT + ADP) + 
  geom_sf_text(aes(label = round(BOX_DMN_w)), size = 1.5, color = "gray50") + 
  scale_fill_viridis_c(trans = "log") +   # LOG OR SQRT?
  scale_color_gradient(low = "red4", high = "orange")

# I can back-calculate number of neighbors using the sample prob (15%)
what <- copy(init.ispn$DMN_INSP_OB$POOLED)
what[, BOX_DMN_nbr := log(1 - BOX_DONOR_SAMPLE_PROB) / log(1 - 0.15) ]
# these neighbor counts are split by gear type... I typically apply these to a mixed gear trip's components separately to get a single probability
# SPLIT BY GEAR TYPE HERE Im summing BOX_DMN_w across gear type...



a2 <- a1 %>% filter(STRATA == "OB_FIXED") %>% 
  mutate(BOX_DMN_nbr = log(1 - BOX_DONOR_SAMPLE_PROB) / log(1 - 0.15)) %>%
  st_drop_geo


a1 <- copy(init.ispn$DMN_INSP_OB$RAW)[STRATA == "OB_FIXED"]
a1[, BOX_DMN_nbr := log(1 - BOX_DONOR_SAMPLE_PROB) / log(1 - 0.15) ]  # calculate # neighbors
# Identify number of trips each permit contributes to each box

trips_4659 <- unique(init.box$og_data[TRIP_ID %in% fixed.pc_effort[PERMIT == 4659, unique(TRIP_ID)]])
trips_4659 <- trips_4659[, .(VES_N = uniqueN(TRIP_ID)), keyby = .(BOX_ID)]
# Identify which boxes these neighbor
trips_4659 <- setnames(data.table(table(unlist(apply(trips_4659, MARGIN = 1, FUN = function(x) {
  rep(init.box$nbr_lst[[x[1]]], times = x[[2]])
})))), c("BOX_ID", "BOX_nbr_count"))
a1[trips_4659, on = .(BOX_ID)]

init.box$og_data[, BOX_n := .N, keyby = BOX_ID]

trips_4659 <- unique(fixed.pc_effort[PERMIT == 4659, .(TRIP_ID, GEAR)])
trips_4659 <- init.box$og_data[trips_4659, on = .(TRIP_ID)][, .(VES_N = uniqueN(TRIP_ID)), keyby = .(GEAR, BOX_ID)]
trips_4659 <- split(trips_4659, by = "GEAR", keep.by = F)
trips_4659 <- rbindlist(lapply(trips_4659, function(x) {
  setnames(data.table(table(unlist(apply(x, MARGIN = 1, function(y) {
    rep(init.box$nbr_lst[[y[[1]]]], times = y[[2]])
  })))), c("BOX_ID", "BOX_nbr_count"))
}), idcol = "GEAR")

init.ispn$DMN_INSP_OB$RAW[STRATA == "OB_FIXED"]
