# Fixed-gear EM Requesting Vessels Evaluation
# Author : Geoffrey Mayhew
# Date : 2023-Nov-02

# This script reviews all vessels requesting to join the fixed-gear EM vessel pool. It ranks requesting vessels in terms
# of their potential for producing data gaps by virtue of moving from the observer pool to the EM pool, as wells as the 
# cost-efficiency of each vessel in terms of how many trips they have taken in the past. 

#======================================================================================================================#
# Data Prep ----
#======================================================================================================================#

ADPyear <- 2025

#===================#
## Load packages ----
#===================#

library(data.table)
library(ggplot2)
library(FMAtools)
library(odbc)
library(readxl)      # For reading xlsx spreadsheets
library(gridExtra)
library(dplyr)       # For handling sf objects
library(sf)          # For spatial statistics
library(ggh4x)       # For facet_nested

#=======================#
## Connect to NORPAC ----
#=======================#

source("common_functions/open_channel.R")

channel_afsc <- open_channel()

### Determine which vessels are requesting to be added to the fixed-gear EM pool. Taken from get_data.R ----

# Vessels have until November 1st to request. Approvals are typically made shortly thereafter.
fgem_request_dt <- setDT(dbGetQuery(channel_afsc, paste(
  "
    SELECT DISTINCT adp, vessel_id, vessel_name, sample_plan_seq_desc, em_request_status, ves.length
    FROM loki.em_vessels_by_adp
      JOIN norpac.atl_lov_vessel ves
        ON vessel_id = ves.permit
    WHERE adp = ", ADPyear,"
      AND em_request_status IN ('NEW', 'O')
      AND sample_plan_seq_desc IN(
        'Electronic Monitoring - Gear Type- Selected Trips',   -- Pre-2024
        'EM Fixed Gear  - Fishing Area')                       -- 2024 onward
  "
)))
fgem_request_dt[, VESSEL_ID := as.numeric(VESSEL_ID)]

#===============#
## Load Data ----
#===============#

fgem_opt_in <- fgem_request_dt[EM_REQUEST_STATUS == "NEW"]
fgem_opt_out <- fgem_request_dt[EM_REQUEST_STATUS == "O"]

# Load the ADFG statistical area shapefile. '../' is needed for Rmarkdown to climb into parent folders.
stat_area_sf <- st_read(
  dsn = "source_data/ADFG_Stat_Area_shapefile/PVG_Statewide_2001_Present_GCS_WGS1984.shp", quiet = T) %>%
  select(STAT_AREA) %>%
  st_transform(crs = 3467)

# Load the Alaska map sf objects
load("source_data/ak_shp.rdata")      # shp_land, shp_nmfs, and shp_centroids added to global

# Load full VALHALLA (work.data) for fishing histories, created by get_data.R
gdrive_download(
  local_path = "source_data/work.data.rdata", 
  gdrive_dribble = gdrive_set_dribble("Projects/ADP/source_data/"))
(load("source_data/work.data.rdata"))

#====================#
## Load Functions ----
#====================#


#' [FIXME: Some of these funcitons are still in here and can be used for this analysis?] 
#' Specifically, dmn_interspersion_figs is needed from here. Howver, since the 2024 ADP, I did some different things...
#source("analyses/allocation_evaluation/functions.R")


source("common_functions/allocation_functions.R")

#======================================================================================================================#
# Fixed Gear EM Evaluation ----
#======================================================================================================================#

## Fishing History
fishing_history <- work.data[VESSEL_ID %in% fgem_opt_in$VESSEL_ID, .(
  VESSEL_ID = as.numeric(VESSEL_ID), ADP, TRIP_ID, STRATA, STRATA_NEW, COVERAGE_TYPE, CVG_NEW, 
  TRIP_TARGET_CODE, REPORTING_AREA_CODE, AGENCY_GEAR_CODE)] |>
  unique()
table(fishing_history$STRATA_NEW)  

# Note that some WGOA vessels may also fish with both TRW and POT Gear!
trw_history <- fishing_history[AGENCY_GEAR_CODE %in% c("PTR", "NPT"), unique(VESSEL_ID)]
fgem_opt_in[VESSEL_ID %in% trw_history]
fishing_history[VESSEL_ID %in% trw_history, .(N = uniqueN(TRIP_ID)), keyby = .(VESSEL_ID, ADP, STRATA_NEW)] |>
  dcast(VESSEL_ID + STRATA_NEW ~ ADP, value.var = "N", fill = 0)
#' [2025ADP: 10562, CAPE ST ELIAS,  vessel fishes in the EM_TRW-GOA stratum and also fishes fixed-gear in the GOA]

# The rest of the analysis will only concern impacts to fixed-gear trips. Retain an original copy of the history
if(!exists("fishing_history_og")) fishing_history_og <- copy(fishing_history)

# Exclude any trips that wouldn't be counted under fixed-gear EM
fishing_history <- fishing_history[(STRATA_NEW %like% c("FIXED"))]

# Count fixed-gear trips by each vessel and year that would fall under fixed-gear EM
fishing_history_years <- fishing_history[order(ADP), unique(ADP)]
fishing_history_N <- dcast(fishing_history[, .(N = uniqueN(TRIP_ID)), keyby = .(VESSEL_ID, ADP)], VESSEL_ID ~ ADP, value.var = "N", fill = 0)
fishing_history_N[, TOTAL := rowSums(fishing_history_N[, -"VESSEL_ID"])]
fishing_history_N[, EARLIEST := fishing_history_years[min(which(lapply(.SD, function(x) x > 0) == T))], by = VESSEL_ID, .SDcols = as.character(fishing_history_years)]
fishing_history_N[, LATEST := fishing_history_years[max(which(lapply(.SD, function(x) x > 0) == T))], by = VESSEL_ID, .SDcols = as.character(fishing_history_years)]
# Merge in vessel_name and Length
fishing_history_N[, c("VESSEL_NAME", "LOA") := fgem_opt_in[fishing_history_N, .(VESSEL_NAME, LENGTH), on = .(VESSEL_ID)]]
# Calculate trips per year in the past 3 years
last_3_years <- as.character(seq((ADPyear - 3), (ADPyear - 1)))
fishing_history_N$TPY3 <- rowSums(fishing_history_N[, ..last_3_years]) / 3 
setorder(fishing_history_N, -TPY3)

#' [2025ADP: 3 vessels with a history didn't fish in 2024 so far]
fishing_history_N[LATEST != (ADPyear - 1)]

# Some vessels may have no fishing history that would count under fixed-gear
no_history <- setdiff(fgem_opt_in$VESSEL_ID, fishing_history$VESSEL_ID)  
fgem_opt_in[VESSEL_ID %in% no_history]
#' [2025ADP: 2527, ANGIE LEE has no fishing history since 2013] 

# Are any vessels requesting EM in the zero-coverage fleet due to LOA < 40?
fgem_opt_in[LENGTH < 40]
#' [2025ADP: 0 vessels are < 40 ft] 

fishing_history_N

#======================================================================================================================#
# Evaluate Interspersion ---- 
#======================================================================================================================#

# -- Assume selection rates of 15% for all strata. Althoug we 'move' vessels from OB to EM, we don't calculate the 
# change to the afforded selection rates from proximity allocation (e.g., OB rates would go up and FG-EM would go down, presumably)
# -- pc_effort_dt is only made for 2015 at earliest

# Specify years of fishing effort to include in review (more takes longer!)
year_vec <- seq((ADPyear - 3), (ADPyear - 1))

# Restrict analysis to fixed-gear trips
fixed.pc_effort <- work.data[ADP %in% year_vec & STRATA_NEW %like% "FIXED|ZERO",] 
# Apply the stratum definitions from the ADP year
fixed.pc_effort[, STRATA := STRATA_NEW][, CVG := CVG_NEW]
fixed.pc_effort <- fixed.pc_effort |> spatiotemp_data_prep()

#' [2025ADP: Have always set 15% here before - consider changing this!]
# Initialize data.table with 15% selection rates for all strata
# selection_15 <- unique(fixed.pc_effort[STRATA != "ZERO", .(ADP, STRATA, BSAI_GOA)])[
# ][, STRATUM_COL := paste0(STRATA, "-", BSAI_GOA)
# ][, SAMPLE_RATE := 0.15][]

# First, move all vessels opting OUT of fixed-gear EM back into the observer pool
dcast(
  fixed.pc_effort[PERMIT %in% fgem_opt_out$VESSEL_ID, .(N = uniqueN(TRIP_ID)), keyby = .(PERMIT, ADP, STRATA, BSAI_GOA)],
  PERMIT + STRATA + BSAI_GOA ~ ADP, value.var = "N", fill = 0) 
# 3297 actually fished a fair amount, probably was a reasonably cost-effective fixed-gear EM vessel?
# 792 fished somewhat consistently, actually fished 10 trips in 2022 but only 1 so far in 2013.
# 3102 hasn't fished since 2015. 32413 hasn't fished since 2018.
fixed.pc_effort[PERMIT %in% fgem_opt_out$VESSEL_ID, ':=' (POOL = "OB", STRATA = gsub("EM_", "OB_", STRATA))]

#=============#
## Initial ----
#=============#

box_params <- list(
  space = c(2e5, 2e5),
  time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = "STRATA", ps_cols = "GEAR",
  stat_area_sf = stat_area_sf
)

init.box <- define_boxes(
  fixed.pc_effort, space = box_params$space, time = box_params$time,
  year_col = box_params$year_col, stratum_cols = box_params$stratum_cols, ps_cols = box_params$ps_cols,
  dmn_lst = list(nst = "GEAR", st = "BSAI_GOA"), geom = T)


# Define acceptor/donor lists for interspersion 
init.box$strata_n_dt[, -c("ADP", "STRATA_N")] |> unique()
# init.box$dmn$strata_dt
fixed_fmp.OB.acceptor_donor_lst <- c(
  rep(list(3:4), times = 2),                # 1-2:   EM Fixed Gear
  rep(list(3:4), times = 2),                # 3-4:   OB Fixed Gear
  rep(list(3:4), times = 1)                 # 5:   ZERO           
)
fixed_fmp.nonOB.acceptor_donor_lst <- c(
  rep(list(1:2), times = 2),               # 1-2: Fixed-gear EM to itself
  rep(list(NULL), times = 3)               # 3: No other donors
)

# Set the assumed monitoring rates for the upcoming ADP year STRATUM x ADP, using the rates from the Draft ADP
gdrive_download(
  local_path = paste0("results/draft_adp_", ADPyear, "_results.rdata"),
  gdrive_dribble = gdrive_set_dribble("Projects/ADP/Output")
)
(load(paste0("results/draft_adp_", ADPyear, "_results.rdata")))  # using rates_adp
# Apply these rates to the past 3 years
rates_adp <- rbind(
  copy(rates_adp)[,ADP := ADPyear - 3],
  copy(rates_adp)[,ADP := ADPyear - 2],
  copy(rates_adp)[,ADP := ADPyear - 1]
)

init.ispn <- dmn_interspersion_figs(
  box_def = init.box,
  selection_rates = rates_adp,
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

fgem_vessel_vec <- fgem_opt_in$VESSEL_ID
prior.effort <- copy(fixed.pc_effort)
prior.ispn <- copy(init.ispn)
add_fgem_vec <- c()
fgem_eval_vec <- list()

while(length(fgem_vessel_vec) > 0) {
  
  # Count the number of OB_FIXED trips by each vessel and year
  trip_counts <- prior.effort |>
    _[PERMIT %in% fgem_vessel_vec & STRATA %like% "OB_FIXED", .(N = uniqueN(TRIP_ID)), keyby = .(PERMIT, ADP)]
  
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
    
    cat("Vessel:", fgem_vessel_vec[i], ": ")
    
    # Copy the most recent stable fishing effort object
    temp.effort <- copy(prior.effort)
    
    # Move the vessel out of OB-FIXED and into EM_FIXED pool
    trips_to_convert <- temp.effort[PERMIT %in% fgem_vessel_vec[i] & STRATA %like% "OB_FIXED", unique(TRIP_ID)]

    # Convert all OB_FIXED trips into EM_FIXED
    temp.effort[TRIP_ID %in% trips_to_convert, STRATA := sub("OB_FIXED", "EM_FIXED", STRATA)]

    # Define boxes
    temp.box <- define_boxes(
      temp.effort, space = box_params$space, time = box_params$time,
      year_col = box_params$year_col, stratum_cols = box_params$stratum_cols, ps_cols = box_params$ps_cols,
      dmn_lst = list(nst = "GEAR", st = "BSAI_GOA"), geom = T)
    
    # Calculate interspersion
    temp.ispn <- dmn_interspersion_figs(
      box_def = temp.box,
      selection_rates = rates_adp,
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
    overall |>
      _[, PRIOR := prior.ispn$DMN_INSP_OB_SMRY$OVERALL[overall, POOL_DMN_INTERSPERSION, on = .(GEAR, ADP, POOL)]
      ][, DIFF := POOL_DMN_INTERSPERSION - PRIOR]
    
    # Calculate differences in FMP-specific interspersion indices
    bsai_goa <- copy(x1$DMN_INSP_OB_SMRY$BSAI_GOA)
    bsai_goa |>
      _[, PRIOR := prior.ispn$DMN_INSP_OB_SMRY$BSAI_GOA[bsai_goa, POOL_DMN_INTERSPERSION, on = .(GEAR, BSAI_GOA, ADP, POOL)]
      ][, DIFF := POOL_DMN_INTERSPERSION - PRIOR]
    
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
#   file = paste0("analyses/fixed_gear_EM_request_evaluation/results/", ADPyear, "fgem_eval_raw.rdata"))

# 6-year Vector:
# 4383 33881 5608 2084 4387 3717 35836 4659 (with 2348, 5735 with no history in 2018-2023)

# Make a scorecard, combining fishing_history_N and results of loop

# Capture ranks of each vessel based on OB_RATIO (per-trip impact to OB-OB interspersion)
eval_dt <- data.table(VESSEL_ID = sapply(fgem_eval_vec, "[[", "PERMIT"))[, RANK := .I][]

# Merge in all vessels and names, including those not evaluated for not having a fishing history
eval_dt <- eval_dt[fgem_request_dt[, .(VESSEL_ID = as.integer(VESSEL_ID), VESSEL_NAME)], on = .(VESSEL_ID)]

# Merge in yearly fishing history
eval_dt <- fishing_history_N[eval_dt, on = .(VESSEL_ID, VESSEL_NAME)]
# Merge in LOA, TYP3, and Rank Metrics
eval_dt <- rbindlist(lapply(fgem_eval_vec, function(x) x$METRICS[1, ])) |>
  _[, .(VESSEL_ID = PERMIT, OB_DIFF, EVAL_N = N, RATIO)
  ][eval_dt, on = .(VESSEL_ID)]
setorder(eval_dt, RANK)
setcolorder(eval_dt, c("VESSEL_ID", "VESSEL_NAME", as.character(2013:(ADPyear-1)), "TOTAL", "EARLIEST", "LATEST", "TPY3", "EVAL_N", "OB_DIFF", "RATIO", "RANK"))
# Melt data for plotting
eval_dt_melt <- suppressWarnings(melt(
  eval_dt, id.vars = c("VESSEL_ID", "VESSEL_NAME"), 
  measure.vars = c("TOTAL", "TPY3", "EVAL_N", "OB_DIFF", "RATIO", "RANK")))
# Assign a fill color, scaled by each variable. Higher values is better, except for RANK!
eval_dt_melt |>
  _[, FILL_RATIO := (value - min(value, na.rm = T)) / diff(range(value, na.rm = T)), by = variable
  ][variable %in% c("OB_DIFF", "RATIO"), FILL_RATIO := -FILL_RATIO + 1]

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
plot_trips_gear <- ggplot(trips_gear_melt, aes(x = as.character(ADP), y = variable, fill = value, label = value)) + 
  facet_grid(as.character(VESSEL_ID) ~ ., scales = "free_y", space = "free_y") + 
  geom_tile(aes(color = variable), linewidth = 1) + geom_text() + 
  scale_fill_viridis_c() + 
  scale_color_manual(values = c(HAL = "red", MIXED = "purple", POT = "blue"), guide = "none") + 
  labs(x = "Year", y = "Gear Used", fill = "Trips", subtitle = "Red = HAL, Purple = MIXED, Blue = POT")

# Score card of metrics. Purple = "good", yellow == "bad"
plot_fgem <- ggplot(eval_dt_melt, aes(x = paste0("(", VESSEL_ID, ") ", VESSEL_NAME), y= value, fill = FILL_RATIO)) + 
  facet_grid(. ~ variable, scales = "free_x") + 
  geom_col(color = "black", na.rm = T) + geom_text(aes(label = round(value, 6)), na.rm = T) + 
  scale_fill_viridis_c() + scale_x_discrete(limits = rev) + coord_flip() +
  labs(fill = "Relative Value", y = "Value", x = "Vessel", subtitle = paste0(ADPyear, ": Purple = good, yellow == bad")) + 
  theme(legend.position = "bottom") + theme_classic()

## Impacts to other pools ----
a <- as.data.table(do.call(rbind, lapply(fgem_eval_vec, "[[", "INSP_CHANGE")))
a[, PERMIT := sapply(fgem_eval_vec, "[[", "PERMIT")]

# this is the first cut. As more vessels are added, impacts to OB become a little greater
first_cut <- fgem_eval_vec[[1]]$METRICS[, .(PERMIT, N)]
first_cut <- first_cut[a, on = .(PERMIT)]
metric_cols <- c("OB", "EM", "ZE")
first_cut[, paste0(metric_cols, "_RATIO") := lapply(.SD, function(x) x / N), by = .(PERMIT), .SDcols = metric_cols]
first_cut_melt <- melt(first_cut, id.vars = c("PERMIT", "N"))
first_cut_melt[, FILL_RATIO := (value - min(value, na.rm = T)) / diff(range(value, na.rm = T)), by = variable]
plot_ob_em_ze_metrics <- ggplot(first_cut_melt, aes(x = as.character(PERMIT), y = value, fill = FILL_RATIO)) + 
  facet_grid(. ~ variable, scales = "free") + geom_col(color = "black") + 
  coord_flip() + scale_fill_viridis_c() + labs(x = "Vessel", y = "Value") + 
  labs(x = "Vessel", y = "Value", fill = "Relative value", subtitle = "First-cut metrics (i.e., non-additive impacts)") + 
  geom_hline(yintercept = 0) +
  scale_x_discrete(limits = rev)

# Of the FGEM vessels that fish, what is the average # of trips taken per year?
fgem_tpy <- fixed.pc_effort[STRATA %like% "EM_FIXED", .(N = uniqueN(TRIP_ID)),  keyby = .(ADP, PERMIT)]
fgem_tpy_mean_median <- fgem_tpy[, .(MEDIAN = median(N), MEAN = mean(N)), keyby = .(ADP)]
plot_fgem_tpy <- ggplot(fgem_tpy, aes(x = N)) + geom_histogram(bins = 30) + facet_grid(ADP ~ .) + 
  geom_vline(data = fgem_tpy_mean_median, aes(xintercept = MEAN), color = "red") + 
  geom_text(data = fgem_tpy_mean_median, aes(x = MEAN, y = 30, label = round(MEAN,2)), color = "red", angle = 90, vjust = 1, hjust = 1, size = 3) + 
  geom_vline(data = fgem_tpy_mean_median, aes(xintercept = MEDIAN)) + 
  geom_text(data = fgem_tpy_mean_median, aes(x = MEDIAN, y = 30, label = round(MEDIAN,2)), angle = 90, vjust = 0, hjust = 1, size = 3) +
  labs(x = "Trips per year", y = "Count of vessels", subtitle = "Mean = red, median = black")
# Red is mean, black is median
# Trips per year does fluctuate 

#==========================#
## Spatial distribution ----
#==========================#

# How the vessels were ranked (i.e., impacts to data gaps), may be explained by where the vessels fish and how that 
# compares to the remaining fishing in the observer pool

a1 <- copy(init.ispn$DMN_INSP_OB$RAW)
a1 <- merge(init.ispn$DMN_INSP_OB$geom, a1, on = "HEX_ID") %>% filter(STRATA %like% "FIXED")

fgem_request_box_id <- lapply(fgem_opt_in$VESSEL_ID, function(x) {
  init.box$og_data[TRIP_ID %in% unique(fixed.pc_effort[PERMIT == x, TRIP_ID])  ]
})
names(fgem_request_box_id) <- fgem_opt_in$VESSEL_ID
fgem_request_box_id <- rbindlist(fgem_request_box_id, idcol = "PERMIT")
fgem_request_box_id <- merge(init.ispn$DMN_INSP_OB$geom, fgem_request_box_id, on = "HEX_ID") %>% filter(STRATA %like% "FIXED")

# Kind of sloppy, but shows where each vessel fished. This is overplotting across all time (years split by weeks!)
plot_spatial_overall <- ggplot(a1 %>% filter(STRATA == "OB_FIXED")) +
  geom_sf(data = ak_low_res %>% st_set_crs(st_crs(init.ispn$DMN_INSP_OB$geom))) + 
  geom_sf(data = fgem_request_box_id, fill = "red", alpha = 0.2) +
  geom_sf(fill = NA) + facet_wrap(~PERMIT, nrow = 2)

# Faceting by year
plot_spatial_by_year <- ggplot(a1 %>% filter(STRATA == "OB_FIXED")) +
  geom_sf(data = ak_low_res %>% st_set_crs(st_crs(init.ispn$DMN_INSP_OB$geom))) + 
  geom_sf(data = fgem_request_box_id, fill = "red", alpha = 0.2) +
  geom_sf(fill = NA) + facet_grid(ADP~PERMIT)

# Calculate weight of boxes to create a map of where OB_FIXED fishing occurs
a2 <- a1 %>% filter(STRATA %like% "OB_FIXED") %>% group_by(ADP, HEX_ID, geometry) %>% summarize(BOX_DMN_w = sum(BOX_DMN_w)) 
a3 <- fgem_request_box_id %>% group_by(ADP, PERMIT, HEX_ID, geometry) %>% summarize(BOX_N = n()) # number of weeks fished in that cell

#' Plot how much each vessel fishes (outlines) overlaid on the total OB_FIXED fishing effort. You don't really want to 
#' add vessels to the EM pool if they represent a lot of fishing in a particular area (purple cells outlined in orange)
plot_spatial_overlay <- ggplot(a2) +
  geom_sf(data = ak_low_res %>% st_set_crs(st_crs(init.ispn$DMN_INSP_OB$geom))) + 
  geom_sf(aes(fill = BOX_DMN_w)) + facet_grid(ADP ~ PERMIT) + 
  scale_fill_viridis_c(trans = "sqrt") + 
  geom_sf(data = a3, aes(color = BOX_N), linewidth = 0.5, fill = NA) + 
  scale_color_gradient(low = "red4", high = "orange") + 
  labs(subtitle = "Purple cells outlined in orange = bad: Vessel fishes a lot in places where less fishing occurs")

#=============#
## Summary ----
#=============#

#' [2025ADP: Summary]
#' VESSEL_ID    VESSEL_NAME         COMMENTS
#'      2527      ANGIE LEE - [bad]  No fishing history since 2013.
#'     26563     JODI MARIE - [bad]  No fishing history since 2022.
#'     25961       DOMINION - [bad]  Not cost-efficient. Only 1.67 trips fished per year.
#'     25080     ALEUT LADY - [bad]  Not cost-efficient. Only 1.00 trips fished per year.
#'       571  NORTHERN FURY - [bad]  Not cost-efficient. Only 1.67 trips fished per year.
#'     30617    OCEAN OASIS - [okay] Not cost-efficient. Only 5.00 trips fished per year, evaluate next year.
#'     10562  CAPE ST ELIAS - *good* Fishes 6.33 trips per year, already wired as a Trawl EM vessel.
#'     35836   CRYSTAL STAR - [bad]  Not cost-efficient. Only 3.67 trips fished per year.
#'      1877      LADY RUTH - [bad]  Fishes 11.67 trips per year but all in the BSAI, highly likely to cause data gaps.

### Save Objects ----
out_name <- paste0("analyses/fixed_gear_EM_request_evaluation/results/", ADPyear, "_fgem_eval.rdata")
save(
  # Raw outputs
  fgem_request_dt, fgem_opt_in, fgem_opt_out, fishing_history, fishing_history_N, add_fgem_vec, fgem_eval_vec,
  # Summaries
  eval_dt, eval_dt_melt,
  # Figures
  plot_fgem, plot_ob_em_ze_metrics, plot_fgem_tpy, plot_trips_gear,
  plot_spatial_overall, plot_spatial_by_year, plot_spatial_overlay,
  file = out_name)
#' Upload to Gdrive
gdrive_upload(local_path = out_name, gdrive_dribble = gdrive_set_dribble("Projects/ADP/source_data"))

### Save figures to upload to the repo
ggsave(plot_fgem, file = "analyses/fixed_gear_EM_request_evaluation/results/figures/plot_fgem.png", width = 10, height = 7.5, units = "in")
ggsave(plot_ob_em_ze_metrics, file = "analyses/fixed_gear_EM_request_evaluation/results/figures/plot_ob_em_ze_metrics.png", width = 10, height = 7.5, units = "in")
ggsave(plot_fgem_tpy, file = "analyses/fixed_gear_EM_request_evaluation/results/figures/plot_fgem_tpy.png", width = 10, height = 7.5, units = "in")
ggsave(plot_trips_gear, file = "analyses/fixed_gear_EM_request_evaluation/results/figures/plot_trips_gear.png", width = 10, height = 7.5, units = "in")
ggsave(plot_spatial_overall, file = "analyses/fixed_gear_EM_request_evaluation/results/figures/plot_spatial_overall.png", width = 10, height = 4.5, units = "in")
ggsave(plot_spatial_by_year, file = "analyses/fixed_gear_EM_request_evaluation/results/figures/plot_spatial_by_year.png", width = 10, height = 4.5, units = "in")
ggsave(plot_spatial_overlay, file = "analyses/fixed_gear_EM_request_evaluation/results/figures/plot_spatial_overlay.png", width = 10, height = 4.5, units = "in")

