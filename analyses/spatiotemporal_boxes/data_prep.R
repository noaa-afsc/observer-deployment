### Author: Geoff Mayhew
### Purpose: Prepares the dataset for analyses/spatiotemporal_boxes/ 
### Outputs: spatiotemporal_boxes.Rdata
# This script is used by spatiotempral_boxes.Rmd

# Load packages --------------------------------------------------------------------------------------------------------
library(data.table)                  # Data wrangling tools

# Load data -------------------------------------------------------------------------------------------------------

# TODO When VALHALLA is updated to have 2018-2021 fishing data, these can be replaced with a SQL pull. 

# The 2021 Annual Report, has full 2021 fishing effort:
# https://drive.google.com/file/d/1bWVlhSlbWOWcmeqrQQxpt-QrbWRr8vHo/view?usp=sharing
load("source_data/2_AR_data.Rdata")  
dat_2021_dt <- copy(work.data)
rm(list = setdiff(ls(), "dat_2021_dt"))

# 2022 Final ADP data (2018-2020 complete)
# https://drive.google.com/file/d/18oQ0bEmJOBFUkTHyWcoAHeLUdFnWD-pQ/view?usp=sharing
load("source_data/2022_Final_ADP_data.rdata")      # work.data is all we need
rm(list = setdiff(ls(), c("dat_2021_dt", "work.data", "fg_em")))  # Keep only dat_2021_dt, work.data, and fg_em

# 2022 Fishing Effort (2023 ADP Data):
# https://drive.google.com/file/d/17ztSrtFm_CPvor3AOQc7i0oxOHNFzaDH/view?usp=sharing
load("source_data/2022-10-12CAS_VALHALLA.RData")

# The 2022 TRW EM EFP vessel list, taken from GitLab's 2022_final_adp_repository/data
efp_list_dt <- fread("source_data/efp_list_2021-09-08.csv")

# Combine fishing effort datasets ---------------------------------------------------------------------------------

# Combine the 2018-2021 datasets, partial coverage only. Rename some columns for shorter names, summarize trip start
# and end dates for each TRIP_ID, and reclassify some columns.
val_cols_vec <- c(
  "ADP", "STRATA", "TENDER", "TRIP_ID", "VESSEL_ID", "REPORTING_AREA_CODE", "AGENCY_GEAR_CODE",  "ADFG_STAT_AREA_CODE", 
  "TRIP_TARGET_CODE", "TRIP_TARGET_DATE", "LANDING_DATE", "WEIGHT_POSTED", "SOURCE_TABLE"
)
val_2018_2021_dt <- rbind(
  unique(dat_2021_dt[COVERAGE_TYPE == "PARTIAL", ..val_cols_vec]),
  unique(work.data[CVG_NEW == "PARTIAL" & ADP != 2021, ..val_cols_vec])
)
setnames(
  val_2018_2021_dt,
  old = c("REPORTING_AREA_CODE", "AGENCY_GEAR_CODE", "TRIP_TARGET_CODE"),
  new = c("NMFS_AREA", "GEAR", "TARGET"))

# Calculate tonnage_dt of each trip by gear type stored in a separate object. Used for FG EM vessel STRATA later.
tonnage_dt <- val_2018_2021_dt[, .(TONNAGE = sum(WEIGHT_POSTED, na.rm = T)), by = .(VESSEL_ID, TRIP_ID, GEAR, SOURCE_TABLE)]
val_2018_2021_dt <- unique(val_2018_2021_dt[, -c("WEIGHT_POSTED", "SOURCE_TABLE")])

val_2018_2021_dt <- unique(val_2018_2021_dt[, ':=' (
  TRIP_START = min(TRIP_TARGET_DATE, LANDING_DATE, na.rm=T),
  TRIP_END = max(TRIP_TARGET_DATE, LANDING_DATE, na.rm=T)), 
  by = .(TRIP_ID)
][, WEEK := week(TRIP_END - as.numeric(TRIP_END - TRIP_START, units="days"))
][, c("TRIP_TARGET_DATE", "LANDING_DATE") := NULL][])
to_int_vec <- c("ADP", "VESSEL_ID", "NMFS_AREA", "ADFG_STAT_AREA_CODE")
val_2018_2021_dt[, (to_int_vec) := lapply(.SD, as.integer), .SDcols = to_int_vec]
rm(to_int_vec, val_cols_vec)

# Clean up the STRATA column, removing tender strata and Zero EM Research
val_2018_2021_dt[, STRATA := ifelse(
  STRATA %in% c("EM_HAL", "EM_POT", "EM_TRW_EFP", "HAL", "POT", "TRW", "ZERO"), STRATA, fcase(
    STRATA == "TenP", "POT",
    STRATA == "TenTR", "TRW",
    STRATA == "EM_TenP", "EM_POT",
    STRATA == "Zero EM Research", "ZERO",
    STRATA == "EM TRW EFP", "EM_TRW_EFP",
    STRATA == "EM HAL", "EM_HAL",
    STRATA == "EM POT", "EM_POT",
    STRATA == "TRW", "TRW"
  ))]

# Calculate TRW EM trip rates -----------------------------------------------------------------------------------------------

# In 2022, 12 trawl vessels fished in the TRW EM EFP although they were not declared in our list in the 2022 ADP...
# For the vessels that WERE in our list, the EFP trip rate was 71.93, pretty close to our estimate of 74.02%.

efp_not_in_list_vec <- unique(VALHALLA[
  STRATA == "EM_TRW_EFP" & 
  !(VESSEL_ID %in% efp_list_dt$PERMIT[efp_list_dt$YEAR_ADDED <= 2022])
]$VESSEL_ID)

# Possible EM_TRW_EFP trips are those that targeted pollock (TARGET == B or P) and fished only with PTR gear
# For vessels listed in Trawl EM, what proportion of trips were taken within EM_TRW_EFP Stratum?
efp_list_prob <- unique(VALHALLA[(
  VESSEL_ID %in% efp_list_dt$PERMIT[efp_list_dt$YEAR_ADDED <= 2022] & 
  TRIP_TARGET_CODE %in% c("P", "B") & 
  AGENCY_GEAR_CODE %in% c("NPT", "PTR")), 
  .(TRIP_ID, COVERAGE_TYPE, STRATA, AGENCY_GEAR_CODE)])[
][, .SD[all(AGENCY_GEAR_CODE == "PTR")], by = .(TRIP_ID)
][, .N, by = .(COVERAGE_TYPE, STRATA)
][, .(STRATA, N, EFP_PROB = N / sum(N)), by = .(COVERAGE_TYPE)
][COVERAGE_TYPE == "PARTIAL" & STRATA == "EM_TRW_EFP", EFP_PROB]

# For vessels NOT listed in Trawl EM...
efp_not_in_list_prob <- unique(VALHALLA[(
  VESSEL_ID %in% efp_not_in_list_vec & 
  TRIP_TARGET_CODE %in% c("P", "B") & 
  AGENCY_GEAR_CODE %in% c("NPT", "PTR")), 
.(TRIP_ID, COVERAGE_TYPE, STRATA, AGENCY_GEAR_CODE)])[
][, .SD[all(AGENCY_GEAR_CODE == "PTR")], by = .(TRIP_ID)
][, .N, by = .(COVERAGE_TYPE, STRATA)
][, .(STRATA, N, EFP_PROB = N / sum(N)), by = .(COVERAGE_TYPE)
][COVERAGE_TYPE == "PARTIAL" & STRATA == "EM_TRW_EFP", EFP_PROB]

# Apply 2022 TRW EM rates to 2018-2021 effort ---------------------------------------------------------------------

set.seed(12345)

# Randomly sample trips listed in TRW EM using probability from efp_list_prob
efp_list_trips <- unique(
  unique(val_2018_2021_dt[(VESSEL_ID %in% efp_list_dt$PERMIT) & (STRATA %like% "TRW"), .(TRIP_ID, TARGET, GEAR)])[
  ][, .SD[all(GEAR == "PTR") & all(TARGET %in% c("P", "B"))], by = .(TRIP_ID)
  ][, .(TRIP_ID)])
efp_list_trips[, NEW_STRATA := ifelse(runif(.N) < efp_list_prob, "EM_TRW", "TRW")]
val_2018_2021_dt[
][, NEW_STRATA1 := efp_list_trips[val_2018_2021_dt, NEW_STRATA, on = .(TRIP_ID)]
][!is.na(NEW_STRATA1), STRATA := NEW_STRATA1]

# Randomly sample trips NOT listed in TRW EM using probability from efp_not_in_list_prob
efp_not_in_list_trips <- unique(
  unique(val_2018_2021_dt[(VESSEL_ID %in% efp_not_in_list_vec) & (STRATA %like% "TRW"), .(TRIP_ID, TARGET, GEAR)])[
  ][, .SD[all(GEAR == "PTR") & all(TARGET %in% c("P", "B"))], by = .(TRIP_ID)
  ][, .(TRIP_ID)])
efp_not_in_list_trips[, NEW_STRATA := ifelse(runif(.N) < efp_not_in_list_prob, "EM_TRW", "TRW") ]
val_2018_2021_dt[
][, NEW_STRATA2 := efp_not_in_list_trips[val_2018_2021_dt, NEW_STRATA, on = .(TRIP_ID)]
][!is.na(NEW_STRATA2), STRATA := NEW_STRATA2]
val_2018_2021_dt[, c("NEW_STRATA1", "NEW_STRATA2") := NULL]

# Apply current FG EM vessel list to 2018-2021 effort -----------------------------------------------------

val_2018_2021_dt[VESSEL_ID %in% as.integer(fg_em$PERMIT), STRATA := fcase(
  STRATA %like% "EM", STRATA,
  STRATA == "ZERO" & GEAR %in% c("HAL", "POT"), paste0("EM_", GEAR),
  STRATA == "ZERO" & GEAR == "JIG", "ZERO",
  STRATA %in% c("POT", "HAL"), paste0("EM_", STRATA)
)]

# Vessels that are no longer in the FG EM list will be placed back into the observer pool
out_of_fg_em_vec <- unique(val_2018_2021_dt[
][STRATA %in% c("EM_HAL", "EM_POT") & !(VESSEL_ID %in% fg_em$PERMIT)
][order(ADP, TRIP_START)])
val_2018_2021_dt[
  VESSEL_ID %in% out_of_fg_em_vec & STRATA %in% c("EM_HAL", "EM_POT"), 
  STRATA := gsub("EM_", "", STRATA)]

# Fix trips with more than one stratum ----------------------------------------------------------------------------

# Some FG EM vessels have more the one STRATA. Use the dominant gear type by tonnage_dt of retained catch. 
multi_strata_trips_dt <- tonnage_dt[TRIP_ID %in% val_2018_2021_dt[, .SD[uniqueN(STRATA) > 1], by = .(TRIP_ID)][, unique(TRIP_ID)]
][, .SD[which(TONNAGE == max(TONNAGE))] , by = .(TRIP_ID)]
val_2018_2021_dt[, DOM_GEAR := multi_strata_trips_dt[val_2018_2021_dt, GEAR, on = .(TRIP_ID)]][
][!is.na(DOM_GEAR), STRATA := paste0("EM_", DOM_GEAR)][, DOM_GEAR := NULL]

# Double-check there are not any trips with more than one stratum!
if( nrow(val_2018_2021_dt[, .SD[uniqueN(STRATA) > 1], by = .(TRIP_ID)]) ) {
  warning("There are trips with more than one stratum!")
}

# Final cleanup and save Rdata file -------------------------------------------------------------------------------

# Re-assign TRIP_ID as an integer an reorder the dataset
val_2018_2021_dt[, TRIP_ID := .GRP, by = TRIP_ID][, TRIP_ID := as.integer(TRIP_ID)]
setorder(val_2018_2021_dt, ADP, STRATA, TRIP_START, NMFS_AREA)
save(val_2018_2021_dt, file = "analyses/spatiotemporal_boxes/spatiotemporal_boxes.RData")
