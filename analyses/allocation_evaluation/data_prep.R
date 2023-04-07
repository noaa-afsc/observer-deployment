# This is the data prep script for analyses/allocation_evaluation.R

### Author: Geoff Mayhew
### Purpose: Prepares the data set for analyses/allocation_evaluation/ 
### Outputs: data_prep.Rdata
# This script is used by allocation_evaluation.rmd

# -----------------------------------------------------------------------------------------------------------------#
# TODO List -------------------------------------------------------------------------------------------------------
# TODO rename STRATA for OB pool trips as OB_HAL, OB_POT, and OB_TRW
# TODO I can't connect to AKRO at the moment so I cannot run the get_data.R script in its entirely for 2015-2022.
# Most importantly, I can't pull the PSC numbers from AKRO to create the trips_melt object for 2015-2022.
# TODO NEED 2015 ONWARDS IF I WANT METRICS FOR 2018!
# TODO Make WEEK and MONTH integer
# -----------------------------------------------------------------------------------------------------------------#

# Load packages --------------------------------------------------------------------------------------------------------
library(data.table)                  # Data wrangling tools
library(rstudioapi)
library(odbc)
library(lubridate)

# Load data ------------------------------------------------------------------------------------------------------------

channel <- dbConnect(odbc::odbc(),"AFSC",
  UID    = rstudioapi::askForPassword("Database user"),
  PWD    = rstudioapi::askForPassword("Database password"))

# Load 2023 Final ADP Data
# https://drive.google.com/file/d/1GeBZgGaBvZPl3T-tVbTvpDZ5aF9I-CS7/view?usp=share_link
load("source_data/2023_Final_ADP_data.rdata")   # Contains some AKRO objects I can't pull myself at the moment

# LOAD VALHALLA 2018 to current. ---------------------------------------------------------------------------------------


# Took me 1 hour 32 minutes. 
# val_2018_current <- setDT(dbGetQuery(channel, paste("SELECT * FROM loki.akr_valhalla WHERE ADP >= 2018")))
# save(val_2018_current, file = "source_data/valhalla_2018_current.Rdata")
load("source_data/valhalla_2018_current.Rdata")  # loads 'val_2018_current'
work.data <- copy(val_2018_current)[ADP != 2022]
rm(val_2018_current)

# Load the 2022 Valhalla data set (preliminary)
load("source_data/2023-01-27CAS_VALHALLA.Rdata")  # Loads VALHALLA object to the environment, 2022 data only
# Before combining, also make sure there are no common TRIP_IDs
setdiff(work.data, VALHALLA) # Have to reconcile different data classes

common_trip_ids <- intersect(work.data$TRIP_ID, as.character(VALHALLA$TRIP_ID))
if(length(common_trip_ids)) stop("There are common trip_ids between the valhalla datasets. Check these before merging!")

# There is only one common trip_id here: 896676 and they appear to be different trips. Re-assign a new trip_id to the 
# older data set.
for(i in common_trip_ids){
  if( nrow(work.data[TRIP_ID == paste0(i, ".1")]) == 0 ) {
    work.data[TRIP_ID == i, TRIP_ID := paste0(x, ".1")]
  } else stop(paste0("TRIP_ID ", i, ".1 already exists! Aborting"))
}
rm(common_trip_ids)

# Unify the column classes
VALHALLA[, TRIP_ID := as.character(TRIP_ID)]
char_to_int <- c("CRUISE", "PERMIT")
work.data[, (char_to_int) := lapply(.SD, as.integer), .SDcols = char_to_int ]
rm(char_to_int)

posixct_to_date <- c("TRIP_TARGET_DATE", "LANDING_DATE")
work.data[, (posixct_to_date) := lapply(.SD, as.Date), .SDcols = posixct_to_date]
rm(posixct_to_date )

work.data <- rbind(work.data, VALHALLA)
rm(VALHALLA)

# * Fixed-gear EM approvals ---- 
# FIXME BROKEN VIEW - I get error: 'view "LOKI.EM_VESSELS_BY_ADP" has errors'
# em_base <- dbGetQuery(channel, paste(
#   "
#   SELECT DISTINCT adp, vessel_id, vessel_name, sample_plan_seq_desc, em_request_status
#   FROM loki.em_vessels_by_adp
#   WHERE adp = ", 2023,"
#     AND em_request_status = 'A'
#     AND sample_plan_seq_desc = 'Electronic Monitoring - Gear Type- Selected Trips'
#   "
# ))

BSAIVoluntary <- dbGetQuery(channel, paste(
 " 
 -- From Andy Kingham
 SELECT extract(year from ovsp.end_date) as year,
   ovsp.*,
   lv.name,
   lv.permit as vessel_id,
   aos.DATE_OPTED,
   DECODE(aos.opt_in_out, 'R', 'Requested, Approved', 'D', 'Denied', 'A', 'Appealed, Approved') opt_in_out_status
 FROM norpac.odds_vessel_sample_plan ovsp
  JOIN norpac.atl_lov_vessel lv
    ON lv.vessel_seq = ovsp.vessel_seq
  JOIN norpac.odds_eligible_opt_strata eos
   ON eos.VESSEL_SEQ = ovsp.VESSEL_SEQ AND eos.sample_plan_seq = ovsp.sample_plan_seq
  JOIN norpac.odds_annual_opt_strata aos
    ON aos.ELIGIBLE_OPT_SEQ = eos.ELIGIBLE_OPT_SEQ 
 WHERE ovsp.SAMPLE_PLAN_SEQ = 8
  AND EXTRACT(Year FROM ovsp.end_date) > ", 2023 - 1, "
  AND aos.year_eligible = ", 2023, "")
)

# Run relevant get_data.R code to modify valhalla pull -----------------------------------------------------------------

# Convert dates using as.Date to avoid timestamp issues
#work.data <- mutate(work.data, TRIP_TARGET_DATE = as.Date(TRIP_TARGET_DATE), LANDING_DATE = as.Date(LANDING_DATE))
# Convert VESSEL_IDs using as.character to facilitate a later join with vessel lengths
work.data <- mutate(work.data, VESSEL_ID = as.character(VESSEL_ID))
# In the past, there was one record for which TENDER == "n", so convert to upper case
work.data <- mutate(work.data, TENDER = toupper(TENDER))
work.data[, ADP := min(ADP), by = .(TRIP_ID)]

# *Update landing dates ------------------------------------------------------------------------------------------
# FIXME  Cannot connect to AKRO here so cannot update landing dates like in get_data.R
# -----------------------------------------------------------------------------------------------------------------#

if(nrow(select(work.data, TRIP_ID, ADP) %>% 
        distinct() %>% 
        group_by(TRIP_ID) %>% 
        filter(n()>1) %>% 
        data.frame()) > 0){
  work.data$TRIP_ID <- paste(work.data$ADP, work.data$TRIP_ID, sep = ".")
  message("Adding 'year.' to duplicate TRIP_IDs across years")
} else{message("No duplicate TRIP_IDs across years")}

# * CVG_NEW ---- 

# Create empty CVG_NEW column
work.data <- mutate(work.data, CVG_NEW = NA)
# Mandatory full coverage
work.data <- mutate(work.data, CVG_NEW = ifelse(COVERAGE_TYPE == "FULL" & STRATA %in% c("FULL", "EM_TRW_EFP"), "FULL", CVG_NEW))
# Voluntary full coverage
work.data <- mutate(work.data, CVG_NEW = ifelse(
  VESSEL_ID %in% BSAIVoluntary$VESSEL_ID &
    FMP %in% c("BS", "AI", "BSAI") &
    AGENCY_GEAR_CODE %in% c("NPT", "PTR", "TRW") & 
    PROCESSING_SECTOR == "S",
  "FULL", CVG_NEW))
# Cooperative full coverage
work.data <- mutate(work.data, CVG_NEW = ifelse(
  AGENCY_GEAR_CODE %in% c("NPT", "PTR", "TRW") &
    MANAGEMENT_PROGRAM_CODE %in% c("RPP", "AFA", "A80"),
  "FULL", CVG_NEW))
# Partial CPs
work.data <- mutate(work.data, CVG_NEW = ifelse(
  VESSEL_ID %in% PartialCPs$VESSEL_ID &
    PROCESSING_SECTOR == "CP_M", 
  "PARTIAL", CVG_NEW))
# View remaining coverage type / strata combinations
distinct(work.data, COVERAGE_TYPE, CVG_NEW, STRATA) %>% 
  arrange(COVERAGE_TYPE, CVG_NEW, STRATA)
# Partial coverage strata
work.data <- mutate(work.data, CVG_NEW = ifelse(is.na(CVG_NEW) & COVERAGE_TYPE == "PARTIAL" & STRATA %in% c("EM", "EM_HAL", "EM_POT", "EM_TenP", "EM_TRW_EFP", "HAL", "POT", "TenH", "TenP", "TenTR", "TRW", "ZERO", "ZERO_EM_RESEARCH"), "PARTIAL", CVG_NEW))
# Check for remaining NAs in CVG_NEW
filter(work.data, is.na(CVG_NEW)) %>% 
  distinct(ADP, COVERAGE_TYPE, CVG_NEW, STRATA) %>% 
  arrange(ADP, COVERAGE_TYPE, CVG_NEW, STRATA)

# * STRATA_NEW ----

# Create empty STRATA_NEW column
work.data <- mutate(work.data, STRATA_NEW = NA)
# Designate STRATA_NEW for vessels that have been approved for fixed gear EM
work.data <- mutate(work.data, STRATA_NEW = ifelse(VESSEL_ID %in% fg_em$PERMIT & STRATA %in% c("HAL", "TenH", "EM_HAL"), "EM_HAL", STRATA_NEW))
work.data <- mutate(work.data, STRATA_NEW = ifelse(VESSEL_ID %in% fg_em$PERMIT & STRATA %in% c("POT", "TenP", "EM_POT", "EM_TenP"), "EM_POT", STRATA_NEW))
work.data <- mutate(work.data, STRATA_NEW = ifelse(VESSEL_ID %in% fg_em$PERMIT & STRATA == "ZERO", "ZERO", STRATA_NEW))
# Designate STRATA_NEW for vessels that have opted out or been removed from fixed gear EM
# TODO str_remove seems to be the only tidyverse function used here, can use gsub instead?
work.data <- mutate(work.data, STRATA_NEW = ifelse(!(VESSEL_ID %in% fg_em$PERMIT) & STRATA %in% c("EM_HAL", "EM_POT"), gsub("EM_", "", STRATA), STRATA_NEW))
# Do the rest
work.data <- mutate(work.data, STRATA_NEW = ifelse(CVG_NEW == "FULL" & STRATA == "EM_TRW_EFP" & is.na(STRATA_NEW), "FULL", STRATA_NEW))
work.data <- mutate(work.data, STRATA_NEW = ifelse(CVG_NEW == "FULL" & STRATA %in% c("FULL", "TRW") & is.na(STRATA_NEW), "FULL", STRATA_NEW))
work.data <- mutate(work.data, STRATA_NEW = ifelse(CVG_NEW == "FULL" & VESSEL_ID %in% BSAIVoluntary$VESSEL_ID & is.na(STRATA_NEW), "FULL", STRATA_NEW))
work.data <- mutate(work.data, STRATA_NEW = ifelse(CVG_NEW == "PARTIAL" & STRATA %in% c("EM", "EM_HAL") & AGENCY_GEAR_CODE == "HAL" & is.na(STRATA_NEW), AGENCY_GEAR_CODE, STRATA_NEW))
work.data <- mutate(work.data, STRATA_NEW = ifelse(CVG_NEW == "PARTIAL" & STRATA %in% c("HAL", "TenH") & is.na(STRATA_NEW), "HAL", STRATA_NEW))
work.data <- mutate(work.data, STRATA_NEW = ifelse(CVG_NEW == "PARTIAL" & STRATA %in% c("POT", "TenP") & is.na(STRATA_NEW), "POT", STRATA_NEW))
work.data <- mutate(work.data, STRATA_NEW = ifelse(CVG_NEW == "PARTIAL" & STRATA %in% c("TRW", "TenTR", "EM_TRW_EFP") & is.na(STRATA_NEW), "TRW", STRATA_NEW))
work.data <- mutate(work.data, STRATA_NEW = ifelse(CVG_NEW == "PARTIAL" & STRATA == "ZERO" & is.na(STRATA_NEW), "ZERO", STRATA_NEW))
work.data <- mutate(work.data, STRATA_NEW = ifelse(VESSEL_ID == 12588, "HAL", STRATA_NEW))
work.data <- left_join(
  work.data,
  work.data %>%
   group_by(TRIP_ID, PORT_CODE) %>% 
   summarise(WEIGHT = sum(WEIGHT_POSTED, na.rm = TRUE)) %>% 
   group_by(TRIP_ID) %>%
   summarise(PORT_NEW = PORT_CODE[WEIGHT == max(WEIGHT)]))
# Any more NAs in STRATA_NEW?
if( nrow(work.data[is.na(STRATA_NEW)]) ) stop("Still some NAs in 'STRATA_NEW'!")

# * Check for more than one STRATA or STRATA_NEW within TRIP_IDs ----
dups <- work.data %>% 
  select(ADP, TRIP_ID, VESSEL_ID, STRATA, STRATA_NEW, LENGTH_OVERALL) %>% 
  distinct() %>% 
  filter(TRIP_ID %in% TRIP_ID[duplicated(TRIP_ID)]) %>% 
  arrange(TRIP_ID) 
if(nrow(dups) != 0){
  print(dups)
  warning("More than one STRATA or STRATA_NEW within some TRIP_IDs")
}
# Split trips that fished both full and partial coverage  
work.data <- copy(work.data)[TRIP_ID %in% work.data[STRATA_NEW == "FULL", TRIP_ID], N := uniqueN(STRATA_NEW), by = .(TRIP_ID)
][N > 1, TRIP_ID := ifelse(STRATA_NEW != "FULL", round(as.numeric(TRIP_ID) + 0.1, 1), round(as.numeric(TRIP_ID) + 0.2, 1))
][, N := NULL]
# For remaining combo strata trips, default to stratum with the most landed weight  
work.data[, N := uniqueN(STRATA_NEW), by = .(TRIP_ID)
][N > 1, STRATA_WEIGHT := sum(WEIGHT_POSTED[SOURCE_TABLE == "Y"]), by = .(TRIP_ID, STRATA_NEW)
][N > 1, ':=' (COVERAGE_TYPE = COVERAGE_TYPE[which.max(STRATA_WEIGHT)], STRATA_NEW = STRATA_NEW[which.max(STRATA_WEIGHT)]), by = .(TRIP_ID)
][, ':=' (N = NULL, STRATA_WEIGHT = NULL)]
# Check for any remaining combo strata trips 
unique(work.data[, .(TRIP_ID, STRATA_NEW)])[, .N, by = .(TRIP_ID)][N > 1]


# * Calculate trip durations ----

# Currently, model_trip_duration does its own query of Valhalla and ODDS tables and merges them together with rolling joins - in the future, just use work.data for Valhalla 
# and a separate query for the ODDS tables. Uses actual days observed in ODDS to build a model that can be applied to all observer-pool PC trips in Valhalla
# to estimate observer assignment durations (using primarily trip end - trip start but also gear type, ADP year, and tender status as covariates. 

model_trip_duration <- function(val_data, use_mod="DAYS ~ RAW") {
  # val_data <- work.data[ADP < 2022]; use_mod <- "DAYS ~ RAW"     # for now, since 2022 is not complete
  
  range1 <- min(val_data$ADP); range2 <- max(val_data$ADP)
  
  #=============================#
  ### DATE CHECKING FUNCTION ####
  #=============================#
  # This function is used to help evaluate which joins are good or bad
  # count up number of overlapping (OVER) or mismatching days (MISM))
  cols <- c("TRIP_START", "TRIP_END", "ODDS_START", "ODDS_END")                   # define columns to be fed into date checking function dc()
  dc <- function(sdcols, TYPE){
    dtn <- function(x) as.numeric(as.Date(x))  # converts POSIXct to date and the numeric date
    v_d <- c(dtn(sdcols[[1]]) : dtn(sdcols[[2]]))
    o_d <- c(dtn(sdcols[[3]]) : dtn(sdcols[[4]]))
    o <- length(intersect(v_d, o_d))
    if(TYPE=="o"){return(o)}                                                    # type = overlap, which returns number of overlapping dates
    if(TYPE=="m"){return(max(length(v_d), length(o_d)) - o)}                    # type = mismatch, which returns number of non-overlapping dates
  }
  
  # Pull data, if not already done so
  if(exists("mod_dat", where=.GlobalEnv)==FALSE){
    
    #=======================#
    # VALHALLA DATA PULL ####
    #=======================#
    
    # Load up observed trips in Valhalla
    
    o_val <- work.data[COVERAGE_TYPE == "PARTIAL" & OBSERVED_FLAG == "Y", .(
      ADP, TRIP_ID, CRUISE, PERMIT, AREA = REPORTING_AREA_CODE, GEAR = AGENCY_GEAR_CODE, SECTOR = PROCESSING_SECTOR, 
      TARGET = TRIP_TARGET_CODE, TRIP_START = TRIP_TARGET_DATE, TRIP_END = LANDING_DATE, STRATA, TENDER
    )]
    # o_val <- setDT(dbGetQuery(channel, paste(
    #   "
    #   SELECT DISTINCT
    #     a.adp, a.trip_id, a.cruise, a.permit, a.reporting_area_code AS AREA, a.agency_gear_code AS GEAR,
    #     a.processing_sector AS SECTOR, a.trip_target_code AS TARGET, a.trip_target_date AS TRIP_START, 
    #     a.landing_date AS TRIP_END, a.strata, a.tender
    #   FROM  loki.akr_valhalla a
    #   WHERE a.coverage_type = 'PARTIAL' AND a.observed_flag = 'Y'
    #   "
    # )))
    
    # Change date class
    #o_val[, ':='(TRIP_START = as.Date(TRIP_START), TRIP_END = as.Date(TRIP_END))]
    
    # Trips in valhalla may have multiple gear types and/or targets - These will be kept in the 'o_val' object, but will be simplified in the 'val' object moving forward
    
    val <- copy(o_val)
    val[, GEAR_N := length(unique(GEAR)), by=.(TRIP_ID)]                            # count number of gear types used in each trip
    val[GEAR_N>1, GEAR := ifelse(GEAR=="NPT", "PTR", ifelse(GEAR %in% c("POT","JIG"), "HAL", GEAR)), by=.(TRIP_ID, GEAR)]    # If multiple gear types were used, prioritze PTR over NPT and HAL over POT and JIG
    val <- val[, .(TRIP_START = min(TRIP_START), TRIP_END = max(TRIP_END)), by=.(ADP, PERMIT, GEAR, TRIP_ID, STRATA, TENDER)]  # simplify the start/end dates - removing TARGET here
    
    #val_sub <- val[ADP %in% (range1:range2) & year(TRIP_END)!=(range2+1)]           # subsetting valhalla to only years 2017/2018 to match days paid
    val_sub <- copy(val)
    val_sub[, REF := as.numeric(as.Date(TRIP_START))]; setkey(val_sub, PERMIT, REF) # Create new column that is numeric start date of each trip, set keys
    
    #===============================#
    ### ODDS SEA DAYS DATA PULL #####
    #===============================#
    
    # Pull billed sea days from ODDS - These match almost exactly with days_paid
    # Runs from 2015 to date
    sea_day <- setDT(dbGetQuery(channel, paste(
      "
      SELECT cruise, permit, all_dates, at_sea_day_count_odds AS odds_days, gear_type, odds_trip_plan_log_seq 
      FROM norpac.odds_billdays_2015_mv
      "
    )))
    sea_day <- sea_day[!is.na(ODDS_DAYS)]
    sea_day[, YEAR := year(ALL_DATES)]
    # sea_day[, .(DAYS = sum(ODDS_DAYS)), keyby=.(YEAR)]                            # 2017 and 2018 match almost perfect with days_paid (2748.5 and 3206.5)
    # odds_trip_plan_log_seq is not a numeric column - it can have multiple odds trip numbers (i.e. if one trip ended but another started on the same day!)
    sea_day[, c("A", "B") := tstrsplit(ODDS_TRIP_PLAN_LOG_SEQ, split=", ")]         # Where "A" is the first odds seq and "B" is the second odds seq if present, NA if not      
    sea_day[, FLAG := !(is.na(B))]                                                  # If a second odds seq exists, flag as TRUE 
    sea_day <- melt(sea_day, id.vars=c("CRUISE", "PERMIT", "ALL_DATES", "ODDS_DAYS", "GEAR_TYPE", "YEAR", "FLAG"), measure.vars = c("A", "B"), value.name = "ODDS_SEQ") # Melt so each odds seq has its own row
    sea_day  <- sea_day [!is.na(ODDS_SEQ)]                                          # remove extraneous rows without secondary odds_seq
    sea_day [FLAG==TRUE, ODDS_DAYS := ODDS_DAYS/2]                                  # if flagged for having two odds_seq, halve the day duration, so each trip gets a half day
    day_sum <- sea_day[, .(DAYS = sum(ODDS_DAYS), ODDS_START = min(ALL_DATES), ODDS_END = max(ALL_DATES)), by=.(CRUISE, PERMIT, ODDS_SEQ, GEAR_TYPE)]
    
    # THIS WAS ADDED BY GM 2023-Jan-11 
    day_sum[, c("ODDS_START", "ODDS_END") := lapply(.SD, as.Date), .SDcols = c("ODDS_START", "ODDS_END")]
    
    odds_sub <- day_sum[year(ODDS_START) %in% (range1:range2) & year(ODDS_END) %in% (range1:range2)]  # get odds days when trip start is within 2017/2018 only - 2016 is kind of a mess!
    odds_sub[, ODDS_SEQ := as.numeric(ODDS_SEQ)]                                    # convert odds_seq to numeric for easier comparison
    # odds_sub[, .(DAYS = sum(DAYS)), keyby=.(YEAR = year(ODDS_START))]             # Totals by year
    odds_sub[, REF := as.numeric(as.Date(ODDS_START))]; setkey(odds_sub, PERMIT, REF)  
    
    # Very rarely (one case found in 2016), a single odds_seq might have more than one record, in this case, two cruises existed under the same seq
    # The total number of observer days for the seq was a large outlier (13.5 days for a 2-day fishing trip) - Such cases will be excluded
    if(sum(duplicated(odds_sub$ODDS_SEQ))>0){message("Some ODDS_SEQ have duplicate records.")}
    odds_dup <- odds_sub[duplicated(odds_sub$ODDS_SEQ), ODDS_SEQ]
    #  odds_sub[ODDS_SEQ %in% odds_dup]                                                # These records will be excluded!
    if(length(odds_dup)>0){message(paste("Duplicated ODDS_SEQ:", paste(odds_dup, collapse=", "), "were removed."))}
    odds_sub <- odds_sub[!(ODDS_SEQ %in% odds_dup)]
    
    #==================#
    ### FIRST LEVEL ####
    #==================#
    
    # First pass, with rolling join of 3 (allow odds_start to be up to 3 days prior to trip start)
    test1 <- val_sub[odds_sub, roll=-3, nomatch=NULL]
    test1[, ':=' (O = dc(.SD, "o"), M = dc(.SD, "m")), .SDcols=cols, by=.(TRIP_ID, ODDS_SEQ)]  # count up overlapping and mismatching days for each join
    
    # Make sure each odds_seq has only one match - if not, exclude joins with zero overlap, and then identify the join with the best fit
    #  test1[O==0]                                                                # Will exclude these trips
    test1[, ODDS_N := .N, by=.(ODDS_SEQ)]                                         # For each odds_seq count up number of joins. If > 1, remove the join with the worse fit
    test1[, CHK := ifelse(O==0, FALSE, ifelse(ODDS_N==1, TRUE, ifelse(O==max(O) & max(O)!=min(O), TRUE, ifelse(M==min(M) & max(M)!=min(M), TRUE, FALSE)))), by=ODDS_SEQ]   # evaluate fit of each join
    if(nrow(test1[ODDS_N>1 & CHK==TRUE, .(N = .N), by=ODDS_SEQ][N>1])>0) {message("Still have duplicate ODDS_SEQ in first level.")}  # Check to make sure there aren't any ties!
    
    # It may be possible that a multiple odds trips will join to a single valhalla trip, especially for trawl trips with extra offload days
    # In some cases, the odds trip was split between the fishing trip and offload
    # Howvever, this may not always be the case - if multiple trips would have matched in the same rolling period, only the closest one would be returned
    # For now, keep only the best join based on overlap and mismatches and the excluded trips will be joined later via the end date or the final slop
    # If there is a tie with O and M - if ODDS_END > TRIP_END, exclude it as the worse match
    test1[, VAL_N := .N, by=.(TRIP_ID)]
    test1[VAL_N>1, CHK := ifelse(O==max(O) & max(O)!=min(O), TRUE, ifelse(M==min(M) & max(M)!=min(M), TRUE, ifelse(ODDS_END > TRIP_END, FALSE, ifelse(ODDS_START < TRIP_START, FALSE, TRUE)))), by=TRIP_ID]
    if(nrow(test1[VAL_N>1 & CHK==TRUE, .(N = .N), by=TRIP_ID][N>1])>0) {message("Still have duplicate TRIP_ID in first level.")}  # Check to make sure there aren't any ties!
    
    keepers <- test1[CHK==TRUE]                                                   # keep the good joins
    val_r1 <- val_sub[!(TRIP_ID %in% keepers$TRIP_ID)]                            # trim val_sub to include only unjoined trips
    odds_r1 <- odds_sub[!(ODDS_SEQ %in% keepers$ODDS_SEQ)]                        # trim odds_sub to include only unjoined trips
    
    #===================#
    ### SECOND LEVEL ####
    #===================#
    
    # Now try to join the remainders by the end dates
    val_r1[, REF := as.numeric(as.Date(TRIP_END))]; setkey(val_r1, PERMIT, REF)
    odds_r1[, REF := as.numeric(as.Date(ODDS_END))]; setkey(odds_r1, PERMIT, REF)
    
    # Allow the odds_end date to be up to 3 days after the trip end
    test2 <- val_r1[odds_r1, roll=3, nomatch=NULL]   #
    
    test2[, ':=' (O = dc(.SD, "o"), M = dc(.SD, "m")), .SDcols=cols, by=.(TRIP_ID, ODDS_SEQ)]  # count up overlapping and mismatching days for each join
    # Make sure each odds_seq has only one match - if not, exclude joins with zero overlap, and then identify the join with the best fit
    # test2[O==0]                                                                 # Any matches without overlap will be excluded
    test2[, ODDS_N := .N, by=.(ODDS_SEQ)]                                         # For each odds_seq count up number of joins. If > 1, remove the join with the worse fit
    test2[, CHK := ifelse(O==0, FALSE, ifelse(ODDS_N==1, TRUE, ifelse(O==max(O) & max(O)!=min(O), TRUE, ifelse(M==min(M) & max(M)!=min(M), TRUE, FALSE)))), by=ODDS_SEQ]   # evaluate fit of each join
    if(nrow(test2[ODDS_N>1 & CHK==TRUE, .(N = .N), by=ODDS_SEQ][N>1])>0) {message("Still have duplicate ODDS_SEQ in second level.")}  # Check to make sure there aren't any ties!
    
    test2[, VAL_N := .N, by=.(TRIP_ID)]                                           # Count number of records for each trip_id
    test2[VAL_N>1, CHK := ifelse(O==max(O) & max(O)!=min(O), TRUE, ifelse(M==min(M) & max(M)!=min(M), TRUE, ifelse(ODDS_END > TRIP_END, FALSE, TRUE))), by=TRIP_ID]
    if(nrow(test2[VAL_N>1 & CHK==TRUE, .(N = .N), by=TRIP_ID][N>1])>0) {message("Still have duplicate TRIP_ID in second level.")}  # Check to make sure there aren't any ties!
    
    keepers <- rbind(keepers, test2[CHK==TRUE])
    val_r2 <- val_sub[!(TRIP_ID %in% keepers$TRIP_ID)]                              # trim val_sub to include only unjoined trips
    odds_r2 <- odds_sub[!(ODDS_SEQ %in% keepers$ODDS_SEQ)]                          # trim odds_sub to include only unjoined trips
    
    #==================#
    ### THIRD LEVEL ####
    #==================#
    
    # Next, try joining using nearest with remaining trips and keeping only those with overlap
    # Sometimes, especially for tender trips, the embark/disembark date will be a few days inside of trip/start and therefore not join sooner
    
    # First try to fit to val_r2 but with roll="nearest"
    test3 <- val_r2[odds_r2, roll="nearest", nomatch=NULL]
    test3[, ':=' (O = dc(.SD, "o"), M = dc(.SD, "m")), .SDcols=cols, by=.(TRIP_ID, ODDS_SEQ)]  # count up overlapping and mismatching days for each join
    # Make sure each odds_seq has only one match - if not, exclude joins with zero overlap, and then identify the join with the best fit
    # test3[O==0]                                                                 # Any matches without overlap will be excluded
    test3[, ODDS_N := .N, by=.(ODDS_SEQ)]                                         # For each odds_seq count up number of joins. If > 1, remove the join with the worse fit
    test3[, CHK := ifelse(O==0, FALSE, ifelse(ODDS_N==1, TRUE, ifelse(O==max(O) & max(O)!=min(O), TRUE, ifelse(M==min(M) & max(M)!=min(M), TRUE, FALSE)))), by=ODDS_SEQ]   # evaluate fit of each join
    if(nrow(test3[ODDS_N>1 & CHK==TRUE, .(N = .N), by=ODDS_SEQ][N>1])>0) {message("Still have duplicate ODDS_SEQ in second level.")}  # Check to make sure there aren't any ties!
    
    test3[, VAL_N := .N, by=.(TRIP_ID)]                                           # Count number of times each TRIP_ID was matched
    # if I have multiple odds_seq for the same trip_id that both have overlap, simplify the dates, sum the days, and store the odds_seq that is higher
    third_join <- test3[O != 0]
    third_join[, VAL_N := .N, by=.(TRIP_ID)]  # recalculate VAL_N after excluding the matches with zero overlap
    third_join[, CHK := ifelse(ODDS_SEQ==min(ODDS_SEQ), TRUE, FALSE), by=TRIP_ID]   # Flag the secondary odds_seq
    odds_seq_keep <- third_join[CHK==FALSE, ODDS_SEQ]                               # Store this odds_seq that is being combined with another
    third_join[VAL_N>1, ':=' (ODDS_START =min(ODDS_START), ODDS_END = max(ODDS_END), DAYS = sum(DAYS)), by=.(TRIP_ID, CRUISE)]   # If obs_seq for the same trip_id and cruise, simplify
    
    keepers <- rbind(keepers, third_join[CHK==TRUE])
    odds_r3 <- odds_sub[!(ODDS_SEQ %in% keepers$ODDS_SEQ | ODDS_SEQ %in% odds_seq_keep)]
    val_r3  <- val_sub[!(TRIP_ID %in% keepers$TRIP_ID)]
    
    #===================#
    ### FOURTH LEVEL ####
    #===================#
    
    # Next, try joining remaining trips with currently existing joins (i.e., the assignment was split into separete obs_seq records)
    # Permit and cruise must match, and try joining using nearest from each side
    odds_r3[, REF := as.numeric(as.Date(ODDS_END))] # use disembark as ref
    tack <- copy(odds_r3)[, .(PERMIT, A_CRUISE = CRUISE, A_ODDS_SEQ = ODDS_SEQ, A_DAYS = DAYS, A_ODDS_START = ODDS_START, A_ODDS_END = ODDS_END, REF)]
    setkey(tack, PERMIT, REF)
    
    exis <- copy(keepers)
    exis[, REF := as.numeric(as.Date(ODDS_START))]; setkey(exis, PERMIT, REF)
    
    # Tacking on from left side (allow disembark date to be within 3 days prior to existing embark date)
    test4 <- exis[tack, roll=-3, nomatch=NULL]
    test4[, VAL_N := .N, by=TRIP_ID]  # some of these might have multiple joins that can be simplified
    if(nrow(test4[VAL_N>1]) >0) {message("Multiple TRIP_ID in the fifth level - must deal with this because this was not an issue before, so no code exists to handle this!")}
    test4[, ':=' (ODDS_START = min(ODDS_START, A_ODDS_START), ODDS_END = max(ODDS_END, A_ODDS_END), DAYS = sum(DAYS, A_DAYS)), keyby=.(ODDS_SEQ, TRIP_ID)]  # Replace existing data
    
    setkey(keepers, ODDS_SEQ, TRIP_ID)  # reorder keepers by odds_seq and trip_id so that replaced values will merge in the correct order
    keepers[ODDS_SEQ %in% test4$ODDS_SEQ, ':=' (ODDS_START = test4$ODDS_START, ODDS_END = test4$ODDS_END, DAYS = test4$DAYS)]
    odds_seq_keep <- c(odds_seq_keep, test4$A_ODDS_SEQ)  # Keep the odds_seq in this secondary list
    
    odds_r4 <- odds_sub[!(ODDS_SEQ %in% keepers$ODDS_SEQ | ODDS_SEQ %in% odds_seq_keep)]
    val_r4  <- val_sub[!(TRIP_ID %in% keepers$TRIP_ID)]
    
    #==================#
    ### FIFTH LEVEL ####
    #==================#
    odds_r4[, REF := as.numeric(as.Date(ODDS_START))] # use new embark as ref
    tack2 <- copy(odds_r4)[, .(PERMIT, A_CRUISE = CRUISE, A_ODDS_SEQ = ODDS_SEQ, A_DAYS = DAYS, A_ODDS_START = ODDS_START, A_ODDS_END = ODDS_END, REF)]
    setkey(tack2, PERMIT, REF)
    
    exis <- copy(keepers)
    exis[, REF := as.numeric(as.Date(ODDS_END))]; setkey(exis, PERMIT, REF)
    
    # Tacking on from right side, but also using "nearest" in case tender trips had observers disembark before end of trip
    test5 <- exis[tack2, roll="nearest", nomatch=NULL]
    
    a_cols <- c("TRIP_START", "TRIP_END", "A_ODDS_START", "A_ODDS_END") 
    test5[, ':=' (A_O = dc(.SD, "o"), A_M = dc(.SD, "m")), .SDcols=a_cols, by=.(TRIP_ID, A_ODDS_SEQ)]
    
    # If tacked on trips have no overlap, first check to see if the existing disembark is within a reasonable time period (2-days?)
    test5[A_O==0, CHK := ifelse(abs(as.numeric(A_ODDS_START - ODDS_END)) <=2, TRUE, FALSE)]
    if(nrow(test5[CHK==FALSE]) > 0 ){message("Some matches were removed in the fifth level for having no overlap and being out of 2-day range - probably a good thing, but double-check this.")}
    #     test5[CHK==FALSE, .(PERMIT, TRIP_ID, TRIP_START, TRIP_END, ODDS_SEQ, CRUISE, ODDS_START, ODDS_END, A_ODDS_SEQ, A_CRUISE, A_ODDS_START, A_ODDS_END)]  # DOUBLE-CHECK THESE
    test5 <- test5[CHK==TRUE]
    
    test5[, VAL_N := .N, by=TRIP_ID]  # some of these might have multiple joins that can be simplified
    test5[, ':=' (A_ODDS_START = min(A_ODDS_START), A_ODDS_END = max(A_ODDS_END), A_DAYS = sum(A_DAYS)), keyby=.(ODDS_SEQ, TRIP_ID)] # simplify
    test5[VAL_N>1, CHK := ifelse(A_ODDS_SEQ == min(A_ODDS_SEQ), TRUE, FALSE), by=.(ODDS_SEQ, TRIP_ID)]       # Now that things are simplified, keep only one record
    odds_seq_keep <- unique(c(odds_seq_keep, test5$A_ODDS_SEQ))                     # Store the tacked on odds_seq
    test5 <- test5[CHK==TRUE]                                                       # Remove extra rows 
    test5[, ':=' (ODDS_START = min(ODDS_START, A_ODDS_START), ODDS_END = max(ODDS_END, A_ODDS_END), DAYS = sum(DAYS, A_DAYS)), keyby=.(ODDS_SEQ, TRIP_ID)]
    setkey(test5, ODDS_SEQ, TRIP_ID)
    
    setkey(keepers, ODDS_SEQ, TRIP_ID)  # reorder keepers by odds_seq and trip_id so that replaced values will merge in the correct order
    keepers[ODDS_SEQ %in% test5$ODDS_SEQ, ':=' (ODDS_START = test5$ODDS_START, ODDS_END = test5$ODDS_END, DAYS = test5$DAYS)]
    
    odds_r5 <- odds_sub[!(ODDS_SEQ %in% keepers$ODDS_SEQ | ODDS_SEQ %in% odds_seq_keep)]
    val_r5  <- val_sub[!(TRIP_ID %in% keepers$TRIP_ID)]
    
    #==========================#
    ### n_val - FIRST LEVEL ####
    #==========================#
    
    # there are several cases where trips were found in valhalla but had observed_flag='N', which must be incorrect
    # Can dig these out of valhalla without too much trouble
    
    lookup <- unique(odds_r5[, .(PERMIT, ADP = year(ODDS_START))])
    lookup_val_scratch <- lookup[ADP==range2, PERMIT]
    lookup_val <- lookup[ADP<range2, PERMIT]
    
    # Looking for trip labeled as non-observed (but actually were)
    
    n_val <- val_data[COVERAGE_TYPE == "PARTIAL" & OBSERVED_FLAG == "N", .(
      ADP, TRIP_ID, CRUISE, PERMIT, AREA = REPORTING_AREA_CODE, GEAR = AGENCY_GEAR_CODE, SECTOR = PROCESSING_SECTOR, 
      TARGET = TRIP_TARGET_CODE, TRIP_START = TRIP_TARGET_DATE, TRIP_END = LANDING_DATE, STRATA, TENDER
    )]

    # n_val <- setDT(dbGetQuery(channel, paste(
    #   "
    #   SELECT DISTINCT
    #     a.adp, a.trip_id, a.cruise, a.permit, a.reporting_area_code AS AREA, a.agency_gear_code AS GEAR,
    #     a.processing_sector AS SECTOR, a.trip_target_code AS TARGET, a.trip_target_date AS TRIP_START, 
    #     a.landing_date AS TRIP_END, a.strata, a.tender
    #   FROM  loki.akr_valhalla a
    #   WHERE a.coverage_type = 'PARTIAL' AND a.observed_flag = 'N'
    #   "
    # )))
    
    # Change date class
    n_val[, ':='(TRIP_START = as.Date(TRIP_START), TRIP_END = as.Date(TRIP_END))]
    
    n_val <- n_val[(ADP < range2 & PERMIT %in% lookup_val) | (ADP == range2 & PERMIT %in% lookup_val_scratch)]
    
    n_val[, YEAR := year(TRIP_START)]                                               # for valhalla, define year by trip_start
    n_val <- n_val[YEAR >= (range1) & YEAR <= (range2)]                             # Trim valhalla,  using the most recent 3 full years of data
    n_val[, GEAR_N := length(unique(GEAR)), by=.(TRIP_ID)]                            # count number of gear types used in each trip
    n_val[GEAR_N>1, GEAR := ifelse(GEAR=="NPT", "PTR", ifelse(GEAR=="POT", "HAL", GEAR)), by=.(TRIP_ID, GEAR)]    # If multiple gear types were used, prioritze PTR over NPT and HAL over POT
    n_val <- n_val[, .(TRIP_START = min(TRIP_START), TRIP_END = max(TRIP_END)), by=.(ADP, PERMIT, GEAR, TRIP_ID, STRATA, TENDER)]  # simplify the start/end dates - removing TARGET here
    n_val[, REF := as.numeric(as.Date(TRIP_START))]; setkey(n_val, PERMIT, REF)     # Create new column that is numeric start date of each trip, set keys
    
    # First pass, with rolling join of 3 (allow odds_start to be up to 3 days prior to trip start)
    odds_r5[, REF := as.numeric(as.Date(ODDS_START))]; setkey(odds_r5, PERMIT, REF)
    
    n_val1 <- n_val[odds_r5, roll="nearest", nomatch=NULL]   
    n_val1[, ':=' (O = dc(.SD, "o"), M = dc(.SD, "m")), .SDcols=cols, by=.(TRIP_ID, ODDS_SEQ)]  # count up overlapping and mismatching days for each join
    n_val1 <- n_val1[O != 0]                                                         # Retain only matches with some overlap
    n_val1 <- n_val1[!n_val1[, .SD[.N>1], by=ODDS_SEQ][, .SD[O == min(O) & M == max(M)], by = .(TRIP_ID)], on = .(TRIP_ID, ODDS_SEQ)] # For ODDS_SEQs with more than one matched TRIP_ID, exclude the match with the least overlap and most mismatch
    n_val1[, ODDS_N := .N, by=ODDS_SEQ]
    n_val1[, CHK := TRUE]
    n_val1[, VAL_N := .N, by=TRIP_ID]
    if(nrow(n_val1[ODDS_N>1])>0){message("Multiple ODDS_SEQ matches in n_val level 1 - I didn't code for this!")}
    #   n_val1[ODDS_N>1]
    
    # When multiple odds_seq match to the same trip_id...
    #   n_val1[VAL_N>1]
    n_val1[VAL_N>1, ':=' (ODDS_START =min(ODDS_START), ODDS_END = max(ODDS_END), DAYS = sum(DAYS)), by=.(TRIP_ID, CRUISE)]   # If odds_seq for the same trip_id and cruise, simplify
    n_val1[VAL_N>1, CHK := ifelse(ODDS_SEQ==max(ODDS_SEQ), FALSE, TRUE), by=.(TRIP_ID, CRUISE)]
    odds_seq_keep <- c(odds_seq_keep, n_val1[VAL_N>1 & CHK==FALSE, ODDS_SEQ])
    n_val1 <- n_val1[CHK==TRUE]
    
    keepers <- rbind(keepers, n_val1)                                             # Final object with all joins
    odds_r6 <- odds_sub[!(ODDS_SEQ %in% keepers$ODDS_SEQ | ODDS_SEQ %in% odds_seq_keep)]   # remaining unmatched odds trips
    val_r6  <- val_sub[!(TRIP_ID %in% keepers$TRIP_ID)]                           # remaining trips in valhalla
    
    mod_dat <- copy(keepers)
    
    match_prop <- round(100*(nrow(mod_dat) - nrow(odds_r6)) / nrow(mod_dat),3)
    message(paste("Matched ", match_prop, " percent of ODDS and Valhalla trips."))
  }
  
  #=============#
  # MODELING ####
  #=============#
  
  mod_dat[, RAW := as.numeric(TRIP_END - TRIP_START, units="days")]             # Calculate raw day difference of trip_end and trip_start
  mod_dat[, ':=' (AGENCY_GEAR_CODE = factor(GEAR, levels=c("HAL", "POT", "PTR", "NPT")), TENDER = as.factor(TENDER), ADP = as.factor(ADP))]
  mod_dat[, GEAR := factor(GEAR, levels=c("HAL", "POT", "PTR", "NPT"))]
  #  table(mod_dat$DAYS)  # Only a handful of assignments were limited to 0.5 days - many of these were probably split between different trips!
  #  mod_dat[, .N, by=ADP]   # There is as much data in 2016 as 2017 and 2018 combined... modeling may need to take this into consideration!
  
  mod <- lm(as.formula(use_mod), data = mod_dat)   
  #  summary(mod)                                # ADP alone was not significant, but GEAR*ADP wasa, especially for POT and somewhat for PTR
  #  plot(mod2)  # The residuals and leverage plot looks good, better than DAYS ~ RAW + GEAR alone
  mod_dat[, MOD := round(predict(mod)/0.5)*0.5]  # round model outputs to the nearest 0.5
  mod_dat[MOD <=0, MOD := 0.5]                  # If a predicted duration is <= 0, make minimum of 0.5 days
  mod_dat_for_plt <- melt(mod_dat, id.vars=c("ADP", "PERMIT", "GEAR", "TRIP_ID", "STRATA", "TENDER", "AGENCY_GEAR_CODE", "DAYS"), measure.vars = c("RAW", "MOD"))
  mod_dat_for_plt[, GEAR := factor(GEAR, levels=c("HAL", "POT", "PTR", "NPT"))]
  mod_dat_for_plt[, CLR := factor(ifelse(variable=="RAW", "Raw", ifelse(TENDER=="N", "NonTen", "Tender")), levels=c("Raw", "NonTen", "Tender"))]
  
  td_plot <- ggplot(mod_dat_for_plt[order(CLR)], aes(x=DAYS, y=value, shape=TENDER, group=TRIP_ID)) + geom_line(color="gray") + geom_point(aes(color=CLR), position=position_jitter(width=0.2, height=0.2), alpha=0.5) +  facet_grid(GEAR ~ ADP) + scale_x_continuous(limits=c(0, 20), breaks=seq(0, 20, 2), minor_breaks=NULL) + scale_y_continuous(limits=c(0, 20), breaks=seq(0, 20, 2), minor_breaks=NULL) + geom_abline(slope=1) + labs(subtitle=paste(use_mod), y="Value", x="Actual Days")+ scale_color_manual(values=c("gray", "blue", "red"))
  # old plot, without RAW or lines
  # td_plot <- ggplot(mod_dat, aes(x=DAYS, y=MOD, color=TENDER)) + geom_point(position=position_jitter(width=0.2, height=0.2)) + facet_grid(GEAR ~ ADP) + scale_x_continuous(limits=c(0, 20), breaks=seq(0, 20, 2), minor_breaks=NULL) + scale_y_continuous(limits=c(0, 20), breaks=seq(0, 20, 2), minor_breaks=NULL) + geom_abline(slope=1)
  # The fit looks prety centered... but PTR is pretty unpredictable
  
  diagnostic1 <- mod_dat[, .(DAYS = sum(DAYS), MOD = sum(MOD)), keyby=.(ADP)][, .(ADP, DAYS, MOD, PERC = 100*(MOD-DAYS)/DAYS)]  # pretty dang close... in 2018, PTRis under by 10%
  diagnostic2 <- mod_dat[, .(DAYS = sum(DAYS), MOD = sum(MOD)), keyby=.(ADP, GEAR)][, .(ADP, GEAR, DAYS, MOD, PERC = 100*(MOD-DAYS)/DAYS)] 
  # Adding GEAR*ADP interaction term keeps all percent diff +/- 5%, which is better than previous models
  
  #============#
  # OUTPUTS ####
  #============#
  
  # export mod_dat to global environent - this also allows this function to be re-used if needed, but will skip the database query step
  assign("mod_dat", mod_dat, envir=.GlobalEnv)
  
  return(list(TD_MOD = mod, TD_PLOT = td_plot, DIAG = list(YEAR = diagnostic1, YEAR_GEAR = diagnostic2)))
  
}
# First, remove the td_mod that we loaded with the Final 2023 ADP data
rm(td_mod)
work.data[, PERMIT := as.character(PERMIT)]  # model_trip_duration needs character class PERMIT

# initial run performs the queries and joins and runs with a default model of DAYS ~ RAW. Spits 'mod_dat' to the global environment
td_init <- model_trip_duration(val_data = work.data)  
td_init$TD_PLOT
td_init$DIAG
# After some testing, the model below was decided as the best for predicting trip durations
td_mod <- model_trip_duration(val_data = work.data, use_mod="DAYS ~ RAW + ADP*AGENCY_GEAR_CODE")
td_mod$TD_PLOT
td_mod$DIAG     
# Subset all observer pool trips for the most recent 3 years
td_dat <- unique(work.data[ADP & CVG_NEW!="FULL" & STRATA_NEW != "ZERO" & AGENCY_GEAR_CODE != "JIG", .(VESSEL_ID, ADP, AGENCY_GEAR_CODE, START=min(TRIP_TARGET_DATE, LANDING_DATE, na.rm=TRUE), END=max(TRIP_TARGET_DATE, LANDING_DATE, na.rm=TRUE), TENDER), by=.(TRIP_ID)])                                          # Subset this to get full 3 years of prior fishing effort
td_dat[, RAW := as.numeric(END-START, units="days")]                              # calculate raw trip duration (fishing end minus fishing start)
td_dat[, ADP := as.factor(ADP)]                                                   # convert ADP to a factor so it is handled by the model correctly
td_dat[, AGENCY_GEAR_CODE := factor(AGENCY_GEAR_CODE, levels=c("HAL", "POT", "PTR", "NPT"))]   # Set factor levels

# Apply the model to predict trip duration
td_dat[, MOD := round(predict(td_mod$TD_MOD, newdata=td_dat)/0.5)*0.5]              # Predict trip durations for each trip, and round to the nearest 0.5 days
message(paste(nrow(td_dat[MOD < 0.5]), "trips had estimated trip durations less than 0.5 days"))
td_dat[MOD < 0.5]                                                                 # These are about to get coerced to 0.5 days - make sure this is reasonable!
td_dat[MOD < 0.5, MOD := 0.5]                                                     # I'm doing it!

# Check the model fit - make sure durations make reasonable sense - limiting axes to exclude outliers
ggplot(td_dat, aes(x=RAW, y=MOD, color=TENDER)) + geom_point(position=position_jitter(width=0.2, height=0.2)) + facet_grid(AGENCY_GEAR_CODE ~ ADP) + xlim(0, 30) + ylim(0, 30) + geom_abline(slope=1) + scale_color_manual(values=c("blue", "red"))
# Notice that tendered PTR/NPT trips have low slops and don't track with RAW. This behavior appears consistent with actuals in ODDS (td_mod$TD_PLOT)

# Some trips may have had more than one gear type (typically HAL+POT and PTR+NPT) The trip durations for these trips will be averaged across both instances
# and rounded to the nearest 0.5 (e.g., an averaged duration of 3.25 would be rounded down to 3.00, but a duration of 3.75 would be rounded up to 4.00)
td_dat[TRIP_ID %in% td_dat[, .(N = .N), by=TRIP_ID][N>1, TRIP_ID]][order(TRIP_ID)]
td_out <- td_dat[, .(DAYS = round(mean(MOD)/0.5)*0.5), keyby=.(TRIP_ID)]          # Final output of estimated trip duration

# -----------------------------------------------------------------------------------------------------------------#
# * Get TRIP_IDs for PSC ----
# FIXME - Can't do this because 'PSC' object is created from AKRO database!
# -----------------------------------------------------------------------------------------------------------------#

# Metrics:
# 1. hlbt_psc - estimated halibut bycatch (t)
# 2. chnk_psc - estimated chinook salmon bycatch (counts)

# 1. hlbt
hlbt_psc <-
  inner_join(
    # 1.1 Get distinct TRIP_ID / REPORT_ID pairs
    work.data %>% distinct(TRIP_ID, REPORT_ID),
    # 1.2 Merge those TRIP_IDs with REPORT_IDs that have halibut PSC
    filter(PSC, SPECIES_GROUP_CODE == "HLBT"), by = "REPORT_ID") %>% 
  # 1.3 Sum weights by TRIP_ID
  group_by(TRIP_ID) %>% 
  summarize(hlbt_psc = sum(PSC_TOTAL_CATCH)) %>%
  filter(hlbt_psc > 0)

#2. chnk
chnk_psc <-
  inner_join(
    # 1.1 Get distinct TRIP_ID / REPORT_ID pairs
    work.data %>% distinct(TRIP_ID, REPORT_ID),
    # 1.2 Merge those TRIP_IDs with REPORT_IDs that have halibut PSC
    filter(PSC, SPECIES_GROUP_CODE == "CHNK"), by = "REPORT_ID") %>% 
  # 1.3 Sum weights by TRIP_ID
  group_by(TRIP_ID) %>% 
  summarize(chnk_psc = sum(PSC_TOTAL_CATCH)) %>%
  filter(chnk_psc > 0)

#3. groundfish discards
discard <-
  left_join(
    # 1.1 Sum groundfish discards by TRIP_ID, REPORT_ID, and SPECIES_GROUP_CODE
    work.data %>% 
      filter(SOURCE_TABLE == "N" & SPECIES_GROUP_CODE != "SCLP" & (GROUNDFISH_FLAG == "Y" | SPECIES_GROUP_CODE == "HLBT")) %>% 
      group_by(ADP, TRIP_ID, REPORT_ID, SPECIES_GROUP_CODE) %>% 
      summarize(WEIGHT_POSTED = sum(WEIGHT_POSTED, na.rm = TRUE), .groups = 'drop'),
    # 1.2 Left join to PSC
    PSC, by = c("REPORT_ID", "SPECIES_GROUP_CODE")) %>% 
  # 1.3 Remove PSC mortality weight
  mutate(WEIGHT_POSTED = WEIGHT_POSTED - replace_na(PSC_TOTAL_MORTALITY_WEIGHT, 0)) %>% 
  # 1.4 Sum weights by TRIP_ID
  group_by(TRIP_ID) %>% 
  summarize(discard = sum(WEIGHT_POSTED, na.rm = TRUE)) %>%
  filter(discard > 0)

# Merge optimization metrics together
metrics <- full_join(hlbt_psc, chnk_psc, on = "TRIP_ID") %>% full_join(discard, on = "TRIP_ID")



# -----------------------------------------------------------------------------------------------------------------#

# * Create trip data frames ----

# Check for duplicate STRATA_NEW or TENDER entries before merging work.data with metrics
if(nrow(
  distinct(work.data, TRIP_ID, ADP, STRATA_NEW, TENDER) %>% 
  group_by(TRIP_ID) %>% 
  filter(n() > 1)) > 0){
  stop("Multiple distinct ADP, STRATA_NEW, or TENDER by TRIP_ID in work.data")}
work.data <- work.data %>% 
  mutate(P_STRATA = ifelse(STRATA_NEW == "POT" & TENDER == "Y", "TenP", STRATA_NEW)) %>% 
  mutate(P_STRATA = ifelse(STRATA_NEW == "TRW" & TENDER == "Y", "TenTR", P_STRATA))

# * efrt object ----

wd <- unique(
  work.data[CVG_NEW == "PARTIAL", .(
    PERMIT = VESSEL_ID, TARGET = TRIP_TARGET_CODE, AREA = REPORTING_AREA_CODE, AGENCY_GEAR_CODE, 
    GEAR = ifelse(AGENCY_GEAR_CODE %in% c("PTR", "NPT"), "TRW", AGENCY_GEAR_CODE), 
    STRATA = STRATA_NEW, TRIP_TARGET_DATE, LANDING_DATE, ADFG_STAT_AREA_CODE
    ), keyby = .(ADP, TRIP_ID)])
pc_trip_id_count <- uniqueN(work.data[CVG_NEW == "PARTIAL", TRIP_ID])

# Identify which FMP had most retained catch, splitting FMP by BSAI and GOA
fmp_bsai_goa <- work.data[CVG_NEW == "PARTIAL", .(
  FMP_WT = sum(WEIGHT_POSTED[SOURCE_TABLE == "Y"], na.rm = T)
  ), by = .(TRIP_ID, BSAI_GOA = FMP)][, .SD[which.max(FMP_WT)], by = .(TRIP_ID)][, FMP_WT := NULL][]
if( (pc_trip_id_count != uniqueN(fmp_bsai_goa$TRIP_ID) ) | (pc_trip_id_count != nrow(fmp_bsai_goa)) ) {
  stop("Something went wrong making 'fmp_bsai_goa'")
}

# Identify which FMP had most retained catch, splitting FMP by BS, AI and GOA
fmp_bs_ai_goa <- copy(work.data)[
][CVG_NEW == "PARTIAL", 
][, BS_AI_GOA := fcase(
  REPORTING_AREA_CODE %in% c(541, 542, 543), "AI",
  REPORTING_AREA_CODE %in% c(508, 509, 512, 513, 514, 516, 517, 518, 519, 521, 523, 524), "BS",
  REPORTING_AREA_CODE %in% c(610, 620 ,630, 640, 649, 650, 659), "GOA")  
][, .(
  FMP_WT = sum(WEIGHT_POSTED[SOURCE_TABLE == "Y"], na.rm = T)
), by = .(TRIP_ID, BS_AI_GOA)][, .SD[which.max(FMP_WT)], by = .(TRIP_ID)][, FMP_WT := NULL][]
if( (pc_trip_id_count != uniqueN(fmp_bs_ai_goa$TRIP_ID) ) | (pc_trip_id_count != uniqueN(fmp_bs_ai_goa[!is.na(BS_AI_GOA), TRIP_ID]) ) ) {
  stop("Something went wrong making 'fmp_bs_ai_goa'")
}

wd[
][, POOL := ifelse(STRATA %in% c("EM_HAL", "EM_POT"), "EM", ifelse(STRATA == "ZERO", "ZE", "OB"))
][, DAYS := td_out[wd, DAYS, on=.(TRIP_ID)]] 
if(nrow(wd[is.na(DAYS) & POOL =="OB" & STRATA != "EM_TRW_EFP"]) >0){message("Some OB pool trips are missing trip durations!")} 

wd <- wd[fmp_bsai_goa, on = .(TRIP_ID)][fmp_bs_ai_goa, on = .(TRIP_ID)]

#=========#
# TIME ####

# TODO In future version of define_poststratum, have it apply TIME after spatial units are specified!
# time = c("WEEK", 1) should now be c("week", 1), and apply the week() function to the centers of trips, and any trips
# with 11+ days and 4+ days in a week will get additional weeks.

if(F) {

  # For each trip define WEEK or MONTH based on center of TRIP_TARGET_DATE and LANDING_DATE
  # For trips that fished 11+ days and fished at least 4 days in a week, allow those instances to span more weeks.
  
  # TOTAL_DAYS (find number of unique days between trip_target_Ddate and landing_date)
  tot_days_fun <- function(x) {
    length(unique(unlist(apply(x, 1, function(y) y[1]:y[2], simplify = F))))
  } 
  
  wd[, S := as.numeric(TRIP_TARGET_DATE)]
  wd[, E := as.numeric(LANDING_DATE)]
  wd[, TOT_DAYS := tot_days_fun(.SD), by = .(TRIP_ID), .SDcols = c("S", "E") ]
  
  wd[TOT_DAYS >= 11]
  
  # TODO HAVE THE DEFINE_POSTSTRATUM function do this!
  
  multi_week <- function(x) {
    # identify number of weeks with 4+ days, then return those
    apply(x, 1, function(y) {
      
      sapply(y[1]:y[2], function(z) week(as.Date(z, origin = origin)))
      
    })
      
  }
  wd[TRIP_ID == 924818,]
  wd[TRIP_ID == 924818, multi_week(.SD), by = .(TRIP_ID), .SDcols = c("S", "E")]
  as.numeric(wd[TRIP_ID == 697414,][1, .(S, E)])
  
  as.POSIXlt(18702 : 18704, origin = "origin")
  
  wd[TRIP_ID == 697414, multi_week(.SD), by = .(TRIP_ID), .SDcols = c("TRIP_TARGET_DATE", "LANDING_DATE")]
  wd[, week(.SD), .SDcols = c("TRIP_TARGET_DATE")]
  wd[, class(.SD), .SDcols = c("TRIP_TARGET_DATE")]
  
  #=================#

  # Find unique WEEKS and MONTH within each TRIPP's ADFG_STAT_AREA_CODE and TRIP_TARGET_DATE, LANDING_DATE combination
  week_dt <- unique(wd[, .(TRIP_ID, ADFG_STAT_AREA_CODE, TRIP_TARGET_DATE, LANDING_DATE)])[
  ][, ':=' (
    START = min(TRIP_TARGET_DATE, LANDING_DATE), END = max(TRIP_TARGET_DATE, LANDING_DATE)), 
    by = .(TRIP_ID, TRIP_TARGET_DATE, LANDING_DATE)][]
  week_dt[, TRIP_DAYS := as.numeric(END - START, units = "days")]
  week_dt
  
  
  # Make a rule that if a part of a trip spans more than one week, 
  week_dt <- week_dt[, DAYS := length(TRIP_TARGET_DATE : LANDING_DATE), by = .(TRIP_ID, TRIP_TARGET_DATE, LANDING_DATE)
  ][, .(
    WEEK = seq(week(min(TRIP_TARGET_DATE, LANDING_DATE)), week(max(TRIP_TARGET_DATE, LANDING_DATE)))
    ), by = .(TRIP_ID, ADFG_STAT_AREA_CODE, TRIP_TARGET_DATE, LANDING_DATE, DAYS)
  ][, WEEK_N := uniqueN(WEEK), by = .(TRIP_ID)][]
  
  week_dt[WEEK_N > 1, table(DAYS)]
  week_dt[WEEK_N == 2, table(DAYS)] 
  # Most trips that span 2-weeks are 3-4 day long...
  
  week_dt[WEEK_N == 2 & DAYS %in% 3:4]
  week_dt[TRIP_ID == 698070]   # "2021-03-18" to "2021-03-21". The 18th is week 11, but 19-21 is week 12.
  
  week_dt[WEEK_N %in% 1:2][, uniqueN(TRIP_ID), by = WEEK_N]
  week_dt[, uniqueN(TRIP_ID), keyby = .(WEEK_N)]  # Vast majority of trips have 1 or 2 weeks, but 2 might be a little inflated.
  
  week_dt[WEEK_N == 7][order(TRIP_TARGET_DATE)]
  wd[TRIP_ID == 1587150][order(TRIP_TARGET_DATE)] # Super long GOA TRW trip targetting pollock and cod
  work.data[TRIP_ID == 1587150, table(TENDER)]  # Not a tender trip but delivered to inshore processor and kodiak?
  
  week_dt[WEEK_N == 8, unique(TRIP_ID)]
  
  wd[TRIP_ID == 924818][order(TRIP_TARGET_DATE)] # Same permit
  work.data[TRIP_ID == 924818, table(TENDER)]  # Not a tender trip but delivered to inshore processor and kodiak?
  
  wd[TRIP_ID == 21147714][order(TRIP_TARGET_DATE)] # the Cerulean partial CP - 2019 AI sablefish/halibut trip
  work.data[TRIP_ID == 21147714]    #
  
  wd[TRIP_ID == 896681][order(TRIP_TARGET_DATE)]  # 2021 GOA Pot trip April through May
  work.data[TRIP_ID == 896681]    # This one was actually observed?
  work.data[TRIP_ID == 896681, table(OBSERVED_FLAG)]
  # Looking through NORPAC, I only see that an observer observed Apr 6-13
  # how is this still considered one trip?
  unique(work.data[TRIP_ID == 896681, .(TRIP_ID, REPORT_ID, TRIP_TARGET_DATE, LANDING_DATE, PORT_CODE)])[order(TRIP_TARGET_DATE)]
  
  
  week_dt[WEEK_N==2 & DAYS == 38][order(TRIP_TARGET_DATE)]  # WTF is up with this trip. At least it's not spanning the entire duration?
  work.data[TRIP_ID == "1587342.1"]  # Tender trawl to inshore processor, spans both GOA and BS
  # The 'DAYS' estimate for this trip is not accurate. Would need to count total number of unique days between each
  # trip_target_date and landing_date
  # Odds is even more confusing. Logged a trip in January, was selected, cancelled. Next trips logged Apr 13 and 22, both
  # of these were released by NMFS. Three more trips logged Apr 27, May 5 and 17, and none were observed. Norpac's Offload
  # has the observer offloading on Apr 13 (7598690 and 7598693). Why doesn't ODDS match with this?
  # ODDS_BILLDAYS_2015_MV has: Apr 6-13, ODDS TRIP PLAN LOG = 134764, which doesn't even show in ODDS_LOGGED_TRIP_SUMMARY_V !
  # But it does show in ODDS_MONITOR, inherited from 132692 (this was a multi-area IFQ trip, RN = 0.5095)
  
  # Whatever...Ugh. Get back to making business rules about which weeks trips should belong to!
  
  # For each row, count number of days between TRIP_TARGET_DATE and LANDING_DATE that match the WEEK
  week_dt[, WEEK_DAYS := sum(week(as.Date(TRIP_TARGET_DATE : LANDING_DATE, origin = origin)) == WEEK ), by = 1:nrow(week_dt)]
  week_dt[WEEK_N > 1]
  week_dt[TRIP_ID == 899029]  # in 515700
  
  # If WEEK_DAYS <= half of DAYS, exclude it?  I dunno..
  week_dt[TRIP_ID == 899029][WEEK_DAYS > DAYS/2]
  
  # TODO might be faster if we were to make columns that convert date to integer first. Date class objects get converted 
  # to character vector automatically when handled by apply?
  total_days <- function(x) {
    length(unique(unlist(apply(x, 1, function(x1) as.Date(x1[1]) : as.Date(x1[2]), simplify = F))))
  }
  week_dt[, TOT_DAYS := total_days(.SD) , by = .(TRIP_ID), .SDcols = c("TRIP_TARGET_DATE", "LANDING_DATE")]
  
  week_dt[WEEK_N == 2]
  week_dt[TRIP_ID == 1581807]  # week 15 has only 1 day in same area (3 days total)
  week_dt[TRIP_ID == 1559533]  # week 34 has 4 days and week 35 has 2 days (6 days total) in same area
  week_dt[TRIP_ID == 1483562]  # 5 total days in same area, 4 days in week 37, 1 day in week 38
  week_dt[TRIP_ID == 924006]
  week_dt[TRIP_ID == 899029]
  week_dt[TRIP_ID == 924006]
  week_dt[TRIP_ID == 700018]   # TODO Just 2 days, needs 'latter week' rule
  week_dt[TRIP_ID == 924413]  
  week_dt[TRIP_ID == 699010]   # TODO Just 2 days, needs 'latter week' rule
  week_dt[TRIP_ID == 698070]
  week_dt[TRIP_ID == 722272]
  week_dt[TRIP_ID == 700352]
  week_dt[TRIP_ID == 925328]
  week_dt[TRIP_ID == 925011]
  week_dt[TRIP_ID == 925024]
  week_dt[TRIP_ID == 702739]  # TODO 5 days, but spans 7 days. Area 625401 had days Mar 21, 22, 25, in week 12, 26 in week 13
  week_dt[TRIP_ID == 700433]  # TODO 5 days, Mar 21,22, Needs to merge ADFG_STAT_AREA_CODE to hex cell
  week_dt[TRIP_ID == 923988]
  week_dt[WEEK_N == 2][41:60]
  
  a <- unique(wd[, .(TRIP_ID, TRIP_TARGET_DATE, LANDING_DATE)])
  a[, ":=" (START = min(TRIP_TARGET_DATE, LANDING_DATE), END = max(TRIP_TARGET_DATE, LANDING_DATE)), by = .(TRIP_ID)]
  a1 <- unique(a[, .(TRIP_ID, START, END)])
  a1[, DAYS := length(unique(START : END)), by = 1:nrow(a1)]
  hist(a1$DAYS)
  a1[, sum(DAYS>7)/.N]  # 9.5% of trips are over 7 days
  a1[, sum(DAYS == 8)/.N] # 3.3%
  a1[, sum(DAYS == 9)/.N] # 2.2%
  a1[, sum(DAYS ==10)/.N] # 1.4%
  a1[, sum(DAYS > 10)/.N] # Only 2.6% are above 11. Only allow those trips to span weeks? Use midpoint otherwise?
  
  # TODO 9 and 10 day trips could potentially span 3 different weeks, so include any weeks with at least 4 days fished?
  
  
  source("analyses/spatiotemporal_boxes/functions.R")
  load("source_data/stat_area.rdata")
  grid_200km <- as.data.table(stat_area_to_hex(2e5, stat_area_sf = stat_area)$STAT_AREA_HEX_DF)
  
  # Assign HEX_ID from ADFG_STAT_AREA_CODE
  test <- unique(wd[, .(ADP, POOL, TRIP_ID, TRIP_TARGET_DATE, LANDING_DATE, ADFG_STAT_AREA_CODE = as.integer(ADFG_STAT_AREA_CODE))])
  test[, HEX_ID := grid_200km[test, HEX_ID, on = .(ADFG_STAT_AREA_CODE)]]
  # Drop ADFG_STAT_AREA_CODE
  test <- unique(test[, .(ADP, POOL, TRIP_ID, TRIP_TARGET_DATE, LANDING_DATE, HEX_ID)])
  # For each HEX_ID, get the range of dates
  test[, HEX_ID_DAYS := length(unique(TRIP_TARGET_DATE : LANDING_DATE)), by = 1:nrow(test)]
  
  test[HEX_ID_DAYS > 7]
  
  test1 <- test[HEX_ID_DAYS > 7][4566,][, .(WEEK = unique(week(TRIP_TARGET_DATE) : week(LANDING_DATE))), by = .(TRIP_ID, TRIP_TARGET_DATE, LANDING_DATE)] # has three weeks
  test1[, WEEK_DAYS := sum(WEEK == week(as.Date(TRIP_TARGET_DATE : LANDING_DATE, origin))) , by = 1:nrow(test1)]
  test1[, WEEK_DAYS_C := WEEK_DAYS > 3]
  # Has 7 days in week 39, 4 in week 4, and only 2 in week 38
  
  # What if a trip fished 13 days, had 7 days fished in the enter week, and only 3 days fished before and after? tough luck!
  
  test2 <- test[HEX_ID_DAYS > 7]
  test3 <- test2[, .(WEEK = unique(week(TRIP_TARGET_DATE) : week(LANDING_DATE))), by = .(TRIP_ID, TRIP_TARGET_DATE, LANDING_DATE, HEX_ID, HEX_ID_DAYS)] # has three weeks
  test3[, WEEK_DAYS := sum(WEEK == week(as.Date(TRIP_TARGET_DATE : LANDING_DATE, origin))) , by = 1:nrow(test3)]
  test3[, WEEK_DAYS_C := WEEK_DAYS > 3]
  
  test3[HEX_ID_DAYS == 13 & WEEK_DAYS == 3, unique(TRIP_ID)] # 8 trips have had this happen
  test3[TRIP_ID == 1593686] # WOMP WOMP
  test3[TRIP_ID == 10794637]
  test3[TRIP_ID == 703550]
  test3[TRIP_ID == 965540]
  test3[, uniqueN(TRIP_ID)] == test3[WEEK_DAYS_C == T, uniqueN(TRIP_ID)] # Good, so no trips were lost
  
  # At the same time, depending on where grid cells are drawn, some trips get more cells than others!
  # For this reason, perhaps just be a liberal as possible? As long as theyre all treated the same?
  # Still, probably best to apply the HEX_ID first?
  
  
  
  # the issue is that if a 2-day trip spans 2 weeks, it has a disproportionately higher impact on neighbor counts than a
  # 7-day trip that occurs within just 1 week. 
  
  
  
  # Bin to 1-week if:
  # 2 days total, default to later week 
  # 3 days total, 2 in 1 week
  # 4 days total, 3 in 1 week
  # 4 days total, 2+2, default to later week?
  # 5 days total, 3-4 in 1 week
  # 6 days total, 4-5 days in one week
  
  # Allow 2-week split if 
  # 6 days total, 3+3
  # 7 days total, 3+ days 
  
  # If WEEK_DAYS >= 3, keep that week
  # If (TOT_DAYS - WEEK_DAYS) < TOT_DAYS/2
  week_dt[, CHECK_1 := WEEK_DAYS >= 3]
  week_dt[, CHECK_2 := (TOT_DAYS - WEEK_DAYS) < TOT_DAYS/2]
  # Need rule for if WEEK_DAYS == TOT_DAYS/2 and is the latter week? 
  
  
  
  # Todo make the define_poststratum() function accept the function name to apply? like "week" and "month" from lubridate?
  # DAYS should be +1 (or count of unique dates between the two)
  
  a2 <- copy(week_dt[TRIP_ID == 899029][1,])
  a2[, length(TRIP_TARGET_DATE : LANDING_DATE)]
  
  week_dt[TRIP_ID == 924006]
  
  
  month_dt <- unique(wd[, .(TRIP_ID, ADFG_STAT_AREA_CODE, TRIP_TARGET_DATE, LANDING_DATE)])[
  ][, .(WEEK = seq(week(min(TRIP_TARGET_DATE, LANDING_DATE)), week(max(TRIP_TARGET_DATE, LANDING_DATE)))), by = .(TRIP_ID, ADFG_STAT_AREA_CODE, TRIP_TARGET_DATE, LANDING_DATE)]
  
  # TESTING
  test <- wd[week_dt, on = .(TRIP_ID, ADFG_STAT_AREA_CODE, TRIP_TARGET_DATE, LANDING_DATE)]
  test[, WEEK_N := uniqueN(WEEK), by= .(TRIP_ID)][WEEK_N>1]
  test[TRIP_ID==899029 ] # Trips that barely span multiple weeks get put into two weeks... kind of unfair? Unavoidable?
  # Before I used the center of TRIP_TARGET_DATE and LANDING_DATE. Only allow weeks to vary based on diff of TRIP_TARGET_DATE 
  # and LANDING_DATE?
  
  wd[, .(ADP, POOL, STRATA, BSAI_GOA, BS_AI_GOA, AREA, TARGET, AGENCY_GEAR_CODE, GEAR, PERMIT, TRIP_ID, TRIP_TARGET_DATE,
         LANDING_DATE, MONTH, DAYS)]
  
}

# 'efrt' is just a simplified verson of work.data for non-jig PC trips for the past 3 years, and defines pool (OB, EM, or ZE)
# Uses 'max_date' to trim dataset to last 3 full years (instead of using ADP)
# wd <- unique(work.data[CVG_NEW=="PARTIAL" & AGENCY_GEAR_CODE!="JIG", .(PERMIT=VESSEL_ID, TARGET=TRIP_TARGET_CODE, PORT = PORT_NEW, AREA=REPORTING_AREA_CODE, AGENCY_GEAR_CODE, GEAR=ifelse(AGENCY_GEAR_CODE %in% c("PTR", "NPT"), "TRW", AGENCY_GEAR_CODE), STRATA=STRATA_NEW, P_STRATA, START=min(TRIP_TARGET_DATE, LANDING_DATE, na.rm=TRUE), END=max(TRIP_TARGET_DATE, LANDING_DATE, na.rm=TRUE)), keyby=.(ADP, TRIP_ID)])
# wd[TARGET=="B", TARGET:="P"]  # Convert all 'bottom pollock' to 'pelagic pollock', allowing us to treat all pollock target trips the same
# wd[, POOL := ifelse(STRATA %in% c("EM_HAL", "EM_POT"), "EM", ifelse(STRATA=="ZERO", "ZE", "OB"))]   # define pool
# wd[, MONTH := month(START)]
# wd[, FMP := ifelse(AREA >=600, "GOA", "BSAI")] # define FMP using area, splitting by GOA and BSAI only
# wd[, DAYS := td_out[wd, DAYS, on=.(TRIP_ID)]]  # Merge in trip durations for OB pool trips
# if(nrow(wd[is.na(DAYS) & POOL =="OB" & STRATA != "EM_TRW_EFP"]) >0){message("Some OB pool trips are missing trip durations!")} 
# wd <- wd[, .(ADP, POOL, STRATA, P_STRATA, FMP, PORT, AREA, TARGET, AGENCY_GEAR_CODE, GEAR, PERMIT, TRIP_ID, START, END, MONTH, DAYS)]
# efrt <- unique(wd)  # final efrt object to output!
# efrt[POOL == "OB", STRATA := paste0("OB_", STRATA)]

#### Normally we remove intances of PTR from any NPT trips - don't bother here?
# If any trips fished both PTR and NPT gear, remove instances of PTR - only trips that fished with PTR exclusively can be within the pollock EFP
# trw_dup <- unique(efrt[STRATA=="TRW", .(ADP, POOL, STRATA, P_STRATA, AGENCY_GEAR_CODE, GEAR, PERMIT, TRIP_ID, START, END, MONTH, DAYS)])
# trw_dup_id <- unique(trw_dup[, .N, by=TRIP_ID][N>1, TRIP_ID])
# if(as.numeric(diff(table(trw_dup[TRIP_ID %in% trw_dup_id, AGENCY_GEAR_CODE]))) != 0){warning("Something else other than NPT/PTR is creating unique records.")}
# efrt <- unique(efrt[TRIP_ID %in% trw_dup_id, AGENCY_GEAR_CODE := "NPT"])        # Remove PTR records by making all trips by these mixed-gear trips have NPT only

# Check that fixed gear duplicates are due only to combo HAL/POT gear trips (which we will retain in AGENCY_GEAR_CODE and GEAR)
# hal_pot_dup <- unique(efrt[POOL%in%c("OB", "ZE") & AGENCY_GEAR_CODE %in% c("HAL", "POT"), .(ADP, POOL, STRATA, P_STRATA, AGENCY_GEAR_CODE, GEAR, PERMIT, TRIP_ID, START, END, MONTH, DAYS)])
# hal_pot_dup_id <- unique(hal_pot_dup[, .N, by=TRIP_ID][N>1, TRIP_ID])
# if(as.numeric(diff(table(hal_pot_dup[TRIP_ID %in% hal_pot_dup_id, AGENCY_GEAR_CODE]))) != 0){warning("Something other than HAL/POT is creating unique records.")}

# Identify gear type used most in trip based on retained catch?

gear_weight <- work.data[
][TRIP_ID %in% wd$TRIP_ID
][, .(GEAR_WEIGHT = sum(WEIGHT_POSTED[SOURCE_TABLE == "Y"])), by = .(TRIP_ID, AGENCY_GEAR_CODE)]
gear_weight[, GEAR_RET := AGENCY_GEAR_CODE[which.max(GEAR_WEIGHT)], by= .(TRIP_ID)
][, c("AGENCY_GEAR_CODE", "GEAR_WEIGHT") := NULL]
gear_weight <- unique(gear_weight)
# Merge in agency_gear_code used most (GEAR_RET)
efrt <- wd[gear_weight, on = .(TRIP_ID)]

# For zero coverage trips, set STRATA equal to the AGENCY_GEAR_CODE with the most landed weight
# efrt <- # Isolate zero coverage trips in work.data, because efrt doesn't contain catch weight
#   work.data[TRIP_ID %in% efrt[POOL == "ZE", TRIP_ID]
#             # Calculate which gear type had the most landed (SOURCE_TABLE == "Y") weight
#   ][, .(GEAR_WEIGHT = sum(WEIGHT_POSTED[SOURCE_TABLE == "Y"])), by = .(TRIP_ID, AGENCY_GEAR_CODE)
#     # Left join this back onto the efrt object
#   ][efrt, on = .(TRIP_ID, AGENCY_GEAR_CODE)
#     # The trips that we're interested in will not be NA for GEAR_WEIGHT
#     # Set AGENCY_GEAR_CODE and GEAR to that which had the most landed weight
#   ][!is.na(GEAR_WEIGHT), STRATA := AGENCY_GEAR_CODE[which.max(GEAR_WEIGHT)], by = .(TRIP_ID)
#     # Remove the GEAR_WEIGHT column
#   ][, GEAR_WEIGHT := NULL]

# View how these trips were re-stratified
# efrt[POOL == "ZE", .(N = uniqueN(TRIP_ID)), by = .(POOL, STRATA)][order(POOL, STRATA)]


# -----------------------------------------------------------------------------------------------------------------#
# Save this version of work.data and efrt -------------------------------------------------------------------------

work.data  # 2018 - 2022 
efrt       # 2018 - 2022

# save(work.data, efrt, file = "source_data/allocation_evaluation_data_prep.Rdata")

# -----------------------------------------------------------------------------------------------------------------#
# START HERE TO QUICK LOAD --------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------#

# FIXME I would like time and space to vary together within a trip
# Can't use the 'start' and 'end' as it currently exists.
# Need to retain the combinations of ADFG_STAT_AREA_CODE and TRIP_TARGET_DATE and LANDING_DATE within trips!
# This is creating more time x space combination than there should be for some trips!

# Load the data used for the 2023 Final ADP, specifically for fg_em and other objects
# https://drive.google.com/file/d/1GeBZgGaBvZPl3T-tVbTvpDZ5aF9I-CS7/view?usp=share_link
# load("source_data/2023_Final_ADP_data.rdata")
# rm(list = setdiff(ls(), "fg_em"))   # Remove all except for fg_em vessels

# Load the effort prediction for the 2023 Final ADP, mostly for the TRW EFP probability
# https://drive.google.com/file/d/19y_Io-xGGVrftw-63ecUSg4i-1JUco2p/view?usp=share_link
load("source_data/effort_prediction.rdata")   # load this, mostly for the TRW_EM probability
trw_em_prob <- efrt[!is.na(EFP_PROB), unique(EFP_PROB)]
rm(efrt, efrt_adpyear, gvf)

# Quick-load the work.data and efrt object created earlier in this script
load("source_data/allocation_evaluation_data_prep.Rdata")  # Loads 'work.data' and 'efrt' objects
 
# Load the 2023 TRW EM permit list
# https://drive.google.com/file/d/1qxR2idmwfLNs0ZTVsF9R2CxX4p_dqa38/view?usp=share_link
trw_em_list <- fread("source_data/efp_list_2022-11-03.csv")

# -----------------------------------------------------------------------------------------------------------------#

# Add ADFG statistical areas and their weeks ---------------------------------------------------------------------------

# Modify 'efrt' object to make a dataset that works for all allocation methods.
# Subset to full years and remove unneeded columns
# val_2018_2022_dt <- unique(efrt[, -c("P_STRATA", "PORT", "TARGET")])
# TODO I could probably exclude 'DAYS' since it is included in the trips_melt object. This is modeled observer sea days.

# Get ADFG stat area code and week from work.data (these are both from the landing reports)
# stat_area_dt <- unique(work.data[
#   TRIP_ID %in% val_2018_2022_dt$TRIP_ID, 
#   .(TRIP_ID, AREA = REPORTING_AREA_CODE, ADFG_STAT_AREA_CODE, TRIP_TARGET_DATE, LANDING_DATE)])[
#   ][, WEEK := week(TRIP_TARGET_DATE + as.numeric(LANDING_DATE - TRIP_TARGET_DATE , units = "days"))
#   ][, c("TRIP_TARGET_DATE", "LANDING_DATE") := NULL][]
# val_2018_2022_dt <- stat_area_dt[val_2018_2022_dt, on = .(TRIP_ID, AREA)]  # Merge in ADFG_STAT_AREA_CODE
# # For some reason, my shapefiles don't include ADFG STAT AREA 515832, perhaps it was merged into 515831 
# val_2018_2022_dt[ADFG_STAT_AREA_CODE == 515832, ADFG_STAT_AREA_CODE := 515831]

# Apply TRW EM trip rates ----------------------------------------------------------------------------------------------

val_2018_2022_dt <- copy(efrt)

# Identify trips that have potential to be in TRW EM
trw_em_trip_ids <- unique(efrt[
  PERMIT %in% trw_em_list$PERMIT & TARGET %in% c("P", "B") & AGENCY_GEAR_CODE %in% c("NPT", "PTR"),
  .(TRIP_ID, AGENCY_GEAR_CODE)])[, .SD[all(AGENCY_GEAR_CODE == "PTR")], by = .(TRIP_ID)]$TRIP_ID
# Randomly sample potential TRW_EM trips at the EFP_PROB rate
set.seed(12345)
trw_em_trip_ids_sel <- trw_em_trip_ids[runif(n = length(trw_em_trip_ids)) < trw_em_prob]
val_2018_2022_dt[TRIP_ID %in% trw_em_trip_ids_sel, ':=' (POOL = "EM", STRATA = "EM_TRW")]

# Get retained catch weight by FMP for stratifying by FMP --------------------------------------------------------------

# fmp_retained_mt <- work.data[
# ][PERMIT %in% val_2018_2022_dt$PERMIT
# ][, .(FMP_RET_MT = sum(WEIGHT_POSTED[SOURCE_TABLE == "Y"], na.rm=T)), by = .(TRIP_ID, FMP)
# ][, .SD[which.max(FMP_RET_MT)], by = .(TRIP_ID)
# ][, FMP_RET_MT := NULL][]
# setnames(fmp_retained_mt, "FMP", "FMP_MT")
# # Merge in FMP retained weight and identify FMP with the higher retained tonnage
# val_2018_2022_dt[, FMP_MT := fmp_retained_mt[val_2018_2022_dt, FMP_MT, on = .(TRIP_ID)]]
# if(nrow(val_2018_2022_dt[, .SD[uniqueN(FMP_MT) > 1], by= .(TRIP_ID)])) stop("A trip has more than one FMP_MT!")

# Apply current fixed-gear EM vessel list ------------------------------------------------------------------------------
# TODO Shouldn't need this since I applied the most current fg_em list above when re-creating get_data.R
# val_2018_2022_dt[PERMIT %in% fg_em$PERMIT, STRATA := ifelse(!(STRATA %like% "EM"), paste0("EM_", STRATA), STRATA)]
# if(length(setdiff(val_2018_2022_dt[STRATA %in% c("EM_HAL", "EM_POT"), unique(PERMIT)], fg_em$PERMIT))) {
#   stop("There is a vessel in FG EM that is not in the list!")}

# Format output and save -----------------------------------------------------------------------------------------------

# Assign a new TRIP_ID as an integer. Smaller integers 
int_cols <- c("AREA", "ADFG_STAT_AREA_CODE", "PERMIT", "ADP")
val_2018_2022_dt[, (int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]
# Currently, TRIP_ID is a character and some TRIP_IDs have decimals, so they cannot be reliably converted to integer.
# Rename the work.data TRIP_ID as wd_TRIP_ID and create a new integer TRIP_ID
setnames(val_2018_2022_dt, "TRIP_ID", "wd_TRIP_ID")
val_2018_2022_dt[, TRIP_ID := .GRP, by = .(wd_TRIP_ID)]

val_2018_2022_dt

setcolorder(val_2018_2022_dt, c("ADP", "POOL",  "STRATA", "GEAR", "AGENCY_GEAR_CODE", "GEAR_RET", "BSAI_GOA", 
                                "BS_AI_GOA", "AREA", "ADFG_STAT_AREA_CODE", "PERMIT", "TRIP_ID", 
                                "TRIP_TARGET_DATE", "LANDING_DATE", "DAYS", "wd_TRIP_ID"))
setorder(val_2018_2022_dt, ADP, STRATA, TRIP_TARGET_DATE, AREA)
# Final checks
if(any(!table(complete.cases(val_2018_2022_dt[, -"DAYS", with = T])))) stop("Still some NAs to address!")
if(nrow(val_2018_2022_dt[(POOL == "EM" & !(STRATA %like% "EM"))])) stop("Some EM Pool trips are not in EM strata!")
if(nrow(val_2018_2022_dt[(POOL == "OB" & (STRATA %like% "EM"))])) stop("Some OB Pool trips are in EM strata!")
if(nrow(val_2018_2022_dt[, .SD[uniqueN(POOL) > 1], by = .(TRIP_ID)])) stop("Some trips are in multiple pools!")
if(nrow(val_2018_2022_dt[, .SD[uniqueN(STRATA) > 1], by = .(TRIP_ID)])) stop("Some trips are in multiple strata!")

# Rename the observer strata to include pool.
val_2018_2022_dt[POOL == "OB", STRATA := paste0("OB_", STRATA)]

# For some reason, my shapefiles don't include ADFG STAT AREA 515832, perhaps it was merged into 515831 
val_2018_2022_dt[ADFG_STAT_AREA_CODE == 515832, ADFG_STAT_AREA_CODE := 515831]

save(val_2018_2022_dt, file = "analyses/allocation_evaluation/allocation_evaluation.Rdata")
