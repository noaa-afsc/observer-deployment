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
work.data <- copy(val_2018_current)
rm(val_2018_current)

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
work.data <- mutate(work.data, TRIP_TARGET_DATE = as.Date(TRIP_TARGET_DATE), LANDING_DATE = as.Date(LANDING_DATE))
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
    val[GEAR_N>1, GEAR := ifelse(GEAR=="NPT", "PTR", ifelse(GEAR%in%c("POT","JIG"), "HAL", GEAR)), by=.(TRIP_ID, GEAR)]    # If multiple gear types were used, prioritze PTR over NPT and HAL over POT and JIG
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
# initial run performs the queries and joins and runs with a default model of DAYS ~ RAW. Spits 'mod_dat' to the global environment
td_init <- model_trip_duration(val_data = work.data[ADP < 2022])  
td_init$TD_PLOT
td_init$DIAG
# After some testing, the model below was decided as the best for predicting trip durations
td_mod <- model_trip_duration(val_data = work.data[ADP < 2022], use_mod="DAYS ~ RAW + ADP*AGENCY_GEAR_CODE")
td_mod$TD_PLOT
td_mod$DIAG     
# Subset all observer pool trips for the most recent 3 years
td_dat <- unique(work.data[ADP < 2022 & CVG_NEW!="FULL" & STRATA_NEW != "ZERO" & AGENCY_GEAR_CODE != "JIG", .(VESSEL_ID, ADP, AGENCY_GEAR_CODE, START=min(TRIP_TARGET_DATE, LANDING_DATE, na.rm=TRUE), END=max(TRIP_TARGET_DATE, LANDING_DATE, na.rm=TRUE), TENDER), by=.(TRIP_ID)])
td_dat <- td_dat[ADP !=  2022]                                             # Subset this to get full 3 years of prior fishing effort
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
# and arounded to the nearest 0.5 (e.g., an averaged duration of 3.25 would be rounded down to 3.00, but a duration of 3.75 would be rounded up to 4.00)
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

# 'efrt' is just a simplified verson of work.data for non-jig PC trips for the past 3 years, and defines pool (OB, EM, or ZE)
# Uses 'max_date' to trim dataset to last 3 full years (instead of using ADP)
wd <- unique(work.data[CVG_NEW=="PARTIAL" & AGENCY_GEAR_CODE!="JIG", .(PERMIT=VESSEL_ID, TARGET=TRIP_TARGET_CODE, PORT = PORT_NEW, AREA=REPORTING_AREA_CODE, AGENCY_GEAR_CODE, GEAR=ifelse(AGENCY_GEAR_CODE %in% c("PTR", "NPT"), "TRW", AGENCY_GEAR_CODE), STRATA=STRATA_NEW, P_STRATA, START=min(TRIP_TARGET_DATE, LANDING_DATE, na.rm=TRUE), END=max(TRIP_TARGET_DATE, LANDING_DATE, na.rm=TRUE)), keyby=.(ADP, TRIP_ID)])
wd <- wd[ADP < 2022]
wd[TARGET=="B", TARGET:="P"]  # Convert all 'bottom pollock' to 'pelagic pollock', allowing us to treat all pollock target trips the same
wd[, POOL := ifelse(STRATA %in% c("EM_HAL", "EM_POT"), "EM", ifelse(STRATA=="ZERO", "ZE", "OB"))]   # define pool
wd[, MONTH := month(START)]
wd[, FMP := ifelse(AREA >=600, "GOA", "BSAI")] # define FMP using area, splitting by GOA and BSAI only
wd[, DAYS := td_out[wd, DAYS, on=.(TRIP_ID)]]  # Merge in trip durations for OB pool trips
if(nrow(wd[is.na(DAYS) & POOL =="OB" & STRATA != "EM_TRW_EFP"]) >0){message("Some OB pool trips are missing trip durations!")} 
wd <- wd[, .(ADP, POOL, STRATA, P_STRATA, FMP, PORT, AREA, TARGET, AGENCY_GEAR_CODE, GEAR, PERMIT, TRIP_ID, START, END, MONTH, DAYS)]
efrt <- unique(wd)  # final efrt object to output!

# If any trips fished both PTR and NPT gear, remove instances of PTR - only trips that fished with PTR exclusively can be within the pollock EFP
trw_dup <- unique(efrt[STRATA=="TRW", .(ADP, POOL, STRATA, P_STRATA, AGENCY_GEAR_CODE, GEAR, PERMIT, TRIP_ID, START, END, MONTH, DAYS)])
trw_dup_id <- unique(trw_dup[, .N, by=TRIP_ID][N>1, TRIP_ID])
if(as.numeric(diff(table(trw_dup[TRIP_ID %in% trw_dup_id, AGENCY_GEAR_CODE]))) != 0){warning("Something else other than NPT/PTR is creating unique records.")}
efrt <- unique(efrt[TRIP_ID %in% trw_dup_id, AGENCY_GEAR_CODE := "NPT"])        # Remove PTR records by making all trips by these mixed-gear trips have NPT only

# Check that fixed gear duplicates are due only to combo HAL/POT gear trips (which we will retain in AGENCY_GEAR_CODE and GEAR)
hal_pot_dup <- unique(efrt[POOL%in%c("OB", "ZE") & AGENCY_GEAR_CODE %in% c("HAL", "POT"), .(ADP, POOL, STRATA, P_STRATA, AGENCY_GEAR_CODE, GEAR, PERMIT, TRIP_ID, START, END, MONTH, DAYS)])
hal_pot_dup_id <- unique(hal_pot_dup[, .N, by=TRIP_ID][N>1, TRIP_ID])
if(as.numeric(diff(table(hal_pot_dup[TRIP_ID %in% hal_pot_dup_id, AGENCY_GEAR_CODE]))) != 0){warning("Something other than HAL/POT is creating unique records.")}

# For zero coverage trips, set STRATA equal to the AGENCY_GEAR_CODE with the most landed weight
efrt <- # Isolate zero coverage trips in work.data, because efrt doesn't contain catch weight
  work.data[TRIP_ID %in% efrt[POOL == "ZE", TRIP_ID]
            # Calculate which gear type had the most landed (SOURCE_TABLE == "Y") weight
  ][, .(GEAR_WEIGHT = sum(WEIGHT_POSTED[SOURCE_TABLE == "Y"])), by = .(TRIP_ID, AGENCY_GEAR_CODE)
    # Left join this back onto the efrt object
  ][efrt, on = .(TRIP_ID, AGENCY_GEAR_CODE)
    # The trips that we're interested in will not be NA for GEAR_WEIGHT
    # Set AGENCY_GEAR_CODE and GEAR to that which had the most landed weight
  ][!is.na(GEAR_WEIGHT), STRATA := AGENCY_GEAR_CODE[which.max(GEAR_WEIGHT)], by = .(TRIP_ID)
    # Remove the GEAR_WEIGHT column
  ][, GEAR_WEIGHT := NULL]

# View how these trips were re-stratified
efrt[POOL == "ZE", .(N = uniqueN(TRIP_ID)), by = .(POOL, STRATA)][order(POOL, STRATA)]


# -----------------------------------------------------------------------------------------------------------------#
# Save this version of work.data and efrt -------------------------------------------------------------------------

work.data  # 2018 - 2021 with partial 2022 data for now.
efrt       # 2018 - 2021 only

# save(work.data, efrt, file = "source_data/allocation_evaluation_data_prep.Rdata")


# -----------------------------------------------------------------------------------------------------------------#
# START HERE TO QUICK LOAD --------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------#

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
val_2018_2021_dt <- unique(efrt[, -c("P_STRATA", "PORT", "TARGET")])
# TODO I could probably exclude 'DAYS' since it is included in the trips_melt object. This is modeled observer sea days.

# Get ADFG stat area code and week from work.data (these are both from the landing reports)
stat_area_dt <- unique(work.data[
  TRIP_ID %in% val_2018_2021_dt$TRIP_ID, 
  .(TRIP_ID, AREA = REPORTING_AREA_CODE, ADFG_STAT_AREA_CODE, TRIP_TARGET_DATE, LANDING_DATE)])[
  ][, WEEK := week(TRIP_TARGET_DATE + as.numeric(LANDING_DATE - TRIP_TARGET_DATE , units = "days"))
  ][, c("TRIP_TARGET_DATE", "LANDING_DATE") := NULL][]
val_2018_2021_dt <- stat_area_dt[val_2018_2021_dt, on = .(TRIP_ID, AREA)]  # Merge in ADFG_STAT_AREA_CODE
# For some reason, my shapefiles don't include ADFG STAT AREA 515832, perhaps it was merged into 515831 
val_2018_2021_dt[ADFG_STAT_AREA_CODE == 515832, ADFG_STAT_AREA_CODE := 515831]

# Apply TRW EM trip rates ----------------------------------------------------------------------------------------------

# Identify trips that have potential to be in TRW EM
trw_em_trip_ids <- unique(efrt[
  PERMIT %in% trw_em_list$PERMIT & TARGET %in% c("P", "B") & AGENCY_GEAR_CODE %in% c("NPT", "PTR"),
  .(TRIP_ID, AGENCY_GEAR_CODE)])[, .SD[all(AGENCY_GEAR_CODE == "PTR")], by = .(TRIP_ID)]$TRIP_ID
# Randomly sample potential TRW_EM trips at the EFP_PROB rate
set.seed(12345)
trw_em_trip_ids_sel <- trw_em_trip_ids[runif(n = length(trw_em_trip_ids)) < trw_em_prob]
val_2018_2021_dt[TRIP_ID %in% trw_em_trip_ids_sel, ':=' (POOL = "EM", STRATA = "EM_TRW")]

# Get retained catch weight by FMP for stratifying by FMP --------------------------------------------------------------

fmp_retained_mt <- work.data[
][PERMIT %in% val_2018_2021_dt$PERMIT
][, .(FMP_RET_MT = sum(WEIGHT_POSTED[SOURCE_TABLE == "Y"], na.rm=T)), by = .(TRIP_ID, FMP)
][, .SD[which.max(FMP_RET_MT)], by = .(TRIP_ID)
][, FMP_RET_MT := NULL][]
setnames(fmp_retained_mt, "FMP", "FMP_MT")
# Merge in FMP retained weight and identify FMP with the higher retained tonnage
val_2018_2021_dt[, FMP_MT := fmp_retained_mt[val_2018_2021_dt, FMP_MT, on = .(TRIP_ID)]]
if(nrow(val_2018_2021_dt[, .SD[uniqueN(FMP_MT) > 1], by= .(TRIP_ID)])) stop("A trip has more than one FMP_MT!")

# Apply current fixed-gear EM vessel list ------------------------------------------------------------------------------
# TODO Shouldn't need this since I applied the most current fg_em list above when re-creating get_data.R
# val_2018_2021_dt[PERMIT %in% fg_em$PERMIT, STRATA := ifelse(!(STRATA %like% "EM"), paste0("EM_", STRATA), STRATA)]
# if(length(setdiff(val_2018_2021_dt[STRATA %in% c("EM_HAL", "EM_POT"), unique(PERMIT)], fg_em$PERMIT))) {
#   stop("There is a vessel in FG EM that is not in the list!")}

# Format output and save -----------------------------------------------------------------------------------------------

# Assign a new TRIP_ID as an integer. Smaller integers 
int_cols <- c("AREA", "ADFG_STAT_AREA_CODE", "PERMIT", "ADP", "MONTH")
val_2018_2021_dt[, (int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]
# Currently, TRIP_ID is a character and some TRIP_IDs have decimals, so they cannot be reliably converted to integer.
# Rename the work.data TRIP_ID as wd_TRIP_ID and create a new integer TRIP_ID
setnames(val_2018_2021_dt, "TRIP_ID", "wd_TRIP_ID")
val_2018_2021_dt[, TRIP_ID := .GRP, by = .(wd_TRIP_ID)]
setcolorder(val_2018_2021_dt, c("ADP", "POOL",  "STRATA", "GEAR","AGENCY_GEAR_CODE", "FMP", "FMP_MT", "AREA",
                                "ADFG_STAT_AREA_CODE", "WEEK", "MONTH", "PERMIT", "TRIP_ID", "START", "END", "DAYS"))
setorder(val_2018_2021_dt, ADP, STRATA, START, AREA)
# Final checks
if(any(!table(complete.cases(val_2018_2021_dt[, -"DAYS", with = T])))) stop("Still some NAs to address!")
if(nrow(val_2018_2021_dt[(POOL == "EM" & !(STRATA %like% "EM"))])) stop("Some EM Pool trips are not in EM strata!")
if(nrow(val_2018_2021_dt[(POOL == "OB" & (STRATA %like% "EM"))])) stop("Some OB Pool trips are in EM strata!")
if(nrow(val_2018_2021_dt[, .SD[uniqueN(POOL) > 1], by = .(TRIP_ID)])) stop("Some trips are in multiple pools!")
if(nrow(val_2018_2021_dt[, .SD[uniqueN(STRATA) > 1], by = .(TRIP_ID)])) stop("Some trips are in multiple strata!")

save(val_2018_2021_dt, file = "analyses/allocation_evaluation/allocation_evaluation.Rdata")
