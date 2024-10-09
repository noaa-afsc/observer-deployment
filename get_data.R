# User inputs -------------------------------------------------------------
ADPyear     <- 2025     # Enter numeric year you are doing the ADP for
ADP_version <- "Final"  # Enter "Draft" or "Final"
EM_final    <- "N"      # Is the final EM list approved? Y/N (caps)
options(scipen = 9999)  # Avoid scientific notation

# Get packages ------------------------------------------------------------
if(!require("devtools"))  install.packages("devtools")
if(!require("FMAtools")) install_github("Alaska-Fisheries-Monitoring-Analytics/FMAtools")
if(!require("odbc")) install.packages("odbc", repos='http://cran.us.r-project.org')
#if(!require("ROracle")) install.packages("ROracle", repos='http://cran.us.r-project.org')
if(!require("data.table"))   install.packages("data.table", repos='http://cran.us.r-project.org')
if(!require("lubridate"))   install.packages("lubridate", repos='http://cran.us.r-project.org') # For fixing datetimes
if(!require("tidyverse"))   install.packages("tidyverse", repos='http://cran.us.r-project.org') # ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats  

# Establish channels ------------------------------------------------------
source("common_functions/open_channel.R")
channel_afsc <- open_channel()
#channel_akro <- eval(parse(text = Sys.getenv("channel_cas")))

# Load AKRO pull ------------------------------------------------------
# The pulls from AKRO were all moved to the sql_pull_akro.R
gdrive_download(
  local_path = paste0("source_data/", paste(ADPyear, ADP_version, "ADP_akro_pull.rdata", sep = "_")),
  gdrive_dribble = gdrive_set_dribble("Projects/ADP/source_data/")
)
load(paste0("source_data/", paste(ADPyear, ADP_version, "ADP_akro_pull.rdata", sep = "_")))

# Data queries ------------------------------------------------------------

# * PSC ----
# PSC <- 
#   dbGetQuery(channel_akro,
#              paste("SELECT
#                    year,
#                    el_report_id as report_id,
#                    species_group_code, 
#                    sum(case when psc_total_count is not null then psc_total_count
#                             when psc_total_catch_weight is not null then psc_total_catch_weight
#                             else 0 end) as psc_total_catch,
#                    sum(psc_total_mortality_weight) as psc_total_mortality_weight
#                    FROM akfish_report.v_cas_psc
#                    WHERE year >=", ADPyear - 4,"
#                    AND el_report_id is not null
#                    GROUP BY year, el_report_id, species_group_code"))

# * PCTC ----
# pctc <- dbGetQuery(channel_akro, 
#                    "SELECT DISTINCT llp.current_vessel_id vessel_id, llp.current_vessel_name vessel_name
#                     FROM akfish.v_llp_groundfish_license llp
#                     JOIN akfish.pctc_llp_initial_quota_share qs ON substr(llp.license,4,4) = qs.license_number
#                     WHERE llp.current_vessel_id is not null
#                     ORDER BY vessel_name")

# * Voluntary full coverage ----
#   Requests to join must be made prior to October 15  
# BSAIVoluntary <-
#   dbGetQuery(channel_akro,
#              if(ADP_version == "Draft" | Sys.Date() < paste0(ADPyear-1, "-10-15")){
#                paste0(
#                  "select distinct 
#                   ev.vessel_id, ev.begin_date, ev.end_date, v.name as vessel_name, e.name as eligibility, trunc(ev.last_modified_date) as last_modified_date
#                   from akfish.eligible_vessel ev
#                   join akfish.eligibility e on e.id = ev.eligibility_id
#                   join akfish_report.vessel v on v.vessel_id = ev.vessel_id
#                   where e.name = 'CV FULL COVERAGE'
#                   and v.end_date is null
#                   and v.expire_date is null
#                   and extract(year from ev.begin_date) < ", ADPyear,"
#                   and (ev.end_date is null or extract(year from ev.end_date) >= ", ADPyear - 1,")
#                   order by v.name")} else
#                    paste0(
#                      "select distinct 
#                       ev.vessel_id, ev.begin_date, ev.end_date, v.name as vessel_name, e.name as eligibility, trunc(ev.last_modified_date) as last_modified_date
#                       from akfish.eligible_vessel ev
#                       join akfish.eligibility e on e.id = ev.eligibility_id
#                       join akfish_report.vessel v on v.vessel_id = ev.vessel_id
#                       where e.name = 'CV FULL COVERAGE'
#                       and v.end_date is null
#                       and v.expire_date is null
#                       and extract(year from ev.begin_date) < ", ADPyear + 1,"
#                       and (ev.end_date is null or extract(year from ev.end_date) >= ", ADPyear,")
#                       order by v.name")
#              )

# * Partial Coverage CPs ----
#   Requests to join must be made prior to July 1  
#   F/V Trident (VESSEL_ID == 662) doesn't have an FFP and operates solely in state waters 
#   so therefore it's not subject to observer coverage. For accounting purposes, 
#   we treat it like one of these small CPs, so it's on this list.
#   For more info on this, see Alicia Miller.  
#   Pacific Mariner (4581) and Kruzof (6039) were approved for 2025 per e-mail from Melanie Rickett 2024-Aug-20
PartialCPs <- data.frame(VESSEL_ID = c(662, 4581, 6039))

# * Fixed-gear EM research ---- 
em_research <- 
  dbGetQuery(channel_afsc, 
      if(ADP_version == "Draft" | EM_final == "N"){
          paste("select distinct adp, vessel_id, vessel_name, sample_plan_seq_desc, em_request_status
                from loki.em_vessels_by_adp
                where sample_plan_seq_desc = 'Electronic Monitoring -  research not logged '
                and adp >=", ADPyear - 1,
                "order by adp, vessel_id")} else{
          paste("select distinct adp, vessel_id, vessel_name, sample_plan_seq_desc, em_request_status
                from loki.em_vessels_by_adp
                where sample_plan_seq_desc = 'Electronic Monitoring -  research not logged '
                and adp =", ADPyear,
                "order by adp, vessel_id")})

# * Fixed-gear EM approvals ---- 
em_base <-
  dbGetQuery(channel_afsc, 
             if(ADP_version == "Draft" | EM_final == "N"){
               # If its the draft, or if approvals for the final have yet to be made, use the prior year's approved vessels
               paste("SELECT DISTINCT adp, vessel_id, vessel_name, sample_plan_seq_desc, em_request_status
                      FROM loki.em_vessels_by_adp
                      WHERE adp >= ", ADPyear - 1,"
                      AND em_request_status = 'A'
                      AND sample_plan_seq_desc = 'Electronic Monitoring - Gear Type- Selected Trips'
                      order by adp, vessel_id")
             } else{ # Vessels have until November 1st to request. Approvals are typically made shortly thereafter.
               paste("SELECT DISTINCT adp, vessel_id, vessel_name, sample_plan_seq_desc, em_request_status
                      FROM loki.em_vessels_by_adp
                      WHERE adp = ", ADPyear,"
                      AND em_request_status = 'A'
                      AND sample_plan_seq_desc = 'Electronic Monitoring - Gear Type- Selected Trips'
                      order by vessel_id")})

# Remove EM research vessels from em_base
em_base <- em_base %>% 
           anti_join(select(em_research, ADP, VESSEL_ID), by = c("ADP", "VESSEL_ID"))

# * Fixed-gear EM requests ---- 
em_requests <-
  dbGetQuery(channel_afsc, 
             # Vessels have until November 1st to request. Approvals are typically made shortly thereafter.
             paste("SELECT DISTINCT adp, vessel_id, vessel_name, sample_plan_seq_desc, em_request_status
                   FROM loki.em_vessels_by_adp
                   WHERE adp = ", ADPyear,"
                   AND em_request_status = 'NEW'
                   AND sample_plan_seq_desc = 'Electronic Monitoring - Gear Type- Selected Trips'"))

#' @UDPATE *This currently reflects what was used in the 2024 ADP. Update for 2025 or remove!*
# Hardcode EM removals, opt-outs, and approvals
em_base <- em_base %>% 
           # removal
           filter(VESSEL_ID != 90) %>% 
           # opt-outs
           filter(!(VESSEL_ID %in% c(792, 32413, 3297, 3102))) %>% 
           # approvals
           plyr::rbind.fill(data.table(VESSEL_ID = c(2084, 3717, 4387), VESSEL_NAME = c("COMMANDER", "CARLYNN", "TANYA M")))
  
# * Trawl EM ----
trawl_em <- read.csv("source_data/efp_list_2023-09-05.csv")
#' @TODO This file is in the [Vision 2024 ADP/Data] folder. We should have a more updated version? 
# https://drive.google.com/file/d/1eSSTal-w_y319xF67FRSdI23rv9BLCtn/view?usp=drive_link

# * Vessel lengths ----
AKROVL <- dbGetQuery(channel_afsc, "select distinct ID as vessel_id, length_overall as akrovl
                     FROM norpac_views.akr_v_vessel_mv")

FMAVL <- dbGetQuery(channel_afsc, "SELECT DISTINCT PERMIT as vessel_id, length as fmavl 
                    FROM norpac.atl_lov_vessel")

# * Valhalla ----
# Pull data from prior years. This pull may never finish when working remotely via VPN. 
work.data <- setDT(dbGetQuery(channel_afsc, paste0("select * from loki.akr_valhalla where ADP >= ", ADPyear - 4)))

# Load data from current year
ADP_dribble <- gdrive_set_dribble("Projects/ADP/source_data")
gdrive_download("source_data/2024-10-01valhalla.Rdata", ADP_dribble)
load("source_data/2024-10-01valhalla.Rdata")

# Append data from current year to data from prior year

# First, ensure data types match between the old and new datasets
date_cols <- c("TRIP_TARGET_DATE", "LANDING_DATE")
work.data[, (date_cols) := lapply(.SD, as.Date), .SDcols = date_cols]

work.data <- rbind(work.data, valhalla, fill = T)

# Convert VESSEL_IDs using as.character to facilitate a later join with vessel lengths
work.data <- mutate(work.data, VESSEL_ID = as.character(VESSEL_ID))

# In the past, there was one record for which TENDER == "n", so convert to upper case
work.data <- mutate(work.data, TENDER = toupper(TENDER))

# Defer to database dates for all trips in valhalla with dates that don't match the database
# up_dates <- setDT(dbGetQuery(channel_akro, paste0("select distinct 
#                                                   cr.el_report_id as report_id,
#                                                   td.calendar_date as db_trip_target_date,
#                                                   cr.el_report_date as db_landing_date,
#                                                   g.gear_code as db_agency_gear_code
#                                                   FROM akfish_report.transaction_fact tf
#                                                   JOIN akfish_report.v_current_keys K ON tf.TRANSACTION_FACT_LOG_PK = k.transaction_fact_log_pk
#                                                   AND tf.PARTITION_KEY_PK = k.partition_key_pk
#                                                   JOIN akfish_report.catch_report cr on cr.catch_report_pk= tf.catch_report_pk
#                                                   JOIN akfish_report.calendar_date td on td.calendar_date_pk = tf.trip_target_date_pk
#                                                   JOIN akfish_report.gear g on g.gear_pk = tf.gear_pk
#                                                   WHERE k.year >=", ADPyear - 12,"
#                                                   AND cr.el_report_id is not null")))

up_dates[, ':='(DB_TRIP_TARGET_DATE = as.Date(DB_TRIP_TARGET_DATE), DB_LANDING_DATE = as.Date(DB_LANDING_DATE))]

# View the changes to be made to dates
changes <- unique(up_dates[work.data, on =.(REPORT_ID)][TRIP_TARGET_DATE != DB_TRIP_TARGET_DATE | LANDING_DATE != DB_LANDING_DATE, .(ADP, TRIP_ID, REPORT_ID, TRIP_TARGET_DATE, DB_TRIP_TARGET_DATE, LANDING_DATE, DB_LANDING_DATE)])
changes[, ':='(TRIP_TARGET_DATE_DIFF = as.numeric(DB_TRIP_TARGET_DATE - TRIP_TARGET_DATE), LANDING_DATE_DIFF = as.numeric(DB_LANDING_DATE - LANDING_DATE))][order(-abs(TRIP_TARGET_DATE_DIFF))]

# Defer to database when dates don't match
work.data <- up_dates[work.data, on =.(REPORT_ID)]
work.data[!is.na(DB_TRIP_TARGET_DATE) & TRIP_TARGET_DATE != DB_TRIP_TARGET_DATE, TRIP_TARGET_DATE := DB_TRIP_TARGET_DATE]
work.data[!is.na(DB_LANDING_DATE) & LANDING_DATE != DB_LANDING_DATE, LANDING_DATE := DB_LANDING_DATE]
work.data[, ':='(DB_TRIP_TARGET_DATE = NULL, DB_LANDING_DATE = NULL)]

# View the changes to be made to gear
work.data[AGENCY_GEAR_CODE != DB_AGENCY_GEAR_CODE, .(REPORT_IDS = uniqueN(REPORT_ID)), by = .(AGENCY_GEAR_CODE, DB_AGENCY_GEAR_CODE)]
          
# Defer to database when gear doesn't match
work.data[AGENCY_GEAR_CODE != DB_AGENCY_GEAR_CODE, AGENCY_GEAR_CODE := DB_AGENCY_GEAR_CODE]
work.data[, DB_AGENCY_GEAR_CODE := NULL]

# Maximum date in valhalla - This will be saved as an output and used to ensure 
# 3 full years of data are packaged for trips_melt, efrt, and full coverage summaries
max_date <- max(work.data$LANDING_DATE, na.rm=TRUE)                    

# Default to the lesser ADP year when a trip spans two years, because that is the ADP year we deploy by.
work.data[, ADP := min(ADP), by = .(TRIP_ID)]

# Add ADP year to all TRIP_IDs if any other TRIP_IDs are found in more than one year
if(nrow(select(work.data, TRIP_ID, ADP) %>% 
        distinct() %>% 
        group_by(TRIP_ID) %>% 
        filter(n()>1) %>% 
        data.frame()) > 0){
   work.data$TRIP_ID <- paste(work.data$ADP, work.data$TRIP_ID, sep = ".")
   message("Adding 'year.' to duplicate TRIP_IDs across years")
  } else{message("No duplicate TRIP_IDs across years")}

# Format data -------------------------------------------------------------

# * Vessel lengths ----
FMAVL <- FMAVL %>% filter(VESSEL_ID %in% unique(work.data$VESSEL_ID)) 
AKROVL <- AKROVL %>% filter(VESSEL_ID %in% unique(work.data$VESSEL_ID)) 

VL <- merge(FMAVL, AKROVL, all = TRUE)

# Update vessel length, giving precedence to FMA lengths
work.data <- merge(work.data, VL, all.x = TRUE)
work.data <- mutate(work.data, LENGTH_OVERALL = ifelse(!is.na(FMAVL), FMAVL, LENGTH_OVERALL))
work.data <- mutate(work.data, LENGTH_OVERALL = ifelse(is.na(LENGTH_OVERALL), AKROVL, LENGTH_OVERALL))                    
work.data <- select(work.data, -c(FMAVL, AKROVL))

# * Combine catcher-processors and motherships into one processing sector ----
work.data <- mutate(work.data, PROCESSING_SECTOR = ifelse(PROCESSING_SECTOR %in% c("CP", "M"), "CP_M", PROCESSING_SECTOR))

# * CVG_NEW ---- 

# View past coverage / strata combinations
distinct(work.data, COVERAGE_TYPE, STRATA) %>% 
arrange(COVERAGE_TYPE, STRATA)

# Create empty CVG_NEW column
work.data <- mutate(work.data, CVG_NEW = NA)

# Mandatory full coverage
work.data <- mutate(work.data, CVG_NEW = ifelse(COVERAGE_TYPE == "FULL" & STRATA %in% c("FULL", "EM_TRW_EFP", "VOLUNTARY", "EM_TRW_BSAI"), "FULL", CVG_NEW))

# PCTC
work.data <- mutate(work.data, CVG_NEW = ifelse(VESSEL_ID %in% pctc$VESSEL_ID &
                                                FMP %in% c("BS", "AI", "BSAI") &
                                                AGENCY_GEAR_CODE %in% c("NPT", "PTR", "TRW") & 
                                                TRIP_TARGET_CODE == "C" &
                                                # Only applies to A & B season
                                                yday(as.Date(paste0(ADP,"-12-31"))) - yday(as.Date(TRIP_TARGET_DATE)) > 204,
                                                "FULL", CVG_NEW))

# Voluntary full coverage: opted in for ADPyear
work.data <- mutate(work.data, CVG_NEW = ifelse(VESSEL_ID %in% BSAIVoluntary$VESSEL_ID &
                                                FMP %in% c("BS", "AI", "BSAI") &
                                                AGENCY_GEAR_CODE %in% c("NPT", "PTR", "TRW") & 
                                                PROCESSING_SECTOR == "S",
                                                "FULL", CVG_NEW))

# Voluntary full coverage: opted in for past years, but not for ADPyear  
work.data <- mutate(work.data, CVG_NEW = ifelse(is.na(CVG_NEW) & COVERAGE_TYPE == "PARTIAL" & STRATA == "FULL", "PARTIAL", CVG_NEW))

# Cooperative full coverage
work.data <- mutate(work.data, CVG_NEW = ifelse(AGENCY_GEAR_CODE %in% c("NPT", "PTR", "TRW") &
                                                MANAGEMENT_PROGRAM_CODE %in% c("RPP", "AFA", "A80"),
                                                "FULL", CVG_NEW))

# Partial CPs
work.data <- mutate(work.data, CVG_NEW = ifelse(VESSEL_ID %in% PartialCPs$VESSEL_ID &
                                                PROCESSING_SECTOR == "CP_M", 
                                                "PARTIAL", CVG_NEW))

# View remaining coverage type / strata combinations
distinct(work.data, COVERAGE_TYPE, CVG_NEW, STRATA) %>% 
arrange(COVERAGE_TYPE, CVG_NEW, STRATA)

# Partial coverage strata
work.data <- mutate(work.data, CVG_NEW = ifelse(is.na(CVG_NEW) & COVERAGE_TYPE == "PARTIAL" & STRATA %in% c("EM", "EM Voluntary", "EM_FIXED_BSAI", "EM_FIXED_GOA", "EM_HAL", "EM_POT", "EM_TRW_EFP", "EM_TRW_GOA", "EM_TenP", "HAL", "OB_FIXED_BSAI", "OB_FIXED_GOA", "OB_TRW_BSAI", "OB_TRW_GOA", "POT", "T", "TRIP", "TRW", "TenH", "TenP", "TenTR", "VESSEL", "ZERO", "ZERO_EM_RESEARCH", "t"), "PARTIAL", CVG_NEW))

# Split trips that fished both full and partial coverage
work.data[, N := uniqueN(CVG_NEW), by = .(TRIP_ID)
          ][N > 1, TRIP_ID := ifelse(CVG_NEW != "FULL", paste(TRIP_ID, 1), paste(TRIP_ID, 2))
            ][, N := NULL]

# Check for remaining NAs in CVG_NEW
filter(work.data, is.na(CVG_NEW)) %>% 
distinct(ADP, COVERAGE_TYPE, CVG_NEW, STRATA) %>% 
arrange(ADP, COVERAGE_TYPE, CVG_NEW, STRATA)

# * STRATA_NEW ----

# For each trip, identify which FMP had most retained catch, splitting FMP by BSAI and GOA
fmp_bsai_goa <- work.data[, .(
  FMP_WT = sum(WEIGHT_POSTED[SOURCE_TABLE == "Y"], na.rm = T)
), by = .(TRIP_ID, BSAI_GOA = FMP)
][, .SD[which.max(FMP_WT)], by = .(TRIP_ID)
][, FMP_WT := NULL][]
# Merge in FMP classifications, the BSAI_COL that identifies which FMP contains the majority of retained catch. 
work.data <- work.data[fmp_bsai_goa, on = .(TRIP_ID)]

# Base STRATA_NEW on AGENCY_GEAR_CODE
work.data <- mutate(work.data, STRATA_NEW = recode(AGENCY_GEAR_CODE, "PTR" = "TRW", "NPT" = "TRW", "JIG" = "ZERO"))

# Full coverage
work.data <- mutate(work.data, STRATA_NEW = ifelse(CVG_NEW == "FULL", "FULL", STRATA_NEW))

# Zero coverage
work.data <- mutate(work.data, STRATA_NEW = ifelse(CVG_NEW == "PARTIAL" & STRATA == "ZERO", "ZERO", STRATA_NEW))

# Fixed-gear EM. Base STRATUM_NEW on gear type and BSAI_GOA
work.data <- mutate(work.data, STRATA_NEW = ifelse(VESSEL_ID %in% em_base$VESSEL_ID & STRATA_NEW %in% c("HAL", "POT"), paste("EM_FIXED", BSAI_GOA, sep = "-"), STRATA_NEW))

# Fixed-gear EM research
work.data <- mutate(work.data, STRATA_NEW = ifelse(VESSEL_ID %in% em_research$VESSEL_ID, "ZERO", STRATA_NEW))

# Trawl EM
work.data <- work.data %>% 
             group_by(TRIP_ID) %>% 
             mutate(STRATA_NEW = ifelse(VESSEL_ID %in% trawl_em$PERMIT & 
                                        all(AGENCY_GEAR_CODE == "PTR") & 
                                        all(TRIP_TARGET_CODE %in% c("B", "P")),
                                        paste("EM_TRW", FMP, sep = "_"),
                                        STRATA_NEW)) %>% 
             ungroup() %>% 
             mutate(CVG_NEW = ifelse(STRATA_NEW == "EM_TRW-GOA", "PARTIAL", CVG_NEW)) %>% 
             setDT()
work.data[STRATA_NEW %like% "EM_TRW_", STRATA_NEW := sub("EM_TRW_", "EM_TRW-", STRATA_NEW)]

# At-sea Observer Strata. Base STRATUM_NEW on gear type and BSAI_GOA
work.data[STRATA_NEW == "TRW", STRATA_NEW := paste0("OB_", STRATA_NEW, "-", BSAI_GOA)]
work.data[STRATA_NEW %in% c("HAL", "POT"), STRATA_NEW := paste0("OB_FIXED-", BSAI_GOA)]


# View all strata conversions
distinct(work.data, CVG_NEW, STRATA, STRATA_NEW) %>% 
arrange(CVG_NEW, STRATA)

# View < 40's with coverage:
under_forties <- filter(work.data, LENGTH_OVERALL < 40 & STRATA_NEW != "ZERO") %>% 
                 distinct(ADP, VESSEL_ID, TRIP_ID, STRATA, STRATA_NEW, AGENCY_GEAR_CODE, LENGTH_OVERALL) %>%
                 left_join(VL)

under_forties

# Flip STRATA_NEW to ZERO for vessels that are < 40 (according to FMA)
work.data <- mutate(work.data, CVG_NEW = ifelse(TRIP_ID %in% under_forties$TRIP_ID, "PARTIAL", CVG_NEW),
                               STRATA_NEW = ifelse(TRIP_ID %in% under_forties$TRIP_ID, "ZERO", STRATA_NEW))

# View > 40's without coverage:
over_forties <- filter(work.data, LENGTH_OVERALL > 39 & AGENCY_GEAR_CODE != "JIG" & STRATA_NEW == "ZERO" & !(VESSEL_ID %in% em_research$VESSEL_ID)) %>% 
                distinct(ADP, VESSEL_ID, TRIP_ID, STRATA, STRATA_NEW, AGENCY_GEAR_CODE, MANAGEMENT_PROGRAM_CODE, LENGTH_OVERALL) %>%
                left_join(VL)

over_forties

# Flip STRATA_NEW to gear-based strata for vessels that are > 40
work.data <- mutate(work.data, STRATA_NEW = ifelse(TRIP_ID %in% over_forties$TRIP_ID & !(VESSEL_ID %in% em_base$VESSEL_ID), AGENCY_GEAR_CODE, STRATA_NEW))

work.data <- work.data %>% 
             group_by(TRIP_ID) %>% 
             mutate(STRATA_NEW = ifelse(TRIP_ID %in% over_forties$TRIP_ID & 
                                        VESSEL_ID %in% em_base$VESSEL_ID &
                                        any(AGENCY_GEAR_CODE %in% c("HAL", "POT")), 
                                        paste("EM_FIXED", FMP, sep = "_"), 
                                        STRATA_NEW)) %>% 
             setDT()

work.data <- work.data %>% 
             group_by(TRIP_ID) %>% 
             mutate(STRATA_NEW = ifelse(any(AGENCY_GEAR_CODE == "JIG") &
                                        n_distinct(STRATA_NEW) > 1, 
                                        unique(STRATA_NEW[STRATA_NEW != "ZERO"]), 
                                        STRATA_NEW)) %>% 
             setDT()

# View over_forties strata conversions to see if they make sense
work.data %>% 
filter(TRIP_ID %in% over_forties$TRIP_ID) %>% 
distinct(AGENCY_GEAR_CODE, STRATA_NEW)

# View jig gear strata conversions to see if they make sense
work.data %>% 
filter(TRIP_ID %in% work.data[AGENCY_GEAR_CODE=="JIG", TRIP_ID]  & (STRATA != "ZERO" | STRATA_NEW != "ZERO")) %>% 
distinct(ADP, VESSEL_ID, LENGTH_OVERALL, TRIP_ID, AGENCY_GEAR_CODE, STRATA, STRATA_NEW) %>% 
arrange(ADP, VESSEL_ID, TRIP_ID)

# View all strata conversions to see if they make sense
distinct(work.data, STRATA, AGENCY_GEAR_CODE, STRATA_NEW) %>% 
arrange(STRATA_NEW, STRATA, AGENCY_GEAR_CODE) %>% 
print(n = Inf)

# View distinct coverage types and strata
distinct(work.data, CVG_NEW, STRATA_NEW) %>% 
arrange(CVG_NEW, STRATA_NEW)

# * PORT_NEW ----

# Some trips have more than one port listed. Default to the port with the most landed fish.
work.data <- left_join(work.data,
                       work.data %>%
                       group_by(TRIP_ID, PORT_CODE) %>% 
                       summarise(WEIGHT = sum(WEIGHT_POSTED, na.rm = TRUE)) %>% 
                       group_by(TRIP_ID) %>%
                       summarise(PORT_NEW = PORT_CODE[WEIGHT == max(WEIGHT)]))

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

# For remaining combo strata trips, default to stratum with the most landed weight
work.data[, N := uniqueN(STRATA_NEW), by = .(TRIP_ID)
          ][N > 1, STRATA_WEIGHT := sum(WEIGHT_POSTED[SOURCE_TABLE == "Y"], na.rm = TRUE), by = .(TRIP_ID, STRATA_NEW)
            ][N > 1, ':=' (CVG_NEW = CVG_NEW[which.max(STRATA_WEIGHT)], STRATA_NEW = STRATA_NEW[which.max(STRATA_WEIGHT)]), by = .(TRIP_ID)
              ][, ':=' (N = NULL, STRATA_WEIGHT = NULL)]

# Check for any remaining combo strata trips 
unique(work.data[, .(TRIP_ID, CVG_NEW)])[, .N, by = .(TRIP_ID)][N > 1]
unique(work.data[, .(TRIP_ID, STRATA_NEW)])[, .N, by = .(TRIP_ID)][N > 1]

# * Get TRIP_IDs for PSC ----

# Metrics:
# 1. hlbt_psc - Estimated halibut PSC (metric tons)
# 2. chnk_psc - Estimated Chinook salmon PSC (counts) 
# 3. discard  - Groundfish discards (metric tons)
# 4. crab_psc - Estimated crab PSC (counts) consisting of:
#               - Blue king crab (BKCR)
#               - Bairdi Tanner crab (BTCR)
#               - Golden (brown) king crab (GKCR)
#               - Hanasaki (spiny) king crab (HKCR)
#               - Opilio Tanner (snow) crab (OTCR)
#               - Red king crab (RKCR)

# 1. Halibut PSC
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

# 2. Chinook PSC
chnk_psc <-
  inner_join(
    # 1.1 Get distinct TRIP_ID / REPORT_ID pairs
    work.data %>% distinct(TRIP_ID, REPORT_ID),
    # 1.2 Merge those TRIP_IDs with REPORT_IDs that have Chinook PSC
    filter(PSC, SPECIES_GROUP_CODE == "CHNK"), by = "REPORT_ID") %>% 
  # 1.3 Sum weights by TRIP_ID
  group_by(TRIP_ID) %>% 
  summarize(chnk_psc = sum(PSC_TOTAL_CATCH)) %>%
  filter(chnk_psc > 0)

# 3. Groundfish discards
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

# 4. Crab PSC
crab_psc <-
  inner_join(
    # 1.1 Get distinct TRIP_ID / REPORT_ID pairs
    work.data %>% distinct(TRIP_ID, REPORT_ID),
    # 1.2 Merge those TRIP_IDs with REPORT_IDs that have crab PSC
    filter(PSC, SPECIES_GROUP_CODE %in% c("BKCR", "BTCR", "GKCR", "HKCR", "OTCR", "RKCR")) %>% 
      group_by(REPORT_ID) %>% 
      summarise(PSC_TOTAL_CATCH = sum(PSC_TOTAL_CATCH, na.rm = TRUE), .groups = "drop"), 
    by = "REPORT_ID") %>% 
  # 1.3 Sum weights by TRIP_ID
  group_by(TRIP_ID) %>% 
  summarize(crab_psc = sum(PSC_TOTAL_CATCH)) %>%
  filter(crab_psc > 0)

# Merge optimization metrics together
metrics <- full_join(hlbt_psc, chnk_psc, by = "TRIP_ID") %>% 
           full_join(discard, by = "TRIP_ID") %>% 
           full_join(crab_psc, by = "TRIP_ID")
           
# * Create trip data frames ----

# Check for duplicate STRATA_NEW or TENDER entries before merging work.data with metrics
if(nrow(
  distinct(work.data, TRIP_ID, ADP, STRATA_NEW, TENDER) %>% 
  group_by(TRIP_ID) %>% 
  filter(n() > 1)) > 0){
  stop("Multiple distinct ADP, STRATA_NEW, or TENDER by TRIP_ID in work.data")}

# Merge relevant work.data columns with metrics
trip_wgts_strata <- left_join(
  work.data %>%
  filter(CVG_NEW!="FULL" & STRATA_NEW!="ZERO") %>% 
  distinct(ADP, TRIP_ID, STRATA_NEW, TENDER),
  metrics, by="TRIP_ID")

# trip_wgts_strata has NAs where there were no PSC for a trip.  Change to zeros
trip_wgts_strata[is.na(trip_wgts_strata)] <- 0

# * Stratification ----
# We are only evaluating one stratification: HAL, POT, TRW, EM_HAL, EM_POT
trip_wgts_strata <- rename(trip_wgts_strata, STRATA_GEAR = STRATA_NEW)

# * Melt trips data ----
# data.table::melt and reshape2::melt() work the same here
# Note this will only add a column if there is one stratification scheme
# First create a vector of all strata_schemes - 
trips_melt_s <- data.table::melt(trip_wgts_strata, 
                                 id.vars = c("TRIP_ID", "TENDER", "discard", "hlbt_psc", "chnk_psc", "crab_psc"), 
                                 measure.vars = c(colnames(select(trip_wgts_strata, dplyr::contains("STRATA")))),
                                 variable.name = "strata_scheme",
                                 value.name = "strata_ID") %>%
                filter(!is.na(strata_ID))

# Then for each strata scheme, create a vector of metrics
trips_melt <- data.table::melt(trips_melt_s, id.vars = c("TRIP_ID", "TENDER", "strata_scheme", "strata_ID"), 
                               measure.vars = c("discard", "hlbt_psc", "chnk_psc", "crab_psc"),
                               variable.name = "Metric",
                               value.name = "Value") %>%
              mutate(comboID = paste(strata_scheme, strata_ID, Metric, sep="."))

# Final formatting
trips_melt <- trips_melt %>% 
  mutate(strata_scheme = as.character(strata_scheme),
         Metric = as.character(Metric))

# * Check for NAs in trips_melt ----
if(nrow(trips_melt %>% filter_all(any_vars(is.na(.)))) != 0){stop("NAs detected in trips_melt")}

# * efrt object ----

# 'efrt' is just a simplified verson of work.data for non-jig PC trips for the past 3 years, and defines pool (OB, EM, or ZE)
# Uses 'max_date' to trim dataset to last 3 full years (instead of using ADP)
wd <- unique(work.data[CVG_NEW=="PARTIAL" & AGENCY_GEAR_CODE!="JIG", .(PERMIT=VESSEL_ID, TARGET=TRIP_TARGET_CODE, PORT = PORT_NEW, AREA=REPORTING_AREA_CODE, AGENCY_GEAR_CODE, GEAR=ifelse(AGENCY_GEAR_CODE %in% c("PTR", "NPT"), "TRW", AGENCY_GEAR_CODE), STRATA=STRATA_NEW, FMP, START=min(TRIP_TARGET_DATE, LANDING_DATE, na.rm=TRUE), END=max(TRIP_TARGET_DATE, LANDING_DATE, na.rm=TRUE)), keyby=.(ADP, TRIP_ID)])
wd[TARGET=="B", TARGET:="P"]  # Convert all 'bottom pollock' to 'pelagic pollock', allowing us to treat all pollock target trips the same
wd[, POOL := ifelse(grepl("EM", STRATA), "EM", ifelse(STRATA == "ZERO", "ZE", "OB"))]   # define pool
wd[, MONTH := month(START)]
wd <- wd[, .(ADP, POOL, STRATA, FMP, PORT, AREA, TARGET, AGENCY_GEAR_CODE, GEAR, PERMIT, TRIP_ID, START, END, MONTH)]
efrt <- unique(wd)  # final efrt object to output!

# If any trips fished both PTR and NPT gear, remove instances of PTR - only trips that fished with PTR exclusively can be within the pollock EFP
trw_dup <- unique(efrt[grepl("TRW", STRATA) & AGENCY_GEAR_CODE %in% c("NPT", "PTR"), .(ADP, POOL, STRATA, AGENCY_GEAR_CODE, GEAR, PERMIT, TRIP_ID, START, END, MONTH)])
trw_dup_id <- unique(trw_dup[, .N, by=TRIP_ID][N>1, TRIP_ID])
if(as.numeric(diff(table(trw_dup[TRIP_ID %in% trw_dup_id, AGENCY_GEAR_CODE]))) != 0){warning("Something else other than NPT/PTR is creating unique records.")}
efrt <- unique(efrt[TRIP_ID %in% trw_dup_id & AGENCY_GEAR_CODE == "PTR", AGENCY_GEAR_CODE := "NPT"])        # Remove PTR records by making all trips by these mixed-gear trips have NPT only

# Check that fixed gear duplicates are due only to combo HAL/POT gear trips (which we will retain in AGENCY_GEAR_CODE and GEAR)
hal_pot_dup <- unique(efrt[POOL %in% c("OB", "ZE") & AGENCY_GEAR_CODE %in% c("HAL", "POT"), .(ADP, POOL, STRATA, AGENCY_GEAR_CODE, GEAR, PERMIT, TRIP_ID, START, END, MONTH)])
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

# Check that all trips in efrt have optimization metrics associated with them  
if((length(unique(trips_melt$TRIP_ID)) == length(unique(efrt[POOL!="ZE", TRIP_ID]))) != TRUE){message("Wait! Some trips are missing metrics in trips_melt!")}

# * Full Coverage Summary ----
full_efrt <- unique(work.data[CVG_NEW=="FULL", .(POOL="FULL", STRATA=STRATA_NEW, FMP, AREA=REPORTING_AREA_CODE, TARGET=TRIP_TARGET_CODE, AGENCY_GEAR_CODE, PERMIT, START=min(TRIP_TARGET_DATE, LANDING_DATE, na.rm=TRUE), END=max(TRIP_TARGET_DATE, LANDING_DATE, na.rm=TRUE), MONTH), keyby=.(ADP, TRIP_ID)])
full_efrt[, GEAR := ifelse(AGENCY_GEAR_CODE %in% c("NPT", "PTR"), "TRW", AGENCY_GEAR_CODE)]   # Create GEAR column (i.e. TRW instead of NPT or PTR)
full_efrt[TARGET=="B", TARGET := "P"]                                           # Simplify 'bottom pollock' and 'pelagic pollock' to have only one 'pollock' target designation
full_efrt <- unique(full_efrt)
unique(full_efrt[, .(TRIP_ID, GEAR)])[, .N, keyby=GEAR]                         # Here is a rough summary of trip counts by gear type without adjusting for EM/COD

# * EM Vessel Summary ----
fg_em <- setDT(copy(em_base))[, .(VESSEL_NAME, PERMIT=VESSEL_ID)]

# Add back EM research vessels
fg_em <- rbind(fg_em, data.table(VESSEL_NAME = em_research$VESSEL_NAME, PERMIT = em_research$VESSEL_ID))
fg_em[, FLAG := ifelse(PERMIT %in% em_research$VESSEL_ID, "RESEARCH", "NONE")]

# Add back EM requesting vessels
fg_em <- rbind(fg_em, setDT(copy(em_requests))[, .(VESSEL_NAME, PERMIT=VESSEL_ID)], fill = TRUE)
fg_em[, FLAG := ifelse(PERMIT %in% em_requests$VESSEL_ID, "REQUEST", FLAG)]

# Make vessel list unique
fg_em <- unique(fg_em)

# Counts of vessels listed in EM
fg_em[, .N, by=FLAG]  

# * Final outputs ----
out_name <- paste(ADPyear, ADP_version, "ADP_data.rdata", sep="_")
out_save <- getPass::getPass(paste0("Would you like to save off a new version of ", out_name, "? (Enter Y or N)"))

if(out_save == "Y"){                              
save(list = c("work.data", "trips_melt", "efrt", "PartialCPs", "full_efrt", "max_date", "fg_em"), file=paste0("source_data/", out_name))
}

#' Upload to shared Gdrive source_data folder
gdrive_upload(
  local_path = paste0("source_data/", out_name),
  gdrive_dribble = gdrive_set_dribble("Projects/ADP/source_data/")
)
