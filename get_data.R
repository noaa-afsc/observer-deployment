# User inputs -------------------------------------------------------------
ADPyear     <- 2024     # Enter numeric year you are doing the ADP for
ADP_version <- "Draft"  # Enter "Draft" or "Final"
EM_final    <- "N"      # Is the final EM list approved? Y/N (caps)
location    <- getPass::getPass('Enter your location: Juneau or Seattle') # Your physical location
options(scipen = 9999)  # Avoid scientific notation

# Get packages ------------------------------------------------------------
if(tolower(location) == "seattle") if(!require("odbc")) install.packages("odbc", repos='http://cran.us.r-project.org')
if(tolower(location) == "juneau") if(!require("ROracle")) install.packages("ROracle", repos='http://cran.us.r-project.org')
if(!require("data.table"))   install.packages("data.table", repos='http://cran.us.r-project.org')
if(!require("lubridate"))   install.packages("lubridate", repos='http://cran.us.r-project.org') # For fixing datetimes
if(!require("tidyverse"))   install.packages("tidyverse", repos='http://cran.us.r-project.org') # ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats  

# Get user-defined functions ----------------------------------------------
source("common_functions/model_trip_duration.R")    # load in trip duration function 

# Establish channels ------------------------------------------------------
channel_afsc <- eval(parse(text = Sys.getenv("channel_afsc")))
channel_akro <- eval(parse(text = Sys.getenv("channel_cas")))

# Data queries ------------------------------------------------------------

# * PSC ----
PSC <- 
  dbGetQuery(channel_akro,
             paste("SELECT
                   year,
                   el_report_id as report_id,
                   species_group_code, 
                   sum(case when psc_total_count is not null then psc_total_count
                            when psc_total_catch_weight is not null then psc_total_catch_weight
                            else 0 end) as psc_total_catch,
                   sum(psc_total_mortality_weight) as psc_total_mortality_weight
                   FROM akfish_report.v_cas_psc
                   WHERE year >=", ADPyear - 12,"
                   AND el_report_id is not null
                   GROUP BY year, el_report_id, species_group_code"))

# * Voluntary full coverage ----
#   Requests to join must be made prior to October 15  
BSAIVoluntary <-
  dbGetQuery(channel_afsc,
             if(ADP_version == "Draft"){
               paste(
                 " -- From Andy Kingham
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
                 ON eos.VESSEL_SEQ = ovsp.VESSEL_SEQ
                 AND eos.sample_plan_seq = ovsp.sample_plan_seq
                 JOIN norpac.odds_annual_opt_strata aos
                 ON aos.ELIGIBLE_OPT_SEQ = eos.ELIGIBLE_OPT_SEQ 
                 WHERE ovsp.SAMPLE_PLAN_SEQ = 8
                 AND EXTRACT(Year FROM ovsp.end_date) > ", ADPyear - 2, "
                 AND aos.year_eligible = ", ADPyear - 1, "")} else
                   paste(
                     " -- From Andy Kingham
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
                     ON eos.VESSEL_SEQ = ovsp.VESSEL_SEQ
                     AND eos.sample_plan_seq = ovsp.sample_plan_seq
                     JOIN norpac.odds_annual_opt_strata aos
                     ON aos.ELIGIBLE_OPT_SEQ = eos.ELIGIBLE_OPT_SEQ 
                     WHERE ovsp.SAMPLE_PLAN_SEQ = 8
                     AND EXTRACT(Year FROM ovsp.end_date) > ", ADPyear - 1, "
                     AND aos.year_eligible = ", ADPyear, "")
                   )

# * Partial Coverage CPs ----
#   Requests to join must be made prior to July 1  
#   F/V Trident doesn't have an FFP and operates solely in state waters 
#   so therefore it's not subject to observer coverage. For accounting purposes, 
#   we treat it like one of these small CPs, so it's on this list.
#   For more info on this, see Alicia Miller.  
PartialCPs <- dbGetQuery(channel_akro,
                         paste("select distinct ev.vessel_id, ev.begin_date, v.name as vessel_name, e.name as elibibility
                               from akfish.eligible_vessel ev
                               join akfish.eligibility e on e.id = ev.eligibility_id
                               join akfish_report.vessel v on v.vessel_id = ev.vessel_id
                               where e.name = 'CP PARTIAL COVERAGE'
                               and ev.end_date is null
                               and v.end_date is null
                               and v.expire_date is null"))

# * Fixed-gear EM research ---- 
em_research <- 
  dbGetQuery(channel_afsc, 
      if(ADP_version == "Draft"){
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

# * Trawl EM ----
trawl_em <- setDT(dbGetQuery(channel_akro, paste0(
            "select distinct
            ev.vessel_id, v.name as vessel_name, extract(year from ev.begin_date) as begin_year
            from akfish.eligible_vessel ev
            join akfish.eligibility e on e.id = ev.eligibility_id
            join akfish_report.vessel v on v.vessel_id = ev.vessel_id
            where e.name like '%Trawl EM%'
            and v.end_date is null
            and v.expire_date is null
            and extract(year from ev.begin_date) < ", ADPyear + 1, "
            and (ev.end_date is null or extract(year from ev.end_date) = ", ADPyear, ")
            order by v.name")))

# * Vessel lengths ----
AKROVL <- dbGetQuery(channel_afsc, "select distinct ID as vessel_id, length_overall as akrovl
                     FROM norpac_views.akr_v_vessel_mv")

FMAVL <- dbGetQuery(channel_afsc, "SELECT DISTINCT PERMIT as vessel_id, length as fmavl 
                    FROM norpac.atl_lov_vessel")

# * Valhalla ----
# Pull data from each of the four years prior to ADPyear.
work.data <- dbGetQuery(channel_afsc, paste0("select * from loki.akr_valhalla"))

# Convert dates using as.Date to avoid timestamp issues
work.data <- mutate(work.data, TRIP_TARGET_DATE = as.Date(TRIP_TARGET_DATE), LANDING_DATE = as.Date(LANDING_DATE))

# Convert VESSEL_IDs using as.character to facilitate a later join with vessel lengths
work.data <- mutate(work.data, VESSEL_ID = as.character(VESSEL_ID))

# In the past, there was one record for which TENDER == "n", so convert to upper case
work.data <- mutate(work.data, TENDER = toupper(TENDER))

# Defer to database dates for all trips in valhalla with dates that don't match the database
up_dates <- setDT(dbGetQuery(channel_akro, paste0("select distinct 
                                                  cr.el_report_id as report_id,
                                                  td.calendar_date as db_trip_target_date,
                                                  cr.el_report_date as db_landing_date
                                                  FROM akfish_report.transaction_fact tf
                                                  JOIN akfish_report.v_current_keys K ON tf.TRANSACTION_FACT_LOG_PK = k.transaction_fact_log_pk
                                                  AND tf.PARTITION_KEY_PK = k.partition_key_pk
                                                  JOIN akfish_report.catch_report cr on cr.catch_report_pk= tf.catch_report_pk
                                                  JOIN akfish_report.calendar_date td on td.calendar_date_pk = tf.trip_target_date_pk
                                                  WHERE k.year >=", ADPyear - 11,"
                                                  AND cr.el_report_id is not null")))

up_dates[, ':='(DB_TRIP_TARGET_DATE = as.Date(DB_TRIP_TARGET_DATE), DB_LANDING_DATE = as.Date(DB_LANDING_DATE))]

# View the changes to be made
changes <- unique(up_dates[work.data, on =.(REPORT_ID)][TRIP_TARGET_DATE != DB_TRIP_TARGET_DATE | LANDING_DATE != DB_LANDING_DATE, .(ADP, TRIP_ID, REPORT_ID, TRIP_TARGET_DATE, DB_TRIP_TARGET_DATE, LANDING_DATE, DB_LANDING_DATE)])
changes[, ':='(TRIP_TARGET_DATE_DIFF = as.numeric(DB_TRIP_TARGET_DATE - TRIP_TARGET_DATE), LANDING_DATE_DIFF = as.numeric(DB_LANDING_DATE - LANDING_DATE))][order(-abs(TRIP_TARGET_DATE_DIFF))]

# Defer to database dates where dates don't match
work.data <- up_dates[work.data, on =.(REPORT_ID)]
work.data[!is.na(DB_TRIP_TARGET_DATE) & TRIP_TARGET_DATE != DB_TRIP_TARGET_DATE, TRIP_TARGET_DATE := DB_TRIP_TARGET_DATE]
work.data[!is.na(DB_LANDING_DATE) & LANDING_DATE != DB_LANDING_DATE, LANDING_DATE := DB_LANDING_DATE]
work.data[, ':='(DB_TRIP_TARGET_DATE = NULL, DB_LANDING_DATE = NULL)]

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

# Update vessel length, giving precedence to AKRO lengths
work.data <- merge(work.data, VL, all.x = TRUE)
work.data <- mutate(work.data, LENGTH_OVERALL = ifelse(!is.na(AKROVL), AKROVL, LENGTH_OVERALL))
work.data <- mutate(work.data, LENGTH_OVERALL = ifelse(is.na(LENGTH_OVERALL), FMAVL, LENGTH_OVERALL))                    
work.data <- select(work.data, -c(FMAVL, AKROVL))

# * Combine catcher-processors and motherships into one processing sector ----
work.data <- mutate(work.data, PROCESSING_SECTOR = ifelse(PROCESSING_SECTOR %in% c("CP", "M"), "CP_M", PROCESSING_SECTOR))

# * View coverage type / strata combinations ----
distinct(work.data, COVERAGE_TYPE, STRATA) %>% arrange(COVERAGE_TYPE, STRATA)
  
# * CVG_NEW ---- 

# View past coverage / strata combinations
distinct(work.data, COVERAGE_TYPE, STRATA) %>% 
arrange(COVERAGE_TYPE, STRATA)

# Create empty CVG_NEW column
work.data <- mutate(work.data, CVG_NEW = NA)

# Mandatory full coverage
work.data <- mutate(work.data, CVG_NEW = ifelse(COVERAGE_TYPE == "FULL" & STRATA %in% c("FULL", "EM_TRW_EFP", "VOLUNTARY"), "FULL", CVG_NEW))

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
work.data <- mutate(work.data, CVG_NEW = ifelse(is.na(CVG_NEW) & COVERAGE_TYPE == "PARTIAL" & STRATA %in% c("EM", "EM Voluntary", "EM_HAL", "EM_POT", "EM_TenP", "EM_TRW_EFP", "HAL", "POT", "t", "T", "TenH", "TenP", "TenTR", "TRIP", "TRW", "VESSEL", "ZERO", "ZERO_EM_RESEARCH"), "PARTIAL", CVG_NEW))

# Check for remaining NAs in CVG_NEW
filter(work.data, is.na(CVG_NEW)) %>% 
distinct(ADP, COVERAGE_TYPE, CVG_NEW, STRATA) %>% 
arrange(ADP, COVERAGE_TYPE, CVG_NEW, STRATA)

# * STRATA_NEW ----

# Base STRATA_NEW on AGENCY_GEAR_CODE
work.data <- mutate(work.data, STRATA_NEW = recode(AGENCY_GEAR_CODE, "PTR" = "TRW", "NPT" = "TRW", "JIG" = "ZERO"))

# Full coverage
work.data <- mutate(work.data, STRATA_NEW = ifelse(CVG_NEW == "FULL", "FULL", STRATA_NEW))

# Zero coverage
work.data <- mutate(work.data, STRATA_NEW = ifelse(CVG_NEW == "PARTIAL" & STRATA == "ZERO", "ZERO", STRATA_NEW))

# Fixed-gear EM
work.data <- mutate(work.data, STRATA_NEW = ifelse(VESSEL_ID %in% em_base$VESSEL_ID & STRATA_NEW %in% c("HAL", "POT"), paste("EM", STRATA_NEW, sep = "_"), STRATA_NEW))

# Fixed-gear EM research
work.data <- mutate(work.data, STRATA_NEW = ifelse(VESSEL_ID %in% em_research$VESSEL_ID, "ZERO", STRATA_NEW))

# Trawl EM ----
work.data <- work.data %>% 
             group_by(TRIP_ID) %>% 
             mutate(STRATA_NEW = ifelse(VESSEL_ID %in% trawl_em$VESSEL_ID & 
                                        all(AGENCY_GEAR_CODE == "PTR") & 
                                        all(TRIP_TARGET_CODE %in% c("B", "P")),
                                        "EM_TRW_EFP",
                                        STRATA_NEW)) %>% 
             ungroup() %>% 
             setDT()

# View all strata conversions
distinct(work.data, CVG_NEW, STRATA, STRATA_NEW) %>% 
arrange(CVG_NEW, STRATA)

# View < 40's with coverage:
under_forties <- filter(work.data, LENGTH_OVERALL < 40 & STRATA_NEW != "ZERO") %>% 
                 distinct(ADP, VESSEL_ID, TRIP_ID, STRATA, STRATA_NEW, AGENCY_GEAR_CODE, LENGTH_OVERALL) %>%
                 left_join(VL)

under_forties

# Reduce the list to vessels that are < 40 according to FMA
under_forties <- filter(under_forties, FMAVL < 40 | is.na(FMAVL))

# Flip STRATA_NEW to ZERO for vessels that are < 40 according to fma
work.data <- mutate(work.data, STRATA_NEW = ifelse(TRIP_ID %in% under_forties$TRIP_ID, "ZERO", STRATA_NEW))

# View > 40's without coverage:
over_forties <- filter(work.data, LENGTH_OVERALL > 39 & AGENCY_GEAR_CODE != "JIG" & STRATA_NEW == "ZERO" & !(VESSEL_ID %in% em_research$VESSEL_ID)) %>% 
                distinct(ADP, VESSEL_ID, TRIP_ID, STRATA, STRATA_NEW, AGENCY_GEAR_CODE, MANAGEMENT_PROGRAM_CODE, LENGTH_OVERALL) %>%
                left_join(VL)

over_forties

# Flip STRATA_NEW to gear-based strata for vessels that are > 40
# Do so manually, based on the characteristics of the vessel's trips
# The VL object contains the most up-to-date vessel lengths, so it could
# be that a vessel which was < 40 (and therefore zero coverage) is now
# > 40 (and should therefore be switched to a gear-based stratum)
work.data <- mutate(work.data, STRATA_NEW = ifelse(TRIP_ID %in% over_forties$TRIP_ID & !(VESSEL_ID %in% em_base$VESSEL_ID), AGENCY_GEAR_CODE, STRATA_NEW))

work.data <- work.data %>% 
             group_by(TRIP_ID) %>% 
             mutate(STRATA_NEW = ifelse(TRIP_ID %in% over_forties$TRIP_ID & 
                                        VESSEL_ID %in% em_base$VESSEL_ID, 
                                        paste("EM", unique(AGENCY_GEAR_CODE[AGENCY_GEAR_CODE != "JIG"]), sep = "_", collapse = " "), 
                                        STRATA_NEW)) %>% 
             setDT()

work.data <- work.data %>% 
             group_by(TRIP_ID) %>% 
             mutate(STRATA_NEW = ifelse(any(AGENCY_GEAR_CODE == "JIG") &
                                        n_distinct(STRATA_NEW) > 1, 
                                        unique(STRATA_NEW[STRATA_NEW != "ZERO"]), 
                                        STRATA_NEW)) %>% 
             setDT()

# View jig gear to see if STRATA_NEW makes sense
work.data %>% 
filter(TRIP_ID %in% work.data[AGENCY_GEAR_CODE=="JIG", TRIP_ID]  & (STRATA != "ZERO" | STRATA_NEW != "ZERO")) %>% 
distinct(ADP, VESSEL_ID, TRIP_ID, AGENCY_GEAR_CODE, STRATA, STRATA_NEW) %>% 
arrange(ADP, VESSEL_ID, TRIP_ID)

# View strata conversions to see if they make sense
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

# initial run performs the queries and joins and runs with a default model of DAYS ~ RAW. Spits 'mod_dat' to the global environment
td_init <- model_trip_duration(ADP_version, ADPyear)  
td_init$TD_PLOT
td_init$DIAG

# After some testing, the model below was decided as the best for predicting trip durations
td_mod <- model_trip_duration(ADP_version, ADPyear, use_mod="DAYS ~ RAW + ADP*AGENCY_GEAR_CODE")
td_mod$TD_PLOT
td_mod$DIAG     

# Subset all observer pool trips for the most recent 3 years
td_dat <- unique(work.data[CVG_NEW!="FULL" & STRATA_NEW != "ZERO" & AGENCY_GEAR_CODE != "JIG", .(VESSEL_ID, ADP, AGENCY_GEAR_CODE, START=min(TRIP_TARGET_DATE, LANDING_DATE, na.rm=TRUE), END=max(TRIP_TARGET_DATE, LANDING_DATE, na.rm=TRUE), TENDER), by=.(TRIP_ID)])
td_dat <- td_dat[END >= (max_date-years(3))]                                             # Subset this to get full 3 years of prior fishing effort
td_dat[, RAW := as.numeric(END-START, units="days")]                              # calculate raw trip duration (fishing end minus fishing start)
td_dat[, ADP := as.factor(ADP)]                                                   # convert ADP to a factor so it is handled by the model correctly
td_dat[, AGENCY_GEAR_CODE := factor(AGENCY_GEAR_CODE, levels=c("HAL", "POT", "PTR", "NPT"))]   # Set factor levels

# Apply the model to predict trip duration
td_dat[, MOD := round(predict(td_mod$TD_MOD, newdata=td_dat)/0.5)*0.5]              # Predict trip durations for each trip, and round to the nearest 0.5 days
message(paste(nrow(td_dat[MOD < 0.5]), "trips had estimated trip durations less than 0.5 days"))
td_dat[MOD < 0.5]                                                                 # These are about to get coerced to 0.5 days - make sure this is reasonable!
td_dat[MOD < 0.5, MOD := 0.5]                                                     # I'm doing it!

# TODO : These trip start/end dates in Valhalla should be verified!
head(td_dat[order(-MOD)], 10)                                                     # Looking at 10 longest trips - Some of these must be keypunch errors!

# Check the model fit - make sure durations make reasonable sense - limiting axes to exclude outliers
ggplot(td_dat, aes(x=RAW, y=MOD, color=TENDER)) + geom_point(position=position_jitter(width=0.2, height=0.2)) + facet_grid(AGENCY_GEAR_CODE ~ ADP) + xlim(0, 30) + ylim(0, 30) + geom_abline(slope=1) + scale_color_manual(values=c("blue", "red"))
# Notice that tendered PTR/NPT trips have low slops and don't track with RAW. This behavior appears consistent with actuals in ODDS (td_mod$TD_PLOT)

# Some trips may have had more than one gear type (typically HAL+POT and PTR+NPT) The trip durations for these trips will be averaged across both instances
# and arounded to the nearest 0.5 (e.g., an averaged duration of 3.25 would be rounded down to 3.00, but a duration of 3.75 would be rounded up to 4.00)
td_dat[TRIP_ID %in% td_dat[, .(N = .N), by=TRIP_ID][N>1, TRIP_ID]][order(TRIP_ID)]
td_out <- td_dat[, .(DAYS = round(mean(MOD)/0.5)*0.5), keyby=.(TRIP_ID)]          # Final output of estimated trip duration

rm(mod_dat, td_init, td_dat)  # removing everything except td_out and td_mod

# * Get TRIP_IDs for PSC ----
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

# Merge back trip durations
trip_wgts_strata[, DAYS := td_out[trip_wgts_strata, DAYS, on=.(TRIP_ID)]]
if(nrow(trip_wgts_strata[is.na(DAYS) & ADP>=ADPyear-3]) > 0){message("Uh-oh! Some trips from the last 3 years are missing durations.")}

# * Melt trips data ----
# data.table::melt and reshape2::melt() work the same here
# Note this will only add a column if there is one stratification scheme
# First create a vector of all strata_schemes - 
trips_melt_s <- data.table::melt(trip_wgts_strata, 
                                 id.vars = c("TRIP_ID", "TENDER", "DAYS", "discard", "hlbt_psc", "chnk_psc"), 
                                 measure.vars = c(colnames(select(trip_wgts_strata, dplyr::contains("STRATA")))),
                                 variable.name = "strata_scheme",
                                 value.name = "strata_ID") %>%
                filter(!is.na(strata_ID))

# Then for each strata scheme, create a vector of metrics
trips_melt <- data.table::melt(trips_melt_s, id.vars = c("TRIP_ID", "TENDER", "DAYS", "strata_scheme", "strata_ID"), 
                               measure.vars = c("discard", "hlbt_psc", "chnk_psc"),
                               variable.name = "Metric",
                               value.name = "Value") %>%
              mutate(comboID = paste(strata_scheme, strata_ID, Metric, sep="."))

# Final formatting
trips_melt <- trips_melt %>% 
  mutate(strata_scheme = as.character(strata_scheme),
         Metric = as.character(Metric))

# * Check for NAs in trips_melt ----
if(nrow(trips_melt %>% select(-DAYS) %>% filter_all(any_vars(is.na(.)))) != 0){stop("NAs detected in trips_melt")}

# * Add post-strata to trips_melt and work.data ----
trips_melt <- trips_melt %>% 
              mutate(P_STRATA = ifelse(strata_ID == "TRW" & TENDER == "Y", "TenTR", strata_ID)) %>%
              mutate(P_STRATA = ifelse(strata_ID == "POT" & TENDER == "Y", "TenP", P_STRATA))

work.data <- work.data %>% 
             mutate(P_STRATA = ifelse(STRATA_NEW == "POT" & TENDER == "Y", "TenP", STRATA_NEW)) %>% 
             mutate(P_STRATA = ifelse(STRATA_NEW == "TRW" & TENDER == "Y", "TenTR", P_STRATA))

# * efrt object ----

# 'efrt' is just a simplified verson of work.data for non-jig PC trips for the past 3 years, and defines pool (OB, EM, or ZE)
# Uses 'max_date' to trim dataset to last 3 full years (instead of using ADP)
wd <- unique(work.data[CVG_NEW=="PARTIAL" & AGENCY_GEAR_CODE!="JIG", .(PERMIT=VESSEL_ID, TARGET=TRIP_TARGET_CODE, PORT = PORT_NEW, AREA=REPORTING_AREA_CODE, AGENCY_GEAR_CODE, GEAR=ifelse(AGENCY_GEAR_CODE %in% c("PTR", "NPT"), "TRW", AGENCY_GEAR_CODE), STRATA=STRATA_NEW, P_STRATA, START=min(TRIP_TARGET_DATE, LANDING_DATE, na.rm=TRUE), END=max(TRIP_TARGET_DATE, LANDING_DATE, na.rm=TRUE)), keyby=.(ADP, TRIP_ID)])
wd <- wd[END >= (max_date-years(3))]                                                  # subsetting so only trips within last 3 years are included
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

# If trips_melt has metrics from more than three years, trim it down (we don't need more than 3 years!)
trips_melt <- trips_melt[TRIP_ID %in% efrt[POOL!="ZE", TRIP_ID]] # subset trips_melt by trip_IDs in the most recent 3 years
if((length(unique(trips_melt$TRIP_ID)) == length(unique(efrt[POOL!="ZE", TRIP_ID]))) != TRUE){message("Wait! Some trips are missing metrics in trips_melt!")}

# * Full Coverage Summary ----
full_efrt <- unique(work.data[CVG_NEW=="FULL", .(POOL="FULL", STRATA, P_STRATA, FMP, AREA=REPORTING_AREA_CODE, TARGET=TRIP_TARGET_CODE, AGENCY_GEAR_CODE, PERMIT, START=min(TRIP_TARGET_DATE, LANDING_DATE, na.rm=TRUE), END=max(TRIP_TARGET_DATE, LANDING_DATE, na.rm=TRUE), MONTH), keyby=.(ADP, TRIP_ID)])
full_efrt[, GEAR := ifelse(AGENCY_GEAR_CODE %in% c("NPT", "PTR"), "TRW", AGENCY_GEAR_CODE)]   # Create GEAR column (i.e. TRW instead of NPT or PTR)
full_efrt <- full_efrt[END >= max_date - years(1)]                              # Subset, so one full calendar year of fishing effort is included
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

# Counts of vessels listed in EM
fg_em[, .N, by=FLAG]  

# * Final outputs ----
out_name <- paste(ADPyear, ADP_version, "ADP_data.rdata", sep="_")
out_save <- getPass::getPass(paste0("Would you like to save off a new version of ", out_name, "? (Enter Y or N)"))

if(out_save == "Y"){                              
save(list = c("work.data", "trips_melt", "efrt", "PartialCPs", "full_efrt", "max_date", "fg_em", "td_mod"), file=paste0("data/", out_name))
}
