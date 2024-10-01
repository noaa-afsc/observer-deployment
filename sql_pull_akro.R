#' *sql_pull_akro*
#' This script is to be run by AKRO staff to pull all the necessary source data for the ADP.
#' AKRO will also need to re-run Valhalla so that it can get mirrored on the loki.valhalla view.

# User inputs ----------------------------------------------------------------------------------------------------------
ADPyear     <- 2025     # Enter numeric year you are doing the ADP for
ADP_version <- "Final"  # Enter "Draft" or "Final"
options(scipen = 9999)  # Avoid scientific notation

# Get packages ---------------------------------------------------------------------------------------------------------
if(!require("devtools"))    install.packages("devtools")
if(!require("FMAtools"))    install_github("Alaska-Fisheries-Monitoring-Analytics/FMAtools")
if(!require("odbc"))        install.packages("odbc", repos='http://cran.us.r-project.org')
if(!require("ROracle"))     install.packages("ROracle", repos='http://cran.us.r-project.org')
if(!require("data.table"))  install.packages("data.table", repos='http://cran.us.r-project.org')

# Establish channels ---------------------------------------------------------------------------------------------------
channel_akro <- eval(parse(text = Sys.getenv("channel_cas")))

# Data queries ---------------------------------------------------------------------------------------------------------

## PSC ----
PSC <- setDT(dbGetQuery(channel_akro, paste(
  "
  SELECT
    year, el_report_id as report_id, species_group_code, 
    sum(CASE 
      WHEN psc_total_count IS NOT NULL THEN psc_total_count
      WHEN psc_total_catch_weight IS NOT NULL THEN psc_total_catch_weight
      ELSE 0 end) AS psc_total_catch,
    sum(psc_total_mortality_weight) AS psc_total_mortality_weight
  FROM akfish_report.v_cas_psc
  WHERE year >=", ADPyear - 4,"
  AND el_report_id IS NOT NULL
  GROUP BY year, el_report_id, species_group_code
  "
)))

## PCTC ----
pctc <- setDT(dbGetQuery(channel_akro, paste( 
  "
  SELECT DISTINCT llp.current_vessel_id vessel_id, llp.current_vessel_name vessel_name
  FROM akfish.v_llp_groundfish_license llp
    JOIN akfish.pctc_llp_initial_quota_share qs ON substr(llp.license,4,4) = qs.license_number
  WHERE llp.current_vessel_id IS NOT NULL
  ORDER BY vessel_name
  "
)))

##  Voluntary full coverage ----
#   Requests to join must be made prior to October 15  
BSAIVoluntary <- setDT(dbGetQuery(channel_akro, paste0(
  "
  SELECT DISTINCT 
    ev.vessel_id, ev.begin_date, ev.end_date, v.name AS vessel_name, e.name AS eligibility,
    trunc(ev.last_modified_date) AS last_modified_date
  FROM akfish.eligible_vessel ev
    JOIN akfish.eligibility e ON e.id = ev.eligibility_id
    JOIN akfish_report.vessel v ON v.vessel_id = ev.vessel_id
  WHERE e.name = 'CV FULL COVERAGE' AND v.end_date IS NULL AND v.expire_date IS NULL 
  ",
  if(ADP_version == "Draft" | Sys.Date() < paste0(ADPyear - 1, "-10-15")) { paste(
    "
    AND EXTRACT(YEAR FROM ev.begin_date) < ", ADPyear," 
    AND (ev.end_date IS NULL OR EXTRACT(YEAR FROM ev.end_date) >= ", ADPyear - 1,")
    "
  )} else { paste(
    "
    AND EXTRACT(YEAR FROM ev.begin_date) < ", ADPyear + 1,"
    AND (ev.end_date IS NULL OR EXTRACT(YEAR FROM ev.end_date) >= ", ADPyear,")
    "
  )},
  "ORDER BY v.name"
)))

## Valhalla Updates ----

# Defer to database dates for all trips in Valhalla with dates that don't match the database
up_dates <- setDT(dbGetQuery(channel_akro, paste0(
  "
  SELECT DISTINCT
    cr.el_report_id as report_id,
    td.calendar_date as db_trip_target_date,
    cr.el_report_date as db_landing_date,
    g.gear_code as db_agency_gear_code
  FROM akfish_report.transaction_fact tf
    JOIN akfish_report.v_current_keys K ON tf.TRANSACTION_FACT_LOG_PK = k.transaction_fact_log_pk
      AND tf.PARTITION_KEY_PK = k.partition_key_pk
    JOIN akfish_report.catch_report cr on cr.catch_report_pk= tf.catch_report_pk
    JOIN akfish_report.calendar_date td on td.calendar_date_pk = tf.trip_target_date_pk
    JOIN akfish_report.gear g on g.gear_pk = tf.gear_pk
  WHERE k.year >=", ADPyear - 12," 
  AND cr.el_report_id is not null"
)))

# Save and Upload to Shared Gdrive -------------------------------------------------------------------------------------

out_name <- paste(ADPyear, ADP_version, "ADP_data.rdata", sep = "_")

save(
  list = c("ADPyear", "ADP_version", "PSC", "pctc", "BSAIVoluntary", "up_dates"), 
  file = paste0("source_data/", out_name)
)

gdrive_upload(
  local_path = paste0("source_data/", out_name),
  gdrive_dribble = gdrive_set_dribble("Projects/ADP/source_data/")
)
