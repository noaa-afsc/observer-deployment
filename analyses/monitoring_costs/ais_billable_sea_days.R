#' *Redo-ing how we model observer assignment durations for trips in Valhalla for the 2026 ADP*
#' We recently switched from billing from the half-day to the hour for PC observer days. The existing method relies
#' on a table that still counts assignments to the half-day and therefore overestimates assignment duration, causing
#' estimates of observer deployment to be overestimated, leading to lower selection rates and underspending.

# Packages

library(data.table)
library(lubridate)

# AIS Billable Days ----

## Load monthly summaries ----

#' [https://drive.google.com/drive/folders/1T1u8JuXQ1XLEAJR9So4HtHEA8upz1I8C]
#' Downloaded as a zip and saved to the path below 

ais_sea_days_path <- "analyses/monitoring_costs/ais_billable_sea_days/"
ais_sea_days_files <- list.files(path = ais_sea_days_path, pattern = ".xlsx")

# The file names and sheet names are not consistent. Need to hardcode to combine them.
ais.2024.10 <- setDT(readxl::read_xlsx(paste0(ais_sea_days_path, "October 2024 Billable Sea Days.xlsx"), sheet = "Sheet1"))
ais.2024.11 <- setDT(readxl::read_xlsx(paste0(ais_sea_days_path, "November 2024 Billable Sea Days.xlsx"), sheet = "Billable Seadays"))
ais.2024.12 <- setDT(readxl::read_xlsx(paste0(ais_sea_days_path, "December 2024 Billable Sea Days.xlsx"), sheet = "Billable Seadays"))
ais.2025.01 <- setDT(readxl::read_xlsx(paste0(ais_sea_days_path, "January 2025 Billable Sea Days.xlsx"), sheet = "Billable Seadays"))
ais.2025.02 <- setDT(readxl::read_xlsx(paste0(ais_sea_days_path, "February 2025 Billable Days (1).xlsx"), sheet = "Billable Seadays"))
ais.2025.03 <- setDT(readxl::read_xlsx(paste0(ais_sea_days_path, "March 2025 Billable Sea Days.xlsx"), sheet = "Billable Seadays"))
ais.2025.04 <- setDT(readxl::read_xlsx(paste0(ais_sea_days_path, "April 2025 Billable Sea Days.xlsx"), sheet = "Billable Seadays"))
ais.2025.05 <- setDT(readxl::read_xlsx(paste0(ais_sea_days_path, "May 2025 Billable Sea Days.xlsx"), sheet = "Billable Seadays"))
ais.2025.06 <- setDT(readxl::read_xlsx(paste0(ais_sea_days_path, "June 2025 Billable Days.xlsx"), sheet = "June 2025 Billable Days"))

# Make a vector of all data files
ais_sea_days_dts <- ls()[grepl("^ais\\.",ls())]

# Standardize the formatting
invisible(lapply(ais_sea_days_dts, function(x) {
  # x <- "ais.2025.06"
  
  #cat(x, ", ")
  x1 <- get(x)
  
  # Standardize naming of trip number column
  trip_num.col <- colnames(x1)[colnames(x1) %like% "Trip Number"]
  setnames(x1, trip_num.col, "Trip Number")
  x1 <- x1[!is.na(`Cruise Number`)]
  
  # Remove columns with unknown CLIN items
  clin_xxxx <- colnames(x1)[colnames(x1) %like% "CLIN  XXXX"]
  if(length(clin_xxxx) > 0) {
    x2 <- subset(x1, select = clin_xxxx)[, lapply(.SD, function(x) gsub("-", NA, x)), .SDcols = clin_xxxx]
    if(any(!is.na(x2))) stop(paste0("There were non-zero values in CLIN  XXXX in ", x))
    x1 <- x1[, -..clin_xxxx]
  }
  
  # Re-assign with the reformatted versions
  assign(x, x1, envir = .GlobalEnv)
}))

out <- rbindlist(lapply(ais_sea_days_dts, get), fill = T) |>
  # Add year and month for easier sumamrization
  _[, YEAR := year(`Trip/Travel End Date`)
  ][, MONTH := month(`Trip/Travel End Date`)
  # Fix the time formatting 
  ][, START := force_tz(as.POSIXct(paste(`Trip/Travel Start Date`, format(`Trip Start Time`, '%H:%M:%S'))), tzone = "UTC")
  ][, END := force_tz(as.POSIXct(paste(`Trip/Travel End Date`, format(`Trip End Time`, '%H:%M:%S'))), tzone = "UTC")][]
# Remove the "-" in the CLIN items and convert to numeric
clin_cols <- colnames(out)[colnames(out) %like% "CLIN"]
out[, (clin_cols) := lapply(.SD, function(x) as.numeric(sub("-", "", x))  ), .SDcols = clin_cols]

# Summaries
# Note that we have > 150 days on CLIN 0004, which should actually be moved to CLIN 0005
out[, lapply(.SD, sum, na.rm = T), .SDcols = clin_cols, by = .(YEAR, MONTH)]
# Eventually we should make totals by contract year
out[, lapply(.SD, sum, na.rm = T), .SDcols = clin_cols]

## Calculate Hours ----

# Double-checking that hours are being calculated correctly, but also can help verify that data was collated correctly

# Round the start and end dates to the nearest 1 hour.
out[, DAYS := as.numeric(round_date(END, "1 hour") - round_date(START, "1 hour"), units = "days")]

cols <- c("YEAR", "MONTH", "Trip Number", "Cruise Number", '# of Sea Days CLIN 0001', "START", "END", "DAYS")
test <- out[`Trip Number` != "Plant", ..cols]
test[, DIFF := `# of Sea Days CLIN 0001` - DAYS]
test[abs(DIFF) > 0.1]
hist(test$DIFF) # All of these are within 0.1... and most are virtually zero
test[, sum(DIFF, na.rm = T)]  # Overall AIS has billed this much over our calculations
test[, round(sum(DIFF, na.rm = T), 5), by = .(YEAR, MONTH)]  # AIS was not rounding properly during the first 3 months
rm(test, cols)
# Note that the rounding functions in excel, MROUND(x, "01:00"), also doesn't consistently round up or down with times of 30 minutes
# lubridate::round_date() seems to consistently round up with times of 30 minutes
# Also note that daylight savings can also cause discrepancies on Mar-31

# Do the same for plant days
cols2 <- c("YEAR", "MONTH", "Trip Number", "Cruise Number", '# of Plant Days CLIN  0004', '# of Plant Days CLIN  0005', "START", "END", "DAYS")
test.plant <- out[`Trip Number` == "Plant", ..cols2]
test.plant[, DIFF := sum(`# of Plant Days CLIN  0004`, `# of Plant Days CLIN  0005`, na.rm = T) - DAYS, by = .I]
test.plant[abs(DIFF) > 0.1]  # No records off by more than 0.1 day
hist(test.plant$DIFF)
test.plant[, sum(DIFF, na.rm = T)]  # Overall AIS has billed this much over our calculations
test.plant[, round(sum(DIFF, na.rm = T), 5), by = .(YEAR, MONTH)]  # AIS was not rounding properly during the first 3 months
rm(test.plant, cols2)

# Link with OLS ----

ais_days <- out[, .(YEAR, MONTH, TRIP_PLAN_LOG_SEQ = `Trip Number`, CRUISE = `Cruise Number`, PERMIT = `Permit Number`, START, END, AIS_DAYS = `# of Sea Days CLIN 0001`)
                ][!(TRIP_PLAN_LOG_SEQ %in% c("-", "Plant"))][, TRIP_PLAN_LOG_SEQ := as.numeric(TRIP_PLAN_LOG_SEQ)][]
# However, some of these trips started before the new contract began with hourly billing, so exclude any trips with a
# start date of 2024-Oct-1 00:00:00
assign_excl <- ais_days[START == as.POSIXct("2024-10-01 00:00:00", tz = "UTC")] # These were all in the start of the contract!!
ais_days <- fsetdiff(ais_days, assign_excl)


## Apply the existing method ----

source("common_functions/model_trip_duration.R")

## Load Valhalla ----

adp_year <- 2026
adp_ver <- "Draft"
adp_data.path <- paste0("source_data/", adp_year, "_", adp_ver, "_ADP_data.rdata")
gdrive_download(
  local_path = adp_data.path,
  gdrive_dribble = gdrive_set_dribble("Projects/ADP/source_data/")
)
(load(adp_data.path))

# Subset to trips on and after Oct-2024 trips only
val.sub <- work.data.recent[CVG_NEW == "PARTIAL" & LANDING_DATE >= as.Date("2024-10-01")]

# Here' what we normally do
channel_afsc <- eval(parse(text = Sys.getenv('channel_afsc')))
og.mod <- model_trip_duration(val.sub, channel = channel_afsc, use_mod = "DAYS ~ RAW + AGENCY_GEAR_CODE")  # dropping ADP since we only have ~ 1 year of data
og.mod$DIAG # Not sure what is wrong with the matching now...
#' Only 82% of trips were matched - Not great? I usually get > 96%
ggplot(mod_dat, aes(x = RAW)) + 
  facet_grid(GEAR ~ .) + 
  geom_abline(slope = 1, linetype = "dashed") +
  geom_point(aes(y = DAYS)) + 
  geom_line(aes(y = MOD), color = "blue") + 
  labs(x = "Valhalla Days", y = "Observed Days", subltitle = "Actual in black points, model estimates in blue")
#' *NOTE* Can see that long valhalla trips generally have fewer than 1:1 observed days. With lm's, where the long trips 
#' have increased leverage, this caused the more common short duration trips to be overestimated in assignment duration, 
#' probably leading to overestimated monitoring costs.
mod_dat[DAYS > 20]  # We had two super-long deployments in 2024?
mod_dat[TRIP_ID == 3153633]

#' Moreover, the `norpac.odds_billdays_2025_mv` table that it's pulling off of still counts to the half-day, so the 
#' model is working off of overestimated durations anyway.

## Pulling norpac.odds_billdays_2015_mv ----

channel_afsc  <- eval(parse(text = Sys.getenv('channel_afsc')))
sea_day <- setDT(dbGetQuery(channel_afsc, paste(
  "
      SELECT DISTINCT cruise, permit, all_dates, at_sea_day_count_odds AS odds_days, odds_trip_plan_log_seq 
      FROM norpac.odds_billdays_2015_mv
      "
)))
sea_day <- sea_day[!is.na(ODDS_DAYS)]
sea_day[, YEAR := year(min(ALL_DATES)), by = ODDS_TRIP_PLAN_LOG_SEQ]
sea_day[, c("A", "B") := tstrsplit(ODDS_TRIP_PLAN_LOG_SEQ, split = ", ")]         # Where "A" is the first odds seq and "B" is the second odds seq if present, NA if not      
sea_day[, FLAG := !(is.na(B))]                                                  # If a second odds seq exists, flag as TRUE 
sea_day <- data.table::melt(sea_day, id.vars = c("CRUISE", "PERMIT", "ALL_DATES", "ODDS_DAYS", "YEAR", "FLAG"), measure.vars = c("A", "B"), value.name = "ODDS_SEQ") # Melt so each odds seq has its own row
sea_day[, ODDS_SEQ := as.numeric(ODDS_SEQ)]
sea_day <- sea_day[!is.na(ODDS_SEQ)]  # remove extraneous rows without secondary odds_seq
sea_day[FLAG == TRUE, ODDS_DAYS := ODDS_DAYS/2]   
# Grab only recent records
sea_day.test <- sea_day[ALL_DATES >= as.Date("2024-10-01") & ALL_DATES < as.Date("2025-07-01")]
sea_day.test[, sum(ODDS_DAYS)] # This table currently has 241 odds assignments totaling 875 sea days since 2024-Oct-1 though June 2025
ais_days[, sum(AIS_DAYS)]  # However, our AIS records have us at only 746.8785 days!
sea_day.test[, sum(ODDS_DAYS)]  - ais_days[, sum(AIS_DAYS)]  # Over estimated by 128.1215 days
(875 - 746.8785) / 746.8785 * 100 # Or 17% difference in days!

# Merge AIS Records with OLS ----

#' [TODO: When our OLS/ODDS data is appropriately reflecting embark/disembark to the hour, use norpac.billable_days_mv]


# Pull all ODDS trip_plan_log_seq to get trip_stratas_seq, may need to get some records from history tables too?
channel_afsc  <- eval(parse(text = Sys.getenv('channel_afsc')))
odds.pull <- setDT(dbGetQuery(channel_afsc, paste(
  "
  SELECT DISTINCT a.trip_plan_log_seq,  a.planned_embark_date, a.planned_disembark_date, a.original_embark_date, a.trip_status_code,
  b.trip_stratas_seq, b.strata AS STRATA_CODE, b.trip_monitor_code 
  FROM odds.odds_trip_plan_log a
    LEFT JOIN odds.odds_trip_stratas b
      ON a.trip_plan_log_seq = b.trip_plan_log_seq
  WHERE extract(YEAR FROM a.original_embark_date) >= 2024 AND b.strata IN (35, 36, 37, 38, 96, 196)
  ORDER BY a.trip_plan_log_seq
  "
)))

if(length(setdiff(ais_days$ODDS_SEQ, odds.pull$TRIP_PLAN_LOG_SEQ))) stop("AIS has TRIP_PLAN_LOG_SEQ that weren't in the ODDS pull!")
odds.sub2 <- odds.pull[TRIP_PLAN_LOG_SEQ %in% ais_days$TRIP_PLAN_LOG_SEQ]
odds.sub2[, table(TRIP_MONITOR_CODE)]  # All of these were monitored. Good.
# Merge AIS days with ODDS records
odds_ais <- odds.sub2[ais_days, on = .(TRIP_PLAN_LOG_SEQ)]
odds_ais[, range(START, END)]  # Range of deployment dates
odds_ais[, ':=' (START_DATE = as.Date(START), END_DATE = as.Date(END))]

# Join AIS/ODDS with Valhalla ----
valhalla.sub <- work.data.recent[CVG_NEW == "PARTIAL", .(TRIP_ID, PERMIT, STRATA, REPORT_ID, AGENCY_GEAR_CODE, TRIP_TARGET_DATE, LANDING_DATE, OBSERVED_FLAG, TENDER)] |> unique()

cols <- c("TRIP_TARGET_DATE", "LANDING_DATE", "START_DATE", "END_DATE")                   # define columns to be fed into date checking function dc()

#' This function counts days that overlap (TYPE = "o") or mismatch (TYPE = "m"), taken from `model_trip_duration`
dc <- function(sdcols, TYPE){
  dtn <- function(x) as.numeric(as.Date(x))  # define function that converts POSIXct to date and the numeric date
  v_d <- c(dtn(sdcols[[1]]) : dtn(sdcols[[2]]))
  o_d <- c(dtn(sdcols[[3]]) : dtn(sdcols[[4]]))
  o <- length(intersect(v_d, o_d))
  if(TYPE=="o"){return(o)}                                                    # type = overlap, which returns number of overlapping dates
  if(TYPE=="m"){return(max(length(v_d), length(o_d)) - o)}                    # type = mismatch, which returns number of non-overlapping dates
}

#odds_ais[, REF := as.Date(START)]; setkey(odds_ais, PERMIT, REF)

# o1 <- odds_ais[PERMIT == 1008]
# v1 <- valhalla.sub[PERMIT == 1008 & year(TRIP_TARGET_DATE) %in% c(2024, 2025) & OBSERVED_FLAG == "Y"]
# m1 <- v1[o1, roll = "nearest", nomatch = NULL] |>
#   _[, O := dc(.SD, TYPE = "o"), .SDcols = cols, by = .I
#   ][, M := dc(.SD, TYPE = "m"), .SDcols = cols, by = .I][]
# 
# match.1 <- valhalla.sub[odds_ais, roll = "nearest", nomatch = NA] |>
#   _[, O := dc(.SD, TYPE = "o"), .SDcols = cols, by = .I
#   ][, M := dc(.SD, TYPE = "m"), .SDcols = cols, by = .I][]
# 
# bad_match <- match.1[O < 1]  # These had a 'nearest' match but failed without any overlap
# keep <- fsetdiff(match.1, bad_match)
# setorder(keep, PERMIT, TRIP_ID, TRIP_TARGET_DATE, LANDING_DATE)
# 
# keep[PERMIT == 155]
# bad_match[PERMIT == 155]     # Only assigned to this vessel Oct01-Oct02
# valhalla.sub[PERMIT == 155]  # One marked monitored Sep23-Oct02, so joining on start date didn't work. Sometimes trip_target_date is earlier, dropping gear before trip??
# 


## Checking matches flagged as unmonitored in Valhalla? ----

#' TODO CHECK THESE LATER

keep[OBSERVED_FLAG == "N"]  

keep[TRIP_ID == "3188135"]  # Jan14-Jan17, OB_FIXED-BSAI
valhalla.sub[PERMIT == 32783 & year(REF) == 2025 & month(REF) == 1]  # This permit has no OB_TWR-GOA trips monitored?
#' [TODO: Should be marked as observed, also have offload record in norpac.atl_offload]

keep[TRIP_ID == "3409251"]  # Mar22-Mar-24 OB_TRW-GOA
valhalla.sub[PERMIT == 34939 & year(REF) == 2025 & month(REF) == 3] 
# Trip 3435193 has Mar23-Mar24, probably has higher O value, and has OBSERVED_FLAG == "Y". I should be able to get a better match than using sloppy joins

valhalla.sub[TRIP_ID %like% "3445896"]   # Trip Feb24-Feb27 was NOT monitored
valhalla.sub[PERMIT == 519 & STRATA_NEW == "OB_TRW-GOA" & month(REF) == 2]  # Overlap is quite good.
#' [TODO: CHECK OBSERVED FLAG, HOW IT MIGHT HAVE GOTTEN MIXED UP WITH SPLITTING LOGIC OF OB AND EM?]

keep[TRIP_ID == "3197495"]   # Trip Jan27-Jan28
valhalla.sub[PERMIT == 5242 & year(REF) == 2025 & month(REF) == 1] 
# Trip 3327962 is a perfect match and has observed_flag == Y, My match should be better

keep[TRIP_ID == "3395842"]   # Trip Mar28-Mar30
valhalla.sub[PERMIT == 7079 & year(REF) == 2025 & month(REF) == 3]
# Trip 3396525 is probably a better match, Mar29-30 with observed_flag == Y


## Make a better matching algorithm -----

# Instead of using the matching in `model_trip_duration`, do it without rolling joins and instead assess date overlap

ols_val_match <- function(odds, val) {
  # odds <- copy(odds_ais); val <- copy(valhalla.sub)
  
  # Split by permit and redorder
  odds.l <- odds[order(PERMIT, TRIP_PLAN_LOG_SEQ), .(TRIP_PLAN_LOG_SEQ, STRATA_CODE, CRUISE, PERMIT, START, END, AIS_DAYS)] |> 
    split(by = "PERMIT")
  val.l <- val[order(PERMIT, LANDING_DATE, TRIP_TARGET_DATE)] |> split(by = "PERMIT", keep.by = F)
  
  match.l <- lapply(odds.l, function(x) {
    # x <- odds.l[[1]]
    # x <- odds.l[["3713"]]
    val.p <- val.l[[as.character(unique(x$PERMIT))]]
    # If there are no valhalla trips to match, return the odds trips without any matches
    if(nrow(val.p) == 0) return(x)
    
    # Do a completed join
    cols <- c("TRIP_TARGET_DATE", "LANDING_DATE", "START", "END")
    cj <- val.p[, as.list(x), by =  val.p] |>
    # Assess overlap and mismatch. This evaluates each odds START/END dates with each REPORT_ID's TRIP_TARGET_DATE/LANDING_DATE
      _[, O := dc(.SD, TYPE = "o"), .SDcols = cols, by = .I
      ][, M := dc(.SD, TYPE = "m"), .SDcols = cols, by = .I][]
    # Keep the best matches for each odds trip seq
    out.p <- cj[, .SD[O == max(O)][M == min(M)], by = TRIP_PLAN_LOG_SEQ]
    # If a TRIP_PLAN_LOG_SEQ is linked to multiple trips that with 0 overlap, remove those matches
    out.p[, V.N := uniqueN(TRIP_PLAN_LOG_SEQ[O == 0]), by = .(TRIP_PLAN_LOG_SEQ)]
    unmatched.o <- out.p[V.N > 0 & O == 0, unique(TRIP_PLAN_LOG_SEQ)]
    rbind(x[TRIP_PLAN_LOG_SEQ %in% unmatched.o], out.p[O > 0], fill = T)[order(TRIP_PLAN_LOG_SEQ)]
  })
  out <- rbindlist(match.l, fill = T)
  setcolorder(out, c("PERMIT", "CRUISE", "TRIP_PLAN_LOG_SEQ", "TRIP_ID"))
}

match.1 <- ols_val_match(odds_ais, valhalla.sub)

# Do any of these matches have OBSEVED_FLAG == F, meaning the match was wrong or VALHALLA's flag is off?
if(nrow(match.1[OBSERVED_FLAG != "Y"])) {
  warning("Some matches have OBSERVED_FLAG = 'N'", immediate. = TRUE)
  match.1[OBSERVED_FLAG != "Y"]
}
#' [TODO: Sent these with Phil to see why. Maybe there wasn't any observer data?] We'll leave these matches unless 
#' we have a reason to remove them.

#' Check cases where TRIP_PLAN_LOG_SEQ did not match to any trips. This could be the case where observers were assigned
#' but the vessel did not fish. We should discard such cases and they are usually very short duration assignments and 
#' don't help with estimates of assignment duration for fishing trips.
if(nrow(match.1[is.na(TRIP_ID)])) {
  warning("Some permits didn't have any trips to match!", immediate. = TRUE) 
  match.1[is.na(TRIP_ID)]
  # odds_ais[TRIP_PLAN_LOG_SEQ == 202429662]   # Dec1-Dec1, probably didn't even fish? 
  # odds_ais[TRIP_PLAN_LOG_SEQ == 202430283]   # Oct7-Oct7
}
# After reviewing these trips, remove them
match.1 <- match.1[!is.na(TRIP_ID)]

# Count number of matches per TRIP_ID and TRIP_PLAN_LOG_SEQ
match.1[!is.na(TRIP_ID), O.N := uniqueN(TRIP_PLAN_LOG_SEQ), by = .(TRIP_ID)]
match.1[!is.na(TRIP_PLAN_LOG_SEQ), V.N := uniqueN(TRIP_ID), by = .(TRIP_PLAN_LOG_SEQ)]

# Check cases where TRIP_ID was matched to more than ones TRIP_PLAN_LOG_SEQ
if(nrow(match.1[O.N > 1])) {
  warning("Some TRIP_ID were matched to more than one TRIP_PLAN_LOG_SEQ.", immediate. = TRUE)
  # This might happen when VALHALLA incorrectly combines REPORT_IDs in the same trip, but double-check.
  # This is not necessarily a 'bad' outcome, as we'll compare the OLS START/END with the REPORT_ID's TRIP_TARGET_DATE/LANDING_DATE, not TRIP_ID
  match.1[O.N > 1]
}
# For the sake of the models, I'm going to manuall split these trips!
match.1[O.N > 1, TRIP_ID := paste0(TRIP_ID, ".", .SD[, rleid(TRIP_PLAN_LOG_SEQ)]), by = .(TRIP_ID)]
match.1[O.N > 1]  # View the modifications
match.1[!is.na(TRIP_ID), O.N := uniqueN(TRIP_PLAN_LOG_SEQ), by = .(TRIP_ID)]  # Re-count ODDS trips per TRIP_ID
if(nrow(match.1[O.N > 1])) stop("There are still TRIP_ID with more than on TRIP_PLAN_LOG_SEQ?") 

# Check cases where TRIP_PLAN_LOG_SEQ was matched to more than one TRIP_ID (case where there are ties in O and M)
if(nrow(match.1[V.N > 1])) {
  warning("Some TRIP_PLAN_LOG_SEQ were matched to more than one TRIP_ID", immediate. = TRUE)
  print(match.1[V.N > 1])
}

## Check to see if mismatches indicate bad matches that made it through ----

# Prepare data for the model ----
match.2 <- unique(match.1[, .(PERMIT, CRUISE, TRIP_PLAN_LOG_SEQ, AIS_DAYS, START, END, TRIP_ID)])
# Pull all valhalla TRIP_ID's that were matched, and get full range of trip target dates and landing dates, calculating RAW difference in dates
match.val <- work.data.recent[TRIP_ID %in% match.2$TRIP_ID, .(TRIP_ID, TRIP_TARGET_DATE, LANDING_DATE, AGENCY_GEAR_CODE, STRATA)] |>
  unique() |>
  _[, .(VAL_START = min(TRIP_TARGET_DATE, LANDING_DATE, na.rm = T), 
        VAL_END = max(TRIP_TARGET_DATE, LANDING_DATE, na.rm = T),
        GEAR = AGENCY_GEAR_CODE), 
    keyby = .(STRATA, TRIP_ID)
  ][, RAW := as.numeric(VAL_END - VAL_START, units = "days") + 1] |>
  unique()
# Merge RAW in
match.3 <- match.2[match.val[, .(STRATA, TRIP_ID, GEAR, RAW)], on = .(TRIP_ID)]
match.3[, GEAR_N := uniqueN(GEAR), by = .(TRIP_PLAN_LOG_SEQ)]
match.3[GEAR_N > 1]
match.3[, GEAR := as.factor(GEAR)]
match.3[, STRATA := as.factor(STRATA)]

# Make a function that performs k-fold validation and evaluates the model predictions
kfold <- function(fun, formula, data, K = 10, seed = 12345, ... ) {
  #  fun <- "lm"; formula <- AIS_DAYS ~ RAW * GEAR; data <- copy(match.3); K <- 10; seed <- 12345
  #  fun <- "lm"; formula <- AIS_DAYS ~ sqrt(RAW) * GEAR; data <- copy(match.3); K <- 10; seed <- 12345
  
  set.seed(seed)
  # Randomly create 10 roughly equal-sized datasets
  # Randomly reorder the trips in each stratum
  x <- copy(data)[order(STRATA, TRIP_PLAN_LOG_SEQ)]
  setkey(x, STRATA, TRIP_PLAN_LOG_SEQ)
  setorder(x, STRATA, TRIP_PLAN_LOG_SEQ)
  # x.split <- split(data[, .(STRATA, TRIP_PLAN_LOG_SEQ)], by = "STRATA", keep.by = F)
  # x.reorder <- rbindlist(lapply(x.split, function(x.i) x[x.i[sample(1:.N, size = .N)]]))
  x[, group := rleid(TRIP_PLAN_LOG_SEQ), by = .(STRATA)]
  x.reorder <- x[, .(STRATA, TRIP_PLAN_LOG_SEQ, group)] |>
    unique() |> 
    _[, .SD[sample(1:.N, size = .N)], by = .(STRATA)
    ][, Kgroup := rep(1:10, length.out = .N), by = .(STRATA)][]
  x.reorder <- x[x.reorder, on = .(STRATA, TRIP_PLAN_LOG_SEQ, group)]
  # Check the number of trips in each Kgroup
  #x.reorder[, uniqueN(TRIP_PLAN_LOG_SEQ), by = .(STRATA, Kgroup)]
  
  # For each k in K, train the model and test
  k.list <- vector(mode = "list", length = K)
  for(k in 1:K) {
    test <- x.reorder[Kgroup == k]
    train <- fsetdiff(x.reorder, test)
    train.mod <- do.call(fun, args = list(formula = formula, data = train, ...))
    # Now evaluate
    test[, PRED := predict(train.mod, newdata = test, type = "response")]
    test.out <- test[, .(MOD = mean(PRED)), keyby = .(STRATA, TRIP_PLAN_LOG_SEQ, AIS_DAYS, RAW, Kgroup)]
    test.out[, DIFF := MOD - AIS_DAYS]
    
    k.list[[k]] <- list(
      OVERALL = test.out[, .(
        N = .N, AIS_DAYS = sum(AIS_DAYS), MOD = sum(MOD), DIFF = sum(DIFF), PERC_DIFF = 100 * (sum(MOD) - sum(AIS_DAYS)) / sum(AIS_DAYS), SE = sd(DIFF) / sqrt(.N))],
      STRATA  = test.out[, .(
        N = .N, AIS_DAYS = sum(AIS_DAYS), MOD = sum(MOD), DIFF = sum(DIFF), PERC_DIFF = 100 * (sum(MOD) - sum(AIS_DAYS)) / sum(AIS_DAYS), SE = sd(DIFF) / sqrt(.N)), by = .(STRATA)],
      raw_data = test.out,
      mod.k = train.mod,
      test.dat = test
    )
  }
  
  # Collate the results
  out.strata <- rbindlist(lapply(k.list, "[[", "STRATA"), idcol = "k")
  out.overall <- rbindlist(lapply(k.list, "[[", "OVERALL"), idcol = "k")
  
  plot.strata <- ggplot(out.strata, aes(x = STRATA)) + geom_boxplot(aes(y = DIFF)) + geom_hline(yintercept = 0) + 
    labs(subtitle = paste0("Model: ", fun, "(", deparse(formula), ")"))
  
  out.raw <- rbindlist(lapply(k.list, "[[", "raw_data"), idcol = "k")
  # mod.lst <- lapply(k.list, "[[", "mod.k")
  
  list(
    formula = formula,
    STRATA = out.strata,
    STRATA.summary = out.strata[, lapply(.SD, mean), .SDcols = c("N", "DIFF", "PERC_DIFF", "SE"), by = .(STRATA)],
    OVERALL = out.overall,
    OVERALL.summary = out.overall[, lapply(.SD, mean), .SDcols = c("N", "DIFF", "PERC_DIFF", "SE")],
    plot = plot.strata,
    DATA = out.raw,
    test =  rbindlist(lapply(k.list, "[[", "test.dat"), idcol = "k")
  )
  
}

plot_predictions <- function(kfold.out) {
  # kfold.out <- copy(mod.0)
  ggplot(kfold.out$test, aes(x = RAW)) + 
    geom_point(aes(y = AIS_DAYS)) +
    geom_line(aes(y = PRED, group = Kgroup), color = "blue")
    geom_abline(slope = 1, linetype = "dashed") + 
    labs(x = "Valhalla days", y = "AIS billed days", subtitle = deparse(kfold.out$formula))
}

# Evaluate a variety of models ----

## lm(AIS_DAYS ~ RAW) ----

mod.0 <- kfold("lm", AIS_DAYS ~ RAW, data = unique(match.3[, -"GEAR"]))
mod.0$STRATA.summary        # Averages across Kfolds. Did very poorly with trawl
mod.0$OVERALL.summary       # Averages across Kfolds. Overall, overestimated days by 10% on average
plot_predictions(mod.0) + facet_grid(STRATA ~ .)
lm(AIS_DAYS ~ RAW, data = match.3[, -"GEAR"])
#' *NOTE:* Off the bat, we see that the model fits are kind of all over the place. Moreover, we see that there are
#' valhalla trips that appear to be very long but have only a few billed observer days. This is because some REPORT_IDs
#' in valhalla are erroneously grouped together, meaning an observed trip may get merged into the same TRIP_ID as an 
#' unobserved trip. If we use such matches with a linear modes, they have such leverage that they cause the common 
#' shorter-duration trips to get overestimated in assignment duration. 

#' The longer-duration trips are not trustworthy, so we will trim them off. Keeping Trawl trips < 6 days, FIXED_BSAI
#' trips < 7 days, and FIXED_GOA trips < 8 days seems to do the trick.
match.4 <- match.3 |>
  _[(STRATA %like% "TRW" & RAW < 6) |(STRATA %like% "FIXED_BSAI" & RAW < 7) | (STRATA %like% "FIXED_GOA" & RAW < 8)]

# Repeat with the same modelhttp://127.0.0.1:13713/graphics/plot_zoom_png?width=716&height=904
mod.1 <- kfold("lm", AIS_DAYS ~ RAW, data = unique(match.4[, -"GEAR"]))
mod.1$STRATA.summary     # OB_TRW_GOA is still a little overestimated in duration, but much improved
mod.1$OVERALL.summary    # 
plot_predictions(mod.1) + facet_grid(STRATA ~ .)
#' *NOTE* model coefficients are generally very stable between the k-folds, and predictions look reliable, but TRW could be better
# If we use this model on the full dataset...
lm(AIS_DAYS ~ RAW, data = unique(match.4[, -"GEAR"]))  
#' *intercept of -0.4154, slope of 1.0154 (lol, basically just RAW minus half a day!)*

## lm(AIS_DAYS ~ RAW + STRATA) ----

# Does using strata or gear improve prediction? 7% overestimations for trawl is more than I'd like

mod.2 <- kfold("lm", AIS_DAYS ~ RAW + STRATA, data = unique(match.4[, -"GEAR"]))
mod.2$STRATA.summary     # All within 2%
mod.2$OVERALL.summary    # 
plot_predictions(mod.2) + facet_grid(STRATA ~ .)  # Doesn't look any better?
summary(lm(AIS_DAYS ~ RAW + STRATA, data = unique(match.4[, -"GEAR"])))  # STRATA as a main effect was not significant

## lm(AIS_DAYS ~ RAW * STRATA) ----

# Using STRATA as an interaction with RAW. This is closer to what we've done in the past which used GEAR
mod.3 <- kfold("lm", AIS_DAYS ~ RAW * STRATA, data = unique(match.4[, -"GEAR"]))
mod.3$STRATA.summary     # All within 1.2%
mod.3$OVERALL.summary    # 
plot_predictions(mod.3) + facet_grid(STRATA ~ .)  # Looks better for Trawl, matches all the 2-4 day long trips well
summary(lm(AIS_DAYS ~ RAW * STRATA, data =  unique(match.4[, -"GEAR"])))  # With the interaction, coefficient was only significant for TRW stratum
#' *NOTE* This looks straightforward and promising. I would be comfortable using this model to predict durations longer
#' than the ranges we used in the trimmed dataset, but it will underestimate fishing durations for long TRW trips.

## lm(AIS_DAYS ~ RAW * GEAR) ----

#' Using GEAR as an interaction with RAW. This is closer to what we've done in the past, using GEAR. The benefit of this 
#' method is it accounts for POT trips tending to have earlier TRIP_TARGET_DATE than the trip actually fishes because 
#' gear is usually deployed before the trip that retrieves it starts. The downside is that I lose independence of my
#' data, as I'm using the same record twice, once for each gear type. In my predictions, I average the predictions for
#' each trip across its two gear types.
#' *NOTE* the benefit of using gear type is that we currently don't have any TRW-BSAI trips in our dataset, so a 
#' stratum-specific model won't be able to make predictions for that stratum. By operating on gear type, it will largely
#' use the NPT-gear estimates from the TRW-GOA stratum to create estimates for the TRW-BSAI stratum trips in the most 
#' recent year in valhalla.
mod.4 <- kfold("lm", AIS_DAYS ~ RAW * GEAR, data = match.4)
mod.4$STRATA.summary     # OB_FIXED_BSAI overestimated by 2% on average, others less than 1
mod.4$OVERALL.summary
plot_predictions(mod.4) + facet_wrap(~ STRATA + GEAR, ncol = 2)  # 
summary(lm(AIS_DAYS ~ RAW * GEAR, data = match.4))  # With the interaction, Trawl gear and interactions were significant
#' Surprisingly POT gear was not considered significantly different from HAL, contrary to what I see in the data
#' regarding the fishing start dates being earlier than the trip starts in some cases. 
#' *NOTE* Again, the potential concern with using this model is that if I feed long TRW trips into this, they'll come 
#' out looking cheaper to monitor than you'd expect. 

# Use the actual durations for observed trips.
ais_days
# For unobserved trips, apply the trip duration model to get the predicted durations, averaging across gear types.
td_mod <- lm(AIS_DAYS ~ RAW * GEAR, data = match.4)
