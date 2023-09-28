# Rune lines 1-100 of analyses/draft_rates.R (through 'Other Parameters')

# Redefine fixed-gear strata
fixed.pc_effort <- unique(copy(pc_effort_dt)[STRATA %like% "HAL|POT", STRATA := paste0(POOL, "_", "FIXED")])
fixed.val <- fixed.pc_effort[ADP %in% 2018:2022]

#======================================================================================================================#
# Define Boxes using different week() functions ---- 
#======================================================================================================================#

# Define boxes again split by FMP, but make neighboring gear-specific for the fixed-gear strata

# Using data.table and WITHOUT lubridate
# Unload lubridate if it isn't already
detach("package:lubridate", unload = TRUE)
# Define boxes using data.table::week()
system.time(fixed_fmp.box_data.table <- define_boxes_3(
  fixed.val, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), dmn_cols = c("GEAR"), geom = T ))

# Define boxes using lubridate::week()
library(lubridate)
system.time(fixed_fmp.box_lubridate <- define_boxes_3(
  fixed.val, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), dmn_cols = c("GEAR"), geom = T ))

# Load Phil's objects (this is just to make sure I can replicate what Phil got, which was confirmed)
load("~/Geoff/RData.rdata")   

# Compare box definitions
fsetequal(fixed_fmp.box_data.table$og_data, fixed_fmp.box$og_data)      # Yup, matched Phil's using data.table::week()
fsetdiff(fixed_fmp.box_lubridate$og_data[, -"BOX_ID"], fixed_fmp.box$og_data[, -"BOX_ID"])    # About 10% records different

#======================================================================================================================#
# Compare week() function behavior ----
#======================================================================================================================#

data.table::week   #  yday(x) %/% 7L + 1L
lubridate::week    # (yday(x) - 1)%/%7 + 1
# lubridate subtracts 1 to make sure the first week (day 1:7) has 7 days. Without it week 1 only has 6 days!

week_test <- data.table(
  dt.week =       data.table::week(fixed.val$TRIP_TARGET_DATE),
  lubr.week =     lubridate::week(fixed.val$TRIP_TARGET_DATE),
  dt.isoweek =    data.table::isoweek(fixed.val$TRIP_TARGET_DATE),
  lubr.isoweek =  lubridate::isoweek(fixed.val$TRIP_TARGET_DATE))

week_test[dt.week != lubr.week]        # a lot of these differ
week_test[dt.isoweek != lubr.isoweek]  # isoweeks are the same
week_test[dt.week != dt.isoweek]
week_test[lubr.week != lubr.isoweek]

dates_2018 <- as.Date(as.Date("2018-01-01") : as.Date("2018-12-31"))
table(sapply(dates_2018, data.table::week))
table(sapply(dates_2018, lubridate::week))
table(sapply(dates_2018, data.table::isoweek))
table(sapply(dates_2018, lubridate::isoweek))

dates_2019 <- as.Date(as.Date("2019-01-01") : as.Date("2019-12-31"))
table(sapply(dates_2019, data.table::week))
table(sapply(dates_2019, lubridate::week))
table(sapply(dates_2019, data.table::isoweek))
table(sapply(dates_2019, lubridate::isoweek))

dates_2020 <- as.Date(as.Date("2020-01-01") : as.Date("2020-12-31"))
table(sapply(dates_2020, data.table::week))
table(sapply(dates_2020, lubridate::week))
table(sapply(dates_2020, data.table::isoweek))
table(sapply(dates_2020, lubridate::isoweek))