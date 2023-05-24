library(data.table)         # Data wrangling
library(ggplot2)            # Plotting
library(odbc)

# This is a preliminary version of work data with 2022. It is an outdated version of the output of 
# analyses/allocation_evaluation/data_prep. Find it on the google drive's source data folder.
# https://drive.google.com/file/d/1aMSthvWlBNqpu--T1Hm_RCn1C4PWTcze/view?usp=share_link
load("source_data/allocation_evaluation_data_prep.Rdata")  # Loads 'work.data' and 'efrt' objects

effort_pc <- unique(work.data[CVG_NEW == "PARTIAL"][, .(ADP, STRATA, STRATA_NEW, TRIP_ID, FMP, AGENCY_GEAR_CODE, OBSERVED_FLAG)])
effort_pc[STRATA == "EM_TRW_EFP", STRATA_NEW := "EM_TRW_EFP"]

#===========================================================#
# Trips that fished multiple gear types by year and stratum #
#===========================================================#

effort_pc_gear <- unique(effort_pc[, .(ADP, STRATA_NEW, TRIP_ID, AGENCY_GEAR_CODE)])
setorder(effort_pc_gear, ADP, STRATA_NEW, AGENCY_GEAR_CODE)
effort_pc_gear <- effort_pc_gear[, .(GEARS = paste(unique(AGENCY_GEAR_CODE), collapse = ",")), by = .(ADP, STRATA_NEW, TRIP_ID)]
effort_pc_gear_n <- effort_pc_gear[, .(N = uniqueN(TRIP_ID)), by = .(ADP, STRATA_NEW, GEARS)]
effort_pc_gear_n[, STRATUM_N := sum(N), by = .(ADP, STRATA_NEW)]
effort_pc_gear_n[, PERC := round(N / STRATUM_N * 100, 2)]

# Counts
dcast(effort_pc_gear_n, STRATA_NEW + GEARS ~ ADP, value.var = "N", fill = 0)
ggplot(effort_pc_gear, aes(x = ADP, fill = GEARS)) + geom_bar() + facet_wrap(~STRATA_NEW, scale = "free_y")

# Percents
dcast(effort_pc_gear_n, STRATA_NEW + GEARS ~ ADP, value.var = "PERC", fill = 0)
ggplot(effort_pc_gear, aes(x = ADP, fill = GEARS)) + geom_bar(position = "fill") + facet_wrap(~STRATA_NEW)

# But is there an observer-effect on whether they fish multiple gear types?
effort_pc_gear_mon <- unique(effort_pc[, .(ADP, STRATA_NEW, TRIP_ID, AGENCY_GEAR_CODE, OBSERVED_FLAG)])
setorder(effort_pc_gear_mon, ADP, STRATA_NEW, AGENCY_GEAR_CODE)
effort_pc_gear_mon <- effort_pc_gear_mon[, .(GEARS = paste(unique(AGENCY_GEAR_CODE), collapse = ",")), by = .(ADP, STRATA_NEW, TRIP_ID, OBSERVED_FLAG)]
effort_pc_gear_mon_n <- effort_pc_gear_mon[, .(N = uniqueN(TRIP_ID)), keyby = .(ADP, STRATA_NEW, GEARS, OBSERVED_FLAG)]
effort_pc_gear_mon_n[, STRATUM_N := sum(N), by = .(ADP, STRATA_NEW)]
effort_pc_gear_mon_n[, STRATUM_PERC := round(N / STRATUM_N * 100, 2)]
effort_pc_gear_mon_n[, GEAR_MON_PERC := round(N / sum(N) * 100, 2), by = .(ADP, STRATA_NEW, GEARS)]

dcast(
  effort_pc_gear_mon_n[STRATA_NEW != "ZERO"], 
  ADP + STRATA_NEW + GEARS ~ OBSERVED_FLAG, value.var = "GEAR_MON_PERC", fill = 0
)[ADP == 2022]
# At least in 2022, it looks like monitored FG-EM trips tend to fish multiple gear types 25% of the time, but unomnitored
# trips have multi-gear only 12-14% of the time (half as often!?)
# Within OB_HAL, there is no difference in multi-gear between monitored and unmonitored trips
# Within OB_POT, observed trips have a less proportion of multi-gear trips.

#=====================================================#
# Trips that fished multiple FMPs by year and stratum #
#=====================================================#

effort_pc_fmp <- unique(effort_pc[, .(ADP, STRATA_NEW, TRIP_ID, FMP)])
setorder(effort_pc_fmp, ADP, STRATA_NEW, FMP)
effort_pc_fmp <- effort_pc_fmp[, .(FMPs = paste(unique(FMP), collapse = ",")), by = .(ADP, STRATA_NEW, TRIP_ID)]
effort_pc_fmp_n <- effort_pc_fmp[, .(N = uniqueN(TRIP_ID)), by = .(ADP, STRATA_NEW, FMPs)]
effort_pc_fmp_n[, STRATUM_N := sum(N), by = .(ADP, STRATA_NEW)]
effort_pc_fmp_n[, PERC := round(N / STRATUM_N * 100, 2)]

# Counts
dcast(effort_pc_fmp_n, STRATA_NEW + FMPs ~ ADP, value.var = "N", fill = 0)
ggplot(effort_pc_fmp, aes(x = ADP, fill = FMPs)) + geom_bar() + facet_wrap(~STRATA_NEW, scale = "free_y")

# Percent
dcast(effort_pc_fmp_n, STRATA_NEW + FMPs ~ ADP, value.var = "PERC", fill = 0)
ggplot(effort_pc_fmp, aes(x = ADP, fill = FMPs)) + geom_bar(position = "fill") + facet_wrap(~STRATA_NEW)

#=====================#
# Prior FG-EM with OB #
#=====================#

channel <- dbConnect(odbc::odbc(),"AFSC",
                     UID    = rstudioapi::askForPassword("Database user"),
                     PWD    = rstudioapi::askForPassword("Database password"))

og_wd <- copy(work.data)
# Load the data used for the 2023 Final ADP, specifically for fg_em 
# https://drive.google.com/file/d/1GeBZgGaBvZPl3T-tVbTvpDZ5aF9I-CS7/view?usp=share_link
load("source_data/2023_Final_ADP_data.rdata")
work.data <- copy(og_wd)

fg_em # This list has all 179 vessels current in the FG_EM pool

# Pull from NORPAC to get vessel lengths and whether an observer ever took a trip on these vessels.

fg_em_permit_string <- paste(fg_em$PERMIT, collapse = ",")
fg_em_ob_pull <- setDT(dbGetQuery(channel, paste(
  "
  SELECT 
    a.permit, a.name, a.length, 
    b.cruise, b.trip_seq, b.start_date, b.did_fishing_occur_flag
  FROM 
    atl_lov_vessel a
  LEFT JOIN atl_fma_trip b ON a.permit = b.permit
  WHERE a.permit IN (", fg_em_permit_string, ") 
  "
)))

fg_em_ob_pull[, YEAR := year(START_DATE)]
fg_em_ob_pull_count <- fg_em_ob_pull[, .(TRIP_N = uniqueN(TRIP_SEQ[DID_FISHING_OCCUR_FLAG == "Y"], na.rm = T)), keyby = .(PERMIT, LENGTH, YEAR)]
ggplot(fg_em_ob_pull_count, aes(x = as.character(YEAR), y = as.character(PERMIT), fill = TRIP_N, label = TRIP_N)) + 
  geom_tile() + scale_fill_viridis_c(trans = "sqrt") + 
  labs(x = "Year", y = "Permit", subtitle = "Vessels with filled 'NA' column have no observer records since 2008.") + theme(legend.position = "bottom")

fg_em_ob_pull_count_wide <- dcast(fg_em_ob_pull_count, PERMIT + LENGTH ~ YEAR, value.var = "TRIP_N", fill = 0)
fg_em_ob_pull_count_wide[, TOT := rowSums(fg_em_ob_pull_count_wide[, -c("PERMIT", "LENGTH")])]
fg_em_ob_pull_count_wide

fg_em_ob_pull_count[is.na(YEAR), ]  # only 38 of the 179 vessels (21.23%) in FG_EM have never had an observer on board.
fg_em_ob_pull_tot <- fg_em_ob_pull[, .(TOT_N = uniqueN(TRIP_SEQ, na.rm = T)), by = .(PERMIT, LENGTH)]

count_3 <- round(fg_em_ob_pull_tot[TOT_N >= 3, .N] / nrow(fg_em) * 100, 2)  # 60.89% of the vessels have taken an observer on 3 or more trips
count_5 <- round(fg_em_ob_pull_tot[TOT_N >= 5, .N] / nrow(fg_em) * 100, 2)  # 50.84% of the vessels have taken an observer on 5 or more trips!
ggplot(fg_em_ob_pull_tot, aes(x = TOT_N, fill = as.factor(LENGTH))) + geom_histogram(bins = 30) + 
  labs(
    x = "Total trips with an observer", y = "Count of fixed-gear EM vessels", fill = "Vessel length",
    subtitle = paste0(count_3, "% vessels with >=3 trips and ", count_5, "% vessels with >= 5 trips.")) + 
  scale_fill_viridis_d(breaks = seq(40, 140, 10))

save(
  effort_pc_gear, effort_pc_gear_n, effort_pc_fmp, effort_pc_fmp_n, fg_em, 
  fg_em_ob_pull_count, fg_em_ob_pull_count_wide, fg_em_ob_pull_tot, count_3, count_5, 
  file = "analyses/stratification/stratum_issues.Rdata")
