# get packages if they aren't already loaded
if(!require("data.table"))   install.packages("data.table", repos='http://cran.us.r-project.org')
if(!require("ggplot2"))   install.packages("ggplot2", repos='http://cran.us.r-project.org')

# avoid scientific notation
options(scipen = 9999)

# user inputs
ADPyear  <- 2024

# get data object produced by get_data.R
load(paste0("source_data/", ADPyear, "_Final_ADP_data.rdata"))
# https://drive.google.com/file/d/1xH-P54wW3vPXtaQmgEDxb3YgHh-yJlp8/view?usp=drive_link

# get list of trawl EM EFP vessels
efp_list <- fread("source_data/efp_list_2023-09-05.csv")
# https://drive.google.com/file/d/1eSSTal-w_y319xF67FRSdI23rv9BLCtn/view?usp=drive_link

# select necessary columns
effort.strata <- work.data[CVG_NEW == "PARTIAL" & AGENCY_GEAR_CODE != "JIG", .(ADP, STRATA = STRATA_NEW, TRIP_TARGET_DATE, TRIP_ID)]

# ensure one trip target date per trip, making it the minimum trip target date
effort.strata[, TRIP_TARGET_DATE := min(TRIP_TARGET_DATE), by = TRIP_ID]

# select only distinct rows
effort.strata <- unique(effort.strata)

# order the data
setorder(effort.strata, ADP, STRATA, TRIP_TARGET_DATE)

# split trips if they occurred in more than one stratum
effort.strata[ , TRIPS := 1/.N, by = TRIP_ID]

# find julian dates
effort.strata[, JULIAN_DATE := yday(TRIP_TARGET_DATE)]

# set julian date to 1 for trips that left in year adp - 1
effort.strata[, JULIAN_DATE := ifelse(year(TRIP_TARGET_DATE) < ADP, 1, JULIAN_DATE)]

# set julian date to 366 for trips that left in year adp + 1
effort.strata[, JULIAN_DATE := ifelse(year(TRIP_TARGET_DATE) > ADP, 366, JULIAN_DATE)]

# isolate the latest date for which we have data in the most recent year of valhalla
max.date <- max(effort.strata[ADP == ADPyear - 1, JULIAN_DATE])

# count trips through October and December by year and stratum
effort.strata <- effort.strata[, .(THRU_OCT_TRIPS = sum(TRIPS[JULIAN_DATE <= max.date]), TOTAL_TRIPS = sum(TRIPS)), by = .(ADP, STRATA)]

# model total trips against year, stratum, and trips through October
effort.mod <- lm(TOTAL_TRIPS ~ ADP * STRATA * THRU_OCT_TRIPS, data = effort.strata[ADP < ADPyear - 1])

# predict ADPyear - 1 effort
new.data <- effort.strata[ADP == ADPyear - 1]
effort.strata[ADP == ADPyear - 1, TOTAL_TRIPS := predict(effort.mod, new.data)]

# plot trips through December against trips through October by stratum
p1 <- ggplot(effort.strata[ADP < ADPyear - 1], aes(x = THRU_OCT_TRIPS, y = TOTAL_TRIPS)) +
      facet_wrap(STRATA ~ ., scales = "free") +
      geom_point() +
      geom_point(data = effort.strata[ADP == ADPyear - 1], color = "red") +
      labs(x = "Trips through October", y = "Trips through December") +
      theme_bw()

# plot trips through December against year by stratum
p2 <- ggplot(effort.strata[ADP < ADPyear - 1], aes(x = ADP, y = TOTAL_TRIPS)) +
      facet_wrap(STRATA ~ ., scales = "free") +
      geom_point() +
      geom_point(data = effort.strata[ADP == ADPyear - 1], color = "red") +
      geom_line(data = effort.strata) +
      scale_x_continuous(breaks = min(effort.strata$ADP):ADPyear - 1) +
      expand_limits(y = 0) +
      labs(x = "Year", y = "Trips through December") +
      theme_bw()

# sum trips by year
effort.year <- effort.strata[, .(TOTAL_TRIPS = sum(TOTAL_TRIPS)), by = ADP]

# plot trips through December against year
p3 <- ggplot(effort.year, aes(x = ADP, y = TOTAL_TRIPS)) +
      geom_point() +
      geom_point(data = effort.year[ADP == ADPyear - 1], color = "red") +
      scale_x_continuous(breaks = min(effort.strata$ADP):ADPyear - 1) +
      expand_limits(y = 0) +
      labs(x = "Year", y = "Trips through December") +
      theme_bw()

# model total trips against year and stratum
effort.mod <- lm(TOTAL_TRIPS ~ ADP * STRATA, data = effort.strata)

# predict ADPyear effort with 95% confidence interval  
new.data <- CJ(ADP = min(effort.strata$ADP):ADPyear, STRATA = unique(effort.strata$STRATA))
effort.pred.strata <- copy(new.data)[, fit := predict(effort.mod, new.data)]
effort.pred.strata[, se := predict(effort.mod, new.data, se.fit = TRUE)$se.fit]
effort.pred.strata[, var := se^2]
effort.pred.year <- effort.pred.strata[, .(fit = sum(fit), var = sum(var)), by = .(ADP)]
effort.pred.year[, ':=' (lwr = fit - (2 * sqrt(var)), upr = fit + (2 * sqrt(var)))]

# plot data, prediction, and prediction interval
p4 <- ggplot(effort.year[ADP < ADPyear - 1], aes(x = ADP, y = TOTAL_TRIPS)) +
      geom_point() +
      geom_point(data = effort.year[ADP == ADPyear - 1], color = "red") +
      geom_point(aes(x = ADP, y = fit), data = effort.pred.year[ADP == ADPyear], color = "red") +
      geom_errorbar(aes(ymin = lwr, ymax = upr), data = effort.pred.year[ADP == ADPyear, .(ADP, TOTAL_TRIPS = fit, lwr, upr)], color = "red", width = 0.2) +
      geom_line(aes(x = ADP, y = fit), data = effort.pred.year, color = "red") +
      scale_x_continuous(breaks = min(effort.year$ADP):ADPyear) +
      expand_limits(y = 0) +
      labs(x = "Year", y = "Partial Coverage Trips") +
      theme_classic()

# png("output_figures/TripsPerYear.png", width = 7, height = 5, units = 'in', res=300)
# p4
# dev.off()

# calculate effort predictions for ADPyear with 95% confidence interval
to_draw <- effort.pred.strata[ADP == ADPyear, .(ADP, STRATA, TRIPS = round(fit), lwr = round(fit - 2 * se), upr = round(fit + 2 * se))]

# create full year of effort to draw trips from
# use January - October of ADPyear - 1 and November - December of ADPyear - 2
draw_from <- rbind(efrt[ADP == ADPyear - 1], efrt[ADP == ADPyear - 2 & yday(START) > max.date])

# relabel year to ADPyear
draw_from[, ADP := ADPyear]

# relabel zero coverage trips temporarily in order to facilitate the merge with to_draw and the prediction of effort
draw_from[POOL == "ZE", STRATA := "ZERO"]

# match TRIP_TARGET_CODE to to_draw
draw_from[, TRIP_TARGET_CODE := fcase(
  !(TARGET %in% c("P", "B", "C", "I", "S")), "Other",
  TARGET %in% c("P", "B"), "Pollock",
  TARGET == "C", "Pacific Cod",
  TARGET == "I", "Halibut",
  TARGET == "S", "Sablefish",
  default = "Other")]

# add probability of trawl EM boats not taking an observer (efp_prob; based on data from vessels that have been in the program prior to ADPyear)
# 2022-05-31 is the last day of the 2022 spring fisheries, and 2023-09-01 is the first day of the 2023 fall fisheries
# by including trips after the 2022 spring fisheries have closed and before the 2023 fall fisheries have opened,
# we calculate efp_prob based on the fall 2022 and spring 2023 seasons (the two most recent full seasons, 
# as the fall 2023 fishery was not finished by the time Valhalla was compiled)  
efp_prob <- unique(work.data[
  TRIP_TARGET_DATE > as.Date("2022-05-31") & TRIP_TARGET_DATE < as.Date("2023-09-01") & 
    VESSEL_ID %in% efp_list$PERMIT[efp_list$YEAR_ADDED < ADPyear] &
    TRIP_TARGET_CODE %in% c("P", "B") & AGENCY_GEAR_CODE %in% c("NPT", "PTR"),
  .(TRIP_ID, COVERAGE_TYPE, STRATA, AGENCY_GEAR_CODE)
])[, .SD[all(AGENCY_GEAR_CODE == "PTR")], by = .(TRIP_ID)
][, .N, by = .(COVERAGE_TYPE, STRATA)
][, .(STRATA, N, EFP_PROB = N / sum(N)), by = .(COVERAGE_TYPE)
][STRATA == "EM_TRW_EFP"]
draw_from[
  PERMIT %in% efp_list$PERMIT & TRIP_TARGET_CODE == "Pollock" & AGENCY_GEAR_CODE == "PTR",
  EFP_PROB := efp_prob[COVERAGE_TYPE == "PARTIAL", EFP_PROB]]
efrt[
  PERMIT %in% efp_list$PERMIT & TARGET %in% c("P", "B") & AGENCY_GEAR_CODE =="PTR", 
  EFP_PROB := efp_prob[COVERAGE_TYPE == "PARTIAL", EFP_PROB]]              

# compare the number of trips to draw (to_draw) to the number of trips available to be drawn (draw_from) 
comp_draw <- merge(
  to_draw[, .(ADP, FMP, TRIP_TARGET_CODE, STRATA, TO_DRAW = C_TRIPS)],
  draw_from[, .(DRAW_FROM = uniqueN(TRIP_ID)), by = .(FMP, TRIP_TARGET_CODE, STRATA)],
  on = .(FMP, TRIP_TARGET_CODE, STRATA), all = TRUE)

# check that we're not trying to draw trips for which we don't have recent data
to_draw_domains   <- unique(to_draw[C_TRIPS > 0, .(DOMAINS = paste(FMP, TRIP_TARGET_CODE, STRATA, sep = " "))])
draw_from_domains <- unique(draw_from[, .(DOMAINS = paste(FMP, TRIP_TARGET_CODE, STRATA, sep = " "))])
if(nrow(to_draw_domains[!(DOMAINS %in% unique(draw_from_domains$DOMAINS))]) != 0) {
  warning("We expect effort in domains for which we don't have recent data")}

# merge the predicted number of trips for each domain onto draw_from
efrt_adpyear <- draw_from[to_draw, on = .(ADP, FMP, TRIP_TARGET_CODE, STRATA)]

# rename cumulative trips column to be clear that this is the predicted number of trips to occur
setnames(efrt_adpyear, "C_TRIPS", "TRIPS_PRED")

# save effort predictions (to_draw) and the population of trips to sample from (draw_from)
save(list = c("efrt", "efrt_adpyear", "gvf"), file = "source_data/effort_prediction.rdata")