# get packages if they aren't already loaded
if(!require("data.table"))   install.packages("data.table", repos='http://cran.us.r-project.org')
if(!require("ggplot2"))   install.packages("ggplot2", repos='http://cran.us.r-project.org')

# user inputs
ADPyear  <- 2024

# get data object produced by get_data.R
load(paste0("source_data/", ADPyear, "_Final_ADP_data.rdata"))
# https://drive.google.com/file/d/1xH-P54wW3vPXtaQmgEDxb3YgHh-yJlp8/view?usp=drive_link

# get list of trawl EM EFP vessels
efp_list <- fread("source_data/efp_list_2023-09-05.csv")
# https://drive.google.com/file/d/1eSSTal-w_y319xF67FRSdI23rv9BLCtn/view?usp=drive_link

# calculate cumulative effort by stratum and species using valhalla from the past four years
cumulative.trips.target <- copy(work.data)[ADP >= ADPyear - 4 & CVG_NEW == "PARTIAL" & AGENCY_GEAR_CODE != "JIG"]

# simplify species and fmp labels
cumulative.trips.target[
][, TRIP_TARGET_CODE := fcase(
  !(TRIP_TARGET_CODE %in% c("P", "B", "C", "I", "S")), "Other",
  TRIP_TARGET_CODE %in% c("P", "B"), "Pollock",
  TRIP_TARGET_CODE == "C", "Pacific Cod",
  TRIP_TARGET_CODE == "I", "Halibut",
  TRIP_TARGET_CODE == "S", "Sablefish")
][, FMP := ifelse(FMP %in% c("BS", "AI"), "BSAI", FMP)]

# select necessary columns
cumulative.trips.target <- cumulative.trips.target[, .(TRIP_ID, FMP, ADP, STRATA_NEW, TRIP_TARGET_DATE, TRIP_TARGET_CODE)]

# rename strata_new as strata
setnames(cumulative.trips.target, "STRATA_NEW", "STRATA")

# ensure one trip target date per trip, making it the minimum trip target date
cumulative.trips.target[, TRIP_TARGET_DATE := min(TRIP_TARGET_DATE), by=TRIP_ID]

# select only distinct rows
cumulative.trips.target <- unique(cumulative.trips.target)

# split trips that occurred in more than one fmp, adp, strata, and/or target
cumulative.trips.target[ , TRIPS := 1/.N, by = TRIP_ID]

# order the data
setorder(cumulative.trips.target, ADP, STRATA, TRIP_TARGET_CODE, TRIP_TARGET_DATE)

# calculate cumulative trips by date for each domain
cumulative.trips.target[, C_TRIPS := cumsum(TRIPS), by = .(ADP, FMP, STRATA, TRIP_TARGET_CODE)]

# reformat date columns
cumulative.trips.target[, JULIAN_DATE := yday(TRIP_TARGET_DATE)]

# set julian date to 1 for trips that left in year adp - 1
cumulative.trips.target[, JULIAN_DATE := ifelse(year(TRIP_TARGET_DATE) < ADP, 1, JULIAN_DATE)]

# set julian date to 366 for trips that left in year adp + 1 (this should be no trips)
cumulative.trips.target[, JULIAN_DATE := ifelse(year(TRIP_TARGET_DATE) > ADP, 366, JULIAN_DATE)]

# isolate the latest date for which we have data in the most recent year of valhalla
max.date <- max(cumulative.trips.target[ADP == ADPyear - 1, JULIAN_DATE])

# plot cumulative trips by year and stratum
# vertical lines signify date cutoff for ADPyear-1 data (red) and end of year (black)

plot_theme_cumulative_trips <- list(
  geom_line(aes(color = as.character(ADP)), linewidth = 1.2), 
  geom_vline(xintercept = max.date, color = "red" ),
  geom_vline(xintercept = 366),
  facet_wrap(FMP + TRIP_TARGET_CODE ~ STRATA, scales = "free"),
  labs(x = "Day of the year", y = "Cumulative trips", color = "Year"),
  theme_bw())


## GOA
p1 <- ggplot(cumulative.trips.target[FMP == "GOA"], aes(JULIAN_DATE, C_TRIPS)) +
      plot_theme_cumulative_trips

## BSAI
p2 <- ggplot(cumulative.trips.target[FMP == "BSAI"], aes(JULIAN_DATE, C_TRIPS)) +
      plot_theme_cumulative_trips

# plot cumulative trips by year and stratum for the portion of (ADPyear - 1) in which we don't have data

## GOA
p3 <- ggplot(cumulative.trips.target[FMP == "GOA"], aes(JULIAN_DATE, C_TRIPS)) +
      plot_theme_cumulative_trips + 
      coord_cartesian(xlim = c(max.date - 20, 366)) 

## BSAI
p4 <- ggplot(cumulative.trips.target[FMP == "BSAI"], aes(JULIAN_DATE, C_TRIPS)) +
      plot_theme_cumulative_trips +
      coord_cartesian(xlim = c(max.date - 20, 366))

# Based on the plots, there are three conditions that stratum/species fisheries find themselves come October:
# 1) Fisheries that increase by a (mostly) consistent ratio between October and December:
# "GOA Halibut EM_HAL",
# "GOA Halibut EM_POT",
# "GOA Halibut HAL",
# "GOA Halibut POT",
# "GOA Halibut ZERO",
# "GOA Pollock EM_TRW_EFP",
# "GOA Pollock TRW",
# "GOA Sablefish EM_HAL",
# "GOA Sablefish EM_POT",
# "GOA Sablefish HAL",
# "GOA Sablefish POT",
# "GOA Sablefish ZERO",
# "BSAI Halibut EM_HAL",
# "BSAI Halibut HAL",
# "BSAI Sablefish POT"

# 2) Fisheries for which we would use a *subset* of years to calculate the ratio:

## use 2021 and 2022 data only for the following fisheries:
# "GOA Pacific Cod EM_POT",
# "GOA Pacific Cod EM_HAL",
# "GOA Pacific Cod HAL",
# "GOA Pacific Cod POT",
# "GOA Pacific Cod ZERO",
# "BSAI Sablefish EM_POT",
# "BSAI Sablefish HAL"

## use 2022 data only for the following fisheries:
# "GOA Pacific Cod TRW"

# 3) Fisheries that don't exist or are finished by October:
# "GOA Other EM_HAL",
# "GOA Other EM_POT",
# "GOA Other HAL",
# "GOA Other TRW",
# "GOA Other ZERO",
# "BSAI Halibut EM_POT",
# "BSAI Halibut POT",
# "BSAI Halibut ZERO",
# "BSAI Other POT",
# "BSAI Pacific Cod EM_HAL",
# "BSAI Pacific Cod EM_POT",
# "BSAI Pacific Cod HAL",
# "BSAI Pacific Cod POT",
# "BSAI Pacific Cod TRW",
# "BSAI Pacific Cod ZERO",
# "BSAI Pollock EM_TRW_EFP",
# "BSAI Sablefish EM_HAL",
# "BSAI Sablefish ZERO"

# calculate the through October trips and the through December trips so that a ratio can later be calculated 
dec.oct.trips <- unique(cumulative.trips.target[JULIAN_DATE<=max.date, .(THRU_OCT_TRIPS=max(C_TRIPS)), by=.(ADP, FMP, TRIP_TARGET_CODE, STRATA)][!is.na(THRU_OCT_TRIPS), .(ADP, FMP, TRIP_TARGET_CODE, STRATA, THRU_OCT_TRIPS)])[
                 unique(cumulative.trips.target[, .(THRU_DEC_TRIPS=max(C_TRIPS)), by=.(ADP, FMP, TRIP_TARGET_CODE, STRATA)][!is.na(THRU_DEC_TRIPS), .(ADP, FMP, TRIP_TARGET_CODE, STRATA, THRU_DEC_TRIPS)]), 
                 on = .(ADP, FMP, STRATA, TRIP_TARGET_CODE)]

# create a column that contains fmp, target, and stratum
dec.oct.trips <- dec.oct.trips[, FMP_TARGET_STRATA:=paste(FMP, TRIP_TARGET_CODE, STRATA, sep = " ")][, .(ADP, FMP, TRIP_TARGET_CODE, STRATA, FMP_TARGET_STRATA, THRU_OCT_TRIPS, THRU_DEC_TRIPS)]

# find the dec/oct ratio for each group of fmp, target, stratum combinations
dec.oct.ratio <- rbind(
                 # group 1 : calculate the december/october trip ratio according to the groups above
                 dec.oct.trips[ADP<ADPyear-1 & # because ADPyear-1 only has data through october
                               FMP_TARGET_STRATA %in% c("GOA Halibut EM_HAL",
                                                        "GOA Halibut EM_POT",
                                                        "GOA Halibut HAL",
                                                        "GOA Halibut POT",
                                                        "GOA Halibut ZERO",
                                                        "GOA Pollock EM_TRW_EFP",
                                                        "GOA Pollock TRW",
                                                        "GOA Sablefish EM_HAL",
                                                        "GOA Sablefish EM_POT",
                                                        "GOA Sablefish HAL",
                                                        "GOA Sablefish POT",
                                                        "GOA Sablefish ZERO",
                                                        "BSAI Halibut EM_HAL",
                                                        "BSAI Halibut HAL",
                                                        "BSAI Sablefish POT"),
                 .(RATIO=mean(THRU_DEC_TRIPS/THRU_OCT_TRIPS)), by = .(FMP_TARGET_STRATA)],
                 # group 2: ratios from a subset of years
                 dec.oct.trips[ADP %in% c(ADPyear-3, ADPyear-2) &
                                  FMP_TARGET_STRATA %in% c("GOA Pacific Cod EM_POT",
                                                           "GOA Pacific Cod EM_HAL",
                                                           "GOA Pacific Cod HAL",
                                                           "GOA Pacific Cod POT",
                                                           "GOA Pacific Cod ZERO",
                                                           "BSAI Sablefish EM_POT",
                                                           "BSAI Sablefish HAL"),
                               .(RATIO=mean(THRU_DEC_TRIPS/THRU_OCT_TRIPS)), by = .(FMP_TARGET_STRATA)],
                 dec.oct.trips[ADP %in% c(ADPyear-2) &
                                 FMP_TARGET_STRATA %in% c("GOA Pacific Cod TRW"),
                               .(RATIO=mean(THRU_DEC_TRIPS/THRU_OCT_TRIPS)), by = .(FMP_TARGET_STRATA)],
                 # group 3: overwrite the mean ratios with 1 for domains that are done by october
                 dec.oct.trips[FMP_TARGET_STRATA %in% c("GOA Other EM_HAL",
                                                        "GOA Other EM_POT",
                                                        "GOA Other HAL",
                                                        "GOA Other TRW",
                                                        "GOA Other ZERO",
                                                        "BSAI Halibut EM_POT",
                                                        "BSAI Halibut POT",
                                                        "BSAI Halibut ZERO",
                                                        "BSAI Other POT",
                                                        "BSAI Pacific Cod EM_HAL",
                                                        "BSAI Pacific Cod EM_POT",
                                                        "BSAI Pacific Cod HAL",
                                                        "BSAI Pacific Cod POT",
                                                        "BSAI Pacific Cod TRW",
                                                        "BSAI Pacific Cod ZERO",
                                                        "BSAI Pollock EM_TRW_EFP",
                                                        "BSAI Sablefish EM_HAL",
                                                        "BSAI Sablefish ZERO"),
                  .(RATIO=1), by = .(FMP_TARGET_STRATA)])

# check that the number of fmp, target, stratum combinations match
if(length(unique(dec.oct.trips$FMP_TARGET_STRATA)) != length(unique(dec.oct.ratio$FMP_TARGET_STRATA))){stop("Domains don't match between trips and ratios.")}

# add fmp_target_strata to cumulative.trips.target and rearrange columns
cumulative.trips.target <- cumulative.trips.target[, .(ADP, TRIP_ID, FMP, TRIP_TARGET_CODE, STRATA, FMP_TARGET_STRATA=paste(FMP, TRIP_TARGET_CODE, STRATA, sep = " "), TRIP_TARGET_DATE, JULIAN_DATE, TRIPS, C_TRIPS)]

# add predicted ADPyear effort to cumulative.trips.target as just the projection of ADPyear-1 effort
cumulative.trips.target <- rbind(
                           rbind(
                           # all cumulative trips available in the data
                           cumulative.trips.target,
                           # maximum cumulative trips for years prior to the most recent year, extended to the end of the year for graphing purposes
                           cumulative.trips.target[ADP<ADPyear-1, .(JULIAN_DATE=max(JULIAN_DATE), C_TRIPS=max(C_TRIPS)), by=.(ADP, FMP, TRIP_TARGET_CODE, STRATA)][, JULIAN_DATE:=366], fill=TRUE),
                           rbind(
                           # maximum cumulative trips for the most recent year (starting point for ADPyear projections)
                           cumulative.trips.target[ADP==ADPyear-1][, .(JULIAN_DATE=max(JULIAN_DATE), C_TRIPS=max(C_TRIPS)), by=.(ADP, FMP, TRIP_TARGET_CODE, STRATA)],
                           # maximum cumulative trips for the most recent year, projected to year's end using december/october effort ratios (end point for ADPyear projections)
                           dec.oct.ratio[cumulative.trips.target[ADP==ADPyear-1], on = "FMP_TARGET_STRATA"][, .(JULIAN_DATE=366, C_TRIPS=max(C_TRIPS*RATIO)), by=.(ADP, FMP, TRIP_TARGET_CODE, STRATA)])[, ADP:=ADPyear], fill = TRUE)

# plot cumulative trips by year and stratum for EM_HAL
# vertical lines signify date cutoff for ADPyear-1 data (red) and end of year (black)
p5 <- ggplot(cumulative.trips.target[STRATA=="EM_HAL" & C_TRIPS>1, ], aes(JULIAN_DATE, C_TRIPS)) + # for confidentiality, exclude domains and portions of the year that only have 1 cumulative trip
      geom_line(aes(color=as.character(ADP)), linewidth=1.5) + 
      geom_vline(xintercept = max.date, color="red") +
      geom_vline(xintercept = 366) +
      facet_wrap(FMP+TRIP_TARGET_CODE~STRATA, scales = "free") +
      labs(x = "Day of the year", y = "Cumulative trips", color = "Year") + 
      theme_bw()

# plot cumulative trips by year and stratum for EM_POT
# vertical lines signify date cutoff for ADPyear-1 data (red) and end of year (black)
p6 <- ggplot(cumulative.trips.target[STRATA=="EM_POT" & C_TRIPS>1, ], aes(JULIAN_DATE, C_TRIPS)) + # for confidentiality, exclude domains and portions of the year that only have 1 cumulative trip
      geom_line(aes(color=as.character(ADP)), linewidth=1.5) + 
      geom_vline(xintercept = max.date, color="red") +
      geom_vline(xintercept = 366) +
      facet_wrap(FMP+TRIP_TARGET_CODE~STRATA, scales = "free") +
      labs(x = "Day of the year", y = "Cumulative trips", color = "Year") + 
      theme_bw()

# plot cumulative trips by year and stratum for HAL
# vertical lines signify date cutoff for ADPyear-1 data (red) and end of year (black)
p7 <- ggplot(cumulative.trips.target[STRATA=="HAL" & C_TRIPS>1, ], aes(JULIAN_DATE, C_TRIPS)) + # for confidentiality, exclude domains and portions of the year that only have 1 cumulative trip
      geom_line(aes(color=as.character(ADP)), linewidth=1.5) + 
      geom_vline(xintercept = max.date, color="red") +
      geom_vline(xintercept = 366) +
      facet_wrap(FMP+TRIP_TARGET_CODE~STRATA, scales = "free") +
      labs(x = "Day of the year", y = "Cumulative trips", color = "Year") + 
      theme_bw()

# plot cumulative trips by year and stratum for POT
# vertical lines signify date cutoff for ADPyear-1 data (red) and end of year (black)
p8 <- ggplot(cumulative.trips.target[STRATA=="POT" & C_TRIPS>1, ], aes(JULIAN_DATE, C_TRIPS)) + # for confidentiality, exclude domains and portions of the year that only have 1 cumulative trip
      geom_line(aes(color=as.character(ADP)), linewidth=1.5) + 
      geom_vline(xintercept = max.date, color="red") +
      geom_vline(xintercept = 366) +
      facet_wrap(FMP+TRIP_TARGET_CODE~STRATA, scales = "free") +
      labs(x = "Day of the year", y = "Cumulative trips", color = "Year") + 
      theme_bw()

# plot cumulative trips by year and stratum for TRW
# vertical lines signify date cutoff for ADPyear-1 data (red) and end of year (black)
p9 <- ggplot(cumulative.trips.target[STRATA=="TRW" & C_TRIPS>1, ], aes(JULIAN_DATE, C_TRIPS)) + # for confidentiality, exclude domains and portions of the year that only have 1 cumulative trip
      geom_line(aes(color=as.character(ADP)), linewidth=1.5) + 
      geom_vline(xintercept = max.date, color="red") +
      geom_vline(xintercept = 366) +
      facet_wrap(FMP+TRIP_TARGET_CODE~STRATA, scales = "free") +
      labs(x = "Day of the year", y = "Cumulative trips", color = "Year") + 
      theme_bw()

# plot cumulative trips by year and stratum for ZERO
# vertical lines signify date cutoff for ADPyear-1 data (red) and end of year (black)
p10 <- ggplot(cumulative.trips.target[STRATA=="ZERO" & C_TRIPS>1, ], aes(JULIAN_DATE, C_TRIPS)) + # for confidentiality, exclude domains and portions of the year that only have 1 cumulative trip
       geom_line(aes(color=as.character(ADP)), linewidth=1.5) + 
       geom_vline(xintercept = max.date, color="red") +
       geom_vline(xintercept = 366) +
       facet_wrap(FMP+TRIP_TARGET_CODE~STRATA, scales = "free") +
       labs(x = "Day of the year", y = "Cumulative trips", color = "Year") + 
       theme_bw()

# count total trips by domain and year
total.trips.target <- cumulative.trips.target[, .(TOTAL_TRIPS = max(C_TRIPS)), by = .(ADP, FMP, TRIP_TARGET_CODE, STRATA)]
total.trips.year   <- total.trips.target[, .(TOTAL_TRIPS = sum(TOTAL_TRIPS)), by = .(ADP)]

# since ADPyear effort is actually a projection of what we think ADPyear - 1 effort will do, make the two equal
total.trips.year[ADP == ADPyear - 1, TOTAL_TRIPS := total.trips.year[ADP == ADPyear, TOTAL_TRIPS]]

# add any Guess Variation Factor (GVF) to ADPyear effort
gvf <- c(-0.05, 0.05)
total.trips.year[ADP == ADPyear, ':=' (MIN = (1 + min(gvf)) * TOTAL_TRIPS, MAX = (1 + max(gvf)) * TOTAL_TRIPS)]

p11 <- ggplot(total.trips.year, aes(x = ADP, y = TOTAL_TRIPS)) +
       geom_errorbar(aes(ymin = MIN, ymax = MAX), color = "red", width = 0.2) +
       annotate("text", x = ADPyear-0.15, y = mean(as.vector(unlist(total.trips.year[ADP==ADPyear, .(TOTAL_TRIPS, MAX)]))), label = "GVF") +
       geom_point() +
       geom_line() +
       geom_point(data = total.trips.year[ADP >= ADPyear - 1], color = "red") +
       scale_x_continuous(limits = c(ADPyear-4, ADPyear+0.25)) +
       expand_limits(y = 0) +
       labs(x = "Year", y = "Partial Coverage Trips") +
       theme_classic()

# png("output_figures/TripsPerYearGVF.png", width = 7, height = 5, units = 'in', res=300)
# p11
# dev.off()

# calculate effort predictions for ADPyear, which are equal to the projected effort for ADPyear-1
to_draw <- cumulative.trips.target[ADP==ADPyear, .(C_TRIPS=round(max(C_TRIPS))), by=.(ADP, FMP, TRIP_TARGET_CODE, STRATA)][order(FMP, TRIP_TARGET_CODE, STRATA)]

# create full year of effort to draw trips from
# use January - October of ADPyear - 1 and November - December of ADPyear - 2
draw_from <- rbind(efrt[ADP==ADPyear-1], efrt[ADP==ADPyear-2 & yday(START) > max.date])

# relabel year to ADPyear
draw_from[, ADP := ADPyear]

# relabel zero coverage trips temporarily in order to facilitate the merge with to_draw and the prediction of effort
draw_from[POOL == "ZE", STRATA := "ZERO"]

# match TRIP_TARGET_CODE to to_draw
draw_from[, TRIP_TARGET_CODE := ifelse(TARGET == "S", "Sablefish", NA)
          ][, TRIP_TARGET_CODE := ifelse(TARGET == "C", "Pacific Cod", TRIP_TARGET_CODE)
            ][, TRIP_TARGET_CODE := ifelse(TARGET == "I", "Halibut", TRIP_TARGET_CODE)
              ][, TRIP_TARGET_CODE := ifelse(TARGET %in% c("P", "B"), "Pollock", TRIP_TARGET_CODE)
                ][, TRIP_TARGET_CODE := ifelse(is.na(TRIP_TARGET_CODE), "Other", TRIP_TARGET_CODE)] 

# add probability of trawl EM boats not taking an observer (efp_prob; based on data from vessels that have been in the program prior to ADPyear)
# 2022-05-31 is the last day of the 2022 spring fisheries, and 2023-09-01 is the first day of the 2023 fall fisheries
# by including trips after the 2022 spring fisheries have closed and before the 2023 fall fisheries have opened,
# we calculate efp_prob based on the fall 2022 and spring 2023 seasons (the two most recent full seasons, 
# as the fall 2023 fishery was not finished by the time Valhalla was compiled)  
efp_prob <- unique(work.data[TRIP_TARGET_DATE > as.Date("2022-05-31") & TRIP_TARGET_DATE < as.Date("2023-09-01") & VESSEL_ID %in% efp_list$PERMIT[efp_list$YEAR_ADDED < ADPyear] & TRIP_TARGET_CODE %in% c("P", "B") & AGENCY_GEAR_CODE %in% c("NPT", "PTR"), .(TRIP_ID, COVERAGE_TYPE, STRATA, AGENCY_GEAR_CODE)])
efp_prob <- efp_prob[, .SD[all(AGENCY_GEAR_CODE == "PTR")], by = .(TRIP_ID)]
efp_prob <- efp_prob[, .N, by = .(COVERAGE_TYPE, STRATA)]
efp_prob <- efp_prob[, .(STRATA, N, EFP_PROB = N / sum(N)), by = .(COVERAGE_TYPE)][STRATA == "EM_TRW_EFP"]
draw_from[PERMIT %in% efp_list$PERMIT & TRIP_TARGET_CODE == "Pollock" & AGENCY_GEAR_CODE == "PTR", EFP_PROB := efp_prob[COVERAGE_TYPE == "PARTIAL", EFP_PROB]]
efrt[PERMIT %in% efp_list$PERMIT & TARGET %in% c("P", "B") & AGENCY_GEAR_CODE =="PTR", EFP_PROB := efp_prob[COVERAGE_TYPE == "PARTIAL", EFP_PROB]]              

# compare the number of trips to draw (to_draw) to the number of trips available to be drawn (draw_from) 
comp_draw <- merge(to_draw[, .(ADP, FMP, TRIP_TARGET_CODE, STRATA, TO_DRAW = C_TRIPS)],
                   draw_from[, .(DRAW_FROM = uniqueN(TRIP_ID)), by = .(FMP, TRIP_TARGET_CODE, STRATA)],
                   on = .(FMP, TRIP_TARGET_CODE, STRATA),
                   all = TRUE)

# check that we're not trying to draw trips for which we don't have recent data
to_draw_domains   <- unique(to_draw[C_TRIPS > 0, .(DOMAINS = paste(FMP, TRIP_TARGET_CODE, STRATA, sep = " "))])
draw_from_domains <- unique(draw_from[, .(DOMAINS = paste(FMP, TRIP_TARGET_CODE, STRATA, sep = " "))])
if(nrow(to_draw_domains[!(DOMAINS %in% unique(draw_from_domains$DOMAINS))]) != 0){warning("We expect effort in domains for which we don't have recent data")}

# merge the predicted number of trips for each domain onto draw_from
efrt_adpyear <- draw_from[to_draw, on = .(ADP, FMP, TRIP_TARGET_CODE, STRATA)]

# rename cumulative trips column to be clear that this is the predicted number of trips to occur
setnames(efrt_adpyear, "C_TRIPS", "TRIPS_PRED")

# save effort predictions (to_draw) and the population of trips to sample from (draw_from)
save(list = c("efrt", "efrt_adpyear", "gvf"), file = "source_data/effort_prediction.rdata")
