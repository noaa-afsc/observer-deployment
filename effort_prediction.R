# get packages if they aren't already loaded
if(!require("data.table"))   install.packages("data.table", repos='http://cran.us.r-project.org')
if(!require("ggplot2"))   install.packages("ggplot2", repos='http://cran.us.r-project.org')
if(!require("gridExtra"))   install.packages("gridExtra", repos='http://cran.us.r-project.org')
if(!require("scales"))   install.packages("scales", repos='http://cran.us.r-project.org')

# avoid scientific notation
options(scipen = 9999)

# user inputs
ADPyear  <- 2024

# get data object produced by get_data.R
load(paste0("source_data/", ADPyear, "_Final_ADP_data.rdata"))
# https://drive.google.com/file/d/1xH-P54wW3vPXtaQmgEDxb3YgHh-yJlp8/view?usp=drive_link

# get list of trawl EM EFP vessels
efp_list <- fread("source_data/efp_list_2023-09-05.csv")[, PERMIT := as.character(PERMIT)]
# https://drive.google.com/file/d/1eSSTal-w_y319xF67FRSdI23rv9BLCtn/view?usp=drive_link

# select necessary columns
effort_strata <- work.data[CVG_NEW == "PARTIAL", .(ADP, STRATA = STRATA_NEW, TRIP_TARGET_DATE, TRIP_ID)]

# ensure one trip target date per trip, making it the minimum trip target date
effort_strata[, TRIP_TARGET_DATE := min(TRIP_TARGET_DATE), by = TRIP_ID]

# select only distinct rows
effort_strata <- unique(effort_strata)

# order the data
setorder(effort_strata, ADP, STRATA, TRIP_TARGET_DATE)

# split trips if they occurred in more than one stratum
effort_strata[ , TRIPS := 1/.N, by = TRIP_ID]

# find julian dates
effort_strata[, JULIAN_DATE := yday(TRIP_TARGET_DATE)]

# set julian date to 1 for trips that left in year adp - 1
effort_strata[, JULIAN_DATE := ifelse(year(TRIP_TARGET_DATE) < ADP, 1, JULIAN_DATE)]

# set julian date to 366 for trips that left in year adp + 1
effort_strata[, JULIAN_DATE := ifelse(year(TRIP_TARGET_DATE) > ADP, 366, JULIAN_DATE)]

# isolate the latest date for which we have data in the most recent year of valhalla
max_date <- max(effort_strata[ADP == ADPyear - 1, JULIAN_DATE])

# count trips through max_date and total trips by year and stratum
effort_strata <- effort_strata[, .(MAX_DATE_TRIPS = sum(TRIPS[JULIAN_DATE <= max_date]), TOTAL_TRIPS = sum(TRIPS)), by = .(ADP, STRATA)]

# make total trips NA for ADPyear - 1, since the year is not over
effort_strata[ADP == ADPyear - 1, TOTAL_TRIPS := NA]

# model total trips against year, stratum, and trips through October
effort_mod1 <- lm(TOTAL_TRIPS ~ ADP, data = effort_strata[ADP < ADPyear - 1])
effort_mod2 <- lm(TOTAL_TRIPS ~ ADP * STRATA, data = effort_strata[ADP < ADPyear - 1])
effort_mod3 <- lm(TOTAL_TRIPS ~ ADP * STRATA * MAX_DATE_TRIPS, data = effort_strata[ADP < ADPyear - 1])

# evaluate candidate models
AIC(effort_mod1, effort_mod2, effort_mod3)

# choose a winning model
effort_mod <- effort_mod3
rm(effort_mod1, effort_mod2, effort_mod3)

# predict ADPyear - 6 effort
effort_strata[ADP == ADPyear - 6, TOTAL_TRIPS_PRED := predict(lm(TOTAL_TRIPS ~ ADP * STRATA * MAX_DATE_TRIPS, data = effort_strata[ADP < ADPyear - 6]), effort_strata[ADP == ADPyear - 6])]

# predict ADPyear - 5 effort
effort_strata[ADP == ADPyear - 5, TOTAL_TRIPS_PRED := predict(lm(TOTAL_TRIPS ~ ADP * STRATA * MAX_DATE_TRIPS, data = effort_strata[ADP < ADPyear - 5]), effort_strata[ADP == ADPyear - 5])]

# predict ADPyear - 4 effort
effort_strata[ADP == ADPyear - 4, TOTAL_TRIPS_PRED := predict(lm(TOTAL_TRIPS ~ ADP * STRATA * MAX_DATE_TRIPS, data = effort_strata[ADP < ADPyear - 4]), effort_strata[ADP == ADPyear - 4])]

# predict ADPyear - 3 effort
effort_strata[ADP == ADPyear - 3, TOTAL_TRIPS_PRED := predict(lm(TOTAL_TRIPS ~ ADP * STRATA * MAX_DATE_TRIPS, data = effort_strata[ADP < ADPyear - 3]), effort_strata[ADP == ADPyear - 3])]

# predict ADPyear - 2 effort
effort_strata[ADP == ADPyear - 2, TOTAL_TRIPS_PRED := predict(lm(TOTAL_TRIPS ~ ADP * STRATA * MAX_DATE_TRIPS, data = effort_strata[ADP < ADPyear - 2]), effort_strata[ADP == ADPyear - 2])]

# predict ADPyear - 1 effort
effort_strata[ADP == ADPyear - 1, TOTAL_TRIPS_PRED := predict(lm(TOTAL_TRIPS ~ ADP * STRATA * MAX_DATE_TRIPS, data = effort_strata[ADP < ADPyear - 1]), effort_strata[ADP == ADPyear - 1])]

# calculate residuals
effort_strata[, RESIDUALS := TOTAL_TRIPS - TOTAL_TRIPS_PRED]

# plot retrospective predictions against actuals for ADPyear - 1
p1 <- ggplot(effort_strata[ADP < ADPyear - 1 & ADP >= ADPyear - 6], aes(x = TOTAL_TRIPS, y = TOTAL_TRIPS_PRED, color = STRATA)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1) +
      theme_bw() +
      theme(legend.position = "bottom") +
      labs(x = "True stratum-specific trips in ADPyear - 1", y = "Predicted stratum-specific trips in ADPyear - 1", color = "Stratum")

# plot retrospective residuals for ADPyear - 1
p2 <- ggplot(effort_strata[ADP < ADPyear - 1 & ADP >= ADPyear - 6], aes(x = RESIDUALS)) +
      geom_histogram(bins = 20) +
      geom_vline(xintercept = 0, color = "red") +
      geom_vline(aes(xintercept = mean(RESIDUALS)), lty = 2, color = "red") +
      theme_bw() +
      labs(x = "Residuals")

# png("Appendix_C/figures/EffortPredictionResiduals1.png", width = 7, height = 10, units = 'in', res=300)
# grid.arrange(p1, p2)
# dev.off()

# roll predictions forward one year
effort_strata <- merge(effort_strata[, !c("TOTAL_TRIPS_PRED", "RESIDUALS")], effort_strata[, .(ADP = ADP + 1, STRATA, TOTAL_TRIPS_PRED)], on = .(ADP, STRATA), all = TRUE)

# recalculate residuals
effort_strata[, RESIDUALS := TOTAL_TRIPS - TOTAL_TRIPS_PRED]

# plot retrospecitve predictions against actuals for ADPyear
p3 <- ggplot(effort_strata[!is.na(RESIDUALS)], aes(x = TOTAL_TRIPS, y = TOTAL_TRIPS_PRED, color = STRATA)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1) +
      theme_bw() +
      theme(legend.position = "bottom") +
      labs(x = "True stratum-specific trips in ADPyear", y = "Predicted stratum-specific trips in ADPyear - 1", color = "Stratum")

# plot retrospective residuals for ADPyear 
p4 <- ggplot(effort_strata[!is.na(RESIDUALS)], aes(x = RESIDUALS)) +
      geom_histogram(bins = 20) +
      geom_vline(xintercept = 0, color = "red") +
      geom_vline(aes(xintercept = mean(RESIDUALS)), lty = 2, color = "red") +
      theme_bw() +
      labs(x = "Residuals")

# png("Appendix_C/figures/EffortPredictionResiduals2.png", width = 7, height = 10, units = 'in', res=300)
# grid.arrange(p3, p4)
# dev.off()

# predict total effort across strata with 95% prediction interval for ADPyear - 1  
# https://stackoverflow.com/questions/39337862/linear-model-with-lm-how-to-get-prediction-variance-of-sum-of-predicted-value
lm_predict <- function (lmObject, newdata, diag = TRUE) {
  ## input checking
  if (!inherits(lmObject, "lm")) stop("'lmObject' is not a valid 'lm' object!")
  ## extract "terms" object from the fitted model, but delete response variable
  tm <- delete.response(terms(lmObject))      
  ## linear predictor matrix
  Xp <- model.matrix(tm, newdata)
  ## predicted values by direct matrix-vector multiplication
  pred <- c(Xp %*% coef(lmObject))
  ## efficiently form the complete variance-covariance matrix
  QR <- lmObject$qr   ## qr object of fitted model
  piv <- QR$pivot     ## pivoting index
  r <- QR$rank        ## model rank / numeric rank
  if (is.unsorted(piv)) {
    ## pivoting has been done
    B <- forwardsolve(t(QR$qr), t(Xp[, piv]), r)
  } else {
    ## no pivoting is done
    B <- forwardsolve(t(QR$qr), t(Xp), r)
  }
  ## residual variance
  sig2 <- c(crossprod(residuals(lmObject))) / df.residual(lmObject)
  if (diag) {
    ## return point-wise prediction variance
    VCOV <- colSums(B ^ 2) * sig2
  } else {
    ## return full variance-covariance matrix of predicted values
    VCOV <- crossprod(B) * sig2
  }
  list(fit = pred, var.fit = VCOV, df = lmObject$df.residual, residual.var = sig2)
}

# run model on independent variable values from ADPyear - 1
pred <- lm_predict(effort_mod, effort_strata[ADP == ADPyear - 1], diag = FALSE)

# verify that these match the ADPyear - 1 predictions that have already been rolled over to ADPyear
identical(pred$fit, effort_strata[ADP == ADPyear, TOTAL_TRIPS_PRED])

# save fitted values from the new function
effort_strata[, TOTAL_TRIPS_PRED := ifelse(ADP >= ADPyear - 1, rep(pred$fit, 2), NA)][, RESIDUALS := NULL]

# make total trips equal to predictions for years >= ADPyear - 1
effort_strata[!is.na(TOTAL_TRIPS_PRED), TOTAL_TRIPS := TOTAL_TRIPS_PRED][, ":=" (PREDICTION = !is.na(TOTAL_TRIPS_PRED), TOTAL_TRIPS_PRED = NULL)]

# summarize by year
effort_year <- effort_strata[, .(TOTAL_TRIPS = sum(TOTAL_TRIPS), PREDICTION = unique(PREDICTION)), by = .(ADP)]

# adjust the variance-covariance matrix with residual variance
VCOV_adj <- with(pred, var.fit + diag(residual.var, nrow(var.fit)))

# sum adjusted variance
effort_year[ADP >= ADPyear - 1, var := rep(sum(VCOV_adj), 2)]

# set alpha level for the prediction interval
alpha <- 0.95

# estimate t-values for the prediction interval
Qt <- qt((1 - alpha) / 2, effort_mod$df.residual, lower.tail = FALSE)

# estimate the prediction intervals for year-level estimates
effort_year[, ':=' (se = sqrt(var), lwr = TOTAL_TRIPS - (Qt * sqrt(var)), upr = TOTAL_TRIPS + (Qt * sqrt(var)), var = NULL)]

# estimate the prediction intervals for stratum-level estimates
pred <- data.frame(predict(lm(TOTAL_TRIPS ~ ADP * STRATA * MAX_DATE_TRIPS, data = effort_strata[ADP < ADPyear - 1]), effort_strata[ADP == ADPyear - 1], interval = "prediction"))
effort_strata[ADP >= ADPyear - 1, ":=" (se = (rep(pred$upr, 2) - TOTAL_TRIPS) / Qt, lwr = rep(pred$lwr, 2), upr = rep(pred$upr, 2))]

# plot trips through December against trips through October by stratum through ADPyear - 1
p5 <- ggplot(effort_strata[ADP <= ADPyear - 1], aes(x = MAX_DATE_TRIPS, y = TOTAL_TRIPS)) +
      facet_wrap(STRATA ~ ., scales = "free") +
      geom_point(aes(color = PREDICTION)) +
      scale_color_manual(values = c("black", "red")) +
      labs(x = "Trips Through Date of Analysis", y = "Trips Through December") +
      theme_bw() +
      theme(legend.position = "none")

# png("Appendix_C/figures/MaxDateTrips.png", width = 7, height = 5, units = 'in', res=300)
# p5
# dev.off()

# plot trips through December against year by stratum through ADPyear
p6 <- ggplot(effort_strata, aes(x = ADP, y = TOTAL_TRIPS)) +
      facet_wrap(STRATA ~ ., scales = "free") +
      geom_point(aes(color = PREDICTION)) +
      scale_color_manual(values = c("black", "red")) +
      geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2, color = "red") + 
      geom_line() + 
      geom_line(data = effort_strata[ADP >= ADPyear - 1], color = "red") +
      scale_x_continuous(breaks = unique(effort_strata$ADP)) +
      expand_limits(y = 0) +
      labs(x = "Year", y = "Trips through December") +
      theme_bw() +
      theme(legend.position = "none")

# plot trips through December against year through ADPyear
p7 <- ggplot(effort_year, aes(x = ADP, y = TOTAL_TRIPS)) +
      geom_point(aes(color = PREDICTION)) +
      scale_color_manual(values = c("black", "red")) +
      geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2, color = "red") + 
      geom_line() + 
      geom_line(data = effort_year[ADP >= ADPyear - 1], color = "red") +
      scale_x_continuous(breaks = min(effort_year$ADP):ADPyear) +
      scale_y_continuous(labels = comma) +
      expand_limits(y = 0) +
      labs(x = "Year", y = "Partial Coverage Trips") +
      theme_classic() +
      theme(legend.position = "none")

# png("Appendix_C/figures/TripsPerYear.png", width = 7, height = 5, units = 'in', res=300)
# p7
# dev.off()

# estimate probability of trawl EM boats not taking an observer (efp_prob; based on data from vessels that have been in 
# the program prior to ADPyear). 2022-05-31 is the last day of the 2022 spring fisheries, and 2023-09-01 is the first day 
# of the 2023 fall fisheries. by including trips after the 2022 spring fisheries have closed and before the 2023 fall 
# fisheries have opened, we calculate efp_prob based on the fall 2022 and spring 2023 seasons (the two most recent full 
# seasons, as the fall 2023 fishery was not finished by the time Valhalla was compiled).  
efp_prob <- efp_list[unique(work.data[
  TRIP_TARGET_DATE > as.Date("2022-05-31") & TRIP_TARGET_DATE < as.Date("2023-09-01") & 
    TRIP_TARGET_CODE %in% c("P", "B") & AGENCY_GEAR_CODE %in% c("NPT", "PTR"),
  .(ADP, VESSEL_ID, TRIP_ID, COVERAGE_TYPE, STRATA, AGENCY_GEAR_CODE)
]), on = c(PERMIT = "VESSEL_ID")
][YEAR_ADDED <= ADP
][, .SD[all(AGENCY_GEAR_CODE == "PTR")], by = .(TRIP_ID)
][, .N, by = .(COVERAGE_TYPE, STRATA)
][, .(STRATA, N, EFP_PROB = N / sum(N)), by = .(COVERAGE_TYPE)
][STRATA == "EM_TRW_EFP"]         

# save effort predictions (to_draw) and the population of trips to sample from (draw_from)
save(list = c("effort_year", "effort_strata", "efp_prob"), file = "source_data/effort_prediction.rdata")
