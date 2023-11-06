# get packages if they aren't already loaded
if(!require("data.table"))   install.packages("data.table", repos='http://cran.us.r-project.org')
if(!require("ggplot2"))   install.packages("ggplot2", repos='http://cran.us.r-project.org')
if(!require("scales"))   install.packages("scales", repos='http://cran.us.r-project.org')

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
effort_strata <- work.data[CVG_NEW == "PARTIAL" & STRATA_NEW != "ZERO" & AGENCY_GEAR_CODE != "JIG", .(ADP, STRATA = STRATA_NEW, TRIP_TARGET_DATE, TRIP_ID)]

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

# count trips through October and December by year and stratum
effort_strata <- effort_strata[, .(THRU_OCT_TRIPS = sum(TRIPS[JULIAN_DATE <= max_date]), TOTAL_TRIPS = sum(TRIPS)), by = .(ADP, STRATA)]

# model total trips against year, stratum, and trips through October
effort_mod <- lm(TOTAL_TRIPS ~ ADP * STRATA * THRU_OCT_TRIPS, data = effort_strata[ADP < ADPyear - 1])

# predict ADPyear - 1 effort
new_data <- effort_strata[ADP == ADPyear - 1]
effort_strata[ADP == ADPyear - 1, TOTAL_TRIPS := predict(effort_mod, new_data)]

# plot trips through December against trips through October by stratum
p1 <- ggplot(effort_strata[ADP < ADPyear - 1], aes(x = THRU_OCT_TRIPS, y = TOTAL_TRIPS)) +
      facet_wrap(STRATA ~ ., scales = "free") +
      geom_point() +
      geom_point(data = effort_strata[ADP == ADPyear - 1], color = "red") +
      labs(x = "Trips through October", y = "Trips through December") +
      theme_bw()

# plot trips through December against year by stratum
p2 <- ggplot(effort_strata[ADP < ADPyear - 1], aes(x = ADP, y = TOTAL_TRIPS)) +
      facet_wrap(STRATA ~ ., scales = "free") +
      geom_point() +
      geom_point(data = effort_strata[ADP == ADPyear - 1], color = "red") +
      geom_line(data = effort_strata) +
      scale_x_continuous(breaks = min(effort_strata$ADP):ADPyear - 1) +
      expand_limits(y = 0) +
      labs(x = "Year", y = "Trips through December") +
      theme_bw()

# sum trips by year
effort_year <- effort_strata[, .(TOTAL_TRIPS = sum(TOTAL_TRIPS)), by = ADP]

# plot trips through December against year
p3 <- ggplot(effort_year, aes(x = ADP, y = TOTAL_TRIPS)) +
      geom_point() +
      geom_point(data = effort_year[ADP == ADPyear - 1], color = "red") +
      scale_x_continuous(breaks = min(effort_strata$ADP):ADPyear - 1) +
      expand_limits(y = 0) +
      labs(x = "Year", y = "Trips through December") +
      theme_bw()

# model total trips against year and stratum
effort_mod <- lm(TOTAL_TRIPS ~ ADP * STRATA, data = effort_strata)

# predict ADPyear effort with 95% confidence interval  
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

# create new data
new_data <- CJ(ADP = min(effort_strata$ADP):ADPyear, STRATA = unique(effort_strata$STRATA))

# predict effort
pred <- lm_predict(effort_mod, new_data, diag = FALSE)

# save fitted values
effort_pred_strata <- copy(new_data)[, fit := pred$fit]

# sum fits by year
effort_pred_year <- effort_pred_strata[, .(fit = sum(fit)), by = .(ADP)]

# adjust the variance-covariance matrix with residual variance
VCOV_adj <- with(pred, var.fit + diag(residual.var, nrow(var.fit)))

# sum adjusted variance
effort_pred_year[, var := sum(VCOV_adj[(((ADP - 2012) * 7) - 6):((ADP - 2012) * 7), (((ADP - 2012) * 7) - 6):((ADP - 2012) * 7)]), by = ADP]

# set alpha level for the prediction interval
alpha <- 0.95

# estimate t-values for the prediction interval
Qt <- qt((1 - alpha) / 2, effort_mod$df.residual, lower.tail = FALSE)

# estimate the prediction interval
effort_pred_year[, ':=' (lwr = fit - (Qt * sqrt(var)), upr = fit + (Qt * sqrt(var)))]

# plot data, prediction, and prediction interval
p4 <- ggplot(effort_year[ADP < ADPyear - 1], aes(x = ADP, y = TOTAL_TRIPS)) +
      geom_point() +
      geom_point(data = effort_year[ADP == ADPyear - 1], color = "red") +
      geom_point(aes(x = ADP, y = fit), data = effort_pred_year[ADP == ADPyear], color = "red") +
      geom_errorbar(aes(ymin = lwr, ymax = upr), data = effort_pred_year[ADP == ADPyear, .(ADP, TOTAL_TRIPS = fit, lwr, upr)], color = "red", width = 0.2) +
      geom_line(aes(x = ADP, y = fit), data = effort_pred_year, color = "red") +
      scale_x_continuous(breaks = min(effort_year$ADP):ADPyear) +
      scale_y_continuous(labels = comma) +
      expand_limits(y = 0) +
      labs(x = "Year", y = "Partial Coverage Trips (Excluding Zero Coverage)") +
      theme_classic()

# png("output_figures/TripsPerYear.png", width = 7, height = 5, units = 'in', res=300)
# p4
# dev.off()

# estimate probability of trawl EM boats not taking an observer (efp_prob; based on data from vessels that have been in 
# the program prior to ADPyear). 2022-05-31 is the last day of the 2022 spring fisheries, and 2023-09-01 is the first day 
# of the 2023 fall fisheries. by including trips after the 2022 spring fisheries have closed and before the 2023 fall 
# fisheries have opened, we calculate efp_prob based on the fall 2022 and spring 2023 seasons (the two most recent full 
# seasons, as the fall 2023 fishery was not finished by the time Valhalla was compiled).  
efp_prob <- unique(work.data[
  TRIP_TARGET_DATE > as.Date("2022-05-31") & TRIP_TARGET_DATE < as.Date("2023-09-01") & 
    VESSEL_ID %in% efp_list$PERMIT[efp_list$YEAR_ADDED < ADPyear] &
    TRIP_TARGET_CODE %in% c("P", "B") & AGENCY_GEAR_CODE %in% c("NPT", "PTR"),
  .(TRIP_ID, COVERAGE_TYPE, STRATA, AGENCY_GEAR_CODE)
])[, .SD[all(AGENCY_GEAR_CODE == "PTR")], by = .(TRIP_ID)
][, .N, by = .(COVERAGE_TYPE, STRATA)
][, .(STRATA, N, EFP_PROB = N / sum(N)), by = .(COVERAGE_TYPE)
][STRATA == "EM_TRW_EFP"]         

# save effort predictions (to_draw) and the population of trips to sample from (draw_from)
save(list = c("effort_pred_year", "effort_pred_strata", "efp_prob"), file = "source_data/effort_prediction.rdata")
