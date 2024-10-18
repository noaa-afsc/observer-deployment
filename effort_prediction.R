# get packages if they aren't already loaded
if(!require("data.table"))   install.packages("data.table", repos='http://cran.us.r-project.org')
if(!require("ggplot2"))   install.packages("ggplot2", repos='http://cran.us.r-project.org')
if(!require("gridExtra"))   install.packages("gridExtra", repos='http://cran.us.r-project.org')
if(!require("scales"))   install.packages("scales", repos='http://cran.us.r-project.org')
if(!require("FMAtools")) devtools::install_github("Alaska-Fisheries-Monitoring-Analytics/FMAtools")

# avoid scientific notation
options(scipen = 9999)

# TODO - make into an API call
#user inputs
ADPyear  <- 2025

#Former
# get data object produced by get_data.R
#load(paste0("source_data/", ADPyear, "_Final_ADP_data.rdata"))
# https://drive.google.com/file/d/1xH-P54wW3vPXtaQmgEDxb3YgHh-yJlp8/view?usp=drive_link

# get list of trawl EM EFP vessels
# efp_list <- fread("source_data/efp_list_2023-09-05.csv")[, PERMIT := as.character(PERMIT)]
# https://drive.google.com/file/d/1eSSTal-w_y319xF67FRSdI23rv9BLCtn/view?usp=drive_link

#New
#TODO - make this an api call
ADP_dribble <- gdrive_set_dribble("Projects/ADP/source_data")

gdrive_download(# Will only execute if you are not already up to date.
  local_path = paste0("source_data/", ADPyear, "_Final_ADP_data.rdata"),
  gdrive_dribble = ADP_dribble
  )

load(paste0("source_data/", ADPyear, "_Final_ADP_data.rdata"))

rm(ADP_dribble)

# Visualize data
ggplot(data = effort_strata[ADP < ADPyear - 1]) +
  geom_col(aes(x = ADP, y = TOTAL_TRIPS)) + facet_wrap(vars(STRATA), scales = "free")

# Model using GLM (# of trips is count data)
# When using poisson GLM, need to check for overdispersion.
# Quick and dirty way of checking is to divide Residual Deviance with Residual Degrees of Freedom (should = ~1)
#   if it's >> 1, use quasipoisson or negative binomial distribution
effort_glm1 <- glm(TOTAL_TRIPS ~ ADP, data = effort_strata[ADP < ADPyear - 1],
                   family = "quasipoisson")
summary(effort_glm1)

effort_glm2 <- glm(TOTAL_TRIPS ~ ADP * STRATA, data = effort_strata[ADP < ADPyear - 1],
                   family = "quasipoisson")
summary(effort_glm2)

effort_glm3 <- glm(TOTAL_TRIPS ~ ADP * STRATA * MAX_DATE_TRIPS, data = effort_strata[ADP < ADPyear - 1],
                   family = "quasipoisson")
summary(effort_glm3)

effort_glm4 <- glm(TOTAL_TRIPS ~ poly(ADP, 2) * STRATA * MAX_DATE_TRIPS, data = effort_strata[ADP < ADPyear - 1], 
                   family = "quasipoisson")
summary(effort_glm4)

effort_glm5 <- glm(TOTAL_TRIPS ~ poly(ADP, 3) * STRATA * MAX_DATE_TRIPS, data = effort_strata[ADP < ADPyear - 1], 
                   family = "quasipoisson")
summary(effort_glm5)

# Compare models
anova(effort_glm2, effort_glm1, test = "F")
anova(effort_glm3, effort_glm2, test = "F")
anova(effort_glm4, effort_glm3, test = "F")
anova(effort_glm5, effort_glm4, test = "F")

# TODO - improve - "Best" model (for now just based on "a feeling")
effort_glm <- effort_glm4

# Evaluate "best" model
# 1) Check for overdispersion - (if = 1, then we don't need to use quasipoisson distribution)
E <- resid(effort_glm, type = "pearson")
N <- nrow(effort_strata[ADP < ADPyear -1])
p <- length(coef(effort_glm)) + 1
sum(E ^ 2) / (N - p) # Check for overdispersion (if > 1, then overdispersed)

# 2) Check residuals
#TODO - better way to get these?
Fit <- fitted(effort_glm)
eta <- predict(effort_glm, type = "link")
plot(x = Fit, y = E, xlab = "Fitted values", ylab = "Pearson residuals")
abline(h = 0, v = 0, lty = 2)
plot(x = eta, y = E, xlab = "Eta", ylab = "Pearson residuals") # This one we can get simply by: plot(effort_glm) - 1st plot 
abline(h = 0, v = 0, lty = 2)

# Plot "best" model for visual evaluation
# General plot (doesn't include third model term)

#TODO - generalize - below is hard code of "effort_glm"
ggplot(effort_strata[ADP < ADPyear - 1], aes(x = ADP, y = TOTAL_TRIPS)) +
  geom_point() +
  stat_smooth(method = "glm", formula = y ~ poly(x, 3), method.args = list(family = "quasipoisson")) +
  facet_wrap(vars(STRATA), scales = "free")

library(dplyr)

maxback <- 6 #TODO - user defined (here by precedent, but add a max possible with an error if it exceeds number of ADP by three or four...)

for(i in 1:maxback){
  preds <- effort_strata %>% filter(ADP == ADPyear - i)
  preds$TOTAL_TRIPS_PRED <- predict(effort_glm, type = "response", preds)
  if(i == 1)
    preds_out <- preds
  else
    preds_out <- rbind(preds, preds_out)
}

# Visualize predictions
ggplot(preds_out, aes(x = ADP)) +
  geom_point(aes(y = TOTAL_TRIPS)) +
  geom_point(aes(y = MAX_DATE_TRIPS), color = "red", alpha = 0.5) +
  geom_line(aes(y = TOTAL_TRIPS_PRED)) +
  facet_wrap(vars(STRATA), scales = "free")
  
effort_strata <- merge(effort_strata, preds_out, all.x = TRUE)

rm(preds, preds_out)

# calculate residuals
effort_strata[, RESIDUALS := TOTAL_TRIPS - TOTAL_TRIPS_PRED] #TODO - useful but maybe calculate MSE?

# plot retrospective predictions against actuals for ADPyear - 1
#TODO - why not plot where RESIDUALS is not NA?
ggplot(effort_strata[ADP < ADPyear - 1 & ADP >= ADPyear - maxback], aes(x = TOTAL_TRIPS, color = STRATA)) +
  geom_point(aes(y = TOTAL_TRIPS_PRED)) +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "True stratum-specific trips in ADPyear - 1", y = "Predicted stratum-specific trips in ADPyear - 1", color = "Stratum")

# plot retrospective residual histograms for ADPyear - 1 (old p2).  This plot shows the desired mean of zero and normal distribution of residuals based on 
# the distribution of residuals you got.  Compare this against the mean in blue we got and the density we got in blue.
# TODO - the text below in the stat_function is cumbersome.
ggplot(effort_strata[ADP < ADPyear - 1 & ADP >= ADPyear - maxback], aes(x = RESIDUALS)) +
  geom_histogram(aes(y = after_stat(density)), fill = "blue", color = "white", alpha = .5, bins = 20) +
  stat_function(
    fun = dnorm, 
    args = list(mean = 0, 
                sd = sd(effort_strata[ADP < ADPyear - 1 & ADP >= ADPyear - 6]$RESIDUALS)), 
    lwd = 2, 
    col = 'black') +
  geom_density(color = "blue", lwd = 2) +
  geom_vline(xintercept = 0, color = "black", lwd = 2) +
  geom_vline(aes(xintercept = mean(RESIDUALS)), lty = 2, color = "blue", lwd = 2) +
  theme_bw() +
  labs(x = "Residuals")

#'`-----------------TESTING----------------------------------------------------`

# Manually calculate 95% Confidence Intervals
# https://fromthebottomoftheheap.net/2018/12/10/confidence-intervals-for-glms/
# Grab inverse function from model
ilink <- family(effort_glm)$linkinv

# Add fit and se to data
ndata <- bind_cols(effort_strata[ADP <= ADPyear - 1],
                   setNames(as_tibble(predict(effort_glm,
                                              newdata = effort_strata[ADP <= ADPyear - 1],
                                              se.fit = TRUE)[1:2]),
                            c("fit_link", "se_link")))

# Create the interval and back transform
ndata <- ndata %>%
  mutate(fit_resp = ilink(fit_link),
         upr = ilink(fit_link + (2 * se_link)),
         lwr = ilink(fit_link - (2 * se_link))) %>%
  # Assign 2024 total trips as predicted trips
  mutate(TOTAL_TRIPS = case_when(is.na(TOTAL_TRIPS) ~ fit_resp,
                                 TRUE ~ TOTAL_TRIPS))

#'` This is the same as above, but uses maths from predFit to calculate CI`
ndata.test <- ndata %>%
  mutate(fit_resp = ilink(fit_link),
         upr = ilink(fit_link + se_link * stats::qnorm((0.95 + 1) / 2)),
         lwr = ilink(fit_link - se_link * stats::qnorm((0.95 + 1) / 2))) %>%
  # Assign 2024 total trips as predicted trips
  mutate(TOTAL_TRIPS = case_when(is.na(TOTAL_TRIPS) ~ fit_resp,
                                 TRUE ~ TOTAL_TRIPS))

# Visualize
ggplot(ndata[ADP >= ADPyear - 6], aes(x = ADP)) +
  geom_point(aes(y = TOTAL_TRIPS)) +
  geom_line(aes(y = TOTAL_TRIPS)) +
  geom_point(data = ndata[ADP == ADPyear - 1], aes(y = TOTAL_TRIPS), color = "red") +
  geom_errorbar(data = ndata[ADP == ADPyear - 1], aes(ymin = lwr, ymax = upr), color = "red", width = 0.2) +
  facet_wrap(vars(STRATA))

#'`----------------------------------------------------------------------------`


detach("package:dplyr")

# png("Appendix_C/figures/EffortPredictionResiduals1.png", width = 7, height = 10, units = 'in', res=300)
# grid.arrange(p1, p2)
# dev.off()

# roll predictions forward one year
#TODO - I had to change the original "on" statement to by.x and by.y...
# This command increases the number of observations by a year * strata.
 effort_strata <- merge(effort_strata[, !c("TOTAL_TRIPS_PRED", "RESIDUALS")], 
   effort_strata[, .(ADP = ADP + 1, STRATA, TOTAL_TRIPS_PRED)], 
   by.x = c("ADP", "STRATA"), by.y = c("ADP", "STRATA"), all = TRUE)
 
# Gotta recalculate residuals.
effort_strata[, RESIDUALS := TOTAL_TRIPS - TOTAL_TRIPS_PRED]

# plot retrospecitve predictions against actuals for ADPyear
ggplot(effort_strata[!is.na(RESIDUALS)], aes(x = TOTAL_TRIPS, y = TOTAL_TRIPS_PRED, color = STRATA)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1) +
      theme_bw() +
      theme(legend.position = "bottom") +
      labs(x = "True stratum-specific trips in ADPyear", y = "Predicted stratum-specific trips in ADPyear - 1", color = "Stratum")

# plot retrospective residuals for ADPyear
#TODO - amend as P2
ggplot(effort_strata[!is.na(RESIDUALS)], aes(x = RESIDUALS)) +
      geom_histogram(bins = 20) +
      geom_vline(xintercept = 0, color = "red") +
      geom_vline(aes(xintercept = mean(RESIDUALS)), lty = 2, color = "red") +
      theme_bw() +
      labs(x = "Residuals")

# png("Appendix_C/figures/EffortPredictionResiduals2.png", width = 7, height = 10, units = 'in', res=300)
# grid.arrange(p3, p4)
# dev.off()

#TODO - replace this with bootstrapped outcomes to generate prediction intervals.

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

#' estimate the prediction intervals for stratum-level estimates `CG: Right here it looks like we create a new `*pred*`object and use that to create the values that are plotted. So what was the point of the variance/covariance matrix above?`
pred <- data.frame(predict(lm(effort_mod$call$formula, data = effort_strata[ADP < ADPyear - 1]), effort_strata[ADP == ADPyear - 1], interval = "prediction"))

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
