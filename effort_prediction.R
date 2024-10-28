# Prediction of future fishing effort
# Craig Faunce (CraigFaunce-NOAA github) and Christian Gredzens (cgredzens-NOAA github)

# This script uses annual numbers of trips from each statum belonging to the ADPyear and prior back to 2013.
# Its goal is to leverage the number of trips that have occurred in the current year to date.
# To do this, it runs a model to the year prior.  User selects best model.
# We then use this model to predict the end of the year trips for the current year.
# Then we see how well this process predicts the NEXT year (remember we are concerned with only next year from now)
# Then we calculate confidence intervals on the current end of year trips.
# This code also contains useful code for the final ADP to generate random draws from the distribution of 
# predictions from the model for the current year, assumed to be the same as next year.

#==============================#
## User inputs ---- ADPyear, whether outputs are to be saved this run, and the auth token for getting data.
#==============================#

ADPyear <- as.numeric(rstudioapi::showPrompt(title = "Enter year",
                                             message = "What year is the ADP year? (four digits, example: 2025)"))

saveoutputs <- "YES" # Must be "YES" to save figures and rdata file. Use any other value to skip saves.

if(!require("FMAtools")) devtools::install_github("Alaska-Fisheries-Monitoring-Analytics/FMAtools")
  ADP_dribble <- gdrive_set_dribble("Projects/ADP/source_data") # Prompts user input.

#==============================#
## Load packages ----
#==============================#

if(!require("data.table"))   install.packages("data.table", repos='http://cran.us.r-project.org')
if(!require("tidyverse"))   install.packages("tidyverse", repos='http://cran.us.r-project.org')
if(!require("ciTools"))   install.packages("ciTools", repos='http://cran.us.r-project.org') # Confidence interval calculation for GLMs
if(!require("ggpubr"))   install.packages("ggpubr", repos='http://cran.us.r-project.org')

# avoid scientific notation
options(scipen = 9999)

#==============================#
## Load data ----
#==============================#

gdrive_download(# Will only execute if you are not already up to date.
  local_path = paste0("source_data/", ADPyear, "_Final_ADP_data.rdata"),
  gdrive_dribble = ADP_dribble
  )

load(paste0("source_data/", ADPyear, "_Final_ADP_data.rdata"))

# Cleanup all but the absolutely necessary inputs
rm(list = setdiff(ls(), c("effort_strata", "ADPyear", "saveoutputs")))
gc()

# Create placeholder data
effort_strata.work <- effort_strata

#==============================#
## Modeling ----
#==============================#

# Visualize data
# Trip counts by stratum, using strata definitions of ADPyear
figure_c1 <- 
  ggplot(data = effort_strata.work[ADP < ADPyear - 1]) +
  geom_col(aes(x = ADP, y = TOTAL_TRIPS)) + facet_wrap(vars(STRATA), scales = "free_y") +
  scale_x_continuous(breaks = seq(min(effort_strata.work$ADP), ADPyear, by = 5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Year",
       y = "Total trips")

figure_c1

if(saveoutputs == "YES"){
  ggsave(filename = "output_figures/figure_c1.png", plot = figure_c1, width = 5, height = 5, units = "in")
}

# Model using GLM (# of trips is count data)
# When using poisson GLM, need to check for overdispersion.
# Quick and dirty way of checking is to divide Residual Deviance with Residual Degrees of Freedom (should = ~1)
#   if it's >> 1, use quasipoisson or negative binomial distribution
effort_glm1 <- glm(TOTAL_TRIPS ~ ADP, data = effort_strata.work[ADP < ADPyear - 1],
                   family = "quasipoisson")
summary(effort_glm1)

effort_glm2 <- glm(TOTAL_TRIPS ~ ADP * STRATA, data = effort_strata.work[ADP < ADPyear - 1],
                   family = "quasipoisson")
summary(effort_glm2)

effort_glm3 <- glm(TOTAL_TRIPS ~ ADP * STRATA * MAX_DATE_TRIPS, data = effort_strata.work[ADP < ADPyear - 1],
                   family = "quasipoisson")
summary(effort_glm3)

effort_glm3b <- glm(TOTAL_TRIPS ~ ADP + STRATA * MAX_DATE_TRIPS, data = effort_strata.work[ADP < ADPyear - 1],
                   family = "quasipoisson")
summary(effort_glm3b)

effort_glm4 <- glm(TOTAL_TRIPS ~ poly(ADP, 2) * STRATA * MAX_DATE_TRIPS, data = effort_strata.work[ADP < ADPyear - 1], 
                   family = "quasipoisson")
summary(effort_glm4)

effort_glm5 <- glm(TOTAL_TRIPS ~ poly(ADP, 3) * STRATA * MAX_DATE_TRIPS, data = effort_strata.work[ADP < ADPyear - 1], 
                   family = "quasipoisson")
summary(effort_glm5)

# Compare models
anova(effort_glm2, effort_glm1, test = "F")
anova(effort_glm3, effort_glm2, test = "F")
anova(effort_glm3b, effort_glm3, test = "F")
anova(effort_glm4, effort_glm3, test = "F")
anova(effort_glm5, effort_glm4, test = "F")

# Final model based on non-significant F tests for more complicated models.
effort_glm <- effort_glm3

# Evaluate "best" model
# 1) Check for overdispersion - (if = 1, then we don't need to use quasipoisson distribution)
E <- resid(effort_glm, type = "pearson")
N <- nrow(effort_strata.work[ADP < ADPyear -1])
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

# Clean up
rm(E, N, p, Fit, eta)

# Plot "best" model for visual evaluation 
ndata <- bind_cols(effort_strata.work,
          setNames(as_tibble(predict(effort_glm,
                                     newdata = effort_strata.work,
                                     se.fit = TRUE, type = "response")[1:2]),
                   c("fitted", "se")))

# Model fit to actual trip counts based on MAX_DATE_TRIPS
ggplot(ndata, aes(x = ADP, y = TOTAL_TRIPS)) +
  geom_point(na.rm = T) +
  geom_line(aes(y = fitted)) +
  geom_ribbon(aes(ymax = fitted + 1.96 * se, ymin = fitted - 1.96 * se), alpha = 0.3) + #Added the 1.96
  facet_wrap(vars(STRATA), scales = "free", ncol = 2) +
  scale_x_continuous(breaks = seq(min(effort_strata.work$ADP), ADPyear, by = 1)) +
  theme_bw()

#==============================#
## Assess model predictions ----
#==============================#

# 1) Used to assess how good our predictions are using the selected model
#     - Answers the following question: What predictions would we have gotten each ADP year using the
#       model informed by the data from prior years only
#   * Figure C2: How do predictions this year compare to true number of trips this year
#   * Figure C3: How do predictions last year compare to true number of trips this year 

#'* ISSUES HERE *:
# 1) Best model for this year not necessarily best model in previous years
# 2) Polynomial terms not useful for earlier datasets - causes issues in predictions

maxback <- 5 #TODO - user defined (here by precedent, but add a max possible with an error if it exceeds number of ADP by three or four...)

for(i in 1:maxback){
 preds <- effort_strata.work %>% filter(ADP == ADPyear - i)
 preds$TOTAL_TRIPS_PRED <- predict(glm(formula = effort_glm$formula, data = effort_strata.work[ADP < ADPyear - i],
                                       family = effort_glm$family$family), type = "response", preds)
 if(i == 1) {
   preds_out <- preds
 } else {
   preds_out <- rbind(preds, preds_out)}
}
# Warnings when using polynomials because polynomial terms not appropriate for some past time series

# Visualize predictions (line), with MAX_DATE_TRIPS in red and TOTAL_TRIPS in black
ggplot(preds_out, aes(x = ADP)) +
  geom_point(aes(y = TOTAL_TRIPS), na.rm = T) +
  geom_point(aes(y = MAX_DATE_TRIPS), color = "red", alpha = 0.5) +
  geom_line(aes(y = TOTAL_TRIPS_PRED)) +
  facet_wrap(vars(STRATA), scales = "free")
  
effort_strata.work <- merge(effort_strata.work, preds_out, all.x = TRUE)

rm(preds, preds_out, ndata)

# calculate residuals
effort_strata.work[, RESIDUALS := TOTAL_TRIPS - TOTAL_TRIPS_PRED] #TODO - useful but maybe calculate MSE?

# plot retrospective predictions against actuals for ADPyear - 1
# Dots are sized to their factor(year) to visually give more emphasis on recent years.
figure_c2a <- 
  ggplot(effort_strata.work[!is.na(RESIDUALS)], aes(x = TOTAL_TRIPS, color = STRATA)) +
  geom_point(aes(y = TOTAL_TRIPS_PRED), 
             size = as.numeric(as.factor(effort_strata.work[!is.na(RESIDUALS)]$ADP))) +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_viridis_d(option = "viridis", aesthetics = "color") +
  guides(color = guide_legend(ncol = 3)) +
  labs(x = "True stratum-specific trips in ADPyear - 1",
       y = "Predicted stratum-specific trips in ADPyear - 1",
       color = "Stratum")

figure_c2a

# plot retrospective residual histograms for ADPyear - 1 (old p2).  This plot shows the desired mean of zero and normal distribution of residuals based on 
# the distribution of residuals you got.  Compare this against the mean in blue we got and the density we got in blue.
# TODO - the text below in the stat_function is cumbersome.
figure_c2b <- 
  ggplot(effort_strata.work[!is.na(RESIDUALS)], aes(x = RESIDUALS)) +
  geom_histogram(aes(y = after_stat(density)), fill = "blue", color = "white", alpha = .5, bins = 20) +
  stat_function(
    fun = dnorm, 
    args = list(mean = 0, 
                sd = sd(effort_strata.work[!is.na(RESIDUALS)]$RESIDUALS)), 
    lwd = 2, 
    col = 'black') +
  geom_density(color = "blue", lwd = 2) +
  geom_vline(xintercept = 0, color = "black", lwd = 2) +
  geom_vline(aes(xintercept = mean(RESIDUALS)), lty = 2, color = "blue", lwd = 2) +
  theme_bw() +
  labs(x = "Residuals", y = "Density")

figure_c2b

figure_c2 <- ggarrange(figure_c2a, figure_c2b, ncol = 1, heights = c(1, 0.75))
figure_c2

if(saveoutputs == "YES"){
  ggsave(filename = "output_figures/figure_c2.png",
       figure_c2,
       width = 6.5, height = 10, units = "in")
}

# roll predictions forward one year
# This command increases the number of observations by a year * strata.
effort_strata.work <- merge(effort_strata.work[, !c("TOTAL_TRIPS_PRED", "RESIDUALS")], 
                       effort_strata.work[, .(ADP = ADP + 1, STRATA, TOTAL_TRIPS_PRED)], 
                       by.x = c("ADP", "STRATA"), by.y = c("ADP", "STRATA"), all = TRUE)
 
# Gotta recalculate residuals.
effort_strata.work[, RESIDUALS := TOTAL_TRIPS - TOTAL_TRIPS_PRED]

# plot retrospecitve predictions against actuals for ADPyear
figure_c3a <- 
  ggplot(effort_strata.work[!is.na(RESIDUALS)], aes(x = TOTAL_TRIPS, y = TOTAL_TRIPS_PRED, color = STRATA)) +
      geom_point(size = as.numeric(as.factor(effort_strata.work[!is.na(RESIDUALS)]$ADP))) +
      geom_abline(intercept = 0, slope = 1) +
      theme_bw() +
      theme(legend.position = "bottom") +
      scale_fill_viridis_d(option = "viridis", aesthetics = "color") +
      guides(color = guide_legend(ncol = 3)) +
      labs(x = "True stratum-specific trips in ADPyear", y = "Predicted stratum-specific trips in ADPyear - 1", color = "Stratum")

figure_c3a

# plot retrospective residuals for ADPyear
figure_c3b <- 
  ggplot(effort_strata.work[!is.na(RESIDUALS)], aes(x = RESIDUALS)) +
  geom_histogram(aes(y = after_stat(density)), fill = "blue", color = "white", alpha = .5, bins = 20) +
  stat_function(
    fun = dnorm, 
    args = list(mean = 0, 
                sd = sd(effort_strata.work[!is.na(RESIDUALS)]$RESIDUALS)), 
    lwd = 2, 
    col = 'black') +
  geom_density(color = "blue", lwd = 2) +
  geom_vline(xintercept = 0, color = "black", lwd = 2) +
  geom_vline(aes(xintercept = mean(RESIDUALS)), lty = 2, color = "blue", lwd = 2) +
  theme_bw() +
  labs(x = "Residuals", y = "Density")

figure_c3b

figure_c3 <- ggarrange(figure_c3a, figure_c3b, ncol = 1, heights = c(1, 0.75))
figure_c3

if(saveoutputs == "YES"){
  ggsave(filename = "output_figures/figure_c3.png",
       figure_c3,
       width = 6.5, height = 10, units = "in")
}

#==============================#
## Confidence intervals ----
#==============================#

# Prediction intervals?
# https://cran.r-project.org/web/packages/ciTools/vignettes/ciTools-glm-vignette.html
# But, meaningless for quasipoisson
# https://stats.stackexchange.com/questions/648734/difficulty-simulating-prediction-interval-for-quasipoisson-glm-in-r
# Can't predict prediction intervals

# Confidence intervals
pred_ints <- 
  add_ci(df = as.data.frame(effort_strata.work[ADP < ADPyear]),
         fit = effort_glm, names = c("lcb", "ucb"), alpha = 0.05) %>%
  select(!c(TOTAL_TRIPS_PRED, RESIDUALS)) %>%
  mutate(TOTAL_TRIPS = case_when(is.na(TOTAL_TRIPS) ~ pred,
                                 TRUE ~ TOTAL_TRIPS))

# Copy current year's data and predicted values
current_yr <- 
  pred_ints %>% 
  filter(ADP == ADPyear - 1) %>%
  mutate(ADP = ADPyear, MAX_DATE_TRIPS = NA)

# Combine data
pred_trips <- rbind(pred_ints, current_yr)

# Visualize
# Black is actual total trips, light blue is the model fit, and red is model predictions
figure_c4 <- 
  ggplot(data = pred_trips, aes(x = ADP, y = TOTAL_TRIPS)) +
  geom_ribbon(aes(ymin = lcb, ymax = ucb), alpha = 0.5, color = "black") +
  geom_point() +
  geom_point(aes(y = pred), color = "cyan", alpha = 0.75) +
  geom_point(data = pred_trips %>% filter(ADP >= ADPyear - 1), aes(y = TOTAL_TRIPS), color = "red") +
  facet_wrap(vars(STRATA), scales = "free_y") +
  scale_x_continuous(breaks = seq(min(pred_trips$ADP), ADPyear, by = 3)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10))) +
  labs(x = "Year",
       y = "Total trips")

figure_c4

if(saveoutputs == "YES"){
  ggsave(filename = "output_figures/figure_c4.png", plot = figure_c4, width = 5, height = 5, units = "in")
}

#==============================#
## Extract values for trip prediction from model ----
#==============================#
# This section is largely incorporated in the ADP script.

# Below we are grabbing the se values to use them for bootstrapping predictions.
# We only need to do this once

# Below we randomly sample a normal distribution of mean of the prediction 
# (in link space, where the assumption of normality is true) for the strata year combination (row)
# using the se.fit.  Then, we use the inverse link to convert back to predicted values.

# Calculate link se estimates using model
pred_ints[, c("mean", "sd")] <- predict(effort_glm, type = "link", se.fit = TRUE,
                                        newdata = pred_ints)[c("fit", "se.fit")]

# Peel off last years and use it for this year
effort_prediction <- pred_ints %>% filter(ADP == max(ADP))
effort_prediction$ADP <- ADPyear
effort_prediction$MAX_DATE_TRIPS <- NA

# Get inverse link function. For a quasipoisson model that uses a log link, the inverse is exp()
ilink <- family(effort_glm)$linkinv

#Set seed for random numbers and repeatability
set.seed(102424)

for(i in 1:nrow(effort_prediction)){
  # Start loop - for each stratum
  trip_draws.i <- 
    merge(
      effort_prediction[i,], 
      data.frame(TRIPS = ilink(rnorm(10000, mean = effort_prediction$mean[i], sd = effort_prediction$sd[i]))),
      all = TRUE)
  
  # go to next stratum.
  if(i == 1) trip_draws <- trip_draws.i
  if(i > 1) trip_draws <- rbind(trip_draws, trip_draws.i)
}

# Plot the results
# Calculate the probability quantiles from the random draws for comparison with the CIs from effort_prediction 
trip_draws_summary <- 
  trip_draws %>%
  group_by(STRATA) %>%
  summarize(Median = quantile(TRIPS, probs = 0.5),
         lcb = quantile(TRIPS, probs = 0.025),
         ucb = quantile(TRIPS, probs = 0.975)
         )

ggplot(data = trip_draws, aes(x = TRIPS)) +
  geom_histogram(fill = "blue", color = "white") +
  #Add values from model output
   geom_vline(data = effort_prediction, aes(xintercept = TOTAL_TRIPS), color = "red") +
   geom_vline(data = effort_prediction, aes(xintercept = lcb), lty = 2, color = 'red') +
   geom_vline(data = effort_prediction, aes(xintercept = ucb), lty = 2, color = "red") +
  # And compare them to those from the random number draws
   geom_vline(data = trip_draws_summary, aes(xintercept = Median), color = "blue", alpha = 0.5) +
   geom_vline(data = trip_draws_summary, aes(xintercept = lcb), color = "blue", alpha = 0.5) +
   geom_vline(data = trip_draws_summary, aes(xintercept = ucb), color = "blue", alpha = 0.5) +
   facet_wrap(~ STRATA, scales = "free", ncol = 2)

#==============================#
## Save results ----
#==============================#

# save effort predictions
if(saveoutputs == "YES"){
  save(list = c("effort_glm", "effort_prediction"),
       file = paste0("source_data/effort_prediction", ADPyear, ".rdata"))
}

gdrive_upload(
  local_path = paste0("source_data/effort_prediction", ADPyear, ".rdata"),
  gdrive_dribble = gdrive_set_dribble("Projects/ADP/source_data")
)

#==============================#
## Testing area ----
#==============================#

