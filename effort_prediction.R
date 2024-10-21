#==============================#
## User inputs ----
#==============================#

# TODO - make into an API call
#user inputs
ADPyear <- 2025

#==============================#
## Load Packages ----
#==============================#

if(!require("data.table"))   install.packages("data.table", repos='http://cran.us.r-project.org')
if(!require("tidyverse"))   install.packages("tidyverse", repos='http://cran.us.r-project.org')
if(!require("FMAtools")) devtools::install_github("Alaska-Fisheries-Monitoring-Analytics/FMAtools")
if(!require("ciTools"))   install.packages("ciTools", repos='http://cran.us.r-project.org') # Confidence interval calculation for GLMs
if(!require("ggpubr"))   install.packages("ggpubr", repos='http://cran.us.r-project.org')

# avoid scientific notation
options(scipen = 9999)

#==============================#
## Load data ----
#==============================#

#TODO - make this an api call
ADP_dribble <- gdrive_set_dribble("Projects/ADP/source_data")

gdrive_download(# Will only execute if you are not already up to date.
  local_path = paste0("source_data/", ADPyear, "_Final_ADP_data.rdata"),
  gdrive_dribble = ADP_dribble
  )

load(paste0("source_data/", ADPyear, "_Final_ADP_data.rdata"))

rm(list = setdiff(ls(), c("effort_strata", "ADPyear")))

# Create placeholder data
effort_strata.work <- effort_strata

#==============================#
## Modeling ----
#==============================#

# Visualize data
figure_c1 <- ggplot(data = effort_strata.work[ADP < ADPyear - 1]) +
  geom_col(aes(x = ADP, y = TOTAL_TRIPS)) + facet_wrap(vars(STRATA), scales = "free_y") +
  scale_x_continuous(breaks = seq(min(effort_strata.work$ADP), ADPyear, by = 5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Year",
       y = "Total trips")

ggsave(filename = "output_figures/figure_c1.png", plot = figure_c1, width = 5, height = 5, units = "in")

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

effort_glm4 <- glm(TOTAL_TRIPS ~ poly(ADP, 2) * STRATA * MAX_DATE_TRIPS, data = effort_strata.work[ADP < ADPyear - 1], 
                   family = "quasipoisson")
summary(effort_glm4)

effort_glm5 <- glm(TOTAL_TRIPS ~ poly(ADP, 3) * STRATA * MAX_DATE_TRIPS, data = effort_strata.work[ADP < ADPyear - 1], 
                   family = "quasipoisson")
summary(effort_glm5)

# Compare models
anova(effort_glm2, effort_glm1, test = "F")
anova(effort_glm3, effort_glm2, test = "F")
anova(effort_glm4, effort_glm3, test = "F")
anova(effort_glm5, effort_glm4, test = "F")

# TODO - improve - "Best" model (for now just based on "a feeling")
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
ndata <- bind_cols(effort_strata.work[ADP <= ADPyear],
          setNames(as_tibble(predict(effort_glm,
                                     newdata = effort_strata.work[ADP <= ADPyear],
                                     se.fit = TRUE, , type = "response")[1:2]),
                   c("fitted", "se")))

ggplot(ndata, aes(x = ADP, y = TOTAL_TRIPS)) +
  geom_point() +
  geom_line(aes(y = fitted)) +
  geom_ribbon(aes(ymax = fitted + se, ymin = fitted - se), alpha = 0.5) +
  facet_wrap(vars(STRATA), scales = "free") +
  scale_x_continuous(breaks = seq(min(effort_strata.work$ADP), ADPyear, by = 1)) +
  theme_bw()

#==============================#
## Assess model predictions ----
#==============================#

# 1) Used to assess how good our predictions are using the selected model
# 2) Predicts data for each year using selected year from "maxback"

#'* ISSUES HERE *:
# 1) Best model for this year not necessarily best model in previous years
# 2) Polynomial terms not useful for earlier datasets - causes issues in predictions

#library(dplyr)

maxback <- 5 #TODO - user defined (here by precedent, but add a max possible with an error if it exceeds number of ADP by three or four...)

for(i in 1:maxback){
 preds <- effort_strata.work %>% filter(ADP == ADPyear - i)
 preds$TOTAL_TRIPS_PRED <- predict(glm(formula = effort_glm$formula, data = effort_strata.work[ADP < ADPyear - i],
                                       family = effort_glm$family$family), type = "response", preds)
 if(i == 1)
   preds_out <- preds
 else
   preds_out <- rbind(preds, preds_out)
}
# Warnings when using polynomials because polynomial terms not appropriate for some past time series

# Visualize predictions
ggplot(preds_out, aes(x = ADP)) +
  geom_point(aes(y = TOTAL_TRIPS)) +
  geom_point(aes(y = MAX_DATE_TRIPS), color = "red", alpha = 0.5) +
  geom_line(aes(y = TOTAL_TRIPS_PRED)) +
  facet_wrap(vars(STRATA), scales = "free")
  
effort_strata.work <- merge(effort_strata.work, preds_out, all.x = TRUE)

rm(preds, preds_out, ndata)

# calculate residuals
effort_strata.work[, RESIDUALS := TOTAL_TRIPS - TOTAL_TRIPS_PRED] #TODO - useful but maybe calculate MSE?

# plot retrospective predictions against actuals for ADPyear - 1
figure_c2a <- ggplot(effort_strata.work[!is.na(RESIDUALS)], aes(x = TOTAL_TRIPS, color = STRATA)) +
  geom_point(aes(y = TOTAL_TRIPS_PRED)) +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_viridis_d(option = "viridis", aesthetics = "color") +
  guides(color = guide_legend(ncol = 3)) +
  labs(x = "True stratum-specific trips in ADPyear - 1",
       y = "Predicted stratum-specific trips in ADPyear - 1",
       color = "Stratum")

# plot retrospective residual histograms for ADPyear - 1 (old p2).  This plot shows the desired mean of zero and normal distribution of residuals based on 
# the distribution of residuals you got.  Compare this against the mean in blue we got and the density we got in blue.
# TODO - the text below in the stat_function is cumbersome.
figure_c2b <- ggplot(effort_strata.work[!is.na(RESIDUALS)], aes(x = RESIDUALS)) +
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
  labs(x = "Residuals")

figure_c2 <- ggarrange(figure_c2a, figure_c2b, ncol = 1, heights = c(1, 0.75))

ggsave(filename = "output_figures/figure_c2.png",
       figure_c2,
       width = 6.5, height = 10, units = "in")

# roll predictions forward one year
#TODO - I had to change the original "on" statement to by.x and by.y...
# This command increases the number of observations by a year * strata.
effort_strata.work <- merge(effort_strata.work[, !c("TOTAL_TRIPS_PRED", "RESIDUALS")], 
                       effort_strata.work[, .(ADP = ADP + 1, STRATA, TOTAL_TRIPS_PRED)], 
                       by.x = c("ADP", "STRATA"), by.y = c("ADP", "STRATA"), all = TRUE)
 
# Gotta recalculate residuals.
effort_strata.work[, RESIDUALS := TOTAL_TRIPS - TOTAL_TRIPS_PRED]

# plot retrospecitve predictions against actuals for ADPyear
figure_c3a <- ggplot(effort_strata.work[!is.na(RESIDUALS)], aes(x = TOTAL_TRIPS, y = TOTAL_TRIPS_PRED, color = STRATA)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1) +
      theme_bw() +
      theme(legend.position = "bottom") +
      scale_fill_viridis_d(option = "viridis", aesthetics = "color") +
      guides(color = guide_legend(ncol = 3)) +
      labs(x = "True stratum-specific trips in ADPyear", y = "Predicted stratum-specific trips in ADPyear - 1", color = "Stratum")

# plot retrospective residuals for ADPyear
#TODO - amend as P2
figure_c3b <- ggplot(effort_strata.work[!is.na(RESIDUALS)], aes(x = RESIDUALS)) +
      geom_histogram(bins = 20) +
      geom_vline(xintercept = 0, color = "red") +
      geom_vline(aes(xintercept = mean(RESIDUALS)), lty = 2, color = "red") +
      theme_bw() +
      labs(x = "Residuals")

figure_c3 <- ggarrange(figure_c3a, figure_c3b, ncol = 1, heights = c(1, 0.75))

ggsave(filename = "output_figures/figure_c3.png",
       figure_c3,
       width = 6.5, height = 10, units = "in")

#==============================#
## Confidence intervals ----
#==============================#

# Prediction intervals?
# https://cran.r-project.org/web/packages/ciTools/vignettes/ciTools-glm-vignette.html
# But, meaningless for quasipoisson
# https://stats.stackexchange.com/questions/648734/difficulty-simulating-prediction-interval-for-quasipoisson-glm-in-r

# Can't predict prediction intervals
#pred_ints <- add_pi(df = as.data.frame(effort_strata.work[ADP < ADPyear - 1]), fit = effort_glm, names = c("lpb", "upb"), nSims = 10000, alpha = 0.05) %>%
#  add_ci(fit = effort_glm, names = c("lcb", "ucb"), alpha = 0.05)

# Confidence intervals
pred_ints <- add_ci(df = as.data.frame(effort_strata.work[ADP < ADPyear]),
                    fit = effort_glm, names = c("lcb", "ucb"), alpha = 0.05) %>%
  select(!c(TOTAL_TRIPS_PRED, RESIDUALS)) %>%
  mutate(TOTAL_TRIPS = case_when(is.na(TOTAL_TRIPS) ~ pred,
                                 TRUE ~ TOTAL_TRIPS))

# Copy current year's data and predicted values
current_yr <- pred_ints %>% filter(ADP == ADPyear - 1) %>%
  mutate(ADP = ADPyear,
         MAX_DATE_TRIPS = NA)

# Combine data
pred_trips <- rbind(pred_ints, current_yr)

# Visualize
figure_c4 <- ggplot(data = pred_trips, aes(x = ADP, y = TOTAL_TRIPS)) +
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

ggsave(filename = "output_figures/figure_c4.png", plot = figure_c4, width = 5, height = 5, units = "in")

#==============================#
## Save results ----
#==============================#

# save effort predictions
save(list = c("pred_trips", "figure_c1", "figure_c2", "figure_c3", "figure_c4"),
     file = "source_data/effort_prediction.rdata")

#==============================#
## Testing area ----
#==============================#

#'`-- Error distribution for incoporating into allocation models ------------- `
#'`-- CRAIG --------------------`

out <- predict(effort_glm, type = "link", se.fit = TRUE,
               newdata = pred_ints)

ilink <- family(effort_glm)$linkinv

#Steps for Geoff:
# TODO - is the value from se.fit the equivalent to sd in rnorm?
testing <- ilink(rnorm(1000, mean = out$fit[96], sd = out$se.fit[96]))

ggplot(data = as.data.frame(testing), aes(x = testing)) +
  geom_histogram() +
  geom_vline(xintercept = pred_ints$pred[96]) +
  geom_vline(xintercept = pred_ints$ucb[96]) +
  geom_vline(xintercept = pred_ints$lcb[96])


#'`-- Calculate CIs by hand ---------------------------------------------------`

# Manually calculate 95% Confidence Intervals
# https://fromthebottomoftheheap.net/2018/12/10/confidence-intervals-for-glms/
# Grab inverse function from model
ilink <- family(effort_glm)$linkinv

# Add fit and se to data (cannot predict future year because missing data for MAX_DATE_TRIPS)
ndata <- bind_cols(effort_strata.work[ADP <= ADPyear],
                   setNames(as_tibble(predict(effort_glm,
                                              newdata = effort_strata.work[ADP <= ADPyear],
                                              se.fit = TRUE)[1:2]),
                            c("fit_link", "se_link")))

# Based off of code used to calculate these in function investr::predFit
# https://rdrr.io/cran/investr/src/R/predFit.R
ndata <- ndata %>%
  mutate(fit_resp = ilink(fit_link),
         upr = ilink(fit_link + se_link * stats::qnorm((0.95 + 1) / 2)),
         lwr = ilink(fit_link - se_link * stats::qnorm((0.95 + 1) / 2))) %>%
  # Assign current year predicted trips as total trips
  mutate(TOTAL_TRIPS = case_when(is.na(TOTAL_TRIPS) ~ fit_resp,
                                 TRUE ~ TOTAL_TRIPS)) %>%
  select(!c(TOTAL_TRIPS_PRED, RESIDUALS, fit_link, se_link))

# Copy current year's data and predicted values
current_yr <- ndata %>% filter(ADP == ADPyear - 1) %>% mutate(ADP = ADPyear)

# Combine data sets
ndata <- ndata %>%
  filter(ADP != 2025) %>%
  rbind(current_yr)

# Visualize
ggplot(ndata[ADP >= ADPyear - 6], aes(x = ADP)) +
  geom_point(aes(y = TOTAL_TRIPS)) +
  geom_line(aes(y = TOTAL_TRIPS)) +
  geom_line(data = ndata[ADP >= ADPyear -1], aes(y = TOTAL_TRIPS), color = "red") +
  geom_point(data = ndata[ADP >= ADPyear - 1], aes(y = TOTAL_TRIPS), color = "red") +
  geom_errorbar(data = ndata[ADP == ADPyear], aes(ymin = lwr, ymax = upr), color = "red", width = 0.2) +
  geom_errorbar(data = ndata[ADP == ADPyear - 1], aes(ymin = lwr, ymax = upr), color = "red", width = 0.2) +
  geom_errorbar(data = ndata[ADP == ADPyear - 2], aes(ymin = lwr, ymax = upr), color = "blue", width = 0.2) +
  facet_wrap(vars(STRATA), scales = "free")
