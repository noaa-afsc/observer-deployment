# Setup ----

# * Get packages ----
library(data.table)
library(ggh4x)
library(scales)
library(tidyverse)

# * Get data ----
load("source_data/2024_Draft_ADP_data.rdata")
load("source_data/strata_timeliness.RData")

# * Get intermediate results ----
load("results/draft_rates_effort.rdata")
load("results/scorecard_dt.rdata")

# * Get functions ----
source("common_functions/evaluate_data_timeliness.R")
source("common_functions/evaluate_trip_variance.R")

# * Define scenarios ----
Scenarios <- CJ(Year = 2022, 
                BUDGET = c(3500000, 4500000, 5250000), 
                Stratification = c("CURRENT", "FIXED_FMP", "FMP"), 
                Allocation = c("EQUAL", "STATUS_QUO", "CWB", "PROX"))

# Evaluate data timeliness ----
data_timeliness_results <- data.table()

for(i in 1:nrow(Scenarios)){
  effort                  <- unique(work.data[TRIP_ID %in% get(paste0("pc_effort.", Scenarios[i, Stratification]))[ADP == Scenarios[i, Year], wd_TRIP_ID], .(ADP, TRIP_ID, COVERAGE_TYPE, STRATA = STRATA_NEW, AGENCY_GEAR_CODE)])
  data_timeliness_results <- rbind(data_timeliness_results, data.table(Scenarios[i], data_timeliness = evaluate_data_timeliness(effort, strata_timeliness, Scenarios[i, BUDGET], rates, Scenarios[i, Stratification], Scenarios[i, Allocation])))
}

# * Format results ----
data_timeliness_results <- data_timeliness_results[, ":=" (Category = "Days", 
                                                           BUDGET = recode(as.character(BUDGET), "3500000" = "$3.5M", "4500000" = "$4.5M", "5250000" = "$5.25M"),
                                                           Stratification = Stratification,
                                                           Allocation = factor(Allocation, levels = levels(scorecard_dt$Allocation)),
                                                           variable = "Data Timeliness",
                                                           value = data_timeliness)][,
                                                           BASELINE := rep(value[Stratification == "CURRENT" & Allocation == "EQUAL"], times = .N), by = .(BUDGET, variable)][,
                                                           DIFF := -1 * ((value - BASELINE) / BASELINE)][,
                                                           .(Category, BUDGET, Stratification, Allocation, variable, value, BASELINE, DIFF, label = formatC(value, format = "f", digits = 0))]

scorecard_dt <- rbind(scorecard_dt, data_timeliness_results)

# Evaluate trip variance ----
trip_variance_results <- data.table()

for(i in 1:nrow(Scenarios)){
  effort                <- get(paste0("pc_effort.", Scenarios[i, Stratification]))[ADP == Scenarios[i, Year]]
  trip_variance_results <- rbind(trip_variance_results, data.table(Scenarios[i], evaluate_trip_variance(effort, work.data, trips_melt, Scenarios[i, BUDGET], rates, Scenarios[i, Stratification], Scenarios[i, Allocation])[, .(variable = metric, value = cv)]))
}

trip_variance_strata_results <- data.table()
effort_wd_join <- "wd_TRIP_ID"

for(i in 1:nrow(Scenarios)){
  effort         <- get(paste0("pc_effort.", Scenarios[i, Stratification]))[ADP == Scenarios[i, Year]]
  budget         <- Scenarios[i, BUDGET]
  stratification <- Scenarios[i, Stratification]
  allocation     <- Scenarios[i, Allocation]
  
  # Rename columns
  metrics <- trips_melt[, .(TRIP_ID, METRIC = Metric, VALUE = Value)]
  
  # Convert optimization metrics to wide format
  metrics <- dcast(metrics[, .(TRIP_ID, METRIC, VALUE)], TRIP_ID ~ METRIC, value.var = "VALUE")
  
  # Get the original monitoring status of trips in the effort object
  wd <- unique(work.data[, .(TRIP_ID, OBSERVED_FLAG)])
  colnames(wd)[colnames(wd) == "TRIP_ID"] <- effort_wd_join
  effort <- wd[effort, on = c(effort_wd_join)]
  effort <- unique(effort[, c(..effort_wd_join, "ADP", "STRATUM_COL", "OBSERVED_FLAG")])
  
  # Join optimization metric values with the most recent year of trips
  colnames(metrics)[colnames(metrics) == "TRIP_ID"] <- effort_wd_join
  data <- metrics[effort, on = c(effort_wd_join)]
  
  # Total number of trips and expected number of monitored trips per stratum
  data <- unique(rates[[paste(stratification, allocation, sep = ".")]][BUDGET == budget, .(ADP, STRATUM_COL, N = STRATA_N, n)])[data, on = .(ADP, STRATUM_COL), nomatch=0]
  
  # Sum metrics by columns of interest
  data <- data[, .(chnk_psc = sum(chnk_psc), hlbt_psc = sum(hlbt_psc), discard = sum(discard), crab_psc = sum(crab_psc)), by = c(effort_wd_join, "STRATUM_COL", "OBSERVED_FLAG", "N", "n")]
  
  # Calculate trip-level mean and variance of monitored trips
  mean <- data[, lapply(.SD, function(x) mean(x[OBSERVED_FLAG=="Y"], na.rm = TRUE)), .SDcols = c("chnk_psc", "hlbt_psc", "discard", "crab_psc"), keyby = c("STRATUM_COL", "N", "n")]
  var <- data[, lapply(.SD, function(x) var(x[OBSERVED_FLAG=="Y"], na.rm = TRUE)), .SDcols = c("chnk_psc", "hlbt_psc", "discard", "crab_psc"), keyby = c("STRATUM_COL", "N", "n")]
  
  mean <- melt(mean, id.vars = c("STRATUM_COL", "N", "n"), variable.name = "metric", value.name = "mean")
  var <- melt(var, id.vars = c("STRATUM_COL", "N", "n"), variable.name = "metric", value.name = "var")
  
  data <- merge(mean, var, by = c("STRATUM_COL", "N", "n", "metric"))
  
  # Estimate the stratum-level mean and variance  
  data[, mean := N * mean]
  data[, var := N^2 * (N-n)/N * 1/n * var]
  
  # Calculate CV
  data[, cv := ifelse(mean > 0, round(sqrt(var) / mean, 4), 0)]
  
  # Append results
  trip_variance_strata_results <- rbind(trip_variance_strata_results, cbind(Scenarios[i], data[, .(STRATUM_COL, variable = metric, value = cv)]))
}

# * Format results ----
trip_variance_results <- trip_variance_results[, ":=" (Category = "Trip-Level\nVariance (CV)", 
                                                       BUDGET = recode(as.character(BUDGET), "3500000" = "$3.5M", "4500000" = "$4.5M", "5250000" = "$5.25M"),
                                                       Allocation = factor(Allocation, levels = levels(scorecard_dt$Allocation)))][,
                                                       BASELINE := rep(value[Stratification == "CURRENT" & Allocation == "EQUAL"], times = .N), by = .(BUDGET, variable)][,
                                                       DIFF := -1 * ((value - BASELINE) / BASELINE)][,
                                                       .(Category, BUDGET, Stratification, Allocation, variable, value, BASELINE, DIFF, label = formatC(value * 100, format = "f", digits = 2))]                                                                    

scorecard_dt <- rbind(scorecard_dt, trip_variance_results)

# Final formatting ----

# Multiply by -1 for metrics for which larger numbers are worse
scorecard_dt[, scaled_value := value][
Category %in% c("Cost", "Days", "Trip-Level\nVariance (CV)"), scaled_value := -1 * scaled_value][, 
# Get the maximum and minimum value for each metric
':=' (worst = min(scaled_value), best = max(scaled_value)), by = .(variable)][
# Assign global values for some metrics
Category %in% c("Interspersion (AK)", "Interspersion (FMP)", "Power to Detect\nRare Events", "Trip-Level\nVariance (CV)"), ':=' (worst = min(scaled_value), best = max(scaled_value)), by = .(Category)][,
# Calculate the spread between best and worst
spread := best - worst][,                                                                                                                                                                                               
# Find the relative difference
DIFF := (scaled_value - worst) / spread][, 
# Recode columns
':=' (Category = recode(Category, "Power to Detect\nRare Events" = "Power to\nDetect"),
      variable = recode_factor(variable, "crab_psc" = "Crab PSC",
                                         "discard"  = "Discards",
                                         "hlbt_psc" = "Halibut PSC",
                                         "chnk_psc" = "Chinook PSC"),
      Allocation = recode(Allocation, "STATUS_QUO" = "STATUS\nQUO"))]

# Multiply by -1 for metrics for which larger numbers are worse
trip_variance_strata_results[, ':=' (label = formatC(value * 100, format = "f", digits = 2), scaled_value = value, Category = "Trip-Level\nVariance (CV)")][
, scaled_value := -1 * scaled_value][, 
# Get the maximum and minimum value within the category
':=' (worst = min(scaled_value), best = max(scaled_value)), by = .(Category)][,
# Calculate the spread between best and worst
spread := best - worst][,                                                                                                                                                                                               
# Find the relative difference
DIFF := (scaled_value - worst) / spread][, 
# Recode columns
':=' (BUDGET = recode(as.character(BUDGET), "3500000" = "$3.5M", "4500000" = "$4.5M", "5250000" = "$5.25M"),
variable = recode_factor(variable, "crab_psc" = "Crab PSC",
                                   "discard"  = "Discards",
                                   "hlbt_psc" = "Halibut PSC",
                                   "chnk_psc" = "Chinook PSC"),
Allocation = recode(Allocation, "STATUS_QUO" = "STATUS\nQUO"))]

# Figures ----

# * Main ----

ggplot(scorecard_dt[BUDGET == "$3.5M"], aes(x = Allocation, y = variable, fill = DIFF)) +
  facet_nested(Category ~ BUDGET + Stratification, scales = "free", space = "free", switch = "y", labeller = labeller(
    BUDGET = function(x) paste("Budget: ", x),
    Stratification = function(x) paste("Stratification: ", x))) +
  geom_tile() +
  scale_fill_gradient2(low = muted("red", l = 40), high = muted("blue", l = 40), midpoint = 0.5, breaks = c(0, 1), labels = c("Worst", "Best")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "bottom", legend.key.width = unit(1, "in")) +
  geom_text(aes(label = label), size = 3) +
  labs(fill = "", y = "Metric")

# ggsave("output_figures/evaluation_3.5.png", width = 8, height = 10, units = "in")

ggplot(scorecard_dt[BUDGET == "$4.5M"], aes(x = Allocation, y = variable, fill = DIFF)) +
  facet_nested(Category ~ BUDGET + Stratification, scales = "free", space = "free", switch = "y", labeller = labeller(
    BUDGET = function(x) paste("Budget: ", x),
    Stratification = function(x) paste("Stratification: ", x))) +
  geom_tile() +
  scale_fill_gradient2(low = muted("red", l = 40), high = muted("blue", l = 40), midpoint = 0.5, breaks = c(0, 1), labels = c("Worst", "Best")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "bottom", legend.key.width = unit(1, "in")) +
  geom_text(aes(label = label), size = 3) +
  labs(fill = "", y = "Metric")

# ggsave("output_figures/evaluation_4.5.png", width = 8, height = 10, units = "in")

ggplot(scorecard_dt[BUDGET == "$5.25M"], aes(x = Allocation, y = variable, fill = DIFF)) +
  facet_nested(Category ~ BUDGET + Stratification, scales = "free", space = "free", switch = "y", labeller = labeller(
    BUDGET = function(x) paste("Budget: ", x),
    Stratification = function(x) paste("Stratification: ", x))) +
  geom_tile() +
  scale_fill_gradient2(low = muted("red", l = 40), high = muted("blue", l = 40), midpoint = 0.5, breaks = c(0, 1), labels = c("Worst", "Best")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "bottom", legend.key.width = unit(1, "in")) +
  geom_text(aes(label = label), size = 3) +
  labs(fill = "", y = "Metric")

# ggsave("output_figures/evaluation_5.25.png", width = 8, height = 10, units = "in")

# * Stratum-specific ----

ggplot(trip_variance_strata_results[BUDGET == "$3.5M"], aes(x = Allocation, y = STRATUM_COL, fill = DIFF)) +
  facet_nested(Stratification ~ BUDGET + variable, scales = "free", space = "free", switch = "y", labeller = labeller(
    BUDGET = function(x) paste("Budget: ", x),
    Stratification = function(x) paste("Stratification: ", x))) +
  geom_tile() +
  scale_fill_gradient2(low = muted("red", l = 40), high = muted("blue", l = 40), midpoint = 0.5, breaks = c(0, 1), labels = c("Worst", "Best")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "bottom", legend.key.width = unit(1, "in")) +
  geom_text(aes(label = label), size = 3) +
  labs(fill = "", y = "Metric")

# ggsave("output_figures/evaluation_strata_3.5.png", width = 8, height = 10, units = "in")

ggplot(trip_variance_strata_results[BUDGET == "$4.5M"], aes(x = Allocation, y = STRATUM_COL, fill = DIFF)) +
  facet_nested(Stratification ~ BUDGET + variable, scales = "free", space = "free", switch = "y", labeller = labeller(
    BUDGET = function(x) paste("Budget: ", x),
    Stratification = function(x) paste("Stratification: ", x))) +
  geom_tile() +
  scale_fill_gradient2(low = muted("red", l = 40), high = muted("blue", l = 40), midpoint = 0.5, breaks = c(0, 1), labels = c("Worst", "Best")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "bottom", legend.key.width = unit(1, "in")) +
  geom_text(aes(label = label), size = 3) +
  labs(fill = "", y = "Metric")

# ggsave("output_figures/evaluation_strata_4.5.png", width = 8, height = 10, units = "in")

ggplot(trip_variance_strata_results[BUDGET == "$5.25M"], aes(x = Allocation, y = STRATUM_COL, fill = DIFF)) +
  facet_nested(Stratification ~ BUDGET + variable, scales = "free", space = "free", switch = "y", labeller = labeller(
    BUDGET = function(x) paste("Budget: ", x),
    Stratification = function(x) paste("Stratification: ", x))) +
  geom_tile() +
  scale_fill_gradient2(low = muted("red", l = 40), high = muted("blue", l = 40), midpoint = 0.5, breaks = c(0, 1), labels = c("Worst", "Best")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "bottom", legend.key.width = unit(1, "in")) +
  geom_text(aes(label = label), size = 3) +
  labs(fill = "", y = "Metric")

# ("output_figures/evaluation_strata_5.25.png", width = 8, height = 10, units = "in")
