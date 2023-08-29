# Setup ----

# * Get packages ----
library(data.table)
library(ggh4x)
library(tidyverse)

# * Get data ----
load("source_data/2024_Draft_ADP_data.rdata")
load("source_data/strata_timeliness.RData")

# * Get intermediate results ----
load("results/draft_rates_effort.rdata")
load("results/scorecard_dt.rdata")

# * Get functions ----
source("common_functions/evaluate_data_timeliness.R")

# Evaluate data timeliness ----
year                    <- 2022
data_timeliness_results <- data.table()

# * $3.5M budget ----
BUDGET <- 3500000

# * * Current stratification ----
Stratification <- "CURRENT"
data           <- unique(work.data[TRIP_ID %in% pc_effort.CURRENT[ADP == year, wd_TRIP_ID], .(ADP, TRIP_ID, COVERAGE_TYPE, STRATA = STRATA_NEW, AGENCY_GEAR_CODE)])

Allocation <- "EQUAL";      data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "STATUS_QUO"; data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "CWB";        data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "PROX";       data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))

# * * Fixed FMP stratification ----
Stratification <- "FIXED_FMP"
data           <- unique(work.data[TRIP_ID %in% pc_effort.FIXED_FMP[ADP == year, wd_TRIP_ID], .(ADP, TRIP_ID, COVERAGE_TYPE, STRATA = STRATA_NEW, AGENCY_GEAR_CODE)])

Allocation <- "EQUAL";      data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "STATUS_QUO"; data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "CWB";        data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "PROX";       data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))

# * * FMP stratification ----
Stratification <- "FMP"
data           <- unique(work.data[TRIP_ID %in% pc_effort.FMP[ADP == year, wd_TRIP_ID], .(ADP, TRIP_ID, COVERAGE_TYPE, STRATA = STRATA_NEW, AGENCY_GEAR_CODE)])

Allocation <- "EQUAL";      data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "STATUS_QUO"; data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "CWB";        data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "PROX";       data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))

# * $4.5M budget ----
BUDGET <- 4500000

# * * Current stratification ----
Stratification <- "CURRENT"
data           <- unique(work.data[TRIP_ID %in% pc_effort.CURRENT[ADP == year, wd_TRIP_ID], .(ADP, TRIP_ID, COVERAGE_TYPE, STRATA = STRATA_NEW, AGENCY_GEAR_CODE)])

Allocation <- "EQUAL";      data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "STATUS_QUO"; data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "CWB";        data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "PROX";       data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))

# * * Fixed FMP stratification ----
Stratification <- "FIXED_FMP"
data           <- unique(work.data[TRIP_ID %in% pc_effort.FIXED_FMP[ADP == year, wd_TRIP_ID], .(ADP, TRIP_ID, COVERAGE_TYPE, STRATA = STRATA_NEW, AGENCY_GEAR_CODE)])

Allocation <- "EQUAL";      data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "STATUS_QUO"; data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "CWB";        data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "PROX";       data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))

# * * FMP stratification ----
Stratification <- "FMP"
data           <- unique(work.data[TRIP_ID %in% pc_effort.FMP[ADP == year, wd_TRIP_ID], .(ADP, TRIP_ID, COVERAGE_TYPE, STRATA = STRATA_NEW, AGENCY_GEAR_CODE)])

Allocation <- "EQUAL";      data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "STATUS_QUO"; data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "CWB";        data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "PROX";       data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))

# * $5.25M budget ----
BUDGET <- 5250000

# * * Current stratification ----
Stratification <- "CURRENT"
data           <- unique(work.data[TRIP_ID %in% pc_effort.CURRENT[ADP == year, wd_TRIP_ID], .(ADP, TRIP_ID, COVERAGE_TYPE, STRATA = STRATA_NEW, AGENCY_GEAR_CODE)])

Allocation <- "EQUAL";      data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "STATUS_QUO"; data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "CWB";        data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "PROX";       data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))

# * * Fixed FMP stratification ----
Stratification <- "FIXED_FMP"
data           <- unique(work.data[TRIP_ID %in% pc_effort.FIXED_FMP[ADP == year, wd_TRIP_ID], .(ADP, TRIP_ID, COVERAGE_TYPE, STRATA = STRATA_NEW, AGENCY_GEAR_CODE)])

Allocation <- "EQUAL";      data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "STATUS_QUO"; data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "CWB";        data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "PROX";       data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))

# * * FMP stratification ----
Stratification <- "FMP"
data           <- unique(work.data[TRIP_ID %in% pc_effort.FMP[ADP == year, wd_TRIP_ID], .(ADP, TRIP_ID, COVERAGE_TYPE, STRATA = STRATA_NEW, AGENCY_GEAR_CODE)])

Allocation <- "EQUAL";      data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "STATUS_QUO"; data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "CWB";        data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))
Allocation <- "PROX";       data_timeliness_results <- rbind(data_timeliness_results, data.table(BUDGET, year, Stratification, Allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, BUDGET, rates, Stratification, Allocation)))

# * Format results ----
data_timeliness_results <- data_timeliness_results[, ":=" (Category = "Data timeliness", 
                                                           BUDGET = recode(as.character(BUDGET), "3500000" = "$3.5M", "4500000" = "$4.5M", "5250000" = "$5.25M"),
                                                           Stratification = Stratification,
                                                           Allocation = factor(Allocation, levels = levels(scorecard_dt$Allocation)),
                                                           variable = "Data timeliness",
                                                           value = data_timeliness)][,
                                                           BASELINE := rep(value[Stratification == "CURRENT" & Allocation == "EQUAL"], times = .N), by = .(BUDGET, variable)][,
                                                           DIFF := -1 * ((value - BASELINE) / BASELINE)][,
                                                           .(Category, BUDGET, Stratification, Allocation, variable, value, BASELINE, DIFF)]

scorecard_dt <- rbind(scorecard_dt, data_timeliness_results)

ggplot(scorecard_dt[BUDGET == "$4.5M"], aes(x = Allocation, y = variable, fill = DIFF)) +
  facet_nested(Category ~ BUDGET + Stratification, scales = "free", space = "free", switch = "y", labeller = labeller(
    BUDGET = function(x) paste("Budget: ", x),
    Stratification = function(x) paste("Stratification: ", x))) +
  geom_tile() +
  scale_fill_gradient2() +
  #scale_fill_gradient2(trans = signed_sqrt) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "bottom") +
  geom_text(aes(label = formatC(value, digits = 3, format = "f")), size = 2) +
  labs(fill = "Relative Benefit", y = "Metric")
