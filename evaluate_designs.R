# Setup ----

# * Get packages ----
library(data.table)
library(tidyverse)

# * Get data ----
load("source_data/2024_Draft_ADP_data.rdata")
load("source_data/strata_timeliness.RData")

# * Get intermediate results ----
load("results/draft_rates_effort.rdata")

# * Get functions ----
source("common_functions/evaluate_data_timeliness.R")

# Evaluate data timeliness ----
year                    <- 2022
data_timeliness_results <- data.table()

# * $3.5M budget ----
budget <- 3500000

# * * Current stratification ----
stratification <- "CURRENT"
data           <- unique(work.data[TRIP_ID %in% pc_effort.CURRENT[ADP == year, wd_TRIP_ID], .(ADP, TRIP_ID, COVERAGE_TYPE, STRATA = STRATA_NEW, AGENCY_GEAR_CODE)])

allocation <- "EQUAL";      data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "STATUS_QUO"; data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "CWB";        data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "PROX";       data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))

# * * Fixed FMP stratification ----
stratification <- "FIXED_FMP"
data           <- unique(work.data[TRIP_ID %in% pc_effort.FIXED_FMP[ADP == year, wd_TRIP_ID], .(ADP, TRIP_ID, COVERAGE_TYPE, STRATA = STRATA_NEW, AGENCY_GEAR_CODE)])

allocation <- "EQUAL";      data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "STATUS_QUO"; data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "CWB";        data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "PROX";       data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))

# * * FMP stratification ----
stratification <- "FMP"
data           <- unique(work.data[TRIP_ID %in% pc_effort.FMP[ADP == year, wd_TRIP_ID], .(ADP, TRIP_ID, COVERAGE_TYPE, STRATA = STRATA_NEW, AGENCY_GEAR_CODE)])

allocation <- "EQUAL";      data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "STATUS_QUO"; data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "CWB";        data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "PROX";       data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))

# * $4.5M budget ----
budget <- 4500000

# * * Current stratification ----
stratification <- "CURRENT"
data           <- unique(work.data[TRIP_ID %in% pc_effort.CURRENT[ADP == year, wd_TRIP_ID], .(ADP, TRIP_ID, COVERAGE_TYPE, STRATA = STRATA_NEW, AGENCY_GEAR_CODE)])

allocation <- "EQUAL";      data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "STATUS_QUO"; data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "CWB";        data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "PROX";       data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))

# * * Fixed FMP stratification ----
stratification <- "FIXED_FMP"
data           <- unique(work.data[TRIP_ID %in% pc_effort.FIXED_FMP[ADP == year, wd_TRIP_ID], .(ADP, TRIP_ID, COVERAGE_TYPE, STRATA = STRATA_NEW, AGENCY_GEAR_CODE)])

allocation <- "EQUAL";      data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "STATUS_QUO"; data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "CWB";        data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "PROX";       data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))

# * * FMP stratification ----
stratification <- "FMP"
data           <- unique(work.data[TRIP_ID %in% pc_effort.FMP[ADP == year, wd_TRIP_ID], .(ADP, TRIP_ID, COVERAGE_TYPE, STRATA = STRATA_NEW, AGENCY_GEAR_CODE)])

allocation <- "EQUAL";      data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "STATUS_QUO"; data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "CWB";        data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "PROX";       data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))

# * $5.25M budget ----
budget <- 5250000

# * * Current stratification ----
stratification <- "CURRENT"
data           <- unique(work.data[TRIP_ID %in% pc_effort.CURRENT[ADP == year, wd_TRIP_ID], .(ADP, TRIP_ID, COVERAGE_TYPE, STRATA = STRATA_NEW, AGENCY_GEAR_CODE)])

allocation <- "EQUAL";      data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "STATUS_QUO"; data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "CWB";        data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "PROX";       data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))

# * * Fixed FMP stratification ----
stratification <- "FIXED_FMP"
data           <- unique(work.data[TRIP_ID %in% pc_effort.FIXED_FMP[ADP == year, wd_TRIP_ID], .(ADP, TRIP_ID, COVERAGE_TYPE, STRATA = STRATA_NEW, AGENCY_GEAR_CODE)])

allocation <- "EQUAL";      data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "STATUS_QUO"; data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "CWB";        data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "PROX";       data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))

# * * FMP stratification ----
stratification <- "FMP"
data           <- unique(work.data[TRIP_ID %in% pc_effort.FMP[ADP == year, wd_TRIP_ID], .(ADP, TRIP_ID, COVERAGE_TYPE, STRATA = STRATA_NEW, AGENCY_GEAR_CODE)])

allocation <- "EQUAL";      data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "STATUS_QUO"; data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "CWB";        data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
allocation <- "PROX";       data_timeliness_results <- rbind(data_timeliness_results, data.table(budget, year, stratification, allocation, data_timeliness = evaluate_data_timeliness(data, strata_timeliness, budget, rates, stratification, allocation)))
