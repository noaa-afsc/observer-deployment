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

# * Wrangle strata_timeliness ----
strata_timeliness <- strata_timeliness[, ":=" (STRATA = recode(STRATA, "EM_TRW_EFP" = "EM_TRW", "HAL" = "OB_HAL", "POT" = "OB_POT", "TRW" = "OB_TRW"))][COVERAGE_TYPE == "PARTIAL", .(COVERAGE_TYPE, STRATA, mean_data_timeliness)]

# * Join strata_timeliness to partial coverage effort
evaluate_data_timeliness(pc_effort.CURRENT[, COVERAGE_TYPE := "PARTIAL"], strata_timeliness)

