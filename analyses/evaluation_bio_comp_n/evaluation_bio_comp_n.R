# 2024 Draft ADP : Evaluation - [Biological And Composition Samples]

# Author: Geoff Mayhew
# Start Date: 24 Aug 2024

#======================================================================================================================#
# Preparation ----------------------------------------------------------------------------------------------------------
#======================================================================================================================#

#===================#
## Load Packages ----
#===================#

library(data.table)         # Data wrangling
library(ggplot2)            # Plotting
library(scales)             # for defining new scales
library(ggh4x)              # for nested facets
library(grid)               # For unit.pmax  to get widths of grobs so that plots have matching dimensions
library(gridExtra)          # For arrangeGrob to combine plots
library(sf)                 # Spatial analyses
library(flextable)          # For print-ready tables
library(dplyr)              # For piping and handling sf objects

#=============================#
## Load data and data prep ----
#=============================#

load("analyses/draft_rates/draft_rates_2.rdata")           # Raw output from nalyses/draft_rates/draft_rates.R - box definitions and raw rates
load("analyses/draft_rates/draft_rates_effort_2.rdata")    # Outputs from analyses/draft_rates/draft_rates.R - compiled rates and effort

#===============#
## Constants ----
#===============#

allo_vec <- c("EQUAL", "STATUS_QUO", "CWB", "PROX")
strat_vec <- c("CURRENT", "FMP", "FIXED_FMP")

#=====================#
# Data Collection  ####
#=====================#

# Bio_n = Trips/offloads with biological collections 
#   Selected trips with at-sea observers ( STRATA %like% "OB|EM_TRW")
#   Selected offloads with shoreside observers

# Comp_n =Trips/offloads with catch composition
#   Selected trips with at-sea observers
#   Selected trips with fixed gear EM
#   All Trawl EM trips.

# Add 'STRATA' back in, as the cost functions need them (and not stratum_col)
rates <- lapply(rates, function(x0) {
  # x0 <- rates[[12]]
  x1 <- copy(x0)
  x1[, STRATA := gsub("[-].*$", "", STRATUM_COL)]
  x1[, RATE := SAMPLE_RATE]
  x1[, FILL := fcase(
    STRATA %like% "OB", "OB", 
    STRATA %like% "EM_TRW", "EM_TRW",
    STRATA %like% "EM_HAL|EM_POT|EM_FIXED", "EM_FG")]
  if( any(x1$STRATUM_COL %like% "BSAI|GOA")) x1[, BSAI_GOA := gsub("^.*[-]", "", STRATUM_COL)]
  x1
})

data_bio_n <- cbind(Category = "Samples: Biological", rbindlist(lapply(rates, function(x) {
  x[, .(
    OB_at_sea_n = sum(n[STRATA %like% "OB_"]),
    EM_TRW_shoreside_n = sum(n[STRATA %like% "EM_TRW"]),
    Bio_total_n = sum(n[STRATA %like% "OB_"]) + sum(n[STRATA %like% "EM_TRW"])),
    keyby = .(ADP, BUDGET)]
}), idcol = "DESIGN"))

data_comp_n <- cbind(Category = "Samples: Composition", rbindlist(lapply(rates, function(x) {
  x[, .(
    OB_at_sea_n = sum(n[STRATA %like% "OB_"]),
    EMFG_at_sea_n = sum(n[STRATA %like% "EM_HAL|EM_POT|EM_FIXED"]),
    # EMTRW_compliance_N = as.numeric(sum(STRATA_N[STRATA %like% "EM_TRW"])),
    Comp_total_n = sum(n[STRATA %like% c("OB|EM_HAL|EM_POT|EM_FIXED")])) ,  # Excluding compliance samples as it doens't differ between designs!
    keyby = .(ADP, BUDGET)]
}), idcol = "DESIGN"))

data_collection_summary <- rbind(
  melt(data_bio_n, id.vars = c("ADP", "BUDGET", "Category", "DESIGN"), value.vars = c("OB_at_sea_n", "EM_TRW_shoreside_n", "Bio_total_n")),
  melt(data_comp_n, id.vars = c("ADP", "BUDGET", "Category", "DESIGN"), value.vars = c("OB_at_sea_n", "EMFG_at_sea_n", "Comp_total_n"))
)

data_collection_summary[, Category := factor(Category, levels = c("Samples: Biological", "Samples: Composition"))]
data_collection_summary[, variable := factor(variable, levels = c("Bio_total_n", "EM_TRW_shoreside_n", "Comp_total_n", "EMFG_at_sea_n", "OB_at_sea_n"))]
data_collection_summary[, c("Stratification", "Allocation") := tstrsplit(DESIGN, split  = "[.]")][, DESIGN := NULL]
data_collection_summary[, BASELINE := rep(value[Stratification == "CURRENT" & Allocation == "EQUAL"], times = .N), by = .(ADP, BUDGET, Category, variable)]
data_collection_summary[, DIFF := (value - BASELINE) / BASELINE]  
data_collection_summary[, Allocation := factor(Allocation, levels = c("EQUAL", "STATUS_QUO", "CWB", "PROX"))]
data_collection_summary[, Stratification := factor(Stratification, levels = c("CURRENT", "FMP", "FIXED_FMP"))]
data_collection_summary[, BUDGET := paste0("$", BUDGET/1e6, "M")]
#data_collection_summary[, value := formatC(value, digits = 0, format = "f")]

ggplot(data_collection_summary[ADP == 2022], aes(x = Allocation, y = variable, fill = DIFF)) + 
  facet_nested(BUDGET + Category ~ Stratification, scales = "free", space = "free", switch = "y", labeller = labeller(
    BUDGET = function(x) paste("Budget: ", x),
    Stratification = function(x) paste("Stratification: ", x))) + 
  geom_tile() + 
  scale_fill_gradient2(limits = c(-1, 1), na.value = muted("red")) +     # This doesn't quite work! Large benefits relative to baseline would also be red!
  #scale_fill_gradient2(trans = signed_sqrt) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "bottom") + 
  geom_text(aes(label = formatC(value, digits = 0, format = "f")), size = 3) + 
  labs(fill = "Relative Benefit", y = "Metric")

# Outputs: 
scorecard_samples_bio <- data_collection_summary[ADP == 2022 & variable %like% "Bio|OB_at_sea|EM_TRW_shoreside", .(BUDGET, Stratification, Allocation, variable, value, BASELINE, DIFF)]
scorecard_samples_comp <- data_collection_summary[ADP == 2022 & variable %like% "Comp|OB_at_sea|EMFG_at_sea", .(BUDGET, Stratification, Allocation, variable, value, BASELINE, DIFF)]

save(scorecard_samples_bio, scorecard_samples_comp, file = "analyses/evaluation_bio_comp_n/scorecard_bio_comp_n.rdata")
# Saved to google drive: https://drive.google.com/file/d/1hFKoG4jiZYvMpyDfTa1LoDTOnUhhoclE/view?usp=drive_link