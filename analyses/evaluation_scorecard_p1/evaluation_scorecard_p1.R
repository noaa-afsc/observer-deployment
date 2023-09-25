# 2024 Draft ADP : Evaluation - [Scorecard Part 1]

# Author: Geoff Mayhew
# Start Date: 24 Aug 2024

# This script compiles the evaluation metrics Geoff Mayhew was working on, incorporation Craig's rare detection metrics,
# to package up and send off to Phil to make the final scorecard tables.

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

#===============#
## Constants ----
#===============#

allo_vec <- c("EQUAL", "STATUS_QUO", "CWB", "PROX")
strat_vec <- c("CURRENT", "FMP", "FIXED_FMP")

#=============================#
## Load data and data prep ----
#=============================#

load("analyses/evaluation_trips_and_costs/scorecard_expense_cv.rdata")  # scorecard_expense_cv
load("analyses/evaluation_bio_comp_n/scorecard_bio_comp_n.rdata") # scorecard_samples_bio, scorecard_samples_comp
load("analyses/evaluation_interspersion/scorecard_interspersion.rdata") # interspersion_ak, interspersion_fmp
# Load Craig's Power to detect tables
# For now, Geoff had a copy placed in the evaluation_interspersion folder as the detection folder wasn't on repo.
load("analyses/evaluation_interspersion/Power_tables.Rdata") # design_detect_table, detect_table, and effects_table     

rare_event_detection_dt <- setDT(copy(design_detect_table))
rare_event_detection_dt[
][, c("Stratification", "Allocation") := tstrsplit(Design, split = "[.]")
][, Allocation := factor(Allocation, levels = allo_vec)
][, Stratification := factor(Stratification, levels = strat_vec)
][, BUDGET := paste0("$", Budget/1e6, "M")
][, variable := Species
][, Category := "Power to Detect\nRare Events"
][, value := Power
][, DIFF := Power / (min(Power) + (max(Power) - min(Power)))
][, label := formatC(Power, digits = 2, format = "f")
][, variable := as.factor(variable)
][, BASELINE := rep(value[Stratification == "CURRENT" & Allocation == "EQUAL"], times = .N), by = .(BUDGET, variable)]

#===========================#
# Final Scorecard part 1 ####
#===========================#

# Define order of metrics
metric_vector <- c("Cost", "Samples:\nBiological", "Samples:\nComposition",  "Interspersion (AK)", "Interspersion (FMP)")    # TODO make each thing an item in a list and rbindlist() (avoids a bunch of cbinds)

# Compbine metrics objects
scorecard_dt <- rbindlist(setNames(list(
  scorecard_expense_cv,
  scorecard_samples_bio,
  scorecard_samples_comp,
  interspersion_ak,
  interspersion_fmp
), metric_vector), idcol = "Category")[, Category := factor(Category, levels = metric_vector)]

# Rename variables to be easier to understand
scorecard_dt[
][variable == "OB to EM_TRW", variable := "EM_TRW to EM_TRW"
][variable == "OB to EM_TRW\nGOA", variable := "EM_TRW to EM_TRW\nGOA"
][variable == "OB_at_sea_n", variable := "OB at sea"
][variable == "EM_TRW_shoreside_n", variable := "EM_TRW shoreside"
][variable == "Bio_total_n", variable := "Total"
][variable == "EMFG_at_sea_n", variable := "EM_FG at sea"
][variable == "Comp_total_n", variable := "Total"]

# Set factor levels
ispn_method_levels <- c("EM_FIXED to EM_FIXED", "EM_TRW to EM_TRW", "OB to ZERO", "OB to EM_FIXED", "OB to OB")
ispn_method_levels_fmp <- c(rbind(sapply(c("EM_FIXED to EM_FIXED", "EM_TRW to EM_TRW", "OB to ZERO", "OB to EM_FIXED", "OB to OB"), function(x) paste0(x, "\n", c("BSAI", "GOA")))))
variable_levels <- c("CV", "Total", "EM_FG at sea", "EM_TRW shoreside", "OB at sea", ispn_method_levels, ispn_method_levels_fmp)
setdiff(scorecard_dt$variable, variable_levels)
scorecard_dt[, variable := factor(variable, levels = variable_levels)]

# Apply formats to labels
scorecard_dt[
][Category == "Cost", label := formatC(value, digits = 2, format = "f")
][Category == "Samples:\nBiological", label := formatC(value, digits = 0, format = "f")
][Category == "Samples:\nComposition", label := formatC(value, digits = 0, format = "f")
][Category %like% "Interspersion", label := formatC(value, digits = 3, format = "f")] 

# Add rare event detection
scorecard_dt <- rbind(
  scorecard_dt,
  rare_event_detection_dt[, .(Category, BUDGET, Stratification, Allocation, variable, value, BASELINE, DIFF, label)]
)

# save(scorecard_dt, file = "analyses/evaluation_scorecard_p1/scorecard_dt.rdata")
# Saved to google drive Draft ADP Outputs Folder: https://drive.google.com/file/d/1Een7cUVuGiCMjMlkedY1NUXW3tCRbsa7/view?usp=drive_link