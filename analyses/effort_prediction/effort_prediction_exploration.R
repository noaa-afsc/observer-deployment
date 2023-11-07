# effort_prediction_exploration.R
# Author: Geoff Mayhew
# Start Date: 2023-Oct-11
# Revised Date:

# This script is a quick visualization of how partial coverage fishing effort has changed in terms of numbers of trips
# between 2015 and 2022. Its purpose is to fuel discussion on effort predictions for the ADPs and whether/how it can 
# be improved.

# It is the precursor to two RMD scripts: effort_prediction_exploration.Rmd and effort_bootstrapping.Rmd

library(data.table)
library(ggplot2)
library(ggh4x)      # For facet_nested()
library(grid)       # For combining plots
library(gridExtra)  # For combining plots
library(flextable)  # For print-ready tables
library(magrittr)   # For piping

#======================================================================================================================#
# Load Data ----
#======================================================================================================================#

# Load pc_effort_dt (Valhalla transformed to 2023 stratum definitions, 2015 to 2023)
load("~/GitHub/2024_ADP/analyses/allocation_evaluation/data_prep_final.Rdata")

# Load the ADFG statistical area shapefile. '../' is needed for Rmarkdown to climb into parent folders.
stat_area_sf <- st_read(
  dsn = "source_data/ADFG_Stat_Area_shapefile/PVG_Statewide_2001_Present_GCS_WGS1984.shp", quiet = T) %>%
  select(STAT_AREA) %>%
  st_transform(crs = 3467)

# Load the Alaska map sf objects
load("source_data/ak_shp.rdata")      # shp_land, shp_nmfs, and shp_centroids added to global

#======================================================================================================================#
# Functions ----
#======================================================================================================================#

# Load 2024 ADP allocation/stratification functions
source("analyses/allocation_evaluation/functions.R")

# This function makes similar plots have the same widths so they look clean when switching tabsets
unify_plot_widths <- function(plot_lst) {
  plot_grobs_lst <- lapply(plot_lst, ggplotGrob)
  figs_widths_lst <- lapply(plot_grobs_lst, function(x) x[["widths"]])
  figs_width_pmax <- do.call(unit.pmax, figs_widths_lst)
  for(i in 1:length(plot_grobs_lst)) plot_grobs_lst[[i]]$widths <- figs_width_pmax 
  # Return list of grobs with unified widths
  plot_grobs_lst
}

#======================================================================================================================#
# Data Prep ----
#======================================================================================================================#

pc_effort_2015_2023 <- pc_effort_dt[!is.na(TARGET) & AGENCY_GEAR_CODE != "JIG", .(ADP, TRIP_ID, TARGET, STRATA, BSAI_GOA, GEAR)]
pc_effort_2015_2023  <- unique(pc_effort_2015_2023 [BSAI_GOA %in% c("BS", "AI"), BSAI_GOA := "BSAI"])
# Some trips have more than one GEAR and TRIP_TARGET
# Group up trip targets, putting all non S, I, C, and P into the 'other' category, 'O'
pc_effort_2015_2023 [, TARGET_2 := fcase(
  TARGET == "B", "P",
  TARGET %in% c("W", "H", "X", "K", "O", "A", "Y", "L"), "O",
  TARGET %in% c("S", "I", "C", "P"), TARGET)]
# Define monitoring method as OB or EM
pc_effort_2015_2023[, MON := fcase(STRATA %like% "EM", "EM", STRATA %like% "OB", "OB", STRATA %like% "ZERO", "ZERO")]

#======================================================================================================================#
# Plots by Gear and FMP, without and with Target ----
#======================================================================================================================#

# Total Effort
pc_effort_2015_2023_count_prop_total <- pc_effort_2015_2023[, .(COUNT = as.numeric(uniqueN(TRIP_ID))), keyby = .(ADP, MON, GEAR, BSAI_GOA, TARGET_2)]
pc_effort_2015_2023_count_prop_total <- pc_effort_2015_2023_count_prop_total[, PROP := COUNT / sum(COUNT), keyby = .(ADP, MON, BSAI_GOA)]
pc_effort_2015_2023_count_prop_total <- melt(pc_effort_2015_2023_count_prop_total, id.vars = c("ADP", "MON", "GEAR", "BSAI_GOA", "TARGET_2"), measure.vars = c("COUNT", "PROP"))

# Gear type only. Proportions by gear type has definitely changed over time.
# in 2016, OB TRW is high, big Cod year.
total_gear <- ggplot(pc_effort_2015_2023_count_prop_total, aes(x = ADP, y = value, fill = GEAR)) + 
  geom_col() +
  facet_nested(variable ~ BSAI_GOA + MON, scales = "free_y") + 
  labs(x = "Year", y = "Value", fill = "Gear") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# So the question is, can we reliably predict changes in counts of fishing trips year to year better than just using
# the previous year's counts? How would we be able to predict inflection points?

# Adding Target as fill color, gear type is line color
total_gear_target <- ggplot(pc_effort_2015_2023_count_prop_total, aes(x = ADP, y = value, color = GEAR, fill = TARGET_2)) + 
  geom_col(size = 1) +
  scale_fill_viridis_d() + 
  facet_nested(variable ~ BSAI_GOA + MON, scales = "free_y") + 
  labs(x = "Year", y = "Value", color = "Gear", fill = "Target") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Split by trip target
pc_effort_2015_2023_count_prop <- pc_effort_2015_2023[, .(COUNT = as.numeric(uniqueN(TRIP_ID))), keyby = .(ADP, MON, GEAR, BSAI_GOA, TARGET_2)]
pc_effort_2015_2023_count_prop[, PROP := COUNT / sum(COUNT), keyby = .(ADP, MON, GEAR, BSAI_GOA)]
pc_effort_2015_2023_count_prop <- melt(pc_effort_2015_2023_count_prop, id.vars = c("ADP", "MON", "GEAR", "BSAI_GOA", "TARGET_2"), measure.vars = c("COUNT", "PROP"))
split_gear <- ggplot(pc_effort_2015_2023_count_prop, aes(x = ADP, y = value, fill = GEAR)) + 
  geom_col(size = 1) +
  facet_nested(variable + GEAR ~ BSAI_GOA + MON, scales = "free_y") + 
  labs(x = "Year", y = "Value", fill = "Gear") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Again, include target as a fill color
split_gear_target <- ggplot(pc_effort_2015_2023_count_prop, aes(x = ADP, y = value, fill = TARGET_2)) + 
  geom_col(size = 1) +
  scale_fill_viridis_d() + 
  facet_nested(variable + GEAR ~ BSAI_GOA + MON, scales = "free_y") + 
  labs(x = "Year", y = "Value", fill = "Target") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Combine plots and unify widths
effort_prediction_exploration_plots <- unify_plot_widths(list(
  GEAR = total_gear, 
  GEAR_TARGET = total_gear_target,
  SPLIT_GEAR = split_gear,
  SPLIT_GEAR_TARGET = split_gear_target))

#grid.arrange(effort_prediction_exploration_plots$GEAR)
#grid.arrange(effort_prediction_exploration_plots$GEAR_TARGET)
#grid.arrange(effort_prediction_exploration_plots$SPLIT_GEAR)
#grid.arrange(effort_prediction_exploration_plots$SPLIT_GEAR_TARGET)

# In the BSAI, most fishing effort via ZERO-HAL, of monitored catch, it's OB-POT gear. Since 2020, ZERO also has POT trips.
# Overall the proportions between fixed-gear EM and OB are very similar between years!
# In recent years, count of HAL has decreased whereas POT has increased
# In Recent years, count of TRW has decreased
# Within HAL, proportions of targets has changed little. Within POT, switched from C to S. Within OB_TRW, no more O.

# Stratify by new stratification definitions? Could also stratify by gear type and monitoring method?

# val_2015_2023_dt <- pc_effort_dt[ADP %in% 2015:2023]
# val_2015_2023_dt[BSAI_GOA %in% c("BS", "AI"), BSAI_GOA := "BSAI"]
# val_2015_2023_dt <- unique(val_2015_2023_dt)

#===============================#
## Prepare trips_melt object ----
#===============================#

# # trips_melt has metrics and trip duration for all trips >= 2015. . 
# # Add ADP year, STRATA, and DAYS of each TRIP_ID to trips_melt
# trips_melt_add_data <- unique(pc_effort_dt[STRATA != "ZERO", .(ADP, TRIP_ID, STRATA, DAYS)])
# trips_melt <- trips_melt_add_data[trips_melt, on = .(TRIP_ID)]
# setcolorder(trips_melt, c("ADP", "STRATA", "TRIP_ID", "DAYS", "Metric", "Value"))
# setkey(trips_melt, ADP, STRATA, TRIP_ID, DAYS, Metric, Value)
# # Error-check
# if(nrow(trips_melt[, .N, by = .(TRIP_ID)][N != uniqueN(trips_melt$Metric)])) stop("At least one trip has number of metrics != 3!")
# # Remove records missing data
# trips_melt <- trips_melt[!is.na(ADP)]

#=================================#
## Stratification: GEAR x FMP ----
#=================================#

# # Define boxes, splitting trips into strata based on FMP: BSAI and GOA
# 
# system.time(fmp.box <- define_boxes_gs(
#   val_2015_2023_dt, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
#   year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), geom = T, ps_cols = c("GEAR") ))
# # Re-calculate trip durations
# #fmp.allo_lst <- update_strata(val_2015_2023_dt, trips_melt, stratum_cols = c("STRATA", "BSAI_GOA"), focus_years = 2015:2023)
# 
# # Assume a 15% sample rate for all strata when calculating interspersion
# fmp.prox_insp <- calculate_interspersion_gs(fmp.box, sample_rate_vec = 0.15)
# fmp.prox_insp$ispn_dt[ADP == 2022]
# fmp.prox_insp$strata_n_dt[ADP == 2022]
# 
# # If you want to break it into months, kind of need to feed it smaller sections
# # For now, focus on 2022 and see if you can create monthly totals? Might need to apply smoother?
# test_2022 <- fmp.box$dt_out[ADP == 2022]  # 50 weeks in 2022
# test_2022[, MONTH := floor((TIME - 1)/4)]


#============================================================
# How has interspersion changed with STRATA_N each year? ####
#============================================================

# Define boxes again split by FMP, but make neighboring gear-specific for the fixed-gear strata
# system.time(fixed_fmp.box <- define_boxes_gs(
#   val_2015_2023_dt, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
#   year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), geom = T, ps_cols = c("GEAR")))
# fixed_fmp.allo_lst <- update_strata(val_2015_2023_dt, trips_melt, stratum_cols = c("STRATA", "BSAI_GOA"), focus_years = 2015:2023)

# fixed_fmp.prox_insp <- calculate_interspersion_gs(fixed_fmp.box, sample_rate_vec = 0.15)
# #fixed_fmp.prox_insp$ispn_dt[, `ISPN/STRATA_N` := ISPN / STRATA_N ]
# #fixed_fmp.prox_insp$ispn_dt[, `STRATA_N/ISPN` := STRATA_N / ISPN ]
# #fixed_fmp.prox_insp$ispn_dt[, `logSTRATA_N/ISPN` := log(STRATA_N) / ISPN ]
# fixed_fmp.prox_insp$ispn_dt[, `ISPN/logSTRATA_N` := ISPN / log(STRATA_N) ]

#fixed_fmp_plot_data <- melt(fixed_fmp.prox_insp$ispn_dt, id.vars = c("ADP", "STRATA", "BSAI_GOA"), measure.vars = c("ISPN", "STRATA_N", "ISPN/STRATA_N", "STRATA_N/ISPN", "logSTRATA_N/ISPN", "ISPN/logSTRATA_N"))
# fixed_fmp_plot_data <- melt(fixed_fmp.prox_insp$ispn_dt, id.vars = c("ADP", "STRATA", "BSAI_GOA"), measure.vars = c("ISPN", "STRATA_N", "ISPN/logSTRATA_N"))

# TODO NOTE THAT THIS IS NOT INTERSPERSION, IT'S THE 'PROXIMITY INDEX'. I HAVE TO FEED THE RATES TO EVALUATION INTERSPERSION THING AS WELL...
# THEREFORE IT IS NOT GEAR SPECIFIC!

# ggplot(fixed_fmp_plot_data, aes(x = ADP, y = value)) + 
#   facet_nested(variable ~ STRATA + BSAI_GOA, scales = "free_y") + 
#   geom_line()
# # I don't really know what this is telling me...
# # OB_POT_GOA is trending upwards
# 
# # overall, proximity increases with STRATA_N in a logarithmic (or square root) relationship
# ggplot(fixed_fmp.prox_insp$ispn_dt, aes(x = STRATA_N, y = ISPN, color = STRATA, shape = BSAI_GOA, linetype = BSAI_GOA)) + geom_point(size = 3, stroke = 1)  + 
#   geom_path(aes(group = interaction(STRATA, BSAI_GOA))) + 
#   scale_shape_manual(values = c(BSAI = 16, GOA = 2)) + 
#   labs(x = "# Trips in Stratum", y = "Proximity Index", shape = "FMP", linetype = "FMP", color = "Stratum")
# 
# # Interestingly, ISPN goes up faster in the BSAI than the GOA as STRATA_N increases. This tells me that fishing is generally more clumped in time and space
# # in the BSAI than in the GOA (which admittedly is much larger in space)
# ggplot(fixed_fmp.prox_insp$ispn_dt, aes(x = STRATA_N, y = ISPN, color = STRATA, shape = BSAI_GOA, linetype = BSAI_GOA)) + geom_point(size = 3, stroke = 1) + scale_shape_manual(values = c(BSAI = 16, GOA = 2)) + 
#   facet_grid(BSAI_GOA ~ STRATA, scales = "free") + geom_path(aes(group = interaction(STRATA, BSAI_GOA))) + 
#   labs(x = "# Trips in Stratum", y = "Proximity Index", shape = "FMP", linetype = "FMP", color = "Stratum")
# 
# # Here we can see how the values changed year to year, and whether a trend exists
# ggplot(fixed_fmp.prox_insp$ispn_dt, aes(x = STRATA_N, y = ISPN, color = ADP, shape = BSAI_GOA)) + geom_point(size = 3, stroke = 1) + scale_shape_manual(values = c(BSAI = 16, GOA = 2)) + 
#   facet_grid(BSAI_GOA ~ STRATA, scales = "free") + scale_color_viridis_c(direction = -1)  + geom_path(aes(group = interaction(STRATA, BSAI_GOA))) + 
#   labs(x = "# Trips in Stratum", y = "Proximity Index", shape = "FMP", color = "Year")
# 
# 



#======================================================================================================================#
# RESTARTING ----
#======================================================================================================================#

val_2015_2023_dt <- pc_effort_dt[ADP %in% 2015:2023]
val_2015_2023_dt[BSAI_GOA %in% c("BS", "AI"), BSAI_GOA := "BSAI"]
val_2015_2023_dt <- unique(val_2015_2023_dt)

sample_rate_vec <- seq(from = 0.0001, to = 0.9950, by = 0.0001)
sample_rate_15 <- 0.15

#===============================#
## Prepare trips_melt object ----
#===============================#

# trips_melt has metrics and trip duration for all trips >= 2015. . 
# Add ADP year, STRATA, and DAYS of each TRIP_ID to trips_melt
trips_melt_add_data <- unique(pc_effort_dt[STRATA != "ZERO", .(ADP, TRIP_ID, STRATA, DAYS)])
trips_melt <- trips_melt_add_data[trips_melt, on = .(TRIP_ID)]
setcolorder(trips_melt, c("ADP", "STRATA", "TRIP_ID", "DAYS", "Metric", "Value"))
setkey(trips_melt, ADP, STRATA, TRIP_ID, DAYS, Metric, Value)
# Error-check
if(nrow(trips_melt[, .N, by = .(TRIP_ID)][N != uniqueN(trips_melt$Metric)])) stop("At least one trip has number of metrics != 3!")
# Remove records missing data
trips_melt <- trips_melt[!is.na(ADP)]


#======================================#
## Prepare fixed_fmp fishing effort ----
#======================================#

# Combine HAL and POT stratum trips into FIXED strata
fixed.val_2015_2023_dt <- unique(copy(val_2015_2023_dt)[STRATA %like% "HAL|POT", STRATA := paste0(POOL, "_", "FIXED")])

# Define Boxes
fixed_fmp.box <- define_boxes_gs(
  fixed.val_2015_2023_dt, space = c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), dmn_cols = c("GEAR"), geom = T, ps_cols = c("GEAR"))
# Recalculate trip durations of each stratum
fixed_fmp.allo_lst <- update_strata(fixed.val_2015_2023_dt, trips_melt, stratum_cols = c("STRATA", "BSAI_GOA"), focus_years = 2015:2023)

# Calculate monitoring costs

cost_params <- list(
  
  OB = list(
    day_rate_intercept          = 1870.4466666667,    # To calculate the sea day cost
    day_rate_slope              =   -0.2263733333,    # To calculate the sea day cost
    travel_day_rate             =  423.7867560407     # Expected cost of travel per day
  ),
  
  EMFG = list(
    emfg_v                      =  172,               # Size of fixed-gear EM vessel pool
    cost_per_vessel             = 5679.9002264045,
    cost_per_review_day         =  150.3237858616
  ),
  
  EMTRW = list(
    emtrw_goa_v                 =   39,               # Number of EM_TRW vessels that fish exclusively in the GOA. Current count is 39 for 2024.
    trip_to_plant_factor        =    3.8059361492,    # Used to predict plant days from trips
    amortized_equipment_per_VY  = 4100.7124800000,    # Per (Vessel x Year) amortized EM equipment install costs for GOA-only vessels
    equipment_upkeep_per_VY     = 4746.0398955882,    # Per (Vessel x Year) EM equipment maintenance cost for GOA-only vessels
    review_day_rate             =   27.9918948996,    # Per sea day cost for EM compliance review
    plant_day_rate              =  908.2225000000     # Per plant day cost for shoreside monitoring by observers
  )
)

budget_lst <- list(5819000)
max_budget <- 7.00e6                        # This is used as a cutoff for allocation functions.

# Estimate monitoring costs from method-specific cost models for a wide range of sample rates. This table will be used
# by all designs as it is design-agnostic. Takes 2-3 minutes to run.
#system.time(cost_dt <- rates_to_costs_all(fixed_fmp.allo_lst, sample_rate_vec, cost_params, max_budget))
# FIXME : the ob_cost() model should be changed to logistic so that CPD doesn't decline infinitely with volume of days.
# For now, just exclude any instances where the OB_TOTAL decreases relative to the previous instance
#cost_dt <- rbindlist(lapply(split(cost_dt, by = "ADP"), function(x) x[(which(x[-.N, OB_TOTAL] < x[-1, OB_TOTAL]))]))


#=============================================#
# Calculate Proximity at a 15% sample rate ----
#=============================================#

# SKIP down to Allocation using random sampling with replacement, and define allo_prox() function


fixed_fmp.prox <- calculate_interspersion_gs(fixed_fmp.box, sample_rate_15)

# create a new column named proximity (the updated name of this index, where interspersion is saved for evaluation)
fixed_fmp.prox$ispn_dt[, PROX := ISPN]
# Calculate relationship of proximity index / log(stratum size), which overall linearizes the relationship.
fixed_fmp.prox$ispn_dt[, `PROX/logSTRATA_N` := PROX / log(STRATA_N) ]
# Melt the dataset so we can facet on metrics
fixed_fmp.plot_data <- melt(fixed_fmp.prox$ispn_dt, id.vars = c("ADP", "STRATA", "BSAI_GOA"), measure.vars = c("PROX", "STRATA_N", "PROX/logSTRATA_N"))

fixed_fmp.prox$ispn_dt[ADP %in% c(2021, 2022, 2023), LABEL := ADP]

# [NOTE] THAT THIS IS NOT INTERSPERSION, IT'S THE 'PROXIMITY INDEX'. Therefore, fixed-strata (both OB and EM in BSAI and GOA) would have separate
# gear-specific interspersion values
prox3 <- ggplot(fixed_fmp.plot_data, aes(x = ADP, y = value)) + 
  facet_nested(variable ~ STRATA + BSAI_GOA, scales = "free_y") + 
  geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x = "Year", y = "Value")
# Overall, year to year, interspersion changes little for most strata, especially in the GOA. BSAI is much more sensitive
# due to the low number of trips fishing there.
# PROX/log(STRATA_N) tells me how much 'clumpiness' changes, 'correcting' for the size of the stratum. Increasing trends indicate 
# Ex, if I had the same PROX values over two years, where effort differed, 100 trips vs 500 trips, I would see a decreasing PROX/log(STRATA_N),
# meaning that fishing effort would have to have been larger but more diffuse.
# Ex., OB_TRW GOA, PROX and STRATA_N both decline over the time series (Pollock trips stayed constant but fishing for COD and OTHER declined
# between 2015 and 2020), but PROX/log(STRATA_N) is increasing, meaning that overall remaining fishing effort must have become more clumped (less diffuse).

# Relating proximity to STRATA_N
# overall, Interspersion increases with STRATA_N in a logarithmic (or square root) relationship
prox1 <- ggplot(fixed_fmp.prox$ispn_dt, aes(x = STRATA_N, y = PROX, color = STRATA, shape = BSAI_GOA, linetype = BSAI_GOA)) + geom_point(size = 3, stroke = 1)  + 
  geom_path(aes(group = interaction(STRATA, BSAI_GOA))) + 
  scale_shape_manual(values = c(BSAI = 16, GOA = 2)) + 
  labs(x = "# Trips in Stratum", y = "Proximity Index", shape = "FMP", linetype = "FMP", color = "Stratum") + 
  geom_text(aes(label = LABEL), size = 3, color = "black")

prox2 <- ggplot(fixed_fmp.prox$ispn_dt, aes(x = log(STRATA_N), y = PROX, color = STRATA, shape = BSAI_GOA, linetype = BSAI_GOA)) + geom_point(size = 3, stroke = 1)  + 
  geom_path(aes(group = interaction(STRATA, BSAI_GOA))) + 
  scale_shape_manual(values = c(BSAI = 16, GOA = 2)) + 
  labs(x = "log(# Trips in Stratum)", y = "Proximity Index", shape = "FMP", linetype = "FMP", color = "Stratum") + 
  geom_text(aes(label = LABEL), size = 3, color = "black")

# Square-root doesn't quite linearize the relationship
ggplot(fixed_fmp.prox$ispn_dt, aes(x = sqrt(STRATA_N), y = PROX, color = STRATA, shape = BSAI_GOA, linetype = BSAI_GOA)) + geom_point(size = 3, stroke = 1)  + 
  geom_path(aes(group = interaction(STRATA, BSAI_GOA))) + 
  scale_shape_manual(values = c(BSAI = 16, GOA = 2)) + 
  labs(x = "sqrt(# Trips in Stratum)", y = "Proximity Index", shape = "FMP", linetype = "FMP", color = "Stratum")

# Changes over time
# Interestingly, ISPN goes up faster in the BSAI than the GOA as STRATA_N increases. This tells me that fishing is generally more clumped in time and space
# in the BSAI than in the GOA (which admittedly is much larger in space)
ggplot(fixed_fmp.prox$ispn_dt, aes(x = STRATA_N, y = ISPN, color = STRATA, shape = BSAI_GOA, linetype = BSAI_GOA)) + geom_point(size = 3, stroke = 1) + scale_shape_manual(values = c(BSAI = 16, GOA = 2)) + 
  facet_grid(BSAI_GOA ~ STRATA, scales = "free") + geom_path(aes(group = interaction(STRATA, BSAI_GOA))) + 
  labs(x = "# Trips in Stratum", y = "Proximity Index", shape = "FMP", linetype = "FMP", color = "Stratum")
# Here we can see how the values changed year to year, and whether a trend exists
prox4 <- ggplot(fixed_fmp.prox$ispn_dt, aes(x = STRATA_N, y = ISPN, color = ADP, shape = BSAI_GOA)) + geom_point(size = 3, stroke = 1) + scale_shape_manual(values = c(BSAI = 16, GOA = 2)) + 
  facet_grid(BSAI_GOA ~ STRATA, scales = "free") + scale_color_viridis_c(direction = -1)  + geom_path(aes(group = interaction(STRATA, BSAI_GOA))) + 
  labs(x = "# Trips in Stratum", y = "Proximity Index", shape = "FMP", color = "Year")

#======================================================================================================================#
# INTERSPERSION ----
#======================================================================================================================#

# For interspersion, again assume 15% sample rate

# With OB as only donors (including TRW_EM to itself as it is observer-based)
fixed_fmp.box$dmn$strata_dt
acceptor_donor_lst <-  c(
  rep(list(1:2), times = 2),                # 1-2: EM_FIXED to itself (BSAI + GOA)
  list(3),                                  # 3:   EM_TRW donates to itself
  rep(list(4:5), times = 2),                # 4-5: OB_FIXED to itself (BSAI + GOA)
  rep(list(6:7), times = 2),                # 6-7: OB_TRW to itself (BSAI + GOA)
  rep(list(8:9), times = 2)                 # 8-9:   Zero to itself (BSAI + GOA)
)

fixed_fmp.insp <- calculate_dmn_interspersion4(box_def = fixed_fmp.box, selection_rates = fixed_fmp.prox$ispn_dt, acceptor_donor_lst = acceptor_donor_lst)

# Are these the same or not?
fixed_fmp.insp_smry <- dmn_interspersion_smry(fixed_fmp.insp)


insp1 <- ggplot(fixed_fmp.insp_smry$BSAI_GOA, aes(x = ADP, y = POOL_DMN_INTERSPERSION, color = BOX_DMN_w)) + 
  facet_grid(BSAI_GOA ~ POOL + GEAR) + 
  geom_point(size = 2) + 
  geom_path(size = 2) + 
  scale_color_viridis_c(trans = "log")  + 
  labs(x = "Year", y = "INSP/log(# Trips)", color = "log(# Trips)") +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


# here are actual interspersion values, separated by BSAI/GOA and gear type (HAL separate from POT)
insp_fmp <- copy(fixed_fmp.insp_smry$BSAI_GOA)
insp_fmp[, `INSP/log(BOX_DMN_w)` := POOL_DMN_INTERSPERSION / BOX_DMN_w]
insp2 <- ggplot(insp_fmp, aes(x = ADP, y = `INSP/log(BOX_DMN_w)`, color = BOX_DMN_w)) + 
  facet_grid(BSAI_GOA ~ POOL + GEAR) + 
  geom_point(size = 2) + 
  geom_path(size = 2) + 
  scale_color_viridis_c(trans = "log") + 
  labs(x = "Year", y = "INSP/log(# Trips)", color = "log(# Trips)")+  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# Overall all sampled strata are very consistent year to year, but OB_TRW-BSAI can vary a lot, especially with such few trips.

# If we omit zero, can see other groups's trends a little more clearly.
insp3 <- ggplot(insp_fmp[POOL != "ZERO"], aes(x = ADP, y = `INSP/log(BOX_DMN_w)`, color = BOX_DMN_w)) + 
  facet_grid(BSAI_GOA ~ POOL + GEAR, scales = "free") + 
  geom_point(size = 2) + 
  geom_path(size = 2) + 
  scale_color_viridis_c(trans = "log") + 
  labs(x = "Year", y = "INSP/log(# Trips)", color = "log(# Trips)")+  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))






dmn_interspersion_smry(fixed_fmp.insp)$BSAI_GOA[GEAR == "TRW" & BSAI_GOA == "GOA" & POOL == "OB"]
fixed_fmp.prox$ispn_dt[STRATA == "OB_TRW" & BSAI_GOA == "GOA"]
# Not exact... not sure if the small differences are due to some crossover trips?

dmn_interspersion_smry(fixed_fmp.insp)$BSAI_GOA[GEAR == "TRW" & BSAI_GOA == "BSAI" & POOL == "OB"]
fixed_fmp.prox$ispn_dt[STRATA == "OB_TRW" & BSAI_GOA == "BSAI"]
# In the BSAI, PROX and Interspersion can be a lot more different. 2017 has 0.923 in interspersion but prox is only 0.721???
# Must be a lot of overlap between these HEX_IDs?
dmn_interspersion_smry(fixed_fmp.insp)$BSAI_GOA[GEAR == "TRW" & BSAI_GOA == "BSAI" & POOL == "OB"]$POOL_DMN_INTERSPERSION - 
  fixed_fmp.prox$ispn_dt[STRATA == "OB_TRW" & BSAI_GOA == "BSAI", PROX]
plot(dmn_interspersion_smry(fixed_fmp.insp)$BSAI_GOA[GEAR == "TRW" & BSAI_GOA == "BSAI" & POOL == "OB"]$POOL_DMN_INTERSPERSION, ylim = c(0.4, 1), type = "l")
lines(fixed_fmp.prox$ispn_dt[STRATA == "OB_TRW" & BSAI_GOA == "BSAI", PROX], col= "red")
# Interspersion values are less jagged. Interspersion is higher because of some interaction with GOA trips.
test_plots <- dmn_interspersion_plot(fixed_fmp.insp)

# Adding NMFS areas is super slow, but seeing BSAI/GOa boundary would be nice, and keep extent limited to hex_ids, not 
# all NMFs areas
# test_plots$PLOTS$TRW[["2022"]] + geom_sf(data = shp_nmfs %>% st_set_crs(st_crs(fixed_fmp.insp$geom)))
test_plots$PLOTS$TRW[["2022"]]

# how about OB_FG?
fixed_fmp.insp_smry$BSAI_GOA[BSAI_GOA == "BSAI" & POOL == "OB" & GEAR %in% c("HAL", "POT")][order(ADP)]
fixed_fmp.prox$ispn_dt[STRATA == "OB_FIXED" & BSAI_GOA == "BSAI"]

# EM_TRW Matches perfectly
dmn_interspersion_smry(fixed_fmp.insp)$BSAI_GOA[GEAR == "TRW" & BSAI_GOA == "GOA" & POOL == "EM"]
fixed_fmp.prox$ispn_dt[STRATA == "EM_TRW" & BSAI_GOA == "GOA"]



#======================================================================================================================#
# Bootstrapping Methods ----
#======================================================================================================================#

# USE THE LOOP BELOW THAT DOES BOTH PROXIMITY AND ALLOCATION

# Does proximity change a lot when bootstrapping with replacement? What happens if we sample the same
# number of trips each year, just with replacement?

# Split each by year and stratum
fixed.year_lst <- split(fixed.val_2015_2023_dt[order(ADP)], by = c("ADP", "STRATA", "BSAI_GOA"))

# Define number of bootstrap iterations
bootstrap_iter <- 100
bootstrap_lst <- vector(mode = "list", length = bootstrap_iter)

set.seed(12345)
for(i in seq_len(bootstrap_iter)) {
  
  # Create bootstrapped fishing effort, sampling with replacement the same number of trips
  bootstrap.effort <- rbindlist(lapply(fixed.year_lst, function(x) {
    # Sample trips with replacement
    trip_ids <- unique(x$TRIP_ID)
    sampled_trip_ids <- data.table(TRIP_ID = sample(trip_ids, size = length(trip_ids), replace = T))
    sampled_trip_ids[, I := .I]
    bootstrap_sample <- x[sampled_trip_ids, on = .(TRIP_ID), allow.cartesian = T]
  }))
  # Re-assign trip_id so that we can differentiate trips sampled multiple times
  bootstrap.effort[, TRIP_ID := .GRP, keyby = .(ADP, STRATA, BSAI_GOA, TRIP_ID, I)]
  if(uniqueN(bootstrap.effort$TRIP_ID) != uniqueN(fixed.val_2015_2023_dt$TRIP_ID)) stop("Count of TRIP_IDs doesn't match!")
  
  # Define boxes of bootstrapped effort
  bootstrap.box <- define_boxes_gs(
    bootstrap.effort, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
    year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), geom = F, ps_cols = c("GEAR"))
  
  # Calculate proximity at 15% sample rate (function is still called interspersion)
  bootstrap.prox <- calculate_interspersion_gs(bootstrap.box, sample_rate_vec = sample_rate_15)
  bootstrap_lst[[i]] <- bootstrap.prox$ispn_dt
}
bootstrap_results_dt <- rbindlist(bootstrap_lst, idcol = "Bootstrap Iter")

bootstrap_1 <- ggplot(bootstrap_results_dt, aes(x = as.character(ADP), y = ISPN)) +
  facet_grid(BSAI_GOA ~ STRATA) + 
  geom_violin(color = "red", draw_quantiles = 0.5) +
  stat_summary(geom = "point", color = "red", fun = mean, shape = 4) + 
  geom_point(data = fixed_fmp.prox$ispn_dt, shape = 1, fill = NA, stroke = 1) + 
  geom_text(data = fixed_fmp.prox$ispn_dt, aes(label = STRATA_N), size = 3, na.rm = T, nudge_y = -0.05, angle = 90) + 
  labs(x = "Year", y = "Proximity Index")

bootstrap_2 <- ggplot(bootstrap_results_dt, aes(x = as.character(ADP), y = ISPN)) +
  facet_wrap(BSAI_GOA ~ STRATA, scales = "free") + 
  geom_violin(color = "red", draw_quantiles = 0.5) +
  stat_summary(geom = "point", color = "red", fun = mean, shape = 4) + 
  geom_point(data = fixed_fmp.prox$ispn_dt, shape = 1, fill = NA, stroke = 1) + 
  labs(x = "Year", y = "Proximity Index")
# Good news is that the distributions seem to be generally normally distributed and not skewed. 



# What are the relative changes to interspersion compared to original?
dcast(
  rbind(
    cbind("Type" = "Bootstrap", bootstrap_results_dt),
    cbind("Type" = "Original", fixed_fmp.prox$ispn_dt),
    fill = T
  )[, .(
    `Perc Diff vs OG` = round(100 * ((mean(ISPN[Type == "Bootstrap"]) / ISPN[Type == "Original"]) - 1), 3)
    ), by = .(ADP, STRATA, BSAI_GOA)],
  BSAI_GOA + STRATA ~ ADP, value.var = "Perc Diff vs OG")
# Like the figures show, it indicates that bootstrapping using sampling with replacement will result in a positive
# bias of proximity indices, especially in the BSI_GOA where the bias can be between +3 and +9 percent. Biases in the GOA
# are generally low, typically well within +1 percent.

# TODO What if we sample without replacement, and repeat this if we need to sample more trips than prior year?
# TODO What happens if use the prior 3 years to sample without replacemnt from each year? Might come with its own issues with changes in trends, and
# would likely warrant using higher sampling probabilities from more recent years.

# TODO Since we didn't actually allocate, it's hard to say exactly how much the rates would change year to year with 
# a specific budget. However, given that it looks like low effort strata in the BSAI generally have much higher bootstrapped
# proximity, meaning those strata would get allocated to relatively less compared to the original. 


#======================================================================================================================#
## Save outputs for effort_prediction_exploration.Rmd ----
#======================================================================================================================#

save(
  effort_prediction_exploration_plots,
  prox1, prox2, prox3, prox4,
  insp1, insp2, insp3, 
  prox_by_rate_1, prox_by_rate_2,
  bootstrap_results_dt, bootstrap_1, bootstrap_2,
  file = "analyses/effort_prediction/effort_prediction_exploration.rdata")

#======================================================================================================================#
# How does sample rate affect proximity ----
#======================================================================================================================#

fixed_fmp.prox_range <- calculate_interspersion_gs(fixed_fmp.box, sample_rate_vec = seq(0.05, 0.25, by = 0.05))
# Increasing sample rate just drags values up towards 1
prox_by_rate_1 <- ggplot(fixed_fmp.prox_range$ispn_dt, aes(x = STRATA_N, y = ISPN, color = STRATA, shape = BSAI_GOA, linetype = BSAI_GOA)) + 
  facet_grid(. ~ SAMPLE_RATE) + 
  geom_point() + geom_path() + labs(x = "# Trips", y = "Proximity", shape = "FMP", linetype = "FMP", color = "Strata") + 
  scale_shape_manual(values = c(BSAI = 16, GOA = 2)) 
# If we allow the y-axes to vary, we see little changes in the overall shape
prox_by_rate_2 <- ggplot(fixed_fmp.prox_range$ispn_dt, aes(x = STRATA_N, y = ISPN, color = STRATA, shape = BSAI_GOA, linetype = BSAI_GOA)) + 
  facet_wrap(. ~ SAMPLE_RATE, scales = "free", nrow = 1) + 
  geom_point() + geom_path() + labs(x = "# Trips", y = "Proximity", shape = "FMP", linetype = "FMP", color = "Strata") + 
  scale_shape_manual(values = c(BSAI = 16, GOA = 2))
# We see the most separation of Proximity at a rate of 5%?


#======================================================================================================================#
# Allocation using random sampling with replacement ----
#======================================================================================================================#

# Get realized proximity of each year
og_prox <- calculate_interspersion_gs(fixed_fmp.box, sample_rate_vec = seq(from = 0.0001, to = 0.9950, by = 0.0001) )

# For each year, calculate what the equal rates allocation would be
# fixed_fmp.equal <- rbindlist(lapply(budget_lst, function(x) cbind(BUDGET = x, allo_equal2(cost_dt, budget = x, fixed_fmp.allo_lst, cost_params))))
# dcast(fixed_fmp.equal, STRATA ~ ADP, value.var = "MON_RATE")
# proximity, indices, and costs.

# This function is a replacemnt for calculate_interspersion_gs(), calculate_index2(), and prox_rates_from_budget2(),
# that runs the allocation faster as it hones in on the budget, rather than doing all and then filtering for any budget.
# TODO NEED THIS FUNCTION FOR BOOTSTRAPPING TO GET RATES
allo_prox <- function(box_def, allo_lst, cost_params, budget, max_budget, index_interval = 0.001, range_var = 1) { 
  # box_def <- copy(fixed_fmp.box); allo_lst <- copy(fixed_fmp.allo_lst);  budget <- budget_lst[[1]]

  # box_def <- copy(bootstrap.box); allo_lst <- copy(bootstrap.allo_lst);  budget <- budget_lst[[1]]; index_interval <- 0.0001; range_var = 0
  
  # box_def <- copy(swor_bootstrap.box); allo_lst <- copy(swor_bootstrap.allo_lst); index_interval <- 0.0001
  
  # Have to do this by year, so feed the data one year at a time
  group_cols <- c(box_def$params$year_col, box_def$params$stratum_cols)
  stratum_cols <- box_def$params$stratum_cols
  year_col <- box_def$params$year_col
  
  year_vec <- unique(box_def$strata_n_dt$ADP)
  
  # TODO FOR NOW, OMIT ZERO and EM_TRW as we will not allocate to either in 2024
  box_def$strata_n_dt <- box_def$strata_n_dt[!(STRATA %like% "ZERO|EM_TRW")]
  box_def$dt_out <- box_def$dt_out[!(STRATA %like% "ZERO|EM_TRW")]
  box_def$box_smry <- box_def$box_smry[!(names(box_def$box_smry) %like% "ZERO|EM_TRW")]
  
  year_res <- vector(mode = "list", length = length(year_vec))
  
  for(i in year_vec) {

    # Quickly calculate proximity for a wide range of sample rates but with low resolution (0.005 instead of 0.0001)
    # makes this 50x faster.
    
    # TODO I Think I should use by = 0.001. the most time consuming part is calculating a huge a huge vector of INDEX, most
    # of which not even close!
    
    box_def_sub.prox.range <- calculate_interspersion_gs(box_def, sample_rate_vec = c(0.0001, seq(0.05, 1, by = 0.001)), omit_strata = "ZERO" )$ispn_dt[ADP == i]
    # Calculate index for each stratum
    box_def_sub.prox.range[
    ][, n := SAMPLE_RATE * STRATA_N
    ][, FPC := (STRATA_N - n) / STRATA_N
    ][, CV_SCALING := sqrt(FPC * (1/n))
    ][, INDEX := ISPN * (1 - CV_SCALING)][is.na(INDEX), INDEX := 0]
    setorderv(box_def_sub.prox.range, c(stratum_cols, "INDEX"))
    # Approximate the costs of a range of indices 
    index_vec <- seq(0, 1, by = 0.025)
    index_vec_rates_costs <- lapply(
      index_vec, 
      function(x) {
        res <- box_def_sub.prox.range[, .SD[findInterval(x, INDEX)], by = c(box_def$params$stratum_cols)]
        stratum_column <- apply(res[, ..stratum_cols], 1, paste, collapse = "-")
        res[, STRATUM_COL := stratum_column]
        res[, INDEX := x]
        index_cost <- calculate_cost_prox(res, cost_params, allo_lst, max_budget) # this is the most this index would cost
        if( nrow(index_cost) > 0 ) res[, INDEX_COST := index_cost$INDEX_COST]
        res
      }
    )
    
    # Omit any indices that go over the maximum budget
    index_costs <- sapply(index_vec_rates_costs, function(x) unique(x$INDEX_COST))
    for(j in seq_along(index_costs)) {
      if( is.null(index_costs[[j]])) {
        index_costs <- unlist(index_costs[1:(j - 1)])
        break()
      } else if (j > 1) {
        if( index_costs[[j]] < index_costs[[j - 1]]) {
          index_costs <- unlist(index_costs[1:(j - 1)])
          break()
        }
      }
    }
    
    # Find the range of indices to explore
    index_near_budget <- findInterval(budget, index_costs)
    index_range <- index_vec[c(index_near_budget - range_var, index_near_budget + 1)]  # I can afford an index somewhere in this range
    # Get all stratum names
    strata_dt <- unique(box_def$strata_n_dt[, ..stratum_cols])
    prox_by_stratum_lst <- vector(mode = "list", length = nrow(strata_dt))
    # Calculate proximity for each stratum using a focused range of sample rates
    for(k in 1:nrow(strata_dt)) {
      # k <- 1
      
      stratum_year <- paste0(i, ".", paste(strata_dt[k], collapse = "." ))
      # Make a new box definition specific to the stratum of focus
      box_stratum <- list(
        box_smry = box_def$box_smry[stratum_year],
        strata_n_dt = box_def$strata_n_dt,
        params = box_def$params
      )
      # Find the stratum's range of sample rates
      sample_range <- sapply(
        index_vec_rates_costs[c(index_near_budget - range_var, index_near_budget + 1)],             # FIXME I have to do +2 instead of +1. findInterval always underestimates?
        function(x) x[strata_dt[k], on = c(box_def$params$stratum_cols), SAMPLE_RATE]
      )
      # box_res <- copy(box_stratum); omit_strata <- NULL; sample_rate_vec <- seq(0.5, 575, by = 0.0001)
      # Now, we go back calculating rates ever 0.0001 here.
      prox_by_stratum <- calculate_interspersion_gs(box_stratum, sample_rate_vec = seq(sample_range[1], sample_range[2], by = 0.0001))$ispn_dt
      prox_by_stratum[
      ][, n := SAMPLE_RATE * STRATA_N
      ][, FPC := (STRATA_N - n) / STRATA_N
      ][, CV_SCALING := sqrt(FPC * (1/n))
      ][, INDEX := ISPN * (1 - CV_SCALING)]
      prox_by_stratum_lst[[k]]  <- prox_by_stratum
      
    }
    prox_by_list_dt <- rbindlist(prox_by_stratum_lst)
    
    # find the common range of indices
    # Find range if INDEX that is common to all strata x ADP
    prox_by_list_dt[, as.list(setNames(range(INDEX), c("MIN", "MAX"))), by = c(stratum_cols)]
    
    index_range_afforded <- prox_by_list_dt[
    ][, .(MIN = min(INDEX), MAX = max(INDEX)), by = group_cols
    ][, .(MIN = max(MIN),  MAX = min(MAX)), by = year_col]
    
    prox_by_list_dt <- prox_by_list_dt[, .SD[between(INDEX, index_range_afforded$MIN, index_range_afforded$MAX )], by = c(stratum_cols)]
    
    # I can set index_interval to 0.0001 to really get the closes to affording the budget. Does take 10x longer...
    prox_index_search <- seq(round(index_range_afforded$MIN,3), round(index_range_afforded$MAX,3), by = index_interval)
    index_costs2 <- lapply(prox_index_search, function(x) {
      x1 <- data.table(INDEX = x)
      x2 <- prox_by_list_dt[, .SD[x1, on = .(INDEX), roll = "nearest"], by = c(stratum_cols)]
      stratum_column <- apply(x2[, ..stratum_cols], 1, paste, collapse = "-")
      x2$STRATUM_COL <- stratum_column
      x2
    })
    # Calculate the cost of each index
    index_costs_vec <- sapply(index_costs2, function(x) {
      calculate_cost_prox(x, cost_params, allo_lst, max_budget)$INDEX_COST
    })
    
    # Find the index that is closest to the budget
    closest_to_budget <- findInterval(budget, unlist(index_costs_vec))
    out <- index_costs2[[closest_to_budget]]
    out[, INDEX_COST := unlist(index_costs_vec)[closest_to_budget]]
    year_res[[which(year_vec == i)]] <- out
  }
  
  # Return the allocated rates, collasping the list of rates by year
  rbindlist(year_res)

}

# Find the rates afforded each year assuming our current 2024 ADP budget
system.time(og_rates <- allo_prox(fixed_fmp.box, fixed_fmp.allo_lst, cost_params, budget_lst[[1]], max_budget, index_interval = 0.0001))
# using sample_rate by = 0.001  and default index_interval = 0.001 takes 53 sec. Changing it to 0.0001 takes 142 sec.
unique(og_rates[, .(ADP, INDEX_COST)])

# Overall, sample rates increase over time because effort overall has decreased
ggplot(og_rates, aes(x = ADP, y = SAMPLE_RATE, fill = STRATUM_COL)) + facet_grid(STRATUM_COL ~ .) + geom_col(color = "black") + labs(x = "Year", y = "Sample Rate", fill = "Stratum")
# When we look at proportions of sample size, there are very few differences
ggplot(og_rates, aes(x = ADP, y = n, fill = STRATUM_COL)) + geom_col(position = "fill", color = "black") + labs(x = "Year", y = "Sample size (n)", fill = "Stratum")

# Now that we can fairly quickly calculate allocated rates, we can do some testing of bootstrapping methods!


#======================================================================================================================#
# Prepare Bootstrapping ----
#======================================================================================================================#

# Define number of bootstrap iterations
bootstrap_iter <- 100

#======================================================================================================================#
# 1-Year Sampling >With< Replacement (current method) ----
#======================================================================================================================#

# Assumptions
# + We will assume we have a perfect prediction of `next year's` fishing effort (STRATA_N)

# We will sample the 'current' year's fishing with replacement knowing the upcoming year's STRATA_N
# We will treat each 

# Initialize list of bootstrapping results
adp_years <- unique(fixed.val_2015_2023_dt[order(ADP), ADP])  # 2015 - 2023
adp_years_to_predict <- adp_years[-1]                     # Will use 2015 to predict 2016, 2016 for 2017, and so on.

swr_1_lst <- vector(mode = "list", length = adp_years_to_predict)
set.seed(12345)
for(j in seq_along(adp_years_to_predict)) {
  # j <- 1
  
  cat("Year", adp_years_to_predict[j], "\n")
  
  # Subset the data to prior year(s)
  adp_years_to_pull <- adp_years_to_predict[j] - (1)
  effort_prior <- fixed.val_2015_2023_dt[ADP %in% adp_years_to_pull]
  effort_prior[, ADP := adp_years_to_predict[j]]            # ADP is the year our prediction is for
  effort_prior_lst <- split(effort_prior[order(STRATA, BSAI_GOA)], by = c("STRATA", "BSAI_GOA"))
  
  # Use actual trip counts as our 'predicted' number of trips to sample to create yearly populations (perfect prediction of effort)
  adp_strata_N <- fixed.val_2015_2023_dt[ADP == adp_years_to_predict[j], .(N = uniqueN(TRIP_ID)), keyby = .(STRATA, BSAI_GOA)]
  adp_strata_N[, STRATUM_COL := paste0(STRATA, ".", BSAI_GOA)] 
  
  # Make sure names and ordering are the same
  if(!identical(names(effort_prior_lst), adp_strata_N$STRATUM_COL)) stop("Stratum names/order are not the same!")
  
  # Initialize bootstrap list
  swr_bootstrap_lst <- vector(mode = "list", length = bootstrap_iter)
  
  # Begin bootstrap sampling WITH replacement
  for(i in seq_len(bootstrap_iter)) {
    
    cat(i, ", ")
    
    # Bootstrap using adp_strata_N to sample each stratum's population size size
    swr_bootstrap.effort <- rbindlist(Map(
      function(prior, strata_N) {
        # prior <- effort_prior_lst[[1]]; strata_N <- adp_strata_N$N[1]
        trip_ids <- unique(prior$TRIP_ID)
        sampled_trip_ids <- data.table(TRIP_ID = sample(trip_ids, size = strata_N, replace = T))
        sampled_trip_ids[, I := .I]
        # Bring in each trip's data
        bootstrap_sample <- prior[sampled_trip_ids, on = .(TRIP_ID), allow.cartesian = T]
      }, 
      prior = effort_prior_lst,
      strata_N = adp_strata_N$N
    ))
 
    # Re-assign trip_id so that we can differentiate trips sampled multiple times
    swr_bootstrap.effort[, TRIP_ID := .GRP, keyby = .(ADP, STRATA, BSAI_GOA, TRIP_ID, I)]
    if(uniqueN(swr_bootstrap.effort$TRIP_ID) !=  sum(adp_strata_N$N)) stop("Count of TRIP_IDs doesn't match!")
    
    # Define boxes of bootstrapped effort
    bootstrap.box <- define_boxes_gs(
      swr_bootstrap.effort, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
      year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), geom = F, ps_cols = c("GEAR"))
  
    bootstrap.allo_lst <- list(effort = unique(swr_bootstrap.effort[, .(ADP, STRATA, BSAI_GOA, TRIP_ID, DAYS)])[
    ][, STRATA := paste0(STRATA, "-", BSAI_GOA)
    ][, BSAI_GOA := NULL
    ][, .(STRATA_N = uniqueN(TRIP_ID), TRP_DUR = mean(DAYS)), keyby = .(ADP, STRATA)])
  
    # Calculate proximity at 15% sample rate (function is still called interspersion)
    bootstrap.prox <- calculate_interspersion_gs(bootstrap.box, sample_rate_vec = sample_rate_15)$ispn_dt
    
    # Calculate rates afforded with a $5.81M budget
    # [NOTE] allo_prox currently omits ZERO and EM_TRW from the allocation, per 2024 ADP. 
    bootstrap.rates <- allo_prox(bootstrap.box, bootstrap.allo_lst, cost_params, budget_lst[[1]], max_budget, index_interval = 0.0001, range_var = 1)
   
    swr_bootstrap_lst[[i]] <- list(prox = bootstrap.prox, rates = bootstrap.rates)
  }
  
  # Capture the results from each year
  swr_1_lst[[j]] <- swr_bootstrap_lst
}

# save(swr_1_lst, file = "analyses/effort_prediction/swr_1_lst.rdata")

swr_1_prox_dt <-  rbindlist(lapply(swr_1_lst, function(x) rbindlist(lapply(x, "[[", "prox"), idcol = "ITER")))
swr_1_rates_dt <-  rbindlist(lapply(swr_1_lst, function(x) rbindlist(lapply(x, "[[", "rates"), idcol = "ITER")))


ggplot(swr_1_rates_dt, aes(x = as.character(ADP), y = SAMPLE_RATE)) + facet_grid(STRATUM_COL ~ ., scales = "free") + 
  geom_violin(color = "red", draw_quantiles = 0.5) + 
  stat_summary(geom = "point", color = "red", fun = mean, shape = 4) + 
  geom_point(data = og_rates, shape = 1, fill = NA, stroke = 1) + 
  labs(x = "Year", y = "Sample Rate")

# As predicted, because proximity is inflated when bootstrapping using sampling with replacement, the smaller BSAI strata
# end up with lower than desired sampling rates, and GOA get higher-than-desired sampling rates.

bootstrap_rates_mean <- bootstrap_rates_dt[, .(SAMPLE_RATE = mean(SAMPLE_RATE)), keyby = .(ADP, STRATUM_COL, STRATA_N)]
bootstrap_rates_mean[, OG_RATE := og_rates[bootstrap_rates_mean, SAMPLE_RATE, on = .(ADP, STRATUM_COL)]]
# Percent difference in rates
dcast(
  bootstrap_rates_mean[, .(PERC_DIFF = round((SAMPLE_RATE - OG_RATE)/OG_RATE * 100,3) ), keyby = .(ADP, STRATUM_COL)],
  STRATUM_COL ~ ADP, value.var = c("PERC_DIFF"))
# Raw difference in rates
dcast(
  bootstrap_rates_mean[, .(RAW_DIFF = round((SAMPLE_RATE - OG_RATE),3) ), keyby = .(ADP, STRATUM_COL)],
  STRATUM_COL ~ ADP, value.var = c("RAW_DIFF"))
# Raw difference in sample size
dcast(
  bootstrap_rates_mean[, .(n_DIFF = round(STRATA_N * (SAMPLE_RATE - OG_RATE),3) ), keyby = .(ADP, STRATUM_COL)],
  STRATUM_COL ~ ADP, value.var = c("n_DIFF"))

# On average, the rates are really not vastly different: on average they end up being within ~ 0.02. In 2022, It mostly
# means 7 more tris for OB_FIXED-GOA and 4.5 fewer for OB_FIXED-BSAI
# Interestingly... OB_TRW-BSAI ends up with more samples just because it is protected by CV_SCALING so much, and
# EM_FIXED-BSAI an OB-FIXED-BSAI are sampled less resulting in more money for sampling other strata.


#======================================================================================================================#
# 3-year Sampling >Without< Replacement ----
#======================================================================================================================#

# [ NOTE ] 
# Assuming we have a perfect prediction of next year's number of fishing trips, how do the sampling methods differ
# in their prediction of Proximity and Sample Rates? The differences are due to how well they predict the spatiotemporal
# arrangement of the fishing.

# If we see what we have above and assume that our estimate for one year would be used to predict the next, we have a 
# baseline level of prediction error, some of which will be due to the bias. If we use sampling without replacement 
# using the prior 3 years of fishing effort, can we resolve this bias issue? And, does it overall have less prediction 
# error?

adp_years <- unique(fixed.val_2015_2023_dt[order(ADP), ADP])  # 2015 - 2023
adp_years_to_predict <- adp_years[-(1:3)]                     # Will use 2015-2017 to predict 2018, and so on.
swor_3_lst <- vector(mode = "list", length = length(adp_years_to_predict))
set.seed(12345)
for(j in seq_along(adp_years_to_predict)) {
  # j <- 1
  
  cat("Year ", adp_years_to_predict[j], "\n")
  
  # Subset the data to prior 3 years
  adp_years_to_pull <- adp_years_to_predict[j] - (3:1)
  effort_prior_3 <- fixed.val_2015_2023_dt[ADP %in% adp_years_to_pull]
  effort_prior_3[, ADP := adp_years_to_predict[j]]
  effort_prior_3_lst <- split(effort_prior_3[order(STRATA, BSAI_GOA)], by = c("STRATA", "BSAI_GOA"))
  
  # Use actual trip counts as our 'predicted' number of trips to sample to create yearly populations
  adp_strata_N <- fixed.val_2015_2023_dt[ADP == adp_years_to_predict[j], .(N = uniqueN(TRIP_ID)), keyby = .(STRATA, BSAI_GOA)]
  adp_strata_N[, STRATUM_COL := paste0(STRATA, ".", BSAI_GOA)] 
  
  # Make sure names and ordering are the same
  if(!identical(names(effort_prior_3_lst), adp_strata_N$STRATUM_COL)) stop("Stratum names/order are not the same!")
  
  # Initialize bootstrap list
  swor_boot_lst <- vector(mode = "list", length = bootstrap_iter)
  
  for(k in seq_len(bootstrap_iter)) {
    # k <- 1
    cat(k, ", ")

    # Bootstrap using adp_strata_N to sample each stratum's population size size
    swor_bootstrap.effort <- rbindlist(Map(
      function(prior, strata_N) {
        # Sample trips WITHOUT replacement
        trip_ids <- unique(prior$TRIP_ID)
        sampled_trip_ids <- data.table(TRIP_ID = sample(trip_ids, size = strata_N, replace = F))
        sampled_trip_ids[, I := .I]
        bootstrap_sample <- prior[sampled_trip_ids, on = .(TRIP_ID), allow.cartesian = T]
      }, 
      prior = effort_prior_3_lst,
      strata_N = adp_strata_N$N
    ))

    # Re-assign trip_id so that we can differentiate trips sampled multiple times
    swor_bootstrap.effort[, TRIP_ID := .GRP, keyby = .(ADP, STRATA, BSAI_GOA, TRIP_ID, I)]
    if(uniqueN(swor_bootstrap.effort$TRIP_ID) != sum(adp_strata_N$N)) stop("Count of TRIP_IDs doesn't match!")
    
    # Adjust the TRIP_TARGET_DATE and LANDING_DATE to be for the current year
    swor_bootstrap.effort[, ':=' (
      TRIP_TARGET_DATE = as.Date(yday(TRIP_TARGET_DATE), origin = paste0(adp_years_to_predict[j], "-01-01")) - 1 ,
      LANDING_DATE =  as.Date(yday(LANDING_DATE), origin = paste0(adp_years_to_predict[j], "-01-01")) - 1
    )]
    # Due to leap years, cram any dates that got pushed to the next to to the previous year, Dec 31
    swor_bootstrap.effort[year(TRIP_TARGET_DATE) == (adp_years_to_predict[j] + 1), TRIP_TARGET_DATE := TRIP_TARGET_DATE - 1 ]

    # Define boxes of bootstrapped effort
    swor_bootstrap.box <- define_boxes_gs(
      swor_bootstrap.effort, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
      year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), geom = F, ps_cols = c("GEAR"))
    
    swor_bootstrap.allo_lst <- list(effort = unique(swor_bootstrap.effort[, .(ADP, STRATA, BSAI_GOA, TRIP_ID, DAYS)])[
    ][, STRATA := paste0(STRATA, "-", BSAI_GOA)
    ][, BSAI_GOA := NULL
    ][, .(STRATA_N = uniqueN(TRIP_ID), TRP_DUR = mean(DAYS)), keyby = .(ADP, STRATA)])
    
    # Calculate proximity at 15% sample rate (function is still called interspersion)
    swor_bootstrap.prox <- calculate_interspersion_gs(swor_bootstrap.box, sample_rate_vec = sample_rate_15)$ispn_dt
    
    # Calculate rates afforded with a $5.81M budget
    swor_bootstrap.rates <- allo_prox(swor_bootstrap.box, swor_bootstrap.allo_lst, cost_params, budget_lst[[1]], max_budget, index_interval = 0.0001)

    # Save iteration results
    swor_boot_lst[[k]] <- list(
      prox = swor_bootstrap.prox, 
      rates = swor_bootstrap.rates
    )

  }
  
  # Capture the results from each year
  swor_3_lst[[j]] <- swor_boot_lst

}

# save(swor_3_lst, file = "analyses/effort_prediction/swor_3_lst.rdata")


#======================================================================================================================#
## Proximity
#======================================================================================================================#

test <- rbindlist(lapply(swor_3_lst, function(x) rbindlist(lapply(x, "[[", "prox"), idcol = "ITER")))
# As a test, using 2015-2017 SWOR (red distribution) to predict 2018 realized (black dot)  
ggplot(test, aes(x = as.character(ADP), y = ISPN)) + 
  facet_grid(BSAI_GOA ~ STRATA, scales = "free") +
  geom_violin(draw_quantiles = 0.5, color = "red", fill = NA) + 
  geom_violin(data = bootstrap_prox_dt[ADP != 2023], aes(x = as.character(ADP + 1)), color = "blue", draw_quantiles = 0.5, fill = NA) +
  geom_point(data = og_prox$ispn_dt[SAMPLE_RATE == 0.15 ], fill = "green", shape = 21, size = 2) +
  geom_text(data = og_prox$ispn_dt[SAMPLE_RATE == 0.15 ], aes(label = STRATA_N), size = 2, nudge_y = -0.01) +
  labs(x = "Year", y = "Proximity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
# RED: SWoR-3; BLUE: SWR-1


# TODO Try bootstrapping just two years?

#======================================================================================================================#
## Allocation
#======================================================================================================================#

test2 <- rbindlist(lapply(swor_3_lst, function(x) rbindlist(lapply(x, "[[", "rates"), idcol = "ITER")))
# As a test, using 2015-2017 SWOR (red distribution) to predict 2018 realized (black dot)  
ggplot(test2, aes(x = as.character(ADP), y = SAMPLE_RATE)) + 
  facet_grid(BSAI_GOA ~ STRATA, scales = "free") +
  geom_violin(draw_quantiles = 0.5, color = "red", fill = NA) + 
  #geom_point(data = og_prox$ispn_dt[SAMPLE_RATE == 0.15 & ADP == 2017], color = "blue") + 
  # Offsetting 1-year SWOR bootstrap by 1 year, e.g., using 2015 to predict 2016
  geom_violin(data = bootstrap_rates_dt[ADP != 2023], aes(x = as.character(ADP + 1)), color = "blue", draw_quantiles = 0.5, fill = NA) + 
  geom_point(data = og_rates, color = "black", size = 2) +
  labs(x = "Year", y = "Sample Rate") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
# Blue is basically just allocating based on previous year (with some biases due to SWR)





# TODO Plot diff relative to OG?


swr_1_diff <- copy(bootstrap_rates_dt)
swr_1_diff[, ADP := ADP + 1]
swr_1_diff <- swr_1_diff[ADP %in% 2018:2023]
swr_1_diff <- swr_1_diff[, .(SWR_1_RATE = mean(SAMPLE_RATE), SWR_1_PROX = mean(ISPN)), keyby = .(ADP, STRATUM_COL)]

swor_3_diff <- test2[, .(SWOR_3_RATE = mean(SAMPLE_RATE), SWOR_3_PROX = mean(ISPN)), keyby = .(ADP, STRATUM_COL)]

compare_rates_prox <- og_rates[ADP %in% c(2018:2023), .(STRATUM_COL, STRATA, BSAI_GOA, ADP, OG_RATE = SAMPLE_RATE, OG_PROX = ISPN)]
compare_rates_prox <- compare_rates_prox[swr_1_diff, on = .(ADP, STRATUM_COL)][swor_3_diff, on = .(ADP, STRATUM_COL)]

compare_rates_prox[, .(
  SWR_1_bias = mean(SWR_1_RATE - OG_RATE),
  SWOR_3_bias = mean(SWOR_3_RATE - OG_RATE),
  SWR_1_total_resid = sum((SWR_1_RATE - OG_RATE)^2),
  SWOR_3_total_resid = sum((SWOR_3_RATE - OG_RATE)^2)
)]

 # Sampling without replacement using only most recent year (and if effort increases, use all trips and then SWR)


#======================================================================================================================#
# 2-year Sampling >Without< Replacement ----
#======================================================================================================================#

# Same thing as before, but using 2 years instead of 3

adp_years <- unique(fixed.val_2015_2023_dt[order(ADP), ADP])  # 2015 - 2023
adp_years_to_predict <- adp_years[-(1:2)]                     # Will use 2015-2017 to predict 2018, and so on.
swor_2_lst <- vector(mode = "list", length = length(adp_years_to_predict))
set.seed(12345)
for(j in seq_along(adp_years_to_predict)) {
  # j <- 1
  
  cat("Year ", adp_years_to_predict[j], "\n")
  
  # Subset the data to prior 3 years
  adp_years_to_pull <- adp_years_to_predict[j] - (2:1)
  effort_prior_3 <- fixed.val_2015_2023_dt[ADP %in% adp_years_to_pull]
  effort_prior_3[, ADP := adp_years_to_predict[j]]
  effort_prior_3_lst <- split(effort_prior_3[order(STRATA, BSAI_GOA)], by = c("STRATA", "BSAI_GOA"))
  
  # Use actual trip counts as our 'predicted' number of trips to sample to create yearly populations
  adp_strata_N <- fixed.val_2015_2023_dt[ADP == adp_years_to_predict[j], .(N = uniqueN(TRIP_ID)), keyby = .(STRATA, BSAI_GOA)]
  adp_strata_N[, STRATUM_COL := paste0(STRATA, ".", BSAI_GOA)] 
  
  # Make sure names and ordering are the same
  if(!identical(names(effort_prior_3_lst), adp_strata_N$STRATUM_COL)) stop("Stratum names/order are not the same!")
  
  # Initialize bootstrap list
  swor_boot_lst <- vector(mode = "list", length = bootstrap_iter)
  
  for(k in seq_len(bootstrap_iter)) {
    # k <- 1
    cat(k, ", ")
    
    # Bootstrap using adp_strata_N to sample each stratum's population size size
    swor_bootstrap.effort <- rbindlist(Map(
      function(prior, strata_N) {
        # Sample trips WITHOUT replacement
        trip_ids <- unique(prior$TRIP_ID)
        sampled_trip_ids <- data.table(TRIP_ID = sample(trip_ids, size = strata_N, replace = F))
        sampled_trip_ids[, I := .I]
        bootstrap_sample <- prior[sampled_trip_ids, on = .(TRIP_ID), allow.cartesian = T]
      }, 
      prior = effort_prior_3_lst,
      strata_N = adp_strata_N$N
    ))
    
    # Re-assign trip_id so that we can differentiate trips sampled multiple times
    swor_bootstrap.effort[, TRIP_ID := .GRP, keyby = .(ADP, STRATA, BSAI_GOA, TRIP_ID, I)]
    if(uniqueN(swor_bootstrap.effort$TRIP_ID) != sum(adp_strata_N$N)) stop("Count of TRIP_IDs doesn't match!")
    
    # Adjust the TRIP_TARGET_DATE and LANDING_DATE to be for the current year
    swor_bootstrap.effort[, ':=' (
      TRIP_TARGET_DATE = as.Date(yday(TRIP_TARGET_DATE), origin = paste0(adp_years_to_predict[j], "-01-01")) - 1 ,
      LANDING_DATE =  as.Date(yday(LANDING_DATE), origin = paste0(adp_years_to_predict[j], "-01-01")) - 1
    )]
    # Due to leap years, cram any dates that got pushed to the next to to the previous year, Dec 31
    swor_bootstrap.effort[year(TRIP_TARGET_DATE) == (adp_years_to_predict[j] + 1), TRIP_TARGET_DATE := TRIP_TARGET_DATE - 1 ]
    
    # Define boxes of bootstrapped effort
    swor_bootstrap.box <- define_boxes_gs(
      swor_bootstrap.effort, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
      year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), geom = F, ps_cols = c("GEAR"))
    
    swor_bootstrap.allo_lst <- list(effort = unique(swor_bootstrap.effort[, .(ADP, STRATA, BSAI_GOA, TRIP_ID, DAYS)])[
    ][, STRATA := paste0(STRATA, "-", BSAI_GOA)
    ][, BSAI_GOA := NULL
    ][, .(STRATA_N = uniqueN(TRIP_ID), TRP_DUR = mean(DAYS)), keyby = .(ADP, STRATA)])
    
    # Calculate proximity at 15% sample rate (function is still called interspersion)
    swor_bootstrap.prox <- calculate_interspersion_gs(swor_bootstrap.box, sample_rate_vec = sample_rate_15)$ispn_dt
    
    # Calculate rates afforded with a $5.81M budget
    swor_bootstrap.rates <- allo_prox(swor_bootstrap.box, swor_bootstrap.allo_lst, cost_params, budget_lst[[1]], max_budget, index_interval = 0.0001)
    
    # Save iteration results
    swor_boot_lst[[k]] <- list(
      prox = swor_bootstrap.prox, 
      rates = swor_bootstrap.rates
    )
    
  }
  
  # Capture the results from each year
  swor_2_lst[[j]] <- swor_boot_lst
  
}

# save(swor_2_lst, file = "analyses/effort_prediction/swor_2_lst.rdata")

#======================================================================================================================#
# 1-year Sampling >Without< Replacement (modified) ----
#======================================================================================================================#

# Here, we use the 'current' 1-year of fishing to predict the next year a fishing, using the next year's STRATA_N (i.e.,
# we have a perfect prediction of the size of the next year of fishing effort.) If we downsample, it's a simple SWoR, and
# if we have to upsample, we sample all trips once (or more if needed), then the remainder with SWoR again.

adp_years <- unique(fixed.val_2015_2023_dt[order(ADP), ADP])  # 2015 - 2023
adp_years_to_predict <- adp_years[-1]                     # Will use 2015-2017 to predict 2018, and so on.
swor_1_lst <- vector(mode = "list", length = length(adp_years_to_predict))
set.seed(12345)
for(j in seq_along(adp_years_to_predict)) {
  # j <- 1
  
  cat("Year ", adp_years_to_predict[j], "\n")
  
  # Subset the data to prior year(s)
  adp_years_to_pull <- adp_years_to_predict[j] - (1)
  effort_prior <- fixed.val_2015_2023_dt[ADP %in% adp_years_to_pull]
  effort_prior[, ADP := adp_years_to_predict[j]]            # ADP is the year our prediction is for
  effort_prior_lst <- split(effort_prior[order(STRATA, BSAI_GOA)], by = c("STRATA", "BSAI_GOA"))
  
  # Use actual trip counts as our 'predicted' number of trips to sample to create yearly populations (perfect prediction of effort)
  adp_strata_N <- fixed.val_2015_2023_dt[ADP == adp_years_to_predict[j], .(N = uniqueN(TRIP_ID)), keyby = .(STRATA, BSAI_GOA)]
  adp_strata_N[, STRATUM_COL := paste0(STRATA, ".", BSAI_GOA)] 
  
  # Make sure names and ordering are the same
  if(!identical(names(effort_prior_lst), adp_strata_N$STRATUM_COL)) stop("Stratum names/order are not the same!")
  
  # Initialize bootstrap list
  swor_boot_lst <- vector(mode = "list", length = bootstrap_iter)
  
  for(k in seq_len(bootstrap_iter)) {
    # k <- 1
    cat(k, ", ")
    
    # Bootstrap using adp_strata_N to sample each stratum's population size size
    swor_bootstrap.effort <- rbindlist(Map(
      function(prior, strata_N) {
        # prior <- effort_prior_lst[[1]]; strata_N <- adp_strata_N$N[1]
        # Sample trips WITHOUT replacement
        trip_ids <- unique(prior$TRIP_ID)
        
        # How many times does prior effort go into future effort?
        prior_vs_future <- floor(strata_N / length(trip_ids))
        # What number of trips should be sampled without replacement?
        swr_n <- strata_N - (length(trip_ids) * prior_vs_future)
        # Create dt of trip_ids
        sampled_trip_ids <- data.table(
          TRIP_ID = c(
            # Repeat trip_id prior_vs_future times
            rep(trip_ids, times = prior_vs_future), 
            # Sample without replacement using swr_n
            sample(trip_ids, size = swr_n, replace = F)
          )
        )
        sampled_trip_ids[, I := .I]
        # Bring in each trip's data
        bootstrap_sample <- prior[sampled_trip_ids, on = .(TRIP_ID), allow.cartesian = T]
      }, 
      prior = effort_prior_lst,
      strata_N = adp_strata_N$N
    ))
    
    # Re-assign trip_id so that we can differentiate trips sampled multiple times
    swor_bootstrap.effort[, TRIP_ID := .GRP, keyby = .(ADP, STRATA, BSAI_GOA, TRIP_ID, I)]
    if(uniqueN(swor_bootstrap.effort$TRIP_ID) != sum(adp_strata_N$N)) stop("Count of TRIP_IDs doesn't match!")
    
    # Adjust the TRIP_TARGET_DATE and LANDING_DATE to be for the current year
    swor_bootstrap.effort[, ':=' (
      TRIP_TARGET_DATE = as.Date(yday(TRIP_TARGET_DATE), origin = paste0(adp_years_to_predict[j], "-01-01")) - 1 ,
      LANDING_DATE =  as.Date(yday(LANDING_DATE), origin = paste0(adp_years_to_predict[j], "-01-01")) - 1
    )]
    # Due to leap years, cram any dates that got pushed to the next to to the previous year, Dec 31
    swor_bootstrap.effort[year(TRIP_TARGET_DATE) == (adp_years_to_predict[j] + 1), TRIP_TARGET_DATE := TRIP_TARGET_DATE - 1 ]
    
    # Define boxes of bootstrapped effort
    swor_bootstrap.box <- define_boxes_gs(
      swor_bootstrap.effort, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
      year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), geom = F, ps_cols = c("GEAR"))
    
    swor_bootstrap.allo_lst <- list(effort = unique(swor_bootstrap.effort[, .(ADP, STRATA, BSAI_GOA, TRIP_ID, DAYS)])[
    ][, STRATA := paste0(STRATA, "-", BSAI_GOA)
    ][, BSAI_GOA := NULL
    ][, .(STRATA_N = uniqueN(TRIP_ID), TRP_DUR = mean(DAYS)), keyby = .(ADP, STRATA)])
    
    # Calculate proximity at 15% sample rate (function is still called interspersion)
    swor_bootstrap.prox <- calculate_interspersion_gs(swor_bootstrap.box, sample_rate_vec = sample_rate_15)$ispn_dt
    
    # Calculate rates afforded with a $5.81M budget
    swor_bootstrap.rates <- allo_prox(swor_bootstrap.box, swor_bootstrap.allo_lst, cost_params, budget_lst[[1]], max_budget, index_interval = 0.0001)
    
    # Why is it taking 4 minutes whereas it took less than one on the original dataset!? That's basically as long as it was before...
    # I had to change the index vector to 0.001 because 0.005 kept breaking (lowest budget was still higher than budget in initial...)
    # In the function, I need to do a better job of initializing the range...
    
    swor_boot_lst[[k]] <- list(
      prox = swor_bootstrap.prox, 
      rates = swor_bootstrap.rates
    )
    
  }
  
  # Capture the results from each year
  swor_1_lst[[j]] <- swor_boot_lst
  
}

# save(swor_1_lst, file = "analyses/effort_prediction/swor_1_lst.rdata")


# og_prox, og_rates, swr_1_lst, swor_3_lst, swor_2_lst, swor_1_lst

# Split bootstrap results into 15% proximity and allocated selection rates

swr_1_prox_dt <-  rbindlist(lapply(swr_1_lst, function(x) rbindlist(lapply(x, "[[", "prox"), idcol = "ITER")))
swr_1_rates_dt <-  rbindlist(lapply(swr_1_lst, function(x) rbindlist(lapply(x, "[[", "rates"), idcol = "ITER")))

swor_3_prox_dt <-  rbindlist(lapply(swor_3_lst, function(x) rbindlist(lapply(x, "[[", "prox"), idcol = "ITER")))
swor_3_rates_dt <-  rbindlist(lapply(swor_3_lst, function(x) rbindlist(lapply(x, "[[", "rates"), idcol = "ITER")))

swor_2_prox_dt <-  rbindlist(lapply(swor_2_lst, function(x) rbindlist(lapply(x, "[[", "prox"), idcol = "ITER")))
swor_2_rates_dt <-  rbindlist(lapply(swor_2_lst, function(x) rbindlist(lapply(x, "[[", "rates"), idcol = "ITER")))

swor_1_prox_dt <-  rbindlist(lapply(swor_1_lst, function(x) rbindlist(lapply(x, "[[", "prox"), idcol = "ITER")))
swor_1_rates_dt <-  rbindlist(lapply(swor_1_lst, function(x) rbindlist(lapply(x, "[[", "rates"), idcol = "ITER")))


#======================================================================================================================#
## Proximity ----
#======================================================================================================================#

# Summarize all results for 2018-2023 (omit 2015-2017 as we can't make comparisons for those years)

# How does Proximity at a 15% selection rate compare vs actual?
plot_prox_raw <- ggplot(swr_1_prox_dt[ADP >= 2018], aes(x = as.character(ADP), y = ISPN)) + 
  facet_grid(BSAI_GOA ~ STRATA, scales = "free") +
  geom_line(data = og_prox$ispn_dt[ADP >= 2018 & SAMPLE_RATE == 0.15], aes(group = interaction(STRATA, BSAI_GOA))) +
  geom_violin(color = "cyan3", draw_quantiles = 0.5, fill = NA) + 
  geom_violin(color = "blue",    draw_quantiles = 0.5, fill = NA, data = swor_3_prox_dt[ADP >= 2018]) +
  geom_violin(color = "purple",  draw_quantiles = 0.5, fill = NA, data = swor_2_prox_dt[ADP >= 2018]) +
  geom_violin(color = "magenta", draw_quantiles = 0.5, fill = NA, data = swor_1_prox_dt[ADP >= 2018]) +
  geom_point(data = og_prox$ispn_dt[ADP >= 2018 & SAMPLE_RATE == 0.15 ], fill = "green", shape = 21, size = 2) +
  geom_text(data = og_prox$ispn_dt[ADP >= 2018 & SAMPLE_RATE == 0.15 ], aes(label = STRATA_N, y = 1), size = 2, vjust = 0) +
  labs(x = "Year", y = "Proximity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
# Cyan and Magenta (swr_1 and swor_1) Are generally very similar and overlapping, swor_1 having a tighter distribution.



prox_res <- rbind(
  #cbind(Method = "Actual", og_prox$ispn_dt[ADP >= 2018 & SAMPLE_RATE == 0.15 ]),
  cbind(Method = "swr_1", swr_1_prox_dt[ADP >= 2018]),
  cbind(Method = "swor_3", swor_3_prox_dt[ADP >= 2018]),
  cbind(Method = "swor_2", swor_2_prox_dt[ADP >= 2018]),
  cbind(Method = "swor_1", swor_1_prox_dt[ADP >= 2018]),
  fill = T
)
rates_res <-rbind(
  #cbind(Method = "Actual", og_rates[ADP >= 2018]),
  cbind(Method = "swr_1", swr_1_rates_dt[ADP >= 2018]),
  cbind(Method = "swor_3", swor_3_rates_dt[ADP >= 2018]),
  cbind(Method = "swor_2", swor_2_rates_dt[ADP >= 2018]),
  cbind(Method = "swor_1", swor_1_rates_dt[ADP >= 2018]),
  fill = T
)[, .(Method, STRATA, BSAI_GOA, STRATUM_COL, ADP, SAMPLE_RATE, n, INDEX, ITER)]

boot_res <- rates_res[prox_res, on = .(Method, ADP, STRATA, BSAI_GOA, ITER)]

# Here, we can see how each method differed relative to the actual (line and point)
plot_prox_res <- ggplot(boot_res, aes(x = as.character(ADP), y = ISPN)) + 
  facet_nested(STRATA + BSAI_GOA ~ Method) +
  geom_line(data = og_prox$ispn_dt[ADP >= 2018 & SAMPLE_RATE == 0.15], aes(group = interaction(STRATA, BSAI_GOA))) +
  geom_violin(aes(color = Method), draw_quantiles = 0.5, fill = NA, position = "identity") + 
  geom_point(data = og_prox$ispn_dt[ADP >= 2018 & SAMPLE_RATE == 0.15]) + 
  geom_text(data = og_prox$ispn_dt[ADP >= 2018 & SAMPLE_RATE == 0.15], aes(label = STRATA_N), size = 2, vjust = 0, nudge_y = 0.1) +
  scale_color_manual(values = c(swr_1 = "cyan3", swor_3 =  "blue", swor_2 = "purple", swor_1 = "magenta")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x = "Year", y = "Proximity")

plot_prox_res_free_scale <- ggplot(boot_res, aes(x = as.character(ADP), y = ISPN)) + 
  facet_nested(STRATA + BSAI_GOA ~ Method, scales = "free") +
  geom_line(data = og_prox$ispn_dt[ADP >= 2018 & SAMPLE_RATE == 0.15], aes(group = interaction(STRATA, BSAI_GOA))) +
  geom_violin(aes(color = Method), draw_quantiles = 0.5, fill = NA, position = "identity") + 
  geom_point(data = og_prox$ispn_dt[ADP >= 2018 & SAMPLE_RATE == 0.15]) + 
  geom_text(data = og_prox$ispn_dt[ADP >= 2018 & SAMPLE_RATE == 0.15], aes(label = STRATA_N), size = 2, vjust = 0, nudge_y = 0.02) +
  scale_color_manual(values = c(swr_1 = "cyan3", swor_3 =  "blue", swor_2 = "purple", swor_1 = "magenta")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x = "Year", y = "Proximity")

plot_prox_lst <- unify_plot_widths(plot_lst = list(original = plot_prox_res, free = plot_prox_res_free_scale))


# Here, we can see how each method differed relative to the actual (line and point)
plot_rates_res <- ggplot(boot_res[!is.na(SAMPLE_RATE)], aes(x = as.character(ADP), y = SAMPLE_RATE)) + 
  facet_nested(STRATA + BSAI_GOA ~ Method) +
  geom_line(data = og_rates[ADP >= 2018], aes(group = interaction(STRATA, BSAI_GOA))) +
  geom_violin(aes(color = Method), draw_quantiles = 0.5, fill = NA, position = "identity", na.rm = T) + 
  geom_point(data = og_rates[ADP >= 2018]) + 
  geom_text(data = og_rates[ADP >= 2018], aes(label = STRATA_N), size = 2, vjust = 0, nudge_y = 0.1) +
  scale_color_manual(values = c(swr_1 = "cyan3", swor_3 =  "blue", swor_2 = "purple", swor_1 = "magenta")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x = "Year", y = "Sample Rate")  

plot_rates_free_scale <-  ggplot(boot_res[!is.na(SAMPLE_RATE)], aes(x = as.character(ADP), y = SAMPLE_RATE)) + 
  facet_nested(STRATA + BSAI_GOA ~ Method, scales = "free") +
  geom_line(data = og_rates[ADP >= 2018], aes(group = interaction(STRATA, BSAI_GOA))) +
  geom_violin(aes(color = Method), draw_quantiles = 0.5, fill = NA, position = "identity", na.rm = T) + 
  geom_point(data = og_rates[ADP >= 2018]) + 
  geom_text(data = og_rates[ADP >= 2018], aes(label = STRATA_N), size = 2, vjust = 0, nudge_y = 0.02) +
  scale_color_manual(values = c(swr_1 = "cyan3", swor_3 =  "blue", swor_2 = "purple", swor_1 = "magenta")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x = "Year", y = "Sample Rate")

plot_rates_lst <- unify_plot_widths(plot_lst = list(original = plot_rates_res, free = plot_rates_free_scale))

# swor_1 has by far the least variability among iterations (no surprise)
# swor_2 and swor_3 have 

#======================================================================================================================#
## Rates ----
#======================================================================================================================#

# How well do predicted rates match realized rates?
plot_rates_raw <- ggplot(swr_1_rates_dt[ADP >= 2018], aes(x = as.character(ADP), y = SAMPLE_RATE)) + 
  facet_grid(BSAI_GOA ~ STRATA, scales = "free") +
  geom_line(data = og_rates[ADP >= 2018], aes(group = interaction(STRATA, BSAI_GOA))) +
  geom_violin(color = "cyan3", draw_quantiles = 0.5, fill = NA) + 
  geom_violin(color = "blue",    draw_quantiles = 0.5, fill = NA, data = swor_3_rates_dt[ADP >= 2018]) +
  geom_violin(color = "purple",  draw_quantiles = 0.5, fill = NA, data = swor_2_rates_dt[ADP >= 2018]) +
  geom_violin(color = "magenta", draw_quantiles = 0.5, fill = NA, data = swor_1_rates_dt[ADP >= 2018]) +
  geom_point(data = og_rates[ADP >= 2018], fill = "green", shape = 21, size = 2) +
  geom_text(data = og_rates[ADP >= 2018], aes(label = STRATA_N, y = 0), size = 2, vjust = 0) +
  labs(x = "Year", y = "Proximity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
# Overall there are fewer differences between the methods in terms of allocated rates.
# The biggest exceptions are for OB_TRW in the GOA and BSAI in 2021, where all methods overestimated rates relative to 'actual'

#======================================================================================================================#
## Compare ----
#======================================================================================================================#

### Proximity ----

og_prox_dt <- og_prox$ispn_dt[SAMPLE_RATE == 0.15, .(ADP, STRATA, BSAI_GOA, STRATA_N, ISPN)]
boot_prox_dt <- rbind(
  cbind(METHOD = "swr_1", swr_1_prox_dt[, .(ADP, STRATA, BSAI_GOA, PROX = ISPN)]),
  cbind(METHOD = "swor_3", swor_3_prox_dt[, .(ADP, STRATA, BSAI_GOA, PROX = ISPN)]),
  cbind(METHOD = "swor_2", swor_2_prox_dt[, .(ADP, STRATA, BSAI_GOA, PROX = ISPN)]),
  cbind(METHOD = "swor_1", swor_1_prox_dt[, .(ADP, STRATA, BSAI_GOA, PROX = ISPN)]))
boot_prox_dt[, METHOD := factor(METHOD, levels = c("swr_1", "swor_3", "swor_2", "swor_1"))]
boot_prox_dt[, ACTUAL := og_prox_dt[boot_prox_dt, ISPN, on = .(ADP, STRATA, BSAI_GOA)]]
boot_prox_dt[, DIFF := PROX - ACTUAL]
boot_prox_dt[, PERC_DIFF := (PROX - ACTUAL)/ACTUAL]
# Merge STRATA_N in
boot_prox_dt[, STRATA_N := og_prox_dt[boot_prox_dt, STRATA_N, on = .(ADP, STRATA, BSAI_GOA)]]

# Proximity is much more difficult to predict in the BSAI (strata with fewer trips) than in the GOA. 
plot_prox_diff <- ggplot(boot_prox_dt[ADP >= 2018], aes(x = as.character(ADP), y = DIFF)) + 
  facet_grid(BSAI_GOA ~ STRATA) + 
  geom_hline(yintercept = 0) +
  geom_boxplot(aes(color = METHOD), fill = NA) + 
  geom_text(data = og_prox_dt[ADP >= 2018], aes(label = STRATA_N, y = 0.4), size = 2, vjust = 0) +
  scale_color_manual(values = c(swr_1 = "cyan3", swor_3 = "blue", swor_2 = "purple", swor_1 = "magenta")) + 
  labs(x = "Year", y = "Diff in Proximity (Boot - Actual)", color = "Method") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# Raw difference in proximity for well within 5% for GOA strata, but generally within 10% for BSAI strata, but 
# OB_TRW-BSAI is highly variable and can vary +/- 0.3 points! Very small stratum

plot_prox_perc_diff <- ggplot(boot_prox_dt[ADP >= 2018], aes(x = as.character(ADP), y = PERC_DIFF)) + 
  facet_grid(BSAI_GOA ~ STRATA) + 
  geom_hline(yintercept = 0) +
  geom_boxplot(aes(color = METHOD), fill = NA) + 
  geom_text(data = og_prox_dt[ADP >= 2018], aes(label = STRATA_N, y = 0.4), size = 2, vjust = 0) +
  scale_color_manual(values = c(swr_1 = "cyan3", swor_3 = "blue", swor_2 = "purple", swor_1 = "magenta")) + 
  labs(x = "Year", y = "% Diff in Proximity (Boot - Actual)/Actual", color = "Method") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# Percent difference in proximity can be vastly different for some methods in certain years (especially for 1-year methods
# following a very low effort year).
# How these differences in proximity affect allocation remains to be seen, but given that the allocation will prioritize 
# allocation to these very small strata first, it might not have a huge impact.

# Difference in Prox vs 'true', using mean across iterations
plot_prox_diff_mean <- ggplot(boot_prox_dt[ADP >= 2018, .(MEAN_DIFF = mean(DIFF)), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)]) + 
  facet_nested(. ~ STRATA + BSAI_GOA) + 
  geom_hline(yintercept = 0) + 
  geom_line(aes(x = as.character(ADP), y = MEAN_DIFF, color = METHOD, group = interaction(METHOD, STRATA, BSAI_GOA)), linewidth = 1) + 
  scale_color_manual(values = c(swr_1 = "cyan3", swor_3 = "blue", swor_2 = "purple", swor_1 = "magenta")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x = "Year", y = "Prox (Boot - Actual)", color = "Method") +
  geom_text(data =og_prox_dt[ADP >= 2018], aes(label = STRATA_N, x = as.character(ADP)), y = 0.3, size = 2)
# All methods generally get prox within 0.1, if not 0.05. Exception is OB_TRW. Notice that stratum size changes greatly 
# between years. Bouncing from 31 to 14 to 37 to 15 will has a large impact on predicting proximity. However, we are unlikely
# to predict any such dramatic changes.


### Rates ----

og_rates_dt <- og_rates[, .(ADP, STRATA, BSAI_GOA, STRATA_N, SAMPLE_RATE)]
boot_rates_dt <- rbind(
  cbind(METHOD = "swr_1", swr_1_rates_dt[, .(ADP, STRATA, BSAI_GOA, SAMPLE_RATE)]),
  cbind(METHOD = "swor_3", swor_3_rates_dt[, .(ADP, STRATA, BSAI_GOA, SAMPLE_RATE)]),
  cbind(METHOD = "swor_2", swor_2_rates_dt[, .(ADP, STRATA, BSAI_GOA, SAMPLE_RATE)]),
  cbind(METHOD = "swor_1", swor_1_rates_dt[, .(ADP, STRATA, BSAI_GOA, SAMPLE_RATE)]))
boot_rates_dt[, METHOD := factor(METHOD, levels = c("swr_1", "swor_3", "swor_2", "swor_1"))]
boot_rates_dt[, ACTUAL := og_rates_dt[boot_rates_dt, SAMPLE_RATE, on = .(ADP, STRATA, BSAI_GOA)]]
boot_rates_dt[, DIFF := SAMPLE_RATE - ACTUAL]
boot_rates_dt[, PERC_DIFF := (SAMPLE_RATE - ACTUAL)/ACTUAL]
boot_rates_dt[, STRATA_N := og_rates_dt[boot_rates_dt, STRATA_N, on = .(ADP, STRATA, BSAI_GOA)]]


# 
# plot_rates_diff <- ggplot(boot_rates_dt[ADP >= 2018], aes(x = as.character(ADP), y = DIFF)) + 
#   facet_grid(BSAI_GOA ~ STRATA) + 
#   geom_hline(yintercept = 0) +
#   geom_boxplot(aes(color = METHOD), fill = NA) + 
#   geom_text(data = og_rates_dt[ADP >= 2018], aes(label = STRATA_N, y = 0.10), size = 2, vjust = 0) +
#   scale_color_manual(values = c(swr_1 = "cyan3", swor_3 = "blue", swor_2 = "purple", swor_1 = "magenta")) + 
#   labs(x = "Year", y = "Diff in Sample Rate (Boot - Actual)", color = "Method") + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
#   geom_text(data = og_rates_dt, aes(label = STRATA_N), y = 0.5, size = 2)
# As far as actual rates go, most have medians within 0.05, or 5%. 

# plot_rates_perc_diff <- ggplot(boot_rates_dt[ADP >= 2018], aes(x = as.character(ADP), y = PERC_DIFF)) + 
#   facet_grid(BSAI_GOA ~ STRATA) + 
#   geom_hline(yintercept = 0) +
#   geom_boxplot(aes(color = METHOD), fill = NA) + 
#   geom_text(data = og_rates_dt[ADP >= 2018], aes(label = STRATA_N, y = 0.40), size = 2, vjust = 0) +
#   scale_color_manual(values = c(swr_1 = "cyan3", swor_3 = "blue", swor_2 = "purple", swor_1 = "magenta")) + 
#   labs(x = "Year", y = "% Diff in Sample Rate (Boot - Actual)/Actual", color = "Method") + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# Percent differences for all methods are generally within 10% (with largest exception for 2021 OB-TRW GOA)

# Difference in Rates vs 'true', using mean across iterations
plot_rates_diff_mean <- ggplot(boot_rates_dt[ADP >= 2018, .(MEAN_DIFF = mean(DIFF)), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)]) + 
  facet_nested(. ~ STRATA + BSAI_GOA) + 
  geom_hline(yintercept = 0) + 
  geom_line(aes(x = as.character(ADP), y = MEAN_DIFF, color = METHOD, group = interaction(METHOD, STRATA, BSAI_GOA)), linewidth = 1) + 
  scale_color_manual(values = c(swr_1 = "cyan3", swor_3 = "blue", swor_2 = "purple", swor_1 = "magenta")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x = "Year", y = "Sample Rate (Boot - Actual)", color = "Method") +
  geom_text(data = og_rates_dt[ADP >= 2018], aes(x = as.character(ADP), label = STRATA_N), y = 0.075, size = 2)

# Can see that for most strata, predicted rates are generally with 2.5%, mostly within 5%

ggplot(boot_rates_dt[ADP >= 2018, .(MEAN_DIFF = mean(PERC_DIFF)), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)]) + 
  facet_nested(. ~ STRATA + BSAI_GOA) + 
  geom_hline(yintercept = 0) + 
  geom_line(aes(x = as.character(ADP), y = MEAN_DIFF, color = METHOD, group = interaction(METHOD, STRATA, BSAI_GOA)), linewidth = 1) + 
  scale_color_manual(values = c(swr_1 = "cyan3", swor_3 = "blue", swor_2 = "purple", swor_1 = "magenta")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x = "Year", y = "% Diff in Sample Rate (Boot - Actual)", color = "Method")
# In terms of percent differences, all methods generally prescribe rates within 10% of where they should be, exception
# being OB_TRW-GOA in 2021/2022. 

# [NOTE] Why is 2021 higher than expected for all methods? How is that possible with the same budget?
# How can these cost the same? Literally all strata are higher?
og_rates[ADP == 2021, .(STRATA, BSAI_GOA, STRATA_N, SAMPLE_RATE, n, INDEX_COST)]
swr_1_rates_dt[ADP == 2021, .(SAMPLE_RATE = mean(SAMPLE_RATE), n = mean(n), INDEX_COST = mean(INDEX_COST)), keyby = .(ADP, STRATA, BSAI_GOA, STRATA_N)]
# TODO Adding d to the output of rates would be useful.

# calculate_cost_prox(x, cost_params, allo_lst, max_budget)$INDEX_COST
# Is trip duration vastly different in 2020 vs 2021? I see this pattern in all methods, regardless of # of years used in bootstrapping.
dcast(fixed_fmp.allo_lst$effort, STRATA ~ ADP, value.var = "TRP_DUR")
# 2021 was actually higher than 2020, so 'realized' rates would be lower, leading to overestimates in my bootstrapping
plot_trip_duration <- ggplot(fixed_fmp.allo_lst$effort[!(STRATA %like% "ZERO|EM_TRW")], aes(x = ADP, y = TRP_DUR, color = STRATA)) + 
  geom_line(linewidth = 2) + geom_point(size = 3) +
  labs(x = "Year", y = "Average Trip Duration (days)", color = "Stratum")
# Trip duration definitely jumped sine 2019, but mostly for BSAI trips.
# This kind of phenomenon isn't something we can ever plan for. # of trips is one thing, but change in fishing behavior itself
# is another.



### Bias/Variance in difference in Proximity ----
# boot_prox_dt[ADP >= 2018, mean(DIFF), keyby = .(METHOD)]  # swor_3 had the highest bias. swor_1 had the least
# boot_prox_dt[ADP >= 2018, var(DIFF), keyby = .(METHOD)]   # swor_2 had the least variance. swor_1 and swr_1 had the highest
# 
# boot_prox_dt[ADP >= 2018, mean(PERC_DIFF), keyby = .(METHOD)] # swor_1 best, then swr_1
# boot_prox_dt[ADP >= 2018, var(PERC_DIFF), keyby = .(METHOD)]  # swor_2 best, then swor_3

boot_prox_dt[ADP >= 2018, mean(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, sum(V1), keyby = .(METHOD)]  # swor_1 least bias, worse with more years
boot_prox_dt[ADP >= 2018, var(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, sum(V1), keyby = .(METHOD)]   # swor_1 least variance, gets worse with more years

boot_prox_dt[ADP >= 2018, mean(PERC_DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, sum(V1), keyby = .(METHOD)] # swor_1 best
boot_prox_dt[ADP >= 2018, mean(PERC_DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, sum(V1), keyby = .(METHOD)] # swor_1 best

ggplot(boot_prox_dt[ADP >= 2018, mean(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)], aes(x = interaction(STRATA, BSAI_GOA), y = METHOD, fill = V1)) + 
  geom_tile() + scale_fill_gradient2() + labs(fill = "Bias of Prox") + facet_grid(. ~ ADP) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + scale_y_discrete(limits = rev)
ggplot(boot_prox_dt[ADP >= 2018, var(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)], aes(x = interaction(STRATA, BSAI_GOA), y = METHOD, fill = V1)) + 
  geom_tile() + scale_fill_gradient2() + labs(fill = "Variance of Prox")+ facet_grid(. ~ ADP) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + scale_y_discrete(limits = rev)
# In 2021, Bias of proximity to OB_TRW-BSAI was overestimated by all methods.
# Don't really use 2023, since it's an incomplete year and we're sampling much fewer trips
# All of the variance in difference of proximity is in the BSAI.
# TODO Omit 2023?


# Don't use these, use what is below
# Bias/Variance in raw difference Rates
# boot_rates_dt[ADP >= 2018, mean(DIFF), keyby = .(METHOD)]  # swor_3 had the highest bias. swr_1 actually had the least! swor_1 was better than swor_2 or swor_3
# boot_rates_dt[ADP >= 2018, var(DIFF), keyby = .(METHOD)]   # swor_3 had the least overall variance, followed closely by swor_1. swr_1 had the highest variance
# # The fact that swor_1 hast he least variance is not surprising - It has the fewest number of trips to sample from between iterations, 
# # and because it's not sampling with replacement, 
# boot_rates_dt[ADP >= 2018, mean(PERC_DIFF), keyby = .(METHOD)] # swor_1 best, then swr_1
# boot_rates_dt[ADP >= 2018, var(PERC_DIFF), keyby = .(METHOD)]  # swor_1 best, then swor_3

dcast(boot_rates_dt[ADP >= 2018, round(mean(DIFF), 4), keyby = .(METHOD, ADP)], METHOD ~ ADP, value.var = "V1")
dcast(boot_rates_dt[ADP >= 2018, round(mean(DIFF), 4), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)], METHOD + STRATA + BSAI_GOA ~ ADP, value.var = "V1")

# stratum-specific differences
# Why are all rates in 2021 higher than what was 'idealized'? We should have perfect knowledge of effort...?
ggplot(boot_rates_dt[ADP >= 2018, mean(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)], aes(x = interaction(STRATA, BSAI_GOA), y = METHOD, fill = V1)) + 
  geom_tile() + scale_fill_gradient2() + labs(x = "Strata", y = "Method", fill = "Bias of Rates") + facet_grid(. ~ ADP) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + scale_y_discrete(limits = rev)
# For each year, rates rates are all within 0.08 points, less than 1%. 2022 was actually pretty good , especially for swor_1

# TODO This probably the best plot? Why doesn't it have the least overall variance summarized by method? Cancelled out since the
# variance is calculated across iterations and strata simultanously?
ggplot(boot_rates_dt[ADP >= 2018, var(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)], aes(x = interaction(STRATA, BSAI_GOA), y = METHOD, fill = V1)) + 
  geom_tile() + scale_fill_gradient2() + labs(x= "Strata", y = "Method", fill = "Variance of Rates") + facet_grid(. ~ ADP) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + scale_y_discrete(limits = rev)
# In terms of rates, swor_1 had consistently lower variance of residuals

# Summing variance across strata and years (first taknig aveage across iterations)
boot_rates_dt[ADP >= 2018, mean(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, sum(V1), keyby = .(METHOD)]  # swr_1 least bias, followed by swor_1
boot_rates_dt[ADP >= 2018, var(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, sum(V1), keyby = .(METHOD)]   # swor_1 least variance

boot_rates_dt[ADP >= 2018, mean(PERC_DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, sum(V1), keyby = .(METHOD)] # swr_1 least bias, follwoed by swor_1
boot_rates_dt[ADP >= 2018, var(PERC_DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, sum(V1), keyby = .(METHOD)]  # swor_1 least variance

# TODO summarize these 

cbind(
  boot_prox_dt[ADP >= 2018, mean(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, .(PROX_TOTAL_DIFF = sum(V1)), keyby = .(METHOD)],  # swor_1 least bias, worse with more years
  boot_prox_dt[ADP >= 2018, var(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, .(PROX_VAR_DIFF = sum(V1)), keyby = .(METHOD)][, -"METHOD"],   # swor_1 least variance, gets worse with more years
  boot_prox_dt[ADP >= 2018, mean(PERC_DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, .(PROX_TOTAL_PERC_DIFF = sum(V1)), keyby = .(METHOD)][, -"METHOD"], # swor_1 best
  boot_prox_dt[ADP >= 2018, var(PERC_DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, .(PROX_VAR_PERC_DIFF = sum(V1)), keyby = .(METHOD)][, -"METHOD"], # swor_1 best
  boot_rates_dt[ADP >= 2018, mean(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, .(RATES_TOTAL_DIFF = sum(V1)), keyby = .(METHOD)][, -"METHOD"],  # swr_1 least bias, followed by swor_1
  boot_rates_dt[ADP >= 2018, var(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, .(RATES_VAR_DIFF = sum(V1)), keyby = .(METHOD)][, -"METHOD"],   # swor_1 least variance
  boot_rates_dt[ADP >= 2018, mean(PERC_DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, .(RATES_TOTAL_PERC_DIFF = sum(V1)), keyby = .(METHOD)][, -"METHOD"], # swr_1 least bias, follwoed by swor_1
  boot_rates_dt[ADP >= 2018, var(PERC_DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, .(RATES_VAR_DIFF = sum(V1)), keyby = .(METHOD)[, -"METHOD"]]  # swor_1 least variance
)

# First, average difference vs actual across iterations (prox and rates)
# Then, across years and strata, calculate the bias (mean difference)
# boot_metrics <- rbind(
#   melt(
#     boot_prox_dt[ADP >= 2018, .(
#       METRIC = "PROX", MEAN_DIFF = mean(DIFF), VAR_DIFF = var(DIFF), MEAN_PERC_DIFF = mean(PERC_DIFF), VAR_PERC_DIFF = var(PERC_DIFF)
#     ), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)
#     ][, lapply(.SD, sum), .SDcols = c("MEAN_DIFF", "VAR_DIFF", "MEAN_PERC_DIFF", "VAR_PERC_DIFF"), keyby = .(METRIC, METHOD)],
#     id.vars = c("METRIC", "METHOD")),
#   melt(
#     boot_rates_dt[ADP >= 2018, .(
#       METRIC = "RATES", MEAN_DIFF = mean(DIFF), VAR_DIFF = var(DIFF), MEAN_PERC_DIFF = mean(PERC_DIFF), VAR_PERC_DIFF = var(PERC_DIFF)
#     ), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)
#     ][, lapply(.SD, sum), .SDcols = c("MEAN_DIFF", "VAR_DIFF", "MEAN_PERC_DIFF", "VAR_PERC_DIFF"), keyby = .(METRIC, METHOD)],
#     id.vars = c("METRIC", "METHOD"))
# )

# # Calculate average difference and percent difference relative to 'actual'
# boot_prox_dt[ADP >= 2018, .(
#   METRIC = "PROX", MEAN_DIFF = mean(DIFF), MEAN_PERC_DIFF = mean(PERC_DIFF)
# ), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)
# ][, .(
#   MEAN = mean(MEAN_DIFF),
#   MEAN_P = mean(MEAN_PERC_DIFF),
#   VAR = var(MEAN_DIFF),
#   VAR_P = var(MEAN_PERC_DIFF)), keyby = .(METHOD)]  # swor_1 has the lowest bias but highest variance. Typical bias-variance tradeoff


# TODO USE THESE METRICS
# MSE : sum((estimate - true value)^2) / (n)
prox_tbl <- boot_prox_dt[
][ADP >= 2018, .(BOOT_MEAN = mean(PROX)), keyby = .(ADP, STRATA, BSAI_GOA, METHOD, ACTUAL)
][, .(
  BIAS = mean(BOOT_MEAN - ACTUAL), 
  #MAPE = mean(abs(BOOT_MEAN - ACTUAL) / abs(ACTUAL))  * 100,
  MSE = mean((BOOT_MEAN - ACTUAL)^2)),
  keyby = .(METHOD)] %>%
  flextable() %>% flextable::colformat_double(j = 2:3, digits = 5)
# PROX: swor_1 has the least bias in proximity, but also the highest MSE (but not large difference)


rates_tbl <- boot_rates_dt[
][ADP >= 2018, .(BOOT_MEAN = mean(SAMPLE_RATE)), keyby = .(ADP, STRATA, BSAI_GOA, METHOD, ACTUAL)
][, .(
  BIAS = mean(BOOT_MEAN - ACTUAL), 
  #MAPE = mean(abs(BOOT_MEAN - ACTUAL) / abs(ACTUAL))  * 100,
  MSE = mean((BOOT_MEAN - ACTUAL)^2)),
  keyby = .(METHOD)] %>%
  flextable() %>% flextable::colformat_double(j = 2:3, digits = 5)




# calcualte metrics by stratum across years
boot_prox_dt[
][ADP >= 2018, .(BOOT_MEAN = mean(PROX)), keyby = .(ADP, STRATA, BSAI_GOA, METHOD, ACTUAL)
][, .(
  BIAS = mean(BOOT_MEAN - ACTUAL),
  MPE = 100 * mean((BOOT_MEAN - ACTUAL)/ACTUAL),
  MSE = mean((BOOT_MEAN - ACTUAL)^2)),
  keyby = .(METHOD, STRATA, BSAI_GOA)] %>%
  dcast(STRATA + BSAI_GOA ~ METHOD, value.var = c("BIAS", "MPE", "MSE"))

# Best method has the fewest dark tiles

stratum_prox_bias_error <- 
  melt(
    boot_prox_dt[
    ][ADP >= 2018, .(BOOT_MEAN = mean(PROX)), keyby = .(ADP, STRATA, BSAI_GOA, METHOD, ACTUAL)
    ][, .(
      BIAS = mean(BOOT_MEAN - ACTUAL),
      #MAPE = mean(abs(BOOT_MEAN - ACTUAL)/abs(ACTUAL)) * 100,
      MSE = mean((BOOT_MEAN - ACTUAL)^2)),
      keyby = .(METHOD, STRATA, BSAI_GOA)],
    id.vars = c("METHOD", "STRATA", "BSAI_GOA"), value.var = c("BIAS", "MSE") # Removed MAPE
  )[, relative_value := value/max(abs(value)), by = .(variable, STRATA, BSAI_GOA)][]
plot_stratum_prox_bias_error <- ggplot(stratum_prox_bias_error, aes(x = METHOD, y = paste0(STRATA, "-", BSAI_GOA), fill = relative_value)) + 
  facet_grid(. ~ variable) + geom_tile() + scale_fill_gradient2() + geom_text(aes(label = formatC(value, digits = 4, format = "f") )) + 
  labs(x = "Method", y = "Stratum", fill = "Relative value", subtitle = "Proximity")

stratum_rates_bias_error <- 
  melt(
    boot_rates_dt[
    ][ADP >= 2018, .(BOOT_MEAN = mean(SAMPLE_RATE)), keyby = .(ADP, STRATA, BSAI_GOA, METHOD, ACTUAL)
    ][, .(
      BIAS = mean(BOOT_MEAN - ACTUAL),
      MSE = mean((BOOT_MEAN - ACTUAL)^2)),
      keyby = .(METHOD, STRATA, BSAI_GOA)],
    id.vars = c("METHOD", "STRATA", "BSAI_GOA"), value.var = c("BIAS", "MSE")
  )[, relative_value := value/max(abs(value)), by = .(variable, STRATA, BSAI_GOA)][]
plot_stratum_rates_bias_error <- ggplot(stratum_rates_bias_error, aes(x = METHOD, y = paste0(STRATA, "-", BSAI_GOA), fill = relative_value)) + 
  facet_grid(. ~ variable) + geom_tile() + scale_fill_gradient2() + geom_text(aes(label = formatC(value, digits = 4, format = "f") )) + 
  labs(x = "Method", y = "Stratum", fill = "Relative value", subtitle = "Sample Rates")
# Although overall bias for swr_1 was lowest, we can see that it's because the bias of EM_FIXED-BSAI offsets other strata.


#======================================================================================================================#
## Save outputs for effort_bootstrapping.Rmd ----
#======================================================================================================================#

save(
  plot_prox_lst, plot_rates_lst, plot_trip_duration, plot_prox_diff_mean, plot_rates_diff_mean,
  plot_stratum_prox_bias_error, plot_stratum_rates_bias_error, prox_tbl, rates_tbl, 
  file = "analyses/effort_prediction/effort_bootstrap.rdata")

#======================================================================================================================#



boot_rates_dt[
][ADP >= 2018, .(BOOT_MEAN = mean(SAMPLE_RATE)), keyby = .(ADP, STRATA, BSAI_GOA, METHOD, ACTUAL)
][, .(BIAS = mean(BOOT_MEAN - ACTUAL), MSE = mean((BOOT_MEAN - ACTUAL)^2)), keyby = .(METHOD)]
# RATES: swr_1 has the least bias by far. swor_1 still has low bias, but also with lower MSE than swr_1



# Mean Percentage Error
# 100% / n * sum(actual - predict)/actual


boot_rates_dt[ADP >= 2018, .(
  METRIC = "PROX", MEAN_DIFF = mean(DIFF), MEAN_PERC_DIFF = mean(PERC_DIFF)
), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)
][, .(
  MEAN = mean(MEAN_DIFF),
  MEAN_P = mean(MEAN_PERC_DIFF),
  VAR = var(MEAN_DIFF),
  VAR_P = var(MEAN_PERC_DIFF)), keyby = .(METHOD)]  # swor_1 has the lowest bias but highest variance. Typical bias-variance tradeoff
# For swor_1, realized rates are on average within 0.35%

# FIXME I DONT THINK THIS IS RIGHT
# Scale weighted value relative to maximum of each metric. 
boot_metrics[, w_value := sign(value) * (abs(value) / max(abs(value))), keyby = .(METRIC, variable)]
boot_metrics[, MEAN_VAR := gsub("_DIFF|_PERC_DIFF", "", variable)]
boot_metrics[, RAW_PERC := ifelse(variable %like% "PERC", "PERC_DIFF", "RAW_DIFF")]
boot_metrics[, RAW_PERC := factor(RAW_PERC, levels = c("RAW_DIFF", "PERC_DIFF"))]
plot_boot_metrics <- ggplot(boot_metrics, aes(x = RAW_PERC, y = METHOD, fill = w_value)) + 
  facet_nested(. ~ METRIC + MEAN_VAR) + geom_tile() + 
  scale_fill_gradient2() + scale_y_discrete(limits = rev) + 
  labs(x = "Total (sum) difference relative to 'Actual'", y = "Method", fill = "Relative Difference") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  geom_text(aes(label = round(value, 3)), size = 3)
# For either metric, zero is the best value, and the metric with the farthest value from zero gets a -1 or 1
plot_boot_metrics
# swor_3 and swor_2 generally have the biggest issues and tend to underestimate proximity. swor_1 had the best predictions
# of proximity, 



#======================================================================================================================#
# Proportions of fishing effort (trips)_ ----
#======================================================================================================================#

a <- fixed.val_2015_2023_dt[, .(N = uniqueN(TRIP_ID)), keyby = .(ADP, STRATA, BSAI_GOA)]
a[, EM := STRATA %like% "EM"]
a[, STRATUM_COL := paste0(STRATA, "-", BSAI_GOA)]
# With ZERO (large pink portions)
ggplot(a, aes(x = as.character(ADP), y = N, fill = STRATUM_COL)) + 
  geom_col(position = "fill", aes(color = EM)) + 
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "red"))
# without zero
ggplot(a[!(STRATA %like% "ZERO")], aes(x = as.character(ADP), y = N, fill = STRATUM_COL)) + 
  geom_col(position = "fill", aes(color = EM)) + 
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "red"))
# Facetiing, splitting EM out
ggplot(a[!(STRATA %like% "ZERO")], aes(x = as.character(ADP), y = N, fill = STRATUM_COL)) + 
  geom_col(position = "fill", aes(color = EM)) + 
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "red")) + 
  facet_grid(EM ~ .)
# Noticeable increases in proportion of OB_FIXED-GOA since 2019
# without EM_TRW
ggplot(a[!(STRATA %like% "ZERO|EM_TRW")], aes(x = as.character(ADP), y = N, fill = STRATUM_COL)) + 
  geom_col(position = "fill", aes(color = EM)) + 
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "red")) + 
  facet_grid(EM ~ .)
