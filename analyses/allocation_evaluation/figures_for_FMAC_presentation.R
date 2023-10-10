# figures_for_FMAC_presentation

#======================================================================================================================#
# Data Prep ------------------------------------------------------------------------------------------------------------
#======================================================================================================================#

#====================#
## Load Packages ----
#===================#

library(data.table)         # Data wrangling
library(ggplot2)            # Plotting
library(sf)                 # Spatial analyses
library(dplyr)              # Data wrangling sf objects
library(flextable)          # Nicer tables in RMarkdown
library(officer)            # Better formatting of flextables
library(gridExtra)          # For combining plots and saving multiple figures to a single PDF
library(ggpattern)          # For adding patterned fills to ggplot.
#   https://cran.r-project.org/web/packages/ggpattern/vignettes/developing-patterns.html for advanced customization


#================#
## Load Data  ----
#================#

# Fishing effort data from 2018 to 2022
load("analyses/allocation_evaluation/allocation_evaluation.RData")   # loads pc_effort_dt
val_2018_2022_dt <- pc_effort_dt[ADP %in% 2018:2022]

# Load the ADFG statistical area shapefile. '../' is needed for Rmarkdown to climb into parent folders.
stat_area_sf <- st_read(
  dsn = "source_data/ADFG_Stat_Area_shapefile/PVG_Statewide_2001_Present_GCS_WGS1984.shp", quiet = T) %>%
  select(STAT_AREA) %>%
  st_transform(crs = 3467)

# Load the Alaska map sf objects
load("source_data/ak_shp.rdata")      # shp_land, shp_nmfs, and shp_centroids added to global

# Prepare trips_melt object
# trips_melt has metrics and trip duration for all trips >= 2015. . 
# Add ADP year, STRATA, and DAYS of each TRIP_ID to trips_melt
trips_melt_add_data <- unique(pc_effort_dt[, .(ADP, TRIP_ID, STRATA, DAYS)])
trips_melt <- trips_melt_add_data[trips_melt, on = .(TRIP_ID)]
setcolorder(trips_melt, c("ADP", "STRATA", "TRIP_ID", "DAYS", "Metric", "Value"))
setkey(trips_melt, ADP, STRATA, TRIP_ID, DAYS, Metric, Value)
# Error-check
if(nrow(trips_melt[, .N, by = .(TRIP_ID)][N != 3])) stop("At least one trip has number of metrics != 3!")

#================#
## Trip Costs ----
#================#

# Use average values from the 2023 Final ADP for now. Also, roughly using trip_end - trip_start + 1 for now until we get
# a better handle on future monitoring costs.
ob_cpd <- 4896623 / 3094         # $1582.619/day
fgem_cpd <- 1e6 / (1040+735)     # $ 563.380/day
trwem_cpd <- 600                 # $600/day according to JF estimates used in set_budget.R in 2022 Final ADP repo
# Get average trip length for all strata, using TRIP_END - TRIP_START + 1
# This function counts number of unique days between each trip target date and landing date
# use this to count days for non-observed trips, which already have a modeled 'DAYS' estimate.
# day_count <- function(x) {
#   length(unique(unlist(apply(x, 1, function(y) as.numeric(as.Date(y[1])) : as.numeric(as.Date(y[2])), simplify = F))))
# }
# val_2018_2022_dt[POOL != "OB", DAYS := day_count(.SD), by = .(TRIP_ID), .SDcols = c("TRIP_TARGET_DATE", "LANDING_DATE")]
trip_cost_dt <- unique(val_2018_2022_dt[, .(ADP, STRATA, TRIP_ID, DAYS)])[, .(MEAN_TRIP_DAYS = mean(DAYS)), by = .(ADP, STRATA)]
# trip_cost_dt <- unique(val_2018_2022_dt[STRATA != "ZERO", .(ADP, STRATA, TRIP_ID, START, END)])[
# ][, .(MEAN_TRIP_DAYS = mean(as.numeric(END - START, units = "days") + 1)), by = .(ADP, STRATA)]
# Merge in day costs
trip_cost_dt[, CPD := fcase(
  STRATA == "EM_TRW", trwem_cpd,
  STRATA %in% c("EM_POT", "EM_HAL"), fgem_cpd,
  STRATA %in% c("OB_HAL", "OB_POT", "OB_TRW"), ob_cpd)]
# Calculate cost per trip
trip_cost_dt[, CPT := MEAN_TRIP_DAYS * CPD]
trip_cost_dt[STRATA == "ZERO", ':=' (CPD = 0, CPT = 0)]

#====================#
## Load Functions ----
#====================#

source("analyses/allocation_evaluation/functions.R")   

#======================================================================================================================#
# Analyses  ------------------------------------------------------------------------------------------------------------
#======================================================================================================================#

#==================#
## Define Boxes ----
#==================#

# Box definition without stratifying by FMP
system.time(box_res <- define_boxes(
  val_2018_2022_dt, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = "STRATA", dmn_cols = c("BSAI_GOA", "GEAR"), geom = T))

# Box definition with FMP stratified, split by BSAI and GOA
system.time(box_res_fmp <- define_boxes(
  val_2018_2022_dt, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), dmn_cols = c("GEAR"), geom = T ))

#==============#
## Allocate ----
#==============#

# Assume $4.5 M budget. Will focus on 2022 fishing effort only.

### Equal Rates ----

# Prepare dataset for equal rates and status quo
er_sq_dt <- unique(val_2018_2022_dt[POOL == "OB", .(ADP, STRATA, TRIP_ID, DAYS)])[
][, .(STRATA_N = uniqueN(TRIP_ID), TRP_DUR = mean(DAYS)), by = .(ADP, STRATA)
][, PRIOR_MTD := unique(
  trips_melt[, .(ADP, STRATA, TRIP_ID, DAYS)])[
  ][ADP %in% (.BY[[1]]-(1:3)) & STRATA == .BY[[2]], mean(DAYS)], 
  by = .(ADP, STRATA)]
# Merge in estimated cost-per-day
er_sq_dt[, CPD := trip_cost_dt[er_sq_dt, CPD, on =  .(ADP, STRATA)]]
setkey(er_sq_dt, ADP, STRATA)
# Allocate for equal rates to observer strata
equal_allocation_res <- allo_equal(er_sq_dt, ob_budget = 4.5e6 - 1e6)


### Status Quo ----

# Use minimum_plus_opt allocation, with 50% confidence that each stratum will reach the 15% minimum hurdle
status_quo_050_res <- allo_min_plus_opt(er_sq_dt, conf = 0.50, tm = trips_melt, ob_budget = 3.5e6)

# Use adjusted minimum_plus_opt allocation, with 95% confidence that each stratum will reach the 15% minimum hurdle
status_quo_095_res <- allo_min_plus_opt(er_sq_dt, conf = 0.95, tm = trips_melt, ob_budget = 3.5e6)


### Proximity ----

sample_rate_vec <- seq(from = 0.005, to = 0.95, by = 0.0001)

# First, get the metrics for proximity method 
system.time(ispn_res <- calculate_interspersion(box_res, sample_rate_vec, omit_strata = "ZERO"))
system.time(index_res <- calculate_index(ispn_res, trip_cost_dt))
budget_tbl <- data.table(INDEX_COST = 4.5e6) # Now, we have to assume a budget to see what rates were afforded
group_cols <- unlist(index_res$params[c("year_col", "stratum_cols")], use.names = F)  # Extract grouping columns
prox_rates_4.5M <- index_res$rates[
][, .SD[budget_tbl, on = .(INDEX_COST), roll = "nearest"], by = group_cols]  # Allocated rates for proximity method


### Proximity w/ FMP ----

system.time(ispn_res_fmp <- calculate_interspersion(box_res_fmp, sample_rate_vec, omit_strata = "ZERO"))
system.time(index_res_fmp <- calculate_index(ispn_res_fmp, trip_cost_dt))
group_cols <- unlist(index_res_fmp$params[c("year_col", "stratum_cols")], use.names = F)  # Extract grouping columns
prox_rates_4.5M_fmp <- index_res_fmp$rates[
][, .SD[budget_tbl, on = .(INDEX_COST), roll = "nearest"], by = group_cols]  # Allocated rates for proximity method


### Cost-Weighted Boxes ----

# What rate would be afforded in 2022 using equal rates and a $4.5M budget? This acts as the starting point.
equal_rate_2022 <- trip_cost_dt[box_res$strata_n_dt, on = .(ADP, STRATA)
][ADP == 2022 , .(STRATA, STRATA_N, CPT)
][, EQUAL_RATE := 4.5e6 / sum(STRATA_N*CPT)][, unique(EQUAL_RATE)] # 0.1480 afforded for all strata
# Calculate probability that each box is sampled
cwb_prop <- calculate_cwb_Ph(box_res, sample_rate_vec)
# Find the Ph that would be achieved by the equal rates
equal_rate_Ph <- cwb_prop$Ph_dt[SAMPLE_RATE == round(equal_rate_2022, 3)][STRATA != "ZERO"]
equal_rate_Ph[, CPT := trip_cost_dt[equal_rate_Ph, CPT, on = .(ADP, STRATA)]]  # Merge in cost per trip

# Make first run at allocation
allo_cwb(equal_rate_Ph, target_budget = 4.5e6)


val_2018_2022_dt[ADP == 2022, .(N = uniqueN(TRIP_ID)), keyby = .(STRATA)]
val_2018_2022_dt[ADP == 2022, .(N = uniqueN(TRIP_ID)), keyby = .(STRATA, BSAI_GOA)]


#======================================================================================================================#
# Count of trips with multiple gear types ------------------------------------------------------------------------------
#======================================================================================================================#

gear_n <- unique(val_2018_2022_dt[, .(POOL, ADP, STRATA, TRIP_ID, AGENCY_GEAR_CODE)])
gear_n[AGENCY_GEAR_CODE %in% c("NPT", "PTR"), AGENCY_GEAR_CODE := "TRW"]
gear_n <- unique(gear_n)
gear_n <- dcast(gear_n, ADP + POOL + STRATA + TRIP_ID ~ AGENCY_GEAR_CODE, value.var = "AGENCY_GEAR_CODE", fill = "")
gear_n <- gear_n[, .(TRIPS = uniqueN(TRIP_ID)), by = .(ADP, POOL, STRATA, HAL, JIG, POT, TRW)]
gear_n[, LABEL := gsub("  ", " ", trimws(paste0(c(HAL, JIG, POT, TRW), collapse = " "), which = "both")), by = 1:nrow(gear_n)]
gear_n[, HATCH := ifelse(LABEL %in% c("HAL", "JIG", "POT", "TRW"), "plain", "patterned")]
gear_n[, POOL := fcase(POOL == "EM", "EM", POOL == "OB", "Observer", POOL == "ZE", "Zero")]

FMAC_fig_mixed_gear_trips <- ggplot(gear_n[ADP == 2022], aes(y = STRATA, x = TRIPS)) + 
  facet_grid(POOL ~ ., scales = "free_y", space = "free_y") + 
  geom_col_pattern(
    mapping = aes(fill = LABEL, pattern = HATCH), 
    pattern_fill = "orange", pattern_density = 0.4, position = "fill", color = "black") + 
  scale_pattern_manual(values = c("crosshatch", "none"), guide = "none") +
  scale_fill_viridis_d() +
  guides(fill = guide_legend(override.aes = list(
    pattern = c("none", "crosshatch", "none", "none", "none"),
    pattern_fill = c("none", "orange", "none", "none", "none")))) +
  labs(x = "Proportion of Trips", fill = "Gear Types", pattern = "Gear Types", y = "Pool and Strata") +
  scale_x_continuous(breaks = seq(0, 1, 0.2))

ggsave(filename = "analyses/allocation_evaluation/figures/FMAC_fig_mixed_gear_trips.png", FMAC_fig_mixed_gear_trips, width = 7, height = 5, units = "in", dpi = 600)

# Split NPT from PTR
gear_n2 <- unique(val_2018_2022_dt[, .(POOL, ADP, STRATA, TRIP_ID, AGENCY_GEAR_CODE)])
gear_n2 <- unique(gear_n2)
gear_n2 <- dcast(gear_n2, ADP + POOL + STRATA + TRIP_ID ~ AGENCY_GEAR_CODE, value.var = "AGENCY_GEAR_CODE", fill = "")
gear_n2 <- gear_n2[, .(TRIPS = uniqueN(TRIP_ID)), by = .(ADP, POOL, STRATA, HAL, JIG, POT, NPT, PTR)]
gear_n2[, LABEL := gsub("  ", " ", trimws(paste0(c(HAL, JIG, POT, NPT, PTR), collapse = " "), which = "both")), by = 1:nrow(gear_n2)]
gear_n2[, HATCH := ifelse(LABEL %in% c("HAL", "JIG", "POT", "NPT", "PTR"), "plain", "patterned")]
gear_n2[, POOL := fcase(POOL == "EM", "EM", POOL == "OB", "Observer", POOL == "ZE", "Zero")]

ggplot(gear_n2[ADP == 2022], aes(y = STRATA, x = TRIPS)) + 
  facet_grid(POOL ~ ., scales = "free_y", space = "free_y") + 
  geom_col_pattern(
    mapping = aes(fill = LABEL, pattern = HATCH), 
    pattern_fill = "orange", pattern_density = 0.4, position = "fill", color = "black") + 
  scale_pattern_manual(values = c("crosshatch", "none"), guide = "none") +
  scale_fill_viridis_d() +
  guides(fill = guide_legend(override.aes = list(
    pattern = c("none", "crosshatch", "none", "none", "crosshatch", "none", "none"),
    pattern_fill = c("none", "orange", "none", "none", "orange", "none", "none")))) +
  labs(x = "Proportion of Trips", fill = "Gear Types", pattern = "Gear Types", y = "Pool and Strata") +
  scale_x_continuous(breaks = seq(0, 1, 0.2))

# Percentage of trips mixed within fixed-gear
gear_n2[ADP == 2022 & (HAL=="HAL" | POT=="POT"), sum(TRIPS[HATCH == "patterned"]) / sum(TRIPS), by =.(POOL)]
# Percentage of trips mixed within TRW
gear_n2[ADP == 2022 & (NPT=="NPT" | PTR=="PTR"), sum(TRIPS[HATCH == "patterned"]) / sum(TRIPS), by =.(POOL)]



# TEST
ggplot(gear_n[ADP == 2022], aes(y = STRATA, x = TRIPS)) + 
  facet_grid(POOL ~ ., scales = "free_y", space = "free_y") + 
  geom_col_pattern(
    mapping = aes(fill = LABEL, pattern = HATCH), 
    pattern_fill = "orange", pattern_density = 0.4, position = "fill", color = "black") + 
  scale_pattern_manual(values = c("stripe", "none"), guide = "none") +
  scale_fill_viridis_d() +
  guides(fill = guide_legend(override.aes = list(
    pattern = c("none", "stripe", "none", "none", "none"),
    pattern_fill = c("none", "orange", "none", "none", "none")))) +
  labs(x = "Proportion of Trips", fill = "Gear Types", pattern = "Gear Types", y = "Pool and Strata") +
  scale_x_continuous(breaks = seq(0, 1, 0.2))



#======================================================================================================================#
# Count of trips in multiple FMPs --------------------------------------------------------------------------------------
#======================================================================================================================#

# Split by AI BS and GOA
fmp_n <- val_2018_2022_dt[ADP == 2022]
fmp_n_bs_ai_goa <- unique(fmp_n[, FMP := fcase(
  AREA %in% 541:543, "AI",
  AREA %in% 500:540, "BS",
  AREA %in% 600:660, "GOA")
  ][, .(TRIP_ID, POOL, STRATA, FMP)])
fmp_n_bs_ai_goa <- dcast(fmp_n_bs_ai_goa, POOL + STRATA + TRIP_ID ~ FMP, value.var = "FMP", fill = "")
fmp_n_bs_ai_goa <- fmp_n_bs_ai_goa[, .(TRIPS = uniqueN(TRIP_ID)), by = .(POOL, STRATA, AI, BS, GOA)]
fmp_n_bs_ai_goa[, LABEL := gsub("  ", " ", trimws(paste0(c(AI, BS, GOA), collapse = " "), which = "both")), by = 1:nrow(fmp_n_bs_ai_goa)]
fmp_n_bs_ai_goa[, HATCH := ifelse(LABEL %in% c("AI", "BS", "GOA"), "plain", "patterned")]
fmp_n_bs_ai_goa[, POOL := fcase(POOL == "EM", "EM", POOL == "OB", "Observer", POOL == "ZE", "Zero")]

FMAC_fig_mixed_fmp_trips <- ggplot(fmp_n_bs_ai_goa, aes(y = STRATA, x = TRIPS)) + 
  facet_grid(POOL ~ ., scales = "free_y", space = "free_y") + 
  geom_col_pattern(
    mapping = aes(fill = LABEL, pattern = HATCH), 
    pattern_fill = "orange", pattern_density = 0.25, position = "fill", color = "black") + 
  scale_pattern_manual(values = c("crosshatch", "none"), guide = "none") +
  scale_fill_viridis_d() +
  guides(fill = guide_legend(override.aes = list(
    pattern = c("none", "crosshatch", "crosshatch", "crosshatch", "none", "crosshatch", "none"),
    pattern_fill = c("none", "orange", "orange", "orange", "none", "orange", "none")))) +
  labs(x = "Proportion of Trips", fill = "FMPs", pattern = "FMPs", y = "Pool and Strata") 

ggsave(filename = "analyses/allocation_evaluation/figures/FMAC_fig_mixed_fmp_trips.png", FMAC_fig_mixed_fmp_trips, width = 7, height = 4, units = "in", dpi = 600)

# Percentages
fmp_n_bs_ai_goa[, .(LABEL, PERC = 100 * TRIPS / sum(TRIPS)), by = .(POOL, STRATA)]
fmp_n_bs_ai_goa[, .(PERC = 100 * sum(TRIPS[HATCH == "patterned"]) / sum(TRIPS)), by = .(POOL, STRATA)]

# If splitting by BSAI vs GOA
fmp_n_bsai_goa <- unique(fmp_n[, FMP := fcase(
  AREA < 600, "BSAI",
  AREA >= 600, "GOA")
][, .(TRIP_ID, POOL, STRATA, FMP)])
fmp_n_bsai_goa <- dcast(fmp_n_bsai_goa, POOL + STRATA + TRIP_ID ~ FMP, value.var = "FMP", fill = "")
fmp_n_bsai_goa <- fmp_n_bsai_goa[, .(TRIPS = uniqueN(TRIP_ID)), by = .(POOL, STRATA, BSAI, GOA)]
fmp_n_bsai_goa[, LABEL := gsub("  ", " ", trimws(paste0(c(BSAI, GOA), collapse = " "), which = "both")), by = 1:nrow(fmp_n_bsai_goa)]
fmp_n_bsai_goa[, HATCH := ifelse(LABEL %in% c("BSAI", "GOA"), "plain", "patterned")]
fmp_n_bsai_goa[, POOL := fcase(POOL == "EM", "EM", POOL == "OB", "Observer", POOL == "ZE", "Zero")]
fmp_n_bsai_goa[, .(LABEL, PERC = 100 * TRIPS / sum(TRIPS)), by = .(POOL, STRATA)]
fmp_n_bsai_goa[, .(PERC = 100 * sum(TRIPS[HATCH == "patterned"]) / sum(TRIPS)), by = .(POOL, STRATA)]


#======================================================================================================================#
# Evaluation Figure ----------------------------------------------------------------------------------------------------
#======================================================================================================================#

stratum_cols_og <- box_res$params$stratum_cols
stratum_dt_og <- unique(box_res$dmn$strata_dmn_n_dt[, ..stratum_cols_og])  #Get all stratum_cols in dataset
setorderv(stratum_dt_og, colnames(stratum_dt_og))
stratum_dt_og[, STRATUM_ID := .I]
acceptor_donor_lst_og <- c(
  rep(list(4:5), times = 2),                # 1-2: EM_HAL and EM_POT 
  list(3),                                  # 3: EM_TRW
  rep(list(4:5), times = 2),                # 4-5: OB_HAL and OB_POT
  list(6),                                  # 6: OB_TRW
  list(4:5)                                 # 7: ZERO
)
names(acceptor_donor_lst_og) <- apply(stratum_dt_og[, ..stratum_cols_og], 1, function(x) paste0(x, collapse = "."))

dmn_insp_prox <- calculate_dmn_interspersion3(box_res, prox_rates_4.5M, stratum_dt_og, acceptor_donor_lst_og)


dmn_res_pool_dt <- copy(dmn_insp_prox)
design_desc <- NULL

year_col <- dmn_res_pool_dt$params$year_col
if(!is.null(design_desc)) design_desc <- paste0("Design: ", design_desc, "; ")

# Plot-ready data set, which groups by 4-week blocks?
pool_4wk <- copy(dmn_res_pool_dt$POOLED)
pool_4wk[, TIME_4 := cut(TIME, seq(min(TIME), max(TIME), by = 4), include.lowest = T, labels = F)]
# Now average over the 4 weeks in each group using a weighted average based on the number of trip components of the receptor pool.
pool_4wk <- pool_4wk[, .(
  BOX_DMN_w = sum(BOX_DMN_w), 
  BOX_DONOR_SAMPLE_PROB = weighted.mean(BOX_DONOR_SAMPLE_PROB, w = BOX_DMN_w)), 
  by = c(year_col, "POOL", "GEAR", "HEX_ID", "TIME_4")]
# Merge geometry back in 
pool_4wk[, POOL := fcase(POOL == "EM", "EM", POOL == "OB", "Observer", POOL == "ZERO", "Zero")]
pool_4wk <- merge(dmn_res_pool_dt$geom, pool_4wk, by = "HEX_ID")

# Subset the data to focus on 2022 HAL effort, and only on OB overlapping EM and ZE pools
hal_2022_sub <- pool_4wk  %>% filter(GEAR == "HAL" & ADP == 2022 & TIME_4 %in% c(9,10))
# hal_2022_sub <- pool_4k  %>% filter(POOL != "Observer" & GEAR == "HAL" & ADP == 2022 & TIME_4 %in% c(9,10))

ak_map_cropped <- ak_low_res %>% st_set_crs(st_crs(hal_2022_sub))

FMAC_interspersion_plot <- ggplot() + 
  facet_grid(TIME_4 ~ GEAR + POOL) + 
  geom_sf(data = ak_map_cropped %>% st_crop(st_bbox(hal_2022_sub))) + 
  geom_sf(data = hal_2022_sub, aes(fill = BOX_DONOR_SAMPLE_PROB)) + 
  scale_fill_viridis_c(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
  theme(
    #legend.position = "bottom",
    axis.text = element_blank(), axis.ticks = element_blank(),
    panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(margin = margin(b = 0.05, t = 0.05))) + 
  labs(y = "Month", fill = "Observer\nsample\nprobability") 


ggsave(filename = "analyses/allocation_evaluation/figures/FMAC_interspersion_plot.png", FMAC_interspersion_plot, width = 8, height = 2.5, units = "in", dpi = 600)


#======================================================================================================================#
# FPC Figure  ----------------------------------------------------------------------------------------------------------
#======================================================================================================================#

# Index = INTERSPERSION / CV_SCALING    
# Both metrics scale from 0 to 1, so this allocation method weights both metrics equally. Both increase as the sampling
# rate increase, but how they do so may differ between strata. CV_scaling (or the variance scaling factor) can also be
# interpreted as the variance savings acquired from larger sample sizes according to the finite population correction.

fpc_tbl <- data.table(
  STRATA_N = rep(c(50, 200, 800), each = 50),
  SAMPLE_RATE = rep(seq(0.01, 0.5, by = 0.01), times = 3 )
)
fpc_tbl[
][, n := SAMPLE_RATE * STRATA_N
][, FPC := (STRATA_N - n) / STRATA_N
][, CV_SCALING := sqrt(FPC * (1/n))]

FMAC_cv_scaling_plot <- ggplot(fpc_tbl, aes(x = SAMPLE_RATE, y = CV_SCALING, color = as.factor(STRATA_N))) + 
  geom_line(linewidth = 2) + 
  scale_color_viridis_d() + labs(x = "Sample rate", y = "Variance Scaling Factor", color = "Stratum size\n(trips)") +
  theme_bw() + 
  #theme(legend.position = "bottom") + 
  geom_hline(yintercept = 0.2, linetype = 2) + 
  coord_cartesian(ylim = c(0, 0.7)) 
  
ggsave(filename = "analyses/allocation_evaluation/figures/FMAC_cv_scaling_plot.png", FMAC_cv_scaling_plot, width = 4, height = 2, units = "in", dpi = 600)

find_cv_scaling <- data.table(CV_SCALING = 0.2)
fpc_tbl[, .SD[find_cv_scaling, on = .(CV_SCALING), roll = "nearest"], by = .(STRATA_N)]

# CV Scaling is a measure of our uncertainty around a stratum-level estimate.
# For any given sample RATE, the smaller a stratum is, the more uncertainty you'll have about an estimate because the 
# SAMPLE SIZE will be lower. For example, a 20% sample rate in a 50-trip stratum is 10 trips, but a 20% sample rate in 
# an 800-trip stratum is a sample size of 160 trips!
# The proximity allocation method is in part weighted by CV scaling - It will balance interspersion with CV scaling such
# that even if strata are very clumped in time and space (and therefore require a small sample rate), if that stratum
# size is very small, CV_scaling will counteract it and require a higher sampling rate to reduce uncertainty due to 
# sample size issues.


#======================================================================================================================#
# Effort Maps  ---------------------------------------------------------------------------------------------------------
#======================================================================================================================#

effort_sub <- box_res$geom_sf %>% filter(ADP == 2022 & STRATA %in% c("OB_HAL", "ZERO") & TIME %in% c(16, 31, 38))
effort_sub <- effort_sub %>% 
  mutate(TIME = as.integer(factor(TIME))) %>% 
  mutate(STRATA = case_when(STRATA == "OB_HAL" ~ "Stratum: A", STRATA == "ZERO" ~ "Stratum: B"))
ak_map_effort <- ak_low_res %>% st_set_crs(st_crs(effort_sub)) %>% st_crop(st_bbox(effort_sub))

FMAC_ak_effort_example_plot <- ggplot() + 
  facet_grid(TIME ~ STRATA, labeller = labeller(TIME = function(x) paste0("Time: ", x))) + 
  geom_sf(data = ak_map_effort) + 
  geom_sf(data = effort_sub, aes(fill = BOX_n)) + 
  scale_fill_viridis_c() + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "bottom") +
  labs(fill = "# of Trips")

ggsave(filename = "analyses/allocation_evaluation/figures/FMAC_ak_effort_example_plot.png", FMAC_ak_effort_example_plot, width = 5, height = 4.5, units = "in", dpi = 600)








