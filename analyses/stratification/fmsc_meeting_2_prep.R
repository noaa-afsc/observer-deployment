#======================================================================================================================#
# Preparation ----------------------------------------------------------------------------------------------------------
#======================================================================================================================#

#===================#
## Load Packages ----
#===================#

library(data.table)         # Data wrangling
library(ggplot2)            # Plotting
library(grid)               # For unit.pmax  to get widths of grobs so that plots have matching dimensions
library(gridExtra)          # For arrangeGrob to combine plots
library(sf)                 # Spatial analyses
library(flextable)          # For print-ready tables
library(dplyr)              # For piping and handling sf objects

#=============================#
## Load data and data prep ----
#=============================#

# https://drive.google.com/file/d/1VWYiW8poe3jJq2r-3ZzunmuURvfwG2wF/view?usp=drive_link
load("analyses/allocation_evaluation/allocation_evaluation.Rdata")   # loads pc_effort_dt and trips_melt

stat_area_sf <- st_read(
  dsn = "source_data/ADFG_Stat_Area_shapefile/PVG_Statewide_2001_Present_GCS_WGS1984.shp", quiet = T) %>%
  select(STAT_AREA) %>%
  st_transform(crs = 3467)

# Load the Alaska map sf objects
load("source_data/ak_shp.rdata")      # shp_land, shp_nmfs, and shp_centroids added to global


#===============#
## Functions ----
#===============#

source("analyses/allocation_evaluation/functions.R")

# function for applying crs and bounding box of data to underlaying map
crop_map <- function(map, data) {
  bbox <- st_bbox(data)
  out <- st_set_crs(map, value = st_crs(data))
  out <- st_crop(out, xmin = bbox[[1]], ymin = bbox[[2]], xmax = bbox[[3]], ymax = bbox[[4]])
  out
}


#===================#
# How boxes look ----
#===================#

load("analyses/stratification/boxes_and_costs.rdata") # https://drive.google.com/file/d/1RJzz_CG2jut_L0V3V9zIt-kqK5ksWO8z/view?usp=drive_link

# TODO having the transformed scale_fill_viridis_c might not be a good idea for all figures!
map_theme_1 <- list(
  theme(legend.position = "bottom", legend.justification = "right", legend.key.width = unit(2, "cm"), axis.text = element_blank(), axis.ticks = element_blank()),
  facet_grid(TIME ~ STRATA, labeller = labeller(TIME = function(x) paste0("WEEK: ", x)))
)
ts_ex <- box_lst$CURRENT$geom_sf %>% filter(ADP == 2022 & STRATA %in% c("OB_HAL", "OB_POT", "OB_TRW") & TIME %in% 11:15)
ts_ex_map <- crop_map(ak_low_res, ts_ex)

# Calculate the weight of trips in each box (shows where fishing is distributed in time and space)
map_ob_box_w <- ggplot(ts_ex_map) + 
  geom_sf() + geom_sf(data = ts_ex, aes(fill = BOX_w), color = "black") + map_theme_1 + 
  labs(fill = expression(italic("w"["b"]))) + 
  scale_fill_viridis_c(trans = "sqrt")

# Count of unique trips in the neighborhood of each box (shows which boxes are likely or unlikely to be neighboring a sampled trip )
map_ob_box_nbr <- ggplot(ts_ex_map) + 
  geom_sf() + geom_sf(data = ts_ex, aes(fill = BOX_nbr), color = "black") + map_theme_1 + 
  labs(fill = expression(italic("t"["G"]))) + 
  scale_fill_viridis_c(trans = "sqrt")

# Assuming a 15% selection rate, what probability that a boxes' neighboring is unsampled?
ts_ex <- ts_ex %>% mutate(PROB = (1 - (1-0.15)^BOX_nbr))
map_ob_box_prob <- ggplot(ts_ex_map) + 
  geom_sf() + geom_sf(data = ts_ex, aes(fill = PROB), color = "black") + map_theme_1 

# If we average these probabilities, we get the expected proportion of boxes that would be sampled
ts_ex_Ph <- ts_ex %>% st_drop_geometry() %>% group_by(STRATA) %>% summarize(Ph = mean(1 - PROB))
ts_ex_Ph # 

map_ob_box_A <- ggplot(ts_ex_map) + 
  geom_sf() + geom_sf(data = ts_ex, aes(fill = (1 - PROB)), color = "black") + map_theme_1 + 
  labs(fill = expression(italic("A"["b"]))) + 
  scale_fill_viridis_c()

# If we then weight each box by BOX_w, the number of trips in each box, we get the expected number of trips near a neighbor (BOX_w * PROB)
# If we do a weighted average of our probabilities by BOX_w, we get the expected proportion of trips near a sampled neighbor
# This is the same as the sum of our counts of expected trips near a neighbor divided by total trips in the stratum, N
ts_ex_insp <- ts_ex %>% st_drop_geometry() %>% group_by(STRATA) %>% summarise(INSP = weighted.mean(PROB, w = BOX_w))
ts_ex_insp # As we can see, with a 15% selection rate, OB_TRW has the highest interspersion as expected, then OB_HAL, then OB_POT, the most diffuse.

# (1-A) * *box_w is the expected number of trips in a box that is in  the neighborhood of a sampled trip
map_ob_box_1_A_wb <- ggplot(ts_ex_map) + 
  geom_sf() + geom_sf(data = ts_ex, aes(fill = ((PROB) * BOX_w)), color = "black") + map_theme_1 + 
  labs(fill = expression(paste("(1 - ", italic("A"["b"]),")", " * ", italic("w"["b"])))) + 
  scale_fill_viridis_c(trans = "sqrt")


ggsave("analyses/stratification/figures/fmsc_2_map_ob_box_w.png",      plot = map_ob_box_w, width = 11, height = 6)
ggsave("analyses/stratification/figures/fmsc_2_map_ob_box_nbr.png",    plot = map_ob_box_nbr, width = 11, height = 6)
ggsave("analyses/stratification/figures/fmsc_2_map_ob_box_prob.png",   plot = map_ob_box_prob, width = 11, height = 6)
ggsave("analyses/stratification/figures/fmsc_2_map_ob_box_A.png",      plot = map_ob_box_A, width = 11, height = 6)
ggsave("analyses/stratification/figures/fmsc_2_map_ob_box_1_A_wb.png", plot = map_ob_box_1_A_wb, width = 11, height = 6)

# Data table preview version
a_dt <- setDT(ts_ex %>% st_drop_geometry())[
][, .SD[1:10, .(BOX_ID, HEX_ID, WEEK = TIME, Nb = BOX_n, wb = BOX_w, tG = BOX_nbr, Ab = 1 - PROB)], keyby = STRATA] 

ob_hal_prob_ft <- a_dt %>% filter(STRATA == "OB_HAL") %>%  flextable() %>% 
  padding(padding.top = 0, padding.bottom = 0, padding.left = 3, padding.right = 3, part = "all") %>% 
  colformat_int(j = c(2,3,4,5,7), big.mark = "") %>% colformat_double(j = c(6,8), digits = 4) %>%
  fontsize(size = 6, part = "all") %>% set_table_properties(width = 0, layout = "autofit")

ob_pot_prob_ft <- a_dt %>% filter(STRATA == "OB_POT") %>%  flextable() %>% 
  padding(padding.top = 0, padding.bottom = 0, padding.left = 3, padding.right = 3, part = "all") %>% 
  colformat_int(j = c(2,3,4,5,7), big.mark = "") %>% colformat_double(j = c(6,8), digits = 4) %>%
  fontsize(size = 6, part = "all") %>% set_table_properties(width = 0, layout = "autofit")

ob_trw_prob_ft <- a_dt %>% filter(STRATA == "OB_TRW") %>%  flextable() %>% 
  padding(padding.top = 0, padding.bottom = 0, padding.left = 3, padding.right = 3, part = "all") %>% 
  colformat_int(j = c(2,3,4,5,7), big.mark = "") %>% colformat_double(j = c(6,8), digits = 4) %>%
  fontsize(size = 6, part = "all") %>% set_table_properties(width = 0, layout = "autofit")

save_as_pptx(ob_hal_prob_ft, ob_pot_prob_ft, ob_trw_prob_ft, path = "analyses/stratification/figures/fmsc_2_slides.pptx")


# Make figure, showing that we calculate Ph (for CWB) and interspersion (for PROX) for a vector of assumed sample rates 0.0005 to 0.9500 by 0.0001
# sample rate on x-axis, y-axis is Ph or PROX
# For Prox, we also calculate the CV_scaling, can put this on the same figure.

# FOR CWB, it's a little trickier, need to iteratively adjust assumed sample rate before getting Ph. For 


# quick load here : https://drive.google.com/file/d/1pKLjF_Sa69OCUQ3nkzHT6uEN4sv1M8HM/view?usp=drive_link
load(file = "analyses/stratification/fmsc_2_data.rdata")

# Need cwb_prop and other objects to make figures of curves! Making them from stratification_allocation_test.R for now
cwb_curves <- ggplot(cwb_prop$Ph_dt[ADP == 2022 & STRATA != "ZERO"], aes(x = SAMPLE_RATE, y = Ph, color = STRATA)) + geom_line(linewidth = 1.5) + 
  labs(x = "Sample Rate", y = expression(italic("P"["h"])), color = "Strata") +
  theme(legend.position = "bottom")  + scale_x_continuous(breaks = seq(0, 1, 0.2)) 
# CWB allocates depends on weighting Ph, N, and Costs, the latter two being constant. However, Ph depends on the rate, so the process is circular.
# To remedy this, we first assume equal rates for all strata, lookup the Ph values and plug it into our allocation algorithm, which ultimately results in a new sample rate.
# We then halve the difference between our original assumed sample rate with the new sample rate, and use it as a new assumed sample rate before re-allocating.
# This process repeats around 6-7 times before it settles such that the assumed rate approaches the allocated rate for all strata.

prox_index_melt <- melt(prox_index$rates[ADP == 2022, .(STRATA, SAMPLE_RATE, Th = ISPN, VSFh = CV_SCALING, Dh = INDEX)], id.vars = c("STRATA", "SAMPLE_RATE"), measure.vars = c("Th", "VSFh", "Dh"))
prox_index_melt[, variable := factor(variable)]
levels(prox_index_melt$variable) <- c(
  expression(italic("T"["h"])), 
  expression(italic("VSF"["h"])), 
  expression(italic("D"["h"]))
)
prox_index_afforded <- prox_rates[ADP == 2022][, .(variable = as.factor("Dh"), value = mean(INDEX))]
levels(prox_index_afforded$variable) <- c(expression(italic("D"["h"])))

prox_curves <- ggplot(prox_index_melt, aes(x = SAMPLE_RATE, y = value, color = STRATA)) + 
  geom_line(linewidth = 1.5, alpha = 0.8) + labs(x = "Sample Rate", y = "Value", color = "Strata") + 
  facet_wrap(. ~ variable, scales = "free_y", dir = "v", ncol = 2, labeller = label_parsed) + 
  theme(legend.position = "bottom") + scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  geom_hline(data = prox_index_afforded, aes(yintercept = value), linetype = 2) 

ggsave(file = "analyses/stratification/figures/fmsc_2_cwb_curve.png", plot = cwb_curves, width = 4, height = 4, units = "in")
ggsave(file = "analyses/stratification/figures/fmsc_2_prox_curve.png", plot = prox_curves, width = 4, height = 4, units = "in")



# TESTING T * (1-VSF)
prox_index_melt <- melt(prox_index$rates[ADP == 2022, .(STRATA, SAMPLE_RATE, Th = ISPN, VSFh = 1-CV_SCALING, Dh = (ISPN * (1-CV_SCALING)) )], id.vars = c("STRATA", "SAMPLE_RATE"), measure.vars = c("Th", "VSFh", "Dh"))
prox_index_melt[, variable := factor(variable)]
levels(prox_index_melt$variable) <- c(
  expression(italic("T"["h"])), 
  expression(italic("VSF"["h"])), 
  expression(italic("D"["h"]))
)
test <- copy(prox_rates[ADP == 2022, ])
test[, Dh := ISPN * (1 - CV_SCALING)]
test[, INDEX2 := ISPN / CV_SCALING]
test[, INDEX3 := (ISPN + (1 - CV_SCALING))/2]
test[, INDEX4 := (ISPN  - CV_SCALING)]
levels(prox_index_afforded$variable) <- c(expression(italic("D"["h"])))

prox_curves <- ggplot(prox_index_melt, aes(x = SAMPLE_RATE, y = value, color = STRATA)) + 
  geom_line(linewidth = 1.5, alpha = 0.8) + labs(x = "Sample Rate", y = "Value", color = "Strata") + 
  facet_wrap(. ~ variable, scales = "free_y", dir = "v", ncol = 2, labeller = label_parsed) + 
  theme(legend.position = "bottom") + scale_x_continuous(breaks = seq(0, 1, 0.2)) #+
  #geom_hline(data = prox_index_afforded, aes(yintercept = value), linetype = 2) 


#====================#
# Get final rates ----
#====================#

# Make functions to get relevant metrics for each allocation method
mpo_tbl <- function(x, year = 2022) {
  x1 <- copy(x)[ADP == year, .(STRATUM_COL, STRATA, STRATA_N, RATE, W_hopt, Cost_Per_Trip = PRIOR_MTD * CPD)]
  x2 <- dcast(attr(x, "metrics")[ADP == year], STRATA ~ Metric, value.var = "S_h")
  x2[x1, on = .(STRATA)][, .(Strata = STRATUM_COL, N = STRATA_N, Rate = RATE, W = W_hopt, discard, chnk_psc, hlbt_psc, Cost_Per_Trip)]
}
mpo_ft <- function(x) {x %>% flextable() %>% colformat_double(j = c(5:8), digits = 2) %>% colformat_double(j = c(3,4), digits = 4) %>% bold(j = c(2, 4:8), part = "header") %>% color(j = 4, color = "forestgreen", part = "header")}

cwb_tbl <- function(x, year = 2022) {
  copy(x)[ADP == year, .(Strata = STRATUM_COL, N = STRATA_N, Rate = RATE, W = `nh/n`, n, `NP/C` = `NP/c_h`, P = Ph, Cost_Per_Trip = CPT)]
}
cwb_ft <- function(x) {x %>% flextable() %>% colformat_double(j = c(5:6,8), digits = 2) %>% colformat_double(j = c(3,4, 7), digits = 4) %>% bold(j = c(2,4,7,8), part = "header") %>% color(j = 4, color = "forestgreen", part = "header")}

prox_tbl <- function(x, year = 2022) {
  copy(x)[ADP == year, .(Strata = STRATUM_COL, N = STRATA_N, Rate = RATE, Index = INDEX, VSF = CV_SCALING, Interspersion = ISPN)]
}
prox_ft <- function(x) {x %>% flextable() %>% colformat_double(j = c(3:6), digits = 4) %>% bold(j = c(2,4,6), part = "header") %>% color(j = 4, color = "darkgreen", part = "header")}

# https://drive.google.com/file/d/1DnkWWMCCM9L97ccHXWTjVpB3iJ8NHzxa/view?usp=drive_link
load("analyses/stratification/rates_lst.rdata")  # loads rates_lst

# Equal Rates. Note that if we were to use MPO in 2022 for all strata, it would revert to equal rates.
er_tbl <- rates_lst$EQUAL.EQUAL[ADP == 2022, .(Strata = STRATA, N = STRATA_N, Rate = RATE)]
er_tbl %>%
  flextable() %>% colformat_double(j = 3, digits = 4) %>% bold(j = 2, part = "header") %>% color(j = 2, color = "darkgreen", part = "header")

# Note that MPO (SQ) is identical in 2022 for all stratification schemes because they all revert to equal rates (insufficient budget)
# `W`, the weight, is in terms of proportion of funds to allocate. Note that in MPO, W is never used because it reverts to Equal under our budget scenario.
mpo_tbl(rates_lst$CURRENT.MPO_SQ) %>% mpo_ft()
cwb_tbl(rates_lst$CURRENT.CWB) %>% cwb_ft()
prox_tbl(rates_lst$CURRENT.PROX) %>% prox_ft()

mpo_tbl(rates_lst$FMP.MPO_SQ_FMP) %>% mpo_ft()
cwb_tbl(rates_lst$FMP.CWB_FMP) %>% cwb_ft()
prox_tbl(rates_lst$FMP.PROX_FMP) %>% prox_ft()

mpo_tbl(rates_lst$FIXED_FMP.MPO_SQ_FIXED_FMP) %>% mpo_ft()
cwb_tbl(rates_lst$FIXED_FMP.CWB_FIXED_FMP_GS) %>% cwb_ft()
prox_tbl(rates_lst$FIXED_FMP.PROX_FIXED_FMP_GS) %>% prox_ft()

# Summary tables of rates across strata
smry_cols <- colnames(er_tbl)

rates_smry <- rbind(
  cbind(Stratification = "Current", rbind(
    cbind(Allo = "Equal Rates", er_tbl),
    cbind(Allo = "Status Quo", mpo_tbl(rates_lst$CURRENT.MPO_SQ)[, ..smry_cols]),
    cbind(Allo = "CWB", cwb_tbl(rates_lst$CURRENT.CWB)[, ..smry_cols]),
    cbind(Allo = "Proximity", prox_tbl(rates_lst$CURRENT.PROX)[, ..smry_cols])
  )),
  cbind(Stratification = "FMP", rbind(
    cbind(Allo = "Status Quo", mpo_tbl(rates_lst$FMP.MPO_SQ_FMP)[, ..smry_cols]),
    cbind(Allo = "CWB", cwb_tbl(rates_lst$FMP.CWB_FMP)[, ..smry_cols]),
    cbind(Allo = "Proximity", prox_tbl(rates_lst$FMP.PROX_FMP) [, ..smry_cols])
  )),
  cbind(Stratification = "FIXED_FMP", rbind(
    cbind(Allo = "Status Quo", mpo_tbl(rates_lst$FIXED_FMP.MPO_SQ_FIXED_FMP)[, ..smry_cols]),
    cbind(Allo = "CWB", cwb_tbl(rates_lst$FIXED_FMP.CWB_FIXED_FMP_GS)[, ..smry_cols]),
    cbind(Allo = "Proximity", prox_tbl(rates_lst$FIXED_FMP.PROX_FIXED_FMP_GS) [, ..smry_cols])
  ))
)

rates_smry_wide <- dcast(rates_smry, Stratification + Strata + N ~ Allo, value.var = "Rate", fill = mean(er_tbl$Rate))
rates_smry_wide

# so ideally I'd be able to crus these tables together...
gear_fmp_dt <- data.table(GEAR = rep(c("HAL", "POT", "TRW"), each = 2), FMP = rep(c("BSAI", "GOA"), times = 3))
t1 <- rates_smry_wide[Stratification == "Current", -"Stratification"]
t1[, GEAR := tstrsplit(Strata, "_")[[2]]]
t1 <- t1[gear_fmp_dt, on = .(GEAR)]
setnames(t1, c("N", "Equal Rates"), c("Current_N", "Current_Rate"))

t2 <- rates_smry_wide[Stratification == "FMP", -"Stratification"]

# N broken up by monitoring method, GEAR and FMP
strata_N_2022 <- pc_effort_dt[ADP == 2022 & STRATA != "ZERO", .(N = uniqueN(TRIP_ID)), keyby = .(STRATA, BS_AI_GOA)]
strata_N_2022[, GEAR := tstrsplit(STRATA, split="_")[[2]]]
strata_N_2022[, c("MON", "GEAR") := tstrsplit(STRATA, split = "_")]

# Current
rates.current.prox <- rates_smry[Stratification == "Current" & Allo == "Proximity"][, .(Stratification, Allo, STRATA = Strata, Rate)]
rates.current.prox <- rates.current.prox[strata_N_2022, on = .(STRATA)]
rates.current.prox[, MON := tstrsplit(STRATA, split = "_")[[1]]]
rates.current.prox <- rates.current.prox[, .(Stratification, Allo, MON, GEAR, BS_AI_GOA, Rate)]

# FMP
fmp_tbl <- data.table(BSAI_GOA = c("BSAI", "BSAI", "GOA"), BS_AI_GOA = c("BS", "AI", "GOA"))
rates.fmp.prox <- rates_smry[Stratification == "FMP" & Allo == "Proximity"]
rates.fmp.prox[, c("STRATA", "BSAI_GOA") := tstrsplit(Strata, split = "[*]")]
rates.fmp.prox[, c("MON", "GEAR") := tstrsplit(STRATA, split = "_")]
rates.fmp.prox <- rates.fmp.prox[, .(Stratification, Allo, MON, GEAR, BSAI_GOA,  Rate)][order(MON, GEAR, BSAI_GOA)]
rates.fmp_prox <- rates.fmp.prox[fmp_tbl, on = .(BSAI_GOA), allow.cartesian = T][, .(Stratification, Allo, MON, GEAR, BS_AI_GOA, Rate)]

# FIXED_FMP
gear_tbl <- data.table(STRATA = c("EM_FIXED", "EM_FIXED", "EM_TRW", "OB_FIXED", "OB_FIXED", "OB_TRW"), GEAR = c("HAL", "POT", "TRW", "HAL", "POT", "TRW"))
rates.fixed_fmp.prox <- rates_smry[Stratification == "FIXED_FMP" & Allo == "Proximity", .(Stratification, Allo, Strata, Rate)]
rates.fixed_fmp.prox[, c("STRATA", "BSAI_GOA") := tstrsplit(Strata, split = "[*]")]
rates.fixed_fmp.prox[, MON := tstrsplit(STRATA, split = "_")[[1]]]
rates.fixed_fmp.prox <- rates.fixed_fmp.prox[fmp_tbl, on = .(BSAI_GOA)][order(STRATA, BS_AI_GOA)]
rates.fixed_fmp.prox <- rates.fixed_fmp.prox[gear_tbl, on = .(STRATA)][, .(Stratification, Allo, MON, GEAR, BS_AI_GOA, Rate)]

rates_gear_fmp <- rbind(rates.current.prox, rates.fmp_prox, rates.fixed_fmp.prox)
rates_gear_fmp_wide <- dcast(rates_gear_fmp, MON + GEAR + BS_AI_GOA ~ Stratification, value.var = "Rate")
rates_gear_fmp_wide <- rates_gear_fmp_wide[strata_N_2022, on = .(MON, GEAR, BS_AI_GOA)]
rates_gear_fmp_wide[, Current_n := floor(Current * N)]
rates_gear_fmp_wide[, FMP_n := FMP * N]
rates_gear_fmp_wide[, FIXED_FMP_n := FIXED_FMP * N]
rates_gear_fmp_wide[, Current_0 := round((1 - Current)^N, 4)]
rates_gear_fmp_wide[, FMP_0 := round((1 - FMP)^N, 4)]
rates_gear_fmp_wide[, FIXED_FMP_0 := round((1 - FIXED_FMP)^N, 4)]
rates_gear_fmp_wide <- rates_gear_fmp_wide[, .(MON, GEAR, BS_AI_GOA, N, Current, Current_n, Current_0, FMP, FMP_n, FMP_0, FIXED_FMP, FIXED_FMP_n, FIXED_FMP_0)]

rates_gear_fmp_wide %>% flextable() %>%
  add_header_row(values = c("", rep(c("Rate", "n", "P(0)"), times = 3)), colwidths = c(4, rep(1, times = 9))) %>%
  add_header_row(values = c("", "Current", "FMP", "Fixed_FMP"), colwidths = c(4, 3, 3, 3))


# What if we cherry-picked which FMP we split by BS/AI/GOA versus BSAI/GOA?
test <- unique(pc_effort_dt[, .(ADP, STRATA, TRIP_ID, BS_AI_GOA)])
test[, STRATA := fcase(
  STRATA == "EM_TRW", "EM_TRW",
  STRATA %in% c("EM_HAL", "EM_POT"), "EM_FIXED",
  STRATA == "OB_TRW", "OB_TRW",
  STRATA %in% c("OB_HAL", "OB_POT"), "OB_FIXED",
  STRATA == "ZERO", "ZERO")]
test <- test[, .(N = uniqueN(TRIP_ID)), by = .(ADP, STRATA, BS_AI_GOA)]
test_wide <- dcast(test, STRATA + BS_AI_GOA ~ ADP, value.var = "N", fill = 0)
test_wide %>% flextable() %>% autofit() %>% padding(padding = 2) %>% merge_v(j = c(1:2)) %>% fix_border_issues() %>% hline()

test2 <- unique(pc_effort_dt[, .(ADP, STRATA, TRIP_ID, BS_AI_GOA)])
test2[, STRATA := fcase(
  STRATA == "EM_TRW", "EM_TRW",
  STRATA %in% c("EM_HAL", "EM_POT"), "EM_FIXED",
  STRATA == "OB_TRW", "OB_TRW",
  STRATA %in% c("OB_HAL", "OB_POT"), "OB_FIXED",
  STRATA == "ZERO", "ZERO")]
test2[, FMP := fcase(
  STRATA %like% "EM", ifelse(BS_AI_GOA == "GOA", "GOA", "BSAI"),
  STRATA %like% "OB", fcase(
    STRATA == "OB_TRW", ifelse(BS_AI_GOA == "GOA", "GOA", "BSAI"),
    STRATA == "OB_FIXED", BS_AI_GOA),
  STRATA == "ZERO", BS_AI_GOA)]
test2 <- test2[, .(N = uniqueN(TRIP_ID)), by = .(ADP, STRATA, FMP)]
test2_wide <- dcast(test2, STRATA + FMP ~ ADP, value.var = "N", fill = 0)
test2_wide %>% flextable() %>% autofit() %>% padding(padding = 2) %>% merge_v(j = c(1:2)) %>% fix_border_issues() %>% hline()

