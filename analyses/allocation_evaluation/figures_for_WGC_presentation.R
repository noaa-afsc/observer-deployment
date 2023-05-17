# Figures for Western Groundfish Conference (WGC) Presentation

library(data.table)
library(dplyr)
library(scales)
library(ggplot2)
library(ggpubr)      # For combining plots  devtools::install_github("kassambara/ggpubr")
library(pals)        # Color palettes 
library(sf)

# Load 2024 Draft ADP Dataset:
# https://drive.google.com/file/d/1Nq202X4JyuOOdnP_ENHRvsI7fWDE2qPo/view?usp=share_link
load("source_data/2024_Draft_ADP_data.rdata") # chiefly work.data, effort from 2013-2022

#======================================================================================================================#

# Slide 4 Figure ####

# Full and Partial coverage vessels and tonnage in 2022

s4_dat <- work.data[ADP == 2022
][, CVG_NEW := fcase(CVG_NEW == "FULL", "Full", CVG_NEW == "PARTIAL", "Partial")
][, .(VESSEL_N = uniqueN(VESSEL_ID), MT = sum(WEIGHT_POSTED[SOURCE_TABLE == "Y"], na.rm = T)), by = .(FMP, CVG_NEW)]

s4_fig_a <- ggplot(s4_dat, aes(x = FMP, y = VESSEL_N, fill = CVG_NEW)) + geom_col(color = "black") + labs(y = "Number of Vessels", fill = "Coverage Type") + 
  theme_classic() + 
  theme(legend.position = "top", axis.title.x = element_blank()) + 
  scale_y_continuous(labels = comma, expand = c(0,0)) +
  scale_fill_manual(values = kelly()[2:22])

s4_fig_b <- ggplot(s4_dat, aes(x = FMP, y = MT, fill = CVG_NEW)) + geom_col(color = "black") + labs(y = "Catch Weight (mt)", fill = "Coverage Type") + 
  theme_classic() + 
  theme(legend.position = "top", axis.title.x = element_blank()) + 
  scale_y_continuous(labels = comma, expand = c(0,0)) + 
  scale_fill_manual(values = kelly()[2:22])

s4_fig <- ggarrange(s4_fig_a, s4_fig_b, ncol = 1, common.legend = T, align = "v")

ggsave("analyses/allocation_evaluation/figures/WGC_s4_fig.png", s4_fig, width = 5, height = 6, units = "in", dpi = 300)

#======================================================================================================================#

# Slide 5 Figure ####

# Partial coverage vessels and tonnage in 2022 by monitored/unmonitored, with zero coverage separate

# Count number of vessels - A vessel with ANY trip monitored is counted as 'monitored', whereas vessels that fished 
# without a single monitored trip are counted as 'unmonitored' (vessel-level definition).
s5_dat_ves <- copy(work.data[ADP == 2022 & CVG_NEW == "PARTIAL"])[
][STRATA_NEW == "ZERO", GROUP := "Zero"
][is.na(GROUP), GROUP := ifelse(any(OBSERVED_FLAG == "Y"), "Monitored", "Unmonitored"), by = .(VESSEL_ID)
][, .(VES = uniqueN(VESSEL_ID)), keyby = .(GROUP, FMP)]

# Sum of tonnage. Here, the monitored and unmonitored groups are defined by the OBSERVED_FLAG (so the grouping is
# defined at the trip-level)
s5_dat_mt <- copy(work.data[ADP == 2022 & CVG_NEW == "PARTIAL"])[
][, GROUP := fcase(STRATA_NEW == "ZERO", "Zero", OBSERVED_FLAG == "Y", "Monitored", OBSERVED_FLAG == "N", "Unmonitored")
][, .(MT = sum(WEIGHT_POSTED[SOURCE_TABLE == "Y"], na.rm = T)), keyby = .(GROUP, FMP)]

s5_dat <- s5_dat_ves[s5_dat_mt]
s5_dat[, GROUP := factor(GROUP, levels = c("Zero", "Unmonitored", "Monitored"))]

s5_fig_a <- ggplot(s5_dat, aes(x = FMP, y = VES, fill = GROUP)) + geom_col(color = "black") + labs(y = "Number of Vessels", fill = "Partial Coverage") + 
  theme_classic() + 
  theme(legend.position = "top", axis.title.x = element_blank()) + 
  scale_y_continuous(labels = comma, expand = c(0,0)) + 
  scale_fill_manual(values = c("gray", "dodgerblue4", "dodgerblue"))

s5_fig_b <- ggplot(s5_dat, aes(x = FMP, y = MT, fill = GROUP)) + geom_col(color = "black") + labs(y = "Catch Weight (mt)", fill = "Partial Coverage") + 
  theme_classic() + 
  theme(legend.position = "top", axis.title.x = element_blank()) + 
  scale_y_continuous(labels = comma, expand = c(0,0)) + 
  scale_fill_manual(values = c("gray", "dodgerblue4", "dodgerblue"))

s5_fig <- ggarrange(s5_fig_a, s5_fig_b, ncol = 1, common.legend = T, align = "v")

ggsave("analyses/allocation_evaluation/figures/WGC_s5_fig.png", s5_fig, width = 5, height = 6, units = "in", dpi = 300)

#======================================================================================================================#

# Slide 13 Figure ####

# Maps featuring interspersion of Observer data to the fixed-gear EM and Zero-selection pools

# Fishing effort data from 2018 to 2021
load("analyses/allocation_evaluation/allocation_evaluation.RData")
val_2018_2022_dt <- pc_effort_dt[ADP %in% 2018:2022]

# Load the ADFG statistical area shapefile. '../' is needed for Rmarkdown to climb into parent folders.
stat_area_sf <- st_read(
  dsn = "source_data/ADFG_Stat_Area_shapefile/PVG_Statewide_2001_Present_GCS_WGS1984.shp", quiet = T) %>%
  select(STAT_AREA) %>%
  st_transform(crs = 3467)

# Load the Alaska map sf objects
load("source_data/ak_shp.rdata")      # shp_land, shp_nmfs, and shp_centroids added to global

# Trip Costs #

# Use average values from the 2023 Final ADP for now. Also, roughly using trip_end - trip_start + 1 for now until we get
# a better handle on future monitoring costs.
ob_cpd <- 4896623 / 3094         # $1582.619/day
fgem_cpd <- 1e6 / (1040+735)     # $ 563.380/day
trwem_cpd <- 600                 # $600/day according to JF estimates used in set_budget.R in 2022 Final ADP repo
# Get average trip length for all strata, using TRIP_END - TRIP_START + 1
# This function counts number of unique days between each trip target date and landing date
# use this to count days for non-observed trips, which already have a modeled 'DAYS' estimate.
day_count <- function(x) {
  length(unique(unlist(apply(x, 1, function(y) as.numeric(as.Date(y[1])) : as.numeric(as.Date(y[2])), simplify = F))))
}
val_2018_2022_dt[POOL != "OB", DAYS := day_count(.SD), by = .(TRIP_ID), .SDcols = c("TRIP_TARGET_DATE", "LANDING_DATE")]
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

# Load Functions #

source("analyses/allocation_evaluation/functions.R")    

# Apply box definition
system.time(box_res <- define_boxes(
  val_2018_2022_dt, c(2e5, 2.5e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = "STRATA", dmn_cols = c("BSAI_GOA", "GEAR"), geom = T))

# Calculate index
sample_rate_vec <- seq(from = 0.005, to = 0.95, by = 0.0001)
system.time(ispn_res <- calculate_interspersion(box_res, sample_rate_vec, omit_strata = "ZERO"))
system.time(index_res <- calculate_index(ispn_res, trip_cost_dt))  # This will take a few minutes!

# Allocate according to a $4.5M budget
budget_tbl <- data.table(INDEX_COST = 4.5e6)
group_cols <- unlist(index_res$params[c("year_col", "stratum_cols")], use.names = F)
rates_4.5M <- index_res$rates[
][, .SD[budget_tbl, on = .(INDEX_COST), roll = "nearest"], by = group_cols]

# Calculate interspersion of domains using afforded rates
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
dmn_insp_og <- calculate_dmn_interspersion3(box_res, rates_4.5M, stratum_dt_og, acceptor_donor_lst_og)

# Modify dmn_interspersion_plot to show only OB interspersion to EM and ZERO for two 4-week chunks
dmn_res_pool_dt <- copy(dmn_insp_og)
design_desc <- NULL

# TEST : dmn_res_pool_dt <- copy(dmn_insp_fmp); design_desc <- NULL

year_col <- dmn_res_pool_dt$params$year_col
if(!is.null(design_desc)) design_desc <- paste0("Design: ", design_desc, "; ")

# Plot-ready data set, which groups by 4-week blocks?
pool_4wk <- copy(dmn_res_pool_dt$POOLED)
pool_4wk[, TIME_4 := cut(TIME, seq(min(TIME), max(TIME), by = 4), include.lowest = T, labels = F)]
# Now average over the 4 weeks in each group using a weighted average based on the number of trip components of the receptor pool.
pool_4k <- pool_4wk[, .(
  BOX_DMN_w = sum(BOX_DMN_w), 
  BOX_DONOR_SAMPLE_PROB = weighted.mean(BOX_DONOR_SAMPLE_PROB, w = BOX_DMN_w)), 
  by = c(year_col, "POOL", "GEAR", "HEX_ID", "TIME_4")]
# Merge geometry back in 
pool_4k <- merge(dmn_res_pool_dt$geom, pool_4k, by = "HEX_ID")

# Subset the data to focus on 2022 HAL effort, and only on OB overlapping EM and ZE pools
hal_2022_sub <- pool_4k  %>% filter(POOL != "OB" & GEAR == "HAL" & ADP == 2022 & TIME_4 %in% c(9,10))
ak_map_cropped <- ak_low_res %>% st_set_crs(st_crs(hal_2022_sub))

s13_fig <- ggplot() + 
  facet_grid(TIME_4 ~ POOL) + 
  geom_sf(data = ak_map_cropped %>% st_crop(st_bbox(hal_2022_sub))) + 
  geom_sf(data = hal_2022_sub, aes(fill = BOX_DONOR_SAMPLE_PROB)) + 
  scale_fill_viridis_c(limits = c(0, 1), breaks = seq(0, 1, 0.2)) + 
  theme(
    legend.position = "bottom", axis.text = element_blank(), axis.ticks = element_blank(),
    panel.spacing = unit(0.1, "lines")) + 
  labs(y = "Month", fill = "Observer sample probability") 

ggsave("analyses/allocation_evaluation/figures/WGC_s13_fig.png", s13_fig, width = 6, height = 4, units = "in", dpi = 300)