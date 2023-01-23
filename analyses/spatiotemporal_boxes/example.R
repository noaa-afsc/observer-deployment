# example script for presentations

library(data.table)
library(sf)
library(dplyr)
library(ggplot2)
library(gridExtra)   # for quickly combining plots

source("analyses/spatiotemporal_boxes/functions.R")

load("analyses/allocation_evaluation/allocation_evaluation.Rdata")
# Load the ADFG statistical area shapefile. '../' is needed for Rmarkdown to climb into parent folders.
stat_area_sf <- st_read(
  dsn = "source_data/ADFG_Stat_Area_shapefile/PVG_Statewide_2001_Present_GCS_WGS1984.shp", quiet = T) %>%
  select(STAT_AREA) %>%
  st_transform(crs = 3467)
# Load the Alaska map sf objects
load("source_data/ak_shp.rdata")      # shp_land, shp_nmfs, and shp_centroids added to global
# TODO a lower-resolution version of our AK map would be adequate and considerably speed things up while plotting.

#=================================================================#
# Example figures for show and tell to stock assessors/ PCFMAC ####
#=================================================================#

# Create 200km cells
# FIXME in the dataset, should change ZE pool STRATA to ZE?
dat <- unique(val_2018_2021_dt[POOL != "ZE", .(ADP, POOL, STRATA, ADFG_STAT_AREA_CODE, WEEK, TRIP_ID)])
# FOR USE WITH OLD VERSION
# dat_ps <- define_poststrata(dat, space = c(2e5, 2e5), time = c("WEEK", 1), stratum_cols = c("ADP", "STRATA"), geom = T)
dat_ps <- define_poststrata_geom(dat, space = c(2e5, 2e5), time = c("WEEK", 1), year_col = "ADP", stratum_cols = c("STRATA"), geom = T)

plot_theme <- list(
  geom_sf(data = shp_land),   # Can comment this out since it's supoer slow to plot the hi-res map repeatedly
  facet_grid(. ~ WEEK, labeller = labeller(WEEK = function(x) paste("WEEK", x))), 
  theme(legend.position = "none", axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()), 
  scale_fill_viridis_c(trans = "log"))

pot_2021_sub <- dat_ps$geom %>% filter(ADP == 2021 & STRATA == "POT" & WEEK %in% 21:23) 
p1 <- ggplot(pot_2021_sub) + plot_theme + geom_sf(alpha=0.5, aes(fill = PS_n)) + geom_sf_text(aes(label = PS_n), size = 2) + labs(subtitle = "Count of trips per cell")
p2 <- ggplot(pot_2021_sub) + plot_theme + geom_sf(alpha=0.5, aes(fill = PS_nbr)) + geom_sf_text(aes(label = PS_nbr), size = 2) + labs(subtitle = "Count of neighboring trips per cell")
p3 <- gridExtra::grid.arrange(p1, p2, ncol = 1)
#   ggsave("C:/Users/geoff.mayhew/Work/GitHub/2024_ADP/analyses/spatiotemporal_boxes/pot_test.png", p3, height=6, width=8, units="in", dpi=600)

trw_2021_sub <- dat_ps$geom %>% filter(ADP == 2021 & STRATA == "TRW" & WEEK %in% 10:12) 
p4 <- ggplot(trw_2021_sub) + plot_theme + geom_sf(alpha=0.5, aes(fill = PS_n)) + geom_sf_text(aes(label = PS_n), size = 2) + labs(subtitle = "Count of trips per cell")
p5 <- ggplot(trw_2021_sub) + plot_theme + geom_sf(alpha=0.5, aes(fill = PS_nbr)) + geom_sf_text(aes(label = PS_nbr), size = 2) + labs(subtitle = "Count of neighboring trips per cell")
p6 <- gridExtra::grid.arrange(p4, p5, ncol = 1)
#   ggsave("C:/Users/geoff.mayhew/Work/GitHub/2024_ADP/analyses/spatiotemporal_boxes/trw_test.png", p6, height=6, width=8, units="in", dpi=600)

em_trw_2021_sub <- dat_ps$geom %>% filter(ADP == 2021 & STRATA == "EM_TRW" & WEEK %in% 10:12) 
p7 <- ggplot(em_trw_2021_sub) + plot_theme + geom_sf(alpha=0.5, aes(fill = PS_n)) + geom_sf_text(aes(label = PS_n), size = 2) + labs(subtitle = "Count of trips per cell")
p8 <- ggplot(em_trw_2021_sub) + plot_theme + geom_sf(alpha=0.5, aes(fill = PS_nbr)) + geom_sf_text(aes(label = PS_nbr), size = 2) + labs(subtitle = "Count of neighboring trips per cell")
p9 <- gridExtra::grid.arrange(p7, p8, ncol = 1)
#   ggsave("C:/Users/geoff.mayhew/Work/GitHub/2024_ADP/analyses/spatiotemporal_boxes/em_trw_test.png", p9, height=6, width=8, units="in", dpi=600)

#==================================================#
# What rates would we expect for a $4M program? ####
#==================================================#

ob_cpd <- 4118880 / 2844         # $1447.27/day
fgem_cpd <- 1e6 / (632 + 255)    # $1127.40/day
trwem_cpd <- 600                 # $600/day according to JF estimates used in set_budget.R in 2022 Final ADP repo
# Get average trip length for all strata, using TRIP_END - TRIP_START + 1
trip_cost_dt <- unique(val_2018_2021_dt[STRATA != "ZERO", .(ADP, STRATA, TRIP_ID, START, END)])[
][, .(MEAN_TRIP_DAYS = mean(as.numeric(END - START, units = "days") + 1)), by = .(ADP, STRATA)]
# Merge in day costs
trip_cost_dt[, CPD := fcase(
  STRATA == "EM_TRW", trwem_cpd,
  STRATA %in% c("EM_POT", "EM_HAL"), fgem_cpd,
  STRATA %in% c("HAL", "POT", "TRW"), ob_cpd)]
# Calculate cost per trip
trip_cost_dt[, CPT := MEAN_TRIP_DAYS * CPD]

sample_rate_vec <- seq(from = 0.005, to = 0.95, by = 0.0001) 
dat_ps2 <- define_poststrata(dat, space = c(2e5, 2e5), time = c("WEEK", 1), stratum_cols = c("ADP", "STRATA"), geom = T)
box_def_mean_prop_n <- calculate_mean_prop_n(dat_ps2, sample_rate_vec)

# Rates using proximity only
allocation_gaps <- calculate_gaps(box_def_mean_prop_n, trip_costs_dt)
allocation_gaps$rates[, .SD[findInterval(4e6, INDEX_COST)], by = c(attr(allocation_gaps, "stratum_cols"))][ADP==2021]

# Rate with proximity / CV scaling
allocation_gaps_CV <- calculate_index(box_def_mean_prop_n, trip_cost_dt)
allocation_gaps_CV$rates[, .SD[findInterval(4e6, INDEX_COST)], by = c(attr(allocation_gaps_CV, "stratum_cols"))][ADP==2021]

dcast(unique(allocation_gaps_CV$melt[, .(ADP, STRATA, N)]), ADP ~ STRATA, value.var = "N")

#=============================#
# Plotting for Jen Cahalan ####
#=============================#

# Fishing effort with ADFG areas, hex cells, NMFS areas, etc

# If you're only interested in plotting things spatially, then define a grid size with 'stat_area_to_hex' and join it
# to your fishing effort on ADFG_STAT_AREA_CODE. If you want to plot things with time as well, then use 
# define_postrata_geom() on your fishing effort.

# For now, just plot things spatially. Let's use 200km-wide hex cells.
hex_grid_200km <- stat_area_to_hex(2e5, stat_area_sf = stat_area_sf)
# Simplify the dataset before merging in geometry. If you want to include time, don't drop your time column!
data_200km <- unique(val_2018_2021_dt[ADP==2021, -c("WEEK", "MONTH", "FMP", "FMP_MT", "AREA", "GEAR", "AGENCY_GEAR_CODE") ] )
# Merge in HEX_ID according to ADFG_STAT_AREA_CODE
data_200km <- merge(data_200km, hex_grid_200km$STAT_AREA_HEX_DF, by = "ADFG_STAT_AREA_CODE")
# Remove ADFG_STAT_AREA_CODE
data_200km <- unique(data_200km[, -"ADFG_STAT_AREA_CODE"])
# Summarize count of trips by HEX_ID for each STRATUM
data_200km_smry <- data_200km[, .(TRIP_COUNT = uniqueN(TRIP_ID)), by = .(POOL, STRATA, HEX_ID)]
# Merge in the geometry on HEX_ID
data_200km_smry <- merge(hex_grid_200km$HEX_GEOMETRY, data_200km_smry, on = .(HEX_ID))
# Plot! It might take a while with all the high-resolution layers (especially shp_nmfs and stat_area_sf)
ggplot() + 
  facet_wrap(~ POOL + STRATA, labeller = labeller(POOL = function(x) paste("POOL", x))) + 
  geom_sf(data = shp_land, fill = "gray") + 
  #geom_sf(data = shp_nmfs, color = "red", alpha = 0) + 
  #geom_sf(data = stat_area_sf, color = "green", alpha = 0) + 
  geom_sf(data = hex_grid_200km$HEX_GEOMETRY, color = "blue", alpha = 0) + 
  geom_sf(data = data_200km_smry, aes(fill = TRIP_COUNT), alpha = 0.8) + 
  scale_fill_viridis_c(trans = "log", breaks = c(1, 5, 10, 25, 100, 250)) + # log-transform the fill scale
  theme(
    legend.position = "bottom", legend.key.width = unit(2, "cm"),
    axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
