# 2024 Draft ADP : Appendix A

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

# Load the ADFG statistical area shapefile. '../' is needed for Rmarkdown to climb into parent folders.
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

#=========================#
# Map of Spatial cells ####
#=========================#

box_sf <- current.box$geom_sf %>% select(HEX_ID, geometry) %>% unique() %>% st_set_crs(value = st_crs(shp_nmfs))
# ggplot() + geom_sf(data = box_sf, color = "blue", fill = NA) + geom_sf_text(data = box_sf, aes(label = HEX_ID))
box_sf_subset <- box_sf %>% 
  filter(HEX_ID %in% c(62, 52, 57, 68, 73, 67, 56)) %>% 
  mutate(FILL = ifelse(HEX_ID == 62, "green", "purple"))
figure_hex_example <- ggplot() + 
  geom_sf(data = shp_nmfs, color = "red", fill = NA) +
  geom_sf(data = shp_land, fill = "gray70") + 
  geom_sf(data = box_sf, color = "blue", fill = NA) + 
  geom_sf(data = box_sf_subset, aes(fill = I(FILL)), alpha =0.5) + 
  theme(
    axis.text = element_blank(), axis.ticks = element_blank(), 
    panel.grid= element_blank(), panel.background = element_blank())

# Became figures 3-3 in the 2024 Draft ADP
ggsave(figure_hex_example, file = "analyses/appendix_A_box_size/figure_hex_example.png", width = 6, height = 4, units = "in", dpi = 300)

#===================================================#
# Appendix A: Box Size Definition Box definition ####
#===================================================#

system.time(box_no_neighbors <- define_boxes_3(
  val_2018_2022_dt, c(2e5, 0), time = c("week", 0, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = "STRATA", dmn_cols = c("BSAI_GOA", "GEAR"), geom = T))

ggplot(box_no_neighbors$dt_out[STRATA != "ZERO" & ADP == 2022], aes(x = STRATA, y= BOX_nbr)) + geom_violin()

# [Table A-1]
# without neighbors, most strata have 75% of boxes with 4 or fewer trips, which with a 15% sample rate, meaning boxes
# are more likely than not to not contain any sampled trips.
box_no_neighbors$dt_out[STRATA != "ZERO" & ADP == 2022, as.list(quantile(BOX_nbr, c(0.1, 0.25, 0.5, 0.75))), keyby = .(STRATA) ]
# By allowing boxes to neighbor, most strata have half of boxes with at least 11 trips in a boxes' neighborhood (16.7% probability of being unsampled)
current.box$dt_out[STRATA != "ZERO" & ADP == 2022, as.list(quantile(BOX_nbr, c(0.1, 0.25, 0.5, 0.75))), keyby = .(STRATA) ]


ggplot(current.box$dt_out[STRATA != "ZERO" & ADP == 2022], aes(x = STRATA, y= BOX_nbr)) + geom_violin()

box_compare <- rbind(
  cbind(BOX = "no neighbors", box_no_neighbors$dt_out[STRATA != "ZERO" & ADP == 2022]),
  cbind(BOX = "with neighbors", current.box$dt_out[STRATA != "ZERO" & ADP == 2022])
)

strata_levels <- unique(box_compare[order(STRATA)]$STRATA)
strata_levels <- c(strata_levels[strata_levels %like% "OB"], strata_levels[strata_levels %like% "EM"])
box_compare[, STRATA := factor(STRATA, levels = strata_levels)]

ggplot(box_compare, aes(x = STRATA, y= BOX_nbr, color = BOX)) + geom_boxplot() + theme(legend.position = "bottom") + 
  labs(x = "Strata", y = "Trips per box", color = "Box definition")

system.time(huge_boxes <- define_boxes_2(
  val_2018_2022_dt, c(2e6, 0), time = c(90, 0),
  time_cols <- c("TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = "STRATA", dmn_cols = c("BSAI_GOA", "GEAR"), geom = T))

huge_boxes$dt_out[STRATA != "ZERO" & ADP == 2022, as.list(quantile(BOX_nbr, c(0.1, 0.25, 0.5, 0.75))), keyby = .(STRATA) ]
ggplot(huge_boxes$dt_out[STRATA != "ZERO" & ADP == 2022], aes(x = STRATA, y= BOX_nbr)) + geom_boxplot()
ggplot(huge_boxes$dt_out[STRATA != "ZERO" & ADP == 2022], aes(x = BOX_nbr)) + facet_grid(STRATA ~ .) + geom_histogram()

system.time(unbalanced_boxes <- define_boxes_2(
  val_2018_2022_dt, c(1e5, 0), time = c(90, 0),
  time_cols <- c("TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = "STRATA", dmn_cols = c("BSAI_GOA", "GEAR"), geom = T))

# [Table A-2]
# numbers of spatial and temporal cells in each stratum 
current.box$dt_out[ADP == 2022, .(HEX_COUNT = uniqueN(HEX_ID), TIME_COUNT = uniqueN(TIME)), keyby = .(STRATA)]
unbalanced_boxes$dt_out[ADP == 2022, .(HEX_COUNT = uniqueN(HEX_ID), TIME_COUNT = uniqueN(TIME)), keyby = .(STRATA)]
