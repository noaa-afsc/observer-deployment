# Selection rates for the 2025 Draft ADP

# Author: Geoff Mayhew
# Start Date: 2024-Aug-07

#======================================================================================================================#
# Preparation ----
#======================================================================================================================#

#===================#
## Load Packages ----
#===================#

library(data.table)         # Data wrangling
library(ggplot2)            # Plotting
library(sf)                 # Spatial analyses
library(dplyr)              # For piping and handling sf objects
library(FMAtools)           # For connectivity to Analytical Services Program's Shared Google Drive
library(readxl)             # For read_xlsx
# library(grid)               # For unit.pmax  to get widths of grobs so that plots have matching dimensions
# library(gridExtra)          # For arrangeGrob to combine plots
# library(flextable)          # For print-ready tables
# library(officer)            # For additional flextable formatting options such as fp_border

#====================#
## Load Functions ----
#====================#

#' Load spatiotemporal functions 
#' TODO grab the functions from the 2023 AR and move them to the common_functions folder! 
# source("functions/spatiotemp_functions.R")

#=====================================#
## Connect to Shared Google Drive ----
#=====================================#



#=============================#
## Load data and data prep ----
#=============================#

#' TODO *loading the most recent version of Valhalla used for the 2023 Annual Report.* Use the outputs of get_data.R
#' when it is prepared. Download from Projects/ADP/ folder
gdrive_download("source_data/2_AR_data.Rdata", gdrive_set_dribble("Projects/AnnRpt-Deployment-Chapter/"))
AR_data_objects <- load("source_data/2_AR_data.Rdata")
# Remove everything except for work.data
rm(list = setdiff(ls(), c("work.data")))

#' TODO *Do this in get_data*
# Get count of GOA-only EM EFP Vessels for the cost_params. 
trawl_em_goa_v_count <- length(na.omit(unlist(unname(
  setDT(readxl::read_xlsx("source_data/2024 EM EFP Vessel List_NMFS.xlsx", col_names = F))[-c(1:3), 7]))))

# Load the ADFG statistical area shapefile.
stat_area_sf <- st_read(
  dsn = "source_data/ADFG_Stat_Area_shapefile/PVG_Statewide_2001_Present_GCS_WGS1984.shp", quiet = T) %>%
  select(STAT_AREA) %>%
  st_transform(crs = 3467)

# Load the Alaska map sf objects
load("source_data/ak_shp.rdata")      # shp_land, shp_nmfs, and shp_centroids added to global

# Wrangle the Valhalla data set for spatiotemporal analyses
pc_effort_st <- spatiotemp_data_prep(work.data)