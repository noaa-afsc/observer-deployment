### Statistical Area to Hex Cells ###

library(data.table)       # data wrangling
library(tidyverse)        # data wrangling and piping, especially useful for sf objects
library(sf)               # simple features
library(ggplot2)          # plotting
library(gridExtra)        # combine plots

#=========#
# DATA ####
#=========#

# 'VALHALLA used for 2021 AR source data from: https://drive.google.com/file/d/1bWVlhSlbWOWcmeqrQQxpt-QrbWRr8vHo/view?usp=sharing
load("source_data/2_AR_data.Rdata")  
rm(list = setdiff(ls(), c("partial", "work.data")))  # remove unneeded objects

# Alaska basemap, NMFS areas and their centroids
load("source_data/ak_shp.rdata")  # loads shp_land, shp_nmfs, and shp_centroids to global environment

# ADFG stat areas from https://soa-adfg.opendata.arcgis.com/datasets/groundfish-statistical-areas-2001
# ADFG stat areas generally have cell sizes of 1 degree longitude and 0.5 degree longitude
stat_area <- st_read("source_data/ADFG_Stat_Area_shapefile/PVG_Statewide_2001_Present_GCS_WGS1984.shp", quiet=T) %>% 
  select(OBJECTID, STAT_AREA, FMP_AREA_C, NMFS_AREA = FEDERAL__1) %>%
  st_transform(crs = 3467)                                                                 # NAD 83 Alaska Albers projection

#=======================#
# SUMMARIZE VALHALLA ####
#=======================#

# Simplify the dataset a bit. Some of these simplifications may not be required at this point (e.g., having a single
# trip start and trip end date per TRIP_ID, instead of simplifying the dates to each hex cell later on), but it should
# make the data easier play with.

effort_2021 <- work.data %>% 
  mutate(GEAR = ifelse(AGENCY_GEAR_CODE %in% c("NPT", "PTR"), "TRW", AGENCY_GEAR_CODE)) %>%    # Coerce NPT and PTR to TRW
  mutate(POOL = case_when(                                                                     # Define POOL
    STRATA %in% c("EM HAL", "EM POT", "EM TRW EFP") ~ "EM",
    STRATA %in% c("ZERO", "Zero EM Research") ~ "Zero", 
    STRATA %in% c("HAL", "POT", "TRW") ~ "Observer",
    STRATA == "FULL" ~ "FULL"
  )) %>%
  select(
    TRIP_ID, VESSEL_ID, CVG = COVERAGE_TYPE, POOL, STRATA, GEAR, STAT_AREA = ADFG_STAT_AREA_CODE, 
    WEIGHT_POSTED, TRIP_TARGET_DATE, LANDING_DATE, OBS = OBSERVED_FLAG) %>%
  group_by(TRIP_ID) %>%                                                               # By TRIP_ID, find trip start/end and unify monitored flag
  mutate(
    TRIP_START = min(TRIP_TARGET_DATE, LANDING_DATE, na.rm=T),
    TRIP_END = max(TRIP_TARGET_DATE, LANDING_DATE, na.rm=T),
    OBS = ifelse(any(OBS=="Y"), "Y", "N")) %>%
  select(-TRIP_TARGET_DATE, -LANDING_DATE) %>%
  group_by(TRIP_ID, TRIP_START, TRIP_END, VESSEL_ID, CVG, POOL, STRATA, GEAR, OBS, STAT_AREA) %>%          # TODO can use across(-c(v1, v2, v3...))
  summarise(AREA_GEAR_WEIGHT = sum(WEIGHT_POSTED, na.rm=T)) %>%                       # Total catch weight by each trip's STAT_AREA and GEAR
  ungroup() %>%
  mutate(ROW_ID = row_number())

#==============================================#
# Map Fishing Effort to Specified Cell Size ####
#==============================================#

effort_map <- function(effort, stat_area, cell_size, time_fun = "quarter", land = shp_land) {
  
  if( st_crs(stat_area)$input == "WGS 84" ) stop("stat_area object is in WGS 84. Must be in Alaska Albers, crs = 3467")
  
  # Generate grid
  stat_area_centroid <- suppressWarnings(st_centroid(stat_area))           # Get centroids of statistical areas
  hex_grid <- st_make_grid(stat_area_centroid, cellsize = cell_size, square=F)[stat_area_centroid]
  hex_grid <- st_as_sf(data.frame(CELL_ID = seq_len(length(hex_grid))), geometry = hex_grid)           # Define CELL_ID
  
  # Identify intersections of hex_grid polygons with statistical area centroids
  hex_grid_intersects <- st_intersects(hex_grid, stat_area_centroid)
  
  # Frequency of statistical areas per cell
  hex_stat_freq <- table(lengths(hex_grid_intersects))  # Most cells have 3-4 stat areas (%51.52), many have 3 (15%)
  hex_stat_plot <- ggplot(data.frame(STAT_AREA_N = as.numeric(names(hex_stat_freq)), FREQ = as.numeric(unname(hex_stat_freq)))) + 
    geom_col(aes(x = STAT_AREA_N, y = FREQ)) + 
    labs(
      subtitle = paste0("Cell size of ", cell_size/1e3, "km, mode of ", unname(which(hex_stat_freq == max(hex_stat_freq)))),
      x = "Statistical Areas per cell", y = "Frequency")
  
  # Create data.frame of hex grid cell and statistical area
  hex_grid_intersects_list <- lapply(hex_grid_intersects, function(x) (stat_area_centroid %>% st_drop_geometry())[x, "STAT_AREA"])
  hex_grid_intersects_df <- do.call(what = rbind, Map(
    function(x,y) data.frame(CELL_ID = x, STAT_AREA = y),
    x = seq_len(length(hex_grid_intersects)),
    y = hex_grid_intersects_list))
 
  # Divide fishing effort by time, using midpoint of trip start and trip end
  effort <- effort %>% mutate(TIME = do.call(what = match.fun(time_fun), list(TRIP_START + as.numeric(TRIP_END - TRIP_START)))) 
  effort_gear_time <- effort %>% 
    filter(CVG == "PARTIAL" & GEAR != "JIG") %>%
    split(~GEAR) %>%
    lapply(FUN = merge, hex_grid_intersects_df, by = "STAT_AREA")  # Merge in statistical area
  # For each GEAR, count number of unique TRIP_IDs within each CELL_ID and TIME
  effort_gear_time_cell_N <- suppressMessages(effort_gear_time %>%
    lapply(FUN = function(x) x %>% group_by(CELL_ID, GEAR, POOL, TIME) %>% summarise(N = uniqueN(TRIP_ID)) %>% ungroup()))
  # Create separate hex grids for each gear type. The base grid is used to draw the same gear-specific cells across all pools
  hex_grid_gear_time_N <- effort_gear_time_cell_N %>% lapply(FUN = merge, hex_grid, by = "CELL_ID") %>% lapply(st_as_sf)
  hex_grid_gear_time_base <- lapply(
    hex_grid_gear_time_N, 
    function(x) x %>% select(CELL_ID, TIME) %>% distinct())
  
  maps_gear_st <- Map(
    f = function(x, y) {
      ggplot() + 
        facet_grid(POOL ~ GEAR + TIME) +     # TODO Add gear type to data.frames so that we can add gear type to x-strip of facets.
        geom_sf(data = land) + 
        geom_sf(data = x, fill = "white", alpha=0.5) + 
        geom_sf(data = y, aes(fill = N), alpha=0.8) + geom_sf_text(data = y, aes(label = N), size = 1.5) + 
        scale_fill_viridis_c(trans = "log", label = function(x) sprintf("%.2f", x)) +   # TODO Currently the color scales vary for the gear types
        scale_x_continuous(breaks = seq(160, 240, 10)) + 
        theme(axis.text.x = element_text(angle=90, hjust=1), legend.position="bottom", strip.text.x = element_text(margin = margin(b=0.2, t=0.2)) ) + 
        labs(subtitle = paste0("Cell size of ", cell_size/1e3, "km"), x = NULL, y = NULL)
    },
    x = hex_grid_gear_time_base,
    y = hex_grid_gear_time_N
  )
  
  # Return output as a list
  return(
    list(
      CELL = cell_size,
      FREQ = hex_stat_plot,
      MAPS = maps_gear_st,
      DATA = effort_gear_time,
      GRID = hex_grid_gear_time_N
    )
  )
}

# The the maps for each hex cell size
hex_125 <- effort_map(effort_2021, stat_area, cell_size = 1.25e5)
hex_200 <- effort_map(effort_2021, stat_area, cell_size = 2.00e5)
hex_250 <- effort_map(effort_2021, stat_area, cell_size = 2.50e5)

# to view the merged effort and grid cells view the DATA and GRID objects, split by gear type:
hex_125$DATA$HAL
hex_125$GRID$HAL

# Save each gear type as a png with the same extent. Run this chunk of code manually!
if(F) {
  
  # Find maximum range of extent of bounding boxes across all grid sizes for each gear type
  bbox_gear <- sapply(
    names(hex_125$DATA),
    FUN = function(x) {
      setNames(unlist(Map(
        f = function(y1, y2, y3) {
          bboxs <- rbind(st_bbox(y1), st_bbox(y2), st_bbox(y3))
          xlim = c(min(bboxs[,1]), max(bboxs[,3]))
          ylim = c(min(bboxs[,2]), max(bboxs[,4]))
          list(xlim, ylim)
        },
        y1 = hex_125$GRID[x],
        y2 = hex_200$GRID[x],
        y3 = hex_250$GRID[x]
      ), recursive=F), c("xlim", "ylim"))
    }, simplify=F
  )
  
  # Generate plots with identical bounding boxes
  lapply(
    names(hex_125$DATA),
    function(z) {
      ggsave(
        filename = paste0("output_figures/ts_", z, "_125.png"), width=10, height=7.5, units="in",
        plot = hex_125$MAPS[[z]] + coord_sf(xlim = bbox_gear[[z]]$xlim, ylim = bbox_gear[[z]]$ylim))
      ggsave(
        filename = paste0("output_figures/ts_", z, "_200.png"), width=10, height=7.5, units="in",
        plot = hex_200$MAPS[[z]] + coord_sf(xlim = bbox_gear[[z]]$xlim, ylim = bbox_gear[[z]]$ylim))
      ggsave(
        filename = paste0("output_figures/ts_", z, "_250.png"), width=10, height=7.5, units="in",
        plot = hex_250$MAPS[[z]] + coord_sf(xlim = bbox_gear[[z]]$xlim, ylim = bbox_gear[[z]]$ylim))
    }
  )
}