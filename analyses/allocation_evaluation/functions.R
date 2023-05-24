# These are the functions from analyses/allocation_evaluation
# Some are further developed version of those in analyses/spatiotemporal_boxes

library(data.table)   # For data wrangling, very efficient with joins, especially rolling joins
library(dplyr)        # For data wrangling/piping with sf package
library(sf)           # For spatial statistics

#======================================================================================================================#
# Space/Time Box Definition  -------------------------------------------------------------------------------------------
#======================================================================================================================#

# Converts ADFG Stat Area to an iso-area hexagon grid. Specify the cell size in meters as the width of each hex cell.
# cell size. The output is a dataframe with corresponding ADFG_STAT_AREA_CODEs and HEX_IDs, as well as the
# simple feature object (class sf) of the hex cell polygons.
stat_area_to_hex <- function(cell_size, stat_area_sf){
  
  # 'cell_size' should be specified as a distance in meters. For hex cells, this is the distance between opposite sides.
  # 'stat_area_sf' is the shapefile of the ADFG stat areas in Alaska Albers projection (3467), with each statistical 
  # area and its polygon geometry. For example:
  # stat_area_sf <- st_read("source_data/ADFG_Stat_Area_shapefile/PVG_Statewide_2001_Present_GCS_WGS1984.shp", quiet = T) %>%
  #   select(STAT_AREA) %>%
  #   st_transform(crs = 3467)
  
  # Get centroids of statistical areas
  stat_area_centroid_sf <- suppressWarnings(st_centroid(stat_area_sf))  
  
  # Generate a hex cell grid using the projection and boundaries from stat_area_centroid_sf, will cell size specified by
  # 'cell_size'. Subset this using stat_area_centroid_sf so only cells overlapping with it are retained.
  hex_grid_sf <- st_make_grid(
    x = stat_area_centroid_sf, 
    cellsize = cell_size, 
    square = F
  )[stat_area_centroid_sf] 
  # Add data frame to simple feature collection to define hex cell identifier
  hex_grid_sf <- st_as_sf(
    x = data.frame(HEX_ID = seq_len(length(hex_grid_sf))), 
    geometry = hex_grid_sf
  )    
  
  # Identify intersections of hex_grid polygons with statistical area centroids
  stat_area_hex_df <- do.call(
    what = rbind,
    args = Map(
      f = function(x,y) data.frame(HEX_ID = x, ADFG_STAT_AREA_CODE = stat_area_centroid_sf$STAT_AREA[y]),
      x = hex_grid_sf$HEX_ID,
      y = st_intersects(hex_grid_sf, stat_area_centroid_sf)
    )
  )
  
  # TODO assign attribute of cell_size to output object?
  return(
    list(
      CELL_SIZE = cell_size,
      STAT_AREA_HEX_DF = stat_area_hex_df,
      HEX_GEOMETRY = hex_grid_sf
    )
  )
}


# This function applies a box definition to fishing effort data. 
define_boxes <- function(data, space, time, year_col, stratum_cols, dmn_cols = NULL, stata_area_sf = stat_area_sf, geom = F) {
  # data <- copy(test); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; dmn_cols <- NULL; stratum_cols <- c("STRATA"); geom = T;
  # Now with gear as a post-stratum column...
  # data <- copy(test); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; dmn_cols <- "GEAR"; stratum_cols <- c("STRATA"); geom = T;

  #==================================#
  # NOTE! Remove any jig-gear trips! #
  #==================================# 
  
  # Jig-gear trips are generally in zero coverage (though sometimes HAL or EM_HAL trips might also fish
  # with jig gear in a trip?). Therefore, from an allocation perspective, it's not important if the rate will be 0.
  # Additionally, from an evaluation perspective, we don't use observed trips to make estimates for jig trips, so
  # we can remove them without issue. MAKE SURE 'data' HAS NO JIG GEAR COMPONENTS!
  
  win <- as.integer(time[2])
  
  # Make sure integer TRIP_ID, integer ADFG_STAT_AREA_CODE, and 'dmn_cols' are specified in the data
  if( length( intersect(colnames(data), c("TRIP_ID", "ADFG_STAT_AREA_CODE", dmn_cols))) !=  length(c("TRIP_ID", "ADFG_STAT_AREA_CODE", dmn_cols)) ) {
    stop(paste0("'data' must have columns 'TRIP_ID' and 'ADFG_STAT_AREA_CODE and dmn_cols: ", paste(dmn_cols, collapse = ", ")))
  } else {
    if( all(lapply(data[, c("TRIP_ID", "ADFG_STAT_AREA_CODE")], class) != "integer")) {
      stop ("'TRIP_ID' and 'ADFG_STAT_AREA_CODE' must be of class integer.")
    }
  }
  if(nrow(unique(data[, .(TRIP_ID, STRATA)])) != uniqueN(data$TRIP_ID)) stop("A trip is not in only one stratum!")
  
  # Remove any unused columns
  keep_cols <- c(year_col, stratum_cols, dmn_cols, "ADFG_STAT_AREA_CODE", time[3], time[4], "TRIP_ID")
  data <- unique(subset(data, select = keep_cols))      
  
  #========================#
  # Convert ADFG to HEX_ID #
  #========================#
  
  stat_area_lst <- stat_area_to_hex(space[1], stat_area_sf)
  stat_area_dist_lst <- suppressWarnings(apply(
    X = round(st_distance(st_centroid(stat_area_lst$HEX_GEOMETRY))), 
    MARGIN = 1, 
    FUN = function(x) which(x <= space[2])))
  data <- unique(data.table(stat_area_lst$STAT_AREA_HEX_DF)[
  ][data, on = .(ADFG_STAT_AREA_CODE)
  ][, -"ADFG_STAT_AREA_CODE"])
  setcolorder(data, neworder = c(year_col, stratum_cols, dmn_cols, "HEX_ID", time[3], time[4]))
  setkeyv(data, cols = c(year_col, stratum_cols, dmn_cols, "HEX_ID", time[3], time[4]))
  
  if(nrow(data[is.na(HEX_ID)])) {
    print(data[is.na(HEX_ID)])
    stop("Could not assign a HEX_ID!")
  }
  
  #======================#
  # Define temporal unit #
  #======================#
  
  # First, get all years and a table converting date to week
  if(time[1] != "week") stop("So far this function only works with 'week()' function!")
  dates_lst <- lapply(
    lapply(
      unique(unlist(data[, ..year_col])),
      function(x) as.Date(paste0(x, c("-01-01", "-12-31")))
    ),
    function(x) as.Date(x[1] : x[2], origin = as.POSIXct("1970-01-01", tz = "UTC"))
  )
  dates_mtx <- cbind(unlist(dates_lst), unlist(lapply(dates_lst, get(time[1]))))
  dates_start <- min(dates_mtx[, 1]) - 1  # Get first date and subtract 1. T
  dates_mtx[, 1] <- dates_mtx[, 1] - dates_start  # This makes it so matrix can be reference by row index, much faster
  
  # Grab time columns and define groups based on TRIP_ID and HEX_ID
  time_cols <- time[3:4]
  time_int <- data[, ..time_cols]
  time_int[, (time_cols) := lapply(.SD, as.integer), .SDcols = time_cols]
  setnames(time_int, new = c("S", "E"))
  data_int <- data[, .(TRIP_ID, HEX_ID)][, GRP := .GRP, by = .(TRIP_ID, HEX_ID)]
  # Convert to matrix and split by TRIP_ID and HEX_ID
  time_lst <- as.matrix(cbind(time_int - dates_start, data_int[, .(GRP)]))
  time_lst <- lapply(split(time_lst, time_lst[, "GRP"], drop = F), matrix, ncol = 3)
  # For each TRIP_ID x HEX_ID, identify unique weeks
  time_lst <- lapply(time_lst, function(x) {
    dates_int <- unique(unlist(apply(x, 1, function(y) y[1] : y[2], simplify = F)))  # get unique days
    unique(dates_mtx[dates_int, 2, drop = F])                     # Identify week using dates_mtx
  }) 
  
  # Collapse into a data.table (this is much faster than rbindlist(lapply(data_lst, as.data.table)))
  data_dt <- as.data.table(
    do.call(rbind, Map(function(x1, x2) cbind(x1, x2), x1 = as.list(seq_along(time_lst)), x2 = time_lst)))
  setnames(data_dt, new = c("GRP", "TIME"))
  # Merge
  data_int <- unique(data_int)[data_dt, on = .(GRP)][, - "GRP"]
  keep_cols2 <- c(setdiff(keep_cols, c(time_cols, "ADFG_STAT_AREA_CODE")), "HEX_ID")
  data <- unique(data[, ..keep_cols2])[data_int, on = .(TRIP_ID, HEX_ID)]
  
  #============#
  # Define Box #
  #============#
  
  setkeyv(data, c(year_col, stratum_cols, "TIME", "HEX_ID"))
  setcolorder(data, c(year_col, stratum_cols, "TIME", "HEX_ID"))
  # 'BOX_ID' is defined by HEX_ID and TIME (including year_col), and is common to all data. This will be used to determine neighbors
  data[, BOX_ID := .GRP, by = c(year_col, "TIME", "HEX_ID")]
  setkey(data, BOX_ID)
  
  #==================#
  # Define Neighbors #
  #==================#

  # For each BOX_ID, find which BOX_IDs are neighboring based on week and spatial cells.
  sub_cols <- c(year_col, "TIME", "HEX_ID", "BOX_ID")
  st_mtx <- as.matrix(unique(data[, ..sub_cols]))
  nbr_lst <- apply(st_mtx, MARGIN = 1, function(x) {
    # x <- c(2022, 27, 106, 6047)
    box_mtx <- st_mtx[st_mtx[,1] == x[1], , drop = F]
    nbr_time <- (x[2] + (-win:win)) 
    box_mtx <- box_mtx[box_mtx[,2] %in% nbr_time, , drop = F] 
    nbr_hex_id <- stat_area_dist_lst[[x[3]]]
    box_mtx[box_mtx[, 3] %in% nbr_hex_id, 4]
  }) # 0.5 sec, but should save time when identifying number of neighbors?
  # each element of this list corresponds to BOX_ID and contains all neighboring BOX_IDs
  
  #===================================================#
  # Split up the dataset by year_col and stratum_cols #
  #===================================================#
  
  if(!is.null(dmn_cols)) {
    data_dmn <- copy(data)       # Create a copy that will keep dmn_cols
    data <- unique(data[, -..dmn_cols])  # Remove 'dmn_cols' for now
  }
  
  group_cols  <- c(year_col, stratum_cols)
  data_lst <- lapply(
    X = split(x = subset(data, select = c(group_cols, "BOX_ID", "TRIP_ID")), by = group_cols, keep.by = F),
    FUN = as.matrix)

  # Make the frequency table of each TRIP_ID (so that trips are properly split by HEX_ID, TIME
  trip_id_mat <- do.call(rbind, lapply(
    data_lst,
    function(p) {
      trip_id_frq <- table(p[, "TRIP_ID"])
      matrix(
        c(as.integer(names(trip_id_frq)), trip_id_frq),
        ncol = 2, dimnames = list(NULL, c("TRIP_ID", "Freq")))
    }))
  trip_id_mat <- trip_id_mat[order(trip_id_mat[,1]), ]
  trip_id_vec <- vector(mode = "integer")
  trip_id_vec[trip_id_mat[, 1]] <- trip_id_mat[, 2]
  
  #==================================================#
  # Identify which TRIP_IDs are neighboring each box #
  #==================================================#
  
  box_smry <- lapply(
    data_lst,
    function(x) {
      # x <- data_lst[[1]] # as [1:BOX_ID] [2:TRIP_ID]
      # Get all unique time and space post-strata
      x1 <- unique(x[, 1])
      
      # Now for each BOX_ID listed in 'x1', count the number unique TRIP_IDs in neighboring BOX_IDs
      x2 <- do.call(rbind, lapply(x1, function(y) {
        # y <- x1[20]
        
        trip_id_centered <- x[x[,1] == y, 2]
        # There shouldn't ever be the same trip counted twice in the same box.
        if( length(trip_id_centered) != length(unique(trip_id_centered))) stop("There is a duplicate trip_id!")
        
        cbind(
          BOX_ID = y,
          # Count of trips centered in BOX_ID
          BOX_n = length(trip_id_centered) ,
          # Weight of trips centered in BOX_ID
          # FIXME calculating BOX_w is slow!
          BOX_w = sum(1/trip_id_vec[trip_id_centered]), 
          # # Count of unique TRIP_IDs in neighboring BOX_IDs
          BOX_nbr = length(unique(x[(x[,1] %in% nbr_lst[[y]]), 2])) 
        )
      }))
    }
  ) 
  
  # Calculate STRATA_N and ensure the sum of weights is equivalent
  strata_N_dt <- data[, .(STRATA_N = uniqueN(TRIP_ID)), by= group_cols]
  
  # Double-check that weights sum to STRATA_N
  if(!(all(
    unname(sapply(box_smry, function(x) sum(x[,"BOX_w"]))) == strata_N_dt $STRATA_N
  ))) stop("STRATA_N and sum(BOX_w) are not equal!")
  
  # Create data.table output
  st_dt <- unique(data[, .(BOX_ID, HEX_ID, TIME)])   # Table of BOX_ID, HEX_ID, and TIME
  dt_out <- setcolorder(
    rbindlist(lapply(box_smry, as.data.table), idcol = "GROUP_COLS")[
      ][, (group_cols) := tstrsplit(GROUP_COLS, split = "[.]")
      ][, GROUP_COLS := NULL],
    c(group_cols, "BOX_ID", "BOX_n", "BOX_w", "BOX_nbr"))[
    ][st_dt, on = .(BOX_ID)]
  dt_out[, which(colnames(dt_out) == year_col) := as.integer(dt_out$ADP)]
  
  # Initialize outputs
  box_res <- list(
    box_smry = box_smry,
    strata_n_dt = strata_N_dt,
    dt_out = dt_out,
    og_data = data,
    nbr_lst = nbr_lst,
    params = list(stratum_cols = stratum_cols, year_col = year_col, dmn_cols = dmn_cols)
  )
  
  #==========================#
  # Handle dmn_cols now (GEAR)
  #==========================#
  
  if(!is.null(dmn_cols)) {
    
    # Get trip weights, splitting by dmn_cols as well
    trip_id_dmn_mat <- as.matrix(data_dmn[, .N, by = .(TRIP_ID)])
    trip_id_dmn_vec <- vector(mode = "integer")
    trip_id_dmn_vec[trip_id_dmn_mat[, 1]] <- trip_id_dmn_mat[, 2]
    
    # Identify all BOX_IDs within a dmn_tbl group
    dmn_tbl <- unique(data_dmn[, ..dmn_cols])
    dmn_lst <- vector(mode = "list", length = nrow(dmn_tbl))
    for(i in 1:nrow(dmn_tbl)) {
      # i <- 1
      
      focus_dmn_dat <- unique(subset(data_dmn[dmn_tbl[i,], on = dmn_cols], select = c(stratum_cols, "BOX_ID", "TRIP_ID")))
      focus_dmn_dat_stratum <- lapply(split(focus_dmn_dat, by = stratum_cols, keep.by = F), as.matrix)
      focus_dmn_box_ids <- unique(focus_dmn_dat$BOX_ID)

      # for each stratum...
      focus_dmn_nbr_lst <- lapply(focus_dmn_dat_stratum, function(x) {
        # x <- focus_dmn_dat_stratum[[1]]
        
        # for each BOX_ID
        x1 <- lapply(focus_dmn_box_ids, function(y) {
          # y <- focus_dmn_box_ids[[1]]
          
          trip_id_centered <- unique(x[x[,1] == y, 2])
          
          c(
            BOX_ID = y,
            BOX_DMN_n = length(trip_id_centered),
            BOX_DMN_w = sum(1/trip_id_dmn_vec[trip_id_centered]),
            BOX_DMN_nbr = length(unique(x[x[,1] %in% nbr_lst[[y]]]))
          )

        })
        as.data.table(do.call(rbind, x1))
        
      })
      focus_dmn_nbr_lst <- rbindlist(focus_dmn_nbr_lst, idcol = "STRATUM_COLS")
      focus_dmn_nbr_lst[, (stratum_cols) := tstrsplit(STRATUM_COLS, split = "[.]")][, STRATUM_COLS := NULL]
      dmn_lst[[i]] <- focus_dmn_nbr_lst
    }
    names(dmn_lst) <- apply(dmn_tbl, 1, paste0, collapse = ".")
    dmn_nbr_dt <- rbindlist(dmn_lst, idcol = "DMN_COLS")
    dmn_nbr_dt[, (dmn_cols) := tstrsplit(DMN_COLS, split = "[.]")][, DMN_COLS := NULL]
    
    box_id_details <- unique(data_dmn[, .(BOX_ID, ADP, HEX_ID, TIME)])
    dmn_nbr_dt <- box_id_details[dmn_nbr_dt, on = .(BOX_ID)]
    setcolorder(dmn_nbr_dt, c(year_col, stratum_cols, dmn_cols, "BOX_ID", "BOX_DMN_n", "BOX_DMN_w", "BOX_DMN_nbr"))
    
    # Calculate Number of trips in each STRATA x dmn_cols. Note that trips that have multiple 
    # 'dmn_cols' were split here!
    strata_dmn_N_dt <- dmn_nbr_dt[, .(STRATA_DMN_N = sum(BOX_DMN_w)), by = c(year_col, stratum_cols, dmn_cols)]
    
    # Double-check that weights sum to STRATA_N
    if(!fsetequal(
      strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)],
      strata_N_dt
    )) stop("STRATA_N and sum(BOX_DMN_w) are not equal!")
    
    box_res$dmn <- list()
    box_res$dmn$strata_dmn_n_dt <- strata_dmn_N_dt
    box_res$dmn$box_dmn_smry_dt <- dmn_nbr_dt
    
  }
  
  # Create sf object with geometry if requested
  if(geom == T) {
    geom_sf <- merge(stat_area_lst$HEX_GEOMETRY, dt_out, on = .(HEX_ID))
    box_res$geom_sf <- geom_sf
    
    if(!is.null(dmn_cols)) {
      geom_dmn_sf <- merge(stat_area_lst$HEX_GEOMETRY, dmn_nbr_dt, on = .(HEX_ID))
      box_res$dmn$geom_dmn_df <- geom_dmn_sf
    }
  }

  # Return results
  box_res
  
}


# A revised version that lets you specify the size of temporal unit (not just week or month)
# This frees up the ability to evaluate a wide range of temporal extents and overlaps!
define_boxes_2 <- function(data, space, time, time_cols, year_col, stratum_cols, dmn_cols = NULL, stata_area_sf = stat_area_sf, geom = F) {
  # data <- copy(val_2018_2022_dt[STRATA != "ZERO"]); space <- c(2.5e5, 2.5e5); time <- c(5, 1); time_cols <- c("TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; dmn_cols <- NULL; stratum_cols <- c("STRATA"); geom = T;
  # Now with gear as a post-stratum column...
  # data <- copy(val_2018_2022_dt[STRATA != "ZERO"]); space <- c(2.5e5, 2.5e5); time <- c(5, 1); time_cols <- c("TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; dmn_cols <-c("FMP", "GEAR"), ; stratum_cols <- c("STRATA"); geom = T;
  #==================================#
  # NOTE! Remove any jig-gear trips! #
  #==================================# 
  
  # Jig-gear trips are generally in zero coverage (though sometimes HAL or EM_HAL trips might also fish
  # with jig gear in a trip?). Therefore, from an allocation perspective, it's not important if the rate will be 0.
  # Additionally, from an evaluation perspective, we don't use observed trips to make estimates for jig trips, so
  # we can remove them without issue. MAKE SURE 'data' HAS NO JIG GEAR COMPONENTS!
  
  # Make sure integer TRIP_ID, integer ADFG_STAT_AREA_CODE, and 'dmn_cols' are specified in the data
  if( length( intersect(colnames(data), c("TRIP_ID", "ADFG_STAT_AREA_CODE", dmn_cols))) !=  length(c("TRIP_ID", "ADFG_STAT_AREA_CODE", dmn_cols)) ) {
    stop(paste0("'data' must have columns 'TRIP_ID' and 'ADFG_STAT_AREA_CODE and dmn_cols: ", paste(dmn_cols, collapse = ", ")))
  } else {
    if( all(lapply(data[, c("TRIP_ID", "ADFG_STAT_AREA_CODE")], class) != "integer")) {
      stop ("'TRIP_ID' and 'ADFG_STAT_AREA_CODE' must be of class integer.")
    }
  }
  if(nrow(unique(data[, .(TRIP_ID, STRATA)])) != uniqueN(data$TRIP_ID)) stop("A trip is not in only one stratum!")
  
  # Remove any unused columns
  keep_cols <- c(year_col, stratum_cols, dmn_cols, "ADFG_STAT_AREA_CODE", time_cols[1], time_cols[2], "TRIP_ID")
  data <- unique(subset(data, select = keep_cols))      
  
  #========================#
  # Convert ADFG to HEX_ID #
  #========================#
  
  stat_area_lst <- stat_area_to_hex(space[1], stat_area_sf)
  stat_area_dist_lst <- suppressWarnings(apply(
    X = round(st_distance(st_centroid(stat_area_lst$HEX_GEOMETRY))), 
    MARGIN = 1, 
    FUN = function(x) which(x <= space[2])))
  data <- unique(data.table(stat_area_lst$STAT_AREA_HEX_DF)[
  ][data, on = .(ADFG_STAT_AREA_CODE)
  ][, -"ADFG_STAT_AREA_CODE"])
  setcolorder(data, neworder = c(year_col, stratum_cols, dmn_cols, "HEX_ID", time_cols[1], time_cols[2]))
  setkeyv(data, cols = c(year_col, stratum_cols, dmn_cols, "HEX_ID", time_cols[1], time_cols[2]))
  
  if(nrow(data[is.na(HEX_ID)])) {
    print(data[is.na(HEX_ID)])
    stop("Could not assign a HEX_ID!")
  }
  
  #======================#
  # Define temporal unit #
  #======================#
  
  # First, get all years and a table converting date to week
  # 
  #   dates_lst <- lapply(
  #     lapply(
  #       unique(unlist(data[, ..year_col])),
  #       function(x) as.Date(paste0(x, c("-01-01", "-12-31")))
  #     ),
  #     function(x) as.Date(x[1] : x[2], origin = as.POSIXct("1970-01-01", tz = "UTC"))
  #   )
  
  date_range <- as.integer(as.Date(paste0(range(data[, ..year_col]), c("-01-01", "-12-31"))))
  dates_int <- date_range[1] : date_range[2]
  time_vec <- cut(dates_int, breaks = seq(date_range[1], date_range[2], by = time[1]), include.lowest = T, labels = F)
  time_mtx <- cbind(dates_int, time_vec)
  
  # Now for each date, define which temporal box it belongs to!
  
  
  # dates_mtx <- cbind(unlist(dates_lst), unlist(lapply(dates_lst, get(time[1]))))
  # 
  
  dates_start <- min(time_mtx[, 1]) - 1  # Get first date and subtract 1. T
  time_mtx[, 1] <- time_mtx[, 1] - dates_start  # This makes it so matrix can be reference by row index, much faster
  
  # Grab time columns and define groups based on TRIP_ID and HEX_ID
  time_int <- data[, ..time_cols]
  time_int[, (time_cols) := lapply(.SD, as.integer), .SDcols = time_cols]
  setnames(time_int, new = c("S", "E"))
  data_int <- data[, .(TRIP_ID, HEX_ID)][, GRP := .GRP, by = .(TRIP_ID, HEX_ID)]
  # Convert to matrix and split by TRIP_ID and HEX_ID
  time_lst <- as.matrix(cbind(time_int - dates_start, data_int[, .(GRP)]))
  time_lst <- lapply(split(time_lst, time_lst[, "GRP"], drop = F), matrix, ncol = 3)
  # For each TRIP_ID x HEX_ID, identify unique time boxes
  time_lst <- lapply(time_lst, function(x) {
    dates_int <- unique(unlist(apply(x, 1, function(y) y[1] : y[2], simplify = F)))  # get unique days
    unique(time_mtx[dates_int, 2, drop = F])                     # Identify week using dates_mtx
  }) 
  
  # Collapse into a data.table (this is much faster than rbindlist(lapply(data_lst, as.data.table)))
  data_dt <- as.data.table(
    do.call(rbind, Map(function(x1, x2) cbind(x1, x2), x1 = as.list(seq_along(time_lst)), x2 = time_lst)))
  setnames(data_dt, new = c("GRP", "TIME"))
  # Merge
  data_int <- unique(data_int)[data_dt, on = .(GRP)][, - "GRP"]
  keep_cols2 <- c(setdiff(keep_cols, c(time_cols, "ADFG_STAT_AREA_CODE")), "HEX_ID")
  data <- unique(data[, ..keep_cols2])[data_int, on = .(TRIP_ID, HEX_ID)]
  
  #============#
  # Define Box #
  #============#
  
  setkeyv(data, c(year_col, stratum_cols, "TIME", "HEX_ID"))
  setcolorder(data, c(year_col, stratum_cols, "TIME", "HEX_ID"))
  # 'BOX_ID' is defined by HEX_ID and TIME (including year_col), and is common to all data. This will be used to determine neighbors
  data[, BOX_ID := .GRP, by = c(year_col, "TIME", "HEX_ID")]
  setkey(data, BOX_ID)
  
  #==================#
  # Define Neighbors #
  #==================#
  
  # For each BOX_ID, find which BOX_IDs are neighboring based on week and spatial cells.
  sub_cols <- c(year_col, "TIME", "HEX_ID", "BOX_ID")
  st_mtx <- as.matrix(unique(data[, ..sub_cols]))
  nbr_lst <- apply(st_mtx, MARGIN = 1, function(x) {
    # x <- c(2022, 27, 106, 6047)
    box_mtx <- st_mtx[st_mtx[,1] == x[1], , drop = F]
    nbr_time <- (x[2] + (-time[2]:time[2])) 
    box_mtx <- box_mtx[box_mtx[,2] %in% nbr_time, , drop = F] 
    nbr_hex_id <- stat_area_dist_lst[[x[3]]]
    box_mtx[box_mtx[, 3] %in% nbr_hex_id, 4]
  }) # 0.5 sec, but should save time when identifying number of neighbors?
  # each element of this list corresponds to BOX_ID and contains all neighboring BOX_IDs
  
  #===================================================#
  # Split up the dataset by year_col and stratum_cols #
  #===================================================#
  
  if(!is.null(dmn_cols)) {
    data_dmn <- copy(data)       # Create a copy that will keep dmn_cols
    data <- unique(data[, -..dmn_cols])  # Remove 'dmn_cols' for now
  }
  
  group_cols  <- c(year_col, stratum_cols)
  data_lst <- lapply(
    X = split(x = subset(data, select = c(group_cols, "BOX_ID", "TRIP_ID")), by = group_cols, keep.by = F),
    FUN = as.matrix)
  
  # Make the frequency table of each TRIP_ID (so that trips are properly split by HEX_ID, TIME
  trip_id_mat <- do.call(rbind, lapply(
    data_lst,
    function(p) {
      trip_id_frq <- table(p[, "TRIP_ID"])
      matrix(
        c(as.integer(names(trip_id_frq)), trip_id_frq),
        ncol = 2, dimnames = list(NULL, c("TRIP_ID", "Freq")))
    }))
  trip_id_mat <- trip_id_mat[order(trip_id_mat[,1]), ]
  trip_id_vec <- vector(mode = "integer")
  trip_id_vec[trip_id_mat[, 1]] <- trip_id_mat[, 2]
  
  #==================================================#
  # Identify which TRIP_IDs are neighboring each box #
  #==================================================#
  
  box_smry <- lapply(
    data_lst,
    function(x) {
      # x <- data_lst[[1]] # as [1:BOX_ID] [2:TRIP_ID]
      # Get all unique time and space post-strata
      x1 <- unique(x[, 1])
      
      # Now for each BOX_ID listed in 'x1', count the number unique TRIP_IDs in neighboring BOX_IDs
      x2 <- do.call(rbind, lapply(x1, function(y) {
        # y <- x1[20]
        
        trip_id_centered <- x[x[,1] == y, 2]
        # There shouldn't ever be the same trip counted twice in the same box.
        if( length(trip_id_centered) != length(unique(trip_id_centered))) stop("There is a duplicate trip_id!")
        
        cbind(
          BOX_ID = y,
          # Count of trips centered in BOX_ID
          BOX_n = length(trip_id_centered) ,
          # Weight of trips centered in BOX_ID
          # FIXME calculating BOX_w is slow!
          BOX_w = sum(1/trip_id_vec[trip_id_centered]), 
          # # Count of unique TRIP_IDs in neighboring BOX_IDs
          BOX_nbr = length(unique(x[(x[,1] %in% nbr_lst[[y]]), 2])) 
        )
      }))
    }
  ) 
  
  # Calculate STRATA_N and ensure the sum of weights is equivalent
  strata_N_dt <- data[, .(STRATA_N = uniqueN(TRIP_ID)), by= group_cols]
  
  # Double-check that weights sum to STRATA_N
  if(!(all(
    unname(sapply(box_smry, function(x) sum(x[,"BOX_w"]))) == strata_N_dt $STRATA_N
  ))) stop("STRATA_N and sum(BOX_w) are not equal!")
  
  # Create data.table output
  st_dt <- unique(data[, .(BOX_ID, HEX_ID, TIME)])   # Table of BOX_ID, HEX_ID, and TIME
  dt_out <- setcolorder(
    rbindlist(lapply(box_smry, as.data.table), idcol = "GROUP_COLS")[
    ][, (group_cols) := tstrsplit(GROUP_COLS, split = "[.]")
    ][, GROUP_COLS := NULL],
    c(group_cols, "BOX_ID", "BOX_n", "BOX_w", "BOX_nbr"))[
    ][st_dt, on = .(BOX_ID)]
  dt_out[, which(colnames(dt_out) == year_col) := as.integer(dt_out$ADP)]
  
  # Initialize outputs
  box_res <- list(
    box_smry = box_smry,
    strata_n_dt = strata_N_dt,
    dt_out = dt_out,
    og_data = data,
    nbr_lst = nbr_lst,
    params = list(stratum_cols = stratum_cols, year_col = year_col, dmn_cols = dmn_cols)
  )
  
  #==========================#
  # Handle dmn_cols now (GEAR)
  #==========================#
  
  if(!is.null(dmn_cols)) {
    
    # Get trip weights, splitting by dmn_cols as well
    trip_id_dmn_mat <- as.matrix(data_dmn[, .N, by = .(TRIP_ID)])
    trip_id_dmn_vec <- vector(mode = "integer")
    trip_id_dmn_vec[trip_id_dmn_mat[, 1]] <- trip_id_dmn_mat[, 2]
    
    # Identify all BOX_IDs within a dmn_tbl group
    dmn_tbl <- unique(data_dmn[, ..dmn_cols])
    dmn_lst <- vector(mode = "list", length = nrow(dmn_tbl))
    for(i in 1:nrow(dmn_tbl)) {
      # i <- 1
      
      focus_dmn_dat <- unique(subset(data_dmn[dmn_tbl[i,], on = dmn_cols], select = c(stratum_cols, "BOX_ID", "TRIP_ID")))
      focus_dmn_dat_stratum <- lapply(split(focus_dmn_dat, by = stratum_cols, keep.by = F), as.matrix)
      focus_dmn_box_ids <- unique(focus_dmn_dat$BOX_ID)
      
      # for each stratum...
      focus_dmn_nbr_lst <- lapply(focus_dmn_dat_stratum, function(x) {
        # x <- focus_dmn_dat_stratum[[1]]
        
        # for each BOX_ID
        x1 <- lapply(focus_dmn_box_ids, function(y) {
          # y <- focus_dmn_box_ids[[1]]
          
          trip_id_centered <- unique(x[x[,1] == y, 2])
          
          c(
            BOX_ID = y,
            BOX_DMN_n = length(trip_id_centered),
            BOX_DMN_w = sum(1/trip_id_dmn_vec[trip_id_centered]),
            BOX_DMN_nbr = length(unique(x[x[,1] %in% nbr_lst[[y]]]))
          )
          
        })
        as.data.table(do.call(rbind, x1))
        
      })
      focus_dmn_nbr_lst <- rbindlist(focus_dmn_nbr_lst, idcol = "STRATUM_COLS")
      focus_dmn_nbr_lst[, (stratum_cols) := tstrsplit(STRATUM_COLS, split = "[.]")][, STRATUM_COLS := NULL]
      dmn_lst[[i]] <- focus_dmn_nbr_lst
    }
    names(dmn_lst) <- apply(dmn_tbl, 1, paste0, collapse = ".")
    dmn_nbr_dt <- rbindlist(dmn_lst, idcol = "DMN_COLS")
    dmn_nbr_dt[, (dmn_cols) := tstrsplit(DMN_COLS, split = "[.]")][, DMN_COLS := NULL]
    
    box_id_details <- unique(data_dmn[, .(BOX_ID, ADP, HEX_ID, TIME)])
    dmn_nbr_dt <- box_id_details[dmn_nbr_dt, on = .(BOX_ID)]
    setcolorder(dmn_nbr_dt, c(year_col, stratum_cols, dmn_cols, "BOX_ID", "BOX_DMN_n", "BOX_DMN_w", "BOX_DMN_nbr"))
    
    # Calculate Number of trips in each STRATA x dmn_cols. Note that trips that have multiple 
    # 'dmn_cols' were split here!
    strata_dmn_N_dt <- dmn_nbr_dt[, .(STRATA_DMN_N = sum(BOX_DMN_w)), by = c(year_col, stratum_cols, dmn_cols)]
    
    # Double-check that weights sum to STRATA_N
    if(!fsetequal(
      strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)],
      strata_N_dt
    )) stop("STRATA_N and sum(BOX_DMN_w) are not equal!")
    
    box_res$dmn <- list()
    box_res$dmn$strata_dmn_n_dt <- strata_dmn_N_dt
    box_res$dmn$box_dmn_smry_dt <- dmn_nbr_dt
    
  }
  
  # Create sf object with geometry if requested
  if(geom == T) {
    geom_sf <- merge(stat_area_lst$HEX_GEOMETRY, dt_out, on = .(HEX_ID))
    box_res$geom_sf <- geom_sf
    
    if(!is.null(dmn_cols)) {
      geom_dmn_sf <- merge(stat_area_lst$HEX_GEOMETRY, dmn_nbr_dt, on = .(HEX_ID))
      box_res$dmn$geom_dmn_df <- geom_dmn_sf
    }
  }
  
  # Return results
  box_res
  
}


#======================================================================================================================#
# Allocation Functions -------------------------------------------------------------------------------------------------
#======================================================================================================================#

## Equal Allocation ----------------------------------------------------------------------------------------------------

# This function calculates the rates afforded with equal allocation. It is also used by allo_min_plus_opt if the budget
# cannot afford 'optimized' days.
allo_equal <- function(x, ob_budget){
  # x <- copy(status_quo_dt); ob_budget <- (4.5e6 - 1e6); trip_costs <- copy(trip_cost_dt)
  
  adp_years <- unique(x$ADP)
  
  adp_list <- lapply(adp_years, function(adp) {
    # adp <- 2018
    
    x[ADP == adp
    ][, EFF_D := sum(STRATA_N * TRP_DUR)    # Effort as total number of days across strata
    ][, AFF_D := ob_budget / CPD
    ][, RAW_OB_RATE := AFF_D/EFF_D        # Raw equal rate afforded
    ][, RAW_OB_N := STRATA_N * RAW_OB_RATE      # Raw Expected number of trips observed within strata
    ][, OB_N := RAW_OB_N + (STRATA_N/sum(STRATA_N)) # Final number of trips expected to be observed 
    ][, OB_RATE := sum(OB_N)/sum(STRATA_N)  # Final equal rate  
    ][, OB_D := OB_N * TRP_DUR      # Final number of days expected to be observed
    ][, STRATA := as.factor(STRATA)
    ][, OPT_N := NA]
    
  })
  out_dt <- rbindlist(adp_list)
  setkey(out_dt, ADP, STRATA)
  
  if( !("MIN_RATE") %in% colnames(x) ) {
    return(out_dt[, .(ADP, STRATA, STRATA_N, TRP_DUR, PRIOR_MTD, CPD, MIN_RATE = NA_real_, MIN_N = NA_real_, MIN_D = NA_real_, TOT_MIN_D = NA_real_, OB_RATE, OB_N, OPT_N, OB_D)])
  }
  
  out_dt[, .(ADP, STRATA, STRATA_N, TRP_DUR, PRIOR_MTD, CPD, MIN_RATE, MIN_N = NA_real_, MIN_D = NA_real_, TOT_MIN_D = NA_real_, OB_RATE, OB_N, OPT_N, OB_D)]
}


## Minimum + Optimization (Status Quo) ---------------------------------------------------------------------------------

# This function calculates the selection rate required to sample ceiling(MIN_RAT * STRATA_N) trips to a specified 
# confidence level. Used by allo_min_plus_opt().
find_conf_rate <- function(strata_n, min_rate, conf){
  # strata_n <- 1270; min_rat <- 0.15; conf <- 0.95
  if(conf < 0.5) stop("'conf' must be set to 0.5 at the minimum!")
  if(conf > 1)   stop("'conf' must be set below 1!")
  if(conf == 0.5) return(min_rate)                                # If the confidence level as 0.5, just return min_rate
  
  # Start with qbinom, then refine with pbinom iteratively
  conf_rate <- qbinom(p=conf, size = strata_n, prob = min_rate)/strata_n                               
  dir <- sign(conf - pbinom(ceiling(min_rate * strata_n), strata_n, conf_rate, lower.tail=F))
  step <- 1e-6 * dir                                                                             
  while( sign(conf - pbinom(ceiling(min_rate * strata_n), strata_n, conf_rate, lower.tail=F)) == dir){
    conf_rate <- conf_rate + step
  }
  if(dir == -1) conf_rate <- conf_rate - step                               # Step back once if initial direction was -1
  
  return(conf_rate)
}


# Meets the hurdle (minimum rate) to a specified confidence interval, and then allocated remaining 'optimized days' 
# according to optimization weights from blended discards, halibut PSC, and Chinook PSC 
# Using conf=0.5 is the same as a 15% minimum. Increasing conf to 0.95, however, increases the minimum rate until there
# is 95% confidence that random selection will result in >= hurdle rate. If the 15% minimum can be afforded but cannot
# be achieved at the specified confidence level, it will allocate at the highest confidence level afforded..
allo_min_plus_opt <- function(x, conf, tm, MIN_RATE = 0.15, ob_budget){
  # x <- copy(status_quo_dt); conf <- 0.95; tm <- copy(trips_melt); MIN_RATE <- 0.15   # for 95% confidence (new method)
  # x <- copy(status_quo_dt); conf <- 0.50; tm <- copy(trips_melt); MIN_RATE <- 0.15   # for 15% minimum (old method)
  # ob_budget <- (4.5e6 - 1e6);  # for now, assume 30% of fixed-gear EM costs $1M
  
  x0 <- copy(x)[
  ][, OB_BUDGET := ob_budget
  ][, AFF_D := OB_BUDGET / CPD][]
  
  # Prepare allocation weights from trips_melt (variance of metrics for each stratum and ADP year)
  weights <- tm[STRATA %in% unique(x$STRATA)  , .(S2_h = var(Value), N_h = .N), keyby = .(ADP, Metric, STRATA)]
  
  # For each ADP year...
  adp_years <- unique(x$ADP)
  adp_list <- lapply(adp_years, function(adp) {
    
    # Determine trips/days to achieve the 15% hurdle and the conf_rate according to the confidence level specified
    x1 <- x0[ADP == adp
    ][, MIN_D := STRATA_N * MIN_RATE * TRP_DUR                # Calculate # days to observer to reach minimum hurdle
    ][, TOT_MIN_D := sum(MIN_D)          # Total days required to achieve minimum rate 
    ][, MIN_N := STRATA_N * MIN_RATE     # For each stratum, number of trips to observe to reach minimum rate
    ][, TOT_MIN_N := sum(MIN_N)          # Total trips required to achieve minimum rate before resorting to equal rates
    ][, CONF := conf                     # Include specified confidence level
    ][, CONF_RATE := find_conf_rate(STRATA_N, MIN_RATE, CONF), by = .(STRATA)            # Find selection rates required
    ][, CONF_N := STRATA_N * CONF_RATE       # Number of trips to observe to meet minimum hurdle at confidence specified
    ][, CONF_D := CONF_N * TRP_DUR           # Days to observe in stratum to meet minimum hurdle at confidence specified                           
    ][, TOT_CONF_D := sum(CONF_D)][]   
    setkey(x1, ADP, STRATA)
    opt_days_afforded <- T   # Initialize whether optimized days are afforded 
    
    # If the minimum rate can be met (i.e. at conf=0.5) but not at the specified confidence, start by allocating by the
    # proportion of CONF_RATE above MIN_RATE
    if( (unique(x1$AFF_D) < sum(x1$CONF_D)) & (unique(x1$AFF_D) > sum(x1$MIN_D)) ) {
      
      opt_days_afforded <- F
      warning(
        paste0(adp, " : Minimum rate of ", MIN_RATE, " afforded but not at ", conf, " confidence level.")
        , call. = F
      )
      
      x1[, CONF_RATE_PROP := (CONF_D-MIN_D)/sum(CONF_D-MIN_D)     # Get the proportions that days were allocated          
      ][, CONF_D := MIN_D + (AFF_D-TOT_MIN_D)*CONF_RATE_PROP      # Apply the proportion to estimate days observed
      ][, CONF_N := CONF_D / TRP_DUR                              # Calculate trips observed
      ][, CONF_RATE_NEW := CONF_N/STRATA_N]                       # Calculate ballparked rate afforded
      
      # Get the confidence interval achieved for all strata, and grab the highest one from which to work down until we 
      # fall under AFF_D
      conf_new <- round(max(x1[, .(CONF_NEW = pbinom(
        q = ceiling(MIN_RATE * STRATA_N), size = STRATA_N, prob = CONF_RATE_NEW, lower.tail=F)), 
        by = .(STRATA)]$CONF_NEW), 4)
      if(conf_new < 0.5) conf_new <- 0.5           # If the estimate for conf_new is below 0.5, set it to 0.5.
      GO <- T
      
      while( GO ){
        x1[, CONF_RATE_NEW := find_conf_rate(STRATA_N, MIN_RATE, conf_new), by = STRATA
        ][, CONF_N := CONF_RATE_NEW * STRATA_N
        ][, CONF_D := CONF_N * TRP_DUR]
        if(  sum(x1$CONF_D) > unique(x1$AFF_D) ){
          conf_new <- conf_new - 0.0001                            # If too many days purchased, reduce confidence level
        } else {
          GO <- F                                                  # Once we fall under AFF_D, stop
        }
      }
      x1[, ':=' (CONF = conf_new, CONF_RATE = CONF_RATE_NEW)        # Update and clean up
      ][, TOT_CONF_D := sum(CONF_D)
      ][, c("CONF_RATE_PROP", "CONF_RATE_NEW") := NULL]
    }
    
    # Calculate number of optimized days afforded
    x1[, C_N := CPD * PRIOR_MTD                               # Average Cost of observing one trip within stratum
    ][, MIN_C := sum(CPD * TRP_DUR * CONF_N), by = .(ADP)     # total cost of affording hurdle
    ][, OPT_BUD := OB_BUDGET - MIN_C                          # budget remaining for optimization after affording hurdle
    ][, OPT_STRATA_N := STRATA_N - CONF_N                     # trips after accounting for trips below the hurdle
    ][, STRATA := as.factor(STRATA)] 
    x1 <- weights[x1, on =.(ADP, STRATA)]                     # Merge in optimization metrics
    x1[, S_h := sqrt(S2_h)                                    # metric standard deviation
    ][, N_h_S_h_div_sqrtC := (OPT_STRATA_N * S_h) / sqrt(C_N)       
    ][, N_h_S_hC := OPT_STRATA_N * S_h * sqrt(C_N)
    ][, sumN_h_S_h_div_sqrtC := sum(N_h_S_h_div_sqrtC), by = .(ADP, Metric)
    ][, sumN_h_S_hC := sum(N_h_S_hC), by = .(ADP, Metric)
    ][, TTL_OPT_N := (OPT_BUD * sumN_h_S_h_div_sqrtC) / sumN_h_S_hC      # Total number of optimized trips across strata
    ][, W_hopt := N_h_S_h_div_sqrtC / sumN_h_S_h_div_sqrtC
    ][, METRIC := "dscd_hlbt_chnk"]                          # coercing the metric column to blended, which happens next 
    x1 <- x1[, .(
      ADP, STRATA, STRATA_N, TRP_DUR, PRIOR_MTD, CPD, MIN_RATE, CONF, CONF_RATE, AFF_D, CPD, TOT_MIN_D, TOT_CONF_D,
      OPT_BUD, MIN_N, MIN_D, CONF_N, CONF_D, METRIC, W_hopt)]
    x1 <- x1[, lapply(.SD, mean), .SDcols=c("W_hopt"), by = .(
      ADP, STRATA, STRATA_N, TRP_DUR, PRIOR_MTD, MIN_RATE, CONF, CONF_RATE, AFF_D, CPD, TOT_MIN_D, TOT_CONF_D, 
      OPT_BUD, MIN_N, MIN_D, CONF_N, CONF_D,  METRIC)]
    setkey(x1, ADP, STRATA)
    
    # Now apply the blended weightings 
    x1[, RAW_OPT_N := OPT_BUD / CPD * W_hopt / TRP_DUR]               # Initial estimate of optimized trips afforded 
    if(opt_days_afforded == F) x1[, OPT_N := 0] else x1[, OPT_N := RAW_OPT_N]
    x1[, OPT_D := OPT_N * TRP_DUR                                     # Estimate optimized days afforded
    ][, OB_D := CONF_D + OPT_D                                        # Total observed days
    ][, OB_N := CONF_N + OPT_N                                        # Total observed trips
    ][, OB_RATE := OB_N / STRATA_N]                                   # Monitoring rate
    
    # If the minimum rate was not afforded, revert to equal rates. 
    if( unique(x1$AFF_D) < unique(x1$TOT_MIN_D) ){
      warning(
        paste0(adp, " : Minimum rate of ", MIN_RATE, " not afforded. Resorting to 'Equal Rates' allocation."), 
        call. = F)
      
      opt_days_afforded <- F
      conf_attempt <- copy(x1)[, .(STRATA, METRIC, W_hopt, CONF, CONF_RATE, CONF_N, CONF_D, TOT_CONF_D)]
      x1 <- allo_equal(x1, ob_budget)[conf_attempt, on = .(STRATA)] 
    }
    
    x1[, .(
      ADP, STRATA, STRATA_N, TRP_DUR, PRIOR_MTD, CPD, MIN_RATE, CONF, CONF_RATE, METRIC, W_hopt, MIN_N, MIN_D, 
      TOT_MIN_D, CONF_N, CONF_D, TOT_CONF_D, OB_RATE, OB_N, OPT_N, OB_D)]
  })
  
  out_dt <- setkey(rbindlist(adp_list), ADP, STRATA)
  out_dt
  
}


## Proximity -----------------------------------------------------------------------------------------------------------

# Calculate the interspersion = expected proportion of trips sampled or neighboring a sampled trip, 
# within strata given a vector of sampling rates
calculate_interspersion <- function(box_res, sample_rate_vec, omit_strata = c(NULL)) {
  # omit_strata <- NULL
  # omit_strata <- "ZERO"
  # x <- 0.15; y <- box_res$box_smry[[1]]
  
  group_cols <- c(box_res$params$year_col, box_res$params$stratum_cols)
  year_col <- box_res$params$year_col
  
  # Use 'omit_strata' to omit any unmonitored strata
  if(!is.null(omit_strata)) {
    keep_strata <- !apply(sapply(omit_strata, function(x) names(box_res$box_smry) %like% x), 1, any)
  } else {
    keep_strata <- rep(T, times = length(box_res$box_smry))
  }
  
  # For for a range of sample rates, calculate the probably that a post-stratum would be near a sampled neighbor
  # (0-1), and then multiply it by that post-stratum's total weight of component trips centered on the post-stratum.
  
  # For each sample rate...
  ispn_lst <- lapply(
    sample_rate_vec,
    function(x) {
      
      # For each stratum...
      sapply(
        box_res$box_smry[keep_strata],
        function(y) {
          
          # For each stratum's BOX_ID, use BOX_nbr to calculate the probability that the box is sampled,
          # and then multiply that by BOX_w to get the expected number of sampled trips in the box. Sum across
          # all boxes to get expected number of sampled trips in stratum.
          # x is the sample rate, y[,4] is 'BOX_nbr' and y[, 3] is 'BOX_w'. Referencing by column is faster.
          
          sum((1 - ((1 - x)^y[,4])) * y[,3])
          # sum((1 - ((1 - x)^y[,"BOX_nbr"])) * y[,"BOX_w"])
          
        }
      )
    }
  )
  
  # Package the results, converting to data.table
  ispn_lst <- lapply(ispn_lst, function(z)  data.frame(GROUP_COLS = names(z), sum_pw = z))
  names(ispn_lst) <- sample_rate_vec
  ispn_dt <- rbindlist(ispn_lst, idcol = "SAMPLE_RATE")[
  ][, SAMPLE_RATE := as.numeric(SAMPLE_RATE)
  ][, (group_cols) := tstrsplit(GROUP_COLS, split = "[.]")
  ][, GROUP_COLS := NULL]
  ispn_dt[, (year_col) := lapply(.SD, as.integer), .SDcol = year_col]
  
  ispn_dt <- box_res$strata_n_dt[
  ][ispn_dt, on = group_cols
  ][, ISPN := sum_pw / STRATA_N][]
  
  setcolorder(ispn_dt, c("SAMPLE_RATE", group_cols, "ISPN"))
  
  ispn_res <- list(
    ispn_dt = ispn_dt,
    strata_n_dt = box_res$strata_n_dt,
    params = box_res$params
  )
  
  ispn_res
  
}


# Combines Interspersion and CV_scaling into an index. Allocates such that all strata have the same index, and determines cost.
# This function is mostly unchanged from the original (just some renaming)
calculate_index <- function(ispn_res, trip_cost_dt) {
  
  group_cols <- c(ispn_res$params$year_col, ispn_res$params$stratum_cols)
  stratum_cols <- ispn_res$params$stratum_cols
  year_col <- ispn_res$params$year_col
  
  # Calculate index as INTERSPERSION / CV_SCALING
  x1 <- copy(ispn_res$ispn_dt)[
  ][, n := SAMPLE_RATE * STRATA_N
  ][, FPC := (STRATA_N - n) / STRATA_N
  ][, CV_SCALING := sqrt(FPC * (1/n))
  ][, INDEX := ISPN / CV_SCALING][]
  
  # Find range if INDEX that is common to all strata x ADP
  x2 <- x1[
  ][, .(MIN = min(INDEX), MAX = max(INDEX)), by = group_cols
  ][, .(MIN = max(MIN),  MAX = min(MAX)), by = year_col]
  
  # Subset each year by the index range
  x1 <- do.call(rbind, apply(x2, MARGIN = 1, function(y) {
    x1[get(year_col) == y[[year_col]]][data.table::between(INDEX, y[["MIN"]], y[["MAX"]])]
  }))
  
  # For each ADP year, create a vector from the minimum to maximum index range
  index_vectors <- apply(x2, 1, function(y) seq(y[2], y[3], by = 0.01))
  
  # For each of those indices, find the rates required of all the STRATA to achieve those indices
  # TODO This takes a while - try to speed this up! Is 0.01 increment for index vectors needlessly small?
  # is findInterval slower?
  # Is using .SD what is slowing things down? If I split by stratum_cols, should be faster if I avoid it?
  
  index_rates <- rbindlist(Map(
    function(y1, y2) {
      rbindlist(lapply(y1, function(z) {
        z1 <- data.table(INDEX = z)
        y2[, .SD[z1, on = .(INDEX), roll = "nearest"], keyby = group_cols]
      }))
    },
    y1 = index_vectors,
    y2 = split(x1, by = year_col)
  ))
  # TODO if we calculated INDEX_COST within each index_rates, could make the loop exit when it
  # reaches a specified budget limit?
  
  # Total costs of affording each index
  cost_cols <- names(trip_cost_dt)[names(trip_cost_dt) %in% group_cols]
  
  index_rates[, CPT := trip_cost_dt[index_rates, CPT, on = cost_cols]]   # Merge in stratum-specific trip costs
  index_rates[, INDEX_COST := sum(CPT * n), by = c(year_col, "INDEX")]               # Total stratum costs by ADP and INDEX
  
  index_rates_melt <- melt(
    index_rates, 
    id.vars = c(stratum_cols, "STRATA_N", "INDEX_COST"), 
    measure.vars = c("INDEX", "ISPN", "CV_SCALING", "SAMPLE_RATE"))
  
  index_out <- list(rates = index_rates, melt = index_rates_melt, params = ispn_res$params)
  index_out
}


## Cost-Weighted Boxes -------------------------------------------------------------------------------------------------

# Calculate Ph, the expected proportion of unsampled boxes in each stratum, for the cost-weighted boxes design
calculate_cwb_Ph <- function(box_res, sample_rate_vec, omit_strata = c(NULL)) {
  # omit_strata <- NULL
  # omit_strata <- "ZERO"
  # x <- 0.15; y <- box_res$box_smry[[1]]
  
  group_cols <- c(box_res$params$year_col, box_res$params$stratum_cols)
  year_col <- box_res$params$year_col
  
  # Use 'omit_strata' to omit any unmonitored strata
  if(!is.null(omit_strata)) {
    keep_strata <- !apply(sapply(omit_strata, function(x) names(box_res$box_smry) %like% x), 1, any)
  } else {
    keep_strata <- rep(T, times = length(box_res$box_smry))
  }
  
  # For for a range of sample rates, calculate the probably that a post-stratum would be near a sampled neighbor
  # (0-1), and then multiply it by that post-stratum's total weight of component trips centered on the post-stratum.
  
  # For each sample rate...
  Ph_lst <- lapply(
    sample_rate_vec,
    function(x) {
      
      # For each stratum...
      sapply(
        box_res$box_smry[keep_strata],
        function(y) {
          
          # For each stratum's BOX_ID, use BOX_nbr to calculate the probability that the box is sampled,
          # and then multiply that by BOX_w to get the expected number of sampled trips in the box. Sum across
          # all boxes to get expected number of sampled trips in stratum.
          # x is the sample rate, y[,4] is 'BOX_nbr' and y[, 3] is 'BOX_w'. Referencing by column is faster.
          
          #sum((1 - ((1 - x)^y[,4]))) / nrow(y)  # For each box, calculate the probability of at least 1 trip being sampled
          
          sum(((1 - x)^y[,4])) / nrow(y)  # For each box, calculate the probability of no trips being sampled
          
          # sum((1 - ((1 - x)^y[,"BOX_nbr"])) * y[,"BOX_w"])
          
        }
      )
    }
  )
  
  # Package the results, converting to data.table
  Ph_lst <- lapply(Ph_lst, function(z)  data.frame(GROUP_COLS = names(z), Ph = z))
  names(Ph_lst) <- sample_rate_vec
  Ph_dt <- rbindlist(Ph_lst, idcol = "SAMPLE_RATE")[
  ][, SAMPLE_RATE := as.numeric(SAMPLE_RATE)
  ][, (group_cols) := tstrsplit(GROUP_COLS, split = "[.]")
  ][, GROUP_COLS := NULL]
  Ph_dt[, (year_col) := lapply(.SD, as.integer), .SDcol = year_col]
  Ph_dt <- box_res$strata_n_dt[Ph_dt, on = group_cols]
  
  setcolorder(Ph_dt, c("SAMPLE_RATE", group_cols, "Ph"))
  
  Ph_res <- list(
    Ph_dt = Ph_dt,
    strata_n_dt = box_res$strata_n_dt,
    params = box_res$params
  )
  
  Ph_res
  
}

# Allocates using an assumed sample rate (SAMPLE_RATE) to determine Ph and monitoring costs. 
# The assumed sample rate may not be equivalent to the allocated sample rate, which is what repeated uses of 
# cwv_half_diff is for.
allo_cwb <- function(x, target_budget) {
  x[, `NP/c_h` := (STRATA_N * Ph) / sqrt(CPT)       # Calculate NP/c for each stratum (numerator of equation 5.23)
  ][, `nh/n` := `NP/c_h` / sum(`NP/c_h`), by = .(ADP)        # # Calculate the optimal sample size (equation 5.23)
  ][, n := (target_budget * sum(`NP/c_h`)) / sum(STRATA_N * Ph * sqrt(CPT)), by = .(ADP)   # calculate afforded total sample size (equation 5.24)
  ][, nh := `nh/n` * n       # Calculate the optimal sample size of each stratum n_h using n_h/n * n
  ][, fh := nh / STRATA_N]   # using n_h / N_h, calculate allocated sample rate (sample fraction)
  setkey(x, ADP, STRATA)
  x
}

# Adjusts results of allo_cwb to get the assumed sample rate (SAMPLE_RATE) for Ph to approach the allocated rate fh
allo_cwb_half_diff <- function(x, Ph_dt, target_budget) {
  x <- copy(x)
  x[, SAMPLE_RATE := round(SAMPLE_RATE + (round(fh,3) - SAMPLE_RATE)/2, 3)]  # Adjust the sample rate to half the difference with N_h/N
  x[, Ph := Ph_dt[x, Ph, on = .(STRATA, ADP, SAMPLE_RATE)]]     #update Ph values using new sample rates
  chunk_numbers(x, target_budget)
  print(x)
}


#======================================================================================================================#
# Evaluation Functions -------------------------------------------------------------------------------------------------
#======================================================================================================================#

# Evaluate the overlap of observed trips to other pools of similar gear type
calculate_dmn_interspersion <- function(box_res, selection_rates, donor_strata = data.table(STRATA = c("OB_HAL", "OB_POT"))) {
  # selection_rates <- copy(rates_4.5M); donor_strata <- data.table(STRATA = c("OB_HAL", "OB_POT"))
  
  year_col <- box_res$params$year_col
  stratum_cols <- box_res$params$stratum_cols
  dmn_cols <- box_res$params$dmn_cols
  
  # If 'geom' is present, get HEX_ID geometries
  if(!is.null(box_res$dmn$geom_dmn_df)) hex_id_geom <- box_res$dmn$geom_dmn_df %>% select(HEX_ID, geometry) %>% unique()
  
  # Identify unique groups of post-strata (only those relevant to donor_strata)
  box_res$dmn$strata_dmn_n_dt
  
  dmn_groups <- unique(box_res$dmn$strata_dmn_n_dt[donor_strata, on = stratum_cols][, ..dmn_cols])
  dmn_ispn_lst <- vector(mode = "list", length = nrow(dmn_groups))
  dmn_ispn_geom_lst <- vector(mode = "list", length = nrow(dmn_groups))
  
  # For each post-stratum group...
  for(i in 1:nrow(dmn_groups)) {
    # i <- 1
    
    # Identify focus post-stratum group and subset data
    focus_dmn <- dmn_groups[i,]
    focus_dmn_dt <- box_res$dmn$box_dmn_smry_dt[focus_dmn, on = dmn_cols]
    
    # Split by donor and acceptor
    focus_dmn_donor <- focus_dmn_dt[donor_strata, on = stratum_cols]
    focus_dmn_acceptor <- fsetdiff(focus_dmn_dt, focus_dmn_donor)
    
    # Merge in sample rates of 'donor_strata' and calculate probability that each BOX_ID is sampled
    # Probability is 1 - the combined probability that no strata within a box are sampled
    focus_sample_rate <- selection_rates[donor_strata, on = stratum_cols][, .(SAMPLE_RATE), by = c(year_col, stratum_cols)]
    focus_dmn_donor_prob <- focus_dmn_donor[
    ][, SAMPLE_RATE := focus_sample_rate[focus_dmn_donor, SAMPLE_RATE, on = c(year_col, stratum_cols)]
    ][, NO_SAMPLE_PROB :=  (1 - SAMPLE_RATE)^BOX_DMN_nbr
    ][, .(BOX_SAMPLE_PROB = 1 - prod(NO_SAMPLE_PROB)), by = c("ADP", "BOX_ID")]
    
    # Merge in box sample rates
    focus_dmn_acceptor[, BOX_SAMPLE_PROB := focus_dmn_donor_prob[focus_dmn_acceptor, BOX_SAMPLE_PROB, on = c("ADP", "BOX_ID")]]
    # Calculate expected number of component trips near a sampled trip
    focus_dmn_sum_pw <- focus_dmn_acceptor[, .(sum_DMN_pw = sum(BOX_SAMPLE_PROB * BOX_DMN_w)), by = c(year_col, stratum_cols, dmn_cols)]
    
    
    focus_dmn_sum_pw <- box_res$dmn$strata_dmn_n_dt[focus_dmn_sum_pw , on = c(year_col, stratum_cols, dmn_cols)]
    focus_dmn_sum_pw[, DMN_ISPN := sum_DMN_pw / STRATA_DMN_N]
    
    dmn_ispn_lst[[i]] <- focus_dmn_sum_pw
    
    
    if(!is.null(box_res$dmn$geom_dmn_df)) dmn_ispn_geom_lst[[i]] <- focus_dmn_acceptor
    
  }
  names(dmn_ispn_lst) <- apply(dmn_groups, 1, paste0, collapse = ".")
  dmn_ispn_dt <- rbindlist(dmn_ispn_lst, idcol = "DMN_COLS")
  dmn_ispn_dt[, (dmn_cols) := tstrsplit(DMN_COLS, split = "[.]")][, DMN_COLS := NULL][]
  setorderv(dmn_ispn_dt, c(dmn_cols, year_col, stratum_cols))
  
  dmn_ispn_res <- list(dmn_ispn_dt = dmn_ispn_dt, params = box_res$params)
  if(!is.null(box_res$dmn$geom_dmn_df)) {
    names(dmn_ispn_geom_lst) <- apply(dmn_groups, 1, paste0, collapse =".")
    dmn_ispn_geom <- merge(hex_id_geom, rbindlist(dmn_ispn_geom_lst), by = "HEX_ID")
    dmn_ispn_res$dmn_ispn_geom <- dmn_ispn_geom
  }
  
  dmn_ispn_res
}

# A modified version of the function that allows you to optionally specify 'acceptor strata'
# FIXME - Currently, if you allow EM_POT to be a donor, it also gets applied to the OB strata which isn't the case. It might
# apply to the ZERO strata though? Could we assume that?
calculate_dmn_interspersion2 <- function(box_res, selection_rates, donor_strata = data.table(STRATA = c("OB_HAL", "OB_POT")), acceptor_strata = NULL) {
  # selection_rates <- copy(rates_4.5M); donor_strata <- data.table(STRATA = c("OB_HAL", "OB_POT")); acceptor_strata = NULL
  # selection_rates <- copy(test); donor_strata <- data.table(STRATA = c("OB_HAL", "OB_POT", "EM_HAL", "EM_POT")); acceptor_strata = data.table(STRATA = c("EM_HAL", "EM_POT"))
  
  year_col <- box_res$params$year_col
  stratum_cols <- box_res$params$stratum_cols
  dmn_cols <- box_res$params$dmn_cols
  
  # If 'geom' is present, get HEX_ID geometries
  if(!is.null(box_res$dmn$geom_dmn_df)) hex_id_geom <- box_res$dmn$geom_dmn_df %>% select(HEX_ID, geometry) %>% unique()
  
  # Combine all donor and acceptor strata, if given
  if(!is.null(acceptor_strata)) {
    focus_strata <- merge(donor_strata, acceptor_strata, all = T)
  } else {focus_strata <- copy(donor_strata)}
  
  # Identify unique groups of post-strata (only those relevant to focus_strata)
  dmn_groups <- unique(box_res$dmn$strata_dmn_n_dt[focus_strata, on = stratum_cols][, ..dmn_cols])
  # Initialize output lists
  dmn_ispn_lst <- vector(mode = "list", length = nrow(dmn_groups))
  dmn_ispn_geom_lst <- vector(mode = "list", length = nrow(dmn_groups))
  
  # For each post-stratum group...
  for(i in 1:nrow(dmn_groups)) {
    # i <- 1
    
    # Identify focus post-stratum group and subset data
    focus_dmn <- dmn_groups[i,]
    focus_dmn_dt <- box_res$dmn$box_dmn_smry_dt[focus_dmn, on = dmn_cols]
    
    # Split by donor and acceptor
    focus_dmn_donor <- focus_dmn_dt[donor_strata, on = stratum_cols, nomatch = 0]
    focus_dmn_acceptor <- focus_dmn_dt[acceptor_strata, on = stratum_cols, nomatch = 0]
    
    # Merge in sample rates of 'donor_strata' and calculate probability that each BOX_ID is sampled
    # Probability is 1 - the combined probability that no strata within a box are sampled
    focus_sample_rate <- selection_rates[donor_strata, on = stratum_cols][, .(SAMPLE_RATE), keyby = c(year_col, stratum_cols)]
    focus_dmn_donor_prob <- focus_dmn_donor[
    ][, SAMPLE_RATE := focus_sample_rate[focus_dmn_donor, SAMPLE_RATE, on = c(year_col, stratum_cols)]
    ][, NO_SAMPLE_PROB :=  (1 - SAMPLE_RATE)^BOX_DMN_nbr
    ][, .(BOX_SAMPLE_PROB = 1 - prod(NO_SAMPLE_PROB)), by = c("ADP", "BOX_ID")]
    
    # Merge in box sample rates from donors to acceptors
    focus_dmn_acceptor[, BOX_SAMPLE_PROB := focus_dmn_donor_prob[focus_dmn_acceptor, BOX_SAMPLE_PROB, on = c(year_col, "BOX_ID")]]
    # Calculate expected number of component trips near a sampled trip
    focus_dmn_sum_pw <- focus_dmn_acceptor[, .(sum_DMN_pw = sum(BOX_SAMPLE_PROB * BOX_DMN_w)), by = c(year_col, stratum_cols, dmn_cols)]
    # Merge in total number of component trips in each domain
    focus_dmn_sum_pw <- box_res$dmn$strata_dmn_n_dt[focus_dmn_sum_pw , on = c(year_col, stratum_cols, dmn_cols)]
    # Calculate interspersion
    focus_dmn_sum_pw[, DMN_ISPN := sum_DMN_pw / STRATA_DMN_N]  
    
    dmn_ispn_lst[[i]] <- focus_dmn_sum_pw
    
    if(!is.null(box_res$dmn$geom_dmn_df)) dmn_ispn_geom_lst[[i]] <- focus_dmn_acceptor
    
  }
  names(dmn_ispn_lst) <- apply(dmn_groups, 1, paste0, collapse = ".")
  dmn_ispn_dt <- rbindlist(dmn_ispn_lst, idcol = "DMN_COLS")
  dmn_ispn_dt[, (dmn_cols) := tstrsplit(DMN_COLS, split = "[.]")][, DMN_COLS := NULL][]
  setorderv(dmn_ispn_dt, c(dmn_cols, year_col, stratum_cols))
  
  dmn_ispn_res <- list(dmn_ispn_dt = dmn_ispn_dt, params = box_res$params)
  if(!is.null(box_res$dmn$geom_dmn_df)) {
    names(dmn_ispn_geom_lst) <- apply(dmn_groups, 1, paste0, collapse =".")
    dmn_ispn_geom <- merge(hex_id_geom, rbindlist(dmn_ispn_geom_lst), by = "HEX_ID")
    dmn_ispn_res$dmn_ispn_geom <- dmn_ispn_geom
  }
  
  dmn_ispn_res
}

# New version with donor and acceptor table
calculate_dmn_interspersion3 <- function(box_def, selection_rates, stratum_dt, acceptor_donor_lst) {
  
  # box_def <- copy(box_res_fmp); selection_rates <- copy(rates_4.5M_fmp); 
  
  year_col <- box_def$params$year_col
  stratum_cols <- box_def$params$stratum_cols
  dmn_cols <- box_def$params$dmn_cols

  # Example acceptor_donor_lst to use when stratifying by FMP, only OB strata apply to other strata
  # TODO But how do I know to put these in this particular format?
  # Have to make sure that all stratum.names are separated by "." and that all are present in dataset?
  # Produce both stratum_dt and paired acceptor_donor_lst?
  
  # TODO Make a check to make sure that each row of stratum dt (with stratum cols collapsed with "." is the same
  # as the names of acceptor_donor_lst. Maybe the object can be fed to the function as a single list?)
  
  if(F) {
    stratum_dt <- unique(box_def$dmn$strata_dmn_n_dt[, ..stratum_cols])  #Get all stratum_cols in dataset
    setorderv(stratum_dt, colnames(stratum_dt))
    stratum_dt[, STRATUM_ID := .I]
    
    # For each stratum (row), enter the STRATUM_IDNO of the strata that can be data donors
    acceptor_donor_lst <- c(
      rep(list(6:9), times = 4),                # EM_HAL.BSAI, EM_HAL.GOA, EM_POT.BSAI and EM_POT.GOA
      list(5),                                  # EM_TRW.BSAI
      rep(list(6:9), times = 4),                # OB_HAL.BSAI, OB_HAL.GOA, OB_POT.BSAI and OB_POT.GOA
      rep(list(10:11), times = 2),              # OB_TRW.BSAI and  OB_TRW.GOA
      rep(list(6:9), times = 2)                 # ZERO.BSAI and ZERO.GOA
    )
    names(acceptor_donor_lst) <- apply(stratum_dt[, ..stratum_cols], 1, function(x) paste0(x, collapse = "."))
  }

  # If 'geom' is present, get HEX_ID geometries
  if(!is.null(box_def$dmn$geom_dmn_df)) hex_id_geom <- box_def$dmn$geom_dmn_df %>% select(HEX_ID, geometry) %>% unique()
  
  # Merge in the specified sample rates
  dmn_dat <- copy(box_def$dmn$box_dmn_smry_dt)
  dmn_dat[, STRATUM_ID := stratum_dt[dmn_dat, STRATUM_ID, on = stratum_cols]]  # Merge in STRATUM_ID
  dmn_dat[, SAMPLE_RATE := selection_rates[dmn_dat, SAMPLE_RATE, on = c(year_col, stratum_cols)]]
  # For each domain, calculate the probability that it will be sampled give the sample rate and number of neighbors
  dmn_dat[, BOX_SAMPLE_PROB :=  1 - (1 - SAMPLE_RATE)^BOX_DMN_nbr]

  out <- vector(mode = "list", length = nrow(stratum_dt))
  
  # For each acceptor stratum...
  for(i in 1:length(out)) {
    # i <- 1
    
    focus_stratum <- stratum_dt[i, ..stratum_cols]
    
    # Subset the data to include the acceptor and its donors, then split by domain
    focus_dmn_lst <- split(unique(rbind(
      dmn_dat[focus_stratum, on = stratum_cols],
      dmn_dat[stratum_dt[acceptor_donor_lst[[i]], ..stratum_cols], on = stratum_cols]
    )), by = dmn_cols)
    
    # For the acceptor's domains, combine the box sample probabilities by all donors
    out[[i]] <- rbindlist(lapply(focus_dmn_lst, function(x) {
      # x <- focus_dmn_lst[[1]]
      
      # Subset acceptor boxes, excluding any that have no trips (weight of 0)
      acceptor_dt <- x[focus_stratum, on = stratum_cols][BOX_DMN_w > 0]
      acceptor_dt[, c("SAMPLE_RATE", "BOX_SAMPLE_PROB", "STRATUM_ID") := NULL]
      
      donor_dt_cols <- c(year_col, "STRATUM_ID",  "BOX_ID", "BOX_SAMPLE_PROB")
      donor_dt <- x[stratum_dt[acceptor_donor_lst[[i]]], on = stratum_cols][, ..donor_dt_cols]

      # Merge in donor probabilities to acceptor by BOX_ID. 
      donor_sample_probs <- merge(acceptor_dt, donor_dt, by = c(year_col, "BOX_ID"), all.x = T)
      # Then, for each BOX, find the probability that at least one trip by any neighboring donor is sampled 
      # which is (1 - probability that no neighboring donor trips are sampled)
      donor_sample_probs[, .(BOX_DONOR_SAMPLE_PROB = 1 - prod(1 - BOX_SAMPLE_PROB)), by = c(year_col, stratum_cols, dmn_cols, "BOX_ID", "HEX_ID", "TIME", "BOX_DMN_n", "BOX_DMN_w")]
      
    }))

  }
  
  out <- rbindlist(out)
  out  # This is a raw output that has probabilities separated by year, stratum, and domain. In practice, we will
  # likely combine strata into pools (e.g., all HAL trips within EM_HAL and EM_POT or all HAL trips within OB_HAL and OB_POT)
  
  
  # TODO Make summaries that groups by POOL and dmn_cols, with and without splitting by BSAI_GOA
  
  # Group by pool! (Include this in stratum_dt, so that this group and dmn_cols are combined)
  
  # Identify Pool
  pool_dt <- copy(out)
  pool_dt[, POOL := fcase(STRATA %like% "EM", "EM", STRATA %like% "OB", "OB", STRATA %like% "ZERO", "ZERO")][]
  
  list(
    RAW = out,
    POOLED = pool_dt,
    params = box_def$params,
    geom = hex_id_geom
  )
  
}
  

dmn_interspersion_smry <- function(dmn_res_pool_dt) {
  
  year_col <- dmn_res_pool_dt$params$year_col
  
  # Overall within pool
  overall <- dmn_res_pool_dt$POOLED[
    , .(BOX_DMN_w = sum(BOX_DMN_w), BOX_DONOR_SAMPLE_PROB = weighted.mean(BOX_DONOR_SAMPLE_PROB, by = BOX_DMN_n)), 
    by = c(year_col, "POOL", "GEAR", "BOX_ID", "HEX_ID", "TIME")
  ][, .(BOX_DMN_w = sum(BOX_DMN_w), POOL_DMN_INTERSPERSION = weighted.mean(BOX_DONOR_SAMPLE_PROB, w = BOX_DMN_w)), keyby = c("GEAR", year_col, "POOL")]
  
  # Split by BSAI_GOA
  bsai_goa <- dmn_res_pool_dt$POOLED[
    , .(BOX_DMN_w = sum(BOX_DMN_w), BOX_DONOR_SAMPLE_PROB = weighted.mean(BOX_DONOR_SAMPLE_PROB, by = BOX_DMN_n)), 
    by = c(year_col, "POOL", "BSAI_GOA", "GEAR", "BOX_ID", "HEX_ID", "TIME")
    ][, .(BOX_DMN_w = sum(BOX_DMN_w), POOL_DMN_INTERSPERSION = weighted.mean(BOX_DONOR_SAMPLE_PROB, w = BOX_DMN_w)), keyby = c("GEAR", "BSAI_GOA", year_col, "POOL")]
  
  list(
    OVERALL = overall,
    BSAI_GOA = bsai_goa,
    params = dmn_res_pool_dt$params,
    geom = dmn_res_pool_dt$geom
  )
  
}

# Low-res AK map used in interspersion_plot. shp_land needs to be loaded first!
ak_low_res <- shp_land %>% st_simplify(dTolerance = 10000) %>% filter(!st_is_empty(shp_land)) %>% select(geometry)


dmn_interspersion_plot <- function(dmn_res_pool_dt, design_desc = NULL) {
  # TEST : dmn_res_pool_dt <- copy(dmn_insp_fmp); design_desc <- NULL
  # TEST : dmn_res_pool_dt <- copy(dmn_insp_og); design_desc <- NULL
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
  
  # For each Gear...
  gears <- unique(pool_4k$GEAR)
  gear_lst <- vector(mode = "list", length = length(gears))
  
  for(i in seq_along(gears)) {
    # i  <- 1
    
    gear_sub <- pool_4k %>% filter(GEAR == gears[i])
    years <- unique(pool_4k$ADP)
    year_lst <- vector(mode = "list", length = length(years))
      
    # For each Year...
    for(j in seq_along(years)) {
      # j <- 1
      
      year_sub <- gear_sub %>% filter(ADP == years[j])
      ak_map_cropped <- ak_low_res %>% st_set_crs(st_crs(year_sub))
      subtitle = paste0(design_desc, "Year: ", years[j], "; Gear: ", gears[i])
      
      year_lst[[j]] <- ggplot() + 
        facet_grid(TIME_4 ~ GEAR + POOL) + 
        # facet_grid(POOL ~ TIME_4 ) + 
        geom_sf(data = ak_map_cropped %>% st_crop(st_bbox(year_sub))) + 
        geom_sf(data = year_sub, aes(fill = BOX_DONOR_SAMPLE_PROB)) + 
        scale_fill_viridis_c() + 
        theme(
          legend.position = "bottom", axis.text = element_blank(), axis.ticks = element_blank(),
          panel.spacing = unit(0.05, "lines"), strip.text.x = element_text(margin = margin(b = 0.05, t = 0.05))
        ) + 
        labs(fill = "Sample probability") #, subtitle = subtitle)
    }
    
    names(year_lst) <- years
    gear_lst[[i]] <- year_lst
  }
  names(gear_lst) <- gears
  
  # for combine fixed gear plot ?
  if(F) {
    
    gear_sub <- pool_4k %>% filter(GEAR %in% c("HAL", "POT"))
    years <- unique(pool_4k$ADP)
    fg_year_lst <- vector(mode = "list", length = length(years))
    
    for(j in seq_along(years)) {
      
      year_sub <- gear_sub %>% filter(ADP == years[j])
      ak_map_cropped <- ak_low_res %>% st_transform(crs = st_crs(year_sub))
      subtitle = paste0(design_desc, "Year: ", years[j], "; Gear: HAL and POT" )
      
      fg_year_lst[[j]] <- ggplot() + 
        facet_grid(TIME_4 ~ GEAR + POOL) + 
        # facet_grid(POOL ~ TIME_4 ) + 
        geom_sf(data = ak_map_cropped %>% st_crop(st_bbox(year_sub))) + 
        geom_sf(data = year_sub, aes(fill = BOX_DONOR_SAMPLE_PROB)) + 
        scale_fill_viridis_c() + 
        theme(legend.position = "bottom", axis.text = element_blank(), axis.ticks = element_blank()) + 
        labs(fill = "Sample probability") # , subtitle = subtitle)
 
    }
    names(fg_year_lst) <- years
  }
  
  list(
    DATA = pool_4k,
    PLOTS =  gear_lst
  )

}


# TODO make the filter operate on stratum_cols and dmn_cols? But ignore FMP...
dmn_interspersion_plot2 <- function(dmn_ispn_res, strata, gear, year, type, map = F) {
  # strata <- "EM_HAL"; gear <- "HAL"; year <- 2022; type <- "S"; map <- T
  # strata <- "ZERO"; gear <- "HAL"; year <- 2022; type <- "S"; map <- T
  
  dat <- dmn_ispn_res$dmn_ispn_geom %>% filter(STRATA %in% strata & GEAR == gear & ADP == year & BOX_DMN_n > 0)
  
  base_hex_id <- dat %>% select(HEX_ID, geometry) %>% unique()
  bbox <- base_hex_id %>% st_bbox()
  if(map) {
    ak_map <- suppressWarnings(ak_low_res %>% st_crop(xmin = bbox[[1]], ymin = bbox[[2]], xmax = bbox[[3]], ymax = bbox[[4]]))
  } else ak_map <- NULL
  
  plot_format <- list(
    geom_sf(data = ak_map, fill = "gray50"),
    geom_sf(data = base_hex_id, color = "gray", fill = "gray", alpha = 0.2),
    facet_wrap(~TIME), theme(legend.position = "bottom", axis.title = element_blank(), legend.key.width = unit(2, "cm")), 
    scale_fill_viridis_c(direction = -1, limits = c(0,1), breaks = seq(0, 1, 0.2)),
    labs(subtitle = paste0("Year ", year, " : [", paste0(strata, collapse = " + "), "] strata fishing ", gear, " gear"))
    )
  
  if(type == "ST") {
    # Space and Time
    
    ggplot() + plot_format + 
      geom_sf(data = dat, aes(fill = BOX_SAMPLE_PROB), alpha = 0.8, color = "black") + 
      geom_sf_text(data = dat, aes(label = round(BOX_DMN_w,2)), size = 2, color = "white")
    
  } else if (type == "S") {
    # Space only
    
    dat_smry <-  dat %>% mutate(TOT_w = sum(BOX_DMN_w)) %>% group_by(HEX_ID)  %>%
      summarize(
        PROP_DMN_w_p = sum(BOX_DMN_w * BOX_SAMPLE_PROB) / sum(BOX_DMN_w), TIME = "ALL",  # Proportion of trips sampled in HEX_ID across time
        PROP_DMN_w = sum(BOX_DMN_w) / unique(TOT_w))   # Proportion of trips in HEX_ID
    ggplot() + plot_format + 
      geom_sf(data = dat_smry, aes(fill = PROP_DMN_w_p), alpha = 0.8, color = "black") + 
      geom_sf_text(data = dat_smry, aes(label = paste0(round(PROP_DMN_w_p,2), "\n", round(PROP_DMN_w,4)*100, "%")), size = 3, color = "white") + 
      labs(fill = "Proportion of trips near a neighbor")
    
  }
}