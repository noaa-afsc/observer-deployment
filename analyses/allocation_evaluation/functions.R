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
  # data <- copy(val_mixed); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; dmn_cols <- c("GEAR"); stratum_cols <- c("STRATA", "BSAI_GOA"); geom = T;
  
  #==================================#
  # NOTE! Remove any jig-gear trips! #
  #==================================# 
  # Jig-gear trips are generally in zero coverage (though sometimes HAL or EM_HAL trips might also fish
  # with jig gear in a trip?). Therefore, from an allocation perspective, it's not important if the rate will be 0.
  # Additionally, from an evaluation perspective, we don't use observed trips to make estimates for jig trips, so
  # we can remove them without issue. MAKE SURE 'data' HAS NO JIG GEAR COMPONENTS!
  # TODO Do I want to do this here? Current our domains/evaluation doesn't allow OB_HAL to cross with ZE_JIG, and 
  # can't if we remove all JIG trips at the very start.
  
  # Check for jig-gear in the dataset
  check_jig_cols <- unique(c(stratum_cols, dmn_cols))
  check_jig_cols <- apply(data[, ..check_jig_cols], 2, function(x) any(x == "JIG"))
  # Remove jig records
  if(any(check_jig_cols)) {
    jig_cols <- names(which(check_jig_cols))
    for(i in jig_cols) {
      jig_trips <- data[which(data[[jig_cols]] == "JIG"), ]
      cat(paste0("Removing ", nrow(jig_trips), " rows from ", uniqueN(jig_trips$TRIP_ID), " JIG trips from ", i, " column."), "\n")
      data <- setdiff(data, jig_trips)
    }
  }
  
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
  setkeyv(data, group_cols)
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
  strata_N_dt <- data[, .(STRATA_N = uniqueN(TRIP_ID)), keyby = group_cols]
  
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
  
  #============================================================================================#
  # Define groupings and count neighbors based on dmn_cols (e.g., FMP and GEAR) for evaluation #
  #============================================================================================#
  
  if(!is.null(dmn_cols)) {
    
    # Get trip weights, splitting by dmn_cols as well
    trip_id_dmn_mat <- as.matrix(data_dmn[, .N, by = .(TRIP_ID)])  # For each TRIP_ID, count number of instances (split by group_cols and dmn_cols and BOX_ID)
    trip_id_dmn_vec <- vector(mode = "integer")
    trip_id_dmn_vec[trip_id_dmn_mat[, 1]] <- trip_id_dmn_mat[, 2]  # Have each index be TRIP_ID and each value be the trip's domain weight
    
    # Identify all BOX_IDs within a dmn_tbl group
    dmn_tbl <- setorderv(unique(data_dmn[, ..dmn_cols]), dmn_cols)
    dmn_lst <- vector(mode = "list", length = nrow(dmn_tbl))
    for(i in 1:nrow(dmn_tbl)) {
      # i <- 1
      
      # Subset the data by domain
      focus_dmn_dat <- unique(subset(data_dmn[dmn_tbl[i,], on = dmn_cols], select = c(stratum_cols, "BOX_ID", "TRIP_ID")))
      if(F) focus_dmn_dat[, sum(1/trip_id_dmn_vec[TRIP_ID])]  # 19670.29 trips in all strata with HAL, 5120.713 in POT,  7221 in TRW
      # Split the data by stratum
      focus_dmn_dat_stratum <- lapply(split(focus_dmn_dat, by = stratum_cols, keep.by = F), as.matrix)
      if(F) sapply(focus_dmn_dat_stratum, function(x) sum(1 / trip_id_dmn_vec[x[, "TRIP_ID"]]))
      # Get all BOX_IDs in the domain (exclude boxes without any trips)
      focus_dmn_box_ids <- unique(focus_dmn_dat$BOX_ID)

      # For each stratum...
      focus_dmn_nbr_lst <- lapply(focus_dmn_dat_stratum, function(x) {
        # x <- focus_dmn_dat_stratum[[8]]
        
        # for each BOX_ID
        x1 <- lapply(focus_dmn_box_ids, function(y) {
          # y <- focus_dmn_box_ids[[1]]
          
          trip_id_centered <- unique(x[x[,1] == y, 2])
          
          c(
            BOX_ID = y,
            BOX_DMN_n = length(trip_id_centered),
            BOX_DMN_w = sum(1/trip_id_dmn_vec[trip_id_centered]),
            BOX_DMN_nbr = length(unique(x[x[,1] %in% nbr_lst[[y]], 2]))   # Compile all trips in neighborhood, count number of unique TRIP_IDs in neighborhood
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

    # Double-check that all weights in strata_N_dt are also in strata_dmn_N_dt. 
    if(!fsetequal(
      strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)][STRATA_N != 0],
      strata_N_dt
    )) stop("STRATA_N and sum(BOX_DMN_w) are not equal!")
    
    if (F) {
      
      # As long as I remove all jig gear trips, total trip counts match up. 
      strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)][, sum(STRATA_N)]
      strata_N_dt[, sum(STRATA_N)] 
      data[, uniqueN(TRIP_ID)]      
      
      fsetdiff(
        strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)],
        strata_N_dt
      )
      fsetdiff(
        strata_N_dt,
        strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)]
      )
      
      a <- strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)]
      a[ADP == 2022 & STRATA == "OB_MIXED"]           # 12 and 218
      strata_N_dt[ADP == 2022 & STRATA == "OB_MIXED"] # 11 and 218: Does a different stratum make up the 1 trip difference?
      
      table(strata_dmn_N_dt$STRATA_DMN_N)  # Are domains all intergers? seems strange to me...
      
      
      
      
      strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = .(year_col, stratum_cols)]
      
      
      
      
      # 2022 BSAI ZERO are 190 and 191, BSAI-OB_MIXED are 11 and 12
      strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)][ADP == 2021 & STRATA == "ZERO"]
      # These are quite far off
      strata_N_dt[ADP == 2021 & STRATA == "ZERO"]
      dmn_cols  # GEAR is used here. 
      
    }
    
    
    
    box_res$dmn <- list()
    box_res$dmn$strata_dmn_n_dt <- strata_dmn_N_dt
    box_res$dmn$box_dmn_smry_dt <- dmn_nbr_dt
    box_res$dmn$strata_dt <- setorderv(unique(strata_N_dt[, ..stratum_cols]), cols = stratum_cols)[, STRATUM_ID := .I][]
    
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

# TODO a test version of define boxes that splits box definitions by gear type (set a ps_cols)
# That is, within a stratum such as fgcombined, we count the number of overlaps in a gear-specific fashion
define_boxes_gs <- function(data, space, time, year_col, stratum_cols, dmn_cols = NULL, stata_area_sf = stat_area_sf, geom = F, ps_cols = NULL) {
  # data <-  copy(val_mixed); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; dmn_cols <- "GEAR"; stratum_cols <- c("STRATA"); geom = T; ps_cols <- "GEAR"
  
  # Testing with ps_cols = NULL
  # data <-  copy(val_mixed); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; dmn_cols <- "GEAR"; stratum_cols <- c("STRATA"); geom = T; ps_cols <- NULL
  
  #==================================#
  # NOTE! Remove any jig-gear trips! #
  #==================================# 
  # Jig-gear trips are generally in zero coverage (though sometimes HAL or EM_HAL trips might also fish
  # with jig gear in a trip?). Therefore, from an allocation perspective, it's not important if the rate will be 0.
  # Additionally, from an evaluation perspective, we don't use observed trips to make estimates for jig trips, so
  # we can remove them without issue. MAKE SURE 'data' HAS NO JIG GEAR COMPONENTS!
  
  # Check for jig-gear in the dataset
  check_jig_cols <- unique(c(stratum_cols, dmn_cols, ps_cols))
  check_jig_cols <- apply(data[, ..check_jig_cols], 2, function(x) any(x == "JIG"))
  # Remove jig records
  if(any(check_jig_cols)) {
    jig_cols <- names(which(check_jig_cols))
    for(i in jig_cols) {
      jig_trips <- data[which(data[[jig_cols]] == "JIG"), ]
      cat(paste0("Removing ", nrow(jig_trips), " rows from ", uniqueN(jig_trips$TRIP_ID), " JIG trips from ", i, " column."), "\n")
      data <- fsetdiff(data, jig_trips)
    }
  }

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
  keep_cols <- unique(c(year_col, stratum_cols, dmn_cols, "ADFG_STAT_AREA_CODE", time[3], time[4], "TRIP_ID", ps_cols))
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
  setcolorder(data, neworder = unique(c(year_col, stratum_cols, ps_cols, dmn_cols, "HEX_ID", time[3], time[4])))
  setkeyv(data, cols = unique(c(year_col, stratum_cols, ps_cols, dmn_cols, "HEX_ID", time[3], time[4])))
  
  if(nrow(data[is.na(HEX_ID)])) {
    print(data[is.na(HEX_ID)])
    stop("Could not assign a HEX_ID!")
  }
  
  #======================#
  # Define temporal unit #
  #======================#
  
  # First, get all years and a table converting date to week

  if(time[1] != "week") stop("So far this function only works with 'week()' function!")
  
  time_cols <- time[3:4]
  
  date_range <- range(unlist(data[, ..time_cols]))     # Get range of dates, converting to numeric
  date_vec <- as.Date(date_range[1] : date_range[2])   # Get date class of all dates within range
  week_vec <- sapply(date_vec, get(time[[1]]))         # Identify the week of each date
  dates_mtx <- cbind(date_vec, week_vec)               # Combine date vector and week vector

  dates_start <- min(dates_mtx[, 1]) - 1  # Get first date and subtract 1. T
  dates_mtx[, 1] <- dates_mtx[, 1] - dates_start  # This makes it so matrix can be reference by row index, much faster
  # TODO Check behavior of neighboring at ADP year thresholds
  
  # Grab time columns and define groups based on TRIP_ID and HEX_ID
  time_int <- data[, ..time_cols]
  time_int[, (time_cols) := lapply(.SD, as.integer), .SDcols = time_cols]
  setnames(time_int, new = c("S", "E"))
  data_int <- data[, .(TRIP_ID, HEX_ID)][, GRP := .GRP, by = .(TRIP_ID, HEX_ID)][]
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
  setcolorder(data, unique(c(year_col, stratum_cols, ps_cols, "TIME", "HEX_ID")))
  # 'BOX_ID' is defined by HEX_ID and TIME and if specified, ps_cols, and is common to all data. This will be used to determine neighbors.
  # 'stratum_cols' is not included so that we can later compare overlap between strata
  data[, BOX_ID := .GRP, by = c(year_col, "TIME", "HEX_ID")]              
  if(!is.null(ps_cols)) {
    data[, PS_ID := .GRP, keyby = ps_cols]
    ps_cols_tbl <- unique(subset(data, select = c(ps_cols, "PS_ID")))
  } else {
    ps_cols_tbl <- data.table(PS = "NA", PS_ID = 1L)
    data[, PS_ID := 1L] 
  }
  setkey(data, BOX_ID)

  #==================#
  # Define Neighbors #
  #==================#
  
  # For each BOX_ID, find which BOX_IDs are neighboring based on week and spatial cells.
  sub_cols <- c(year_col, "TIME", "HEX_ID", "BOX_ID")
  st_mtx <- as.matrix(unique(data[, ..sub_cols]))
  nbr_lst <- apply(st_mtx, MARGIN = 1, function(x) {
    # x <- st_mtx[1,]
    box_mtx <- st_mtx[st_mtx[,1] == x[1], , drop = F]             # subset by year
    nbr_time <- (x[2] + (-win:win))                               # identify neighboring TIME
    box_mtx <- box_mtx[box_mtx[,2] %in% nbr_time, , drop = F]     # subset by time range
    nbr_hex_id <- stat_area_dist_lst[[x[3]]]                      # identify neighboring HEX_IDs
    box_mtx[box_mtx[, 3] %in% nbr_hex_id, 4]                      # subset by neighboring HEX_IDs, then grab BOX_ID
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
  setkeyv(data, cols = c(group_cols, "PS_ID", "BOX_ID"))
  data_lst <- lapply(
    X = split(x = subset(data, select = c(group_cols, "BOX_ID", "TRIP_ID", "PS_ID")), by = group_cols, keep.by = F),
    FUN = as.matrix)

  # Make the frequency table of each TRIP_ID (so that trips are properly split by HEX_ID, TIME, and if present, ps_cols
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
    function(stratum_mtx) {
      # as [1:BOX_ID] [2:TRIP_ID] and if specified, [3:PS_ID]
      # stratum_mtx <- data_lst[[23]]   # 2022.OB_FIXED
      # stratum_mtx <- data_lst[[24]]   # 2022.OB_TRW
      
      # First, split by PS_ID
      ps_lst <- lapply(split(stratum_mtx[, 1:2], stratum_mtx[,3]), matrix, ncol = 2)
      
      # Now for each PS_ID group, identify neighboring trips
      lapply(ps_lst, function(x) {
        # x <- ps_lst[[1]]    # Here PS_ID = 1 is POT
        
        # Get all unique time and space boxes
        x1 <- unique(x[, 1])
        
        # Now for each BOX_ID listed in 'x1', count the number unique TRIP_IDs in neighboring BOX_IDs
        x2 <- do.call(rbind, lapply(x1, function(y) {
          # y <- x1[1]   # Box 5749
          
          trip_id_centered <- x[x[,1] == y, 2]    # Identify number of trips actually  the box
          # There shouldn't ever be the same trip counted twice in the same box.
          if( length(trip_id_centered) != length(unique(trip_id_centered)) ) stop("There is a duplicate trip_id!")
          
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
      })
    }
  ) 
  
  # Calculate STRATA_N and ensure the sum of weights is equivalent
  strata_N_dt <- data[, .(STRATA_N = uniqueN(TRIP_ID)), by = group_cols]

  # Double-check that weights sum to STRATA_N
  if(!(all(
    unname(sapply(box_smry, function(x) sum(sapply(x, function(y) sum(y[, "BOX_w"]))))) == strata_N_dt$STRATA_N
  ))) stop("STRATA_N and sum(BOX_w) are not equal!")
  
  # Get post-strata weights (number of component trips in post-strata)
  ps_W_dt <- data[, .(ps_W = sum(1 / trip_id_vec[TRIP_ID])), by = c(group_cols, "PS_ID")]
  ps_W_dt <- ps_W_dt[ps_cols_tbl, on = .(PS_ID)]  # Merge in ps_cols on PS_ID
  setkeyv(ps_W_dt, c(year_col, stratum_cols, "PS_ID"))
  
  # Create data.table output
  st_dt <- unique(data[, .(BOX_ID, HEX_ID, TIME)])   # Table of BOX_ID, HEX_ID, and TIME
  dt_out <- 
    rbindlist(lapply(box_smry, function(x) rbindlist(lapply(x, as.data.table), idcol = "PS_ID")), idcol = "GROUP_COLS")[
    ][, (group_cols) := tstrsplit(GROUP_COLS, split = "[.]")
    ][, GROUP_COLS := NULL][
    ][st_dt, on = .(BOX_ID)]
  dt_out <- dt_out[
  ][, which(colnames(dt_out) == year_col) := as.integer(dt_out$ADP)
  ][, PS_ID := as.integer(PS_ID)
  ][ps_cols_tbl, on = .(PS_ID)]
  setcolorder(dt_out, c(group_cols, "PS_ID", "BOX_ID", "BOX_n", "BOX_w", "BOX_nbr", ps_cols, "HEX_ID", "TIME"))
  
  # Initialize outputs
  box_res <- list(
    box_smry = box_smry,
    strata_n_dt = strata_N_dt,
    ps_W_dt = ps_W_dt,
    dt_out = dt_out,
    og_data = data,
    nbr_lst = nbr_lst,
    params = list(stratum_cols = stratum_cols, year_col = year_col, dmn_cols = dmn_cols, ps_cols = ps_cols)
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
      strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)][STRATA_N != 0],
      strata_N_dt
    )) stop("STRATA_N and sum(BOX_DMN_w) are not equal!")
    
    box_res$dmn <- list()
    box_res$dmn$strata_dmn_n_dt <- strata_dmn_N_dt
    box_res$dmn$box_dmn_smry_dt <- dmn_nbr_dt
    box_res$dmn$strata_dt <- setorderv(unique(strata_N_dt[, ..stratum_cols]), cols = stratum_cols)[, STRATUM_ID := .I][]
    
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


# THe most recently implemented version. Basically the same as define_boxes() (not the second version), but with a fix
# that does allows domains to identify neighbors based on BOX_ID for ALL trips, not just those in the same BSAI/GOA domain or stratum.
define_boxes_3 <- function(data, space, time, year_col, stratum_cols, dmn_cols = NULL, stata_area_sf = stat_area_sf, geom = F){
  
  #==================================#
  # NOTE! Remove any jig-gear trips! #
  #==================================# 
  # Jig-gear trips are generally in zero coverage (though sometimes HAL or EM_HAL trips might also fish
  # with jig gear in a trip?). Therefore, from an allocation perspective, it's not important if the rate will be 0.
  # Additionally, from an evaluation perspective, we don't use observed trips to make estimates for jig trips, so
  # we can remove them without issue. MAKE SURE 'data' HAS NO JIG GEAR COMPONENTS!
  # TODO Do I want to do this here? Current our domains/evaluation doesn't allow OB_HAL to cross with ZE_JIG, and 
  # can't if we remove all JIG trips at the very start.
  
  # Check for jig-gear in the dataset
  check_jig_cols <- unique(c(stratum_cols, dmn_cols))
  check_jig_cols <- apply(data[, ..check_jig_cols], 2, function(x) any(x == "JIG"))
  # Remove jig records
  if(any(check_jig_cols)) {
    jig_cols <- names(which(check_jig_cols))
    for(i in jig_cols) {
      jig_trips <- data[which(data[[jig_cols]] == "JIG"), ]
      cat(paste0("Removing ", nrow(jig_trips), " rows from ", uniqueN(jig_trips$TRIP_ID), " JIG trips from ", i, " column."), "\n")
      data <- fsetdiff(data, jig_trips)
    }
  }
  
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
  setkeyv(data, group_cols)
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
  strata_N_dt <- data[, .(STRATA_N = uniqueN(TRIP_ID)), keyby = group_cols]
  
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

  
  if(!is.null(dmn_cols)) {
    
    # Get trip weights, splitting by dmn_cols as well
    trip_id_dmn_mat <- as.matrix(data_dmn[, .N, by = .(TRIP_ID)])  # For each TRIP_ID, count number of instances (split by group_cols and dmn_cols and BOX_ID)
    trip_id_dmn_vec <- vector(mode = "integer")
    trip_id_dmn_vec[trip_id_dmn_mat[, 1]] <- trip_id_dmn_mat[, 2]  # Have each index be TRIP_ID and each value be the trip's domain weight
    
    # Identify all BOX_IDs within a dmn_tbl group
    
    # in both time and space. Essentially, any spatial or temporal post-strata should be excluded from this list.
    
    dmn_tbl <- setorderv(unique(data_dmn[, ..dmn_cols]), dmn_cols)
    dmn_lst <- vector(mode = "list", length = nrow(dmn_tbl))
    for(i in 1:nrow(dmn_tbl)) {
      # i <- 1  # BSAI x HAL
      # i <- 4  #  GOA x HAL
      
      # Subset the data by domain
      focus_dmn_dat <- unique(subset(data_dmn[dmn_tbl[i,], on = dmn_cols], select = c(stratum_cols, "BOX_ID", "TRIP_ID")))                 # this only has trips within the domain
      if(F) focus_dmn_dat[, sum(1/trip_id_dmn_vec[TRIP_ID])]  # 19670.29 trips in all strata with HAL, 5120.713 in POT,  7221 in TRW
      # Split the data by stratum
      focus_dmn_dat_stratum <- lapply(split(focus_dmn_dat, by = stratum_cols, keep.by = F), as.matrix)
      if(F) sapply(focus_dmn_dat_stratum, function(x) sum(1 / trip_id_dmn_vec[x[, "TRIP_ID"]]))
      
      # Make a list of non-spatial domain columns. Although we might define "BSAI_GOA" as a domain, BOX_ID is used to identify all neighbors.
      # This is so that within a domain that may be defined by space (BSAI_GOA), we can still look outside that domain for spatial neighbors 
      non_spatial_dmn_cols <- setdiff(dmn_cols, c("BSAI_GOA", "BS_AI_GOA"))
      non_spatial_dmn_dat <- unique(subset(data_dmn[dmn_tbl[i, ..non_spatial_dmn_cols], on = non_spatial_dmn_cols], select = c(stratum_cols, "BOX_ID", "TRIP_ID")))
      # Also split it by stratum
      non_spatial_dmn_dat_stratum <- lapply(split(non_spatial_dmn_dat, by = stratum_cols, keep.by = F), as.matrix)
      non_spatial_dmn_dat_stratum <- non_spatial_dmn_dat_stratum[names(focus_dmn_dat_stratum)]   # Make sure the strata names are in the same order
      
      # Get all BOX_IDs in the domain (exclude boxes without any trips)
      focus_dmn_box_ids <- unique(focus_dmn_dat$BOX_ID)
      
      if(!all(names(focus_dmn_dat_stratum) == names(non_spatial_dmn_dat_stratum))) stop("Names of focus_dmn_dat_stratum aren't the same as non_spatial_dmn_dat_stratum!")
      
      # For each stratum...
      focus_dmn_nbr_lst <- mapply(
        function(x1, x2) {
          # x1 <- focus_dmn_dat_stratum[["OB_HAL"]]
          # x2 <- non_spatial_dmn_dat_stratum[["OB_HAL"]]
          
          # x1 <- focus_dmn_dat_stratum[["EM_HAL"]]
          # x2 <- non_spatial_dmn_dat_stratum[["EM_HAL"]]
          
          # for each BOX_ID
          x1.1 <- lapply(focus_dmn_box_ids, function(y) {
            # y <- focus_dmn_box_ids[[1]]
            
            trip_id_centered <- unique(x1[x1[,1] == y, 2])
            
            c(
              BOX_ID = y,
              BOX_DMN_n = length(trip_id_centered),
              BOX_DMN_w = sum(1/trip_id_dmn_vec[trip_id_centered]),
              BOX_DMN_nbr = length(unique(x2[x2[,1] %in% nbr_lst[[y]], 2]))   # Compile all trips in neighborhood, count number of unique TRIP_IDs in neighborhood
            )
            
          })
          
          as.data.table(do.call(rbind, x1.1))
        }, 
        x1 = focus_dmn_dat_stratum, 
        x2 = non_spatial_dmn_dat_stratum,
        SIMPLIFY = F
      )
      
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
    
    # Double-check that all weights in strata_N_dt are also in strata_dmn_N_dt. 
    if(!fsetequal(
      strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)][STRATA_N != 0],
      strata_N_dt
    )) stop("STRATA_N and sum(BOX_DMN_w) are not equal!")
    
    if (F) {
      
      # As long as I remove all jig gear trips, total trip counts match up. 
      strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)][, sum(STRATA_N)]
      strata_N_dt[, sum(STRATA_N)] 
      data[, uniqueN(TRIP_ID)]      
      
      fsetdiff(
        strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)],
        strata_N_dt
      )
      fsetdiff(
        strata_N_dt,
        strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)]
      )
      
      a <- strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)]
      a[ADP == 2022 & STRATA == "OB_MIXED"]           # 12 and 218
      strata_N_dt[ADP == 2022 & STRATA == "OB_MIXED"] # 11 and 218: Does a different stratum make up the 1 trip difference?
      
      table(strata_dmn_N_dt$STRATA_DMN_N)  # Are domains all intergers? seems strange to me...
      
      
      
      
      strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = .(year_col, stratum_cols)]
      
      
      
      
      # 2022 BSAI ZERO are 190 and 191, BSAI-OB_MIXED are 11 and 12
      strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)][ADP == 2021 & STRATA == "ZERO"]
      # These are quite far off
      strata_N_dt[ADP == 2021 & STRATA == "ZERO"]
      dmn_cols  # GEAR is used here. 
      
    }
    
    
    
    box_res$dmn <- list()
    box_res$dmn$strata_dmn_n_dt <- strata_dmn_N_dt
    box_res$dmn$box_dmn_smry_dt <- dmn_nbr_dt
    box_res$dmn$strata_dt <- setorderv(unique(strata_N_dt[, ..stratum_cols]), cols = stratum_cols)[, STRATUM_ID := .I][]
    
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

## Update Strata -------------------------------------------------------------------------------------------------------

# This function prepares the inputs for the allo_equal() and allo_min_plus_opt() functions
update_strata <- function(effort, tm, stratum_cols = c(), focus_years) {
  # effort <- copy(pc_effort_dt); tm <- copy(trips_melt); stratum_cols <- c("STRATA", "BSAI_GOA"); focus_years <- 2018:2022
  
  # Update STRATA
  sub_cols <- c("ADP", "TRIP_ID", "DAYS", stratum_cols)
  new_effort <- unique(effort[, ..sub_cols])
  new_strata <- apply(new_effort[, ..stratum_cols], 1, paste0, collapse = "-")
  new_effort[, STRATA := new_strata]
  if(length(setdiff(stratum_cols, "STRATA")) != 0)  new_effort[, setdiff(stratum_cols, "STRATA") := NULL]    
  
  # Update STRATA in trips_melt
  new_trips_melt <- copy(tm)
  new_trips_melt[, STRATA := unique(
    new_effort[, .(TRIP_ID, STRATA)]
  )[new_trips_melt, STRATA, on = .(TRIP_ID)]]
  
  # Summarize the data for use by equal_rates() and min_plus_opt() functions
  new_effort_focus <- new_effort[ADP %in% focus_years][
  ][, .(STRATA_N = uniqueN(TRIP_ID), TRP_DUR = mean(DAYS, na.rm = T)), by = .(ADP, STRATA)
  ][, PRIOR_MTD := unique(
    new_trips_melt[, .(ADP, STRATA, TRIP_ID, DAYS)])[
    ][ADP %in% (.BY[[1]]-(1:3)) & STRATA == .BY[[2]], mean(DAYS, na.rm = T)], 
    by = .(ADP, STRATA)][]
  setkey(new_effort_focus, ADP, STRATA)
  
  list(
    effort = new_effort_focus,
    tm = new_trips_melt
  )
  
}

## Equal Allocation ----------------------------------------------------------------------------------------------------

# This function calculates the rates afforded with equal allocation. It is also used by allo_min_plus_opt if the budget
# cannot afford 'optimized' days.
allo_equal <- function(x, budget){
  # x <- allo_dt[STRATA != "ZERO"];  budget <- 4.5e6
  
  if(length(budget) != 1 & length(budget) != length(unique(x$ADP)) ) stop(
    "'budget' must be length = 1 or length(unique(x$ADP))!"
  )
  
  out_dt <- copy(x)
  if( !("BUDGET" %in% colnames(x)) ) {
    if(length(budget) == length(unique(x$ADP))) {
      budget_tbl <- data.table(ADP = as.integer(names(budget)), BUDGET = budget)
      out_dt[, BUDGET := budget_tbl[out_dt, BUDGET, on = .(ADP)]]
    } else out_dt[, BUDGET := budget]
  }
  
  out_dt[
  ][, MON_RATE := unique(BUDGET) / sum(STRATA_N * TRP_DUR * CPD) , by = .(ADP)
  ][, MON_N := MON_RATE * STRATA_N
  ][, MON_D := MON_RATE * STRATA_N * TRP_DUR
  ][, OPT_N := NA
  ][, BUDGET := NULL]

  setkey(out_dt, ADP, STRATA)
  
  if( !("MIN_RATE") %in% colnames(x) ) {
    return(out_dt[, .(ADP, STRATA, STRATA_N, TRP_DUR, PRIOR_MTD, CPD, MIN_RATE = NA_real_, MIN_N = NA_real_, MIN_D = NA_real_, TOT_MIN_D = NA_real_, MON_RATE, MON_N, MON_D, OPT_N)])
  }
  
  out_dt[, .(ADP, STRATA, STRATA_N, TRP_DUR, PRIOR_MTD, CPD, MIN_RATE, MIN_N, MIN_D, TOT_MIN_D, MON_RATE, MON_N, MON_D, OPT_N)]
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
# TODO add term to specify which optimization metrics to use.
allo_min_plus_opt <- function(allo_lst, em_carve_off, conf, MIN_RATE = 0.15, budget, metrics = c("discard", "chnk_psc", "hlbt_psc")){
  # allo_lst <- copy(allo_lst); em_carve_off <- F; conf <- 0.95; MIN_RATE <- 0.15; budget <- 4.5e6; metrics <- c("discard", "chnk_psc", "hlbt_psc")
  
  # conf <- 0.95; MIN_RATE <- 0.15; budget <- 4.5e6
  
  # 'em_carve_off' must be TRUE or FALSE
  # 'budget' must be length = 1
  
  # em_carve_off <- T
  # em_carve_off <- F
  
  
  
  # TODO MIGHT NOT NEED THIS IF I HAVE SQ TERM
  # if(length(budget) != 1 & length(budget) != length(unique(x$ADP)) ) stop(
  #   "'budget' must be length = 1 or length(unique(x$ADP))!"
  # )
  
  # TODO MIGHT NOT NEED THIS IF I HAVE SQ TERM
  # out_dt <- copy(x)
  # if( !("BUDGET" %in% colnames(x)) ) {
  #   if(length(budget) == length(unique(x$ADP))) {
  #     budget_tbl <- data.table(ADP = as.integer(names(budget)), BUDGET = budget)
  #     out_dt[, BUDGET := budget_tbl[out_dt, BUDGET, on = .(ADP)]]
  #   } else out_dt[, BUDGET := budget]
  # }
  
  out_dt <- copy(allo_lst$effort)
  
  # If em_carve_off = T, then set fixed-gear EM strata to 30% and TRW_EM to 1/3
  if(em_carve_off) {
    
    out_dt_em <- out_dt[STRATA %like% "EM"]
    out_dt_em[
    ][, MON_RATE := ifelse(STRATA %like% "EM_TRW", 1/3, 0.3)
    ][, MON_N := STRATA_N * MON_RATE
    ][, MON_D := MON_N * TRP_DUR] 
    # Calculate estimated total cost of EM strata by ADP
    em_cost_by_adp <- out_dt_em[, .(EM_COST = sum(STRATA_N * MON_RATE * TRP_DUR * CPD)), by = .(ADP)]
    #em_cost_by_adp <- setNames(em_cost_by_adp$EM_COST, em_cost_by_adp$ADP)
    
    # Allocate the remaining funds to observer strata
    out_dt <- out_dt[STRATA %like% "OB"][
    ][em_cost_by_adp, on = .(ADP)
    ][, BUDGET := budget - EM_COST
    ][, EM_COST := NULL][]

  } else out_dt[, BUDGET := budget]

  # Omit ZERO strata
  out_dt <- out_dt[!(STRATA %like% "ZERO")]
  
  # Prepare allocation weights from trips_melt (variance of metrics for each stratum and ADP year)
  weights <- allo_lst$tm[
    STRATA %in% unique(out_dt$STRATA) & Metric %in% metrics, 
    .(S2_h = var(Value), N_h = .N), keyby = .(ADP, Metric, STRATA)]
  
  # For each ADP year...
  adp_years <- unique(out_dt$ADP)
  adp_list <- lapply(adp_years, function(adp) {
    
    # Determine trips/days to achieve the 15% hurdle and the conf_rate according to the confidence level specified
    x1 <- out_dt[ADP == adp
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
    
    # If the minimum rate is afforded at the specified confidence level, say so:
    if ( unique(x1$BUDGET) > x1[, sum(CONF_D * CPD)]) {
      cat(
        paste0(adp, " : Hurdle afforded at ", conf, " confidence level. Allocating optimized samples.\n"))
    } 
      
    # If the minimum rate can be met (i.e. at conf=0.5) but not at the specified confidence, start by allocating by the
    # proportion of CONF_RATE above MIN_RATE
    if( (unique(x1$BUDGET) < x1[, sum(CONF_D * CPD)]) & (unique(x1$BUDGET) >  x1[, sum(MIN_D * CPD)]) ) {
    
      opt_days_afforded <- F
      
      # TODO Does using the proportion of days above MIN_D only work if CPD is the same for all strata?
      # Using the proportion of funds allocated to each stratum above MIN_RATE to achieve CONF_RATE, calculate CONF_NEW 
      # as the 
      
      x1[, MIN_COST := (MIN_D * CPD)]    # Cost of affording minimum rate
      x1[, CONF_COST := (CONF_D * CPD)]  # cost of affording confidence rate
      x1[, CONF_COST_PROP := (CONF_COST - MIN_COST) / sum(CONF_COST - MIN_COST)] # Proportion of funds above min_rate allocated to achieving conf
      x1[, LEFT_OVER := BUDGET - sum(MIN_COST)]   # Calculate total funds left over after affording minimum rate
      x1[, CONF_RATE_NEW := MIN_RATE + (LEFT_OVER * CONF_COST_PROP / CPD / TRP_DUR / STRATA_N)]

      # Get the confidence level achieved for all strata, and grab the highest one from which to work down until we 
      # fall under AFF_D. Using the highest confidence level means we'll start close to but still over budget, e.g., 
      # a conservative ballpark estimate that will save considerable time in the upcoming loop.
      conf_new <- round(max(x1[, .(CONF_NEW = pbinom(
        q = ceiling(MIN_RATE * STRATA_N), size = STRATA_N, prob = CONF_RATE_NEW, lower.tail = F)), 
        by = .(STRATA)]$CONF_NEW), 4)
      if(conf_new < 0.5) conf_new <- 0.5           # If the estimate for conf_new is below 0.5, set it to 0.5.
      GO <- T
      
      while( GO ){
        x1[, CONF_RATE_NEW := find_conf_rate(STRATA_N, MIN_RATE, conf_new), by = STRATA
        ][, CONF_N := CONF_RATE_NEW * STRATA_N
        ][, CONF_D := CONF_N * TRP_DUR]
        if(  sum(x1[, CONF_D * CPD]) > unique(x1$BUDGET) ){
          conf_new <- conf_new - 0.0001                            # If too many days purchased, reduce confidence level
        } else {
          GO <- F                                                  # Once we fall under AFF_D, stop
        }
      }
      x1[, ':=' (CONF = conf_new, CONF_RATE = CONF_RATE_NEW)        # Update and clean up
      ][, TOT_CONF_D := sum(CONF_D)
      ][, c("MIN_COST", "CONF_COST", "CONF_COST_PROP", "LEFT_OVER") := NULL]
      
      warning(
        paste0(adp, " : Minimum rate of ", MIN_RATE, " afforded but only at ", round(conf_new, 4), " confidence level."), 
        call. = F, immediate. = T
      )
      
      
    }

    # Calculate number of optimized days afforded
    x1[, C_N := CPD * PRIOR_MTD                               # Average Cost of observing one trip within stratum
    ][, MIN_C := sum(CPD * TRP_DUR * CONF_N), by = .(ADP)     # total cost of affording hurdle
    ][, OPT_BUD := BUDGET - MIN_C                             # budget remaining for optimization after affording hurdle
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
    ][, METRIC := paste(metrics, collapse = "+")]                          # coercing the metric column to blended, which happens next 
    
    # Save the weights from the individual metrics
    metrics <- x1[, .(ADP, STRATA, STRATA_N, CONF_RATE, Metric, S_h, OPT_STRATA_N, C_N, N_h_S_h_div_sqrtC, sumN_h_S_h_div_sqrtC, W_hopt, TTL_OPT_N)]

    x1 <- x1[, .(
      ADP, STRATA, STRATA_N, TRP_DUR, PRIOR_MTD, CPD, BUDGET, MIN_RATE, CONF, CONF_RATE, CPD, TOT_MIN_D, TOT_CONF_D,
      OPT_BUD, MIN_N, MIN_D, CONF_N, CONF_D, METRIC, W_hopt)]
    x1 <- x1[, lapply(.SD, mean), .SDcols = c("W_hopt"), by = .(
      ADP, STRATA, STRATA_N, TRP_DUR, PRIOR_MTD, BUDGET, MIN_RATE, CONF, CONF_RATE, CPD, TOT_MIN_D, TOT_CONF_D, 
      OPT_BUD, MIN_N, MIN_D, CONF_N, CONF_D,  METRIC)]
    setkey(x1, ADP, STRATA)

    # Now apply the blended weightings 
    x1[, RAW_OPT_N := OPT_BUD / CPD * W_hopt / TRP_DUR]               # Initial estimate of optimized trips afforded 
    if(opt_days_afforded == F) x1[, OPT_N := 0] else x1[, OPT_N := RAW_OPT_N]
    x1[, OPT_D := OPT_N * TRP_DUR                                     # Estimate optimized days afforded
    ][, MON_D := CONF_D + OPT_D                                        # Total observed days
    ][, MON_N := CONF_N + OPT_N                                        # Total observed trips
    ][, MON_RATE := MON_N / STRATA_N]                                   # Monitoring rate
    
    # If the minimum rate was not afforded, revert to equal rates. 
    if( unique(x1$BUDGET) < x1[, sum(MIN_D * CPD)] ){
      warning(
        paste0(adp, " : Minimum rate of ", MIN_RATE, " not afforded. Resorting to 'Equal Rates' allocation."), 
        call. = F, immediate. = T)
      
      conf_attempt <- copy(x1)[, .(
        STRATA, METRIC, W_hopt, CONF = NA_real_, CONF_RATE = NA_real_,
        CONF_N = NA_real_, CONF_D = NA_real_, TOT_CONF_D = NA_real_
      )]
      x1 <- allo_equal(x1, unique(x1$BUDGET))[conf_attempt, on = .(STRATA)] 
    }
    
    list(
      out = x1[, .(
        ADP, STRATA, STRATA_N, TRP_DUR, PRIOR_MTD, CPD, MIN_RATE, CONF, CONF_RATE, METRIC, W_hopt, MIN_N, MIN_D, 
        TOT_MIN_D, CONF_N, CONF_D, TOT_CONF_D, MON_RATE, MON_N, MON_D, OPT_N)],
      metrics = metrics
    )

  })
  
  # Prepare outputs
  metrics_dt <- rbindlist(lapply(adp_list, "[[", "metrics") )
  out_dt <- setkey(rbindlist(lapply(adp_list, "[[", "out")), ADP, STRATA)
  if(em_carve_off) out_dt <- rbind(out_dt, out_dt_em, fill = T)
  setkey(out_dt, ADP, STRATA)
  setattr(out_dt, "metrics", metrics_dt)
  
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

# A gear-specific version (for when ps_cols = "GEAR" using define_boxes_gear)
calculate_interspersion_gs <- function(box_res, sample_rate_vec, omit_strata = c(NULL)) {
  # omit_strata <- NULL
  # omit_strata <- "ZERO"
  # box_res <- box_mixed_gs; omit_strata = "ZERO"
  
  group_cols <- c(box_res$params$year_col, box_res$params$stratum_cols)
  ps_cols <- box_res$params$ps_cols
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
      # x <- 0.15

      # For each stratum...
      sapply(
        box_res$box_smry[keep_strata],
        function(y) {
          # y <- box_res$box_smry[keep_strata][[19]]  # 2022.OB_FIXED
          # y <- box_res$box_smry[keep_strata][[20]]  # 2022.OB_TRW
          # lengths(box_res$box_smry[keep_strata])  # Note that some strata have more than one post-stratum group!
          # y[[1]]
          # y[[2]]
          
          # For each post-stratum's BOX_ID, use BOX_nbr to calculate the probability that the box is sampled,
          # and then multiply that by BOX_w to get the expected number of sampled trips in the box. Sum across
          # all boxes to get expected number of sampled trips in stratum.
          # x is the sample rate, y[,4] is 'BOX_nbr' and y[, 3] is 'BOX_w'. Referencing by column is faster.
          # sapply(y, function(z) sum((1 - ((1 - x)^z[,4])) * z[,3]))
          # z <- y[[1]]
          
          #  sapply(y, function(z) sum(z[, 3]))  # total PS_ID weight
          #  sapply(y, function(z) sum((1 - ((1 - x)^z[,4])) * z[,3]) /  sum(z[, 3]))  # PS_ID-specific interspersion

          #  mean(sapply(y, function(z) sum((1 - ((1 - x)^z[,4])) * z[,3]) /  sum(z[, 3])) )  # Unweighted (treat PS_ID equally, average without weighting)
          # Interspersion weighted by the size of the post-strata
          # weighted.mean(
          #   x = sapply(y, function(z) sum((1 - ((1 - x)^z[,4])) * z[,3]) /  sum(z[, 3])), 
          #   w = sapply(y, function(z) sum(z[, 3]))  # total PS_ID weight)
          # ) 
          
          a <- do.call(rbind, y)
          sum((1 - ((1 - x)^a[,4])) * a[,3])   # Just doing the sum across is the same as the weighted average!
          # FIXME I don't need to split post-strata into additional lists unless I want to weight post-strata separately!
          # I think that within a stratum, we want to weight by each trip's interspersion.
          # We don't want to weight interspersion of HAL and POT post-strata equally regardless of how many trips are in each.
          
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
  # ispn_res <- copy(box_mixed_bsai_goa_gs_insp); trip_cost_dt <- copy(trip_cost_dt_fg_combined_bsai_goa_cwb)
  
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
      # y1 <- index_vectors[[5]]; y2 <- split(x1, by = year_col)[[5]] # 2022
      
      
      rbindlist(lapply(y1, function(z) {
        # z <- y1[950]
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

# A quick function that searches for rates from the proximity allocation method given a budget.
prox_rates_from_budget <- function(index_res, budget) {
  # index_res <- copy(box_fixed_bsai_goa_gs_index); budget <- 4.5e6
  
  group_cols <- unname(unlist(index_res$params[c("year_col", "stratum_cols")]))
  budget_dt <- data.table(INDEX_COST = budget)
  index_res$rates[, .SD[budget_dt, on = .(INDEX_COST), roll = "nearest"], keyby = group_cols]
}

prox_rates_smry <- function(rates_res) {
  # rates_res <- copy(prox_rates_from_budget(box_fixed_bsai_goa_gs_index, 4.5e6))
  
  rates_res[ADP == 2022]
  
}


## Cost-Weighted Boxes -------------------------------------------------------------------------------------------------

# Calculate Ph, the expected proportion of unsampled boxes in each stratum, for the cost-weighted boxes design
calculate_cwb_Ph <- function(box_res, sample_rate_vec, omit_strata = c(NULL)) {
  # omit_strata <- NULL
  # omit_strata <- "ZERO"
  # x <- 0.15; y <- box_res$box_smry[[1]]
  # box_res <- copy(box_mixed)
  
  
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


# Gear-specific version:
# TODO Should be able to work around having a second function here - using do.call(rbind, x)
# before calculating Ph works for length=1 lists as well.
calculate_cwb_Ph_gs <- function(box_res, sample_rate_vec, omit_strata = c(NULL)) {
  # omit_strata <- NULL
  # omit_strata <- "ZERO"
  # x <- 0.15; y <- box_res$box_smry[[1]]
  # box_res <- copy(box_fixed_gs)
  
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
          
          z <- do.call(rbind, y)           # Flatten the list of post-strata
          sum(((1 - x)^z[,4])) / nrow(z)   # Calculate the expected proportion of boxes not near a sampled trip.
          

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
  #][, n := (target_budget * sum(`NP/c_h`)) / sum(STRATA_N * Ph * sqrt(CPT)), by = .(ADP)   # calculate afforded total sample size (equation 5.24)
  ][, n := target_budget / sum(`nh/n` * CPT), by = .(ADP)   # calculate afforded total sample size (equation 5.24, but simplified)
  ][, nh := `nh/n` * n       # Calculate the optimal sample size of each stratum n_h using n_h/n * n
  ][, fh := nh / STRATA_N][]   # using n_h / N_h, calculate allocated sample rate (sample fraction)
  setkey(x, ADP, STRATA)
  x
}

# A version of allo_cwb without sqrt weighting of costs. Shuould be able to make this function take a sqrt_cost term
# that calcualtes NP/c_h with the 'c' term squared or not!
allo_cwb2 <- function(x, target_budget) {
  x[, `NP/c_h` := (STRATA_N * Ph) / CPT       # Calculate NP/c for each stratum (numerator of equation 5.23)
  ][, `nh/n` := `NP/c_h` / sum(`NP/c_h`), by = .(ADP)        # # Calculate the optimal sample size (equation 5.23)
  ][, n := target_budget / sum(`nh/n` * CPT), by = .(ADP)    # calculate afforded total sample size (equation 5.24 simplified)
  ][, nh := `nh/n` * n       # Calculate the optimal sample size of each stratum n_h using n_h/n * n
  ][, fh := nh / STRATA_N][]   # using n_h / N_h, calculate allocated sample rate (sample fraction)
  setkey(x, ADP, STRATA)
  x
}

# Adjusts results of allo_cwb to get the assumed sample rate (SAMPLE_RATE) for Ph to approach the allocated rate fh
allo_cwb_half_diff <- function(x, cwb_prop, target_budget, sqrt_cost = T) {
  
  Ph_dt <- copy(cwb_prop$Ph_dt)
  year_strata <- unlist(cwb_prop$params[c("year_col", "stratum_cols")], use.names = F)
  
  x <- copy(x)
  x[, SAMPLE_RATE := round(SAMPLE_RATE + (round(fh,3) - SAMPLE_RATE)/2, 3)]  # Adjust the sample rate to half the difference with N_h/N
  x[, Ph := Ph_dt[x, Ph, on = c(year_strata, "SAMPLE_RATE")]][]     #update Ph values using new sample rates
  
  if(sqrt_cost) allo_cwb(x, target_budget) else allo_cwb2(x, target_budget)
  x
}

# This loop repeatedly allocates according to CWB until the assumed rates approximate the allocated rates
allo_cwb_loop <- function(cwb_prop, trip_costs, budget, sqrt_cost = T) {
  # cwb_prop <- copy(cwb_prop); trip_costs <- copy(trip_cost_dt)
  # cwb_prop <- copy(cwb_prop_bsai_goa); trip_costs <- copy(trip_cost_dt_bsai_goa_cwb)
  # cwb_prop <- copy(cwb_prop_fg_combined); trip_costs <- copy(trip_cost_dt_fgcombined)
  # budget <- 4.5e6
  
  year_strata <- unlist(cwb_prop$params[c("year_col", "stratum_cols")], use.names = F)
  
  # Start with 15% across the board
  init_rates <- unique(copy(cwb_prop$Ph_dt))[SAMPLE_RATE == 0.150][!(STRATA %like% "ZERO")]
  init_rates[, CPT := trip_costs[init_rates, CPT, on = c(year_strata)]]     
  
  # Make the initial allocation assuming 15%
  if(sqrt_cost) old_step <- allo_cwb(init_rates, target_budget = budget) else {
    old_step <- allo_cwb2(init_rates, target_budget = budget)
  }
  old_diff <- Inf
  new_diff <- old_step[, abs(sum(fh - SAMPLE_RATE))]
  old_var <- Inf
  new_var <- old_step[, var(fh - SAMPLE_RATE)]
  GO <- T
  i <- 0
  steps <- matrix(nrow = 20, ncol=2)
  colnames(steps) <- c("abs_diff", "var")
  
  # Keep halving differences between assumed SAMPLE_RATE and allocate rate 'fh'
  while(GO) {
    i <- i + 1
    new_step <- allo_cwb_half_diff(old_step, cwb_prop, budget, sqrt_cost = sqrt_cost)
    new_diff <- new_step[, abs(sum(fh - SAMPLE_RATE))]
    new_var <- new_step[, var(fh - SAMPLE_RATE)]
    
    # Once there are no improvements, stop the loop
    
    steps[i,] <- c(new_diff, new_var)
    
    if(new_var < old_var){
        
      old_step <- copy(new_step)
      old_diff <- new_diff
      old_var <- new_var
    } else GO <- F
  }
  
  list(
    rates = old_step,
    steps = steps[!is.na(steps[,1]), , drop = F]
  )
  
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
  setkeyv(dmn_ispn_dt, c(year_col, stratum_cols, dmn_cols))
  
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
calculate_dmn_interspersion3 <- function(box_def, selection_rates, acceptor_donor_lst) {
  
  # box_def <- copy(box_res_fmp); selection_rates <- copy(rates_4.5M_fmp); 
  
  year_col <- box_def$params$year_col
  stratum_cols <- box_def$params$stratum_cols
  dmn_cols <- box_def$params$dmn_cols

  stratum_dt <- box_def$dmn$strata_dt
  
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
  
# A newer version that allows for NULL entries in acceptor_donor_lst. 
calculate_dmn_interspersion4 <- function(box_def, selection_rates, acceptor_donor_lst) {
  
  # TODO Manually add the fill for the pools!
  # box_def <- copy(box_sq); selection_rates <- copy(eval_rates$CURRENT.PROX); 
  
  if(F) {
    
    box_def$dmn$strata_dt      # Get STRATUM_ID
    # With OB as only donors (including TRW_EM to itself as it is observer-based)
    acceptor_donor_lst <-  c(
      rep(list(4:5), times = 2),                # 1-2: OB to FG_EM
      list(3),                                  # 3:   EM_TRW donates to itself (still an observer!)
      rep(list(4:5), times = 2),                # 4-5: OB_HAL and OB_POT
      list(6),                                  # 6:   OB: OB_TRW
      list(4:5)                                 # 7:   OB to ZERO
    )
    
    # With non-OB Donors
    acceptor_donor_lst <-  c(
      rep(list(1:2), times = 2),                # 1-2: FG_EM to itself
      rep(list(NULL), times = 5)                # No other comparisons
    )
    
  }
  
  # Testing with FMP
  if(F) {
    acceptor_donor_lst <- copy(ob_adl)
  }
  
  
  year_col <- box_def$params$year_col
  stratum_cols <- box_def$params$stratum_cols
  dmn_cols <- box_def$params$dmn_cols
  
  stratum_dt <- box_def$dmn$strata_dt
  
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

    if( nrow(stratum_dt[ acceptor_donor_lst[[i]] ]) == 0 ) {
      # If there are no donors, skip
      out[[i]] <- NULL
    } else {
      
      # Identify the 'donor' stratum
      focus_stratum <- stratum_dt[i, ..stratum_cols]
      
      # Subset the data to include the acceptor and its donors, then split by domain
      focus_dmn_lst <- split(unique(rbind(
        dmn_dat[focus_stratum, on = stratum_cols],
        dmn_dat[stratum_dt[acceptor_donor_lst[[i]], ..stratum_cols], on = stratum_cols]
      )), by = dmn_cols)
      
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

    # For the acceptor's domains, combine the box sample probabilities by all donors
 
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


# This version should not be used - it was meant to to omit spatial 'dmn_cols' to allow identification of neighbors
# based on BOX_ID (e.g., the spatial component HEX_ID). The implemented solution was done upstream when boxes are 
# defined. Don't use it! Left her e just in case
if(F) {  calculate_dmn_interspersion5 <- function(box_def, selection_rates, acceptor_donor_lst) {
  
  # TODO Manually add the fill for the pools!
  # box_def <- copy(box_sq); selection_rates <- copy(eval_rates$CURRENT.PROX); 
  
  if(F) {
    
    box_def$dmn$strata_dt      # Get STRATUM_ID
    # With OB as only donors (including TRW_EM to itself as it is observer-based)
    acceptor_donor_lst <-  c(
      rep(list(4:5), times = 2),                # 1-2: OB to FG_EM
      list(3),                                  # 3:   EM_TRW donates to itself (still an observer!)
      rep(list(4:5), times = 2),                # 4-5: OB_HAL and OB_POT
      list(6),                                  # 6:   OB: OB_TRW
      list(4:5)                                 # 7:   OB to ZERO
    )
    
    # With non-OB Donors
    acceptor_donor_lst <-  c(
      rep(list(1:2), times = 2),                # 1-2: FG_EM to itself
      rep(list(NULL), times = 5)                # No other comparisons
    )
    
  }
  
  # Testing with FMP
  if(F) {
    acceptor_donor_lst <- copy(ob_adl)
  }
  
  
  year_col <- box_def$params$year_col
  stratum_cols <- box_def$params$stratum_cols
  dmn_cols <- box_def$params$dmn_cols
  
  stratum_dt <- box_def$dmn$strata_dt
  
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
    
    if( nrow(stratum_dt[ acceptor_donor_lst[[i]] ]) == 0 ) {
      # If there are no donors, skip
      out[[i]] <- NULL
    } else {
      
      # Identify the 'donor' stratum
      focus_stratum <- stratum_dt[i, ..stratum_cols]
      
      # Subset the data to include the acceptor and its donors, then split by domain
      # focus_dmn_lst <- split(unique(rbind(
      #   dmn_dat[focus_stratum, on = stratum_cols],
      #   dmn_dat[stratum_dt[acceptor_donor_lst[[i]], ..stratum_cols], on = stratum_cols]
      # )), by = dmn_cols)
      
      focus_dmn_lst <- split(dmn_dat[focus_stratum, on = stratum_cols], by = dmn_cols)
      donor_all_dt <- dmn_dat[stratum_dt[acceptor_donor_lst[[i]], ..stratum_cols], on = stratum_cols] 
      
      out[[i]] <- rbindlist(lapply(focus_dmn_lst, function(x) {
        # x <- focus_dmn_lst[[1]]
        
        # # Subset acceptor boxes, excluding any that have no trips (weight of 0)
        # acceptor_dt <- x[focus_stratum, on = stratum_cols][BOX_DMN_w > 0]
        # acceptor_dt[, c("SAMPLE_RATE", "BOX_SAMPLE_PROB", "STRATUM_ID") := NULL]
        # 
        # donor_dt_cols <- c(year_col, "STRATUM_ID",  "BOX_ID", "BOX_SAMPLE_PROB")
        # donor_dt <- x[stratum_dt[acceptor_donor_lst[[i]]], on = stratum_cols][, ..donor_dt_cols]
        
        # Subset acceptor boxes, excluding any that have no trips (weight of 0)
        acceptor_dt <- x[focus_stratum, on = stratum_cols][BOX_DMN_w > 0]
        acceptor_dt[, c("SAMPLE_RATE", "BOX_SAMPLE_PROB", "STRATUM_ID", "BOX_DMN_nbr") := NULL]
        
        # BSAI_GOA as a dmn_col is only useful for the acceptor groups. As donors, we use BOX_ID to identify donor data in the correct spatiotemporal neighborhood
        # If BSAI_GOA is present, don't use it to subset the domains! If Gear is present, use it!
        non_spatial_dmn_cols <- setdiff(dmn_cols, c("BSAI_GOA", "BS_AI_GOA"))
        dmn_sub_dt <- unique(acceptor_dt[, ..non_spatial_dmn_cols])
        
        # Subset donor data
        donor_dt_cols <- c(year_col, "STRATUM_ID",  "BOX_ID", "BOX_DMN_nbr", "SAMPLE_RATE", "BOX_SAMPLE_PROB")
        donor_dt <- donor_all_dt[dmn_sub_dt, on = non_spatial_dmn_cols][stratum_dt[acceptor_donor_lst[[i]]], on = stratum_cols][, ..donor_dt_cols]
         
        # Merge in donor probabilities to acceptor by BOX_ID. 
        donor_sample_probs <- merge(acceptor_dt, donor_dt, by = c(year_col, "BOX_ID"), all.x = T)
        # Then, for each BOX, find the probability that at least one trip by any neighboring donor is sampled 
        # which is (1 - probability that no neighboring donor trips are sampled)
        donor_sample_probs[, .(BOX_DONOR_SAMPLE_PROB = 1 - prod(1 - BOX_SAMPLE_PROB)), by = c(year_col, stratum_cols, dmn_cols, "BOX_ID", "HEX_ID", "TIME", "BOX_DMN_n", "BOX_DMN_w")]
        
      }))
      
    }
    
    # For the acceptor's domains, combine the box sample probabilities by all donors
    
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
  
}  }


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

# This is a wrapper for the domain interspersion figs to generate summary figures
dmn_interspersion_figs <- function(box_def, selection_rates, ob_adl, nonob_adl) {
  # ob_adl is the acceptor_donor_lst where OB is the donor to all pools (also TRW-EM to itself)
  # nonob_adl is the acceptor_donor_lst where fixed-gear EM applies to itself

  # TEST for current
  if(F) {
    box_def <- copy(box_sq); selection_rates <- eval_rates$EQUAL.EQUAL; 
    
    ob_adl <- c(
      rep(list(4:5), times = 2),                # 1-2: EM_HAL and EM_POT 
      list(3),                                  # 3: EM_TRW
      rep(list(4:5), times = 2),                # 4-5: OB Fixed Gear
      list(6),                                  # 6: OB Trawl                         
      list(4:5)                                 # 7: ZERO           
    )
    
    nonob_adl <- c(
      rep(list(1:2), times = 2),                # 1-2: Fixed-gear EM to itself
      rep(list(NULL), times = 5)                # 4-7: No other donors
    )
  }
  
  # TEST for FMP
  if(F) {
    box_def <- copy(box_fmp); selection_rates <- eval_rates$FMP.PROX_FMP; 
    
    ob_adl <- c(
      rep(list(6:9), times = 4),                # 1-4:   EM_HAL and EM_POT 
      list(5),                                  # 5:     EM_TRW
      rep(list(6:9), times = 4),                # 6-9:   OB Fixed Gear
      rep(list(10:11), times = 2),              # 10-11: OB Trawl                         
      rep(list(6:9), times = 2)                 # 12-13: ZERO           
    )
    
    nonob_adl <- c(
      rep(list(1:4), times = 4),               # 1-4: Fixed-gear EM to itself
      rep(list(NULL), times = 9)               # 5-13: No other donors
    )
  }
  
  dmn_insp_plot_theme <- list(
    scale_x_continuous(limits = c(0,1), breaks = seq(0, 1, 0.2)),
    scale_y_discrete(limits = rev),
    theme(
      legend.position = "none",
      strip.text.x = element_text(margin = margin(b = 0.1, t = 0.1))),
    labs(x = "Domain Interspersion", y = "Gear Type")
  )
  
  dmn_insp_OB <- calculate_dmn_interspersion4(
    box_def = box_def,
    selection_rates = selection_rates,
    acceptor_donor_lst = ob_adl
  )
  
  dmn_insp_nonOB <- calculate_dmn_interspersion4(
    box_def = box_def,
    selection_rates = selection_rates,
    acceptor_donor_lst = nonob_adl
  )
  
  dmn_smry_OB  <- dmn_interspersion_smry(dmn_insp_OB)
  dmn_smry_nonOB  <- dmn_interspersion_smry(dmn_insp_nonOB)

  dmn_smry_OB$OVERALL[, FILL := fcase(
    POOL == "EM" & GEAR %in% c("HAL", "POT"), "dodgerblue",
    POOL == "EM" & GEAR == "TRW", "dodgerblue4",
    POOL == "OB", "chartreuse3",
    POOL == "ZERO", "darkorchid4"
  )]
  
  dmn_smry_OB$BSAI_GOA[, FILL := fcase(
    POOL == "EM" & GEAR %in% c("HAL", "POT"), "dodgerblue",
    POOL == "EM" & GEAR == "TRW", "dodgerblue4",
    POOL == "OB", "chartreuse3",
    POOL == "ZERO", "darkorchid4"
  )]
  
  dmn_plot_overall <- ggplot(dmn_smry_OB$OVERALL, aes(y = GEAR, x = POOL_DMN_INTERSPERSION)) + 
    facet_grid(ADP ~ POOL) + geom_col(aes(fill = I(FILL))) + dmn_insp_plot_theme +
    geom_point(data = dmn_smry_nonOB$OVERALL, shape = 23, fill = "yellow", stroke = 1) + 
    geom_text(aes(label = round(BOX_DMN_w)), x = 0.15, hjust = 1, size = 3, color = "white")
  
  dmn_plot_fmp <- ggplot(dmn_smry_OB$BSAI_GOA, aes(y = GEAR, x = POOL_DMN_INTERSPERSION)) +
    facet_grid(ADP ~ POOL + BSAI_GOA) + geom_col(aes(fill = I(FILL))) + dmn_insp_plot_theme + 
    geom_point(data = dmn_smry_nonOB$BSAI_GOA, shape = 23, fill = "yellow", stroke = 1) + 
    geom_text(aes(label = round(BOX_DMN_w)), x = 0.3, hjust = 1, size = 3, color = "white")
  
  # Final outputs
  list(
    DMN_INSP_OB = dmn_insp_OB,
    DMN_INSP_NONOB = dmn_insp_nonOB,
    DMN_INSP_OB_SMRY = dmn_smry_OB,
    DMN_INSP_NONOB_SMRY = dmn_smry_nonOB,
    DMN_PLOT_OVERALL = dmn_plot_overall,
    DMN_PLOT_FMP = dmn_plot_fmp
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


#====================================================#
# Functions used in 2024 Draft Preliminary Rates. ####
#====================================================#

# These functions incorporated big changes to how monitoring costs are estimated. They are modified version of the functions
# above, but I opted to not overwrite the old functions as doing so will break most other scripts. 

#===============================#
## Monitoring Cost Functions ####

ob_cost <- function(x, cost_params, sim = F) {
  # Requires ADP, STRATA, N, MON_RATE (if average is assumed), MEAN_TRIP_DAYS, n (if simulated)
  # x <- copy(x0); 
  
  day_rate_intercept <- cost_params$OB$day_rate_intercept
  day_rate_slope     <- cost_params$OB$day_rate_slope
  travel_day_rate    <- cost_params$OB$travel_day_rate
  
  # Subset OB pool trips
  x1 <- x[STRATA %like% "OB_"]
  
  # If simulating, use 'n' 
  if(sim) ob_days <- x1[, sum(d)] else {
    # Otherwise, calculate total expected number of observed days
    ob_days <- x1[, sum(STRATA_N * TRP_DUR * MON_RATE)]
  }
  
  # Calculate the day rate for observers
  ob_day_rate <- day_rate_intercept + (day_rate_slope * ob_days)
  
  # Calculate the total cost and cost per day
  ob_total <- (ob_day_rate * ob_days) + (travel_day_rate * ob_days)
  ob_cpd <- ob_total / ob_days
  
  data.table(OB_TOTAL = ob_total, OB_CPD = ob_cpd, OB_DAYS = ob_days)
  
}

emfg_cost <- function(x, cost_params, sim = F) {
  # Requires ADP, STRATA, N, MON_RATE, MEAN_TRIP_DAYS, # of EM fixed-gear vessels
  # x <- copy(x2[['2022']]); 
  
  emfg_v <- cost_params$EMFG$emfg_v
  cost_per_vessel <- cost_params$EMFG$cost_per_vessel
  cost_per_review_day <-  cost_params$EMFG$cost_per_review_day
  
  # subset to EM fixed-gear strata
  x1 <- x[STRATA %like% "EM_HAL|EM_POT|EM_FIXED"]
  if(nrow(x1) == 0) return(
    data.table(EMFG_TOTAL = 0, EMFG_CPD = NA_real_, EMFG_DAYS = 0, EMFG_BASE = 0)
  )
  
  # Total EM days to review
  # If simulating....
  if(sim) emfg_days <- x1[, sum(d)] else {
    # Otherwise, assume average from monitoring rate
    emfg_days <- x1[, sum(STRATA_N * TRP_DUR * MON_RATE)]
  }
  
  # Calculate base cost, total cost and cost per day
  emfg_base <- (cost_per_vessel * emfg_v)
  emfg_total <- emfg_base + (cost_per_review_day * emfg_days)
  emfg_cpd <- emfg_total / emfg_days
  
  data.table(EMFG_TOTAL = emfg_total, EMFG_CPD = emfg_cpd, EMFG_DAYS = emfg_days, EMFG_BASE = emfg_base)
  
}

emtrw_cost <- function(x, cost_params, sim = F) {
  # Requires ADP, STRATA, N, MON_RATE, MEAN_TRIP_DAYS, # EM Trawl  vessels fishing exclusively in the GOA
  # x <- copy(x0); 
  
  emtrw_goa_v                <- cost_params$EMTRW$emtrw_goa_v
  trip_to_plant_factor       <- cost_params$EMTRW$trip_to_plant_factor   # Used to predict plant days from trips
  amortized_equipment_per_VY <- cost_params$EMTRW$amortized_equipment_per_VY   # Per (Vessel x Year) amortized EM equipment install costs for GOA-only vessels
  equipment_upkeep_per_VY    <- cost_params$EMTRW$equipment_upkeep_per_VY      # Per (Vessel x Year) EM equipment maintenance cost for GOA-only vessels
  review_day_rate            <- cost_params$EMTRW$review_day_rate      # Per sea day cost for EM compliance review
  plant_day_rate             <- cost_params$EMTRW$plant_day_rate    # Per plant day cost for shoreside monitoring by observers
  
  # Subset EM_TRW
  x1 <- x[STRATA %like% "EM_TRW"]
  if(nrow(x1) == 0) return(
    data.table(
      EMTRW_TOTAL = 0, EMTRW_CPT = NA_real_, 
      EMTRW_PLANT_n = 0, EMTRW_PLANT_DAYS = 0, EMTRW_BASE = 0
    )
  )
  
  # Calculate total days, trips and estimate plant days
  emtrw_review_D <- x1[, sum(STRATA_N * TRP_DUR)]
  # If simulating, use n
  if(sim) emtrw_plant_n <- sum(x1$n) else {
    # Otherwise assume average from selection rate
    emtrw_plant_n <- x1[, sum(STRATA_N * MON_RATE)]
  }
  emtrw_plant_days <- emtrw_plant_n * trip_to_plant_factor
  
  # Calculate base cost of GOA-vessels
  emtrw_base <- emtrw_goa_v * (amortized_equipment_per_VY + equipment_upkeep_per_VY) 
  
  # Calculate total EM Trawl costs and calculate cost per trip monitored shoreside
  emtrw_total <- emtrw_base +   
    (review_day_rate * emtrw_review_D) +     # Cost of compliance review
    (plant_day_rate * emtrw_plant_days)
  emtrw_cpt <- emtrw_total / emtrw_plant_n   # Cost per trip reviewed shoreside 
  
  data.table(
    EMTRW_TOTAL = emtrw_total, EMTRW_CPT = emtrw_cpt, 
    EMTRW_PLANT_n = emtrw_plant_n, EMTRW_PLANT_DAYS = emtrw_plant_days, EMTRW_BASE = emtrw_base
  )
}

# versions use for simulations where 'n' is fed instead of sample rate


# For each monitoring method, calculate total given a monitoring rate, up to a maximum budget
rates_to_costs <- function(allo_lst_effort, rate_vec, cost_params, max_budget, cost_fun) {
  # allo_lst_effort <- copy(x0); cost_fun <- "emtrw_cost"
  # cost_fun <- "ob_cost"
  # cost_fun <- "emfg_cost"
  # cost_fun <- "emtrw_cost"
  
  # allo_lst_effort <- copy(current.allo_lst$effort); cost_fun <- "ob_cost"; rate_vec <- copy(sample_rate_vec)
  
  
  # initialize results list
  out_lst <- vector(mode = "list", length = length(rate_vec))
  
  use_cost_fun <- get(cost_fun)
  x0 <- copy(allo_lst_effort)
  
  # Loop through rate_vec, stopping when all years exceed the max budget
  for(i in seq_along(rate_vec)) {
    x1 <- copy(x0)
    x1[, MON_RATE := rate_vec[i]]
    x2 <- split(x1, by = "ADP")
    
    cost_res <- rbindlist(lapply(x2, use_cost_fun, cost_params), idcol = "ADP")
    total_col <- which(colnames(cost_res) %like% "_TOTAL")
    
    # If all year have totals above out max, stop the loop.
    # FIXME The ob_cost() model should be made logistic so CPD doesn't become low at extremely high volumns
    if(all(cost_res[[total_col]] > max_budget)) break() 
    else{
      out_lst[[i]] <- cbind(MON_RATE = rate_vec[i], cost_res)
    }
  }
  rbindlist(out_lst)
  
}

# These functions would work a lot faster with matrices rather than data.frames! A lot if simple calculations...
# TODO VECTORIZE THIS: Will go much faster once vectorized
# TODO Make it return an object like allo_lst that includes trip durations!
rates_to_costs_all <- function(allo_lst, rate_vec, cost_params, max_budget, cost_fun_vec = c("ob_cost", "emfg_cost", "emtrw_cost")) {
  # rate_vec <- seq(0.001, 0.99, by = 0.001)
  # rate_vec <- c(0.3, 0.3333)  # Conventient for status quo allocation with carve_off for EM methods
  
  # from status quo : rate_vec <- seq(afforded_rate, afforded_rate + 0.001, by = 0.0001)
  
  # allo_lst <- copy(current.allo_lst); rate_vev <- copy(sample_rate_vec); cost_fun_vec <- c("ob_cost", "emfg_cost", "emtrw_cost")
  
  
  # Exclude Zero coverage
  x0 <- allo_lst$effort[!(STRATA %like% "ZERO")]
  
  # Calculate costs of each monitoring method up to the max budget
  x1 <- lapply(cost_fun_vec, function(x) {
    rates_to_costs(x0, rate_vec, cost_params, max_budget, cost_fun = x)
  })
  
  # in cases where entire monitoring methods are excluded, trim empty list elements
  x1 <- x1[lengths(x1) > 0 ]
  
  # Combine across MON_RATE
  if(length(x1) > 1) {
    j <- 2
    x2 <- x1[[1]]
    while(j <= length(x1)) {
      x2 <- merge(x2, x1[[j]], by = c("MON_RATE", "ADP"), all = T)
      j <- j + 1
    }
  }
  
  # Total the total columns
  total_cols <- colnames(x2)[which(colnames(x2) %like% "_TOTAL")]
  rate_total <- rowSums(x2[, ..total_cols])
  x2[, RATE_TOTAL := rate_total][]
  x2[, ADP := as.integer(ADP)]
  
  setorder(x2, "ADP", "MON_RATE")
  x2  
  
}

#==========================#
## Allocation Functions ####

# Equal Rates
allo_equal2 <- function(cost_dt, budget, allo_lst, cost_params) {
  # cost_dt <- copy(current.cost_dt); budget <- 4.5e6
  # cost_dt <- copy(cost_dt_ob_equal)[ADP == 2022]; budget <- 2280389; allo_lst <- copy(year_sub)
  
  # budget <- 4.50e6; allo_list <- current.allo_lst 
  
  # for status quo:
  # allo_equal2()
  # cost_dt <- copy(cost_dt_ob_equal[ADP == 2022]); budget <- budget - out_dt_em_cost[ADP == 2022, EM_COST]; allo_lst <- year_sub
  
  
  
  cost_dt_year_lst <- split(cost_dt[!is.na(RATE_TOTAL)], by = "ADP")
  
  rbindlist(lapply(cost_dt_year_lst, function(x) {
    # x <- cost_dt_year_lst[["2022"]]
    
    adp_year <- unique(x$ADP)
    
    # First, find the rates with expenses just under and over our budget
    afforded_rate <- x[findInterval(budget, x$RATE_TOTAL), MON_RATE]
    
    # TODO Should be able to get rid of this chunk if we do 0.0001 resolution in the cost_dt
    # Now use the barely afforded rate and add 0.001 to redo the cost function at 0.0001 resolution
    refined_rate <- rates_to_costs_all(allo_lst, rate_vec = seq(afforded_rate, afforded_rate + 0.001, by = 0.0001), cost_params, max_budget = budget)[
    ][ADP == adp_year][!is.na(RATE_TOTAL)]
    refined_rate <- refined_rate[findInterval(budget, refined_rate$RATE_TOTAL)]  # Get the closest rate just below our budget
    
    # Prepare outputs for equal rates allocation for each year
    year_out <- allo_lst$effort[!(STRATA %like% "ZERO") & ADP == adp_year]
    
    year_out[
    ][, CPD := fcase(
      STRATA %like% "OB_", refined_rate$OB_CPD,
      STRATA %like% "EM_HAL|EM_POT|EM_FIXED", refined_rate$EMFG_CPD,
      STRATA %like% "EM_TRW|ZERO", NA_real_)
    ][, CPT := fcase(
      STRATA %like% "OB_|EM_HAL|EM_POT|EM_FIXED", TRP_DUR * CPD,
      STRATA %like% "EM_TRW", refined_rate$EMTRW_CPT,
      STRATA %like% "ZERO", NA_real_)
    ][STRATA %like% "EM_TRW", CPD := CPT / TRP_DUR    # 'cost_per_day' is kind of pointless for EM_TRW but we'll use it to calculate CPT again.
    ][, CPT := NULL
    ][, c("MIN_RATE", "MIN_N", "MIN_D", "TOT_MIN_D") := NA_real_   # Create additional columns for the sake of using it with min_plus_opt
    ][, MON_RATE := refined_rate$MON_RATE
    ][, MON_N := MON_RATE * STRATA_N
    ][, MON_D := MON_N * TRP_DUR
    ][, OPT_N := NA_real_]
    
  }))
  
}

# Status quo (carve-pff for fixed-gear EM (30%) and trawl EM (33%), equal rates to OB pool
allo_status_quo <- function(allo_lst, budget, cost_dt, cost_params) {
  # cost_dt <- copy(current.cost_dt); budget <- 4.5e6; max_budget <- 4.5e6
  
  # [[NOTE]]
  # This function assumes that the status quo allocation method 'min_plu_opt' will never achieve the 15% adjusted
  # hurdle minimum and afford optimized days! It assumes that the carve-off from EM leaves too few funds and reverts
  # to the equal-rates method to be applied to the observer strata.
  
  em_rates_and_cost <- rates_to_costs_all(allo_lst, rate_vec = c(0.3, 0.3333), cost_params, max_budget = budget)
  
  year_vec <- unique(em_rates_and_cost$ADP)
  
  # Split EM and OB methods
  out_dt <- copy(allo_lst$effort)
  out_dt_em <- out_dt[STRATA %like% "EM"]
  out_dt_ob <- out_dt[!(STRATA %like% "EM|ZERO")]
  
  out_em_lst <- lapply(year_vec, function(x) {
    
    # Pull out total costs
    year_cost_sub <- em_rates_and_cost[ADP == x]
    
    # Prepare the output tables, plugging in rates
    year_sub <- out_dt_em[ADP == x]
    year_sub[, CPD := fcase(
      STRATA %like% "EM_HAL|EM_POT|EM_FIXED", year_cost_sub[MON_RATE == 0.3, EMFG_CPD],
      STRATA %like% "EM_TRW", year_cost_sub[MON_RATE == 0.3333, EMTRW_CPT]/TRP_DUR )
    ][, MON_RATE := fcase(STRATA %like% "EM_HAL|EM_POT|EM_FIXED", 0.3, STRATA %like% "EM_TRW", 0.3333)
    ][, MON_N := STRATA_N * MON_RATE
    ][, MON_D := MON_N * TRP_DUR] 
    
    year_cost <- year_sub[, .(EM_COST = sum(STRATA_N * MON_RATE * TRP_DUR * CPD))]                                   
    
    list(
      YEAR_SUB = year_sub,
      YEAR_COST = c(ADP = x, year_cost)
    )
  })
  
  out_dt_em <- rbindlist(lapply(out_em_lst, "[[", "YEAR_SUB"))
  out_dt_em_cost <- rbindlist(lapply(out_em_lst, "[[", "YEAR_COST"))
  
  # Now allocate the rest to Observer strata, assuming equal rates
  
  # First, modify cost_dt to total only include OB_cost
  cost_dt_ob_equal <- copy(cost_dt)
  cost_dt_ob_equal[, RATE_TOTAL := OB_TOTAL]
  # Find what is afforded with equal rates each year
  out_dt_ob <- rbindlist(lapply(year_vec, function(x) {
    # Subset Observer data for the year
    year_sub <- list(effort = out_dt_ob[ADP == x])
    # Allocate according to equal rates, subtracting total EM costs from the budget 
    year_sub <- allo_equal2(cost_dt_ob_equal[ADP == x], budget = budget - out_dt_em_cost[ADP == x, EM_COST], allo_lst = year_sub, cost_params)
  }))
  
  res <- rbind(
    out_dt_em,
    out_dt_ob[, - c("MIN_RATE", "MIN_N", "MIN_D", "TOT_MIN_D", "OPT_N")]
  )[order(ADP, STRATA)]
  
  res
  
}

# TODO INCOMPLETE ()
allo_min_plus_opt2 <- function(allo_lst, conf, MIN_RATE = 0.15, budget, cost_dt, cost_params, metrics = c("discard", "chnk_psc", "hlbt_psc")){
  # allo_lst <- copy(current.allo_lst); conf <- 0.95; MIN_RATE <- 0.15; budget <- 7e6; metrics <- c("discard", "chnk_psc", "hlbt_psc")
  
  # conf <- 0.95; MIN_RATE <- 0.15; budget <- 4.5e6
  
  # 'em_carve_off' must be TRUE or FALSE
  # 'budget' must be length = 1
  
  # em_carve_off <- T
  # em_carve_off <- F
  
  
  
  # TODO MIGHT NOT NEED THIS IF I HAVE SQ TERM
  # if(length(budget) != 1 & length(budget) != length(unique(x$ADP)) ) stop(
  #   "'budget' must be length = 1 or length(unique(x$ADP))!"
  # )
  
  # TODO MIGHT NOT NEED THIS IF I HAVE SQ TERM
  # out_dt <- copy(x)
  # if( !("BUDGET" %in% colnames(x)) ) {
  #   if(length(budget) == length(unique(x$ADP))) {
  #     budget_tbl <- data.table(ADP = as.integer(names(budget)), BUDGET = budget)
  #     out_dt[, BUDGET := budget_tbl[out_dt, BUDGET, on = .(ADP)]]
  #   } else out_dt[, BUDGET := budget]
  # }
  
  out_dt <- copy(allo_lst$effort)
  
  # If em_carve_off = T, then set fixed-gear EM strata to 30% and TRW_EM to 1/3
  if(em_carve_off) {
    
    out_dt_em <- out_dt[STRATA %like% "EM"]
    out_dt_em[
    ][, MON_RATE := ifelse(STRATA %like% "EM_TRW", 1/3, 0.3)
    ][, MON_N := STRATA_N * MON_RATE
    ][, MON_D := MON_N * TRP_DUR] 
    # Calculate estimated total cost of EM strata by ADP
    em_cost_by_adp <- out_dt_em[, .(EM_COST = sum(STRATA_N * MON_RATE * TRP_DUR * CPD)), by = .(ADP)]
    #em_cost_by_adp <- setNames(em_cost_by_adp$EM_COST, em_cost_by_adp$ADP)
    
    # Allocate the remaining funds to observer strata
    out_dt <- out_dt[STRATA %like% "OB"][
    ][em_cost_by_adp, on = .(ADP)
    ][, BUDGET := budget - EM_COST
    ][, EM_COST := NULL][]
    
  } else out_dt[, BUDGET := budget]
  
  # Omit ZERO strata
  out_dt <- out_dt[!(STRATA %like% "ZERO")]
  
  # Prepare allocation weights from trips_melt (variance of metrics for each stratum and ADP year)
  weights <- allo_lst$tm[
    STRATA %in% unique(out_dt$STRATA) & Metric %in% metrics, 
    .(S2_h = var(Value), N_h = .N), keyby = .(ADP, Metric, STRATA)]
  
  # For each ADP year...
  adp_years <- unique(out_dt$ADP)
  adp_list <- lapply(adp_years, function(adp) {
    
    # Determine trips/days to achieve the 15% hurdle and the conf_rate according to the confidence level specified
    x1 <- out_dt[ADP == adp
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
    
    # If the minimum rate is afforded at the specified confidence level, say so:
    if ( unique(x1$BUDGET) > x1[, sum(CONF_D * CPD)]) {
      cat(
        paste0(adp, " : Hurdle afforded at ", conf, " confidence level. Allocating optimized samples.\n"))
    } 
    
    # If the minimum rate can be met (i.e. at conf=0.5) but not at the specified confidence, start by allocating by the
    # proportion of CONF_RATE above MIN_RATE
    if( (unique(x1$BUDGET) < x1[, sum(CONF_D * CPD)]) & (unique(x1$BUDGET) >  x1[, sum(MIN_D * CPD)]) ) {
      
      opt_days_afforded <- F
      
      # TODO Does using the proportion of days above MIN_D only work if CPD is the same for all strata?
      # Using the proportion of funds allocated to each stratum above MIN_RATE to achieve CONF_RATE, calculate CONF_NEW 
      # as the 
      
      x1[, MIN_COST := (MIN_D * CPD)]    # Cost of affording minimum rate
      x1[, CONF_COST := (CONF_D * CPD)]  # cost of affording confidence rate
      x1[, CONF_COST_PROP := (CONF_COST - MIN_COST) / sum(CONF_COST - MIN_COST)] # Proportion of funds above min_rate allocated to achieving conf
      x1[, LEFT_OVER := BUDGET - sum(MIN_COST)]   # Calculate total funds left over after affording minimum rate
      x1[, CONF_RATE_NEW := MIN_RATE + (LEFT_OVER * CONF_COST_PROP / CPD / TRP_DUR / STRATA_N)]
      
      # Get the confidence level achieved for all strata, and grab the highest one from which to work down until we 
      # fall under AFF_D. Using the highest confidence level means we'll start close to but still over budget, e.g., 
      # a conservative ballpark estimate that will save considerable time in the upcoming loop.
      conf_new <- round(max(x1[, .(CONF_NEW = pbinom(
        q = ceiling(MIN_RATE * STRATA_N), size = STRATA_N, prob = CONF_RATE_NEW, lower.tail = F)), 
        by = .(STRATA)]$CONF_NEW), 4)
      if(conf_new < 0.5) conf_new <- 0.5           # If the estimate for conf_new is below 0.5, set it to 0.5.
      GO <- T
      
      while( GO ){
        x1[, CONF_RATE_NEW := find_conf_rate(STRATA_N, MIN_RATE, conf_new), by = STRATA
        ][, CONF_N := CONF_RATE_NEW * STRATA_N
        ][, CONF_D := CONF_N * TRP_DUR]
        if(  sum(x1[, CONF_D * CPD]) > unique(x1$BUDGET) ){
          conf_new <- conf_new - 0.0001                            # If too many days purchased, reduce confidence level
        } else {
          GO <- F                                                  # Once we fall under AFF_D, stop
        }
      }
      x1[, ':=' (CONF = conf_new, CONF_RATE = CONF_RATE_NEW)        # Update and clean up
      ][, TOT_CONF_D := sum(CONF_D)
      ][, c("MIN_COST", "CONF_COST", "CONF_COST_PROP", "LEFT_OVER") := NULL]
      
      warning(
        paste0(adp, " : Minimum rate of ", MIN_RATE, " afforded but only at ", round(conf_new, 4), " confidence level."), 
        call. = F, immediate. = T
      )
      
      
    }
    
    # Calculate number of optimized days afforded
    x1[, C_N := CPD * PRIOR_MTD                               # Average Cost of observing one trip within stratum
    ][, MIN_C := sum(CPD * TRP_DUR * CONF_N), by = .(ADP)     # total cost of affording hurdle
    ][, OPT_BUD := BUDGET - MIN_C                             # budget remaining for optimization after affording hurdle
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
    ][, METRIC := paste(metrics, collapse = "+")]                          # coercing the metric column to blended, which happens next 
    
    # Save the weights from the individual metrics
    metrics <- x1[, .(ADP, STRATA, STRATA_N, CONF_RATE, Metric, S_h, OPT_STRATA_N, C_N, N_h_S_h_div_sqrtC, sumN_h_S_h_div_sqrtC, W_hopt, TTL_OPT_N)]
    
    x1 <- x1[, .(
      ADP, STRATA, STRATA_N, TRP_DUR, PRIOR_MTD, CPD, BUDGET, MIN_RATE, CONF, CONF_RATE, CPD, TOT_MIN_D, TOT_CONF_D,
      OPT_BUD, MIN_N, MIN_D, CONF_N, CONF_D, METRIC, W_hopt)]
    x1 <- x1[, lapply(.SD, mean), .SDcols = c("W_hopt"), by = .(
      ADP, STRATA, STRATA_N, TRP_DUR, PRIOR_MTD, BUDGET, MIN_RATE, CONF, CONF_RATE, CPD, TOT_MIN_D, TOT_CONF_D, 
      OPT_BUD, MIN_N, MIN_D, CONF_N, CONF_D,  METRIC)]
    setkey(x1, ADP, STRATA)
    
    # Now apply the blended weightings 
    x1[, RAW_OPT_N := OPT_BUD / CPD * W_hopt / TRP_DUR]               # Initial estimate of optimized trips afforded 
    if(opt_days_afforded == F) x1[, OPT_N := 0] else x1[, OPT_N := RAW_OPT_N]
    x1[, OPT_D := OPT_N * TRP_DUR                                     # Estimate optimized days afforded
    ][, MON_D := CONF_D + OPT_D                                        # Total observed days
    ][, MON_N := CONF_N + OPT_N                                        # Total observed trips
    ][, MON_RATE := MON_N / STRATA_N]                                   # Monitoring rate
    
    # If the minimum rate was not afforded, revert to equal rates. 
    if( unique(x1$BUDGET) < x1[, sum(MIN_D * CPD)] ){
      warning(
        paste0(adp, " : Minimum rate of ", MIN_RATE, " not afforded. Resorting to 'Equal Rates' allocation."), 
        call. = F, immediate. = T)
      
      conf_attempt <- copy(x1)[, .(
        STRATA, METRIC, W_hopt, CONF = NA_real_, CONF_RATE = NA_real_,
        CONF_N = NA_real_, CONF_D = NA_real_, TOT_CONF_D = NA_real_
      )]
      x1 <- allo_equal(x1, unique(x1$BUDGET))[conf_attempt, on = .(STRATA)] 
    }
    
    list(
      out = x1[, .(
        ADP, STRATA, STRATA_N, TRP_DUR, PRIOR_MTD, CPD, MIN_RATE, CONF, CONF_RATE, METRIC, W_hopt, MIN_N, MIN_D, 
        TOT_MIN_D, CONF_N, CONF_D, TOT_CONF_D, MON_RATE, MON_N, MON_D, OPT_N)],
      metrics = metrics
    )
    
  })
  
  # Prepare outputs
  metrics_dt <- rbindlist(lapply(adp_list, "[[", "metrics") )
  out_dt <- setkey(rbindlist(lapply(adp_list, "[[", "out")), ADP, STRATA)
  if(em_carve_off) out_dt <- rbind(out_dt, out_dt_em, fill = T)
  setkey(out_dt, ADP, STRATA)
  setattr(out_dt, "metrics", metrics_dt)
  
  out_dt
  
}


# Cost-weighted Boxes
calculate_cost_cwb <- function(rates_cwb, cost_params, allo_lst) {
  # rates_cwb <- copy(init_rates); 
  
  # FIXME need stratum_cols
  # allo_lst has column STRATA That is essentially STRATUM_COL
  # rates_cwb would ideally have a similar column
  
  rates_cwb[, TRP_DUR := allo_lst$effort[rates_cwb, TRP_DUR, on = c(ADP = "ADP", STRATA = "STRATUM_COL")]]         # TODO NEED stratum_cols here
  rates_cwb[, MON_RATE := SAMPLE_RATE]
  rbindlist(lapply(
    split(rates_cwb, by = "ADP"),
    function(x) {
      # x <- init_rates[ADP == 2022]
      x1 <- copy(x)
      x1[STRATA %like% "OB", CPD := ob_cost(x, cost_params)$OB_CPD ]
      x1[STRATA %like% "EM_HAL|EM_POT|EM_FIXED", CPD := emfg_cost(x, cost_params)$EMFG_CPD]
      x1[STRATA %like% "EM_TRW", CPT := emtrw_cost(x, cost_params)$EMTRW_CPT]
      x1[!(STRATA %like% "EM_TRW"), CPT := CPD * TRP_DUR]
      
      x1
    })
  )
  
}
allo_cwb_half_diff2 <- function(x, cwb_prop, budget, sqrt_cost = T, allo_lst, cost_params) {
  # x <- copy(old_step)
  
  Ph_dt <- copy(cwb_prop$Ph_dt)
  year_strata <- unlist(cwb_prop$params[c("year_col", "stratum_cols")], use.names = F)
  
  x <- copy(x)
  x[, SAMPLE_RATE := round(SAMPLE_RATE + (round(fh,4) - SAMPLE_RATE)/2, 4)]  # Adjust the sample rate to half the difference with N_h/N
  x <- calculate_cost_cwb(x, cost_params, allo_lst)      # Update CPT    
  x[, Ph := Ph_dt[x, Ph, on = c(year_strata, "SAMPLE_RATE")]][]     #update Ph values using new sample rates
  
  if(sqrt_cost) allo_cwb(x, budget) else allo_cwb2(x, budget)
  x
}
allo_cwb_loop2 <- function(cwb_prop, allo_lst, cost_params, budget, sqrt_cost = T) {
  # cwb_prop <- copy(cwb_prop); sqrt_cost <- T; budget <- 4.5e6
  
  # cwb_prop <- copy(fixed_fmp.cwb_prop); allo_lst <- copy(allo_fixed_fmp_lst); budget <- 4.5e6
  
  
  # budget <- 4.5e6
  
  year_strata <- unlist(cwb_prop$params[c("year_col", "stratum_cols")], use.names = F)
  stratum_cols <- unlist(cwb_prop$params$stratum_cols)
  
  # Start with 15% across the board
  init_rates <- unique(copy(cwb_prop$Ph_dt))[SAMPLE_RATE == 0.150][!(STRATA %like% "ZERO")]
  # init_rates[, CPT := trip_costs[init_rates, CPT, on = c(year_strata)]]     
  
  # Make a stratum_col to match STRATA in allo_lst$effort
  stratum_column <- apply(init_rates[, ..stratum_cols], 1, paste, collapse = "-")
  init_rates[, STRATUM_COL := stratum_column]
  
  # Calculate cost per trip
  init_rates <- calculate_cost_cwb(init_rates, cost_params, allo_lst)
  
  # Make the initial allocation assuming 15%
  if(sqrt_cost) old_step <- allo_cwb(init_rates, target_budget = budget) else {
    old_step <- allo_cwb2(init_rates, target_budget = budget)
  }
  old_diff <- Inf
  old_bias <- Inf
  old_var <- Inf
  new_diff <- old_step[, abs(sum(fh - SAMPLE_RATE))]
  new_bias <- old_step[, mean(fh - SAMPLE_RATE)]
  new_var <- old_step[, var(fh - SAMPLE_RATE)]
  GO <- T
  i <- 0
  steps <- matrix(nrow = 50, ncol=2)
  colnames(steps) <- c("abs_diff", "var")
  
  # Keep halving differences between assumed SAMPLE_RATE and allocate rate 'fh'
  # TODO Adjust the costs as the rates change!!
  while(GO) {
    i <- i + 1
    new_step <- allo_cwb_half_diff2(old_step, cwb_prop, budget, sqrt_cost = sqrt_cost, allo_lst, cost_params)
    new_diff <- new_step[, abs(sum(fh - SAMPLE_RATE))]
    new_bias <- new_step[, mean(fh - SAMPLE_RATE)]
    new_var <- new_step[, var(fh - SAMPLE_RATE)]
    
    # Once there are no improvements, stop the loop
    
    steps[i,] <- c(new_diff, new_var)
    
    # if bias goes down and variance goes down, continue 
    # if bias goes up a little but variance goes down, continue
    if(i > 2) {
      if(steps[i, 1] == steps[i-2, 1]) GO <- F
    }
    
    if((new_diff < old_diff) | (new_var < old_var)){
      
      old_step <- copy(new_step)
      old_diff <- new_diff
      old_var <- new_var
    } else GO <- F
  }
  
  list(
    rates = old_step,
    steps = steps[!is.na(steps[,1]), , drop = F]
  )
  
}

# Proximity
calculate_cost_prox <- function(index_rates, cost_params, allo_lst, max_budget) {
  
  index_rates_copy <- copy(index_rates)
  index_rates_copy[, I := .GRP, by = .(ADP, INDEX)] # Give an identifier to each
  allo_lst_effort <- copy(allo_lst$effort[!(STRATA %like% "ZERO")])
  index_rates_copy[, TRP_DUR := allo_lst_effort[index_rates_copy, TRP_DUR, on = c(ADP = "ADP", STRATA = "STRATUM_COL")]]  # merge in trip duration
  index_rates_copy[, MON_RATE := SAMPLE_RATE]
  
  # Now for each index, calculate costs
  out <- rbindlist(lapply(
    split(index_rates_copy, by = "ADP"),
    function(x) {
      
      index_vec <- unique(x$I)
      index_rates_lst <- vector(mode = "list", length = uniqueN(index_vec))
      
      for(i in 1:length(index_vec)) {
        index_rates_sub <- x[I == index_vec[i]]
        index_rates_sub_res <- cbind(
          ob_cost(index_rates_sub[STRATA %like% "OB_"], cost_params),
          emfg_cost( index_rates_sub[STRATA %like% "EM_HAL|EM_POT|EM_FIXED"], cost_params),
          emtrw_cost( index_rates_sub[STRATA %like% "EM_TRW"], cost_params)
        )
        index_rates_sub_res[, INDEX_COST := sum(OB_TOTAL, EMFG_TOTAL, EMTRW_TOTAL)]
        # If the cost exceeds the specified maximum budget, don't bother continuing cost calculations
        if(index_rates_sub_res$INDEX_COST > max_budget) break 
        else (index_rates_lst[[i]] <- cbind(index_rates_sub_res, unique(index_rates_sub[, .(ADP, INDEX, I)])))
      }  #how long does this take? Around 2 minutes?
      rbindlist(index_rates_lst)
    }
  ))
  
  # Remove copied objects
  rm(index_rates_copy,  allo_lst_effort)
  out
  
}
calculate_index2 <- function(ispn_res, cost_params, allo_lst, max_budget) {
  # ispn_res <- copy(prox_insp); 
  
  # ispn_res <- copy(fixed_fmp.prox_insp); allo_lst <- copy(allo_fixed_fmp_lst);  max_budget <- 7e6
  
  group_cols <- c(ispn_res$params$year_col, ispn_res$params$stratum_cols)
  stratum_cols <- ispn_res$params$stratum_cols
  year_col <- ispn_res$params$year_col
  
  # Calculate index as INTERSPERSION / CV_SCALING
  x1 <- copy(ispn_res$ispn_dt)[
  ][, n := SAMPLE_RATE * STRATA_N
  ][, FPC := (STRATA_N - n) / STRATA_N
  ][, CV_SCALING := sqrt(FPC * (1/n))
    # ][, INDEX := ISPN / CV_SCALING][]
  ][, INDEX := ISPN * (1 - CV_SCALING)][]                             # TODO Trying this index calculation!
  
  
  # Find range if INDEX that is common to all strata x ADP
  x2 <- x1[
  ][, .(MIN = min(INDEX), MAX = max(INDEX)), by = group_cols
  ][, .(MIN = max(MIN),  MAX = min(MAX)), by = year_col]
  
  # Subset each year by the index range
  x1 <- do.call(rbind, apply(x2, MARGIN = 1, function(y) {
    x1[get(year_col) == y[[year_col]]][data.table::between(INDEX, y[["MIN"]], y[["MAX"]])]
  }))
  
  # For each ADP year, create a vector from the minimum to maximum index range
  # index_vectors <- apply(x2, 1, function(y) seq(y[2], y[3], by = 0.01))                
  index_vectors <- apply(x2, 1, function(y) seq(y[2], y[3], by = 0.001))                  # TODO New INDEX also allows by to be 0.001!
  
  # For each of those indices, find the rates required of all the STRATA to achieve those indices
  # TODO This takes a while - try to speed this up! Is 0.01 increment for index vectors needlessly small?
  # is findInterval slower?
  # Is using .SD what is slowing things down? If I split by stratum_cols, should be faster if I avoid it?
  
  index_rates <- rbindlist(Map(
    function(y1, y2) {
      # y1 <- index_vectors[[5]]; y2 <- split(x1, by = year_col)[[5]] # 2022
      
      rbindlist(lapply(y1, function(z) {
        # z <- y1[950]
        z1 <- data.table(INDEX = z)
        y2[, .SD[z1, on = .(INDEX), roll = "nearest"], keyby = group_cols]
      }))
    },
    y1 = index_vectors,
    y2 = split(x1, by = year_col)
  ))
  
  # Create a new STRATUM_COL using stratum_cols
  stratum_column <- apply(index_rates[, ..stratum_cols], 1, paste, collapse = "-")
  index_rates[, STRATUM_COL := stratum_column]
  
  # Calculate the costs of each index. 
  index_cost <- calculate_cost_prox(index_rates, cost_params, allo_lst, max_budget)
  index_rates[, INDEX_COST := index_cost[index_rates, INDEX_COST, on = .(ADP, INDEX)]]  # Merge in index costs
  
  # cost_cols <- names(trip_cost_dt)[names(trip_cost_dt) %in% group_cols]
  # index_rates[, CPT := trip_cost_dt[index_rates, CPT, on = cost_cols]]   # Merge in stratum-specific trip costs
  # index_rates[, INDEX_COST := sum(CPT * n), by = c(year_col, "INDEX")]               # Total stratum costs by ADP and INDEX
  
  index_rates_melt <- melt(
    index_rates, 
    id.vars = c(stratum_cols, "STRATA_N", "INDEX_COST"), 
    measure.vars = c("INDEX", "ISPN", "CV_SCALING", "SAMPLE_RATE"))
  
  # TODO Return the costs in the list
  index_out <- list(rates = index_rates, melt = index_rates_melt, params = ispn_res$params, costs = index_cost)
  index_out
}
prox_rates_from_budget2 <- function(index_res, budget) {
  # index_res <- copy(prox_index); budget <- 4.5e6
  
  group_cols <- unname(unlist(index_res$params[c("year_col", "stratum_cols")]))
  budget_dt <- data.table(INDEX_COST = budget)
  out <- index_res$rates[, .SD[budget_dt, on = .(INDEX_COST), roll = "nearest"], keyby = group_cols]
  index_costs <- index_res$costs[unique(out[, .(ADP, INDEX)]), on = .(ADP, INDEX)]  
  
  setattr(out, "costs", index_costs)  
  out
}

# this is a version that calculates interspersion the old way, ISPN / CV_SCALING
calculate_index3 <- function(ispn_res, cost_params, allo_lst, max_budget) {
  # ispn_res <- copy(prox_insp); 
  
  # ispn_res <- copy(fixed_fmp.prox_insp); allo_lst <- copy(allo_fixed_fmp_lst);  max_budget <- 7e6
  
  group_cols <- c(ispn_res$params$year_col, ispn_res$params$stratum_cols)
  stratum_cols <- ispn_res$params$stratum_cols
  year_col <- ispn_res$params$year_col
  
  # Calculate index as INTERSPERSION / CV_SCALING
  x1 <- copy(ispn_res$ispn_dt)[
  ][, n := SAMPLE_RATE * STRATA_N
  ][, FPC := (STRATA_N - n) / STRATA_N
  ][, CV_SCALING := sqrt(FPC * (1/n))
  ][, INDEX := ISPN / CV_SCALING][]
  #][, INDEX := ISPN * (1 - CV_SCALING)][]                             # TODO Trying this index calculation!
  
  
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
  #index_vectors <- apply(x2, 1, function(y) seq(y[2], y[3], by = 0.001))                  # TODO New INDEX also allows by to be 0.001!
  
  # For each of those indices, find the rates required of all the STRATA to achieve those indices
  # TODO This takes a while - try to speed this up! Is 0.01 increment for index vectors needlessly small?
  # is findInterval slower?
  # Is using .SD what is slowing things down? If I split by stratum_cols, should be faster if I avoid it?
  
  index_rates <- rbindlist(Map(
    function(y1, y2) {
      # y1 <- index_vectors[[5]]; y2 <- split(x1, by = year_col)[[5]] # 2022
      
      rbindlist(lapply(y1, function(z) {
        # z <- y1[950]
        z1 <- data.table(INDEX = z)
        y2[, .SD[z1, on = .(INDEX), roll = "nearest"], keyby = group_cols]
      }))
    },
    y1 = index_vectors,
    y2 = split(x1, by = year_col)
  ))
  
  # Create a new STRATUM_COL using stratum_cols
  stratum_column <- apply(index_rates[, ..stratum_cols], 1, paste, collapse = "-")
  index_rates[, STRATUM_COL := stratum_column]
  
  # Calculate the costs of each index. 
  index_cost <- calculate_cost_prox(index_rates, cost_params, allo_lst, max_budget)
  index_rates[, INDEX_COST := index_cost[index_rates, INDEX_COST, on = .(ADP, INDEX)]]  # Merge in index costs
  
  # cost_cols <- names(trip_cost_dt)[names(trip_cost_dt) %in% group_cols]
  # index_rates[, CPT := trip_cost_dt[index_rates, CPT, on = cost_cols]]   # Merge in stratum-specific trip costs
  # index_rates[, INDEX_COST := sum(CPT * n), by = c(year_col, "INDEX")]               # Total stratum costs by ADP and INDEX
  
  index_rates_melt <- melt(
    index_rates, 
    id.vars = c(stratum_cols, "STRATA_N", "INDEX_COST"), 
    measure.vars = c("INDEX", "ISPN", "CV_SCALING", "SAMPLE_RATE"))
  
  # TODO Return the costs in the list
  index_out <- list(rates = index_rates, melt = index_rates_melt, params = ispn_res$params, costs = index_cost)
  index_out
}

# This function is a faster version of allocating rates according to the Proximity allocation method given a budget.
# (a replacement for calculate_interspersion_gs(), calculate_index2(), and prox_rates_from_budget2())
# The original procedure was to feed a wide and high-resolution vector of sample rates to all strata to calculate the
# proximity/cv_scaling/index of each stratum at each rate, then cost out each index, then filter indices by budget.
# This method does a short 'hone' so metrics are not calculated for a stratum at rates far away from what is relevant
# from the budget, saving time.
allo_prox <- function(box_def, allo_lst, cost_params, budget, max_budget, index_interval = 0.001, range_var = 1) { 
  
  # TODO This function could still be faster. `index_interval` should be replaced by a level of resolution that we want 
  # to achieve for the index afforded, and rates should be honed for each stratum to achieve that. Initialize 
  # proximity/cv_scaling/index range with coarse resolution of rates, if needed subdivide those until a particular
  # resolution of indices is acquired, find the cost of each of those indices, hone in on a particular range, repeatedly
  # adjusting rates until the specified resolution of indices is acquired. 
  
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
