# These are the functions from analyses/allocation_evaluation
# Some are further developed version of those in analyses/spatiotemporal_boxes

library(data.table)   # For data wrangling, very efficient with joins, especially rolling joins
library(dplyr)        # For data wrangling/piping with sf package
library(sf)           # For spatial statistics

#' TODO There may be more functions in the Annual Report Repo in spatiotemp_functions.R that may be useful to add here.

#======================================================================================================================#
# Data Preparation -----------------------------------------------------------------------------------------------------
#======================================================================================================================#

# This function is used to prepare VALHALLHA data set for spatiotemporal analyses, e.g., define boxes.
spatiotemp_data_prep <- function(valhalla){
  #' This function is used for the Annual Report, and therefore uses STRATA to define strata, not STRATA_NEW, which is 
  #' used for the Annual Deployment Plans.
  
  pc_effort_dt <- valhalla |>
    # Subset to partial coverage trips only
    _[COVERAGE_TYPE == "PARTIAL"
      # Change TRIP_ID to integer class, keeping original TRIP_ID for posterity as (wd_TRIP_ID, or 'work.data TRIP_ID')
    ][, wd_TRIP_ID := TRIP_ID
    ][, TRIP_ID := NULL
    ][, TRIP_ID := .GRP, keyby = .(wd_TRIP_ID)
      # Change VESSEL_ID to integer class and use it to replace PERMIT (some years don't have PERMIT!)
    ][, PERMIT := NULL
    ][, PERMIT := as.integer(VESSEL_ID)]
  # Count total of partial coverage trips
  pc_trip_id_count <- uniqueN(pc_effort_dt$TRIP_ID)
  
  #' `BSAI_GOA` is now created in get_data.R
  
  #' `BS_AI_GOA` is no longer needed, but can still be created here if desired.
  # # For each trip, Identify which FMP had most retained catch, splitting FMP by BS, AI and GOA
  # fmp_bs_ai_goa <- copy(pc_effort_dt)[
  # ][, BS_AI_GOA := fcase(
  #   REPORTING_AREA_CODE %in% c(541, 542, 543), "AI",
  #   REPORTING_AREA_CODE %in% c(508, 509, 512, 513, 514, 516, 517, 518, 519, 521, 523, 524), "BS",
  #   REPORTING_AREA_CODE %in% c(610, 620 ,630, 640, 649, 650, 659), "GOA")  
  # ][, .(
  #   FMP_WT = sum(WEIGHT_POSTED[SOURCE_TABLE == "Y"], na.rm = T)
  # ), by = .(TRIP_ID, BS_AI_GOA)][, .SD[which.max(FMP_WT)], by = .(TRIP_ID)][, FMP_WT := NULL][]
  # if( 
  #   (pc_trip_id_count != uniqueN(fmp_bs_ai_goa$TRIP_ID) ) | 
  #   (pc_trip_id_count != uniqueN(fmp_bs_ai_goa[!is.na(BS_AI_GOA), TRIP_ID]) ) 
  # ){
  #   stop("Something went wrong making 'fmp_bs_ai_goa'")
  # }
  
  # Simplify the output, keeping only the columns that are needed
  pc_effort_dt <- pc_effort_dt[, .(
    PERMIT, TARGET = TRIP_TARGET_CODE, AREA = as.integer(REPORTING_AREA_CODE), AGENCY_GEAR_CODE, BSAI_GOA,
    GEAR = ifelse(AGENCY_GEAR_CODE %in% c("PTR", "NPT"), "TRW", AGENCY_GEAR_CODE), STRATA, OBSERVED_FLAG,
    TRIP_TARGET_DATE, LANDING_DATE, ADFG_STAT_AREA_CODE = as.integer(ADFG_STAT_AREA_CODE), DAYS, wd_TRIP_ID
    ), keyby = .(ADP = as.integer(ADP), TRIP_ID)] |>
    unique()
  
  #' Merge in FMP classifications, only if you want to add "BS_AI_GOA" 
  # pc_effort_dt <- pc_effort_dt[fmp_bs_ai_goa, on = .(TRIP_ID)]
  
  # Assign Pool
  pc_effort_dt[, POOL := ifelse(STRATA %like% "EM", "EM", ifelse(STRATA == "ZERO", "ZE", "OB"))]
  
  # Check for missing landing dates
  if(nrow(pc_effort_dt[is.na(LANDING_DATE)])) {
    warning(paste0(nrow(pc_effort_dt[is.na(LANDING_DATE)]), " records are missing LANDING_DATE! and were removed!"))
    pc_effort_dt <- pc_effort_dt[!is.na(LANDING_DATE)]
  }
  
  # Make sure no full coverage trips are still in the dataset.
  if(any(unique(pc_effort_dt$STRATA) == "FULL")) stop("There are some FULL coverage trips in the partial coverage dataset!")
  
  # Finalize formatting
  pc_effort_dt <- pc_effort_dt |>
    # Set the order of columns
    setcolorder(neworder = c(
      "ADP", "POOL", "PERMIT", "TRIP_ID", "STRATA", "AGENCY_GEAR_CODE", "GEAR", "TRIP_TARGET_DATE", "LANDING_DATE", "AREA", 
      "ADFG_STAT_AREA_CODE", "BSAI_GOA", "TARGET", "wd_TRIP_ID", "OBSERVED_FLAG"
    )) |>
    # Arrange trips by key columns
    setorder(ADP, POOL, PERMIT, TRIP_TARGET_DATE) |>
    # For some reason, my shapefiles don't include ADFG STAT AREA 515832. Seem like it was merged into 515831. 
    _[ADFG_STAT_AREA_CODE == 515832, ADFG_STAT_AREA_CODE := 515831] |>
    unique()

  # Double-check that all trips have a non-NA for STRATA
  if(nrow(pc_effort_dt[is.na(STRATA)])) stop("Some trips don't have a non-NA STRATA")
  
  # Output the results
  pc_effort_dt
}

# This function returns a list with each trip's list of unique Julian dates. Used by ob_cost_new() and bootstrap_allo()
trip_dates <- function(x) {

  trip_dates_dt <- x |>
    _[, c("START", "END") := lapply(.SD, as.integer), .SDcols = c("TRIP_TARGET_DATE", "LANDING_DATE")
    ][, as.list(range(START, END)), keyby = .(ADP, STRATA, TRIP_ID)]
  trip_dates_lst <- apply(trip_dates_dt, 1, function(x) seq(x[["V1"]], x[["V2"]], 1))
  names(trip_dates_lst) <- trip_dates_dt$TRIP_ID
  list(
    dt = trip_dates_dt,
    lst = trip_dates_lst
  )
  
}


#======================================================================================================================#
# Space/Time Box Definition  -------------------------------------------------------------------------------------------
#======================================================================================================================#

# Converts ADFG Stat Area to an iso-area hexagon grid. Specify the cell size in meters as the width of each hex cell.
# The output is a dataframe with corresponding ADFG_STAT_AREA_CODEs and HEX_IDs, as well as the
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
  
  # Generate a hex cell grid using the projection and boundaries from stat_area_centroid_sf, with cell size specified by
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


#' Applies a box definition to fishing effort prepared by spatiotemp_data_prep(), defined by `space`: width of hex cell 
#' in km and the radius of its neighborhood) as well as `time`: the function (currently only tested with 'week') and the 
#' number of time units. `year_col`. In allocation, `stratum_cols` is used to specify the columns used in stratification
#' and optionally, `ps_cols` is used to define the columns used in post-stratification, such as gear type to split HAL 
#' and POT trips within the fixed-gear strata. Optionally, `dmn_cols` is used in evaluation to specify attributes to 
#' allow neighboring across strata  and is given as a list where the `nst` item is a vector of non-spatiotemporal
#' columns such as gear type and `st` is a vector of spatial columns such as BSAI_GOA.

# modified version of define_boxes_gs
# that was used in the 2024 ADP, but adjusted to separate spatiotemporal domains from nonspatiotemporal domains, which
# allows trips that are defined to be in either the BSAI or GOA to still neighbor each other according to the normal
# neighboring rules of the box definition
define_boxes <- function(data, space, time, year_col, stratum_cols, dmn_lst = NULL, stata_area_sf = stat_area_sf, geom = F, ps_cols = NULL) {
  # TODO Haven't done much testing with both ps_cols and dmn_lst defined with GEAR type. 
  
  # data <- copy(swor_bootstrap.effort); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; stratum_cols <- c("STRATA"); geom <- F; ps_cols <- c("GEAR"); dmn_lst <- NULL
  
  
  # data <-  copy(val_mixed); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; dmn_cols <- "GEAR"; stratum_cols <- c("STRATA"); geom = T; ps_cols <- "GEAR"
  
  # data <- copy(pc_dat); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; dmn_cols <- "BSAI_GOA"; stratum_cols <- c("STRATA");  geom = T; ps_cols <- NULL
  
  # using new dmn_cols
  #'*GEAR BSAI_GOA*   data <- copy(pc_dat); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; dmn_lst <- list(nst = "GEAR", st = "BSAI_GOA"); stratum_cols <- c("STRATA");  geom = T; ps_cols <- NULL
  #'*GEAR*            data <- copy(pc_dat); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; dmn_lst <- list(nst = "GEAR", st = NULL); stratum_cols <- c("STRATA");  geom = T; ps_cols <- NULL
  #'*BSAI_GOA*        data <- copy(pc_dat); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; dmn_lst <- list(nst = NULL, st = "BSAI_GOA"); stratum_cols <- c("STRATA");  geom = T; ps_cols <- NULL
  #'*NULL*            data <- copy(pc_dat); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; dmn_lst <- NULL; stratum_cols <- c("STRATA");  geom = T; ps_cols <- NULL
  
  #' TODO Make sure none are duplicated between ts and nonts dmn_cols
  
  # Testing with ps_cols = NULL
  # data <-  copy(val_mixed); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; dmn_cols <- "GEAR"; stratum_cols <- c("STRATA"); geom = T; ps_cols <- NULL
  
  if( is.null(dmn_lst) ){
    dmn_lst <- list(nst = NULL, st = NULL)
  } else {
    # Make sure dmn_lst doesn't have repeats
    if( length(intersect(dmn_lst$nst, dmn_lst$st)) > 1 ) {
      stop("`dmn_lst` can't have the same column under both categorical and spatiotemporal elements!")
    }
  }
  # Prepare domain column vector
  dmn_cols <- unlist(dmn_lst, use.names = F)
  
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
  
  dates_start <- min(dates_mtx[, 1]) - 1  # Get first date and subtract 1.
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
  # For each TRIP_ID × HEX_ID, identify unique weeks
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
  #' TODO *NEW* Add columns for ts_dmn_cols and nonts_dmn_cols if NULL?
  
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
  
  data_dmn <- copy(data)
  if(!is.null(dmn_cols)) {
    data <- unique(data[, -..dmn_cols])  # Remove 'dmn_cols' for now
  }
  
  group_cols  <- c(year_col, stratum_cols)
  setkeyv(data, cols = c(group_cols, "PS_ID", "BOX_ID"))
  data_lst <- lapply(
    X = split(x = subset(data, select = c(group_cols, "BOX_ID", "TRIP_ID", "PS_ID")), by = group_cols, keep.by = F),
    FUN = as.matrix)
  
  # Make the frequency table of each TRIP_ID (so that trips are properly split by HEX_ID, TIME, and if present, ps_cols)
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
          
          trip_id_centered <- x[x[,1] == y, 2]    # Identify number of trips actually in the box
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
  
  #' TODO dmn_lst is never not null since it is now created if left null. Can remove? Always create dmn outputs?
  if( !is.null(dmn_cols) ) {
    
    # Save the raw form of data_dmn for the output
    data_dmn_og <- copy(data_dmn)
    
    # Get trip weights, splitting by dmn_cols as well (both categorical and spatiotemporal)
    trip_id_dmn_mat <- as.matrix(data_dmn[, .N, by = .(TRIP_ID)])
    trip_id_dmn_vec <- vector(mode = "integer")
    trip_id_dmn_vec[trip_id_dmn_mat[, 1]] <- trip_id_dmn_mat[, 2]
    
    # Codify domains. If nonspatiotemporal or spatiotemporal domains are null, create placeholders
    if( is.null(dmn_lst$nst) ){
      data_dmn[, NST_DMN := 1L]
      nst_dmn_vec <- "NST_DMN"
      nst_dmn_tbl <- unique(data_dmn[, ..nst_dmn_vec])
    } else {
      nst_dmn_vec <- unlist(dmn_lst$nst, use.names = F)
      data_dmn[, NST_DMN := .GRP, keyby = nst_dmn_vec]
      nst_dmn_tbl <- unique(subset(data_dmn, select = c("NST_DMN", nst_dmn_vec)))
      data_dmn[, (nst_dmn_vec) := NULL]
    }
    if( is.null(dmn_lst$st) ){
      data_dmn[, ST_DMN := 1L]
      st_dmn_vec <- "ST_DMN"
      st_dmn_tbl <- unique(data_dmn[, ..st_dmn_vec])
    } else {
      st_dmn_vec <- unlist(dmn_lst$st, use.names = F)
      data_dmn[, ST_DMN := .GRP, keyby = st_dmn_vec]
      st_dmn_tbl <- unique(subset(data_dmn, select = c("ST_DMN", st_dmn_vec)))
      data_dmn[, (st_dmn_vec) := NULL]
    }
    
    #' Split by domain
    nst_dmn_lst <- split(data_dmn, by = "NST_DMN", keep.by = F)
    #' *For each non-spatiotemporal domain [x]*
    dmn_nbr_dt <- rbindlist(lapply(nst_dmn_lst, function(x) {
      # x <- nst_dmn_lst[[1]]
      
      # Identify boxes relevant to nst_dmn
      nst_dmn_boxes <- unique(x$BOX_ID)
      #' Split by stratum and convert to matrix
      nst_dmn_stratum_lst <- lapply(
        split(x[, -c(year_col, "TIME", "HEX_ID"), with = F], by = stratum_cols, keep.by = F), 
        as.matrix
      )
      
      #' *For each stratum [y]*
      rbindlist(lapply(nst_dmn_stratum_lst, function(y) {
        # y <- nst_dmn_stratum_lst[[1]]
        
        nst_dmn_stratum_st_dmn_lst <- lapply(
          split(y, f = y[, "ST_DMN"]), 
          matrix, ncol = ncol(y), dimnames = list(NULL, colnames(y))
        )
        
        #' *For each spatiotemporal domain*
        rbindlist(lapply(nst_dmn_stratum_st_dmn_lst, function(z) {
          # z <- nst_dmn_stratum_st_dmn_lst[[1]]
          
          #' *For each BOX_ID*
          as.data.table(do.call(rbind, lapply(nst_dmn_boxes, function(z1) {
            # z1 <- nst_dmn_boxes[1]
            
            # Identify all trips in the domain × stratum in this box
            trip_id_centered <- unique(z[z[, "BOX_ID"] == z1, "TRIP_ID"])
            c(
              BOX_ID = z1,
              BOX_DMN_n = length(trip_id_centered),
              BOX_DMN_w = sum(1 / trip_id_dmn_vec[trip_id_centered]),
              # count number of neighboring trips in the nst_dmn × stratum ()
              BOX_DMN_nbr = length(unique(y[y[, "BOX_ID"] %in% nbr_lst[[z1]], "TRIP_ID"]))
            )
            
          })))
        }), idcol = "ST_DMN")
      }), idcol = "STRATUM_COLS")
    }), idcol = "NST_DMN")
    
    # Restore stratum and domain column names
    dmn_groups <- c("NST_DMN", "ST_DMN")
    dmn_nbr_dt[
    ][, (stratum_cols) := tstrsplit(STRATUM_COLS, split = "[.]")
    ][, STRATUM_COLS := NULL
    ][, (dmn_groups) := lapply(.SD, as.integer), .SDcols = dmn_groups]
    dmn_nbr_dt <- dmn_nbr_dt[nst_dmn_tbl, on = .(NST_DMN)][st_dmn_tbl, on = .(ST_DMN)]
    dmn_nbr_dt[, (dmn_groups) := NULL]
    
    #' TODO *is BOX_ID unique to ADP or can there be crossover between years? Does time start over each year, or is it independent?*
    
    # Merge details back in (ADP year, HEX_ID and TIME)
    box_id_details <- unique(data_dmn[, .(BOX_ID, ADP, HEX_ID, TIME)])
    dmn_nbr_dt <- box_id_details[dmn_nbr_dt, on = .(BOX_ID)]
    setcolorder(dmn_nbr_dt, c(year_col, stratum_cols, dmn_cols, "BOX_ID", "BOX_DMN_n", "BOX_DMN_w", "BOX_DMN_nbr"))
    
    # Calculate Number of trips in each STRATA × dmn_cols. Note that trips that have multiple 
    # 'dmn_cols' were split here!
    strata_dmn_N_dt <- dmn_nbr_dt[, .(STRATA_DMN_N = sum(BOX_DMN_w)), by = c(year_col, stratum_cols, dmn_cols)]
    
    # Double-check that weights sum to STRATA_N
    if(!fsetequal(
      strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)][STRATA_N != 0],
      strata_N_dt
    )) stop("STRATA_N and sum(BOX_DMN_w) are not equal!")
    
    box_res$dmn <- list()
    box_res$dmn$og_data <- data_dmn_og 
    box_res$dmn$strata_dmn_n_dt <- strata_dmn_N_dt
    box_res$dmn$box_dmn_smry_dt <- dmn_nbr_dt
    box_res$dmn$strata_dt <- setorderv(unique(strata_N_dt[, ..stratum_cols]), cols = stratum_cols)[, STRATUM_ID := .I][]
    
  }
  
  # Create sf object with geometry if requested
  if(geom == T) {
    geom_sf <- merge(stat_area_lst$HEX_GEOMETRY, dt_out, on = .(HEX_ID))
    box_res$geom_sf <- geom_sf
    
    geom_dmn_sf <- merge(stat_area_lst$HEX_GEOMETRY, dmn_nbr_dt, on = .(HEX_ID))
    box_res$dmn$geom_dmn_df <- geom_dmn_sf
  }
  
  # Return results
  box_res
  
}

#======================================================================================================================#
# Allocation Functions -------------------------------------------------------------------------------------------------
#======================================================================================================================#

# This function calculates the rates afforded with equal allocation.
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

## Proximity -----------------------------------------------------------------------------------------------------------

#' Bootstrap fishing effort for each stratum, sampling trips without replacement.
#' `sample_N` is a data.table of columns `STRATA` and the predicted number of trips as `N`.
bootstrap_allo <- function(pc_effort_st, sample_N, box_params, cost_params, budget_lst, seed = 12345) {
  #' [Defaults to use when effort_prediction.R is not yet run]
  # seed <- 12345
  
  time_start <- Sys.time()
  if( length(sample_N) > 1 ) cat(paste0("[Start: ", time_start, "]\n"))
  
  pc_effort_lst <- split(pc_effort_st, by = "STRATA")
  
  # Make sure names and ordering are the same
  if(!identical(names(pc_effort_lst), sample_N[[1]]$STRATA)) stop("Strata names/order are not the same!")
  
  # sample_N_lst <- split(sample_N, by = "STRATA")
  
  # Initialize bootstrap list
  swor_boot_lst <- vector(mode = "list", length = length(sample_N))
  
  set.seed(seed)
  for(k in seq_along(sample_N)) {
    # k <- 1
    cat(paste0(k, ", "))
    
    # Bootstrap using each item in strata_N to resample each stratum's population size
    swor_bootstrap.effort <- rbindlist(Map(
      function(prior, strata_N) {
        # prior <- pc_effort_lst[[5]]; strata_N <-  split(sample_N[[k]], by = "STRATA")[[5]]
        
        # Create vector of TRIP_IDs
        trip_ids <- unique(prior$TRIP_ID)
        # How many times does prior effort go into future effort?
        prior_vs_future <- floor(strata_N$N / length(trip_ids))
        # What number of trips should be sampled without replacement?
        swr_n <- strata_N$N - (length(trip_ids) * prior_vs_future)
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
        bootstrap_sample
      }, 
      prior = pc_effort_lst,
      strata_N = split(sample_N[[k]], by = "STRATA")
    ))
    
    # Re-assign trip_id so that we can differentiate trips sampled multiple times
    swor_bootstrap.effort[, TRIP_ID := .GRP, keyby = .(ADP, STRATA, BSAI_GOA, TRIP_ID, I)]
    if(uniqueN(swor_bootstrap.effort$TRIP_ID) != sum(sample_N[[k]]$N)) stop("Count of TRIP_IDs doesn't match!")
    
    # Define boxes of bootstrapped effort
    swor_bootstrap.box <- define_boxes(
      swor_bootstrap.effort, space = box_params$space, time = box_params$time,
      year_col = box_params$year_col, stratum_cols = box_params$stratum_cols, ps_cols = box_params$ps_cols )
    
    # Calculate each stratum's mean trip duration of bootstrapped effort
    swor_bootstrap.allo_lst <- list(
      effort = unique(swor_bootstrap.effort[, .(ADP, STRATA, BSAI_GOA, TRIP_ID, DAYS)]) |>
        _[, .(STRATA_N = uniqueN(TRIP_ID), TRP_DUR = mean(DAYS)), keyby = .(ADP, STRATA)]
    )
    
    # For observer stratum trips, create a vector of Julian Days
    ob_contract_dates <- trip_dates(swor_bootstrap.effort[STRATA %like% "OB_"])
    # For each trip, calculate the proportion of dates the occurred BEFORE the contract change date
    ob_contract_calc <- ob_contract_dates$dt[, .(TRIP_ID)]
    ob_contract_calc$PROP <- sapply(
      ob_contract_dates$lst, function(x) sum(x < julian.Date(cost_params$OB$contract_change_date)) / length(x)
    )
    # Summarize the number of days on each partial coverage contract for each stratum
    ob_contract_calc <- swor_bootstrap.effort |>
      _[STRATA %like% "OB_", .(ADP, STRATA, TRIP_ID, DAYS)] |>
      unique() |>
      # Merge in proportion of days on each contract for each trip
      _[ob_contract_calc, on = .(TRIP_ID)
        # For each Stratum, calculate the number of days on contract period 1 and contract period 2
      ][, .(
        CONTRACT_1 = sum(DAYS * PROP),
        CONTRACT_2 = sum(DAYS *(1 - PROP))
      ), keyby = .(ADP, STRATA)]
    
    # Merge in contract proportions
    swor_bootstrap.allo_lst$effort <- ob_contract_calc[swor_bootstrap.allo_lst$effort, on = .(ADP, STRATA)]
    
    # Calculate the number of sea days in Trawl EM and shuttle this to cost_params
    cost_params$EMTRW$emtrw_sea_days <-  swor_bootstrap.effort |>
      _[STRATA == "EM_TRW-GOA", .(TRIP_ID, DAYS)] |> 
      unique() |>
      _[, sum(DAYS)]
    
    # Calculate rates afforded with the specified budget.
    #' [NOTE:] The `allo_prox()` function currently excludes the "EM_TRW" strata by default.
    swor_bootstrap.rates <- allo_prox(
      swor_bootstrap.box, swor_bootstrap.allo_lst, cost_params, 
      budget_lst[[1]], max_budget = 6e6, index_interval = 0.0001
    )
    # Capture results of iteration
    swor_boot_lst[[k]] <- list(
      rates = swor_bootstrap.rates,
      strata_N = sample_N[[k]],
      resample = unique(swor_bootstrap.effort[, .(wd_TRIP_ID, I)])
    )
  }
  
  time_end <- Sys.time()
  if( length(sample_N) > 1 ) {
    cat(paste0("[End:   ", time_end, "]\n"))
    cat(paste("[Elapsed: ", round(as.numeric(time_end - time_start, units = "mins"),3), "min]"))
  }
  
  swor_boot_lst
}

#' This was the version used in the 2024 Final ADP in the final_rates.R script. Used `calculate_prox`
#' and `calculate_cost`
allo_prox <- function(box_def, allo_lst, cost_params, budget, max_budget, index_interval = 0.001, range_var = 1) { 
  
  # TODO I think this function could be faster. index_interval should be replaced by a level of resolution that we want 
  # to achieve for the index afforded, and rates should be adjusted for each stratum to achieve that. Initialize 
  # proximity/cv_scaling/index range with coarse resolution of rates, if needed subdivide those until a particular
  # resolution of indices is acquired, find the cost of each of those indices, hone in on a particular range, repeatedly
  # adjusting rates until the specified resolution of indices is acquired. 
  
  # Have to do this by year, so feed the data one year at a time
  group_cols <- c(box_def$params$year_col, box_def$params$stratum_cols)
  stratum_cols <- box_def$params$stratum_cols
  year_col <- box_def$params$year_col
  
  year_vec <- unique(box_def$strata_n_dt$ADP)
  
  # Omit ZERO and EM_TRW-GOA strata as we will not allocate to either via the proximity method
  box_def$strata_n_dt <- box_def$strata_n_dt[!(STRATA %like% "ZERO|EM_TRW")]
  box_def$dt_out <- box_def$dt_out[!(STRATA %like% "ZERO|EM_TRW")]
  box_def$box_smry <- box_def$box_smry[!(names(box_def$box_smry) %like% "ZERO|EM_TRW")]
  
  year_res <- vector(mode = "list", length = length(year_vec))
  
  for(i in year_vec) {
    
    box_def_sub.prox.range <- calculate_prox(
      box_def, sample_rate_vec = c(0.0001, seq(0.05, 1, by = 0.001)), omit_strata = "ZERO" 
    )$prox_dt[ADP == i]
    # Calculate index for each stratum
    box_def_sub.prox.range[
    ][, n := SAMPLE_RATE * STRATA_N
    ][, FPC := (STRATA_N - n) / STRATA_N
    ][, CV_SCALING := sqrt(FPC * (1/n))
    ][, INDEX := PROX * (1 - CV_SCALING)][INDEX < 0, INDEX := 0]
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
        index_cost <- calculate_cost(res, cost_params, allo_lst, max_budget) # this is the most this index would cost
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
        index_vec_rates_costs[c(index_near_budget - range_var, index_near_budget + 1)],
        function(x) x[strata_dt[k], on = c(box_def$params$stratum_cols), SAMPLE_RATE]
      )

      # Now, we go back calculating rates every 0.0001 here.
      prox_by_stratum <- calculate_prox(box_stratum, sample_rate_vec = seq(sample_range[1], sample_range[2], by = 0.0001))$prox_dt
      prox_by_stratum[
      ][, n := SAMPLE_RATE * STRATA_N
      ][, FPC := (STRATA_N - n) / STRATA_N
      ][, CV_SCALING := sqrt(FPC * (1/n))
      ][, INDEX := PROX * (1 - CV_SCALING)]
      prox_by_stratum_lst[[k]]  <- prox_by_stratum
      
    }
    prox_by_list_dt <- rbindlist(prox_by_stratum_lst)
    
    # Find range if INDEX that is common to all strata × ADP
    prox_by_list_dt[, as.list(setNames(range(INDEX), c("MIN", "MAX"))), by = c(stratum_cols)]
    
    index_range_afforded <- prox_by_list_dt[
    ][, .(MIN = min(INDEX), MAX = max(INDEX)), by = group_cols
    ][, .(MIN = max(MIN),  MAX = min(MAX)), by = year_col]
    
    prox_by_list_dt <- prox_by_list_dt[, .SD[between(INDEX, index_range_afforded$MIN, index_range_afforded$MAX )], by = c(stratum_cols)]
    
    # I can set index_interval to 0.0001 to really get the closest to affording the budget. Does take 10× longer...
    prox_index_search <- seq(round(index_range_afforded$MIN,3), round(index_range_afforded$MAX,3), by = index_interval)
    index_costs2 <- lapply(prox_index_search, function(x) {
      x1 <- data.table(INDEX = x)
      x2 <- prox_by_list_dt[, .SD[x1, on = .(INDEX), roll = "nearest"], by = c(stratum_cols)]
      stratum_column <- apply(x2[, ..stratum_cols], 1, paste, collapse = "-")
      x2$STRATUM_COL <- stratum_column
      x2
    })
    
    # Calculate the cost of each index
    index_cost_lst <- lapply(index_costs2, function(x) {
      calculate_cost(x, cost_params, allo_lst, max_budget)
    })
    # Grab only the final costs
    index_costs_vec <- sapply(index_cost_lst, "[[", "INDEX_COST")
    
    
    # Find the index that is closest to the budget
    closest_to_budget <- findInterval(budget, unlist(index_costs_vec))
    out <- index_costs2[[closest_to_budget]]
    out[, INDEX_COST := unlist(index_costs_vec)[closest_to_budget]]
    # Add the cost summaries as an attribute to the table
    setattr(out, "cost_summary", index_cost_lst[[closest_to_budget]])
    
    year_res[[which(year_vec == i)]] <- out
    
  }
  
  #' Return the allocated rates, collapsing the list of rates by year. If there is only one year (like for the ADP),
  #' grab the only item so that the cost_summary attribute is retained.
  if(length(year_res) > 1) rbindlist(year_res) else(year_res[[1]])
  
}


# Proximity
calculate_cost <- function(index_rates, cost_params, allo_lst, max_budget) {
  
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
          ob_cost_new(index_rates_sub[STRATA %like% "OB_"], cost_params, allo_lst$effort[STRATA %like% "OB_"]),
          emfg_cost( index_rates_sub[STRATA %like% "EM_HAL|EM_POT|EM_FIXED"], cost_params),
          emtrw_cost_new(cost_params)
        )
        index_rates_sub_res[, INDEX_COST := sum(OB_TOTAL, EMFG_TOTAL, EMTRW_TOTAL, na.rm = T)]
        # If the cost exceeds the specified maximum budget, don't bother continuing cost calculations
        if(index_rates_sub_res$INDEX_COST > max_budget) break 
        else (index_rates_lst[[i]] <- cbind(index_rates_sub_res, unique(index_rates_sub[, .(ADP, INDEX, I)])))
      }
      rbindlist(index_rates_lst)
    }
  ))
  
  # Remove copied objects
  rm(index_rates_copy,  allo_lst_effort)
  out
  
}


#' Calculate interspersion  * RENAME TO PROXIMITY*
calculate_prox <- function(box_res, sample_rate_vec, omit_strata = c(NULL)) {
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
  
  # For a range of sample rates, calculate the probably that a post-stratum would be near a sampled neighbor
  # (0-1), and then multiply it by that post-stratum's total weight of component trips centered on the post-stratum.
  
  # For each sample rate...
  prox_lst <- lapply(
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
  prox_lst <- lapply(prox_lst, function(z)  data.frame(GROUP_COLS = names(z), sum_pw = z))
  names(prox_lst) <- sample_rate_vec
  prox_dt <- rbindlist(prox_lst, idcol = "SAMPLE_RATE")[
  ][, SAMPLE_RATE := as.numeric(SAMPLE_RATE)
  ][, (group_cols) := tstrsplit(GROUP_COLS, split = "[.]")
  ][, GROUP_COLS := NULL]
  prox_dt[, (year_col) := lapply(.SD, as.integer), .SDcol = year_col]
  
  prox_dt <- box_res$strata_n_dt[
  ][prox_dt, on = group_cols
  ][, PROX := sum_pw / STRATA_N][]
  
  setcolorder(prox_dt, c("SAMPLE_RATE", group_cols, "PROX"))
  
  prox_res <- list(
    prox_dt = prox_dt,
    strata_n_dt = box_res$strata_n_dt,
    params = box_res$params
  )
  
  prox_res
  
}
#' TODO Are these other proximity/index functions needed?

# Calculate the interspersion = expected proportion of trips sampled or neighboring a sampled trip, 
# within strata given a vector of sampling rates
calculate_proximity_old <- function(box_res, sample_rate_vec, omit_strata = c(NULL)) {
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
  
  # For a range of sample rates, calculate the probably that a post-stratum would be near a sampled neighbor
  # (0-1), and then multiply it by that post-stratum's total weight of component trips centered on the post-stratum.
  
  # For each sample rate...
  prox_lst <- lapply(
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
  prox_lst <- lapply(prox_lst, function(z)  data.frame(GROUP_COLS = names(z), sum_pw = z))
  names(ispn_lst) <- sample_rate_vec
  prox_dt <- rbindlist(prox_lst, idcol = "SAMPLE_RATE")[
  ][, SAMPLE_RATE := as.numeric(SAMPLE_RATE)
  ][, (group_cols) := tstrsplit(GROUP_COLS, split = "[.]")
  ][, GROUP_COLS := NULL]
  prox_dt[, (year_col) := lapply(.SD, as.integer), .SDcol = year_col]
  
  prox_dt <- box_res$strata_n_dt[
  ][prox_dt, on = group_cols
  ][, PROX := sum_pw / STRATA_N][]
  
  setcolorder(prox_dt, c("SAMPLE_RATE", group_cols, "PROX"))
  
  prox_res <- list(
    prox_dt = prox_dt,
    strata_n_dt = box_res$strata_n_dt,
    params = box_res$params
  )
  
  prox_res
  
}


# Combines proximity and CV_scaling into an index. Allocates such that all strata have the same index, and determines cost.
#' *No longer used - this is done within allo_prox, using PROX = (1 - CV_SCALING)]. Remove?*
calculate_index <- function(prox_res, trip_cost_dt) {
  # ispn_res <- copy(box_mixed_bsai_goa_gs_insp); trip_cost_dt <- copy(trip_cost_dt_fg_combined_bsai_goa_cwb)
  
  group_cols <- c(prox_res$params$year_col, prox_res$params$stratum_cols)
  stratum_cols <- prox_res$params$stratum_cols
  year_col <- prox_res$params$year_col
  
  # Calculate index as INTERSPERSION / CV_SCALING
  x1 <- copy(prox_res$ispn_dt)[
  ][, n := SAMPLE_RATE * STRATA_N
  ][, FPC := (STRATA_N - n) / STRATA_N
  ][, CV_SCALING := sqrt(FPC * (1/n))
  ][, INDEX := PROX / CV_SCALING][]
  
  # Find range of INDEX that is common to all strata × ADP
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
    measure.vars = c("INDEX", "PROX", "CV_SCALING", "SAMPLE_RATE"))
  
  index_out <- list(rates = index_rates, melt = index_rates_melt, params = prox_res$params)
  index_out
}

# A quick function that searches for rates from the proximity allocation method given a budget.
prox_rates_from_budget <- function(index_res, budget) {
  # index_res <- copy(box_fixed_bsai_goa_gs_index); budget <- 4.5e6
  
  group_cols <- unname(unlist(index_res$params[c("year_col", "stratum_cols")]))
  budget_dt <- data.table(INDEX_COST = budget)
  index_res$rates[, .SD[budget_dt, on = .(INDEX_COST), roll = "nearest"], keyby = group_cols]
}

# Evaluation Functions -------------------------------------------------------------------------------------------------
#======================================================================================================================#


#' *===================*
#' *Everything below new*

#' This is a wrapper for `calculate_dmn_interspersion()` and `dmn_interspersion_smry` to summarize a variety of 
#' comparisons (i.e., OB to OB, OB to EM, OB to ZE, EM to EM). Used by `fixed_gear_EM_request_evaluation.R`
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
  
  dmn_insp_OB <- calculate_dmn_interspersion( #' [CHANGED: was calculate_dmn_interspersion4() ]
    box_def = box_def,
    selection_rates = selection_rates,
    acceptor_donor_lst = ob_adl
  )
  
  dmn_insp_nonOB <- calculate_dmn_interspersion( #' [CHANGED: was calculate_dmn_interspersion4() ]
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

#' Used by `dmn_interspersion_figs()` to combine comparisons of domain interspersion. 
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

#' *Everything above new*
#' *===================*

# Evaluate the overlap of observed trips to other pools of similar gear type
calculate_dmn_interspersion <- function(box_def, selection_rates, acceptor_donor_lst) {
  
  # TODO Manually add the fill for the pools!
  
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
    # i <- 4
    
    if( nrow(stratum_dt[ acceptor_donor_lst[[i]] ]) == 0 ) {
      # If there are no donors, skip
      out[[i]] <- NULL
    } else {
      
      # Identify the 'donor' stratum
      focus_stratum <- stratum_dt[i, ..stratum_cols]
      
      # Subset the data to include the acceptor and its donors, then split by domain
      focus_dmn_dt <- unique(rbind(
        dmn_dat[focus_stratum, on = stratum_cols],
        dmn_dat[stratum_dt[acceptor_donor_lst[[i]], ..stratum_cols], on = stratum_cols]
      ))
      if(is.null(dmn_cols)){
        focus_dmn_lst <- list(focus_dmn_dt)
      } else {
        focus_dmn_lst <- split(focus_dmn_dt, by = dmn_cols)
      }
      # focus_dmn_lst <- split(unique(rbind(
      #   dmn_dat[focus_stratum, on = stratum_cols],
      #   dmn_dat[stratum_dt[acceptor_donor_lst[[i]], ..stratum_cols], on = stratum_cols]
      # )), by = dmn_cols)
      
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

# A wrapper for calculate_realized_interspersion, but randomly samples to create new `monitored_trips` object
simulate_interspersion <- function(box_def, sample_rates, iter, seed, hex_smry = F) {
  # box_def <- copy(box_def.stratum); sample_rates <- copy(programmed_rates); iter <- 1; seed <- 12345
  # box_def <- copy(box_def.stratum_fmp); sample_rates <- copy(programmed_rates); iter <- 1000; i <- 1; seed <- 12345
  
  year_strata <- unlist(box_def$params[c("year_col", "stratum_cols")], use.names = F)
  domains <- box_def$params$dmn_cols
  sim_lst <- vector(mode = "list", length = iter)
  mon_lst <- vector(mode = "list", length = iter)  # A list of TRIP_IDs sampled
  
  set.seed(seed)
  
  # Iterate sampling of trips, then calculate the interspersion achieved from each iteration of sampling
  for(i in seq_len(iter)){
    
    if(i %% round(iter/10) == 0) cat(paste0(i, ", "))
    
    # Sample according to sample rates, 
    sample_lst <- apply(setorderv(sample_rates, year_strata), 1, function(x) runif(x[["STRATA_N"]]) < x[["SAMPLE_RATE"]])
    # Make a data.table of trips
    trips <- setorderv(unique(subset(box_def$og_data, select = c(year_strata, "TRIP_ID"))), year_strata)
    #trips$SAMPLE <- unlist(sample_lst)
    
    mon_lst[[i]] <- trips[unlist(sample_lst), TRIP_ID]
    sim_lst[[i]] <- calculate_realized_interspersion(box_def, trips[unlist(sample_lst)])
  } 
  
  sim_dt <- rbindlist(sim_lst, idcol = "ITER")
  
  # sim_dt[, .(MEAN = mean(INSP)), keyby = year_strata] 
  # Capture the domains as an attribute
  setattr(sim_dt, "year_strata_domains", c(year_strata, domains))
  setattr(sim_dt, "mon_lst", mon_lst)
  
  # If you want a hex-level summary of each, count the number of times each BOX was sampled
  if(hex_smry) {
    # For each simulation, total monitored/neighboring monitored trips for each HEX_ID
    cat("\nCompiling summary of HEX_IDs for spatial analyses...\n")
    
    #' TODO THIS IS WAY TOO SLOW!!! Takes almost a long as the simulations...
    # I should simply have lists for each stratum_year, list of HEX_ID, and vector of HEX_mon. Everything else I can do afterwards!
    
    #' *Faster version?*
    nbr_smry <- lapply(sim_lst, function(x) {
      # x <- sim_lst[[1]]
      mon_box.lst <- lapply(
        split(setkeyv(attr(x, "sampled_boxes"), year_strata), by = c(year_strata), keep.by = F), 
        unlist, use.names = F)
      # Identify neighbors of monitored trips
      # unique runs a little faster on integers rather than numeric. Should make `nbr_lst` integer...
      mon_nbr.lst <- lapply(mon_box.lst, function(x) unique(as.integer(unlist(box_def$nbr_lst[x]))))
    })
    
    # Compile results for each year_strata across iterations
    hex_smry_dt <- rbindlist(lapply(names(nbr_smry[[1]]), function(x) {
      # x <- year_strata.vec[[1]]
      cat(paste0(x, ", "))
      
      focus_stratum <- setnames(data.table(t(unlist(strsplit(x, split = "[.]")))), year_strata)
      focus_stratum[, ADP := as.integer(ADP)]
      focus_stratum_dt <- box_def$dt_out[focus_stratum, on = year_strata]
      
      # The
      rbindlist(lapply(
        lapply(nbr_smry, "[[", x),
        function(y) {
          focus_stratum_dt[, .(HEX_w = sum(BOX_w[BOX_ID %in% y])), keyby = c(year_strata, "HEX_ID")]
        }
      ), idcol = "ITER")
    }))
    
    setattr(sim_dt, "hex_smry", hex_smry_dt)
    
  }
  
  # Return the results
  sim_dt
}


# Function for getting percentile given distribution and realized rates
insp_percentile <- function(realized_insp, prog_sim, real_sim){
  # realized_insp <- copy(real_interspersion.stratum_fmp); prog_sim <- copy(sim.programmed.stratum_fmp); real_sim = copy(sim.realized.stratum_fmp)
  
  year_strata_domains <- attr(prog_sim, "year_strata_domains")
  
  out_lst <- vector(mode = "list", length = nrow(realized_insp))
  for(i in 1:length(out_lst)) {
    
    # Subset the data for each group
    dat_sub <- realized_insp[i,]
    # Define functions to calculate percentiles (feed in all simulated interspersion values from both distributions
    ecdf_fun.prog <- ecdf(prog_sim[dat_sub[, ..year_strata_domains], on = (year_strata_domains), INSP])
    ecdf_fun.real <- ecdf(real_sim[dat_sub[, ..year_strata_domains], on = (year_strata_domains), INSP])
    # Calculate the percentiles and send to output list
    out_lst[[i]] <- cbind(dat_sub, PERC_PROG = ecdf_fun.prog(dat_sub$INSP), PERC_REAL = ecdf_fun.real(dat_sub$INSP))
  }
  
  setorderv(rbindlist(out_lst), year_strata_domains)[]
  
}

dmn_insp_percentile <- function(realized_insp, prog_sim, real_sim){
  
  # realized_insp <- copy(test); prog_sim <- copy(dmn_insp.prog); real_sim <- copy(dmn_insp.real)
  year_pool_domains <- c("ADP", "POOL", "BSAI_GOA", "GEAR")
  
  out_lst <- vector(mode = "list", length = nrow(realized_insp))
  for(i in 1:length(out_lst)) {
    
    # Subset the data for each group
    dat_sub <- realized_insp[i,]
    # Define functions to calculate percentiles (feed in all simulated interspersion values from both distributions)
    ecdf_fun.prog <- ecdf(prog_sim$dim_insp.pool[dat_sub[, ..year_pool_domains], on = (year_pool_domains), INSP])
    ecdf_fun.real <- ecdf(real_sim$dim_insp.pool[dat_sub[, ..year_pool_domains], on = (year_pool_domains), INSP])
    # Calculate the percentiles and send to output list
    out_lst[[i]] <- cbind(dat_sub, PERC_PROG = ecdf_fun.prog(dat_sub$INSP), PERC_REAL = ecdf_fun.real(dat_sub$INSP))
  }
  
  setorderv(rbindlist(out_lst), year_pool_domains)[]
  
}

calculate_density <- function(sim_res, fill_color, adjust = NULL) {
  # sim_res <- copy(sim.programmed.stratum); fill_color <- "dodgerblue"
  # sim_res <- copy(sim.programmed.stratum_fmp); fil_color <- "dodgerblue"; adjust = rep(2, times = 24)
  
  year_strata_domains <- attr(sim_res, "year_strata_domains")
  domain_tbl <- setorderv(unique(subset(sim_res, select = year_strata_domains)), year_strata_domains)
  tail_color <- paste0(fill_color, "4")
  
  # Adjust the bandwidth. Null will set the bandwidth to the default, 1
  if(is.null(adjust)) adj <- rep(1, times = nrow(domain_tbl)) else {
    if(length(adjust) == 1) adj <- rep(adjust, times = nrow(domain_tbl)) else {
      if(length(adjust) != nrow(domain_tbl)) stop("'adjust' needs to be the same as `nrow(domain_tbl)`")
      adj <- adjust
    }
  }
  
  domain_density <- vector(mode = "list", length = nrow(domain_tbl))
  for(i in 1:nrow(domain_tbl)) {
    
    dmn_subset <- sim_res[domain_tbl[i,], on = year_strata_domains ]
    dmn_quantile <- quantile(dmn_subset$INSP, probs = c(0.025, 0.975))
    dmn_median <- quantile(dmn_subset$INSP, probs = 0.5)
    
    # Due to rounding errors, INSP can be above 1.0. Truncate such cases, but make a check that INSP wasn't incorrectly
    # calculated to be above 1.
    if(nrow(dmn_subset[INSP > 1 + 1e5])) stop( paste(unname(domain_tbl[i,]), collapse = " "), " had INSP values > 1!")
    dmn_subset[INSP > 1, INSP := trunc(INSP, 0)]
    
    # Run geom_density and extract the result
    dmn_density <- ggplot_build(
      #' *ORIGINAL* ggplot(dmn_subset, aes(x = INSP)) + geom_density(bounds = c(0, 1))
      
      ggplot(dmn_subset, aes(x = INSP)) + geom_density(bounds = c(0, 1), adjust = adj[i])
      
      
      
    )$data[[1]]
    dmn_density <- data.table(domain_tbl[i,], X = dmn_density$x, Y = dmn_density$y)
    
    # Identify the quantiles and assign them to each datapoint
    dmn_density$quant = factor(findInterval(dmn_density$X, dmn_quantile))
    # Define plotting fill colors and plotting groups
    dmn_density[, FILL := as.factor(fcase(quant %in% c(0, 2), tail_color, quant == 1, fill_color))]
    
    # Extract the cutoffs of each group
    cutoffs <- dmn_density[, .SD[c(1, .N),], keyby = c(year_strata_domains, "quant", "FILL")]
    # For the lower, add the x values of the tails 0 and 3
    quant_1.min <- cutoffs[quant == 0][.N][, ':=' (FILL = fill_color, quant = 1)][]
    quant_1.max <- cutoffs[quant == 2][1][, ':=' (FILL = fill_color, quant = 1)][]
    
    # Identify the median and split the middle.
    quant_1 <- rbind(quant_1.min, dmn_density[quant == 1], quant_1.max)
    quant_1$GROUP <- findInterval(quant_1$X, dmn_median) + 1
    quant_1_g2 <- copy(quant_1[GROUP == 1][.N])
    quant_1_g2[, GROUP := 2]
    quant_1 <- setorder(rbind(quant_1, quant_1_g2), X, GROUP)
    
    quant_0 <- dmn_density[quant == 0][, GROUP := 0][]
    quant_2 <- dmn_density[quant == 2][, GROUP := 3][]
    
    # Put it all back together. If distributions are so tight that there are not 3 quantile groups, omit empty groups.
    dmn_density <- rbind(quant_0, quant_1, quant_2)[!is.na(STRATA)]
    
    domain_density[[i]] <-  dmn_density
  }
  domain_density <- rbindlist(domain_density)
  domain_density[, GROUP := as.factor(GROUP)]
  
  setattr(domain_density, "year_strata_domains", year_strata_domains)
  domain_density[]
}

# Low-res AK map used in interspersion_plot. shp_land needs to be loaded first!
ak_low_res <- shp_land %>% st_simplify(dTolerance = 10000) %>% filter(!st_is_empty(shp_land)) %>% select(geometry)

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

# This version was created for the 2025 ADP to handle new PC contract information
ob_cost_new <- function(x, cost_params, allo_sub, sim = F) {

  x1 <- x[STRATA %like% "OB_"]
  
  # Count the number of days on each contract
  if(sim == T) {
    
    #' *If simulating trip selection, allo_sub must be subset to selected trips*
    
    #' *=====================================================*
    #' [TODO: Modified from* `bootstrap_allo()` *Should probably make this a function]
    
    ob_contract_dates <- trip_dates(allo_sub[STRATA %like% "OB_"])
    # For each trip, calculate the proportion of dates that occurred BEFORE the contract change date
    ob_contract_calc <- ob_contract_dates$dt[, .(TRIP_ID)]
    ob_contract_calc$PROP <- sapply(
      ob_contract_dates$lst, function(x) sum(x < as.integer(cost_params$OB$contract_change_date)) / length(x)
    )
    # Merge ADP, STRATA, and DAYS back in
    ob_contract_calc <- unique(allo_sub[STRATA %like% "OB_", .(ADP, STRATA, TRIP_ID, DAYS)])[
    ][ob_contract_calc, on = .(TRIP_ID)]
    # For each Stratum, calculate the number of days on contract period 1 and contract period 2
    ob_contract_calc <- ob_contract_calc[, .(
      CONTRACT_1 = sum(DAYS * PROP),
      CONTRACT_2 = sum(DAYS *(1 - PROP))
    ), keyby = .(ADP, STRATA)]
    #' *=====================================================*
    
    day_costs.contract <- rbind(
      ob_contract_calc[, .(PERIOD = 1, OB_DAYS = sum(CONTRACT_1))],
      ob_contract_calc[, .(PERIOD = 2, OB_DAYS = sum(CONTRACT_2))]
    )[cost_params$OB$contract_rates, on = .(PERIOD)]
    
  } else {
    
    #' *If NOT simulating trip selection, get the average from all trips based on trip duration*
    
    # Merge contract days with monitoring rate
    day_costs <- copy(allo_sub)[x1[, .(ADP, STRATA, MON_RATE)],  on = .(ADP, STRATA)]
    # Calculate expected number of days monitored on each contract, and merge in day rates on by contract period
    day_costs.contract <- rbind(
      day_costs[, .(PERIOD = 1, OB_DAYS = sum(CONTRACT_1 * MON_RATE))],
      day_costs[, .(PERIOD = 2, OB_DAYS = sum(CONTRACT_2 * MON_RATE))]
    )[cost_params$OB$contract_rates, on = .(PERIOD)]
 
  }
  
  # Account for days already on the contract
  day_costs.contract[PERIOD == 1, DAYS_ON_CONTRACT := cost_params$OB$current_contract_days]
  # Count number of base days and guaranteed days on each contract. Check to see if we allocated the minimum number
  # of base days on contract 1
  min_days_met <- day_costs.contract[PERIOD == 1, OB_DAYS + DAYS_ON_CONTRACT] >= cost_params$OB$contract_day_min
  
  day_costs.contract[
  ][PERIOD == 1, BASE_DAYS := min(cost_params$OB$contract_day_min - DAYS_ON_CONTRACT, OB_DAYS)
  ][PERIOD == 2, BASE_DAYS := min(cost_params$OB$contract_day_min, OB_DAYS)
  ][, OPT_DAYS := OB_DAYS - BASE_DAYS
  ][, DAY_COST := (Base_Day_Cost * BASE_DAYS) + (Optional_Day_Cost * OPT_DAYS)]
  # Total the day costs
  ob_day_cost <- sum(day_costs.contract$DAY_COST)
  
  # Estimate the total travel costs
  ob_travel_cost <- sum(day_costs.contract$OB_DAYS) * cost_params$OB$travel_cpd
  
  # Output a summary table
  data.table(
    OB_TOTAL = ob_day_cost + ob_travel_cost,
    OB_CPD = (ob_day_cost + ob_travel_cost) / sum(day_costs.contract$OB_DAYS),
    OB_DAYS = sum(day_costs.contract$OB_DAYS),
    OB_DAY_COST = ob_day_cost,
    OB_TRAVEL_COST = ob_travel_cost,
    OB_C1_BASE = day_costs.contract[PERIOD == 1, BASE_DAYS + DAYS_ON_CONTRACT],
    OB_C1_OPT = day_costs.contract[PERIOD == 1, OPT_DAYS],
    OB_C2_BASE = day_costs.contract[PERIOD == 2, BASE_DAYS],
    OB_C2_OPT = day_costs.contract[PERIOD == 2, OPT_DAYS]
  )
  
}


emfg_cost <- function(x, cost_params, sim = F) {
  # Requires ADP, STRATA, N, MON_RATE, MEAN_TRIP_DAYS, # of EM fixed-gear vessels
  # x <- copy(x2[['2022']]); 
  
  #' [Example:] x <- copy(em_fg_d); sim <- T
  
  emfg_v <- cost_params$EMFG$emfg_v
  cost_per_vessel <- cost_params$EMFG$emfg_nonamortized_cpv
  cost_per_review_day <-  cost_params$EMFG$emfg_review_cpd
  
  # subset to EM fixed-gear strata
  x1 <- x[STRATA %like% "EM_HAL|EM_POT|EM_FIXED"]
  if(nrow(x1) == 0) return(
    data.table(EMFG_TOTAL = 0, EMFG_CPD = NA_real_, EMFG_DAYS = 0, EMFG_BASE = 0)
  )
  
  # Total EM days to review
  # If simulating....
  if(sim == T) {
    
    emfg_days <- x1[, sum(d)] 
    
  } else {
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
  amortized_equipment_per_VY <- cost_params$EMTRW$amortized_equipment_per_VY   # Per (Vessel × Year) amortized EM equipment install costs for GOA-only vessels
  equipment_upkeep_per_VY    <- cost_params$EMTRW$equipment_upkeep_per_VY      # Per (Vessel × Year) EM equipment maintenance cost for GOA-only vessels
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

# Now that it is a carve-off and we total the costs in monitoring_costs.R, this function becomes very simple
emtrw_cost_new <- function(cost_params) {
  # Calculate review costs, based on the cost per review day and the number of days fished
  
  emtrw_data_cost <- cost_params$EMTRW$emtrw_data_cpd * cost_params$EMTRW$emtrw_sea_days
  
  out <- data.table(
    EMTRW_PLANT_OBS = cost_params$EMTRW$emtrw_summary |>
      _[Category %in% c("Observer Plant Day Costs", "Lodging", "Per Diem"), sum(Cost)],
    EMTRW_BASE = cost_params$EMTRW$emtrw_summary[Category == "Equipment Maintenance", Cost],
    EMTRW_DATA = emtrw_data_cost
  ) |>
    _[, EMTRW_TOTAL := cost_params$EMTRW$emtrw_total_cost][]
  setcolorder(out, c("EMTRW_TOTAL", "EMTRW_PLANT_OBS", "EMTRW_BASE", "EMTRW_DATA"))
  out
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
    # FIXME The ob_cost() model should be made logistic so CPD doesn't become low at extremely high volumes
    if(all(cost_res[[total_col]] > max_budget)) break() 
    else{
      out_lst[[i]] <- cbind(MON_RATE = rate_vec[i], cost_res)
    }
  }
  rbindlist(out_lst)
  
}

# These functions would work a lot faster with matrices rather than data.frames! A lot of simple calculations...
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
