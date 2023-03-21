# These are the functions from analyses/allocation_evaluation
# Some are further developed version of those in analyses/spatiotemporal_boxes

library(data.table)   # For data wrangling, very efficient with joins, especially rolling joins
library(dplyr)        # For data wrangling/piping with sf package
library(sf)           # For spatial statistics

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


# Low-res AK map used in interspersion_plot
ak_low_res <- shp_land  %>% st_simplify(dTolerance = 10000)


# TODO make the filter operate on stratum_cols and dmn_cols? But ignore FMP...
dmn_interspersion_plot <- function(dmn_ispn_res, strata, gear, year, type, map = F) {
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
