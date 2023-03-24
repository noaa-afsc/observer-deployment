# Functions for analyses/spatiotemporal_boxes
# Some of these functions work sequentially:
#    define_poststrata() -->  calculate_mean_prop_n() --> calculate_index()   # If using the gaps/CV_scaling allocation method 

# TODO Remove dependency on data.table? Some things can be easily removed, but joining may severerly suffer without it. 

library(data.table)   # TODO For data wrangling in some functions, but should be able to manage without it?
library(dplyr)        # For data wrangling/piping with sf package
library(sf)           # For spatial statistics

# stat_area_to_hex.R provides a function to use bin ADFG statistical areas within iso-area hex cells of a specified 
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

# define_post_strata() is used to apply a post-stratum definition to fishing effort and determine which trips
# populate each post-stratum. Spacially, you can specify the cell size and the search radius of each cell. Temporally, 
# you specify the column name with the time groups and the degree of temporal overlap. You can also specify additional
# grouping variables.
define_poststrata <- function(data, space, time, ps_cols = NULL, stratum_cols, stata_area_sf = stat_area_sf, geom = F) {
  # data <- copy(effort_target); space <- c(2e5, 2e5); time <- c("WEEK", 2); ps_cols <- "TARGET"; stratum_cols <- c("ADP", "STRATA")
  # data <- copy(effort_target); space <- c(2e5, 2e5); time <- c("WEEK", 2); ps_cols <- NULL; stratum_cols <- c("ADP", "STRATA")
  # data <- copy(effort); space <- c(2e5, 2e5); time <- c("WEEK", 2); ps_cols <- c("TARGET", "GEAR"); stratum_cols <- c("ADP", "STRATA")
  # data <- copy(val_2018_2021_dt[POOL != "ZE"]); space <- c(2e5, 2e5); time <- c("WEEK", 1); ps_cols <- NULL; stratum_cols <- c("ADP", "STRATA"); geom = T
  
  
  # NOTE: stratum_cols must include ADP! Typically includes STRATA, and works with FMP added as well
  
  #============================#
  # Initialize and error-check #
  #============================#
  win <- as.integer(time[2])   # Specify the size of the temporal window, convert from character to integer. 
  
  # Make sure integer TRIP_ID, integer ADFG_STAT_AREA_CODE, and 'ps_cols' are specified in the data
  if( length( intersect(colnames(data), c("TRIP_ID", "ADFG_STAT_AREA_CODE", ps_cols))) !=  length(c("TRIP_ID", "ADFG_STAT_AREA_CODE", ps_cols)) ) {
    stop(paste0("'data' must have columns 'TRIP_ID' and 'ADFG_STAT_AREA_CODE and ps_cols: ", paste(ps_cols, collapse = ", ")))
  } else {
    if( all(lapply(data[, c("TRIP_ID", "ADFG_STAT_AREA_CODE")], class) != "integer")) {
      stop ("'TRIP_ID' and 'ADFG_STAT_AREA_CODE' must be of class integer.")
    }
  }
  # Remove any unused columns
  keep_cols <- c(stratum_cols, ps_cols, "ADFG_STAT_AREA_CODE", time[1], "TRIP_ID")
  data <- unique(data[, ..keep_cols])      # TODO subsetting columns in base r with a data.frame is faster!
  
  # If 'ps_cols' is not null, ensure that it is coded numerically. Factor and character classes are coded to integer.
  if( !is.null(ps_cols) ) {
    
    if( any( !(sapply(ps_cols, function(x) class(x) %in% c("integer", "numeric", "factor", "character")))) ) {
      stop("'column' classes must be of class integer, numeric, factor, or character.")
    }
    column_class <- sapply(data[, ..ps_cols], class)
    col_to_integer <- names(column_class)[which(column_class %in% c("factor", "character"))]
    
    # If any columns need to be converted to integer, create conversion table for reversibility in the future
    if( length(col_to_integer) ) {
      int_to_char_lst <- setNames(vector(mode = "list", length = length(col_to_integer)), col_to_integer)
      for(i in col_to_integer) {
        int_to_char_element <- unique(data[, ..i])
        int_to_char_element[, paste0(i, "_INT") := .GRP, by = i]
        int_to_char_lst[[i]] <- int_to_char_element
        # Replace factor/character with integer
        data <- int_to_char_element[data, on = i][, (i) := NULL][]
        ps_cols[which(ps_cols == i)] <- paste0(i, "_INT")
      }
    }
    if( nrow(data) != nrow(unique(data))) stop ("Number of rows changed - This shouldn't happen?")
  }
  
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
  setcolorder(data, neworder = c(stratum_cols, ps_cols, "HEX_ID", time[1]))
  setkeyv(data, cols = c(stratum_cols, ps_cols, "HEX_ID", time[1]))
  
  # Define post-strata and identify their populations
  # Organize fishing effort into list of matrices. Split by stratum groupings (e.g., strata and year if present)
  data_lst <- lapply(
    X = split(x = data, by = stratum_cols, keep.by = F), 
    FUN = as.matrix)
  
  # Make the frequency table of each TRIP_ID (so that trips are properly split by 'ps_cols' groupings.
  trip_id_mat <- do.call(rbind, lapply(
    data_lst,
    function(p) {
      trip_id_frq <- table(p[, "TRIP_ID"])
      matrix(
        c(as.integer(names(trip_id_frq)), trip_id_frq),
        ncol = 2, dimnames = list(NULL, c("TRIP_ID", "Freq")))
    }))
  
  #==================================================#
  # Identify which TRIP_IDs are neighboring each box #
  #==================================================#
  
  # First, split matrices by 'ps_cols' if given
  if( !is.null(ps_cols) ) {
    data_lst <- lapply(
      data_lst,
      function(x) lapply(
        split(x[, -which(colnames(x) %in% ps_cols)], f = asplit(x[, ps_cols, drop = F], 2), drop = T), 
        matrix, ncol = 3, dimnames = list(NULL, setdiff(colnames(x), ps_cols)))
    )
  } else data_lst <- lapply(data_lst, list)
  
  # for each adp_year group 'x'
  out <- lapply(
    data_lst,
    function(x) {
      # for each 'ps_cols' group 'y', if specified
      
      x1 <- lapply(
        x,
        function(y) {
          
          # Get unique time and space post-strata
          y1 <- unique(y[, 1:2, drop = F])       # Columns: [1=HEX_ID], [2=WEEK]
          # for each time and space posts-stratum, find all trips that overlap
          y2 <- apply(
            X = y1, MARGIN = 1, simplify = F,     # Simplify = F makes sure the resulting vector is returned as one row
            function(z)  {
              # Identify trips in each post-stratum: Match HEX_ID and search for WEEK with -2 to +2 search range
              # Return result with the post-stratum's identifying HEX_ID and WEEK (y)
              z1 <- y[(y[,1] %in% stat_area_dist_lst[[ z[1] ]]) & (y[,2] %in% (z[2] + -win:win)), , drop = F]
              # # For trips centered in the post-strata, sum up the weight (reciprocal of freq) of the trips
              z2 <- sum(1/trip_id_mat[match(z1[z1[,1] == z[1] & z1[,2] == z[2], , drop = F][, 3], trip_id_mat[, 1]), 2])
              list(
                ps_trip_id = z1,
                W = z2
              ) 
            }
          )
          
          # FIXME
          # y2[[1]] and y3[[1]] have no rows! --------------------------------------------------------------------------------------
          if(F) {
            y1[[1]]  # This is NA, meaning no 
            y1[[2]]
          }
          
          
          # Flatten list so that each ADP.STRATA is a matrix of columns PS_ID, HEX_ID, WEEK, TRIP_ID
          y3 <- lapply(y2, "[[", "ps_trip_id")   # Extract ps_trip_id objects
          y4 <- lapply(y2, "[[", "W")   # Extract ps_trip_id objects
          
          # Flatten again, giving an integer identifier of time+space post-strata
          list(
            ts_ps_trip_id = do.call(rbind, Map(
              function(m1, m2) cbind(TS_PS_ID = m1, m2),
              m1 = seq_along(y3),
              m2 = y3
            )), 
            W = do.call(rbind, Map(
              function(m1, m2) cbind(TS_PS_ID = m1, W = m2),
              m1 = seq_along(y4),
              m2 = y4
            ))
          )
        }
      )
      
      # Put the 'ps_cols' names back in, using int_to_char_lst
      if( !is.null(ps_cols) ) {
        x1 <- Map(
          function(m1, m2) lapply(m1, function(m3) cbind( m2[rep(1, times = nrow(m3)), , drop = F], m3)), 
          m1 = x1,
          m2 = lapply(
            strsplit(names(x1), split = "[.]"), 
            function(x) matrix(as.integer(x), nrow = 1, dimnames = list(NULL, ps_cols) ))
        )
      }
      
      # Define PS_ID, or post-stratum ID, as a combination of the time+space post-stratum and any 'ps_cols'
      if( is.null(names(x1)) ) group <- 1 else group <- seq_along(names(x1))
      x2 <- lapply(
        list(
          ps_trip_id = do.call(rbind, Map(
            function(m1, m2) cbind(GROUP = m1, m2),
            m1 = group,
            m2 = lapply(x1, "[[", "ts_ps_trip_id")
          )),
          W = do.call(rbind, Map(
            function(m1, m2) cbind(GROUP = m1, W = m2),
            m1 = group,
            m2 = lapply(x1, "[[", "W")
          ))
        ),
        function(x) cbind(PS_ID = as.integer(interaction(x[, "GROUP"], x[,"TS_PS_ID"], drop = T, lex.order = T)), x)
      )
      
      # Remove unneeded columns from matrices and return dt with full dataset
      list(
        ps_trip_id = x2[[1]][, which(colnames(x2[[1]]) %in% c("PS_ID", "TRIP_ID")), drop = F],
        W = x2[[2]][, which(colnames(x2[[2]]) %in% c("PS_ID", "W")), drop = F],
        dt = as.data.table(x2[[1]])
      )
    }
  )
  
  #==========================#
  # Create data.table output #
  #==========================#
  
  out_dt <- rbindlist(lapply(out, "[[", "dt"), idcol = "stratum_cols")
  out_dt[, (stratum_cols) := tstrsplit(stratum_cols, split = "[.]")]
  # If any stratum_cols columns were numeric, convert those back
  int_col <- names(which(sapply(out_dt[, ..stratum_cols], function(x) !any(grepl("[^0-9.]", x))) ))
  out_dt[, (int_col) := lapply(.SD, as.integer), .SDcols = int_col]
  # If 'ps_cols' was specified, use  int_to_char_lst to revert
  if( !is.null(ps_cols) ){
    for(i in seq_along(int_to_char_lst) ) {
      merge_col <- names(int_to_char_lst)[i]
      out_dt[, (merge_col) := int_to_char_lst[[i]][out_dt, merge_col, on = paste0(merge_col, "_INT"), with = F]][]
    }
  }
  out_dt[, c("stratum_cols", "GROUP", "TS_PS_ID", ps_cols) := NULL]
  col_order <- c(stratum_cols, "PS_ID", gsub("_INT", "", ps_cols), "HEX_ID", time[1])
  setcolorder(out_dt, col_order)
  setkeyv(out_dt, col_order)
  
  #======================#
  # Prepare final output #
  #======================#

  out_lst <- list(
    ps_trip_id = lapply(out, "[[", "ps_trip_id"),
    W = lapply(out, "[[", "W"),
    dt = out_dt,
    strata_N_dt = data[, .(N = uniqueN(TRIP_ID)), keyby = stratum_cols]
  )
  
  # 'geom' is set to T when you want geometries and a data set ready for plotting. Be dafault this is set to F because
  # it may be time consuming and not necessary in some applications.
  if(geom == T) {
    
    # Count of neighbors
    geom_nbr <- out_dt[, .(PS_nbr = uniqueN(TRIP_ID)), by = c(stratum_cols, "PS_ID")]
    # Count of trips centered in each PS_ID
    geom_n <- out_dt[
    ][, .(PS_n = uniqueN(TRIP_ID)), keyby = c(stratum_cols, "HEX_ID", "WEEK")
    ][, PS_ID := rleid(HEX_ID, WEEK), by = c(stratum_cols)][]
    # Weight of trips centered in each PS_ID
    geom_w <- rbindlist(lapply(lapply(out, "[[", "W"), as.data.table), idcol = "STRATUM_COLS")[
    ][, (stratum_cols) := tstrsplit(STRATUM_COLS, split = "[.]")
    ][, STRATUM_COLS := NULL][, ADP := as.integer(ADP)][]
    # Combine
    geom_dt <- geom_w[geom_nbr, on = c(stratum_cols, "PS_ID")][geom_n, on = c(stratum_cols, "PS_ID")]
    setcolorder(geom_dt, c(stratum_cols, "PS_ID", "HEX_ID", "WEEK", "PS_n", "W", "PS_nbr" ))
    # Merge in geometry
    geom_sf <- merge(stat_area_lst$HEX_GEOMETRY, geom_dt, on = .(HEX_ID))
    # Add to output object
    
    out_lst$geom <- geom_sf
  }

  setattr(out_lst, "stratum_cols", stratum_cols)
  out_lst
}


# A newer, slightly better version, that keeps PS_ID straight? define_postrata() didnt seem to do this correctly
define_poststrata_geom <- function(data, space, time, year_col, stratum_cols, ps_cols = NULL, stata_area_sf = stat_area_sf, geom = F) {
  # data <- copy(test); space <- c(2e5, 2e5); time <- c("WEEK", 1); year_col <- "ADP"; ps_cols <- NULL; stratum_cols <- c("STRATA"); geom = T; acceptor <- NULL
  
  # This function allows for multiple years of data to be defined, and just needs 'year_col' to be specified so that the
  # definitions can be done separately. The data's hierarchy should be:
  # [year_col] -> [stratum_cols] -> [ps_cols if specified] -> [time x space]
  
  # NOTE: stratum_cols typically includes just 'STRATA', but can also include 'FMP' if you want to subdivide.
  
  #============================#
  # Initialize and error-check #
  #============================#
  win <- as.integer(time[2])   # Specify the size of the temporal window, convert from character to integer. 
  
  # Make sure integer TRIP_ID, integer ADFG_STAT_AREA_CODE, and 'ps_cols' are specified in the data
  if( length( intersect(colnames(data), c("TRIP_ID", "ADFG_STAT_AREA_CODE", ps_cols))) !=  length(c("TRIP_ID", "ADFG_STAT_AREA_CODE", ps_cols)) ) {
    stop(paste0("'data' must have columns 'TRIP_ID' and 'ADFG_STAT_AREA_CODE and ps_cols: ", paste(ps_cols, collapse = ", ")))
  } else {
    if( all(lapply(data[, c("TRIP_ID", "ADFG_STAT_AREA_CODE")], class) != "integer")) {
      stop ("'TRIP_ID' and 'ADFG_STAT_AREA_CODE' must be of class integer.")
    }
  }
  # Remove any unused columns
  keep_cols <- c(year_col, stratum_cols, ps_cols, "ADFG_STAT_AREA_CODE", time[1], "TRIP_ID")
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
  # TODO define 'ts_cols" here as ts_cols <- c("HEX_ID", time[1])
  setcolorder(data, neworder = c(year_col, stratum_cols, ps_cols, "HEX_ID", time[1]))
  setkeyv(data, cols = c(year_col, stratum_cols, ps_cols, "HEX_ID", time[1]))
  
  # TODO should I combine year_col, stratum_cols, and ps_cols here? could remove one step of nesting...
  group_cols <- c(year_col, stratum_cols, ps_cols)
  
  # Specify Box Identifiers 
  data[, PS_ID := rleidv(.SD, which(colnames(.SD) %in% c("HEX_ID", time[1]))), by = group_cols]
  
  # Separate data into list according to group_cols
  data_lst <- lapply(
    X = split(x = data, by = group_cols, keep.by = F), 
    FUN = as.matrix)
  
  # Make the frequency table of each TRIP_ID (so that trips are properly split by WEEK, time[1], and 'ps_cols' groupings.
  trip_id_mat <- do.call(rbind, lapply(
    data_lst,
    function(p) {
      trip_id_frq <- table(p[, "TRIP_ID"])
      matrix(
        c(as.integer(names(trip_id_frq)), trip_id_frq),
        ncol = 2, dimnames = list(NULL, c("TRIP_ID", "Freq")))
    }))
  
  #==================================================#
  # Identify which TRIP_IDs are neighboring each box #
  #==================================================#
  
  out <- lapply(
    data_lst,
    function(x) {
      # x <- data_lst[[1]]
      
      # get all unique time and space post-strata
      x1 <- unique(x[, 1:2, drop = F])   # Columns: [1=HEX_ID], [2=WEEK]
      
      # for each time and space posts-stratum, find all trips that overlap
      x2 <- apply(
        X = x1, MARGIN = 1, simplify = F, 
        function(y) {
          # y <- x1[1, , drop = F]
          # Identify trips in each post-stratum: Match HEX_ID and search for WEEK with -2 to +2 search range
          # Return result with the post-stratum's identifying HEX_ID and WEEK (y)
          y1 <- x[(x[,1] %in% stat_area_dist_lst[[ y[1] ]]) & (x[,2] %in% (y[2] + -win:win)), , drop = F]
          # # For trips centered in the post-strata, sum up the weight (reciprocal of freq) of the trips
          y2 <- sum(1/trip_id_mat[match(y1[y1[,1] == y[1] & y1[,2] == y[2], , drop = F][, 3], trip_id_mat[, 1]), 2])
          list(
            ps_trip_ids = y1,
            W = y2
          ) 
        }
      )
      
      x2
      
    }
  )
  
  # Create out_dt object, with group_cols, BOX_ID, ts_col, and TRIP_ID
  out_dt <- rbindlist(
    lapply(
      out, 
      function(x) rbindlist(lapply(lapply(x, "[[", "ps_trip_ids"), as.data.table), idcol = "OG_PS_ID")
    ),
    idcol = "GROUP_COLS"
  )
  out_dt[, (group_cols) := tstrsplit(GROUP_COLS, split = "[.]")][, GROUP_COLS := NULL]
  setcolorder(out_dt, c(group_cols, "PS_ID", "HEX_ID", time[1], "TRIP_ID"))
  int_cols <- c(year_col, "PS_ID", "HEX_ID", time[1], "TRIP_ID")
  out_dt[, (int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]
  
  #======================#
  # Prepare final output #
  #======================#
  
  out_lst <- list(
    ps_trip_id = lapply(out, function(x) lapply(x, "[[", "ps_trip_ids")),
    W = lapply(out, function(x) lapply(x, "[[", "W")),
    dt = out_dt,
    strata_N_dt = data[, .(N = uniqueN(TRIP_ID)), keyby = stratum_cols],
    og_dt = data
  )
  
  if(geom == T){
    
    # Create out_ts, which is ready for to plot with BOX_ID level-summaries
    # counts of trips centered in each box. Also shows which HEX_ID x time[1] correspond to 'BOX_ID'
    ps_n <- data[, .(PS_n = uniqueN(TRIP_ID)), keyby = c(group_cols, "HEX_ID", time[1], "PS_ID")]
    # Count neighbors. Use OG_PS_ID, which is the name of the PS_ID, whereas the existing PS_ID is where TRIP_ID is from!
    ps_nbr <- out_dt[, .(PS_nbr = uniqueN(TRIP_ID)), keyby = c(group_cols, "OG_PS_ID")]
    setnames(ps_nbr, "OG_PS_ID", "PS_ID")
    # Get the weight of each PS_ID
    ps_w <- rbindlist(lapply(out, function(x) data.table(W = unlist(lapply(x, "[[", "W")))), idcol = "GROUP_COLS")
    ps_w[, PS_ID := seq.int(.N), by = .(GROUP_COLS)][, (group_cols) := tstrsplit(GROUP_COLS, split = "[.]")
    ][, GROUP_COLS := NULL][, (year_col) := lapply(.SD, as.integer), .SDcols = year_col]
    # Merge, and also merge with the geometry data
    geom_sf <- merge(
      stat_area_lst$HEX_GEOMETRY,
      ps_n[ps_nbr, on = c(group_cols, "PS_ID")][ps_w, on = c(group_cols, "PS_ID")],
      on = "HEX_ID"
    )
    
    out_lst$geom <- geom_sf
  }
  
  setattr(out_lst, "stratum_cols", stratum_cols)
  out_lst
  
}

#===========================================#
# TEST TO SEE IF IT IS WORKING CORRECTLY ####
#===========================================#
# focus on 2021 HAL HEX_ID==143 WEEK==25

if(F) {
  
  geom_sf %>% filter(ADP==2021 & STRATA=="HAL" & WEEK==25 & HEX_ID==143)  
  
  
  cells_only <- geom_sf %>% filter(ADP==2021 & STRATA == "HAL" & WEEK%in%24:26) %>% select(HEX_ID, geometry) %>% unique()
  plot_style <- list(
    theme(axis.title=element_blank()), 
    geom_sf_text(data = cells_only, aes(label = HEX_ID), size = 2, nudge_y = -5e4, color = "blue")
  )
  hal_sub <- geom_sf %>% filter(ADP==2021 & STRATA == "HAL" & WEEK%in%24:26)
  p1 <- ggplot(hal_sub) + facet_grid(.~WEEK) + geom_sf() + geom_sf_text(aes(label = PS_n)) + plot_style 
  p2 <- ggplot(hal_sub) + facet_grid(.~WEEK) + geom_sf() + geom_sf_text(aes(label = PS_nbr)) + plot_style
  grid.arrange(p1, p2, ncol = 1)
  
  # FIXME even the weight is missing! did any trip actually fish here or is the trip only here because it was created from
  # the post-stratum generation process?
  # I need to create an object that has just hex_id and weeks of trips where fishing ACTUALLY occured
  # Then another object that represents where that data can go
  # And when I compare the data to fishing effort, I remove any data cells without any effort.
  # This should work well for the OB-EM and OB-ZE comparions as well!
  
  geom_dt[ADP==2021 & STRATA=="HAL" & WEEK==25 & HEX_ID==143]
  
  #===========================================#
  
}
  
# Helper function used in spatiotemporal_boxes_analysis.R
# TODO should be able to use this in mean_prop_n as well?
# Quickly counts the number of trips per box
count_trips_per_cell <- function(x) {
  stratum_cols <- attr(x, "stratum_cols")
  rbindlist(
    lapply(x$ps_trip_id, function(y) as.data.table(y)[, .(CELL_N = uniqueN(TRIP_ID)), by = PS_ID]), 
    idcol = "stratum_cols")
}

# Calculates the expected proportion of trips sampled or near a sampled neighbor in a stratum x year
calculate_mean_prop_n <- function(ps_res, sample_rate_vec) {
  # Collapse PS total weights into a dt.
  stratum_cols <- attr(ps_res, "stratum_cols")
  
  ps_w_dt <- rbindlist(
    lapply(ps_res$W, as.data.table),
    idcol = "stratum_cols"                 
  )[, (stratum_cols) := tstrsplit(stratum_cols, split = "[.]")
  ][, ADP := as.integer(ADP)
  ][, stratum_cols := NULL][]
  
  # Count trips in post-strata, combine with weights and STRATA N
  ps_smry_dt <- ps_res$dt[, .(n = length(unique(TRIP_ID))), keyby = c(stratum_cols, "PS_ID") ]
  ps_smry_dt[, N := ps_res$strata_N_dt[ps_smry_dt, N, on = stratum_cols]]
  ps_smry_dt[, W := ps_w_dt[ps_smry_dt, W, on = c(stratum_cols, "PS_ID")]]
  
  # For for a range of sample rates, calculate the probably that a post-stratum would be near a sampled neighbor
  # (0-1), and then multiply it by that post-stratum's total weight of component trips centered on the post-stratum.
  ps_prop_n <- lapply(
    sample_rate_vec,
    # For each sample rate 'x'...
    function(x) {
      x1 <- copy(ps_smry_dt)
      x1[, p := 1 - ((1 - x)^n)]   # Calculate the probability that the post-stratum will be near a sampled neighbor
      x1[, .(sum_W_p = sum(W * p)), by = c(stratum_cols, "N")][   # Multiply the probability by the weight (number of component trips centered in the box)
      ][, MEAN_PROP_N_IN_SAMPLED_PS := sum_W_p / N][]
    }
  )
  names(ps_prop_n) <- sample_rate_vec
  ps_prop_n <- rbindlist(ps_prop_n, idcol = "SAMPLE_RATE")[, SAMPLE_RATE := as.numeric(SAMPLE_RATE)][]
  setattr(ps_prop_n, "stratum_cols", stratum_cols)
  ps_prop_n
}

# Calculate Index : this function calculates an index that is MEAN_PROP_N / CV_SCALING, which balances the spatiotemporal
# proximity of trips in a stratum with the number of trips in the stratum. Strata with trips that are more distance in
# time and space need more monitoring and strata with fewer trips need more monitoring. The CV_SCALING metric prevents
# small strata that are tightly clumped in space and time from being allocated very low sampling rates that result in
# low sample sizes that ultimately result in poor CVs. 
# This function requires trip costs to determine what rates are afforded while keeping this index constant for all strata.

calculate_index <- function(x, costs){
  # x <- copy(s175_1_tW_1); costs <- copy(trip_cost_dt)
  # x <- copy(s175_2_tW_2); costs <- copy(trip_cost_dt)
  
  # TODO check to make sure the 'costs' object is not missing before doing anything else!
  
  stratum_cols <- attr(x, "stratum_cols")
  
  x1 <- copy(x)
  
  # View 'allocation-background.html' document in 2024_ADP/analyses/allocation_background
  x1[, n := SAMPLE_RATE * N]
  x1[, FPC := (N - n) / N]
  x1[, CV_SCALING := sqrt(FPC * (1/n))]  # As long as n > 1 then CV_scaling should be between 0 and 1
  x1[, INDEX := MEAN_PROP_N_IN_SAMPLED_PS / CV_SCALING]
  
  # Find range of INDEX that is similar to all strata x ADP
  x2 <- x1[
  ][, .(MIN = min(INDEX), MAX = max(INDEX)), by = stratum_cols
  ][, .(MIN = max(MIN), MAX = min(MAX)), by = .(ADP)]
  
  # Subset each year by the index range
  x1 <- do.call(rbind, apply(x2, MARGIN = 1, function(y) {
    x1[ADP == y[["ADP"]]][data.table::between(INDEX, y[["MIN"]], y[["MAX"]])]
  }))
  
  # For each ADP year, create a vector from the minimum to maximum index range
  index_vectors <- apply(x2, 1, function(y) seq(y[2], y[3], by = 0.01))
  
  # For each of those indices, find the rates required of all the STRATA to achieve those indices
  # TODO Should be able to do this by ADP x STRATA (something like dt[, .SD[z1, on = .(INDEX, roll = T)], by = .(ADP, STRATA)])
  # TODO This takes a while - try to speed this up! Is 0.01 increment for index vectors needlessly small?
  
  index_rates <- rbindlist(Map(
    function(y1, y2) {
      rbindlist(lapply(y1, function(z) {
        z1 <- data.table(INDEX = z)
        y2[, .SD[z1, on = .(INDEX), roll = "nearest"], keyby = stratum_cols]
      }))
    },
    y1 = index_vectors,
    y2 = split(x1, by = "ADP")
  ))
  
  # Total costs of affording each index, then plot how all variables change with increased budget
  index_rates[, CPT := trip_cost_dt[index_rates, CPT, on = .(ADP, STRATA)]]   # Merge in stratum-specific trip costs
  index_rates[, INDEX_COST := sum(CPT * n), by = .(ADP, INDEX)]               # Total stratum costs by ADP and INDEX
  index_rates_melt <- melt(index_rates, id.vars = c(stratum_cols, "N", "INDEX_COST"), measure.vars = c("INDEX", "MEAN_PROP_N_IN_SAMPLED_PS", "CV_SCALING", "SAMPLE_RATE"))
  
  return(
    setattr(
      list(rates = index_rates, melt = index_rates_melt), 
      "stratum_cols", stratum_cols
    )
  )
  
}

# Allocate based on gaps only (NOT CV_SCALING)
calculate_gaps <- function(x, costs){
  # x <- copy(s175_1_tW_1); costs <- copy(trip_cost_dt)
  # x <- box_def_mean_prop_n; costs <- copy(trip_cost_dt)
  # x <- copy(box_def_with_strata_mean_prop_n); costs <- copy(trip_cost_dt)
  
  # TODO check to make sure the 'costs' object is not missing before doing anything else!
  
  stratum_cols <- attr(x, "stratum_cols")
  
  x1 <- copy(x)
  
  # View 'allocation-background.html' document in 2024_ADP/analyses/allocation_background
  x1[, n := SAMPLE_RATE * N]
  x1[, FPC := (N - n) / N]
  x1[, CV_SCALING := sqrt(FPC * (1/n))]  # As long as n > 1 then CV_scaling should be between 0 and 1
  x1[, INDEX := MEAN_PROP_N_IN_SAMPLED_PS]   #for this function, INDEX is solely MEAN_PROP_N (proximity), excluding CV
  
  # Find range of INDEX that is similar to all strata x ADP
  x2 <- x1[
  ][, .(MIN = min(INDEX), MAX = max(INDEX)), by = stratum_cols
  ][, .(MIN = max(MIN), MAX = min(MAX)), by = .(ADP)]
  
  # Subset each year by the index range
  x1 <- do.call(rbind, apply(x2, MARGIN = 1, function(y) {
    x1[ADP == y[["ADP"]]][data.table::between(INDEX, y[["MIN"]], y[["MAX"]])]
  }))
  
  # For each ADP year, create a vector from the minimum to maximum index range
  index_vectors <- apply(x2, 1, function(y) seq(y[2], y[3], by = 0.001))
  
  # For each of those indices, find the rates required of all the STRATA to achieve those indices
  # TODO Should be able to do this by ADP x STRATA (something like dt[, .SD[z1, on = .(INDEX, roll = T)], by = .(ADP, STRATA)])
  # TODO This takes a while - try to speed this up! Is 0.01 increment for index vectors needlessly small?
  
  index_rates <- rbindlist(Map(
    function(y1, y2) {
      rbindlist(lapply(y1, function(z) {
        z1 <- data.table(INDEX = z)
        y2[, .SD[z1, on = .(INDEX), roll = "nearest"], keyby = stratum_cols]
      }))
    },
    y1 = index_vectors,
    y2 = split(x1, by = "ADP")
  ))
  
  # Total costs of affording each index, then plot how all variables change with increased budget
  index_rates[, CPT := trip_cost_dt[index_rates, CPT, on = .(ADP, STRATA)]]   # Merge in stratum-specific trip costs
  index_rates[, INDEX_COST := sum(CPT * n), by = .(ADP, INDEX)]               # Total stratum costs by ADP and INDEX
  index_rates_melt <- melt(index_rates, id.vars = c(stratum_cols, "N", "INDEX_COST"), measure.vars = c("MEAN_PROP_N_IN_SAMPLED_PS", "SAMPLE_RATE"))
  
  return(
    setattr(
      list(rates = index_rates, melt = index_rates_melt), 
      "stratum_cols", stratum_cols
    )
  )
  
}

# Calculates the expected proportion of boxes sampled or near a sampled box in a stratum x year
calculate_mean_prop_box <- function(ps_res, sample_rate_vec) {
  # ps_res <- copy(box_def); 
  
  # Collapse PS total weights into a dt.
  stratum_cols <- attr(ps_res, "stratum_cols")
  
  # Count trips in post-strata, combine with weights and STRATA N
  ps_smry_dt <- ps_res$dt[, .(n = length(unique(TRIP_ID))), keyby = c(stratum_cols, "PS_ID") ]
  ps_smry_dt[, N := ps_res$strata_N_dt[ps_smry_dt, N, on = stratum_cols]]
  
  # For for a range of sample rates, calculate the probably that a post-stratum would be near a sampled neighbor
  # (0-1), and then multiply it by that post-stratum's total weight of component trips centered on the post-stratum.
  ps_prop_box <- lapply(
    sample_rate_vec,
    # For each sample rate 'x'...
    function(x) {
      x1 <- copy(ps_smry_dt)
      x1[, p := 1 - ((1 - x)^n)]   # Calculate the probability that the post-stratum will be near a sampled neighbor
      x1[, .(MEAN_PROP_BOX = sum(p)/.N), by = c(stratum_cols, "N")]   # Multiply the probability by the weight (number of component trips centered in the box)
    }
  )
  names(ps_prop_box) <- sample_rate_vec
  ps_prop_box <- rbindlist(ps_prop_box, idcol = "SAMPLE_RATE")[, SAMPLE_RATE := as.numeric(SAMPLE_RATE)][]
  setattr(ps_prop_box, "stratum_cols", stratum_cols)
  ps_prop_box
}



