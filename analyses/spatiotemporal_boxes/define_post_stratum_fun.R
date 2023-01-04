# define_post_stratum_fun() is used to apply a post-stratum definition to fishing effort and determine which trips
# populate each post-stratum. Spacially, you can specify the cell size and the search radius of each cell. Temporally, 
# you specify the column name with the time groups and the degree of temporal overlap. You can also specify additional
# grouping variables.

define_poststrata_fun <- function(data, space, time, columns = NULL, year_strata, stata_area_sf = stat_area_sf) {
  # data <- copy(effort_target); space <- c(2e5, 2e5); time <- c("WEEK", 2); columns <- "TARGET"; year_strata <- c("ADP", "STRATA")
  # data <- copy(effort_target); space <- c(2e5, 2e5); time <- c("WEEK", 2); columns <- NULL; year_strata <- c("ADP", "STRATA")
  # data <- copy(effort); space <- c(2e5, 2e5); time <- c("WEEK", 2); columns <- c("TARGET", "GEAR"); year_strata <- c("ADP", "STRATA")

  # Initialize and error-check --------------------------------------------------------------------------------------

  win <- as.integer(time[2])
  
  # Make sure integer TRIP_ID, integer ADFG_STAT_AREA_CODE, and 'columns' are specified in the data
  if( length( intersect(colnames(data), c("TRIP_ID", "ADFG_STAT_AREA_CODE", columns))) !=  length(c("TRIP_ID", "ADFG_STAT_AREA_CODE", columns)) ) {
    stop("'data' must have columns 'TRIP_ID' and 'ADFG_STAT_AREA_CODE.")
  } else {
    if( all(lapply(data[, c("TRIP_ID", "ADFG_STAT_AREA_CODE")], class) != "integer")) {
      stop ("'TRIP_ID' and 'ADFG_STAT_AREA_CODE' must be of class integer.")
    }
  }
  # Remove any unused columns
  keep_cols <- c(year_strata, columns, "ADFG_STAT_AREA_CODE", time[1], "TRIP_ID")
  data <- unique(data[, ..keep_cols])
  
  # If 'columns' is not null, ensure that it is coded numerically. Factor and character classes are coded to integer.
  if( !is.null(columns) ) {
    
    if( any( !(sapply(columns, function(x) class(x) %in% c("integer", "numeric", "factor", "character")))) ) {
      stop("'column' classes must be of class integer, numeric, factor, or character.")
    }
    column_class <- sapply(data[, ..columns], class)
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
        columns[which(columns == i)] <- paste0(i, "_INT")
      }
    }
    if( nrow(data) != nrow(unique(data))) stop ("Number of rows changed - This shouldn't happen?")
  }

  # Convert ADFG to HEX_ID ------------------------------------------------------------------------------------------

  stat_area_lst <- stat_area_to_hex_fun(space[1], stat_area_sf)
  stat_area_dist_lst <- suppressWarnings(apply(
    X = round(st_distance(st_centroid(stat_area_lst$HEX_GEOMETRY))), 
    MARGIN = 1, 
    FUN = function(x) which(x <= space[2])))
  data <- unique(data.table(stat_area_lst$STAT_AREA_HEX_DF)[
  ][data, on = .(ADFG_STAT_AREA_CODE)
  ][, -"ADFG_STAT_AREA_CODE"])
  setcolorder(data, neworder = c(year_strata, columns, "HEX_ID", time[1]))
  setkeyv(data, cols = c(year_strata, columns, "HEX_ID", time[1]))
  
  # Define post-strata and identify their populations ---------------------------------------------------------------

  # Organize fishing effort into list of matrices. Split by stratum groupings (e.g., strata and year if present)
  data_lst <- lapply(
    X = split(x = data, by = year_strata, keep.by = F), 
    FUN = as.matrix)
  
  # Make the frequency table of each TRIP_ID (so that trips are properly split by 'columns' groupings.
  trip_id_mat <- do.call(rbind, lapply(
    data_lst,
    function(p) {
      trip_id_frq <- table(p[, "TRIP_ID"])
      matrix(
        c(as.integer(names(trip_id_frq)), trip_id_frq),
        ncol = 2, dimnames = list(NULL, c("TRIP_ID", "Freq")))
  }))
  
  # First, split matrices by 'colnames' if given
  if( !is.null(columns) ) {
    data_lst <- lapply(
      data_lst,
      function(x) lapply(
        split(x[, -which(colnames(x) %in% columns)], f = asplit(x[, columns, drop = F], 2), drop = T), 
        matrix, ncol = 3, dimnames = list(NULL, setdiff(colnames(x), columns)))
    )
  } else data_lst <- lapply(data_lst, list)
  
  # Testing:    x <- data_lst[[1]];  y <- x[[1]]; z <- y0[1,]
  
  # for each adp_year group 'x'
  out <- lapply(
    data_lst,
    function(x) {
      # for each 'columns' group 'y', if specified
      
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
      
      # Put the 'columns' names back in, using int_to_char_lst
      if( !is.null(columns) ) {
        x1 <- Map(
          function(m1, m2) lapply(m1, function(m3) cbind( m2[rep(1, times = nrow(m3)), , drop = F], m3)), 
          m1 = x1,
          m2 = lapply(
            strsplit(names(x1), split = "[.]"), 
            function(x) matrix(as.integer(x), nrow = 1, dimnames = list(NULL, columns) ))
        )
      }
      
      # Define PS_ID, or post-stratum ID, as a combination of the time+space post-stratum and any 'columns'
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
  
  # TODO Can I also merge in the weights for the trips that are centered in those post-strata?
  # I have to do this before I obliterate 
  # FIXME - double-check that the weights are not fucked up because of the 'columns' variable... I THINK THEY ARE

  
  # Create data.table output ----------------------------------------------------------------------------------------

  out_dt <- rbindlist(lapply(out, "[[", "dt"), idcol = "year_strata")
  out_dt[, (year_strata) := tstrsplit(year_strata, split = "[.]")]
  # If any year_strata columns were numeric, convert those back
  int_col <- names(which(sapply(out_dt[, ..year_strata], function(x) !any(grepl("[^0-9.]", x))) ))
  out_dt[, (int_col) := lapply(.SD, as.integer), .SDcols = int_col]
  # If 'columns' was specified, use  int_to_char_lst to revert
  if( !is.null(columns) ){
    for(i in seq_along(int_to_char_lst) ) {
      merge_col <- names(int_to_char_lst)[i]
      out_dt[, (merge_col) := int_to_char_lst[[i]][out_dt, merge_col, on = paste0(merge_col, "_INT"), with = F]][]
    }
  }
  out_dt[, c("year_strata", "GROUP", "TS_PS_ID", columns) := NULL]
  col_order <- c(year_strata, "PS_ID", gsub("_INT", "", columns), "HEX_ID", time[1])
  setcolorder(out_dt, col_order)
  setkeyv(out_dt, col_order)
  
  # Prepare final output --------------------------------------------------------------------------------------------

  list(
    ps_trip_id = lapply(out, "[[", "ps_trip_id"),
    W = lapply(out, "[[", "W"),
    dt = out_dt
  )
}
