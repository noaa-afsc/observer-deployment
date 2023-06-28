evaluate_trip_variance <- function(effort, trips_melt, trip_col = "TRIP_ID"){
  
  # Rename columns
  metrics <- copy(trips_melt)[, ':=' (TRIP_ID = get(trip_col), STRATA = strata_ID, METRIC = Metric, VALUE = Value)]
  
  # Convert optimization metrics to wide format
  metrics <- dcast(metrics[, .(TRIP_ID, STRATA, METRIC, VALUE)], TRIP_ID + STRATA ~ METRIC, value.var = "VALUE")
  
  # Join optimization metric values with the most recent year of trips
  data <- metrics[effort, on = .(TRIP_ID, STRATA), nomatch=0]
  
  # Total number of trips and number of monitored trips per stratum
  data[, ':=' (N = uniqueN(TRIP_ID), n = uniqueN(TRIP_ID[OBSERVED_FLAG == "Y"])), keyby = .(STRATA)]
  
  # Sum metrics by columns of interest
  data <- data[, .(chnk_psc = sum(chnk_psc), hlbt_psc = sum(hlbt_psc), discard = sum(discard), crab_psc = sum(crab_psc)), by = .(TRIP_ID, STRATA, OBSERVED_FLAG, N, n)]
  
  # Calculate trip-level mean and variance of monitored trips
  mean <- data[, lapply(.SD, function(x) mean(x[OBSERVED_FLAG=="Y"], na.rm = TRUE)), .SDcols = c("chnk_psc", "hlbt_psc", "discard", "crab_psc"), keyby = c("STRATA", "N", "n")]
  var <- data[, lapply(.SD, function(x) var(x[OBSERVED_FLAG=="Y"], na.rm = TRUE)), .SDcols = c("chnk_psc", "hlbt_psc", "discard", "crab_psc"), keyby = c("STRATA", "N", "n")]
  
  mean <- melt(mean, id.vars = c("STRATA", "N", "n"), variable.name = "metric", value.name = "mean")
  var <- melt(var, id.vars = c("STRATA", "N", "n"), variable.name = "metric", value.name = "var")
  
  data <- merge(mean, var, by = c("STRATA", "N", "n", "metric"))
  
  # Estimate the stratum-level mean and variance  
  data[, mean := N * mean]
  data[, var := N^2 * (N-n)/N * 1/n * var]
  
  # Return the mean, variance, and standard error for each metric
  return(data[, .(mean = sum(mean), variance = sum(var), standard_error = sqrt(sum(var))), keyby = .(metric)])
}