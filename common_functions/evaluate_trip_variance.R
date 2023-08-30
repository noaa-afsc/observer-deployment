evaluate_trip_variance <- function(effort, work.data, trips_melt, budget, rates, stratification, allocation, effort_wd_join = "wd_TRIP_ID"){
  
  # Rename columns
  metrics <- trips_melt[, .(TRIP_ID, METRIC = Metric, VALUE = Value)]
  
  # Convert optimization metrics to wide format
  metrics <- dcast(metrics[, .(TRIP_ID, METRIC, VALUE)], TRIP_ID ~ METRIC, value.var = "VALUE")
  
  # Get the original monitoring status of trips in the effort object
  wd <- unique(work.data[, .(TRIP_ID, OBSERVED_FLAG)])
  colnames(wd)[colnames(wd) == "TRIP_ID"] <- effort_wd_join
  effort <- wd[effort, on = c(effort_wd_join)]
  
  # Join optimization metric values with the most recent year of trips
  colnames(metrics)[colnames(metrics) == "TRIP_ID"] <- effort_wd_join
  data <- metrics[effort, on = c(effort_wd_join)]
  
  # Total number of trips and expected number of monitored trips per stratum
  data <- unique(rates[[paste(stratification, allocation, sep = ".")]][BUDGET == budget, .(ADP, STRATUM_COL, N = STRATA_N, n)])[data, on = .(ADP, STRATUM_COL), nomatch=0]
  
  # Sum metrics by columns of interest
  data <- data[, .(chnk_psc = sum(chnk_psc), hlbt_psc = sum(hlbt_psc), discard = sum(discard), crab_psc = sum(crab_psc)), by = .(TRIP_ID, STRATUM_COL, OBSERVED_FLAG, N, n)]
  
  # Calculate trip-level mean and variance of monitored trips
  mean <- data[, lapply(.SD, function(x) mean(x[OBSERVED_FLAG=="Y"], na.rm = TRUE)), .SDcols = c("chnk_psc", "hlbt_psc", "discard", "crab_psc"), keyby = c("STRATUM_COL", "N", "n")]
  var <- data[, lapply(.SD, function(x) var(x[OBSERVED_FLAG=="Y"], na.rm = TRUE)), .SDcols = c("chnk_psc", "hlbt_psc", "discard", "crab_psc"), keyby = c("STRATUM_COL", "N", "n")]
  
  mean <- melt(mean, id.vars = c("STRATUM_COL", "N", "n"), variable.name = "metric", value.name = "mean")
  var <- melt(var, id.vars = c("STRATUM_COL", "N", "n"), variable.name = "metric", value.name = "var")
  
  data <- merge(mean, var, by = c("STRATUM_COL", "N", "n", "metric"))
  
  # Estimate the stratum-level mean and variance  
  data[, mean := N * mean]
  data[, var := N^2 * (N-n)/N * 1/n * var]
  
  # Return the mean, variance, and standard error for each metric
  return(data[, .(mean = round(sum(mean)), variance = round(sum(var)), standard_error = round(sqrt(sum(var))), cv = round(sqrt(sum(var)) / sum(mean), 4)), keyby = .(metric)])
}