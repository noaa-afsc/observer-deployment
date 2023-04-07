perm_test <- function(data, units, metrics, groups, yn_var, n_rep, boxcox, lambda = seq(-6, 6, 0.01)){ 

  # Create table of trip counts per group by sampled status----------------------------------
  N_table <- data[, c(..units, ..groups, ..metrics, ..yn_var)][, .(N = uniqueN(get(..units)), n = uniqueN(get(units)[get(yn_var) == "Y"])), by = c(groups)][, rate := n / N][order(mget(groups))]
  
  # Transform data as in Christensen and Zabriskie 2021 ------------------------------
  # https://github.com/WFChristensen/permutation
  
  if(boxcox){
    
  # Put data in long format
  data <- melt.data.table(data, measure.vars = metrics, variable.name = "METRICS", value.name = "vals")
  
  # Center values about the mean within group and yes / no variable
  data[, vals_centered := vals - mean(vals), keyby = c(groups, "METRICS", yn_var)]
  
  # Shift values to be positive within each group
  data[, vals_shifted := vals_centered - min(vals_centered) + 0.01, keyby = c(groups, "METRICS")]
  
  # Calculate the likelihood of many values of Box-Cox lambda for each group and metric
  if(length(unique(data[, get(yn_var)])) > 1){
  bc <- data[, .(lambda = lambda, logLik = MASS::boxcox(vals_shifted ~ get(yn_var), lambda = lambda, plotit = FALSE)$y), keyby = c(groups, "METRICS")]
  } else{
  bc <- data[, .(lambda = lambda, logLik = MASS::boxcox(vals_shifted ~ 1, lambda = lambda, plotit = FALSE)$y), keyby = c(groups, "METRICS")]  
  }
  
  # Find the value of lambda that maximizes the likelihood for each group and metric
  bc <- bc[, .(lambda = lambda[logLik == max(logLik)]), keyby = c(groups, "METRICS")]
  
  # Join the most likely lambdas to each group
  data <- bc[data, on = c(groups, "METRICS")]
  
  # Apply Box-Cox lambdas to the data
  data[, vals_trans := ifelse(lambda == 0, log(vals), (vals^lambda - 1) / lambda)]
  
  # Remove columns that are no longer needed
  data[, ':=' (vals = NULL, vals_centered = NULL, vals_shifted = NULL, lambda = NULL)]
  
  # Put data back into wide format
  data <- dcast(data, ... ~ METRICS, value.var = "vals_trans")
  
  }
  
  # Store observed differences --------------------------------------------------------------
  
  ## Isolate the columns of interest
  dat <- data[, c(..groups, ..metrics, ..yn_var)]
  
  ## Calculate the raw difference in means between the "yes" and "no" groups
  obs_raw <- dat[, lapply(.SD, function(x) mean(x[get(yn_var)=="Y"], na.rm = TRUE) - mean(x[get(yn_var)=="N"], na.rm = TRUE)), .SDcols = metrics, keyby = c(groups)][, SCALE := "RAW"]
  
  ## Calculate the difference in means between the "yes" and "no" groups as a percentage of the mean for the "no" group
  obs_pct <- dat[, lapply(.SD, function(x) (mean(x[get(yn_var)=="Y"], na.rm = TRUE) - mean(x[get(yn_var)=="N"], na.rm = TRUE)) / mean(x[get(yn_var)=="N"], na.rm = TRUE) * 100), .SDcols = metrics, keyby = c(groups)][, SCALE := "PCT"]
  
  ## Bind results together
  obs <- rbind(obs_raw, obs_pct)
  
  # Find expected differences ---------------------------------------------------------------
  ## These are under the null hypothesis that "yes" and "no" groups are the same
  
  ## Duplicate data by number of replicates (n_rep)
  pdat <- dat[rep(seq(1, nrow(dat)), n_rep)]
  
  ## Label replicates
  pdat[, REP := sort(rep(1:n_rep, nrow(pdat)/n_rep))]
  
  ## Sample the yes/no variable without replacement
  pdat[, (yn_var) := sample(get(yn_var)), keyby = c("REP", groups)]
    
  ## Calculate the raw difference in means between the "yes" and "no" groups
  exp_raw <- pdat[, lapply(.SD, function(x) mean(x[get(yn_var)=="Y"], na.rm = TRUE) - mean(x[get(yn_var)=="N"], na.rm = TRUE)), .SDcols = metrics, by = c("REP", groups)][, SCALE := "RAW"]
    
  ## Calculate the difference in means between the "yes" and "no" groups as a percentage of the mean for the "no" group
  exp_pct <- pdat[, lapply(.SD, function(x) (mean(x[get(yn_var)=="Y"], na.rm = TRUE) - mean(x[get(yn_var)=="N"], na.rm = TRUE)) / mean(x[get(yn_var)=="N"], na.rm = TRUE) * 100), .SDcols = metrics, by = c("REP", groups)][, SCALE := "PCT"]
    
  ## Bind results together
  exp <- rbind(exp_raw, exp_pct)
  
  # Calculate p-values  ---------------------------------------------------------------------- 
  
  ## Bind expected differences (null hypothesis) to observed differences in preparation for p-values
  p_val <- rbind(obs_raw[, TYPE:="OBS"], exp_raw[, ':='(REP = NULL, TYPE = "EXP")])
  
  ## Test whether the expected difference is greater than or equal to the observed difference
  p_val <- p_val[, lapply(.SD, function(x) as.numeric(abs(x[TYPE=="EXP"]) >= abs(x[TYPE=="OBS"]))), .SDcols = metrics, by = c(groups)]
  
  ## Calculate the p-value as the proportion of permutations that produced a difference more extreme than the observed difference
  p_val <- p_val[, lapply(.SD, function(x) sum(x) / n_rep), .SDcols = metrics, by = c(groups)]
  
  # Return results ---------------------------------------------------------------------------
  return(list("N_table" = N_table, "obs" = obs, "exp" = exp, "p-value" = p_val))
    
}
