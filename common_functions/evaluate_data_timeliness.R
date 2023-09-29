evaluate_data_timeliness <- function(data, strata_timeliness, budget, rates, stratification, allocation){
  
  fgem_n_new <- sum(
    rates[
    ][[paste(stratification, allocation, sep = ".")]][
    ][ADP == unique(data$ADP) & BUDGET == budget & STRATUM_COL %in% c("EM_HAL", "EM_POT", "EM_FIXED-BSAI", "EM_FIXED-GOA", "EM_HAL-BSAI", "EM_HAL-GOA", "EM_POT-BSAI", "EM_POT-GOA"), n])
  strata_timeliness <- strata_timeliness[
  ][COVERAGE_TYPE == "PARTIAL"
  ][STRATA %in% c("EM_HAL", "EM_POT"), mean_data_timeliness := round(mean_data_timeliness * (fgem_n_new / 285), 2)]
  
  dt <- rbind(
    merge(
      data[STRATA != "ZERO" & AGENCY_GEAR_CODE != "JIG"], strata_timeliness, 
      by = c("COVERAGE_TYPE", "STRATA")), 
    merge(
      data[STRATA == "ZERO" & AGENCY_GEAR_CODE != "JIG"], strata_timeliness, 
      by.x = c("COVERAGE_TYPE", "AGENCY_GEAR_CODE"), by.y = c("COVERAGE_TYPE", "STRATA"), all.x = TRUE))
  
  dt <- unique(dt[, .(TRIP_ID, mean_data_timeliness)])
  
  return(round(mean(dt[['mean_data_timeliness']], na.rm = TRUE), 2))  
}