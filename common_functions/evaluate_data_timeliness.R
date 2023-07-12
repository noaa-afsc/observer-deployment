evaluate_data_timeliness <- function(data, strata_timeliness){
  
  dt <- rbind(merge(data[STRATA != "ZERO" & AGENCY_GEAR_CODE != "JIG"], strata_timeliness, by = c("COVERAGE_TYPE", "STRATA")), 
              merge(data[STRATA == "ZERO" & AGENCY_GEAR_CODE != "JIG"], strata_timeliness, by.x = c("COVERAGE_TYPE", "AGENCY_GEAR_CODE"), by.y = c("COVERAGE_TYPE", "STRATA"), all.x = TRUE))
  
  dt <- unique(dt[, .(TRIP_ID, mean_data_timeliness)])
  
  return(round(mean(dt[['mean_data_timeliness']]), 2))  
}