evaluate_data_timeliness <- function(data, strata_timeliness){
  
  dt <- rbind(merge(data[STRATA != "ZERO"], strata_timeliness, by = c("COVERAGE_TYPE", "STRATA")), 
              merge(data[STRATA == "ZERO"], strata_timeliness, by.x = c("COVERAGE_TYPE", "AGENCY_GEAR_CODE"), by.y = c("COVERAGE_TYPE", "STRATA"), all.x = TRUE))
  
  dt[AGENCY_GEAR_CODE == "JIG", one_week_quantile := 1]
  
  dt <- unique(dt[, .(TRIP_ID, one_week_quantile)])
  
  return(round(sum(dt[['one_week_quantile']]) / nrow(dt), 4))  
}