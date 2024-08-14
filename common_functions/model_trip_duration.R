library(data.table)
library(odbc)

#======================================================================================================================#
# Model Trip Duration --------------------------------------------------------------------------------------------------
#======================================================================================================================#

# This function is a modified version of the one that is used in get_data.R in previous ADPs.
model_trip_duration <- function(val_data, use_mod = "DAYS ~ RAW", channel) {
  # val_data <- copy(work.data); use_mod <- "DAYS ~ RAW"; channel <- channel_afsc   
  
  range1 <- min(val_data$ADP); range2 <- max(val_data$ADP)
  
  #========================#
  # DATE CHECKING FUNCTION #
  #========================#
  # This function is used to help evaluate which joins are good or bad
  # count up number of overlapping (OVER) or mismatching days (MISM))
  cols <- c("TRIP_START", "TRIP_END", "ODDS_START", "ODDS_END")                   # define columns to be fed into date checking function dc()
  dc <- function(sdcols, TYPE){
    dtn <- function(x) as.numeric(as.Date(x))  # define function that converts POSIXct to date and the numeric date
    v_d <- c(dtn(sdcols[[1]]) : dtn(sdcols[[2]]))
    o_d <- c(dtn(sdcols[[3]]) : dtn(sdcols[[4]]))
    o <- length(intersect(v_d, o_d))
    if(TYPE=="o"){return(o)}                                                    # type = overlap, which returns number of overlapping dates
    if(TYPE=="m"){return(max(length(v_d), length(o_d)) - o)}                    # type = mismatch, which returns number of non-overlapping dates
  }
  
  # Pull data, if not already done so
  if(exists("mod_dat", where = .GlobalEnv) == FALSE){
    
    #====================#
    # VALHALLA DATA PULL #
    #====================#
    
    # Load up observed trips in Valhalla
    
    o_val <- unique(val_data[COVERAGE_TYPE == "PARTIAL" & OBSERVED_FLAG == "Y", .(
      ADP, TRIP_ID, CRUISE, PERMIT, AREA = REPORTING_AREA_CODE, GEAR = AGENCY_GEAR_CODE, SECTOR = PROCESSING_SECTOR, 
      TARGET = TRIP_TARGET_CODE, TRIP_START = TRIP_TARGET_DATE, TRIP_END = LANDING_DATE, STRATA, TENDER
    )])
    
    # Trips in valhalla may have multiple gear types and/or targets - These will be kept in the 'o_val' object, but will be simplified in the 'val' object moving forward
    val <- copy(o_val)
    val[, GEAR_N := length(unique(GEAR)), by = .(TRIP_ID)]                            # count number of gear types used in each trip
    val <- val[, .(TRIP_START = min(TRIP_START), TRIP_END = max(TRIP_END)), by=.(ADP, PERMIT, GEAR, TRIP_ID, STRATA, TENDER)]  # simplify the start/end dates - removing TARGET here
    val_sub <- copy(val)
    val_sub[, REF := as.numeric(as.Date(TRIP_START))]; setkey(val_sub, PERMIT, REF) # Create new column that is numeric start date of each trip, set keys
    
    #=========================#
    # ODDS SEA DAYS DATA PULL #
    #=========================#
    
    # Pull billed sea days from ODDS - These match almost exactly with days_paid
    # Runs from 2015 to date
    sea_day <- setDT(dbGetQuery(channel, paste(
      "
      SELECT cruise, permit, all_dates, at_sea_day_count_odds AS odds_days, gear_type, odds_trip_plan_log_seq 
      FROM norpac.odds_billdays_2015_mv
      "
    )))
    sea_day <- sea_day[!is.na(ODDS_DAYS)]
    sea_day[, YEAR := year(ALL_DATES)]
    sea_day[, c("A", "B") := tstrsplit(ODDS_TRIP_PLAN_LOG_SEQ, split = ", ")]         # Where "A" is the first odds seq and "B" is the second odds seq if present, NA if not      
    sea_day[, FLAG := !(is.na(B))]                                                  # If a second odds seq exists, flag as TRUE 
    sea_day <- data.table::melt(sea_day, id.vars = c("CRUISE", "PERMIT", "ALL_DATES", "ODDS_DAYS", "GEAR_TYPE", "YEAR", "FLAG"), measure.vars = c("A", "B"), value.name = "ODDS_SEQ") # Melt so each odds seq has its own row
    sea_day <- sea_day[!is.na(ODDS_SEQ)]                                          # remove extraneous rows without secondary odds_seq
    sea_day [FLAG == TRUE, ODDS_DAYS := ODDS_DAYS/2]                                  # if flagged for having two odds_seq, halve the day duration, so each trip gets a half day
    day_sum <- sea_day[, .(DAYS = sum(ODDS_DAYS), ODDS_START = min(ALL_DATES), ODDS_END = max(ALL_DATES)), by = .(CRUISE, PERMIT, ODDS_SEQ, GEAR_TYPE)]
    
    day_sum[, c("ODDS_START", "ODDS_END") := lapply(.SD, as.Date), .SDcols = c("ODDS_START", "ODDS_END")]
    odds_sub <- day_sum[year(ODDS_START) %in% (range1:range2) & year(ODDS_END) %in% (range1:range2)]  # get odds days when trip start is within 2017/2018 only - 2016 is kind of a mess!
    odds_sub[, ODDS_SEQ := as.numeric(ODDS_SEQ)]                                    # convert odds_seq to numeric for easier comparison
    odds_sub[, REF := as.numeric(as.Date(ODDS_START))]; setkey(odds_sub, PERMIT, REF)  
    
    # Very rarely (one case found in 2016), a single odds_seq might have more than one record, in this case, two cruises existed under the same seq
    # The total number of observer days for the seq was a large outlier (13.5 days for a 2-day fishing trip) - Such cases will be excluded
    if(sum(duplicated(odds_sub$ODDS_SEQ)) > 0){message("Some ODDS_SEQ have duplicate records.")}
    odds_dup <- odds_sub[duplicated(odds_sub$ODDS_SEQ), ODDS_SEQ]
    #  odds_sub[ODDS_SEQ %in% odds_dup]                                                # These records will be excluded!
    if(length(odds_dup) > 0){message(paste("Duplicated ODDS_SEQ:", paste(odds_dup, collapse = ", "), "were removed."))}
    odds_sub <- odds_sub[!(ODDS_SEQ %in% odds_dup)]
    
    #=============#
    # FIRST LEVEL #
    #=============#
    
    # First pass, with rolling join of 3 (allow odds_start to be up to 3 days prior to trip start)
    test1 <- val_sub[odds_sub, roll = -3, nomatch = NULL]
    test1[, ':=' (O = dc(.SD, "o"), M = dc(.SD, "m")), .SDcols = cols, by = rownames(test1)]
    
    # Make sure each odds_seq has only one match - if not, exclude joins with zero overlap, and then identify the join with the best fit
    test1[, ODDS_N := .N, by=.(ODDS_SEQ)]                                         # For each odds_seq count up number of joins. If > 1, remove the join with the worse fit
    test1[, CHK := ifelse(O == 0, FALSE, ifelse(ODDS_N == 1, TRUE, ifelse(O == max(O) & max(O) != min(O), TRUE, ifelse(M == min(M) & max(M) != min(M), TRUE, FALSE)))), by = ODDS_SEQ]   # evaluate fit of each join
    if(nrow(test1[ODDS_N > 1 & CHK == TRUE, .(N = .N), by = ODDS_SEQ][N > 1]) > 0) {message("Still have duplicate ODDS_SEQ in first level.")}  # Check to make sure there aren't any ties!
    
    # It may be possible that multiple odds trips will join to a single valhalla trip, especially for trawl trips with extra offload days
    # In some cases, the odds trip was split between the fishing trip and offload
    # However, this may not always be the case - if multiple trips would have matched in the same rolling period, only the closest one would be returned
    # For now, keep only the best join based on overlap and mismatches and the excluded trips will be joined later via the end date or the final slop
    # If there is a tie with O and M - if ODDS_END > TRIP_END, exclude it as the worse match
    test1[, VAL_N := .N, by = .(TRIP_ID)]
    test1[VAL_N > 1, CHK := ifelse(O == max(O) & max(O) != min(O), TRUE, ifelse(M == min(M) & max(M) != min(M), TRUE, ifelse(ODDS_END > TRIP_END, FALSE, ifelse(ODDS_START < TRIP_START, FALSE, TRUE)))), by = TRIP_ID]
    if(nrow(test1[VAL_N > 1 & CHK == TRUE, .(N = .N), by = TRIP_ID][N > 1]) > 0) {message("Still have duplicate TRIP_ID in first level.")}  # Check to make sure there aren't any ties!
    
    keepers <- test1[CHK == TRUE]                                                   # keep the good joins
    val_r1 <- val_sub[!(TRIP_ID %in% keepers$TRIP_ID)]                            # trim val_sub to include only unjoined trips
    odds_r1 <- odds_sub[!(ODDS_SEQ %in% keepers$ODDS_SEQ)]                        # trim odds_sub to include only unjoined trips
    
    #==============#
    # SECOND LEVEL #
    #==============#
    
    # Now try to join the remainders by the end dates
    val_r1[, REF := as.numeric(as.Date(TRIP_END))]; setkey(val_r1, PERMIT, REF)
    odds_r1[, REF := as.numeric(as.Date(ODDS_END))]; setkey(odds_r1, PERMIT, REF)
    
    # Allow the odds_end date to be up to 3 days after the trip end
    test2 <- val_r1[odds_r1, roll = 3, nomatch = NULL]   #
    test2[, ':=' (O = dc(.SD, "o"), M = dc(.SD, "m")), .SDcols = cols, by = rownames(test2)]  # count up overlapping and mismatching days for each join
    # Make sure each odds_seq has only one match - if not, exclude joins with zero overlap, and then identify the join with the best fit
    test2[, ODDS_N := .N, by = .(ODDS_SEQ)]                                         # For each odds_seq count up number of joins. If > 1, remove the join with the worse fit
    test2[, CHK := ifelse(O == 0, FALSE, ifelse(ODDS_N == 1, TRUE, ifelse(O == max(O) & max(O) != min(O), TRUE, ifelse(M == min(M) & max(M) != min(M), TRUE, FALSE)))), by = ODDS_SEQ]   # evaluate fit of each join
    if(nrow(test2[ODDS_N > 1 & CHK == TRUE, .(N = .N), by = ODDS_SEQ][N > 1]) > 0) {message("Still have duplicate ODDS_SEQ in second level.")}  # Check to make sure there aren't any ties!
    
    test2[, VAL_N := .N, by = .(TRIP_ID)]                                           # Count number of records for each trip_id
    test2[VAL_N > 1, CHK := ifelse(O == max(O) & max(O) != min(O), TRUE, ifelse(M == min(M) & max(M) != min(M), TRUE, ifelse(ODDS_END > TRIP_END, FALSE, TRUE))), by = TRIP_ID]
    if(nrow(test2[VAL_N > 1 & CHK == TRUE, .(N = .N), by = TRIP_ID][N > 1]) > 0) {message("Still have duplicate TRIP_ID in second level.")}  # Check to make sure there aren't any ties!
    
    keepers <- rbind(keepers, test2[CHK == TRUE])
    val_r2 <- val_sub[!(TRIP_ID %in% keepers$TRIP_ID)]                              # trim val_sub to include only unjoined trips
    odds_r2 <- odds_sub[!(ODDS_SEQ %in% keepers$ODDS_SEQ)]                          # trim odds_sub to include only unjoined trips
    
    #=============#
    # THIRD LEVEL #
    #=============#
    
    # Next, try joining using nearest with remaining trips and keeping only those with overlap
    # Sometimes, especially for tender trips, the embark/disembark date will be a few days inside of trip/start and therefore not join sooner
    
    # First try to fit to val_r2 but with roll="nearest"
    test3 <- val_r2[odds_r2, roll = "nearest", nomatch = NULL]
    test3[, ':=' (O = dc(.SD, "o"), M = dc(.SD, "m")), .SDcols = cols, by = .(TRIP_ID, ODDS_SEQ)]  # count up overlapping and mismatching days for each join
    # Make sure each odds_seq has only one match - if not, exclude joins with zero overlap, and then identify the join with the best fit
    test3[, ODDS_N := .N, by = .(ODDS_SEQ)]                                         # For each odds_seq count up number of joins. If > 1, remove the join with the worse fit
    test3[, CHK := ifelse(O == 0, FALSE, ifelse(ODDS_N == 1, TRUE, ifelse(O == max(O) & max(O) != min(O), TRUE, ifelse(M == min(M) & max(M) != min(M), TRUE, FALSE)))), by = ODDS_SEQ]   # evaluate fit of each join
    if(nrow(test3[ODDS_N > 1 & CHK == TRUE, .(N = .N), by = ODDS_SEQ][N > 1]) > 0) {message("Still have duplicate ODDS_SEQ in second level.")}  # Check to make sure there aren't any ties!
    
    test3[, VAL_N := .N, by = .(TRIP_ID)]                                           # Count number of times each TRIP_ID was matched
    # if I have multiple odds_seq for the same trip_id that both have overlap, simplify the dates, sum the days, and store the odds_seq that is higher
    third_join <- test3[O != 0]
    third_join[, VAL_N := .N, by = .(TRIP_ID)]  # recalculate VAL_N after excluding the matches with zero overlap
    third_join[, CHK := ifelse(ODDS_SEQ == min(ODDS_SEQ), TRUE, FALSE), by = TRIP_ID]   # Flag the secondary odds_seq
    odds_seq_keep <- third_join[CHK == FALSE, ODDS_SEQ]                               # Store this odds_seq that is being combined with another
    third_join[VAL_N > 1, ':=' (ODDS_START = min(ODDS_START), ODDS_END = max(ODDS_END), DAYS = sum(DAYS)), by = .(TRIP_ID, CRUISE)]   # If obs_seq for the same trip_id and cruise, simplify
    
    keepers <- rbind(keepers, third_join[CHK == TRUE])
    odds_r3 <- odds_sub[!(ODDS_SEQ %in% keepers$ODDS_SEQ | ODDS_SEQ %in% odds_seq_keep)]
    val_r3  <- val_sub[!(TRIP_ID %in% keepers$TRIP_ID)]
    
    #==============#
    # FOURTH LEVEL #
    #==============#
    
    # Next, try joining remaining trips with currently existing joins (i.e., the assignment was split into separate obs_seq records)
    # Permit and cruise must match, and try joining using nearest from each side
    odds_r3[, REF := as.numeric(as.Date(ODDS_END))] # use disembark as ref
    tack <- copy(odds_r3)[, .(PERMIT, A_CRUISE = CRUISE, A_ODDS_SEQ = ODDS_SEQ, A_DAYS = DAYS, A_ODDS_START = ODDS_START, A_ODDS_END = ODDS_END, REF)]
    setkey(tack, PERMIT, REF)
    
    exis <- copy(keepers)
    exis[, REF := as.numeric(as.Date(ODDS_START))]; setkey(exis, PERMIT, REF)
    
    # Tacking on from left side (allow disembark date to be within 3 days prior to existing embark date)
    test4 <- exis[tack, roll = -3, nomatch=NULL]
    test4[, VAL_N := .N, by = TRIP_ID]  # some of these might have multiple joins that can be simplified
    if(nrow(test4[VAL_N > 1]) > 0) {message("Multiple TRIP_ID in the fifth level - must deal with this because this was not an issue before, so no code exists to handle this!")}
    test4[, ':=' (ODDS_START = min(ODDS_START, A_ODDS_START), ODDS_END = max(ODDS_END, A_ODDS_END), DAYS = sum(DAYS, A_DAYS)), keyby = .(ODDS_SEQ, TRIP_ID)]  # Replace existing data
    
    setkey(keepers, ODDS_SEQ, TRIP_ID)  # reorder keepers by odds_seq and trip_id so that replaced values will merge in the correct order
    keepers[ODDS_SEQ %in% test4$ODDS_SEQ, ':=' (ODDS_START = test4$ODDS_START, ODDS_END = test4$ODDS_END, DAYS = test4$DAYS)]
    odds_seq_keep <- c(odds_seq_keep, test4$A_ODDS_SEQ)  # Keep the odds_seq in this secondary list
    
    odds_r4 <- odds_sub[!(ODDS_SEQ %in% keepers$ODDS_SEQ | ODDS_SEQ %in% odds_seq_keep)]
    val_r4  <- val_sub[!(TRIP_ID %in% keepers$TRIP_ID)]
    
    #=============#
    # FIFTH LEVEL #
    #=============#
    odds_r4[, REF := as.numeric(as.Date(ODDS_START))] # use new embark as ref
    tack2 <- copy(odds_r4)[, .(PERMIT, A_CRUISE = CRUISE, A_ODDS_SEQ = ODDS_SEQ, A_DAYS = DAYS, A_ODDS_START = ODDS_START, A_ODDS_END = ODDS_END, REF)]
    setkey(tack2, PERMIT, REF)
    
    exis <- copy(keepers)
    exis[, REF := as.numeric(as.Date(ODDS_END))]; setkey(exis, PERMIT, REF)
    
    # Tacking on from right side, but also using "nearest" in case tender trips had observers disembark before end of trip
    test5 <- exis[tack2, roll = "nearest", nomatch = NULL]
    
    a_cols <- c("TRIP_START", "TRIP_END", "A_ODDS_START", "A_ODDS_END") 
    test5[, ':=' (A_O = dc(.SD, "o"), A_M = dc(.SD, "m")), .SDcols = a_cols, by = rownames(test5)]
    
    # If tacked on trips have no overlap, first check to see if the existing disembark is within a reasonable time period (2-days?)
    test5[A_O == 0, CHK := ifelse(abs(as.numeric(A_ODDS_START - ODDS_END)) <= 2, TRUE, FALSE)]
    if(nrow(test5[CHK == FALSE]) > 0 ){message("Some matches were removed in the fifth level for having no overlap and being out of 2-day range - probably a good thing, but double-check this.")}
    test5 <- test5[CHK == TRUE]
    
    test5[, VAL_N := .N, by = TRIP_ID]  # some of these might have multiple joins that can be simplified
    test5[, ':=' (A_ODDS_START = min(A_ODDS_START), A_ODDS_END = max(A_ODDS_END), A_DAYS = sum(A_DAYS)), keyby = .(ODDS_SEQ, TRIP_ID)] # simplify
    test5[VAL_N > 1, CHK := ifelse(A_ODDS_SEQ == min(A_ODDS_SEQ), TRUE, FALSE), by = .(ODDS_SEQ, TRIP_ID)]       # Now that things are simplified, keep only one record
    odds_seq_keep <- unique(c(odds_seq_keep, test5$A_ODDS_SEQ))                     # Store the tacked on odds_seq
    test5 <- test5[CHK == TRUE]                                                       # Remove extra rows 
    test5[, ':=' (ODDS_START = min(ODDS_START, A_ODDS_START), ODDS_END = max(ODDS_END, A_ODDS_END), DAYS = sum(DAYS, A_DAYS)), keyby = .(ODDS_SEQ, TRIP_ID)]
    setkey(test5, ODDS_SEQ, TRIP_ID)
    
    setkey(keepers, ODDS_SEQ, TRIP_ID)  # reorder keepers by odds_seq and trip_id so that replaced values will merge in the correct order
    keepers[ODDS_SEQ %in% test5$ODDS_SEQ, ':=' (ODDS_START = test5$ODDS_START, ODDS_END = test5$ODDS_END, DAYS = test5$DAYS)]
    
    odds_r5 <- odds_sub[!(ODDS_SEQ %in% keepers$ODDS_SEQ | ODDS_SEQ %in% odds_seq_keep)]
    val_r5  <- val_sub[!(TRIP_ID %in% keepers$TRIP_ID)]
    
    #=====================#
    # n_val - FIRST LEVEL #
    #=====================#
    
    # there are several cases where trips were found in valhalla but had observed_flag='N', which must be incorrect
    # Can dig these out of valhalla without too much trouble
    
    lookup <- unique(odds_r5[, .(PERMIT, ADP = year(ODDS_START))])
    lookup_val_scratch <- lookup[ADP == range2, PERMIT]
    lookup_val <- lookup[ADP < range2, PERMIT]
    
    # Looking for trip labeled as non-observed (but actually were)
    
    n_val <- val_data[COVERAGE_TYPE == "PARTIAL" & OBSERVED_FLAG == "N", .(
      ADP, TRIP_ID, CRUISE, PERMIT, AREA = REPORTING_AREA_CODE, GEAR = AGENCY_GEAR_CODE, SECTOR = PROCESSING_SECTOR, 
      TARGET = TRIP_TARGET_CODE, TRIP_START = TRIP_TARGET_DATE, TRIP_END = LANDING_DATE, STRATA, TENDER
    )]
    
    # Change date class
    n_val[, ':='(TRIP_START = as.Date(TRIP_START), TRIP_END = as.Date(TRIP_END))]
    
    n_val <- n_val[(ADP < range2 & PERMIT %in% lookup_val) | (ADP == range2 & PERMIT %in% lookup_val_scratch)]
    
    n_val[, YEAR := year(TRIP_START)]                                               # for valhalla, define year by trip_start
    n_val <- n_val[YEAR >= (range1) & YEAR <= (range2)]                             # Trim valhalla,  using the most recent 3 full years of data
    n_val[, GEAR_N := length(unique(GEAR)), by = .(TRIP_ID)]                            # count number of gear types used in each trip
    n_val[GEAR_N > 1, GEAR := ifelse(GEAR == "NPT", "PTR", ifelse(GEAR == "POT", "HAL", GEAR)), by = .(TRIP_ID, GEAR)]    # If multiple gear types were used, prioritze PTR over NPT and HAL over POT
    n_val <- n_val[, .(TRIP_START = min(TRIP_START), TRIP_END = max(TRIP_END)), by = .(ADP, PERMIT, GEAR, TRIP_ID, STRATA, TENDER)]  # simplify the start/end dates - removing TARGET here
    n_val[, REF := as.numeric(as.Date(TRIP_START))]; setkey(n_val, PERMIT, REF)     # Create new column that is numeric start date of each trip, set keys
    
    # First pass, with rolling join of 3 (allow odds_start to be up to 3 days prior to trip start)
    odds_r5[, REF := as.numeric(as.Date(ODDS_START))]; setkey(odds_r5, PERMIT, REF)
    
    n_val1 <- n_val[odds_r5, roll = "nearest", nomatch = NULL]   
    n_val1[, ':=' (O = dc(.SD, "o"), M = dc(.SD, "m")), .SDcols = cols, by = .(TRIP_ID, ODDS_SEQ)]  # count up overlapping and mismatching days for each join
    n_val1 <- n_val1[O != 0]                                                         # Retain only matches with some overlap
    n_val1 <- n_val1[!n_val1[, .SD[.N > 1], by = ODDS_SEQ][, .SD[O == min(O) & M == max(M)], by = .(TRIP_ID)], on = .(TRIP_ID, ODDS_SEQ)] # For ODDS_SEQs with more than one matched TRIP_ID, exclude the match with the least overlap and most mismatch
    n_val1[, ODDS_N := .N, by = ODDS_SEQ]
    n_val1[, CHK := TRUE]
    n_val1[, VAL_N := .N, by = TRIP_ID]
    if(nrow(n_val1[ODDS_N > 1]) > 0){message("Multiple ODDS_SEQ matches in n_val level 1 - I didn't code for this!")}
    
    # When multiple odds_seq match to the same trip_id...
    n_val1[VAL_N > 1, ':=' (ODDS_START = min(ODDS_START), ODDS_END = max(ODDS_END), DAYS = sum(DAYS)), by = .(TRIP_ID, CRUISE)]   # If odds_seq for the same trip_id and cruise, simplify
    n_val1[VAL_N > 1, CHK := ifelse(ODDS_SEQ == max(ODDS_SEQ), FALSE, TRUE), by = .(TRIP_ID, CRUISE)]
    odds_seq_keep <- c(odds_seq_keep, n_val1[VAL_N > 1 & CHK == FALSE, ODDS_SEQ])
    n_val1 <- n_val1[CHK == TRUE]
    
    keepers <- rbind(keepers, n_val1)                                             # Final object with all joins
    odds_r6 <- odds_sub[!(ODDS_SEQ %in% keepers$ODDS_SEQ | ODDS_SEQ %in% odds_seq_keep)]   # remaining unmatched odds trips
    val_r6  <- val_sub[!(TRIP_ID %in% keepers$TRIP_ID)]                           # remaining trips in valhalla
    
    mod_dat <- copy(keepers)
    
    match_prop <- round(100 * (nrow(mod_dat) - nrow(odds_r6)) / nrow(mod_dat), 3)
    message(paste("Matched ", match_prop, " percent of ODDS and Valhalla trips."))
  }
  
  #==========#
  # MODELING #
  #==========#
  
  mod_dat[, RAW := as.numeric(TRIP_END - TRIP_START, units = "days")]             # Calculate raw day difference of trip_end and trip_start
  mod_dat[, ':=' (AGENCY_GEAR_CODE = factor(GEAR, levels = c("HAL", "POT", "PTR", "NPT", "TRW")), TENDER = as.factor(TENDER), ADP = as.factor(ADP))]
  mod_dat[, GEAR := factor(GEAR, levels = c("HAL", "POT", "PTR", "NPT", "TRW"))]
  mod <- lm(as.formula(use_mod), data = mod_dat)   
  mod_dat[, MOD := round(predict(mod)/0.5) * 0.5]  # round model outputs to the nearest 0.5
  mod_dat[MOD <= 0, MOD := 0.5]                    # If a predicted duration is <= 0, make minimum of 0.5 days
  mod_dat_for_plt <- data.table::melt(mod_dat, id.vars = c("ADP", "PERMIT", "GEAR", "TRIP_ID", "STRATA", "TENDER", "AGENCY_GEAR_CODE", "DAYS"), measure.vars = c("RAW", "MOD"))
  mod_dat_for_plt[, GEAR := factor(GEAR, levels = c("HAL", "POT", "PTR", "NPT", "TRW"))]
  mod_dat_for_plt[, CLR := factor(ifelse(variable == "RAW", "Raw", ifelse(TENDER == "N", "NonTen", "Tender")), levels = c("Raw", "NonTen", "Tender"))]
  
  td_plot <- ggplot(mod_dat_for_plt[order(CLR)], aes(x = DAYS, y = value, shape = TENDER, group = TRIP_ID)) + 
    geom_line(color = "gray") + 
    geom_point(aes(color = CLR), position = position_jitter(width = 0.2, height = 0.2), alpha = 0.5) +  
    facet_grid(GEAR ~ ADP) + 
    scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 2), minor_breaks = NULL) + 
    scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 2), minor_breaks = NULL) + 
    geom_abline(slope = 1) + 
    labs(subtitle = paste(use_mod), y = "Value", x = "Actual Days") + 
    scale_color_manual(values = c("gray", "blue", "red"))
  
  diagnostic1 <- mod_dat[, .(DAYS = sum(DAYS), MOD = sum(MOD)), keyby = .(ADP)][, .(ADP, DAYS, MOD, PERC = 100 * (MOD-DAYS)/DAYS)]  # pretty dang close... in 2018, PTR is under by 10%
  diagnostic2 <- mod_dat[, .(DAYS = sum(DAYS), MOD = sum(MOD)), keyby = .(ADP, GEAR)][, .(ADP, GEAR, DAYS, MOD, PERC = 100 * (MOD-DAYS)/DAYS)] 
  
  #=========#
  # OUTPUTS #
  #=========#
  
  # export mod_dat to global environent - this also allows this function to be re-used if needed, but will skip the database query step
  assign("mod_dat", mod_dat, envir = .GlobalEnv)
  
  return(list(TD_MOD = mod, TD_PLOT = td_plot, DIAG = list(YEAR = diagnostic1, YEAR_GEAR = diagnostic2)))
  
}
