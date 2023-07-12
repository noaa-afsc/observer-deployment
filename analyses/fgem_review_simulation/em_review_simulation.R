# Data Prep and Simulation Function ------------------------------------------------------------------------------------

library(data.table)         # Data wrangling
library(ggplot2)            # Plotting

load("analyses/allocation_evaluation/allocation_evaluation.Rdata")   # loads pc_effort_dt and trips_melt

# Subset 2022 EM data
fgem_2022 <- unique(pc_effort_dt[
][ADP == 2022 & STRATA %in% c("EM_HAL", "EM_POT")
][, .(
  START = min(TRIP_TARGET_DATE, LANDING_DATE), END = max(TRIP_TARGET_DATE, LANDING_DATE)
  ), by = .(TRIP_ID, wd_TRIP_ID, STRATA, DAYS)])

# Come up with some sample rates from allocation, but using placeholders here (status quo and half of that.
sample_rates_30 <- data.table(STRATA = c("EM_HAL", "EM_POT"), RATE = c(0.30, 0.30))
sample_rates_15 <- data.table(STRATA = c("EM_HAL", "EM_POT"), RATE = c(0.15, 0.35))

# This function simulates review times of FG_EM hard drives given an average time it takes for one reviewer to complete
# review of an HD, the number of reviewers, a sampling rate of trips, and the temporal arrangement of fishing effort.
# This function currently assumes that reviewers will prioritize the most recently recieved HD.
simulate_review <- function(fgem_effort, sample_rates, em_review_time, reviewer_count, iter, seed = NULL) {

  if( !is.null(seed) ) set.seed(seed)
  
  x0 <- copy(fgem_effort)
  x0[, RATE := sample_rates[x0, RATE, on = .(STRATA)]]
  x0[, DAY := as.integer(START - min(START), units = "days") + 1L] 
  
  # Initialize results list
  iter_res <- vector(mode = "list", length = iter)
  
  # Simulate sampling and review
  for(i in 1:iter) {
    
    # Simulate sampling
    x1 <- copy(x0)
    x1[, RN := runif(.N, 0, 1)]  # Generate random number
    selected <- x1[RN < RATE][order(END)]    # Subset selected trips
    # hist(selected$DAY, breaks = 365)
    
    # Simulate review
    
    # Initialize the review loop
    to_review <- as.matrix(copy(selected)[, .(TRIP_ID = as.integer(TRIP_ID), DAY = as.integer(DAY))])
    day <- 0L 
    # Format for each reviewer is TRIP_ID, the DAY the trip ended, and the # of days spent on HD review.
    reviewer_lst <- as.list(rep(
      list(c(TRIP_ID = NA_integer_, END = NA_integer_, REVIEW_DAY = NA_integer_)), 
      times = reviewer_count))
    backlog <- to_review[0, ]
    completed <- c()
    loop_run <- T
    
    while( loop_run ) {
      
      # Advance the day
      day <- day + 1L
      # cat(paste0(day, ", "))
      reviewer_lst <- lapply(reviewer_lst, function(x) {
        # x <- reviewer_lst[[1]]
        if(!is.na(x[3])) {
          x[3] <- x[3] + 1L
          x
        } else x
      })
      
      # Check if any more hard drives are in.
      new_index <- which(to_review[, "DAY"] == day)
      if( length(new_index) ) {
        new <- to_review[new_index, , drop = F]           # Add to new list
        to_review <- to_review[-new_index, , drop = F]    # Remove from to review list
      } else new <- to_review[0, , drop = F]              # If no new trips, make it an empty matrix
      
      # Assign hard drives to reviewers without an assignment. Prioritize new hard drives over backlog, and within 
      # the backlog, prioritize the most recently added hard drives.
      for(j in 1L:reviewer_count) {
        # j <- 1L
        
        x <- reviewer_lst[[j]]
        
        if( is.na(x[1]) ) {
          
          # If there are new hard drives, assign them
          if( nrow(new) ) {
            add_new <- new[1, ]
            new <- new[-1, , drop = F]
            reviewer_lst[[j]] <- c(add_new, REVIEW_DAY = 1L)
          } else {
            # If there are no new hard drives but there is a backlog, assign the most recently added HD
            if( nrow(backlog) ) {
              add_backlog_index <- which.max(backlog[, "DAY"])[1]
              add_backlog <- backlog[add_backlog_index, ]
              backlog <- backlog[-add_backlog_index, , drop = F]
              reviewer_lst[[j]] <- c(add_backlog, REVIEW_DAY = 1L)
            }
          }
        } 
      }
      
      # When REVIEW_DAY = em_review_time, move HD to the complete list and empty the reviewer's assignment
      for(j in 1L:reviewer_count) {
        x <- reviewer_lst[[j]]
        
        if( !is.na(x["REVIEW_DAY"]) ) {
          if( x["REVIEW_DAY"] == (em_review_time) + 1L) {   # Add one day to account for day the HD was assignment  
            # Move the assignment as a new row of the completed matrix
            completed <- rbind(completed, c(x, COMPLETED = day, REVIEWED_BY = j))
            # clear out the assignment
            reviewer_lst[[j]] <- c(TRIP_ID = NA_integer_, END = NA_integer_, REVIEW_DAY = NA_integer_)
          }
        }
      }
      
      # If there are new HDs that could not be assignment, add them to the backlog matrix
      backlog <- rbind(backlog, new)
      # Update the loop_run control
      loop_run <- (nrow(to_review) != 0) | any(sapply(reviewer_lst, function(x) !is.na(x[1])))
    
    }
    
    iter_res[[i]] <- completed

  }
  
  # Calculate review time
  iter_res <- lapply(iter_res, function(x) cbind(x, REVIEW_TIME = x[, "COMPLETED"] - x[, "DAY"]))
  iter_res
    
}


# Run the Simulation ---------------------------------------------------------------------------------------------------

# Combine different combinations of EM selection rates, review time (2-5 days) and reviewer counts (3-6)
selection_rate_lst <- list(sample_rates_15, sample_rates_30)
em_review_time_vec <- 1:3L
reviewer_count_vec <- 3:6L
iter <- 100L
seed <- 12345L
# For each selection rates
i_lst <- vector(mode = "list", length = length(selection_rate_lst))
for(i in seq_along(selection_rate_lst)) {
  # For each assumed time to review an HD
  j_lst <- vector(mode = "list", length = length(em_review_time_vec))
  for(j in em_review_time_vec) {
    # For each number of reviewers
    k_lst <- vector(mode = "list", length = length(reviewer_count_vec))
    for(k in reviewer_count_vec) {
      k_lst[[which(reviewer_count_vec == k)]] <- simulate_review(
        fgem_2022, selection_rate_lst[[i]], em_review_time = j, reviewer_count = k, iter = iter, seed = seed
      )
    }
    names(k_lst) <- paste0("REVIEWER_", reviewer_count_vec)
    j_lst[[which(em_review_time_vec == j)]] <- k_lst
  }
  names(j_lst) <- paste0("REVIEW_TIME_", em_review_time_vec)
  i_lst[[i]] <- j_lst
}
names(i_lst) <- paste0("SEL_", c(15, 30))
em_review_res <- unlist(unlist(i_lst, recursive = F), recursive = F)  # 32 combinations (4*4*2), each with 100 iterations

em_review_dt <- rbindlist(
  lapply(em_review_res, function(x) rbindlist(lapply(x, as.data.table), idcol = "ITER")),
  idcol = "ID")
em_review_dt[, c("DESIGN", "DAYS_TO_REVIEW", "REVIEWERS") := tstrsplit(ID, split = "[.]")  ]
to_int <- c("DAYS_TO_REVIEW", "REVIEWERS")
em_review_dt[, (to_int) := lapply(.SD, function(x) as.integer(gsub("^.*[_]", "", x))), .SDcols = to_int]
em_review_dt[, REVIEW_START := COMPLETED - DAYS_TO_REVIEW]   # identify the day when an HD began its review


# Summarize Simulation Results -----------------------------------------------------------------------------------------

em_review_smry <- em_review_dt[, .(
  MEAN = round(mean(REVIEW_TIME),1), PROP_7 = sum(REVIEW_TIME <= 7)/.N
  ), keyby = .(DESIGN, REVIEWERS, DAYS_TO_REVIEW)]
# Get quantiles
em_review_quantiles <- em_review_dt[,  as.list(
  quantile(REVIEW_TIME, probs = c(0.25, 0.5, 0.75, 0.9, 0.975))
  ), keyby = .(DESIGN, REVIEWERS, DAYS_TO_REVIEW)]
em_review_smry <- em_review_smry[em_review_quantiles]

# Proportion of HDs reviewed within seven days of axquisition
em_review_smry_plot <- ggplot(em_review_smry, aes(x = sub("REVIEWER_", "",  REVIEWERS), fill = DESIGN, y = PROP_7)) +
  facet_grid(. ~ DAYS_TO_REVIEW, labeller = labeller(DAYS_TO_REVIEW = function(x) paste0("Days to Review : ", x))) + 
  geom_col(position ="dodge") + theme(legend.position = "bottom") + 
  geom_text(
    aes(label = formatC(MEAN, format = "f", digits = 1), y = 1), 
    position = position_dodge(width = 1), size = 3, hjust = 0.5, vjust = 0) + 
  labs(
    x = "# of Reviewers", y = "Proportion of HDs reviewd within 7 days", 
    subtitle = "Annotations show mean review time in days") + 
  scale_fill_manual(values = c("darkorchid4", "chartreuse4"))

if(F) {
  ggsave(
    filename = "analyses/inseason_management/figures/em_review_smry_plot.png", 
    plot = em_review_smry_plot, dpi = 600, width = 8, height = 6, units = "in")
}

# Visualize Time Series ------------------------------------------------------------------------------------------------

# This function counts the number of iterations where a reviewer was occupied reviewing a hard drive
review_day <- function(x) {
  x <- apply(x, 2, as.integer)
  x <- setnames(as.data.table(table(unlist(apply(x, 1, function(y) seq(y[1], y[2], 1))))), c("DAY", "COUNT"))
  x <- x[, lapply(.SD, as.integer)]
}
em_review_alpha <- em_review_dt[
  , review_day(.SD), by = .(DESIGN, DAYS_TO_REVIEW, REVIEWERS, REVIEWED_BY), 
  .SDcols = c("REVIEW_START", "COMPLETED")]
em_review_alpha[, ALPHA := COUNT / iter]

# For each iteration, create a vector 1:count for each day. Then get frequencies of counts across iterations
backlog_day <- function(x) {
  x <- lapply(split(x, by = "ITER", keep.by = F), function(y) apply(y, 2, as.integer))
  max_day <- max(sapply(x, function(x) max(x[, 3])))
  # For each iteration, for each hard drive, identify days it was in the backlog. 
  # Then for each day across iterations, create a vector of 1:number of hard drives
  
  x1 <- lapply(x, function(y) {
    day_lst <- vector(mode = "list", length = max_day) # initialize 
    # Identify days that hard drives were in a backlog. This is the difference of vectors between day of 
    # arrival to completion and day of review start to completion.
    y1 <- unlist(apply(y, 1, function(z) setdiff(seq(z[1], z[3], 1), seq(z[2], z[3], 1)), simplify = F))
    if(length(y1) > 0) {
      y1 <- sapply(table(y1), seq_len)  # For each day, make a vector 1:number of hard drives
      for(i in names(y1)) day_lst[[as.integer(i)]] <- y1[[i]]
    }
    day_lst
  })
  
  # Now across iterations, get frequency of hard drive counts for each day
  day_freq_lst <- vector(mode = "list", length = max_day)
  for(i in 1:max_day) {
    day_freq <- table(unlist(lapply(x1, "[[", i)))
    if(length(day_freq)) {
      day_freq_dt <- setNames(as.data.table(day_freq), c("COUNT", "FREQ"))[, lapply(.SD, as.integer)]
      day_freq_lst[[i]] <- day_freq_dt
    } else {
      day_freq_lst[[i]] <- data.table(COUNT = NA_integer_, FREQ = NA_integer_)
    }
  }
  
  rbindlist(day_freq_lst, idcol = "DAY")[!is.na(COUNT)]

}
em_review_backlog_alpha <- em_review_dt[
  , backlog_day(.SD), by = .(DESIGN, DAYS_TO_REVIEW, REVIEWERS),
  .SDcols = c("ITER", "DAY", "REVIEW_START", "COMPLETED")]
em_review_backlog_alpha[, ALPHA := FREQ / 100]

# Make a plot, putting the reviewers on top and backlog below
em_review_ts_plot <- ggplot(em_review_alpha, aes(y = REVIEWED_BY, x = DAY)) + 
  facet_grid(
    REVIEWERS ~ DESIGN + DAYS_TO_REVIEW, scale = "free", space = "free", 
    labeller = labeller(
      DAYS_TO_REVIEW = function(x) paste0("Days to Review: ", x),
      REVIEWERS = function(x) gsub("IEWER", ":", x))) + 
  geom_tile(aes(fill = DESIGN, alpha = ALPHA)) + 
  geom_tile(data = em_review_backlog_alpha, aes(alpha = ALPHA, y = -COUNT, x = DAY)) + 
  scale_fill_manual(values = c("darkorchid4", "chartreuse4")) + 
  scale_alpha_continuous(range = c(0, 1)) + 
  theme(legend.position = "none", strip.text = element_text(margin = margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2 ))) + 
  labs(x = "Day", y = "(-) = # HDs in backlog, (+) = Reviewer busy", subtitle = "With reviewers prioritizing the most recent HD")

if(F) {
  ggsave(
    filename = "analyses/inseason_management/figures/em_review_ts_plot.png",
    plot = em_review_ts_plot, dpi = 600, width = 8, height = 6, units = "in")
}