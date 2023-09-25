# 2024 Draft ADP : Exploratory stuff for PCFMAC

# Author: Geoff Mayhew
# Start Date: 11 Sep 2024

#======================================================================================================================#
# Preparation ----------------------------------------------------------------------------------------------------------
#======================================================================================================================#

#===================#
## Load Packages ----
#===================#

library(data.table)         # Data wrangling
library(ggplot2)            # Plotting
library(scales)             # for defining new scales
library(ggh4x)              # for nested facets
library(grid)               # For unit.pmax  to get widths of grobs so that plots have matching dimensions
library(gridExtra)          # For arrangeGrob to combine plots
library(sf)                 # Spatial analyses
library(flextable)          # For print-ready tables
library(dplyr)              # For piping and handling sf objects

#=============================#
## Load data and data prep ----
#=============================#

load("analyses/draft_rates/draft_rates_2.rdata")           # Raw output from nalyses/draft_rates/draft_rates.R - box definitions and raw rates
load("analyses/draft_rates/draft_rates_effort_2.rdata")    # Outputs from analyses/draft_rates/draft_rates.R - compiled rates and effort
load("~/GitHub/2024_ADP/source_data/2024_Draft_ADP_data.rdata")   # Most recent valhalla version

#===============#
## Functions ----
#===============#

source("analyses/allocation_evaluation/functions.R")

#======================#
# TRIPS PER VESSEL #####
#======================#

#Distribution of Trips per vessel per monitoring method/gear
trips_per_vessel <- unique(pc_effort.CURRENT[ADP == 2022, .(STRATUM_COL, PERMIT, TRIP_ID)])[STRATUM_COL != "ZERO"]
trips_per_vessel[, METHOD := fcase(STRATUM_COL %like% "OB", "OB", STRATUM_COL %like% "EM_TRW", "EM_TRW" , STRATUM_COL %like% "EM_HAL|EM_POT", "EM_FG")]
trips_per_vessel <- trips_per_vessel[, .(N = uniqueN(TRIP_ID)), keyby = .(METHOD, PERMIT)]
ggplot(trips_per_vessel, aes(x = N)) + facet_nested(METHOD ~ ., scales = "free_y", space = "free_y") + geom_histogram()
# Looks like a lot of EM_HAL vessels 

ggplot(trips_per_vessel[METHOD == "EM_FG"], aes(x = N)) + geom_histogram(bins = 30)  # Many vessels take fewer than 5 trips
trips_per_vessel[METHOD == "EM_FG"]  # 137 vessels in 2022 
trips_per_vessel[METHOD == "EM_FG"][N <= 5]  # 61 fished 5 or fewer trips
trips_per_vessel[METHOD == "EM_FG"][N <= 2]  # 25 fished 1-2 or fewer trips, plus 172-137 = 35 trips that fished 0 trips

# At 30%, fishing two trips mean you're as likely as not to get selected





# Look at actual VALHALLA to see how many actually were monitored?

val_2022_fgem <- unique(work.data[ADP == 2022 & STRATA %like% "EM_HAL|EM_POT", .(TRIP_ID, VESSEL_ID, STRATA, STRATA_NEW, OBSERVED_FLAG)])
val_2022_fgem[, uniqueN(VESSEL_ID)]  # 128 fixed-gear em vessels in 2022 (pool size was 172 something?)
val_2022_fgem[OBSERVED_FLAG == "Y", uniqueN(VESSEL_ID)]   # Only 82 actually actually had a monitored trip

val_2022_fgem[, uniqueN(TRIP_ID)]   # 1007 trips
val_2022_fgem[OBSERVED_FLAG == "Y", uniqueN(TRIP_ID)]   # 218 monitored (is this reviewed? Only 21.7%, not 30% of trips monitored?)
# assuming 30%, randomized

val_2022_fgem
iter <- 1e5
set.seed(12345)
fgem_sample_lst <- vector(mode = "integer", length = iter)
for(i in seq_len(iter)) fgem_sample_lst[i] <- uniqueN(val_2022_fgem[runif(n = .N) < 0.3]$VESSEL_ID)
mean(fgem_sample_lst)  # 104-105 vessels on average expected to be sampled
val_2022_fgem[, .(N = uniqueN(TRIP_ID)), by = .(VESSEL_ID)][, mean(1 - (0.7^N)) * uniqueN(VESSEL_ID)]  # Mathematical expectation
hist(fgem_sample_lst)
quantile(fgem_sample_lst, c(0.10, 0.25, 0.5, 0.75, 0.9))

#==============================#
# How many vessels would need to be moved? ####
#==============================#

# Falvey's ideas:

# 1) zero selection (move portion of at-sea observer group into zero selection): 
# -- Keep 40ft requirement, add 100-200 vessels when 'costs stabilize'?
# -- Use previous catch characteristics to determine 'core' monitored group, second group for more expensive/less important
# trips. Randomly select a portion of second group
# -- Adjust the number of vessels in second group to boost selection rates in monitored group.
# -- Wants to know how many vessels would have to be taken out to afford 30% selection rates and 50% on trawl.

# Rebuttal: There are very few 'expensive' at-sea observer trips - just a handful in the Aleutians, but these are 
# arguably more important trips to monitor because of the spatiotemporal uniqueness of these trips. We have not ever
# analyzed the cost of each trip - this is not impossible but it does require us to track each observer, where they fly
# and the associated costs, and keep this is context with all of the vessels and ports each observer may be assigned to.
# Just for comparison's sake, if we wanted to get from a 15% to a 30% monitoring rate on fixed-gear vessels, we would 
# have to to move approximately half of these vessels into zero coverage. It's not like half of the vessels are cost-
# ineffective! We would have to move even more vessels into zero if you wanted to achieve a 50% monitoring rate on 
# 'PSC-limited' fisheries, which I assume is to be interpreted as trawl fisheries. Restricting our sample frame by creating
# a large zero-coverage fleet would only potentially worsen bias issues, may cause changes to fishermen behavior (moving
# IFQ permit holders to vessels in the 'unmonitored fleet' every year) and creates a larger disparity in the burden of 
# monitoring. 
# TLDR: This would result in a minute cost efficiency and NOT improve data quality, but would likely worsen it. We would
# be trading our current observer effect (which so far has not been detected as large) with a potentially very large bias,
# would encourage changes to fishermen behavior and less equitable burden of monitoring.

# 2) Fixed gear EM (again, move less cost-effective vessels into the zero pool)
# -- Have all vessels use their monitoring system on their first trip each year for data quality purposes, only use the
# data for those that were selected.
# Conclusion: We have already made the investment to equip vessels to join the EM pool. To move them into the zero pool
# might reduce maintenance costs, but it's entirely wasting the cost of the equipment. Having EM vessels use their gear
# every year for data quality purposes should help provide better feedback, but this is not really a cost-efficiency 
# improvement. The additional review of these trips does have a cost as well. Moving cost-inefficient EM boats into the
# observer pool is not possible with current regulations, but this would improve cost-efficiency without creating bias 
# issues.

# 3) Shore-based observers. - Have observers live in communities - This is covered in the draft ADP for trawl EM, but
# applying this to the rest of partial coverage is much more complex (many ports, would require boats to travel and pick
# up their observers)

# 4) At-sea observers. - Having separate stock-assessment observers deployed opportunistically
# We don't really have 'idle' observers, and using an opportunistic method of collecting data is not advised. Observers
# are already trained to collect all data types, and they do prioritize their data collections depending on the gear
# their vessel fishes and what is targeted. See the observer manual to see what observer priorities are.  Our issue with 
# not getting more stock assessment data can be handled through stratification/allocation and ensuring fixed-gear EM 
# vessels are not displacing observers in time/spaces where more sampling opportunities are needed.

#==============================#
# Rates required to hit 15% ####
#==============================#

# NOTE this isn't even to get to the 95% prob of getting 15%, it's just to 15%!
allo_status_quo(current.allo_lst, budget = 6.664e6, cost_dt, cost_params)[ADP == 2022]  # Budget of $6,664,000 required to get 15% under STATUS QUO
allo_equal2(cost_dt, budget = 6.047e6, current.allo_lst, cost_params)[ADP == 2022]      # Budget of $6,047,000 required to get 15% under MIN_PLUS_OPT all

#=========================================#
# How many vessels actually cross FMP? ####
#=========================================#

multi_fmp_2022 <- unique(pc_effort.CURRENT[ADP == 2022, .(TRIP_ID, PERMIT, STRATA, AREA)])
multi_fmp_2022[, FMP := fcase(AREA >600, "GOA", AREA <= 600, "BSAI")]
multi_fmp_2022 <- unique(multi_fmp_2022[, .(TRIP_ID, PERMIT, STRATA, FMP)])
multi_fmp_2022[, FMP_N := uniqueN(FMP), keyby = .(TRIP_ID)]

# Number of vessels that crossed FMP in 2022, with percentages, broken out with current stratifications
multi_fmp_2022_smry <- multi_fmp_2022[, .(
  TOTAL_VES = uniqueN(PERMIT),
  TOTAL_TRIPS = uniqueN(TRIP_ID),
  MULTI_FMP_VES = uniqueN(PERMIT[FMP_N > 1]), 
  MULTI_FMP_TRIPS = uniqueN(TRIP_ID[FMP_N > 1])), keyby = .(STRATA)
][, ':=' (
  PERC_VES = 100 * round(MULTI_FMP_VES / TOTAL_VES, 4), 
  PERC_TRIPS = 100 * round(MULTI_FMP_TRIPS / TOTAL_TRIPS, 4))
][, .(STRATA, TOTAL_VES, MULTI_FMP_VES, PERC_VES, TOTAL_TRIPS, MULTI_FMP_TRIPS, PERC_TRIPS)]
multi_fmp_2022_smry
# This was added to Figure 3-1 in the 2024 Draft ADP version to the council (not to the PCFMAC version)

#===================================#
# EM Equipment Replacement Costs ####
#===================================#

# From Ferdinand
# The replacement costs for both trawl and fixed gear run about the same right now. The two companies have pretty 
# different costs, but they still probably average [$12K] or so per vessel.
# I think the replacement lifespan for systems is closer to [7] years, but neither EM company is really very transparent
# when it comes to that!