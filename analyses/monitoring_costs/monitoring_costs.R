#' *Monitoring Costs*
#  `This script sets the monitoring cost parameters for the ADP.

#' [Author: Geoff Mayhew]
#' [StartDate: 2024-Aug-13]


#======================================================================================================================#
# Preparation ----
#======================================================================================================================#

adp_year <- 2025

#===================#
## Load Packages ----
#===================#

library(data.table)
library(ggplot2)            # Plotting
library(FMAtools)
library(odbc)               # For database connectivity

#===================#
## Load Functions ----
#===================#

source("common_functions/open_channel.R")

#========================================#
## Load Rates from the 'current' year ----
#========================================#

gdrive_download("results/final_adp_2024_results.rdata", gdrive_set_dribble("Projects/ADP/Output/") )
current_adp_items <- (load("results/final_adp_2024_results.rdata"))
# Remove everything aside from the rates
rm(list = setdiff(current_adp_items, c("rates_adp_2024_final")))


#======================================================================================================================#
# Observer Costs ----
#======================================================================================================================#

#==================#
## Travel Costs ----
#==================#

#' [2025 Draft] The new PC contract is planned to go into effect October 1st. We can incorporate the day costs for 
#' the preliminary winning contract, but will use prior data from the `FMA Days Paid` spreadsheet to estimate travel 
#' costs. When the contract is finalized, we can update this spreadsheet with new day costs instead of hard-code. 

#' Download and save an updated instance of FMA Days Paid to analyses/monitoring_costs/ folder.
#' TODO [Make a google spreadsheet that mirrors the FMA Days Paid data using IMPORTRANGE() function]
#' [https://docs.google.com/spreadsheets/d/1KGmZNo7uVuB6FCVRZd4UV2PROa4Jl7-J/edit?gid=355780110#gid=355780110]

FMA_days_paid <- lapply(read_xl_allsheets("analyses/monitoring_costs/FMA Days Paid.xlsx"), setDT)

#' [Estimate Travel costs per sea Day]
#' Total number of sea days and travel costs by calendar year.
travel_costs <- FMA_days_paid$Days_Paid[!is.na(Contract), .(
  Sea_Days = sum(Base_Days, Option_Days, na.rm = T), 
  Travel_Dollars = sum(Travel_Dollars, na.rm = T)), 
  keyby = .(Calendar)]
#' Calculate travel costs per sea day
travel_costs[, Travel_CPD := Travel_Dollars / Sea_Days]

# Apply an inflation adjustment for 2025 dollars
#' [https://www.statista.com/statistics/244983/projected-inflation-rate-in-the-united-states/]
inflation_dt <- data.table(
  Calendar = 2017:adp_year,
  Inflation_Pct = c(2.13, 2.44, 1.81, 1.25, 4.69, 8, 4.5, 2.3, 2.1)
)
# Calculate the cumulative inflation percentages, then convert percentage to multiplication factor
inflation_dt[
][, Infl_Cumu := rev(cumsum(rev(Inflation_Pct)))
][, Infl_Factor := 1 + (Infl_Cumu / 100)] 
# Merge in the inflation table to the travel costs table
travel_costs <- inflation_dt[travel_costs, on = .(Calendar)]
travel_costs[, Travel_CPD_infl := Travel_CPD * Infl_Factor]

travel_costs 
#' 2020 and 2021 are outliers because there were a lot of quarantine-associated costs included under 'Travel'.
#' 2022 is still kind of high (still had some quarantine days) but 2023 is on the low side, but otherwise travel cost 
#' per day is quite consistent. 2024 Is not yet completed, so we can't be sure that estimate is accurate. For now, 
#' can take an average of 2022 and 2023 costs to be safe?

#' Calculate an estimate of the travel cost per day to apply to the ADP year.
#' *For the 2025 ADP, I'm simply taking the average of the 2 full years post-COVID: years 2022 and 2023*
travel_cpd <- travel_costs[Calendar %in% 2022:2023, mean(Travel_CPD_infl)]

#===================#
## Sea Day Costs ---- 
#===================#

#' Calculating sea day costs is always complicated because the ADP uses the calendar year whereas the partial coverage
#' contract changes mid-year and sometimes mid-month. This affects how many guaranteed and optional days we can expect
#' each ADP year, since we start the ADP year mid-contract where typically some number of guaranteed days were already
#' realized. The ADP therefore has to estimate the number of guaranteed days we start the ADP year on, as well as the 
#' number of remaining guaranteed days that are used until the optional days are used, and calculate how many optional 
#' days are used before the contract changes and starts the counter for guaranteed days back to zero, and count the 
#' number of guaranteed days will be used on the new conract. Get it?


#==================================================================================#
#' [2025 Draft]  *Remove this hard-coding once FMA Days Paid is updated* ----

#' Option Year 4 (the current contract) was extended to September 30. 
#' *However, Lisa is currently negotiating new day rates for the Aug-Sep range with AIS for this extension*
FMA_days_paid$Sea_Day_Costs[Contract == "Aug23Aug24", Dates := "8/17/23 - 9/30/24"]
#' Add values for the preliminary winner for the PC observer contract
FMA_days_paid$Sea_Day_Costs <- rbind(
  FMA_days_paid$Sea_Day_Costs,
  data.table(
    Contract = paste0(paste0("Oct", 24:28), paste0("Sep", 25:29)),
    Dates = paste0(paste0("10/01/", 24:28), " - ", paste0("9/30/", 25:29)),
    Option_Year = c("Base Period", paste0("Option Year ", 1:4)),
    Base_Day_Cost = c(1695.58, 1754.93, 1807.58, 1857.29, 1899.08),
    Optional_Day_Cost = c(877.73, 908.45, 935.70, 961.43, 983.06),
    Quarantine_Day_Cost = rep(NA, 5),
    Quarantine_Travel = rep(NA, 5),
    Plant_Day_Cost = c(517.62, 535.74, 551.81, 566.98, 579.74),
    Optional_Plant_Day_Cost = c(545.37, 564.46, 581.39, 597.38, 610.82)  #' TODO Add this to FMA Days Paid spreadsheet?
  ), fill = T
)

FMA_days_paid$Sea_Day_Costs

# Note that the new contract has optional day cost that are significantly higher than those in the last contract,
# so even thought he base day cost is higher given the lower minimum cost, this contract isn't really affording us much
# more than before...
# Not sure how optional day costs jumped up 20% between Option year 4 and the base year...

#' [CHAT WITH LISA ABOUT THIS]
#' -- Base day costed jumped up 20%, which might make sense with a lower # of guaranteed days, but Optional day costs 
#' also a lot higher than before, also jumping up 20%, so I don't see how we are affording any more.
#' -- Optional Plant days are more expensive than Guaranteed Plant days. Why?

#================================================#
# Proportion of days before contract changes` ----
#================================================#

#' *Do I need all of this or just contract_end?*

#' Run `selection_rates.R` to create the `pc_effort_st` object. We will use the most recent year to calculate the 
#' proportion of trips that occur before and after the year-day the contract year changes.

#' Subset all OB trips and their date ranges
ob_trips_adp <- pc_effort_st[
  STRATA %like% "OB_", 
  .(START = min(TRIP_TARGET_DATE, LANDING_DATE), END = max(TRIP_TARGET_DATE, LANDING_DATE)),
  keyby = .(ADP, TRIP_ID)]

#' Identify the contracts pertinent to the ADP year
adp_contract_sea_day <- copy(FMA_days_paid$Sea_Day_Costs)[
][grepl(paste0("[[:alpha:]]{3}", substr(adp_year, 3, 4)), Contract) ]
# Get the final day of the contract that is ending during the ADP year
contract_end <- as.Date(gsub("^.* - ", "", adp_contract_sea_day[1, Dates]), format = "%m/%d/%y")

#' For each trip, create a vector of each date between fishing start and end. Calculate the proportion of days that 
#' occurred on or before the contract changes
ob_dates <- as.Date(unlist(
  apply(ob_trips_adp, 1, function(x) seq(as.integer(as.Date(x["START"])), as.integer(as.Date(x["END"])), 1))
))
ob_contract_prop <- sum(yday(ob_dates) <= yday(contract_end)) / length(ob_dates) 
ob_contract_prop

#' Roughly, we can apply the total number of Observer sea Days to the proportion before/after the contract changes. But,
#' if fishing effort differs among strata greatly, could this result in a bias?
#' The ob_cost() function could instead take the contract_end yday(), count observed days using the rates, and attribute the costs?
#' ob_cost currently takes the yearly summary of rates, n, and TRP_DUR
adp_contract_sea_day

#==================================================================================#

## Number of guaranteed days on contract  ----

#' The ADP year start mid-contract, so there will be some guaranteed sea days already on the contract. We will estimate
#' this using the previous year's fishing effort. Given that the contract changes no October 1st, it is not likely we 
#' will have any actual data in time for the Draft ADP, and the Final ADP may only have actual day counts for October. 

#' Identify trips that start after the contract change date. This will result in a underestimate of the number of 
#' guaranteed days on the contract, which is a more conservative approach for budgeting purposes than using end date.
#' This is an underestimate because trips that start before or on the contract change date may have fishing days after
#' the new contract begins.
current_contract_days.smry <- pc_effort_st[
][STRATA %like% "OB_", .(
  START = min(TRIP_TARGET_DATE, LANDING_DATE), END = max(TRIP_TARGET_DATE, LANDING_DATE)
  ), keyby = .(STRATA, TRIP_ID, DAYS)
][yday(START) > yday(contract_end), .(OB_DAYS = sum(DAYS)), keyby = .(STRATA)]
# Merge in 'current' ADP's monitoring rates and estimate observed days
current_contract_days.smry[
][, SAMPLE_RATE := rates_adp_2024_final[current_contract_days.smry, SAMPLE_RATE, on = .(STRATA)]
][, OB_d := OB_DAYS * SAMPLE_RATE]
current_contract_days <- sum(current_contract_days.smry$OB_d)

#==================================================================================#
# EM Trawl Costs ----
#==================================================================================#

#' Per conversation with [Caren Braby], new trawl EM equipment installs cost *~$15K* and yearly maintenance costs *~$5K* 
#' per vessel. Can e-mail [Courtney Paiva] for review costs.

#' Per [Jennifer Cahalan], planning to have 2 observers each at two stations in Kodiak, so 4 observers during the 
#' fishing seasons. 


trawl_em_goa_v_count <- length(na.omit(unlist(unname(setDT(readxl::read_xlsx("source_data/2024 EM EFP Vessel List_NMFS.xlsx", col_names = F))[-c(1:3), 7]))))
trawl_em_goa_v_count * 5000  # ~ $200K

#' Assuming 150 sea days among 4 observers, that's a season of 37.5 plant days. Will likely need more plant days than
#' the guaranteed minimum, which are marginally more expensive per unit. 
em_trw_ves_id <- work.data[STRATA == "EM TRW EFP" & COVERAGE_TYPE == "PARTIAL", unique(VESSEL_ID)]
em_trw_trips <- work.data[ADP == 2023 & VESSEL_ID %in% em_trw_ves_id & TRIP_TARGET_CODE %in% c("C", "P")][, .(START = min(TRIP_TARGET_DATE, LANDING_DATE, na.rm = T), END = max(TRIP_TARGET_DATE, LANDING_DATE, na.rm = T)), keyby = .(TRIP_ID)]
em_trw_days <- apply(em_trw_trips, 1, function(x) unique(seq(
  julian.Date(as.Date(x["START"])),
  julian.Date(as.Date(x["END"]))
)), simplify = F)
em_trw_dates <- data.table(DATE = as.Date(unlist(em_trw_days)))
ggplot(em_trw_dates, aes(x = DATE)) + geom_histogram() + scale_x_date()
uniqueN(em_trw_dates$DATE) # We have fishing by these vessels over 237 days of the year. 
# Using this as a rough estimate, we will need 237 * 4 plant days
plant_days_needed <- 237 * 4

# For now, assuming all these days are within the first contract year, not accounting for any after Oct1 in option year 1
adp_year_plant_day_costs <- FMA_days_paid$Sea_Day_Costs[Contract == "Oct24Sep25"]
(150 * adp_year_plant_day_costs$Plant_Day_Cost) + ((plant_days_needed - 150) * adp_year_plant_day_costs$Optional_Plant_Day_Cost)
# Just these sea days is over $500K?

# Email from Chelsae Raddell 2024-Jun-24 had:
# 2023: 59 + 91 + 104 = 254 total sea days
# 2024: 248 + 296 + 106 = 650 total sea days.
# However, I think we are reducing the number of observers at the plants

channel <- open_channel()

# Get assignments for observers at GOA Plants? They don't show up in NORPAC.ODDS_CNT_PLANT_BILLABLE_DAY_V, only for 2021?

kodiak_plant_days <- setDT(dbGetQuery(channel, paste(
  "
  SELECT a.*, b.name, b.port_code
  FROM norpac.ols_vessel_plant a
    JOIN norpac.atl_lov_plant b
      ON a.permit = b.permit
  WHERE b.port_code = 14 AND EXTRACT(YEAR FROM a.embark_date) = 2023
  ORDER BY EMBARK_DATE
  "
)))
#' Total up assigned plant days in 2023. Some observers were assigned to multiple plants at the same time, so count 
#' unique days by CRUISE
kodiak_plant_days[, ':=' (EMBARK_DATE = as.Date(EMBARK_DATE), DISEMBARK_DATE = as.Date(DISEMBARK_DATE))]

assign_dates <- apply(kodiak_plant_days, 1, function(x) data.table(DATE = seq(julian.Date(as.Date(x["EMBARK_DATE"])), julian.Date(as.Date(x["DISEMBARK_DATE"])))), simplify = F)
names(assign_dates) <- kodiak_plant_days$CRUISE
assign_dates_dt <- rbindlist(assign_dates, idcol = "CRUISE")[, .(DAYS = uniqueN(DATE)), keyby = .(CRUISE)]
assign_dates_dt[, sum(DAYS)]  # 519 unique days of plant observers assigned in 2023, a lot more than the 254 reported by Chelsae???


# Can see that before, we sometimes had more than 2 observers at the same plant
ggplot(kodiak_plant_days) + 
  facet_grid(NAME ~ .) + 
  geom_linerange(aes(xmin = EMBARK_DATE, xmax = DISEMBARK_DATE, y = as.factor(CRUISE))) + 
  scale_x_date(date_breaks = "1 month", labels = month, minor_breaks = NULL)
# How many unique days did we have?
rbindlist(assign_dates, idcol = "CRUISE")[, uniqueN(DATE)]  # 144 unique dates in 2023
# So even if we had 144 unique dates in 2023, with 4 observers that is still 576 days or $310K, putting our total at $500K before travel
(150 * adp_year_plant_day_costs$Plant_Day_Cost) + ((576 - 150) * adp_year_plant_day_costs$Optional_Plant_Day_Cost)

#' TODO [I'm pretty sure, but make sure the 'plant day rate' is NOT inclusive of the lodging costs, which are under travel, right?]


a <- pc_effort_st[STRATA == "EM_TRW-GOA", .(ADP = as.factor(2023), START = min(TRIP_TARGET_DATE, LANDING_DATE), END = max(TRIP_TARGET_DATE, LANDING_DATE)), keyby = .(TRIP_ID, AGENCY_GEAR_CODE)]
a[, RAW := as.numeric(END - START, units = "days")]
a[, DAYS := round(predict(td_mod$TD_MOD, newdata = a) / 0.5) * 0.5]
a[, sum(DAYS) * 0.10] * 1985.53   # 10% of these trips would cost 168.95 days and cost 335K
a[, sum(DAYS) * 0.15] * 1985.53   # at 15% it would cost 500K

#' TODO [Come up with a new cost function?]
ob_cost

new_contract_costs <- FMA_days_paid$Sea_Day_Costs[Contract == "Oct24Sep25"]


cost_params$OB





#' In reality, we should be using the new contract costs for Jan thru Sep, then option year 1 for Oct thru Dec
#' For now, assuming just base year

new_ob_params <- list(
  sea_day_min = 1200,
  sea_day_rate_gua = FMA_days_paid$Sea_Day_Costs[Contract == "Oct24Sep25", Base_Day_Cost],
  sea_day_rate_opt = FMA_days_paid$Sea_Day_Costs[Contract == "Oct24Sep25", Optional_Day_Cost],
  travel_day_rate = travel_cpd
)
new_cost_params <- copy(cost_params)
new_cost_params$OB <- new_ob_params



ob_cost(ob_strata_summary, cost_params = cost_params)  # Old ADP values had cost-per-day of ~$1963.67

ob_cost_new(ob_strata_summary, cost_params = new_cost_params)  # Old ADP values had cost-per-day of ~$1963.67
