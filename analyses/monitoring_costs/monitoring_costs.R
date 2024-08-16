#' *Monitoring Costs*
#  `This script sets the monitoring cost parameters for the ADP.

#' [Author: Geoff Mayhew]
#' [StartDate: 2024-Aug-13]


#======================================================================================================================#
# Preparation ----
#======================================================================================================================#

adp_year <- 2025  #' This should come from get_data.R

#' *PREREQUISITE* [First run selection_rates.R to create pc_effort_st object and upload it to the Gdrive]

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

#===============#
## Load Data ----
#===============#

#====================#
### pc_effort_st ----

#' TODO upload `pc_effort_st` to the shared Google drive
# gdrive_download()
# load()

#====================#
### FMA Days Paid ----

#' [2025 Draft] The new PC contract is planned to go into effect October 1st. We can incorporate the day costs for 
#' the preliminary winning contract, but will use prior data from the `FMA Days Paid` spreadsheet to estimate travel 
#' costs. When the contract is finalized, we can update this spreadsheet with new day costs instead of hard-code. 
#' `This spreadsheet includes day rates for both at-sea and shoreside observers`

#' Download and save an updated instance of FMA Days Paid to analyses/monitoring_costs/ folder.
#' TODO [Make a google spreadsheet that mirrors the FMA Days Paid data using IMPORTRANGE() function]
#' [https://docs.google.com/spreadsheets/d/1KGmZNo7uVuB6FCVRZd4UV2PROa4Jl7-J/edit?gid=355780110#gid=355780110]

FMA_days_paid <- lapply(read_xl_allsheets("analyses/monitoring_costs/FMA Days Paid.xlsx"), setDT)

#====================================#
### Rates from the 'current' year ----

#' The 'current' year is the year before `adp_year`, i.e., the year we are currently in as we plan for the next year.

gdrive_download("results/final_adp_2024_results.rdata", gdrive_set_dribble("Projects/ADP/Output/") )
current_adp_items <- (load("results/final_adp_2024_results.rdata"))
# Remove everything aside from the rates
rm(list = setdiff(current_adp_items, c("rates_adp_2024_final")))

#=================#
### Data Pulls ----

channel <- open_channel()

#### Kodiak Plant Days ----

# Get assignments for observers at Kodiak plants as well as their deliveries
kodiak_plant_days <- setDT(dbGetQuery(channel, paste(
  "
  SELECT 
    a.*, b.name, b.port_code, EXTRACT(YEAR FROM a.embark_date) AS YEAR,
    c.delivery_vessel_adfg, c.gear_type_code, c.delivery_end_date, c.nmfs_area, c.landing_report_id
  FROM norpac.ols_vessel_plant a
    JOIN norpac.atl_lov_plant b
      ON a.permit = b.permit
    LEFT JOIN norpac.atl_offload c
      ON a.cruise = c.cruise AND b.permit = c.permit
  WHERE b.port_code = 14 AND EXTRACT(YEAR FROM a.embark_date) >= 2023
  ORDER BY EMBARK_DATE
  "
)))

#======================================================================================================================#
# AT-SEA OBSERVER COSTS ----
#======================================================================================================================#

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
    Optional_Plant_Day_Cost = c(545.37, 564.46, 581.39, 597.38, 610.82)  #' TODO Add this column to FMA Days Paid spreadsheet
  ), fill = T
)
#==================================================================================#

#' Identify the contracts pertinent to the ADP year
adp_contract_day_rates <- copy(FMA_days_paid$Sea_Day_Costs)[
][grepl(paste0("[[:alpha:]]{3}", substr(adp_year, 3, 4)), Contract)]
# Get the final day of the contract that is ending during the ADP year
contract_end <- as.Date(gsub("^.* - ", "", adp_contract_day_rates[1, Dates]), format = "%m/%d/%y")

#==================#
## Travel Costs ----
#==================#

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

### Proportion of days before contract changes` ----

#' Run `selection_rates.R` to create the `pc_effort_st` object. We will use the most recent year to calculate the 
#' proportion of trips that occur before and after the year-day the contract year changes.

#' Subset all OB trips and their date ranges
ob_trips_adp <- pc_effort_st[
  STRATA %like% "OB_", 
  .(START = min(TRIP_TARGET_DATE, LANDING_DATE), END = max(TRIP_TARGET_DATE, LANDING_DATE)),
  keyby = .(ADP, TRIP_ID)]

#' For each trip, create a vector of each date between fishing start and end. Calculate the proportion of days that 
#' occurred on or before the contract changes
ob_dates <- as.Date(unlist(
  apply(ob_trips_adp, 1, function(x) seq(as.integer(as.Date(x["START"])), as.integer(as.Date(x["END"])), 1))
))
ob_contract_prop <- sum(yday(ob_dates) <= yday(contract_end)) / length(ob_dates) 
ob_contract_prop


#' TODO APPLY THIS
#' 
#' 


#' Roughly, we can apply the total number of Observer sea Days to the proportion before/after the contract changes. But,
#' if fishing effort differs among strata greatly, could this result in a bias?
#' The ob_cost() function could instead take the contract_end yday(), count observed days using the rates, and attribute the costs?
#' ob_cost currently takes the yearly summary of rates, n, and TRP_DUR
adp_contract_sea_day

### Number of guaranteed days on contract  ----

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
current_contract_days <- round(sum(current_contract_days.smry$OB_d))

#=============================#
## [Summary] ----
#=============================#

current_contract_days    #' [Estimated number of days on the contract when ADP year starts]
travel_cpd               #' [Estimated cost of travel per sea day]


#======================================================================================================================#
# FIXED-GEAR EM COSTS ----
#======================================================================================================================#

#' TODO 

#=====================#
## Equipment Costs ----
#=====================#


#========================#
## Video Review Costs ----
#========================#


## [Summary] ----



#==================================================================================#
# EM TRAWL COSTS ----
#==================================================================================#

#=========================#
## GOA Plant Day Costs ---- 
#=========================#

#' [2025] Per [https://docs.google.com/document/d/12iFmlgZlk9vL0-ML583tqiUMDMhXK9ucQ3clxUwu35A/edit], planning to have 
#' *2* observers at Kodiak Trident to cover both lines during 12-hour shifts , and *3* observers to cover the other 3 
#' Kodiak Plants, SBS, APS, OBI,  so *5* total observers during the fishing seasons. Need to estimate the duration of
#' the fishing season as total days of the year. Whether Sand Point or False Pass will operate during 2025 is unknown,
#' so we will assume they will not operate and will not be accounted for.

goa_plant_obs <- 5

#=============================#
### Count of GOA Plant Days ----
#=============================#

date_cols <- c("EMBARK_DATE", "DISEMBARK_DATE", "DELIVERY_END_DATE")
kodiak_plant_days[, (date_cols) := lapply(.SD, as.Date), .SDcols = date_cols]
kodiak_assignments <- unique(kodiak_plant_days[, .(YEAR, CRUISE, PERMIT, EMBARK_DATE, DISEMBARK_DATE, NAME)])

# 2023 A and B season and 2024 A season. 
fig.kodiak_plant_deliveries <- ggplot(kodiak_assignments, aes(y = as.factor(CRUISE)) ) + 
  facet_grid(NAME ~ ., scales = "free_y", space = "free_y") + 
  geom_linerange(aes(xmin = EMBARK_DATE, xmax = DISEMBARK_DATE), linewidth = 1) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y", minor_breaks = NULL) + 
  geom_vline(xintercept = as.Date("2024-01-01")) + 
  labs(
    x = "Month-Year", y = "Cruise", subtitle = "GOA plant assignments (black) and deliveries (blue) 2023-2024") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  geom_point(data = kodiak_plant_days, aes(x = DELIVERY_END_DATE), color = "blue", na.rm = T)
fig.kodiak_plant_deliveries
#' Note that assignments and deliveries during B season were pretty sparse, but span a 2+ month range.

#' Total up assigned plant days each year. Some observers were assigned to multiple plants at the same time, so count 
#' unique days by CRUISE
assign_dates <- apply(kodiak_assignments, 1, function(x) data.table(DATE = seq(julian.Date(as.Date(x["EMBARK_DATE"])), julian.Date(as.Date(x["DISEMBARK_DATE"])))), simplify = F)
names(assign_dates) <- kodiak_assignments$CRUISE
assign_dates_dt <- rbindlist(assign_dates, idcol = "CRUISE")[, .(DAYS = uniqueN(DATE)), keyby = .(CRUISE)]
cruise_year <- unique(kodiak_assignments[, .(CRUISE = as.character(CRUISE), YEAR)])
assign_dates_dt <- assign_dates_dt[cruise_year, on = .(CRUISE)]
assign_dates_dt[, .(CRUISE_N = uniqueN(CRUISE), DAYS = sum(DAYS)), keyby = .(YEAR)]

#' Total of 519 unique CRUISE x days of plant observers assigned in 2023. According to e-mail from [Chelsae Radell] sent
#' 2024-Jun-24, 2023 planned 279 days in A season and 150 days in B season = 429 days, plus another 104 days at Sand 
#' Point. Not sure why there is a large discrepancy of 90 days. The 517 plant days in 2024 was only during A season, as 
#' there were 3 observers stationed at APA/OBI/SBS and 4 observers stationed at Trident-Kodiak.

#' How many days were an observer assigned each year?
unique_assigned_days <- rbindlist(assign_dates, idcol = "CRUISE")[
][cruise_year, on = .(CRUISE)
][, .(DAYS = uniqueN(DATE)), keyby = .(YEAR)]
unique_assigned_days

#' 144 days during 2023 A and B season, 85 days for 2024 A season only since this analysis is occurring mid-Aug. We will
#' round this up to 150 days for the season in 2025 
goa_season_days <- 150

#' Assuming all plant observers are active all days of the season, calculate the number of plant days
goa_plant_ob_days <- goa_season_days * goa_plant_obs

#' [2025 ADP ONLY]
#' The Trawl EM EFP will run through Dec 31 of 2024, so the partial coverage fee will not be used to fund Trawl EM until
#' Jan 1 2025. Therefore, there will not be any days already on the contract like there will be for at-sea observers.
#' However, we will need to being estimating the number of days that will be on the contract starting in 2026.

contract_days <- rbindlist(assign_dates, idcol = "CRUISE")[
][cruise_year, on = .(CRUISE)][
][, DATE := as.Date(DATE)
][, CONTRACT_HALF := ifelse(yday(DATE) <= yday(contract_end), 1, 2)
][, .(DAYS = uniqueN(DATE)), keyby = .(YEAR, CONTRACT_HALF)
][, PROP := DAYS / sum(DAYS), keyby = .(YEAR)][]
# Apply the estimated length of the season to the proportions before and after the contract change date
contract_days[YEAR == adp_year - 2,] 

plant_sea_day_costs <- adp_contract_day_rates[
][, .(CONTRACT_HALF = 1:2, Plant_Day_Cost, Optional_Plant_Day_Cost)
][contract_days[YEAR == adp_year - 2,], on = .(CONTRACT_HALF)]
plant_sea_day_costs[, EST_DAYS := PROP * goa_season_days * goa_plant_obs]
# The PC contract has 150 guaranteed plant days. These will all occur before the contract  year changes
plant_sea_day_costs[, GUA_COST := 150 * Plant_Day_Cost]
# Calculate the cost of the number of optional days purchased before the contract change date
plant_sea_day_costs[, OPT_COST := (EST_DAYS - 150) * Optional_Plant_Day_Cost]

trw_em_day_costs <- sum(plant_sea_day_costs[, .(GUA_COST, OPT_COST)])

#' *Summary*
goa_plant_obs     #' Number of observers during fishing season
goa_season_days   #' Total number of days in pollock season
goa_plant_ob_days #' Total number of sea days, as  number of observers X number of days in season
trw_em_day_costs  #' Total cost of sea days, accounting for guaranteed and optional days on both contracts

#==================#
## Travel Costs ---- 
#==================#

#' Since there are only 5 observers and they won't be traveling much, I'm not accounting for air travel costs.

### Lodging ----

#' Per e-mail from [Lisa Thompson 2024-Aug-16], lodging and per diem rates.

#' Use the government lodging rates to estimate the cost per room day. Varies at different times of year.
#' Mar-1 to Sep-30 = ($223 per night) aka PERIOD = 1
#' Oct-1 to Feb-28 = ($121 per night) aka PERIOD = 2
kodiak_lodging_dt <- data.table(PERIOD = 1:2, COST_PER_NIGHT = c(223, 121))

### Per Diem ----

#' Assuming the Kodiak plant observers do not have a food plan organized between the plant and the PC contract holder,
#' we are obligated to pay per-diem rates of $109 per day.
goa_plant_per_diem <- 109

#=================================#
## Data and Video Review Costs ---- 
#=================================#

#' The 2022 Trawl EM EA-RIR provides an estimate for data and video review costs in [Table- E-1-3]
#' [https://meetings.npfmc.org/CommentReview/DownloadFile?p=e31b9c56-d3a4-4d1e-b621-b0b4bd892b5a.pdf&fileName=C3%20Trawl%20EM%20Analysis.pdf]

em_trw.data_transmittal <- 5720
em_trw.data_review <- 101488
em_trw.data_processing_storage <- 9403
em_trw.review_days <- 4882
em_trw_data_cost <- (em_trw.data_transmittal + em_trw.data_review + em_trw.data_processing_storage) / em_trw.review_days
# This is an estimate for costs in 2021, so inflation-adjust this to 2025
em_trw_data_cpd <- em_trw_data_cost * inflation_dt[Calendar == 2021, Infl_Factor]

### Count of days to review ----

#' This cost assumes the number of review days in pc_effort_object, not in any effort predictions
em_trw_review_ND <- unique(pc_effort_st[STRATA == "EM_TRW-GOA", .(ADP, TRIP_ID, DAYS)])[
][, .(N = uniqueN(TRIP_ID), D = sum(DAYS)), by = .(ADP)]
em_trw_review_days <- em_trw_review_ND$D

#' Calculate total cost of review in ADP year
em_trw_total_data_cost <- em_trw_review_days * em_trw_data_cpd

#=====================#
## Equipment Costs ----
#=====================#

#' The cost of installation and maintenance of equipment for GOA-only EM Trawl vessels comes from the fee, so we will
#' multiply the estimated costs by the number of GOA-only vessels. However, according to [Chelsae Radell] and 
#' [Jennifer Ferdinand] for 2025, the installation costs for new vessels will be paid for by he Murkowski funds (held 
#' by PSMFC), so we will only account for maintenance costs. Per conversation with [Caren Braby], new trawl EM 
#' equipment installs cost *~$15K* and yearly maintenance costs *~$5K* per vessel. 
equipment_upkeep_per_VY <- 5000

#===============================#
### Count of GOA-only vessels ----
#===============================#

#' Count the total of GOA-only trawl EM vessels to apply to the maintenance costs.
emtrw_goa_v <- setnames(
  data.table(readxl::read_xlsx("source_data/EM_TRW 2025 Vessel List for NMFS_July192024.xlsx", col_names = F))[-(1:4), 7:8],
  new = c("VESSEL_NAME", "STATUS")
)[!is.na(VESSEL_NAME)]
#' [2025 DRAFT] Chelsae Radell listed 3 vessels as 'maybe' for joining trawl EM in 2025. Will not include these in the 
#' draft and will wait for vessel to formally declare before the Final ADP.
emtrw_goa_v_count <- trawl_em_goa_v[STATUS != "Maybe?", .N]


### ***MOVE THIS ***Lodging Costs P2 ---- 

#' Mar-1 to Sep-30 = ($223 per night) aka PERIOD = 1
#' Oct-1 to Feb-28 = ($121 per night) aka PERIOD = 2

cruise_date <- unique(
  rbindlist(assign_dates, idcol = "CRUISE")[
  ][cruise_year, on = .(CRUISE)
  ][, DATE := as.Date(DATE)
  ][, .(YEAR, CRUISE, DATE)]
)

year_date_period <- unique(cruise_date[YEAR == adp_year - 2, .(YEAR, DATE)])[
][, PERIOD := ifelse(
  (yday(DATE) >= yday(as.Date(paste0(adp_year, "-03-01")))) & (yday(DATE) < yday(as.Date(paste0(adp_year, "-10-01")))),
  1, 2
)][, .(DAYS = uniqueN(DATE)), keyby = .(YEAR, PERIOD)
][, PROP := DAYS / sum(DAYS)
  # merge in lodging rates by period
][kodiak_lodging_dt, on = .(PERIOD)]

#' Again, assuming we have 150 fishing days in the year, each with 5 observers, and assuming you can put 2 observers in
#' each room

kodiak_lodging_cost <- year_date_period[, sum(goa_season_days * PROP * (COST_PER_NIGHT * (goa_plant_obs/2)))]

#' Total per-diem, number of season days X number of plant observers X per diem rate
kodiak_per_diem_cost <- goa_plant_per_diem * (goa_season_days * goa_plant_obs)


## [Summary (carve-off)] ---- 

#' [Assumptions]
#' - Kodiak plants will operate at a similar capacity as in 2023/2024; No GOA deliveries at Sand Point and False Pass 
#' - All observers are assigned to plants for all days where pollock fishing is open
#' - No air travel costs for plant observers
#' - No meal plan so plant observers are paid government per diem rates in Kodiak 
#' - 2 plant observers per room and nightly rates are equivalent to government lodging rates in Kodiak
#' - No equipment installation or replacement costs, only accounting for equipment maintenance
#' - Video review costs reflect costs in the GOA only, assuming BSAI costs are separate or riembursed

cost_summary.em_trw <- data.table(
  Category = c("Observer Plant Day Costs", "Lodging", "Per Diem", "Equipment Maintenance", "Video Review"),
  Cost = c(
    trw_em_day_costs, kodiak_lodging_cost, kodiak_per_diem_cost, 
    equipment_upkeep_per_VY * emtrw_goa_v_count, em_trw_total_data_cost
  )
)

cost_summary.em_trw


#==========================================#
# Upload outputs to Shared Google Drive ----
#==========================================#

#' TODO