#' *Monitoring Costs*
#  `This script sets the monitoring cost parameters for the ADP.

#' [Author: Geoff Mayhew]
#' [StartDate: 2024-Aug-13]

#' *PREREQUISITE* [First run selection_rates.R to create pc_effort_st object and upload it to the Gdrive]

#======================================================================================================================#
# Preparation ----
#======================================================================================================================#

adp_year <- 2025  #' This should come from get_data.R

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

#' Download from the shared Gdrive
gdrive_download(
  local_path = paste0("source_data/pc_effort_st", "_", adp_year, ".Rdata"),
  gdrive_dribble = gdrive_set_dribble("Projects/ADP/Output/")
)
#' Load the `pc_effort_data` object prepared by `selection_rates.R`
(load(paste0("source_data/pc_effort_st", "_", adp_year, ".Rdata")))

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
# Save the SQL pull
save(kodiak_plant_days, file = "analyses/monitoring_costs/sql_pulls/kodiak_plant_days.rdata")
gdrive_upload(
  local_path = "analyses/monitoring_costs/sql_pulls/kodiak_plant_days.rdata",
  gdrive_dribble = gdrive_set_dribble("Projects/ADP/Monitoring Costs - CONFIDENTIAL/")
)

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

adp_contract_day_rates[, .(PERIOD = 1:2, Base_Day_Cost, Optional_Day_Cost)]

#' Identify the date where the contracts change, specifically, the day that the second contract starts.
contract_change_date <- as.Date("2025-10-01")

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

contract_change_date     #' [Date that the second contract begins]
current_contract_days    #' [Estimated number of days on the contract when ADP year starts]
travel_cpd               #' [Estimated cost of travel per sea day]

#======================================================================================================================#
# FIXED-GEAR EM COSTS ----
#======================================================================================================================#

#' These cost estimates are pulled from what was prepared for the 2024 ADP. At the moment, no new information was 
#' available, so these estimates were inflation-adjusted for the 2025 ADP. See the original 2024 document:
#' [https://docs.google.com/spreadsheets/d/1kcdLjq2Ck4XJBYP0EhrQpuknRgtQFt01LN3xUaCg7cI/edit?pli=1&gid=0#gid=0]

#' Fixed-gear EM costs generally fall into three categories:

#' - Costs that scale with the size of the fixed-gear EM pool, such as equipment maintenance and project management. 
#' These are generally recurring costs. 
#' - Equipment installation costs, commonly referred to as 'amortized' costs, that are large one-time payments. 
#' - Data review and storage costs, that scale with the number of monitored sea days
 
#' For the purpose of estimating costs, we will assume that equipment installation costs will be funded via the 
#' Murkowski dollars, and the fee will only be used to pay for the equipment maintenance, project management, and data
#' review/storage costs. By categorizing the cost summaries from the 2015-2021 Annual Reports, we can divide by the 
#' number of vessels in the fixed-gear EM pool each year to get an estimate of the per-vessel recurring costs.
#' See the `2024 Monitoring Costs` document above, then navigate to the `Fixed-gear EM tab`. The table in columns L-O
#' total up the non-amortized costs and then subtract the estimated review costs.


#========================#
## Video Review Costs ----
#========================#

#' Reporting in the Annual Reports is inconsistent and oftentimes incomplete at the time of reporting. Using information
#' from 2018 to 2021, I can get a rough estimate of the review cost per day.

#' Compile cost of review and the number of reviewed days. 
emfg_review <- data.table(
  YEAR = 2018:2021,
  REVIEW_COST = c(191961, 96501, 189225, 242747),
  REVIEWED_DAYS = c(1005, 1817, 1442, 1458)
)
#' Look at how bad this data is. You would think cost would be positively correlated with the number of days reviewed.
#' I'm going to inflation-adjust each year so that at least the costs are comparable, but rather than taking a yearly
#' average, I'm going to total the inflation-adjusted costs and divide by the total number or reviewed days.
ggplot(emfg_review, aes(x = REVIEWED_DAYS, y = REVIEW_COST)) + geom_point()

emfg_review[, Infl_Factor := inflation_dt[emfg_review, Infl_Factor, on = c(Calendar = "YEAR")]]
emfg_review[, Infl_REVIEW_COST := REVIEW_COST * Infl_Factor]
emfg_review_cpd <- emfg_review[, sum(Infl_REVIEW_COST) / sum(REVIEWED_DAYS)]

#=========================#
## Non-Amortized Costs ----
#=========================#

#' There are sunken costs in fixed-gear EM for program management and equipment that recurr each year and are assumed to
#' scale with the number of vessels in the fixed-gear EM pool.  These cost were compiled between 2015 and 2021, 
#' removing any data-related costs or 'amortized costs' from equipment installation inflation-adjusted, and divided by 
#' the number of fixed-gear EM vessels each year. 

#' TODO Have not gotten confirmation from [Jenn Ferdidnand] that the fee was used to equip new EM vessels in recent 
#' years. Will we assume any new fixed-gear EM vessels in 2025 will be paid via Murkowski funds?

# ' Note that year 2017 below is actually the sum of 2015, 2016, and 2017. The program was much smaller those years.
emfg_nonamortized <- data.table(
  YEAR = c(2017:2021),
  POOL_SIZE = c(13 + 42 + 96, 141, 168, 169, 170),
  NON_AMORTIZED = c(784031, 663886, 642946, 926844, 915425),
  AMORTIZED = c(967395, 871244, 377613, 153313, 203422),
  REVIEWED_DAYS = c(259 + 357 + 706, 1005, 1817, 1442, 1458)
)
emfg_nonamortized[, TOTAL := NON_AMORTIZED + AMORTIZED]
# Apply an inflation-adjusted non-amortized cost
emfg_nonamortized[, Infl_Factor := inflation_dt[emfg_nonamortized, Infl_Factor, on = c(Calendar = "YEAR")]]
emfg_nonamortized[, Infl_NON_AMORTIZED := NON_AMORTIZED * Infl_Factor]
# Estimate the cost of review, inflation-adjusted
emfg_nonamortized[, Infl_REVIEW_COST := emfg_review_cpd * REVIEWED_DAYS]
# Subtract the cost of review from the non-amortized costs
emfg_nonamortized[, Infl_TOTAL_VESSEL_COST := Infl_NON_AMORTIZED - Infl_REVIEW_COST]
# Calculate the cost per vessel each year
emfg_nonamortized[, Infl_CPV := Infl_TOTAL_VESSEL_COST / POOL_SIZE]
emfg_nonamortized
#' There is definitely a lot of variation across years, so we will again get a total per-vessel costs across all years 
#' and divide by the total pool size to arrive at the average no-amortized cost-per-vessel
emfg_nonamortized_cpv <- emfg_nonamortized[, sum(Infl_TOTAL_VESSEL_COST) / sum(POOL_SIZE)]

## [Summary] ----

#' emfg_v                 # The number of fixed-gear EM vessels will be specified by the outputs of `get_data.R`
emfg_nonamortized_cpv     # The recurring cost per vessel in the fixed-gear EM pool
emfg_review_cpd           # The cost per review day, which will be multiplied by the trip duration * monitoring rate 
#' amortized_cpv          # The equipment installation/replacement costs. Assumed to be covered by Murkowski funds.

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

#' 144 days during 2023 A and B season, 85 days for 2024 A season only since this analysis is occurring mid-Aug.
goa_season_days <- 144

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

# Identify how many fishing days are in each lodging period
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

#' Again, assuming we have 5 observers, and assuming you can put 2 observers in each room
kodiak_lodging_cost <- year_date_period[, sum(goa_season_days * PROP * (COST_PER_NIGHT * (goa_plant_obs/2)))]


### Per Diem ----

#' Assuming the Kodiak plant observers do not have a food plan organized between the plant and the PC contract holder,
#' we are obligated to pay per-diem rates of $109 per day.
goa_plant_per_diem <- 109

#' Total per-diem, number of season days X number of plant observers X per diem rate
kodiak_per_diem_cost <- goa_plant_per_diem * (goa_season_days * goa_plant_obs)


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

#' This cost assumes the number of review days in `pc_effort_object`, not in any effort predictions
#' TODO can add this to `emtrw_cost()` function but we wouldn't be estimating the full EM TWR total carve-off here.
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
  data.table(
    readxl::read_xlsx("source_data/EM_TRW 2025 Vessel List for NMFS_July192024.xlsx", col_names = F)
  )[-(1:4), 7:8],
  new = c("VESSEL_NAME", "STATUS")
)[!is.na(VESSEL_NAME)]
#' [2025 DRAFT] Chelsae Radell listed 3 vessels as 'maybe' for joining trawl EM in 2025. Will not include these in the 
#' draft and will wait for vessel to formally declare before the Final ADP.
emtrw_goa_v_count <- emtrw_goa_v[STATUS != "Maybe?", .N]

## [Summary (carve-off)] ---- 

#' [Assumptions]
#' - Kodiak plants will operate at a similar capacity as in 2023/2024; No GOA deliveries at Sand Point and False Pass 
#' - All observers are assigned to plants for all days where pollock fishing is open
#' - No air travel costs for plant observers
#' - No meal plan so plant observers are paid government per diem rates in Kodiak 
#' - 2 plant observers per room and nightly rates are equivalent to government lodging rates in Kodiak
#' - No equipment installation or replacement costs, only accounting for equipment maintenance
#' - Video review costs reflect costs in the GOA only, assuming BSAI costs are separate or reimbursed

cost_summary.em_trw <- data.table(
  Category = c("Observer Plant Day Costs", "Lodging", "Per Diem", "Equipment Maintenance", "Data"),
  Cost = c(
    trw_em_day_costs, kodiak_lodging_cost, kodiak_per_diem_cost, 
    equipment_upkeep_per_VY * emtrw_goa_v_count, em_trw_total_data_cost
  )
)
sum(cost_summary.em_trw$Cost)   # Total
cost_summary.em_trw             # Broken up by cost category

#======================================================================================================================#
# ***TODO*** Upload outputs to Shared Google Drive ----
#======================================================================================================================#

#' Compile costs into `cost_params` object

cost_params <- list(
  OB = list(
    contract_rates = adp_contract_day_rates[, .(PERIOD = 1:2, Base_Day_Cost, Optional_Day_Cost)],
    contract_day_min = 1200,
    contract_change_date = contract_change_date,
    current_contract_days = current_contract_days,
    travel_cpd = travel_cpd
  ),
  EMFG = list(
    #  Initialize emfg_v here, then number of fixed-gear EM vessels, but specify this from the outputs of get_data.R
    emfg_v = NULL,                                 
    emfg_nonamortized_cpv = emfg_nonamortized_cpv,
    emfg_review_cpd = emfg_review_cpd
  ),
  EMTRW = list(
    emtrw_total_cost = sum(cost_summary.em_trw$Cost),
    emtrw_summary = cost_summary.em_trw,
    goa_plant_ob_days = goa_plant_ob_days,
    emtrw_goa_v_count = emtrw_goa_v_count
  )
)

#' Save cost_params locally 
save(cost_params, file = paste0("source_data/cost_params_", adp_year, ".Rdata"))

#' Upload cost_params to shared google drive
gdrive_upload(
  local_path = paste0("source_data/cost_params_", adp_year, ".Rdata"),
  gdrive_dribble = gdrive_set_dribble("Projects/ADP/Monitoring Costs - CONFIDENTIAL/")
)
