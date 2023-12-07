
# Observer-recorded Mammal Events and Interactions in PC
# ADK 20230724
# CHF 20230727
# Final run for 2024 ADP data pull on 8/18/2023

# Goal -----------------------------------------------------------------------------------------------------------------
#The goal of this script is to query the Marine Mammal interaction tables and the FMA tables
#To estimate the bycatch of mammals on a per observed trip basis.

#The data uses the logistics tables that has partial and full coverage labels (2013-2023)

#These data will be aggregated by stratum for the 2024 ADP
#and reduced to relevant information necessary for use in ObsCovgTools package https://connect.fisheries.noaa.gov/content/bb44513d-4956-44dd-b0a6-673c9f2a3e3a/
#to generate the likelihood of detection for our anticipated effort and sample size in the 2024 ADP.

# Open NORPAC database connection -------
library(sqldf)

channel <- dbConnect(odbc::odbc(),"AFSC",
                     UID    = rstudioapi::askForPassword("Enter your NORPAC Username: "),
                     PWD    = rstudioapi::askForPassword("Enter your NORPAC Password: "))

# Query FMA trips ------------------------------------------------------------------------------------------------------
# Observed fishing trips PC pre-filter
# NOTE: includes mixed deployments, see below.
df_FMA_trips_fishing_occrd_PC <-
  dbGetQuery(
    channel,
    "SELECT DISTINCT t.cruise, t.permit, t.trip_seq, t.trip_number, trunc(t.start_date) AS start_date, trunc(t.end_date) AS end_date,
            h.gear_type_code, gt.description AS gear_type_desc,
            h.vessel_type, vt.description AS vessel_type_desc,
            nv.general_region AS fmp_area,     ---NOTE: some trips have multiple FMP_areas, these are returned as separate rows (long format).
            extract(year FROM h.retrv_date_time) AS ADP,  ---NOTE: some trips have hauls that span calendar years, these are returned as separate rows (long format).    
            extract(year FROM t.end_date) AS ADPYR  ---NOTE: I only want a trip to belong to one year so I changed the above to this (cf).    
            
       FROM norpac.atl_fma_trip t
       JOIN norpac.atl_haul h        --NOTE: inner joining to atl_haul de facto filters out all non-fishing trips.
         ON h.cruise = t.cruise AND h.permit = t.permit AND h.trip_seq = t.trip_seq
       JOIN norpac.atl_lov_gear_type gt
         ON gt.gear_type_code = h.gear_type_code
        AND h.geartype_form = gt.geartype_form
       JOIN norpac.atl_lov_vessel_type vt
         ON vt.vessel_type = h.vessel_type
       JOIN norpac.atl_nmfs_area_v nv
         ON h.nmfs_area = nv.nmfs_area
             
       --    The following logic will ensure the query only returns trips for cruise/permits that have a PARTIAL COVERAGE record in logistics.
       --    Note that while this filters the vast majority of trip data for PC, it is inexact as some cruise/permits since 2020 may have done both FC and PC trips.
       --    In those cases the query will return all trips for that cruise/permit, and some of those trips may be FC.
       --    Additional matching may be required based on dates to truly filter out the FC trips.
       --    That is why I am ALSO providing a query of ols_vessel_plant (as a separate DF), so that you can do that matching yourself 
       --    as I don't have time to sort that out using date matching myself right now!!
    
     WHERE EXISTS (SELECT * FROM norpac.ols_vessel_plant 
                      WHERE cruise = t.cruise AND permit = t.permit AND coverage_type_code = 'P'
                  )
     ")              

# Cleaning - Vessel TYPE ------------------------------------------------------------------------------------------------------

#The above df has records where vessel type == 6 and Vessel_type_desc is "catcher vessel - Fish Discarded"
#For this analysis, change these to Vessel type 3, Catcher Vessel.
library(dplyr)

hold <- df_FMA_trips_fishing_occrd_PC
table(hold$VESSEL_TYPE)
hold$VESSEL_TYPE_DESC[hold$VESSEL_TYPE == 6] <- paste(hold %>% filter(VESSEL_TYPE == 3) %>% 
                                                        select(VESSEL_TYPE_DESC) %>% unique())
hold$VESSEL_TYPE[hold$VESSEL_TYPE == 6] <- 3
table(hold$GEAR_TYPE_CODE, hold$VESSEL_TYPE_DESC)

#Looks good
df_FMA_trips_fishing_occrd_PC <- hold
rm(hold)

#There are trips that are identical in dates - 
df_FMA_trips_fishing_occrd_PC %>% filter(CRUISE == 23576, PERMIT == 10158) %>% arrange(TRIP_SEQ)

# Query Logistics records ----------------------------------------------------------------------------------------------
# You can use this to further filter out the mixed PC/FC cruise/permits, so you can TRULY filter for PC.
# Use EMBARK_DATE and DISEMBARK_DATE to do this.
# A suggestion might be, to only include TRIPS that have a START_DATE after the EMBARK_DATE and an END_DATE before the DISEMBARK_DATE.

df_vessel_plant_assnmts <-
  dbGetQuery(
    channel, 
    "SELECT * FROM norpac.ols_vessel_plant vp
      WHERE EXISTS (SELECT 1 FROM norpac.ols_vessel_plant 
                     WHERE cruise = vp.cruise AND permit = vp.permit AND coverage_type_code = 'P'
                    )
            ") 

#If there are multiple ols_vessel_plant records, then these ARE two trips...
df_vessel_plant_assnmts %>% filter(CRUISE == 23576, PERMIT == 10158) %>% arrange(EMBARK_DATE)
#Nope.

#Create a label so we can eventually remove identical date trips that have different trip seq.
testing_df<- df_FMA_trips_fishing_occrd_PC 
testing_df<- 
  testing_df %>% 
  mutate(ADPYR = format(END_DATE, format = "%Y"), 
         JOIN_LABEL = paste0(ADP, CRUISE, PERMIT, TRIP_SEQ), # This is for merging with mammal data later
         LABEL = paste0(ADP, CRUISE, PERMIT, START_DATE, END_DATE) # This is for finding unique trips.
         ) 
 
 
testing_df %>% filter(CRUISE == 23576, PERMIT == 10158) %>% arrange(LABEL)
#NOTE how this has two LABEL records but three JOIN_LABELS

# Use the df_vessel_plant_assnmts object to FURTHER FILTER the df_FMA_trips_PC_fishing_occrd object: remove any hanging FC trips.
# Use date ranges to do this.

testing_df <- merge(df_vessel_plant_assnmts,
                    testing_df) %>% filter(EMBARK_DATE <= END_DATE &
                                                                DISEMBARK_DATE >= END_DATE)
#Reduces the df from 9691 to 9444 (8/17/2023)
rm(df_vessel_plant_assnmts)

# Cleaning - GEAR TYPE ------------------------------------------------------------------------------------------------------
#Translate Gear codes to those used by Geoff

testing_df <- 
  testing_df %>% 
  mutate(CURRENT.GEAR = 
           ifelse(GEAR_TYPE_CODE %in% c(1,2), "TRW", 
                  ifelse(GEAR_TYPE_CODE == 6, "POT", 
                         "HAL")
                  ),
         FIXED.GEAR = ifelse(GEAR_TYPE_CODE %in% c(1,2), "TRW", "FIXED")
  ) %>% 
  select(-GEAR_TYPE_CODE, -GEAR_TYPE_DESC) %>% 
  distinct()
#df reduced to 9213

df_FMA_trips_fishing_occrd_PC <- testing_df
rm(testing_df)

# Query mammal interactions --------------------------------------------------------------------------------------------
# Mammal mortality interactions
# NOTE: query includes ALL interactions including full coverage.

df_lethal_mammal_interactions <-
  dbGetQuery(
    channel,
    "SELECT (SELECT extract(year FROM retrv_date_time)
               FROM atl_haul h WHERE h.cruise = m.cruise ANd h.permit = m.permit AND h.haul_seq = m.haul_seq) AS ADP,
            m.cruise, m.permit, m.mammal_seq, mi.interaction_seq, m.mammal_species_code, lms.common_name, m.offload_seq, 
            nvl(m.trip_seq, (SELECT trip_seq FROM atl_haul WHERE cruise = m.cruise AND permit = m.permit AND haul_seq = m.haul_seq)) AS trip_seq,
            m.haul_seq, mi.mammal_interact_code, lmi.description as interaction_description,
            m.number_of_animals as number_of_animals_in_event, mi.number_of_animals AS number_of_animals_in_interaction,
            mi.deterrence_success_flag, mi.condition_code, lc.description as animal_condition,
            mi.interaction_date, mi.observation_flag, mi.comments, mi.animal_injured
       FROM norpac.atl_mammal m
       JOIN norpac.atl_lov_mammal_species_code lms
         ON lms.mammal_species_code = m.mammal_species_code
       JOIN norpac.atl_mammal_interaction mi
         ON mi.cruise = m.cruise AND mi.permit = m.permit AND mi.mammal_seq = m.mammal_seq
       JOIN norpac.atl_lov_mammal_interaction lmi
         ON lmi.mammal_interact_code = mi.mammal_interact_code
       LEFT OUTER JOIN norpac.atl_lov_condition lc
         ON lc.condition_code = mi.condition_code AND lc.animal_type_code = mi.condition_animal_type
      WHERE mi.mammal_interact_code in (4,5,7,8)  --includes all lethal interaction types
        AND m.mammal_species_code in ('EJ', -- Stellar sea
                                      'MN', -- Humpback whale
                                      'OO'  -- Killer whale
                                     )        
     ") 

rm(channel)

#ADD Merge label
df_lethal_mammal_interactions <- 
  df_lethal_mammal_interactions %>% 
  mutate(JOIN_LABEL = paste0(ADP, CRUISE, PERMIT, TRIP_SEQ))

#Check for duplicates
dups <- df_lethal_mammal_interactions[duplicated(df_lethal_mammal_interactions$LABEL),] %>% select(JOIN_LABEL)
dups_df <- df_lethal_mammal_interactions[df_lethal_mammal_interactions$LABEL %in% dups$JOIN_LABEL,] %>% arrange(JOIN_LABEL)

#No duplicated mammal interaction records.
rm(dups, dups_df)

#Assign correct label to interaction data
df_lethal_mammal_interactions<- 
  merge(df_lethal_mammal_interactions, 
        df_FMA_trips_fishing_occrd_PC %>% 
          select(JOIN_LABEL, LABEL) %>%
          distinct(), all.x = TRUE)

# This will be what we merge into trips
Mammals <- 
  df_lethal_mammal_interactions %>%
  filter(!is.na(LABEL)) %>%
  select(LABEL, MAMMAL_SPECIES_CODE, COMMON_NAME, INTERACTION_DESCRIPTION, NUMBER_OF_ANIMALS_IN_INTERACTION, 
         ) %>% distinct()

# Find Duplicate Trips and assign Trip Weights ------------------------------------------------------------------------------
testing_df <- 
  df_FMA_trips_fishing_occrd_PC %>% 
  select(LABEL, FMP_AREA, CURRENT.GEAR) %>% distinct()
#9120

#Assign a trip weight for duplicate FMPs under FIXED.GEAR
dups <- testing_df[duplicated(testing_df$LABEL),] %>% select(LABEL)
dups_df <- testing_df[testing_df$LABEL %in% dups$LABEL,] %>% arrange(LABEL)

hold <- dups_df %>% select(LABEL, CURRENT.GEAR, FMP_AREA) %>% 
  group_by(LABEL) %>% summarise(NUM_GEARS = n_distinct(CURRENT.GEAR),
                                NUM_FMPS = n_distinct(FMP_AREA))
play <- merge(dups_df, hold)

play <- 
  merge(dups_df, hold, all.x = TRUE) %>% 
  mutate(GEAR_WEIGHT = 1/NUM_GEARS, 
         FMP_WEIGHT = 1/NUM_FMPS,
         GEAR_FMP_WEIGHT = min(GEAR_WEIGHT, FMP_WEIGHT)) %>% 
  arrange(LABEL)
table(play$GEAR_WEIGHT, play$FMP_WEIGHT, play$GEAR_FMP_WEIGHT)

#Hmm one entry is strange..
play %>% filter(FMP_WEIGHT < 1 & GEAR_WEIGHT <1)
#No, that makes sense.  

testing_df <- 
  merge(play %>% 
          select(LABEL, GEAR_WEIGHT, FMP_WEIGHT, GEAR_FMP_WEIGHT) %>% 
          distinct()
        ,
        df_FMA_trips_fishing_occrd_PC, all.y = TRUE) %>%
  mutate(GEAR_WEIGHT = ifelse(is.na(GEAR_WEIGHT), 1, GEAR_WEIGHT),
         FMP_WEIGHT = ifelse(is.na(FMP_WEIGHT), 1, FMP_WEIGHT), 
         GEAR_FMP_WEIGHT = ifelse(is.na(GEAR_FMP_WEIGHT), 1, GEAR_FMP_WEIGHT))
#9214

df_FMA_trips_fishing_occrd_PC <- testing_df
rm(testing_df, hold, dups, dups_df, play)

Trips <- 
  df_FMA_trips_fishing_occrd_PC %>% 
  select(LABEL, CURRENT.GEAR, FIXED.GEAR, FMP_AREA, GEAR_WEIGHT, 
         GEAR_FMP_WEIGHT,  FMP_WEIGHT, ADP) %>%
  distinct()
#9120

# MERGE-------------------------------------------------------------------------------------------------------------
ALL <- merge(Trips, Mammals, all.x = TRUE)

# Summaries ------------------------------------------------------------------------------------------------------------
Ntest <- 
  ALL %>% 
  summarize(Ngear = sum(ALL$GEAR_WEIGHT),
         NFMP = sum(FMP_WEIGHT),
         Ngear_FMP = sum(GEAR_FMP_WEIGHT),
         N = n_distinct(LABEL)
  )
Ntest
# Ngear NFMP Ngear_FMP    N
#1  9033 9048      8962 8962

#Format for later use with birds and groundfish
mammals <- ALL %>%
  mutate(SPECIES_GROUP_CODE = "EJ",
         OBSERVED_FLAG = "Y",
         BSAI_GOA = FMP_AREA,
         METRIC = "COUNT") %>% select(-FMP_AREA)

# Cleanup --------------------------------------------------------------------------------------------------------------

#rm(list= ls()[!(ls() %in% c('mammals'))])

# Save -----------------------------------------------------------------------------------------------------------------

save.image(file = "~/2024 Integrated Analysis/2024_ADP_repo/analyses/power_analyses/Mammal_datapull.Rdata")

# above file saved remotely at https://drive.google.com/drive/folders/1nTLGGb8jx6m-ZlJNMSt6HrUzfnH09jMh?usp=sharing

#load(file = "~/2024 Integrated Analysis/2024_ADP_repo/analyses/Design Performance Evaluations/Mammal_datapull.Rdata")


