#Craig Faunce
# GOAL - This file aims to format bird data in our valhalla souce code for use in the 2024 Analysis.  It will stop at Report ID, not trip id.
# Start - 8/19/2023
# End - 8/24/2023

library(dplyr)
options(dplyr.summarise.inform = FALSE)

#Load the source data for the ADPs - takes a while
load("~/2024 Integrated Analysis/2024_ADP_repo/source_data/2024_Draft_ADP_data.rdata")

#Cleanup environment
rm(list= ls()[!(ls() %in% c('work.data'))])

# Fix known problems -------------------------------------------------------------------------------------------------
#First add each species to each report id
report_flags <- 
  work.data %>% 
  select(ADP, REPORT_ID, TRIP_ID, CURRENT.GEAR = AGENCY_GEAR_CODE, OBSERVED_FLAG, 
         STRATA = STRATA_NEW, CVG_NEW, FMP) %>%
  mutate(BSAI_GOA = ifelse(FMP %in% c('AI', 'BS', 'BSAI'), "BSAI", "GOA"),
         STRATA = ifelse(STRATA == "EM_TRW_EFP", "EM_TRW", STRATA), #Correct
         STRATA = ifelse(STRATA %in% c("POT", "HAL", "TRW"), #This formats like Geoff
                             paste0("OB_", STRATA), STRATA),
         CURRENT.GEAR = ifelse(CURRENT.GEAR %in% c("PTR", "NPT"), "TRW", CURRENT.GEAR),
         FIXED.GEAR = ifelse(CURRENT.GEAR %in% c("HAL", "POT"), #This preps for later.
                             "FIXED", CURRENT.GEAR),
         STRATA.FIXED = ifelse((FIXED.GEAR == "FIXED" & STRATA %in% c("EM_HAL", "EM_POT")), "EM_FIXED", NA),
         STRATA.FIXED = ifelse((FIXED.GEAR == "FIXED" & STRATA %in% c("OB_HAL", "OB_POT")), "OB_FIXED", STRATA.FIXED),
         STRATA.FIXED = ifelse(is.na(STRATA.FIXED), STRATA, STRATA.FIXED)) %>% 
  ungroup() %>% distinct() %>%
  filter(CURRENT.GEAR != "JIG") %>% #A lot of trips report jig gear on reports in state waters.
  select(-FMP) %>% distinct()  #This removes any reports duplicated by AI and BS

#Check
table(report_flags$STRATA.FIXED, report_flags$STRATA)
#Looks good

#COMBO OBSERVATION STATUS.  I want to fix this now
observed <- 
  merge(report_flags %>% filter(OBSERVED_FLAG == "Y") %>% select(REPORT_ID) %>% unique(),
        report_flags %>% filter(!is.na(REPORT_ID)) %>% #there are report ids missing!
          select(ADP, REPORT_ID, TRIP_ID) %>% distinct(), 
        all.x = TRUE)

report_flags_corrected <- 
  report_flags %>% select(-OBSERVED_FLAG) %>% distinct() %>%
  mutate(OBSERVED_FLAG = ifelse(TRIP_ID %in% observed$TRIP_ID, "Y", "N"))

rm(report_flags, observed)

# Start bird pull ------------------------------------------------------------------------------------------------
#see file 'Bird_exploration.r' for details

library(readxl)
nontarget <- rbind(read_excel("~/2024 Integrated Analysis/2024_ADP_repo/source_data/nontarget_ecosystem_catch.xlsx", sheet = 1),
                   read_excel("~/2024 Integrated Analysis/2024_ADP_repo/source_data/nontarget_ecosystem_catch.xlsx", sheet = 2,
                              col_names = c("YEAR", "REPORT_ID", "SPECIES_GROUP_CODE",
                                            "SPECIES_COUNT", "SPECIES_WEIGHT")))
birds <- nontarget %>% filter(SPECIES_GROUP_CODE %in% c('STBR', 'LBRD')) %>%
  group_by(REPORT_ID, SPECIES_GROUP_CODE) %>% summarize(SPECIES_COUNT = sum(SPECIES_COUNT))

rm(nontarget)

#Now we add our blank birds
bird_reports <- 
  merge(report_flags_corrected,
        birds %>% ungroup() %>% select(SPECIES_GROUP_CODE) %>% distinct()
        )

#bird_reports should now be twice as long as the original
nrow(bird_reports)/nrow(report_flags_corrected)
#[1] 2

#Merge bird data into our 'clean' dataframe
bird_reports <- 
  merge(bird_reports, birds, all.x = TRUE) %>%
  mutate(SPECIES_COUNT = ifelse(is.na(SPECIES_COUNT), 0, SPECIES_COUNT)) #This converts our missing species to zeros

#Second test
birds %>% filter(REPORT_ID %in% bird_reports$REPORT_ID) %>% group_by(SPECIES_GROUP_CODE) %>% summarize(TOTAL = sum(SPECIES_COUNT))
# # A tibble: 2 × 2
# SPECIES_GROUP_CODE   TOTAL
# <chr>                <dbl>
#   1 LBRD               677.   
# 2 STBR                 0.156

bird_reports %>% group_by(SPECIES_GROUP_CODE) %>% summarize(TOTAL = sum(SPECIES_COUNT))
# # A tibble: 2 × 2
# SPECIES_GROUP_CODE   TOTAL
# <chr>                <dbl>
#   1 LBRD               677.   
# 2 STBR                 0.156

#Looks good

 #Reduce data to per trip
 bird_trips <- 
   bird_reports %>% ungroup() %>%
   group_by(ADP, CVG_NEW, SPECIES_GROUP_CODE, CURRENT.GEAR, STRATA, FIXED.GEAR, 
            STRATA.FIXED, BSAI_GOA, TRIP_ID, OBSERVED_FLAG) %>%
   summarize(METRIC = "COUNT",
             Value = sum(SPECIES_COUNT))

# Cleanup --------------------------------------------------------------------------------------------------------------

rm(list= ls()[!(ls() %in% c('bird_trips'))])

# Save -----------------------------------------------------------------------------------------------------------------
save.image(file = "~/2024 Integrated Analysis/2024_ADP_repo/analyses/power_analyses/Bird_datapull.Rdata")

# above file saved remotely at https://drive.google.com/drive/folders/1nTLGGb8jx6m-ZlJNMSt6HrUzfnH09jMh?usp=sharing


# (DEPRECATED) Assign weightings for combination trips ------------------------------------------------------------------------------
# 
# dups.fxn <- function(data, column){
#   combo <- data %>% select(TRIP_ID, {{column}}) %>% distinct() %>% select(-{{column}}) %>%
#     group_by_all() %>%
#     filter(n()>1) %>% distinct() %>%
#     ungroup()
#   
#   dups <- merge(data, combo)
#   #  return(dups)
#   if(length(dups >=0)){return(dups)}else{print("No duplicate records")} 
# }
# 
# #Combo observed flag
# combo_obs <- dups.fxn(report_flags_corrected, OBSERVED_FLAG) 
# 
# #Combo stratum
# combo_stratum <- dups.fxn(report_flags_corrected, STRATA)
# combo_fixed_stratum <- dups.fxn(report_flags_corrected, STRATA.FIXED)
# 
# #COMBO FMP TRIPS
# combo_fmp <- dups.fxn(report_flags_corrected, BSAI_GOA)
# 
# #COMBO GEAR TRIPS
# combo_gear <- dups.fxn(report_flags_corrected, CURRENT.GEAR)
# combo_fixed <- dups.fxn(report_flags_corrected, FIXED.GEAR)









