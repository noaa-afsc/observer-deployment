#Craig Faunce
# GOAL - This file aims to format bird and groundfish data for merging with rates object
# Start - 8/21/2023
# End - 8/25/23

library(dplyr)
options(dplyr.summarise.inform = FALSE)


# Load data ------------------------------------------------------------------------------------------------------------

#Load mammals
load(file = "~/2024 Integrated Analysis/2024_ADP_repo/analyses/power_analyses/Mammal_datapull.Rdata")
rm(list= ls()[!(ls() %in% c('mammals'))])

#Load birds
load(file = "~/2024 Integrated Analysis/2024_ADP_repo/analyses/power_analyses/Bird_datapull.Rdata")

#Birds has full coverage trips in it.  Remove these. 
birds <- bird_trips %>% filter(CVG_NEW != "FULL", !STRATA %in% c("FULL", "ZERO")) #TODO - still has zero - ok?

#There are records improperly labelled - fix these fixed gear strata
table(birds$STRATA.FIXED, birds$STRATA)
birds$STRATA.FIXED[birds$STRATA.FIXED == "EM_POT" & birds$STRATA == "EM_POT"] <- "EM_FIXED"
birds$STRATA.FIXED[birds$STRATA.FIXED == "OB_POT" & birds$STRATA == "OB_POT"] <- "OB_FIXED"

table(birds$STRATA.FIXED, birds$CURRENT.GEAR)
birds$STRATA.FIXED[birds$STRATA.FIXED == "EM_FIXED" & birds$CURRENT.GEAR == "TRW"] <- "EM_TRW"
birds$STRATA.FIXED[birds$STRATA.FIXED == "OB_FIXED" & birds$CURRENT.GEAR == "TRW"] <- "OB_TRW"
birds$STRATA.FIXED[birds$STRATA.FIXED == "OB_TRW" & birds$CURRENT.GEAR == "POT"] <- "OB_FIXED"

# Identify format for stratum ------------------------------------------------------------------------------------------

# Three fishing effort objects 
# (one for each of our stratifications of CURRENT, FMP, and FIXED_FMP) 
# that are virtually identical except for the STRATUM_COL, 
# which defines each trip's stratum under the proposed designs.draft_rates.rdata
load(file = "~/2024 Integrated Analysis/2024_ADP_repo/source_data/draft_rates_effort.Rdata")

table(pc_effort.CURRENT$STRATUM_COL)
#EM_HAL EM_POT EM_TRW OB_HAL OB_POT OB_TRW   ZERO 
# 6490   1725   5526  13329   5988   4203   9698
table(pc_effort.FMP$STRATUM_COL)
#EM_HAL-BSAI  EM_HAL-GOA EM_POT-BSAI  EM_POT-GOA  EM_TRW-GOA OB_HAL-BSAI  OB_HAL-GOA OB_POT-BSAI  OB_POT-GOA 
#   457        6033         399        1326        5526        2059       11270        2453        3535 
#OB_TRW-BSAI  OB_TRW-GOA   ZERO-BSAI    ZERO-GOA 
#   202        4001        2080        7618
table(pc_effort.FIXED_FMP$STRATUM_COL)
#EM_FIXED-BSAI  EM_FIXED-GOA    EM_TRW-GOA OB_FIXED-BSAI  OB_FIXED-GOA   OB_TRW-BSAI    OB_TRW-GOA 
#    856          7359          5526          4512         14804           202          4001 
#ZERO-BSAI      ZERO-GOA 
#    2080          7618 

#Treating data in each ADP as a stratum, so we can just sum across them (mean is weighted mean)   

# Mammals hardcode -----------------------------------------------------------------------------------------------------
#CURRENT
mammals.CURRENT <-
      mammals %>%
      group_by(SPECIES_GROUP_CODE, METRIC, GEAR = CURRENT.GEAR, FIXED.GEAR) %>%
      mutate(Value = ifelse(is.na(NUMBER_OF_ANIMALS_IN_INTERACTION), 
                            0,
                            NUMBER_OF_ANIMALS_IN_INTERACTION * GEAR_FMP_WEIGHT)
      ) %>%          
      summarize(Xbar = mean(Value),
                s2 = var(Value),
                Nh = sum(GEAR_FMP_WEIGHT),
                nh = sum(GEAR_FMP_WEIGHT[OBSERVED_FLAG == "Y"])) %>% ungroup() %>%
        mutate(STRATIFICATION = "CURRENT",
               STRATUM_COL = paste("OB", GEAR, sep = '_'))

#FMP
mammals.FMP <-
  mammals %>%
  group_by(SPECIES_GROUP_CODE, METRIC, GEAR = CURRENT.GEAR, FIXED.GEAR, BSAI_GOA) %>%
  mutate(Value = ifelse(is.na(NUMBER_OF_ANIMALS_IN_INTERACTION), 
                        0,
                        NUMBER_OF_ANIMALS_IN_INTERACTION * GEAR_FMP_WEIGHT)
  ) %>%          
  summarize(Xbar = mean(Value),
            s2 = var(Value),
            Nh = sum(GEAR_FMP_WEIGHT),
            nh = sum(GEAR_FMP_WEIGHT[OBSERVED_FLAG == "Y"])) %>% ungroup() %>%
  mutate(STRATIFICATION = "FMP",
         STRATUM_COL = paste0(paste0("OB_", GEAR),
                              paste0("-", BSAI_GOA))
  )

#FIXED_FMP
mammals.FIXED_FMPx <- 
  mammals.FMP %>% ungroup() %>%
  mutate(STRATIFICATION = "FIXED_FMP",
         STRATUM_COL = paste0(paste0("OB_", FIXED.GEAR),
                              paste0("-", BSAI_GOA)
                              )
         )

Ns <- 
  mammals.FIXED_FMPx %>%
  group_by(SPECIES_GROUP_CODE, METRIC, STRATUM_COL) %>%
  summarize(Nfixed = sum(Nh),
            nfixed = sum(nh))

mammals.FIXED_FMPx <- 
  merge(mammals.FIXED_FMPx, Ns)

#Now boil up to fixed gear
mammals.FIXED_FMP <- 
  mammals.FIXED_FMPx %>% ungroup() %>%
  group_by(STRATIFICATION, SPECIES_GROUP_CODE, METRIC, STRATUM_COL) %>%
  summarize(Xbar = weighted.mean(Xbar, w = Nh / max(Nfixed)),
            s2 = weighted.mean(s2, w = (Nh / max(Nfixed))^2),
            Nh = max(Nfixed),
            nh = max(nfixed)
  )

#All in one object
# mammals.ALL <-
#   rbind(mammals.CURRENT %>% select(-GEAR, -FIXED.GEAR),
#         mammals.FMP %>% select(-GEAR, -FIXED.GEAR, -BSAI_GOA),
#         mammals.FIXED_FMP %>%
#           select(SPECIES_GROUP_CODE, METRIC, Xbar,
#                  s2, Nh, nh, STRATIFICATION, STRATUM_COL)
#   )

rm(Ns)

# Birds hardcode -------------------------------------------------------------------------------------------------------

#CURRENT
birds.CURRENT <-
      birds %>% ungroup() %>%
      group_by(SPECIES_GROUP_CODE, METRIC, STRATA) %>%
      summarize(Xbar = mean(Value),
                s2 = var(Value),
                Nh = n_distinct(TRIP_ID),
                nh = n_distinct(TRIP_ID[OBSERVED_FLAG == "Y"])) %>% ungroup() %>%
        mutate(STRATIFICATION = "CURRENT",
               STRATUM_COL = STRATA)

#FMP               
birds.FMP <-
  birds %>% ungroup() %>%
  group_by(SPECIES_GROUP_CODE, METRIC, STRATA, BSAI_GOA) %>%
  summarize(Xbar = mean(Value),
            s2 = var(Value),
            Nh = n_distinct(TRIP_ID),
            nh = n_distinct(TRIP_ID[OBSERVED_FLAG == "Y"])) %>% ungroup() %>%
  mutate(STRATIFICATION = "FMP",
         STRATUM_COL = paste0(STRATA, paste0("-", BSAI_GOA)
                              ) 
         )
               
               # STRATUM_COL = ifelse(STRATIFICATION == "CURRENT",
               #                      STRATUM, #Label for CURRENT
               #                      ifelse(STRATIFICATION == "FMP", 
               #                             paste0(STRATUM, #Label for FMP
               #                                    paste0("-", BSAI_GOA)),
               #                             paste(STRATUM, BSAI_GOA, sep = "-")
               #                             )
               #                      )

#FIXED_FMP
birds.FIXED_FMPx <-
  birds %>% ungroup() %>%
  group_by(SPECIES_GROUP_CODE, METRIC, GEAR = CURRENT.GEAR, FIXED.GEAR, STRATA.FIXED, BSAI_GOA) %>%
  summarize(Xbar = mean(Value),
            s2 = var(Value),
            Nh = n_distinct(TRIP_ID),
            nh = n_distinct(TRIP_ID[OBSERVED_FLAG == "Y"])) %>% ungroup() %>%
  mutate(STRATIFICATION = "FIXED_FMP",
         STRATUM_COL = paste(STRATA.FIXED, BSAI_GOA, sep = "-")
         )

Ns <- 
   birds.FIXED_FMPx %>% ungroup() %>%
   group_by(SPECIES_GROUP_CODE, METRIC, STRATUM_COL) %>%
   summarize(Nfixed = sum(Nh),
             nfixed = sum(nh))

birds.FIXED_FMPx <- merge(birds.FIXED_FMPx, Ns)

#Now boil up to fixed gear
 birds.FIXED_FMP <- 
   birds.FIXED_FMPx %>% ungroup() %>%
   group_by(STRATIFICATION, SPECIES_GROUP_CODE, METRIC, STRATUM_COL) %>%
   summarize(Xbar = weighted.mean(Xbar, w = Nh / max(Nfixed)),
             s2 = weighted.mean(s2, w = (Nh / max(Nfixed))^2), #This is reduced form
            #s2 = (Nh / max(Nfixed))^2) * ((Nh - nh) / Nh) * (s2 / nh), # This is full form - probably not right since s2 comes from the population
             Nh = max(Nfixed),
             nh = max(nfixed)
   )


# Cleanup --------------------------------------------------------------------------------------------------------------
rm(bird_trips, birds, mammals, Ns,
   birds.FIXED_FMPx, mammals.FIXED_FMPx,
   pc_effort.CURRENT, pc_effort.FIXED_FMP, pc_effort.FMP)

# Save -----------------------------------------------------------------------------------------------------------------
save.image(file = "~/2024 Integrated Analysis/2024_ADP_repo/analyses/power_analyses/Bird_Mammal_dataready.Rdata")

# above file saved remotely at https://drive.google.com/drive/folders/1nTLGGb8jx6m-ZlJNMSt6HrUzfnH09jMh?usp=sharing
 
  