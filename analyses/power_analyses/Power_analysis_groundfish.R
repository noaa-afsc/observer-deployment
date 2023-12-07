# Craig Faunce
# GOAL - To calculate power to detect for observer effects for the 2024 ADP designs
# Start 8/24/2023
# End 8/25/2023

#We will need the 2024 valhalla data source - takes a while
load("~/2024 Integrated Analysis/2024_ADP_repo/source_data/2024_Draft_ADP_data.rdata")
rm(list= ls()[!(ls() %in% c('work.data'))])


#Designs are here:
#https://docs.google.com/document/d/17b7S_aC8whaIKTO8JXy8hpixlFTYo6cV/edit

#Stratifications are: current, fmp, fixed_fmp,
#Allocations are: equal, status_quo, cwb, box. Not including prox!
# So a design is stratification.allocation (e.g. current.box)

# Source data ----------------------------------------------------------------------------------------------------------
# Draft 2024 rates are in the google drive, Vision 2024 ADP/data folder:
#   https://drive.google.com/file/d/1feAzcFhFvMfADQP2Lp2vu4mhh0hQiceQ/view?usp=drive_link
# 
# This object has 'rates' 
# (a list of our 12 designs, 
# each item is a data.frame with 3 budget levels, 
# years 2018-2022, and their resulting rates, N, and n for each of the strata 
# (defined by STRATUM_COL), 
#load(file = "~/2024 Integrated Analysis/2024_ADP_repo/source_data/draft_rates.Rdata")

# 
# and three fishing effort objects 
# (one for each of our stratifications of CURRENT, FMP, and FIXED_FMP) 
# that are virtually identical except for the STRATUM_COL, 
# which defines each trip's stratum under the proposed designs.draft_rates.rdata
load(file = "~/2024 Integrated Analysis/2024_ADP_repo/source_data/draft_rates_effort.Rdata")

library(dplyr)
options(dplyr.summarise.inform = FALSE)

# Loop set up ------------------------------------------------------------------------------------------------------effort_to_use <- 
library(stringr) #https://stringr.tidyverse.org

efforts <- ls()[str_detect(ls(), "pc_effort")]

#Im going to need this translation for the FIXED_FMP design
fmp_ids <- pc_effort.FMP %>% select(FMP_STRATUM_COL = "STRATUM_COL", "wd_TRIP_ID") %>% distinct()

#Test
fixed_ids <- pc_effort.FIXED_FMP %>% select(FIXED_FMP_STRATUM_COL = "STRATUM_COL", "wd_TRIP_ID") %>% distinct()

fmp_to_fixed_ids <- merge(fmp_ids, fixed_ids, all = TRUE) #Same nrow as source ids.
table(is.na(fmp_to_fixed_ids)) #No NAs - all good

#Now generate the correct gear for those stratum
#GEOFF GEAR
fmp_to_fixed_ids <- 
  fmp_to_fixed_ids %>% mutate(GEOFF_GEAR = sub("*.._", "", FMP_STRATUM_COL))

fmp_to_fixed_ids <- 
  fmp_to_fixed_ids %>% mutate(GEOFF_GEAR = sub("-.*", "", GEOFF_GEAR))

table(fmp_to_fixed_ids$GEOFF_GEAR)

rm(fixed_ids, fmp_ids)

#GEOFF FMP
# Remove all before and up to ":":
#gsub(".*:","",foo)
fmp_to_fixed_ids <- 
  fmp_to_fixed_ids %>% mutate(GEOFF_FMP = gsub(".*-", "", FMP_STRATUM_COL))

table(fmp_to_fixed_ids$GEOFF_FMP)

#Remove elements we are not including
#my_list[names(my_list) != "b"] 

#rates_2024 <- rates[!names(rates) %in% c("CURRENT.PROX", "FMP.PROX", "FIXED_FMP.PROX")]
rates_2024 <- rates
# Start loop -----------------------------------------------------------------------------------------------------------

for(k in 1:length(rates_2024)){
  
effort_to_use <- 
  efforts[unique(str_split(efforts, "\\.", 
                           simplify = TRUE))[,2] == 
            str_split(names(rates_2024)[k],  #Note I have K here!
                                   "\\.", simplify = T)[,1]
  ]

effort <- get(effort_to_use, inherits = TRUE) #Grabs the right dataset and searches outside environments

design <-  data.frame(DESIGN = names(rates_2024[k]),
                      STRATIFICATION = str_split(names(rates_2024[k]), "\\.", simplify = T)[,1],
                      ALLOCATION = str_split(names(rates_2024[k]), "\\.", simplify = T)[,2]
)

rates_to_use <- as.data.frame(rates_2024[[k]] %>% filter(ADP == 2022) %>% select(-SAMPLE_RATE))

#Get the equivalent work.data for the trips identified in the effort object

new.wd <- merge(work.data %>% filter(ADP > 2019), #TODO - if birds, use all data.
                unique(effort[c("STRATUM_COL", "wd_TRIP_ID")]), 
                all.y = TRUE, by.x = "TRIP_ID", by.y = "wd_TRIP_ID") #Note, wd_TRIP_ID is not in output

#Add in FIXED gear - will only use this for FIXED_FMP stratifications
new.wd <- merge(new.wd, fmp_to_fixed_ids %>% select(wd_TRIP_ID, GEOFF_GEAR, GEOFF_FMP), 
                  by.x = "TRIP_ID", by.y = "wd_TRIP_ID")

#Calculate metrics for each trip
groundfish <- 
  new.wd %>% 
  filter(SOURCE_TABLE == "Y") %>% #TODO - verify this is right - only retained species?
  group_by(ADP, STRATUM_COL, TRIP_ID, OBSERVED_FLAG, GEOFF_GEAR, GEOFF_FMP) %>%
  summarize(SPECIES = n_distinct(AGENCY_SPECIES_CODE, na.rm = TRUE),
            RETAINED_CATCH = sum(as.numeric(WEIGHT_POSTED), na.rm = TRUE),
            MIN_DATE = min(as.Date(TRIP_TARGET_DATE), na.rm = TRUE),
            MAX_DATE = max(as.Date(LANDING_DATE), na.rm = TRUE)
  ) %>%
  mutate(SPECIES_GROUP_CODE = "GROUNDFISH",
         DURATION = as.numeric(difftime(MAX_DATE, MIN_DATE, units = "days"))) %>%
  select(-MIN_DATE, -MAX_DATE) %>% ungroup() %>% distinct() %>%
  distinct() %>% ungroup()

#Reshape groundfish
groundfish_long <- tidyr::pivot_longer(data = groundfish, #I am using tidyr without loading
                                       names_to = "METRIC",
                                       values_to = "Value",
                                       cols = c(SPECIES, DURATION, RETAINED_CATCH))

# Boil up to totals

# Boil up --------------------------------------------------------------------------------------------------------------

#condition by stratification
if(design$STRATIFICATION %in% c("CURRENT", "FMP")){

  groundfish.summary <-   
  groundfish_long %>%
  group_by(STRATUM_COL, OBSERVED_FLAG, SPECIES_GROUP_CODE, METRIC) %>%
  summarize(Xbar = mean(Value),
            s2 = var(Value),
            sd = sqrt(s2),
            ni = n_distinct(TRIP_ID))

} 

if(design$STRATIFICATION == "FIXED_FMP"){
  groundfish.summary_h <-   
    groundfish_long %>%
    group_by(STRATUM_COL, GEOFF_GEAR, GEOFF_FMP, OBSERVED_FLAG, SPECIES_GROUP_CODE, METRIC) %>%
    summarize(Xbarh = mean(Value),
              s2h = var(Value),
            #  sdh = sqrt(s2h),
              nih = n_distinct(TRIP_ID))
  
  #Now add a column for FIXED
  groundfish.summary_h <- 
    groundfish.summary_h %>% 
    ungroup() %>%
    mutate(GEOFF_FIXED = ifelse(GEOFF_GEAR %in% c("POT", "HAL"),
                                "FIXED", 
                                GEOFF_GEAR)
    )
  
#Get N and n for fixed x FMP
 Ns <- 
   groundfish.summary_h %>% 
   ungroup() %>%
   group_by(STRATUM_COL, GEOFF_FIXED, GEOFF_FMP, OBSERVED_FLAG, SPECIES_GROUP_CODE, METRIC) %>%
   summarize(Nfixed = sum(nih))

 #Add in Ns
 groundfish.summary_h <- merge(groundfish.summary_h, Ns)
  
#Boil up by FMP for fixed gear
groundfish.summary <- 
  groundfish.summary_h %>% ungroup() %>%
  group_by(STRATUM_COL, GEOFF_FIXED, GEOFF_FMP, OBSERVED_FLAG, SPECIES_GROUP_CODE, METRIC) %>%
  summarize(Xbar = weighted.mean(Xbarh, w = nih / max(Nfixed)),
            s2 = weighted.mean(s2h, w = (nih / max(Nfixed))^2),
            sd = sqrt(s2),
            ni = max(Nfixed)
  ) %>% ungroup() %>% select(-GEOFF_FIXED, - GEOFF_FMP)

}

# Power analysis -------------------------------------------------------------------------------------------------------
pwr_ready <- 
  tidyr::pivot_wider(groundfish.summary, 
              values_from = c(Xbar:ni), names_from = OBSERVED_FLAG)

pwr_ready <- merge(pwr_ready, rates_to_use)  #Drops ZERO stratum from outputs bc not in rates

library(pwrss)

pwr <- 
  pwr_ready %>% 
  mutate(POWER = round(
    suppressWarnings(pwrss.t.2means(mu1 = Xbar_Y, 
                                    mu2 = Xbar_N, 
                                    sd1 = sd_Y, 
                                    sd2 = sd_N, 
                                    kappa = n / (STRATA_N - n),
                                    n2 = STRATA_N - n,
                                    alpha = 0.05,
                                    alternative = "not equal", 
                                    verbose = FALSE)$power), 
                     digits = 3),
    EFFECT_SIZE = round(Xbar_N - Xbar_Y, digits = 3),
    EFFECT_PCT_OBS = round(EFFECT_SIZE / Xbar_Y * 100, digits = 1)
    )


# Formatting -----------------------------------------------------------------------------------------------------------
pwr_out <- merge(design, pwr) %>% select(-ADP)

print(paste0(k, "of", length(rates_2024)))  #Shows where we are.

if(k == 1){
  power_out <- pwr_out
}
if(k > 1){ #TODO - rbind failing at FIXED_FMP - not same columns.
  power_out <- rbind(power_out, pwr_out)
}

# rm(list= ls()[!(ls() %in% 
#                   c('work.data', 'power_out', 'rates', 
#                     'pc_effort.CURRENT', 'pc_effort.FMP', 'pc_effort.FIXED_FMP',
#                     'fmp_to_fixed_ids', 'efforts'))])
}

#Output table
monitoring_effects_power_out <- power_out %>% 
 # ungroup() %>% 
  arrange(BUDGET, METRIC)


# Cleanup --------------------------------------------------------------------------------------------------------------

rm(list= ls()[!(ls() %in% c('monitoring_effects_power_out'))])

# save -----------------------------------------------------------------------------------------------------------------

save.image(file = "~/2024 Integrated Analysis/2024_ADP_repo/analyses/power_analyses/Power_analysis_groundfish.Rdata")

# above file saved remotely at https://drive.google.com/drive/folders/1nTLGGb8jx6m-ZlJNMSt6HrUzfnH09jMh?usp=sharing


#load(file = "~/2024 Integrated Analysis/2024_ADP_repo/analyses/Design Performance Evaluations/Power_analysis_groundfish.Rdata")



  

