#Craig Faunce 
# GOAL - to explore where bird data bycatch is located
# Begin - June 2023
# End - 8/19/2023

#Phil Ganz - NOAA Federal,
#I put a csv with the translations from species to species group here: 
#https://drive.google.com/file/d/1pfIHXo7CcLYlNtbcEplsXTlljr0fYOyR/view?usp=drive_link

#If you want birds, you can filter(species_group_id >= 209 and species_group_id <= 224). 
#Alternatively, I think all the bird species_group_names start with "Birds"
species.codes <- 
  read.csv("~/2024 Integrated Analysis/2024_ADP_repo/analyses/power_analyses/Birds/species_to_group.csv")

library(dplyr)
bird.codes <- species.codes %>% filter(grepl('Birds', SPECIES_GROUP_NAME)) %>%
  select(SPECIES_GROUP_NAME, SPECIES_GROUP_CODE, AGENCY_SPECIES_CODE, AGENCY_SPECIES_NAME) %>%
  distinct()

#What are the codes for Albatross?
bird.codes %>% filter(grepl('Albatross', SPECIES_GROUP_NAME)) %>%
  select(SPECIES_GROUP_NAME, SPECIES_GROUP_CODE, AGENCY_SPECIES_CODE, AGENCY_SPECIES_NAME) %>%
  distinct()
# SPECIES_GROUP_NAME SPECIES_GROUP_CODE AGENCY_SPECIES_CODE    AGENCY_SPECIES_NAME
# 1 Birds - Black-footed Albatross               BBRD                 852 BLACK-FOOTED ALBATROSS
# 2       Birds - Laysan Albatross               LBRD                 851       LAYSAN ALBATROSS
# 3 Birds - Short-tailed Albatross               STBR                 850 SHORT-TAILED ALBATROSS
# 4 Birds - Unidentified Albatross               UABR                 849 ALBATROSS UNIDENTIFIED

#So AGENCY_SPECIES_CODE %in% c('LBRD', 'STBR')

#How about eiders
bird.codes %>% filter(grepl('EIDER', AGENCY_SPECIES_NAME)) %>%
  select(SPECIES_GROUP_NAME, SPECIES_GROUP_CODE, AGENCY_SPECIES_CODE, AGENCY_SPECIES_NAME) %>%
  distinct()
#Darn, from this, Spectacled Eider is species 864, Stellars Eider is 865, but these are all under
# SPECIES_GROUP_CODE 'OBRD'
# SPECIES_GROUP_NAME SPECIES_GROUP_CODE AGENCY_SPECIES_CODE AGENCY_SPECIES_NAME
# 1      Birds - Other               OBRD                1163  UNIDENTIFIED EIDER
# 2      Birds - Other               OBRD                 863          KING EIDER
# 3      Birds - Other               OBRD                 864    SPECTACLED EIDER
# 4      Birds - Other               OBRD                 865     STELLER'S EIDER
# 5      Birds - Other               OBRD                 866        COMMON EIDER

#Now, IF there are Eiders in FMA data, I'll need to take catch data for seabirds 
#from observer data to get rates, Mean and variance,
#And I will have to do my best for matching stratum to what is in each design.

#Lets check the FMA data

# Open NORPAC database connection -------
library(sqldf)

channel <- dbConnect(odbc::odbc(),"AFSC",
                     UID    = rstudioapi::askForPassword("Enter your NORPAC Username: "),
                     PWD    = rstudioapi::askForPassword("Enter your NORPAC Password: "))
# Query Bird data ------------------------------------------------------------------------------------------------------
#From Andy Kingham FMA, there is a view to query the bird data.
#Lets vet this for the species of interest
birds_in <-
  dbGetQuery(channel,
  "SELECT *
  FROM norpac.atl_birds_in_samples v
  where v.species_code in(850, 851, 864, 865) -- Albatross and Eiders
  order by v.species_code desc, v.year")

table(birds_in$SPECIES_CODE) #No eiders! Only species 850 & 851.  
#850 851 
#7 179
#This means that there were no eiders in observer samples by FMA.  
#So we are not missing them (i.e. they are not hidden in OBRD in CAS)

#Therefore we will grab them from the valhalla data.

#Update August 2021.  
#On Wed, Aug 2, 2023 at 10:53 AM Phil Ganz - NOAA Federal <phil.ganz@noaa.gov> wrote:#
#  Hi Craig,
#Rerunning valhalla was taking a very long time 
#(so long in fact that my computer kept shutting down or throwing an error 
#before it could finish), so instead I took an approach similar to what we do for PSC and 
#pulled the nontarget and ecosystem catch for each report_id since 2012. 
#You should then be able to join that to the multiyear valhalla object that is 
#in 2024_ADP_data.rdata. 
#I note that there's a species_count column in the nontarget and ecosystem catch spreadsheet, 
#so you may need to rename that before joining, 
#since I think there is also a species_count column in valhalla.

#Also, it looks like the query returned so many rows that sql split the output 
#between the first two spreadsheets in the workbook. 
#I checked that all the counts and weights are there, though, and 
#you can bring them in with something like:
# library(readxl)
# nontarget <- rbind(read_excel("~/2024 Integrated Analysis/2024_ADP_repo/source_data/nontarget_ecosystem_catch.xlsx", sheet = 1),
#                    read_excel("~/2024 Integrated Analysis/2024_ADP_repo/source_data/nontarget_ecosystem_catch.xlsx", sheet = 2, 
#                               col_names = c("YEAR", "REPORT_ID", "SPECIES_GROUP_CODE", 
#                                             "SPECIES_COUNT", "SPECIES_WEIGHT")))
# birds <- nontarget %>% filter(SPECIES_GROUP_CODE %in% c('STBR', 'LBRD')) %>%
#   group_by(REPORT_ID, SPECIES_GROUP_CODE) %>% summarize(SPECIES_COUNT = sum(SPECIES_COUNT))



