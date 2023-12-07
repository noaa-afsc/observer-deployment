# Craig Faunce
# June 2023
options(scipen=999)

# Environment ----------------------------------------------------------------------------------------------------------
# i is iteration for ObsCovgTools
# j is stratum in a design
# k is a design
# l is a species
# m is an effort dataset
# r is a rate (design)


# Goal -----------------------------------------------------------------------------------------------------------------
#Our goal is to summarize the mean to variance of seabird and chinook salmon species 
# for use in ObsCovgTools.


# Method ---------------------------------------------------------------------------------------------------------------
#We will accomplish this by merging our work.data object with the appropriate strata
#definitions used in the 2024ADP from Geoff Mayhew.
#All of these RData files are in source data folder on the google drive for the 2024 ADP project

# Data load -----------------------------------------------------------------------------------------------
#This data set is for play.  Emailed from Geoff Mayhew to me on 7/6/2023. 
#Goal is to write code to aggregate catch for designs and merge these with stratifcations
#to enable use with ObsCovgTools::

#It's 2022 data only. 
#There are two effort objects, 
#one with current stratifications (pc_effort.current) 
#and another that lumps HAL and POT into fixed gear strata (there will be more in the final)
#as well as splits between BSAI and GOA (pc_effort.fixed_fmp). 
#The final column 'STRATUM_COL" is a single column that also corresponds to what is in the rates_2022 object, 
#which is a list showing the allocated rates for the two previously mentioned stratifications with two allocation schemes: 
#min_plus_opt status quo (with the EM-carve off; MPO.SQ) and from the proximity method (PROX).
#The column wd_TRIP_ID corresponds to the TRIP_ID in work.data (for merging with catch data)

#N is determined from the *_effort objects and n is determined by multiplying this value by
#the sample rate in the rates_2022 object.
load("~/2024 Integrated Analysis/2024_ADP_repo/analyses/Design Performance Evaluations/sandbox.Rdata")

#Load the source data for the ADPs - takes a while
load("~/2024 Integrated Analysis/2024_ADP_repo/source_data/2024_Draft_ADP_data.rdata")

# Data munging ---------------------------------------------------------------------------------------------------------
#We will Summarize effort data to get STRATA_COL and N
#We will merge STRATA_COL onto work.data and summarize by STRATA_COL (bycatch, var, mean)
#We will combine outputs from the prior two steps
#Merge with the SAMPLE_RATE from rates_2022
#Calculate n, BPUE, dispersion
#Feed into ObsCovgRates

#Of course and then turn into a function....

#Do you have a handy way to extract names before or after the '.' in an object name? thinking about matching up pc_effort.current with rates_2022$current.mpo_sq etc...
#unlist(strsplit("just.for.example", split = "[.]"))
#then you can index the result using [1] or [2] to grab the one you want


#Summarize effort data to get STRATA_COL and N
library(stringr) #https://stringr.tidyverse.org
library(dplyr)
#https://cran.r-project.org/web/packages/stringr/vignettes/stringr.html
#x=ls()[ls() %in% c("a","c")])

#List of fishing efforts to use
efforts <- ls()[str_detect(ls(), "pc_effort")]

#List of species to use
species <- c('STBR', 'LBRD') 

#TODO - FOR EACH ELEMENT IN rates_2022....
#data_files <- list.files()
#all_vars <- lapply(data_files, function(file)
#{
#  vars_loaded <- load(file)
#  mget(vars_loaded, parent.frame())
#})
for(k in 1:length(rates_2022)){

for(s in 1:length(species)){

#effort_to_use <- 
#  efforts[unique(str_split(efforts, "\\.", simplify = TRUE))[,2] == designs$STRATIFICATION[k]]

effort_to_use <- 
  efforts[unique(str_split(efforts, "\\.", 
                           simplify = TRUE))[,2] == 
            str_to_lower(str_split(names(rates_2022)[k],  #Note I have K here!
                                   "\\.", simplify = T)[,1])
          ]

effort <- get(effort_to_use, inherits = TRUE) #Grabs the right dataset and searches outside environments

#table(is.na(new.wd$STRATUM_COL))

#DO STUFF
#for each species....
design <-  data.frame(DESIGN = str_to_lower(names(rates_2022[k])),
                      STRATIFICATION = str_to_lower(str_split(names(rates_2022[k]), "\\.", simplify = T)[,1]),
                      ALLOCATION = str_to_lower(str_split(names(rates_2022[k]), "\\.", simplify = T)[,2])
                      )
effort.summary <- 
        merge(design, effort %>% 
          group_by(STRATUM_COL) %>% 
          summarize(N = n_distinct(TRIP_ID)) 
        )
effort.summary <- merge(effort.summary, rates_2022[[k]], all = TRUE) %>% ungroup()

effort.summary$Species <- species[s]

#Get the equivalent work.data for the trips identified in the effort object
new.wd <- merge(work.data,
                unique(effort[c("STRATUM_COL", "wd_TRIP_ID")]), 
                all.y = TRUE, by.x = "TRIP_ID", by.y = "wd_TRIP_ID") #Note, wd_TRIP_ID is not in output

new.summary <- 
  new.wd %>% group_by(STRATUM_COL, TRIP_ID) %>% 
  summarize(Bycatch.trip = sum(SPECIES_COUNT[SPECIES_GROUP_CODE == species[s]], 
                               na.rm = TRUE)) %>% 
  ungroup() %>%
  group_by(STRATUM_COL) %>% summarize(Bycatch = sum(Bycatch.trip),
                                               VAR = var(Bycatch.trip),
                                               MEAN = mean(Bycatch.trip)) %>% 
  ungroup()

#Combine summaries
summary.df <- 
  merge(effort.summary, new.summary) %>% 
  group_by(ADP, STRATUM_COL) %>% 
  mutate(n = ifelse(SAMPLE_RATE > 0, round(N * SAMPLE_RATE, 1), 0), 
         BYPUE = ifelse(Bycatch > 0, round(Bycatch / N, 6), 0), 
         dispersion = ifelse(Bycatch > 0, round(VAR/MEAN, 6), 0))

# APPLY FUNCTION -------------------------------------------------------------------------------------------------------
if(s == 1){
x <- test.function(summary.df, effort, new.wd)
}
if(s > 1){
  x <- rbind(x, test.function(summary.df, effort, new.wd))
}

} #End of species loop
if(k == 1){
  y <- x
}
if(k > 1){
  y <- merge(y, x, all = TRUE) #TODO - bc some elements in rates_2022 have 6 and some have seven columns.
}

}
# End of k loop.

y <- y %>% arrange(DESIGN, STRATUM_COL)
#TODO- amend test.function to work with mapply to generate added columns on summary.df
#TODO- lapply the above for all objects in rates_2022


#From whiteboard with Geoff: (not quite right but use for ideas)
#mapply(
#function(x,y){
#  effort <- get(strsplit(y)[1])
#  y1
#  y2
#    lapply(x(), something, y1, y2)
#    x is rates_2022
#    y <- names(something)
#}
#)
#Also see: https://www.rforecology.com/post/how-to-use-apply-functions/
#https://ademos.people.uic.edu/Chapter4.html, example 5.0.2
my.matrx <- matrix(c(1:10, 11:20, 21:30), nrow = 10, ncol = 3)
tdata <- as.data.frame(cbind(c(1,1,1,1,1,2,2,2,2,2), my.matrx))
colnames(tdata)
tdata$V5 <- mapply(function(x, y) x/y, tdata$V2, tdata$V4)
#tdata$V5

#List extraction
#https://www.spsanderson.com/steveondata/posts/2023-07-19/index.html