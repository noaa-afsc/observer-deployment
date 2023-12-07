# Craig Faunce
# July 2023


# Environment ----------------------------------------------------------------------------------------------------------



# Goal -----------------------------------------------------------------------------------------------------------------
#Our goal is to summarize the mean to variance of seabird and chinook salmon species 
# for use in ObsCovgTools.


# Method ---------------------------------------------------------------------------------------------------------------
#We will accomplish this by merging our work.data object with the appropriate stata
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
#load("~/2024 Integrated Analysis/2024_ADP_repo/analyses/Design Performance Evaluations/sandbox.Rdata")

#Load the source data for the ADPs - takes a while
#load("~/2024 Integrated Analysis/2024_ADP_repo/source_data/2024_Draft_ADP_data.rdata")

# Function starts ------------------------------------------------------------------------------------------------------
test.function <- function(summary.df, effort, new.wd){
  # library(devtools)
  # install_github("kacurtis/ObsCovgTools")
  # if(require(ObsCovgTools)) install.packages('ObsCovgTools')
  library(ObsCovgTools)
  
#Feed a df of only positive data (function will fail with BPUE = 0)
obs.df <- summary.df[summary.df$BYPUE > 0 & !is.na(summary.df$n), ] 
#second of these conditions above excludes estimates from zero coverage

if(nrow(obs.df) > 0){
#Initialize loop for every stratum with positive data
 for(j in 1:nrow(obs.df)){
  
  targetProb <- c(seq(1, 100, 1)) # create distribution of probabilities
  cov <- NULL
  dv <- ifelse(obs.df$dispersion[j] < 1, 1, obs.df$dispersion[j])
  for(i in targetProb){
    covx <- data.frame(
      targetP = i,
      ADP = obs.df$ADP[j],
      STRATUM_COL = obs.df$STRATUM_COL[j],
      minCov = plot_probposobs(showplot = FALSE, silent = TRUE, 
                               te = obs.df$N[j], 
                               bpue = obs.df$BYPUE[j], 
                               d = dv, 
                               targetppos = i)$targetoc)
    cov <- rbind(cov, covx)
  }

#Whats my MINIMUM power to detect given my coverage rate?
  if(length(cov[cov$targetP == max(cov$targetP[cov$minCov <= (obs.df$SAMPLE_RATE[j] * 100)]),]) == 0){
    Prob <- cov[cov$targetP == min(cov$targetP),]
    Prob$targetP <- "TOO RARE"
  }
  if(length(cov[cov$targetP == max(cov$targetP[cov$minCov <= (obs.df$SAMPLE_RATE[j] * 100)]),]) != 0){
    Prob <- cov[cov$targetP == max(cov$targetP[cov$minCov <= (obs.df$SAMPLE_RATE[j] * 100)]),]
  }
  
  
# CV 
  targetProb <- c(seq(0.01, 0.99, .01)) # create distribution of probabilities
  cv <- NULL
  dv <- ifelse(obs.df$dispersion[j] < 1, 1, obs.df$dispersion[j])
  for(i in targetProb){
    covx <- data.frame(
      targetcv = i,
      ADP = obs.df$ADP[j],
      STRATUM_COL = obs.df$STRATUM_COL[j],
      minCov = plot_cv(showplot = FALSE,
                       silent = TRUE, 
                       te = obs.df$N[j], 
                       bpue = obs.df$BYPUE[j], 
                       d = dv, 
                       targetcv = i)$targetoc)
    cv <- rbind(cv, covx)
  }
  #Grab CV from coverage
    if(length(cv$targetcv[cv$minCov<=(obs.df$SAMPLE_RATE[j] * 100)]) == 0){
       CVact <- cv[cv$targetcv == max(cv$targetcv),]
       CVact$targetcv <- "TOO RARE"
       }
    if(length(cv$targetcv[cv$minCov<=(obs.df$SAMPLE_RATE[j] * 100)]) != 0){
       CVact <- cv[cv$targetcv == min(cv$targetcv[cv$minCov<=(obs.df$SAMPLE_RATE[j] * 100)]),]
    }
  
  if(j == 1) {
    out <- merge(select(Prob, -"minCov"), select(CVact, -"minCov"))
    } else {
    out <- rbind(out, merge(select(Prob, -"minCov"), select(CVact, -"minCov")))
    }
   print(paste(j,'of',nrow(obs.df),'strata complete'))
 } #End of stratum loop
 
} #End of conditional on positive bycatch

if(nrow(obs.df) == 0){
  out <- data.frame(targetP = NA, targetcv = NA)
}

summary.df <- merge(summary.df, out, all.x = TRUE)

return(summary.df)
}

#LOOP WORKING FOR ONE SPECIES