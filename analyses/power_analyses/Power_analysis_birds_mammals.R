#Craig Faunce
#Goal - to provide a power of detection for rare event species such as birds and mammals 
# for the 2024 ADP
#Start - 8/25/23
#End - 8/25/23

# Environment ----------------------------------------------------------------------------------------------------------
library(stringr) #https://stringr.tidyverse.org
library(dplyr)
options(dplyr.summarise.inform = FALSE)

# Load and datamunge -----------------------------------------------------------------------------------------------------------------

# This file builds on the file Bird_Mammal_dataready.
load(file = "~/2024 Integrated Analysis/2024_ADP_repo/analyses/power_analyses/Bird_Mammal_dataready.Rdata")

# ...and is similar in structure to Power_analysis_groundfish
#rates_2024 <- rates[!names(rates) %in% c("CURRENT.PROX", "FMP.PROX", "FIXED_FMP.PROX")]
rates_2024 <- rates
mammal_efforts <- ls()[str_detect(ls(), "mammals.")]

# Loop through designs -------------------------------------------------------------------------------------------------
for(k in 1:length(rates_2024)){

design <-  data.frame(DESIGN = names(rates_2024[k]),
                      STRATIFICATION = str_split(names(rates_2024[k]), "\\.", simplify = T)[,1],
                      ALLOCATION = str_split(names(rates_2024[k]), "\\.", simplify = T)[,2]
)

mammal_effort_to_use <- 
  mammal_efforts[unique(str_split(mammal_efforts, "\\.", 
                           simplify = TRUE))[,2] == 
            str_split(names(rates_2024)[k],  #Note I have K here!
                      "\\.", simplify = T)[,1]
          ] 

bird_efforts <- ls()[str_detect(ls(), "birds.")]

bird_effort_to_use <- 
  bird_efforts[unique(str_split(bird_efforts, "\\.", 
                           simplify = TRUE))[,2] == 
            str_split(names(rates_2024)[k],  #Note I have K here!
                      "\\.", simplify = T)[,1]
          ] 

rates_to_use <- as.data.frame(rates_2024[[k]] %>% filter(ADP == 2022))

#Put birds and mammals into one
new.wd <- 
  merge(get(mammal_effort_to_use, inherits = TRUE), #Grabs the right dataset and searches outside environments
        get(bird_effort_to_use, inherits = TRUE),
  all = TRUE)

#Calculate metrics for each stratum
new.wd <- new.wd %>% ungroup() %>% mutate(DISPERSION = s2 / Xbar) #results in NaNs for zero bycatch

#filter out records with no bycatch
#!!!!!!!!!!NEW new.wd <- new.wd %>% filter(!is.nan(DISPERSION)) 

#Now add in our N and n for 2024...
pwr_ready <- merge(new.wd, rates_to_use)  #Drops ZERO stratum from outputs bc not in rates
pwr_ready <- merge(pwr_ready, design) #Add in the design
pwr_ready <- pwr_ready %>% mutate(DISPERSION_TO_USE = ifelse(DISPERSION < 1, 1, round(DISPERSION, digits = 1)))

#Reduce to relevant columns
pwr_ready <- 
  pwr_ready %>% 
  select(BUDGET, DESIGN, STRATIFICATION, ALLOCATION, SPECIES_GROUP_CODE, 
         METRIC, Xbar, s2, DISPERSION_TO_USE, STRATUM_COL, STRATA_N, SAMPLE_RATE, n)

if(k == 1)
  detect_power_out <- pwr_ready
if(k > 1)
  detect_power_out <- rbind(detect_power_out, pwr_ready)
} #end K loop

#WE WANT ALL OF OUR DATA IN ONE DF SO WE DO THE BELOW ONCE!

# Power to detect ---------------------------------------------------------------------------------------------
library(ObsCovgTools)

results <- vector(mode = "numeric", length = nrow(detect_power_out))


for(j in 1:nrow(detect_power_out)){

targetProb <- c(seq(1, 100, 1)) # create distribution of probabilities
cov <- NULL
for(i in targetProb){
  if(detect_power_out$Xbar[j] > 0){
  covx <- data.frame(
    targetP = i,
    STRATUM_COL = detect_power_out$STRATUM_COL[j],
    SAMPLE_PCT = round(detect_power_out$SAMPLE_RATE[j] * 100, 1),
    minCov = plot_probposobs(showplot = FALSE, silent = TRUE, 
                             te = detect_power_out$STRATA_N[j], 
                             bpue = detect_power_out$Xbar[j], 
                             d = detect_power_out$DISPERSION_TO_USE[j], 
                             targetppos = i)$targetoc) #Note these are percents!
  cov <- rbind(cov, covx)

cov

hold <- cov %>% filter(minCov <= SAMPLE_PCT)

PROBABILITY <- ifelse(nrow(hold) >= 1, max(hold$targetP) / 100, 0)
  }
  
if(detect_power_out$Xbar[j] == 0){
PROBABILITY <- NA  
}  
}
results[j] <- PROBABILITY

}

detect_power_out$PROBABILITY_TO_DETECT <- results
# Clean up -----------------------------------------------------------------------------------------------

rm(list= ls()[!(ls() %in% c('detect_power_out'))])

# TEST x DESIGN --------------------------------------------------------------------------------------------------------

# In this section we will calculate the TOTAL estimated bycatch for each DESIGN and its power

#Aggregate to design using stratified estimators - only for strata that have dead stuff!
design_detect_power_out <- 
  detect_power_out %>%
  group_by(BUDGET, DESIGN, SPECIES_GROUP_CODE) %>%
  summarize(N = sum(STRATA_N),
            nt = sum(n),
            Xbar = sum((STRATA_N * Xbar)) / N,
            s2 = sum(
              (STRATA_N / N)^2 * 
                ((STRATA_N - n) / STRATA_N) * 
                (s2 / n)
              ),
            DISPERSION = s2 / Xbar
  )

design_detect_power_out <- 
  design_detect_power_out %>% 
  mutate(DISPERSION_TO_USE = 
           ifelse(DISPERSION < 1, 1, round(DISPERSION, digits = 1)),
         SAMPLE_RATE = nt / N)

design_results <- vector(mode = "numeric", length = nrow(design_detect_power_out))

for(j in 1:nrow(design_detect_power_out)){
  
  targetProb <- c(seq(1, 100, 1)) # create distribution of probabilities
  cov <- NULL
  for(i in targetProb){
    covx <- data.frame(
      targetP = i,
    #  STRATUM_COL = design_detect_power_out$STRATUM_COL[j],
      SAMPLE_PCT = round(design_detect_power_out$SAMPLE_RATE[j] * 100, 1),
      minCov = plot_probposobs(showplot = FALSE, silent = TRUE, 
                               te = design_detect_power_out$N[j], 
                               bpue = design_detect_power_out$Xbar[j], 
                               d = design_detect_power_out$DISPERSION_TO_USE[j], 
                               targetppos = i)$targetoc) #Note these are percents!
    cov <- rbind(cov, covx)
  }
  cov
  
  hold <- cov %>% filter(minCov <= SAMPLE_PCT)
  
  PROBABILITY <- ifelse(nrow(hold) >= 1, max(hold$targetP) / 100, 0)
  
  design_results[j] <- PROBABILITY
  
}

design_detect_power_out$PROBABILITY_TO_DETECT <- design_results

#Cleanup (Again)
rm(list= ls()[!(ls() %in% c('detect_power_out', 'design_detect_power_out'))])

# Save -----------------------------------------------------------------------------------------------------------------
save.image(file = "~/2024 Integrated Analysis/2024_ADP_repo/analyses/power_analyses/Power_analysis_birds_mammals.Rdata")

# above file saved remotely at https://drive.google.com/drive/folders/1nTLGGb8jx6m-ZlJNMSt6HrUzfnH09jMh?usp=sharing
