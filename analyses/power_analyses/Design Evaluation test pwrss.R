#Craig Faunce
#Goal - to test drive the package for power of test
# Bulus, M. (2023). pwrss: Statistical Power and Sample Size Calculation Tools. 
#R package version 0.3.1. https://CRAN.R-project.org/package=pwrss
#https://rpubs.com/metinbulus/pwrss
#Start 8/22/2023
#End 


library(pwrss)
load(file = "~/2024 Integrated Analysis/2024_ADP_repo/analyses/Design Performance Evaluations/Birds Groundfish/Bird_Groundfish_datapull.Rdata")

testd <- bg_ready[1:4,]

library(dplyr)
testd$Value <- c(0.11, 0.1, 0.2, 0)
testd$OBSERVED_FLAG <- c("Y", "Y", "N", "N")
testd$DESIGN <- "CURRENT"

# What is the statistical power given that the sample size for the second group is 50 (n2 = 50) 
# and groups have equal sample sizes (kappa = n1 / n2 = 1)?

# pwrss.t.2means(mu1 = 30, mu2 = 28, sd1 = 12, sd2 = 8, kappa = 1, 
#                n2 = 50, alpha = 0.05,
#                alternative = "not equal")

fortest <- 
  testd %>% 
  group_by(DESIGN, OBSERVED_FLAG) %>%
  summarize(Xbar = mean(Value),
            s2 = var(Value),
            sd = sqrt(s2))#,
        #    sdtest = sd(Value)) #This is same as sqrt var which is cool.


#In our theoretical example we would grab N24 and n24 from Geoffs rates_2022 object 
#and pc_effort objects
data2024 <- data.frame(N24 = 100, n24 = 15)


# Start function -------------------------------------------------------------------------------------------------------

power.fxn <- function(data1, data2){
 
  fortest <- 
    data1 %>% 
    group_by(DESIGN, OBSERVED_FLAG) %>%
    summarise(Xbar = mean(Value),
              s2 = var(Value),
              sd = sqrt(s2))  
 
fortest <- fortest %>% mutate(POWER = round(
                       pwrss.t.2means(mu1 = Xbar[OBSERVED_FLAG == "Y"], 
                       mu2 = Xbar[OBSERVED_FLAG == "N"], 
                       sd1 = sd[OBSERVED_FLAG == "Y"], 
                       sd2 = sd[OBSERVED_FLAG == "N"], 
                       kappa = data2$n24 / (data2$N24 - data2$n24),
                       n2 = data2$N24 - data2$n24,
                       alpha = 0.05,
                       alternative = "not equal", verbose = FALSE)$power, digits = 3))

design_out <- 
  fortest %>% ungroup() %>% group_by(DESIGN, POWER) %>%
  summarise(EFFECT = round(Xbar[OBSERVED_FLAG == "N"] - Xbar[OBSERVED_FLAG == "Y"], digits = 3),
            EFFECT_PCT_OBS = round(EFFECT / Xbar[OBSERVED_FLAG == "Y"] * 100, digits = 1))

return(design_out)
}

test_out <- power.fxn(testd, data2024)



