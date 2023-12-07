# Use the ObsCovgTools package to understand observer coverage effort needed


# Install the ObsCovgTools package if needed
library(devtools)
install_github("kacurtis/ObsCovgTools")


# Load Packages
library(ObsCovgTools)
library(ggplot2)


## 1. Management objective: p(bycatch is positive)
# Create a distribution to give variability in the target probability

?plot_probposobs # check documentation

effort <- 6575 
bpue <- 0.3

plot_probposobs(te = effort, bpue = bpue, d = 2, targetppos = 95) # single run

targetProb <- c(seq(5, 90, 5), seq(91,100,1)) # create distribution of probabilities
cov <- NULL
for(i in targetProb){
  covx <- data.frame(
    targetP = i, 
    minCov_d1 = plot_probposobs(te = effort, bpue = bpue, d = 1, targetppos = i)$targetoc,
    minCov_d2 = plot_probposobs(te = effort, bpue = bpue, d = 2, targetppos = i)$targetoc,
    minCov_d3 = plot_probposobs(te = effort, bpue = bpue, d = 3, targetppos = i)$targetoc)
  cov <- rbind(cov, covx)
}

goal1 <- tidyr::gather(cov, d, coverage, minCov_d1:minCov_d3) # convert to long dataset

(goal1plot <- 
    ggplot(goal1, aes(x = coverage, y = targetP, linetype = d)) +
    geom_line() + 
    scale_linetype_manual(name = "", values = c(2,1,3), labels = c("d=1","d=2","d=3")) +
    theme(legend.position = c(0.9, 0.7)) +
    labs(y = "Probability of positive bycatch", x = "Coverage (%)"))

