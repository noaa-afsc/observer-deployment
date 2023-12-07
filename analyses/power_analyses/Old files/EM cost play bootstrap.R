#Bootstrapped estimates

DF <- data.frame(YEAR = c(seq(2015,2022)),
                 CPD = c(1394.83, 1741.26,1718.70,1874.08,675.41,887.20,1815.77,826.14), 
                 WGTS = c(259, 357,706,1005,1817,1442,695, 1221)
                 )
library(boot)

#https://bookdown.org/dereksonderegger/570/3-confidence-intervals-via-bootstrapping.html
mean.function <- function(x, index) {
  d <- x[index]     # This first line will go in ever bootstrap function you make.
  return(mean(d))  
}

BootDist <- 
  boot(data = DF$CPD, weights = DF$WGTS,
       statistic = mean.function, R=100000)

library(ggplot2)
BootDist.graph <- data.frame(xbar=BootDist$t)
ggplot(BootDist.graph, aes(x=xbar) ) +
  geom_histogram(binwidth = 100, color="black", fill = "white") + 
  geom_vline(xintercept = 1203.18, col = "red") +
  geom_vline(xintercept = 
               quantile(BootDist$t, 
                        probs=c(.25, .5, .75)) , 
                        col = "blue",
                        lty = 2) +
  ggtitle('Estimated Sampling distribution of xbar' )

quantile(BootDist$t, 
         probs=c(.25, .5, .75))