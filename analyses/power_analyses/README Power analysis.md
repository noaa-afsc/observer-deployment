---
title: "Power Analysis"
#author: "Garrett"
#date: "July 10, 2014"
output:
  md_document:
    variant: markdown_github
---


I'm working from an example document located [here](https://github.com/bbest/rmarkdown-example/blob/master/README.md).

And using this [guide](https://rmarkdown.rstudio.com/articles_intro.html) for how to write markdown.

# Welcome to the power analysis section of the draft 2024 ADP

These analyses are born from [this google doc] (https://docs.google.com/document/d/1zs4E3N3OCeG5RUAD8xUL2UUIVHg-xujmrCuTwM296nE/edit?usp=sharing).

There are three steps to these analyses:
* Pull data (._datapull.R, ._datapull.Rdata)
* Ready the data (_dataready.R & .Rdata)
* Power analysis on the data (Power_analysis_.r & .Rdata).

There were two types of power analyses performed:
* Power to detect rare events (birds and mammals)
* Power to detect differences between means (monitoring effects)

## Power to detect rare species

This is a novel analysis for an ADP that addresses mandates to monitor birds and mammals under various statutes.  These organisms are relatively rare in the catch and even more so for partial coverage fishing.  Therefore it is worth exploring how likely it would be for monitoring under the 2024 ADP to detect these species should they occur at rates seen in the past (individuals per trip). 

For the pull data I did a pull for mammals and birds.

Mammals were sourced from the FMA database 2013- 2023 from mammals with condition codes that indicated dead animals.  Three species were investigated - Killer whales, Humpback whales, and Stellar sea lions. In a ten year period, no whales of interest were present in partial coverage trips so they were dropped from the analysis.

Bird data was sourced from a query Phil had provided from CAS nontarget data, also from 2013-2023.  Species of interest were Short-tailed albatross, Laysan Albatross, Spectacled eiders, and Steller's eider.

An initial query of available data (Bird_exploration.R) showed that although eiders are coded in CAS as 'Other Birds', they ALSO were not seen in FMA data over a ten year period, so they were dropped from the analysis.

The file 'Bird_datapull.R' contains the data prep. The number of birds of interest from CAS non-target data were added into all 2013- 2022 fishing trips (including zeros). The file 'Bird_Mammal_dataready.R' takes bird data and reduces it to only partial coverage trips.  It then takes the resulting bird (and complementary mammal) data and generates means (Xbar), variance (s2), and total trips (N) values for each stratum from historical data.  These are treated as population estimates.  For designs with the stratification FIXED_FMP, values were first aggregated to each FMP x gear, and then these estimates were aggregated to each FMP x FIXED using the formula for a stratified estimator.  However, the formula for s2 reduces in this case to (Nh/N)^2 * s2.

Combining the above with the N, n, and designs for 2024 data (sourced from draft_rates_effort.Rdata) yielded a dataframe to perform power to detect.  The package [ObsCovgTools](https://github.com/kacurtis/ObsCovgTools) was used to determine the power to detect the stratum level Xbar (individuals per trip) at the sampling level denoted by n for the anticipated N given the variability in the data expressed as Xbar/s2 (called the dispersion factor by [Curtis and Carretta 2020](https://www.sciencedirect.com/science/article/pii/S0165783620300102?via%3Dihub).  These analyses are performed in the file 'Power_analysis_birds_mammals.R'.

## Power to detect monitoring effects

Monitoring effects have been shown to occur in the past Annual Reports and help analysts identify the potential for bias in inferences about fishing effort, catch and bycatch from fisheries-dependent collection sources such as observers and EM.

For this analysis 2020-2022 fishing trips that identify the stratum each (monitored or unmonitored) trip would belong to for each 2024 design were used to extract three metrics to examine monitoring effects:

* retained weight (mt) of groundfish
* number of retained species
* duration (days) of the fishing trip.

As was done for the first power analysis, this analysis aggregates the trip-level data into the mean, variance, and N for each 2024 stratum x monitoring status, each design. For designs with the stratification FIXED_FMP, values were first aggregated to each FMP x gear, and then these estimates were aggregated to each FMP x FIXED using the formula for a stratified estimator.  However, the formula for s2 reduces in this case to (Nh/N)^2 * s2. The resulting data was then combined with the N, n, and designs for 2024 data (sourced from draft_rates_effort.Rdata).  These resulting dataframe was used to perform a power of test.  Specifically, [a two-sample test of independence was conducted](https://cran.r-project.org/web/packages/pwrss/vignettes/examples.html#2_Mean_Difference_(t_Tests)) that treats the monitored and monitored values as different distributions.  The test calculates the power to detect a difference between monitored and unmonitored trips given its magnitude and variance in the past and the expected sample size in 2024.  High power is desirable, and power increases with the magnitude of the differences, lower variances in the distributions, and greater sample size.  The file 'Power_analysis_groundfish.R' was used for this analysis.

## Outputs

loading the file 'Power_tables.Rdata' will provide the two outputs that are formatted as (eventual) tables.















