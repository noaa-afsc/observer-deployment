### stratum_issues.R
Dependencies: 
-  source_data/allocation_evaluation_data_prep.Rdata (now defunct but saved in google drive source data folder)
Outputs: 
-  analyses/stratification/stratum_issues.Rdata
Description: 
-  This is an old script that feeds into analyses/stratification/stratum_issues.Rmd


### stratum_issues.Rmd
Dependencies:
-  analyses/stratification/stratum_issues.Rdata
Outputs:
-  stratum_issues.html
Description: 
-  This document looks at stratification issues following the 2023-Jan PCFMAC, looking at previous fishing effort to see how many trips fished with multiple gear types or fised in multiple FMPs. It also looks at how many current fixed-gear EM vessels previously took an observer.


### stratification_exploration.R
Dependencies:
-  analyses/allocation_evaluation/allocation_evaluation.Rdata
Outputs: 
-  stratification_tables.xlsx
Description:
-  A more involved look at prior partial coverage fishing effort that fished with multiple gear types or fished in multiple FMPs on the same trip. It considers different alternative stratification methods, counting the number of trips that would presumably occur in each stratum. The script does not directly create stratification_tablex.xlsx, but crunches the numbers for it.


### stratification_tables.xlsx
Description:
-  A spreadsheet showing trip counts using current alternative stratification schemes. Conditional formatting was applied to show visualize the risk of getting 0 or fewer than 3 trips in a stratum assuming a 15% selection rate.