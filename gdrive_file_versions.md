# Gdrive Files

This document lists the `.rdata` file versions of the data objects created and/or used in the final version of each Annual Deployment Plan analysis. The version number is used to specify which version of a file to `gdrive_download()` to recreate the analyses.

------------------------------------------------------------------------

## 2024 Final ADP

### SQL data pulls:

-   **trawl_em_plant_days_2025.rdata [ver: 5]**

    Assignments of plant observers to estimate duration of GOA pollock season.

    -   created by: `analyses/monitoring_costs/monitoring_costs.R`
    -   local: `sql_pulls/trawl_em_plant_days_2025.rdata`
    -   gdrive: `Projects/ADP/Monitoring Costs - CONFIDENTIAL/`

### Other inputs:

-   **final_adp_2024_results.rdata [ver: 1]**

    Previous ADP's selection rates, used to estimate additional monitored days before next ADP year.

    -   created by: `final_rates.R` of 2024 Final ADP (see [release](https://github.com/Alaska-Fisheries-Monitoring-Analytics/ADP/tree/Final_2024))
    -   used by: `analyses/monitoring_costs/monitoring_costs.R`
    -   local: `results/final_adp_2024_results.rdata`
    -   gdrive: `Projects/ADP/Output/`
    
-   **2025_Draft_ADP_tables.rdata [ver: 1]**
    
    Draft ADP's output tables, used to compare with outputs of the final version
    
    -  created by `selection_rates.R` of 2025 Draft ADP (see [release](https://github.com/Alaska-Fisheries-Monitoring-Analytics/ADP/tree/Draft_2025))
    -  used by: `selection_rates.R`
    -  local: `results/2025_Draft_ADP_tables.rdata`
    -  gdrive: `Projects/ADP/Output`

### Intermediate products:

-   **2025_Final_ADP_data.rdata [ver: 12]**

    Lightly munged version of Valhalla, used for effort prediction and allocation.

    -   created by: `get_data.R`
    -   used by: `selection_rates.R`
    -   local: `source_data/2025_Final_ADP_data.rdata`
    -   gdrive: `Projects/ADP/source_data/`

### Final outputs:

------------------------------------------------------------------------

## 2025 Draft ADP

### SQL data pulls:

-   **kodiak_plant_days.rdata [ver: 1]**

    Assignments of plant observers to estimate duration of GOA pollock season.

    -   created by: `get_data.R`
    -   local: `source_data/2025_Draft_ADP_data.rdata`
    -   gdrive: `Projects/ADP/Monitoring Costs - CONFIDENTIAL/`

### Other inputs:

-   **final_adp_2024_results.rdata [ver: 1]**

    Previous ADP's selection rates, used to estimate additional monitored days before next ADP year.

    -   created by: `final_rates.R` of 2024 Final ADP (see [release](https://github.com/Alaska-Fisheries-Monitoring-Analytics/ADP/tree/Final_2024))
    -   used by: `analyses/monitoring_costs/monitoring_costs.R`
    -   local: `results/final_adp_2024_results.rdata`
    -   gdrive: `Projects/ADP/Output/`

-   **cost_params_2025.Rdata [ver: 7]**

    Cost estimates and parameters of all monitoring methods, used by allocation algorithm.

    -   created by: `analyses/monitoring_costs/monitoring_costs.R`
    -   used by: `selection_rates.R`
    -   local: `source_data/cost_params_2025.Rdata`
    -   gdrive: `Projects/ADP/Monitoring Costs - CONFIDENTIAL/`

-   **pc_effort_st_2025.Rdata [ver: 11]**

    Munged version of `2025_Draft_ADP_data.rdata` representing fishing effort in the upcoming ADP year, to be used by the allocation algorithm.

    -   created by: `/selection_rates.R`
    -   used by:
        -   `selection_rates.R`
        -   `analyses/monitoring_costs/monitoring_costs.R`
    -   local: `source_data/pc_effort_st_2025.Rdata`
    -   gdrive: `Projects/ADP/Output/`

### Intermediate products:

-   **2025_Draft_ADP_data.rdata [ver: 1]**

    Lightly munged version of Valhalla, used for effort prediction and allocation.

    -   created by: `get_data.R`
    -   used by: `selection_rates.R`
    -   local: `source_data/2025_Draft_ADP_data.rdata`
    -   gdrive: `Projects/ADP/source_data/`

-   **cost_params_2025.Rdata [ver: 3]**

    Cost estimates and parameters of all monitoring methods, used by allocation algorithm.

    -   created by: `analyses/monitoring_costs/monitoring_costs.R`
    -   used by: `selection_rates.R`
    -   local: `source_data/cost_params_2025.Rdata`
    -   gdrive: `Projects/ADP/Monitoring Costs - CONFIDENTIAL/`

-   **pc_effort_st_2025.Rdata [ver: 4]**

    Munged version of `2025_Draft_ADP_data.rdata` representing fishing effort in the upcoming ADP year, to be used by the allocation algorithm.

    -   created by: `/selection_rates.R`
    -   used by:
        -   `selection_rates.R`
        -   `analyses/monitoring_costs/monitoring_costs.R`
    -   local: `source_data/pc_effort_st_2025.Rdata`
    -   gdrive: `Projects/ADP/Output/`

### Final outputs:

-   **draft_adp_2025_results.rdata [ver: 1]**

    Final outputs of the ADP analyses, containing objects used by the allocation algorithm, the algorithm's outputs, and summary tables.

    -   created by: `selection_rates.R`
    -   local: `results/draft_adp_2025_results.rdata`
    -   gdrive: `Projects/ADP/Output`

------------------------------------------------------------------------
