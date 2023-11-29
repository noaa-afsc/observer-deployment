---
title: "2024_ADP/results Readme"
output: html_document
---

This folder (`2024_ADP/results/`) is where raw outputs of analysis scripts (.rdata files) are locally saved to be referenced by other scripts. These data files reside on the Google drive, and each file's hyperlinks should be included in the scripts that `save()` and `load()` them. This readme is meant to catalog all data files that may reside here, with a description of the file, the file's hyperlink to the Google drive, and a list of the scripts that use them. 

---

### Draft ADP Outputs

---

**draft_rates_effort.rdata**

This object contains the allocated rates for all designs evaluated in the 2024 Draft ADP as well as the fishing effort objects used to create them, where each effort object had a different stratification definition applied, i.e., current (2023 definition), FMP, or FIXED_FMP.

- Created by: `analyses/draft_rates/draft_rates_effort.R`
- Saved to: [Vision 2024 ADP/Data](https://drive.google.com/file/d/1feAzcFhFvMfADQP2Lp2vu4mhh0hQiceQ/view?usp=drive_link)
- Loaded by: 
  - `evaluate_designs.R` 
  - `analyses/`
    - `appendix_A_box_size/appendix_A_box_size.R`
    - `draft_rates/exploration_for_pcfmac.R`
    - `evaluation_bio_comp_n/evaluation_bio_comp_n.R`
    - `evaluation_interspersion/evaluation_interspersion.R` 
    - `evaluation_trips_and_costs/evaluation_trips_and_costs.R`
    - `trip_variance/trip_variance.Rmd`
- Contains objects: `rates`, `pc_effort.CURRENT`, `pc_effort.FMP`, `pc_effort.FIXED_FMP`

---

**scorecard_dt.rdata**

This object is a formatted compilation of outputs from several (but not all) evaluation analyses in the 2024 Draft ADP (see `analyses/` folder: `evaluation_trips_and_costs`, `evaluation_bio_comp_n`)

- Created by: `analyses/evaluation_scorecard_p1/evaluation_scorecard_p1.R`
- Saved to: [Vision 2024 ADP/Data/Draft ADP Outputs](https://drive.google.com/file/d/1Een7cUVuGiCMjMlkedY1NUXW3tCRbsa7/view?usp=drive_link)
- Loaded by: `evaluate_designs.R`
- Contains objects: `scorecard_dt`

---

**scorecard_expense_cv.rdata**

CVs of monitoring expenses from all designs x budgets evaluated in the 2024 Draft ADP.

- Created by: `analyses/evaluation_trips_and_costs/evaluation_trips_and_costs.R`
- Saved to: [Vision 2024 ADP/Data/Draft ADP Outputs](https://drive.google.com/file/d/1RM16ONVHDtWfaIDMnX5znRa4mWxXDr5x/view?usp=drive_link)
- Loaded by: `analyses/evaluation_scorecard_p1/evaluation_scorecard_p1.R`
- Contains objects: `scorecard_expense_cv`

---

**scorecard_bio_comp_n.R**

Counts of sample units (trips or offloads) with biological or composition data from each of the sample designs x budgets evaluated in the 2024 Draft ADP

- Created by: `analyses/evaluation_bio_comp_n/evaluation_bio_comp_n.R`
- Saved to: [Vision 2024 ADP/Data/Draft ADP Outputs](https://drive.google.com/file/d/1hFKoG4jiZYvMpyDfTa1LoDTOnUhhoclE/view?usp=drive_link)
- Loaded by: `analyses/evaluation_scorecard_p1/evaluation_scorecard_p1.R`
- Contains objects: `scorecard_samples_bio`, `scorecard_samples_comp`

---

**scorecard_interspersion.rdata**

Interspersion values (both AK-wide and FMP-specific) from each of the sample designs x budgets evaluated in the 2024 Draft ADP

- Created by: `analyses/evaluation_interspersion/evaluation_interspersion.R`
- Saved to: [Vision 2024 ADP/Data/Draft ADP Outputs](https://drive.google.com/file/d/15ImmB1LDkfeZCtfKy94IhSvk5OF9YQqC/view?usp=drive_link)
- Loaded by: `analyses/evaluation_scorecard_p1/evaluation_scorecard_p1.R`
- Contains objects: `interspersion_ak`, `interspersion_fmp`

---

**Power_tables.Rdata**

Results of power analyses (detection of rate events of marine mammals and seabirds and monitoring effects) from each of the sample designs x budgets evaluation in the 2024 Draft ADP.

- Created by: `analyses/power_analyses/Power tables and figures.R`
- Saved to: [Vision 2024 ADP/Data/Draft ADP Outputs](https://drive.google.com/file/d/1McTozutpdH5dJGWYu7CmG9tHtZkP59xG/view?usp=drive_link)
- Loaded by: `analyses/evaluation_scorecard_p1/evaluation_scorecard_p1.R`
- Contains objects: `design_detect_table`, `detect_table`, `effects_table`

---

### Final ADP Outputs

---

**swor_boot_lst.rdata**

Results of allocation to each of 1,000 bootstrapped fishing effort populations. 

- Created by: `final_rates.R`
- Saved to: [Vision 2024 ADP/Data/Final ADP Outputs](https://drive.google.com/file/d/1HIMAF29uFuUSBdcXZziU5mgSIbYV0FaH/view?usp=drive_link)
- Loaded by: `final_rates.R` (quick load to avoid lengthy run time)
- Contains objects: `swor_boot_lst`

---

**costs_boot_lst.rdata**

Realized costs of simulated deployment (1,000 ODDS iterations) to each of 1,000 bootstrapped fishing effort populations with variation in total N. 

- Created by: `final_rates.R`
- Saved to: [Vision 2024 ADP/Data/Final ADP Outputs](https://drive.google.com/file/d/1Hutbg-HIiC5LnSlc4V4E-fC0-4yMOqOm/view?usp=drive_link)
- Loaded by: `final_rates.R` (quick load to avoid lengthy run time)
- Contains objects: `costs_boot_lst`

---

**final_adp_tables_and_figures_2024.rdata**

Objects containing data for tables and figures to be used by the markdown `final_adp_tables_and_figures.Rmd` to export into report-ready format.

- Created by: `final_rates.R`
- Saved to: [Vision 2024 ADP/Data/Final ADP Outputs](https://drive.google.com/file/d/1hDdNxy98E-DM8ZEfg4mJws_COkop6-vZ/view?usp=drive_link)
- Loaded by: `final_adp_tables_and_figures.Rmd`
- Contains objects: `table_b2_flex`, `table_b3_flex`, `table_2_flex`, `figure_b2`

---

**final_adp_2024_results.rdata**

This data file contains the primary inputs and outputs of the 2024 Final ADP (fishing effort, budget, monitoring costs, rates, and bootstrap results) to be used by the Annual Report.

- Created by: `final_rates.R`
- Saved to: [Vision 2024 ADP/Data/Final ADP Outputs](https://drive.google.com/file/d/17lUFGYKTgD3WE1tOz3ZHRzDiUjWwejVd/view?usp=drive_link)
- Loaded by: *TBD* (2025 Annual Report)
- Contains objects: 
  - Fishing effort: `pc_effort_sub` (most recent 1-year to sample from for 2024), `pc_effort_dt` (all prior fishing effort)
  - Outputs of `effort_prediction.R`: `effort_year`,`effort_strata`,`efp_prob`
  - Budget used and parameters of monitoring costs: `budget_lst` and `cost_params`
  - Final rates: `rates_adp_2024_final`
  - Bootstrap results: `boot_dt`, `costs_dt`
