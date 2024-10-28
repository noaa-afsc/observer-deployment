# ADP

The main script, `selection_rates.R`, is where the selection rates for the Draft and Final ADPs are determined. However, several processes must take place each year to update the ADP, outline below.

### Reference:
- The *ADP year* is the year for which the ADP is intended. If it is 2024 now (aka the *current year*), we are planing for the 2025 ADP year.
- The [google drive for this project](https://drive.google.com/drive/folders/18yn0IewNpQuPpCIL2Ejp-Pg-5GPMD5xl) resides in the FMA Analytical Services Program shared google drive. This is where the source data and outputs are saved to, separately from this respository.

## Project Workflow:
- The **`loki.valhalla`** table in the NORPAC database must be up-to-date, meaning it contains all trips through the year *prior to the current year*. This is typically performed around April of the current year, as it is essential for the Annual Report that is performed earlier in the year. This is pulled via an SQL query in `get_data.R`
- An AKRO team member must run a version of **`valhalla`** for the *current* year and also save this to the shared google drive. If they don't also have a NORPAC connection, they may first need an FMA team member to run the `sql_pull_fma_for_valhalla.R` script and e-mail them the resulting .rdata file. 
- A AKRO team member must run the **'sql_pull_akro.R`** script in this repo and upload the outputs to the shared google drive.
- The `selection_rates.R` script can be run, but may also need updates from additional scripts in the `analyses/` subfolder, including `analyses/monitoring_costs/` and `analyses/effort_prediction/`. The products of those files are also uploaded to the Shared G-drive and saved locally to the `source_data` folder.
