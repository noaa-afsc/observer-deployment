# 2024 ADP Style Guide

### GitHub Rules
* Each folder should contain a readme - 
  - Readme file - orientation of the analysis, where things are, how things work
  - What the project is about, ordering of R scripts (as needed)
  - README_folder_name
* Try to keep one .rmd per folder - but could have multiple R scripts that feed into it.
* Functions are saved in .R files
  - Exccluding short function that is specific to one immediate task
  - Functions with similar purposes or minor variations can be saved in the same script.
  - Functions used in multiple scripts should be saved in the common_functions/ folder; otherwise those function scripts reside in the folder they are used in.

### Repository Structure
* output_figures/
* source_data/, e.g., valhalla (local), shape files, budget information (local) csv, etc
* final_documents/ - when complete, save PDFs of final report, analytic plan, etc.
* common_functions/ - for functions with uses in multiple scripts
* analyses/
  - allocation_designs
    - status_quo
    - equal_rates
    - etc
  - allocation_evaluation/
  - zero_coverage/
    - Optimize on vessels using strata sample package
    - zc_data_munge
    - zc_analysis
    - zc.rmd
  - spatiotemporal_boxes/
  - monitoring_costs/
  - comparison_of_designs/

### Coding Rules
- Script header info
  - Script authors and what it does
  - (optional) Specify ordering of .R scripts (that need to be run prior to current script)
  - Load all packages at top of script, followed by functions to load
- Provide section breaks and headers
  - Control+Shift+R makes sections / section labels
  - Four dashes# after comment defines headers
    - # header 1 ----
    - ## header 2 ----
- Naming conventions:
  - lowercase and underscore (get_data)
  - Files - 
    - Scripts should end in .R (default)
    - Workspaces end in .RData (default); names of output matches R script names (e.g. get_data.RData)
  - Functions - 
    - Must be action words/verbs; 
    - More descriptive verbs are better (e.g. transform_input, better than transform)
  - Objects
    - ..._df for dataframe
    - ..._lst for lists
    - ..._mat for matrices
- Comment often
  - Each action should be commented
    - Keep actions to as few lines of code as practicable
    - If it is not obvious what a line of code is doing, comment
      - Code reviewers can ask for additional comments
  - Comments left justified; above code
- Spaces before and after operators, with nested parentheses as needed
