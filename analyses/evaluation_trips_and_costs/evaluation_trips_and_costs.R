# 2024 Draft ADP : Evaluation - [Trip Counts and Costs]

# Author: Geoff Mayhew
# Start Date: 24 Aug 2024

#======================================================================================================================#
# Preparation ----------------------------------------------------------------------------------------------------------
#======================================================================================================================#

#===================#
## Load Packages ----
#===================#

library(data.table)         # Data wrangling
library(ggplot2)            # Plotting
library(scales)             # for defining new scales
library(ggh4x)              # for nested facets
library(grid)               # For unit.pmax  to get widths of grobs so that plots have matching dimensions
library(gridExtra)          # For arrangeGrob to combine plots
library(sf)                 # Spatial analyses
library(flextable)          # For print-ready tables
library(dplyr)              # For piping and handling sf objects

#=============================#
## Load data and data prep ----
#=============================#

load("results/draft_rates.rdata")           # Raw output from nalyses/draft_rates/draft_rates.R - box definitions and raw rates
load("results/draft_rates_effort.rdata")    # Outputs from analyses/draft_rates/draft_rates.R - compiled rates and effort
load("source_data/ak_shp.rdata")            # shp_land, shp_nmfs, and shp_centroids added to global

# Add 'STRATA' back in, as the cost functions need them (and not stratum_col)
rates <- lapply(rates, function(x0) {
  # x0 <- rates[[12]]
  x1 <- copy(x0)
  x1[, STRATA := gsub("[-].*$", "", STRATUM_COL)]
  x1[, RATE := SAMPLE_RATE]
  x1[, FILL := fcase(
    STRATA %like% "OB", "OB", 
    STRATA %like% "EM_TRW", "EM_TRW",
    STRATA %like% "EM_HAL|EM_POT|EM_FIXED", "EM_FG")]
  if( any(x1$STRATUM_COL %like% "BSAI|GOA")) x1[, BSAI_GOA := gsub("^.*[-]", "", STRATUM_COL)]
  x1
})

#===============#
## Functions ----
#===============#

source("analyses/allocation_evaluation/functions.R")

#===============#
## Constants ----
#===============#

allo_vec <- c("EQUAL", "STATUS_QUO", "CWB", "PROX")
strat_vec <- c("CURRENT", "FMP", "FIXED_FMP")

#================#
# Cost Curves ----
#================#

# NOTE: In this figure, at-sea observers and fixed-gear EM have cost per day plotted, whereas EM_TRW is CPT. We could
# scale OB and EM_FG by mean trip duration to also get CPT, but given that we're using free y-scales and obscuring the 
# actual values, it doesn't actually change the figures at all (scalar change doesn't result in a different curve.)

cost_dt_melt <- melt(cost_dt, id.vars = c("ADP", "MON_RATE"), measure.vars = c("OB_CPD", "EMFG_CPD", "EMTRW_CPT"))
cost_dt_melt[, Monitoring_Method := fcase(variable == "OB_CPD", "At-sea Observer", variable == "EMFG_CPD", "Fixed-gear EM", variable == "EMTRW_CPT", "Trawl EM")]
figures_cost_curves <- ggplot(cost_dt_melt[ADP == 2022 & MON_RATE > 0.025], aes(x = MON_RATE, y = value)) +
  facet_wrap(. ~ Monitoring_Method, scales = "free") + geom_line(linewidth = 1)  + 
  theme(
    axis.text = element_blank(), axis.ticks = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank()  ) + 
  labs(x = "Monitoring rate", y = "Cost per Unit")

# Figure 2-1
ggsave(
  figures_cost_curves, 
  file = "output_figures/figure_cost_curves.png", 
  width = 6, height = 2, units = "in"
)

#================#
# Trip Counts ----
#================#

# This is not the evaluation metric of the number of trips sampled (composition vs biological), but is simply the number
# of sampled trips by each stratum (direcy output of the allocation rates X STRATA_N).

# Sample rates and sample sizes for all strata for all designs at all budgets
non_eval_2022_dt <- rbindlist(lapply(rates, function(x) {
  x1 <- copy(x)[ADP == 2022]
  x1[, RATE := round(SAMPLE_RATE * 100, 2)]
}), idcol = "DESIGN", fill = T)
non_eval_2022_dt[, c("Stratification", "Allocation") := tstrsplit(DESIGN, split  = "[.]")][, DESIGN := NULL]
non_eval_2022_dt[, Allocation := factor(Allocation, levels = allo_vec)]
non_eval_2022_dt[, Stratification := factor(Stratification, levels = strat_vec)]
non_eval_2022_dt <- dcast(non_eval_2022_dt, Stratification + STRATUM_COL + STRATA_N ~ Allocation + BUDGET, value.var = c("RATE", "n"))
setnames(non_eval_2022_dt, c("STRATUM_COL", "STRATA_N"), c("Stratum", "N"))
new_col_order <-sapply(
  sapply(c(3.5e6, 4.5e6, 5.25e6), function(x) paste0(allo_vec, "_", x)),
  function(x) paste0(c("RATE", "n"), "_", x))
setcolorder(non_eval_2022_dt, c("Stratification", "Stratum", "N", new_col_order))
non_eval_2022_flex <- non_eval_2022_dt %>% flextable() %>% 
  merge_v(j = 1) %>%
  set_header_labels(colnames(non_eval_2022_dt), values = c("Stratification", "Stratum", "N", rep(c("Rate", "n"), times = 12))) %>%
  add_header_row(values = c("", rep(c("EQUAL", "STATUS_QUO", "CWB", "PROX"), times = 3)), colwidths = c(3, rep(2, times = 12))) %>%
  add_header_row(values = c("", "$3.5M", "$4.5M", "$5.25M"), colwidths = c(3, rep(8, times = 3))) %>%
  vline(part = "header") %>% align(part = "header", align = "center") %>%
  vline(j = seq(1, 27, by = 2)) %>%
  vline(j = c(11, 19)) %>%
  hline(j = 1) %>%
  hline(j = c(2:27), i = c(6,17)) %>%
  fontsize(part = "all", size = 8) %>%
  padding(part = "all", padding.top = 2, padding.bottom = 2, padding.left = 4, padding.right = 4) %>% 
  fix_border_issues() %>% autofit()

# split this by BUDGET into three separate tables!
main_cols <- c("Stratification", "Stratum", "N")
non_eval_35M_cols <- c(main_cols, colnames(non_eval_2022_dt)[colnames(non_eval_2022_dt) %like% "3500000"])
non_eval_45M_cols <- c(main_cols, colnames(non_eval_2022_dt)[colnames(non_eval_2022_dt) %like% "4500000"])
non_eval_525M_cols <- c(main_cols, colnames(non_eval_2022_dt)[colnames(non_eval_2022_dt) %like% "5250000"])

non_eval_rates_3.5M_flex <- non_eval_2022_dt[, ..non_eval_35M_cols] %>% flextable() %>% 
  merge_v(j = 1) %>%
  set_header_labels(colnames(non_eval_2022_dt), values = c("Stratification", "Stratum", "N", rep(c("Rate", "n"), times = 4))) %>%
  add_header_row(values = c("", allo_vec), colwidths = c(3, rep(2, times = 4))) %>%
  add_header_row(values = c("Budget: $3.5M", "Allocation scheme"), colwidths = c(3, 8)) %>%
  vline(part = "header") %>% align(part = "header", align = "center") %>%
  vline(j = seq(1, 11, by = 2)) %>%
  hline(j = 1) %>%
  hline(j = c(2:11), i = c(6,17)) %>%
  fontsize(part = "all", size = 8) %>%
  padding(part = "all", padding.top = 2, padding.bottom = 2, padding.left = 4, padding.right = 4) %>% 
  fix_border_issues() %>% autofit()

non_eval_rates_4.5M_flex <- non_eval_2022_dt[, ..non_eval_45M_cols] %>% flextable() %>% 
  merge_v(j = 1) %>%
  set_header_labels(colnames(non_eval_2022_dt), values = c("Stratification", "Stratum", "N", rep(c("Rate", "n"), times = 4))) %>%
  add_header_row(values = c("", allo_vec), colwidths = c(3, rep(2, times = 4))) %>%
  add_header_row(values = c("Budget: $4.5M", "Allocation scheme"), colwidths = c(3, 8)) %>%
  vline(part = "header") %>% align(part = "header", align = "center") %>%
  vline(j = seq(1, 11, by = 2)) %>%
  hline(j = 1) %>%
  hline(j = c(2:11), i = c(6,17)) %>%
  fontsize(part = "all", size = 8) %>%
  padding(part = "all", padding.top = 2, padding.bottom = 2, padding.left = 4, padding.right = 4) %>% 
  fix_border_issues() %>% autofit()

non_eval_rates_5.25M_flex <- non_eval_2022_dt[, ..non_eval_525M_cols] %>% flextable() %>% 
  merge_v(j = 1) %>%
  set_header_labels(colnames(non_eval_2022_dt), values = c("Stratification", "Stratum", "N", rep(c("Rate", "n"), times = 4))) %>%
  add_header_row(values = c("", allo_vec), colwidths = c(3, rep(2, times = 4))) %>%
  add_header_row(values = c("Budget: $5.25M", "Allocation scheme"), colwidths = c(3, 8)) %>%
  vline(part = "header") %>% align(part = "header", align = "center") %>%
  vline(j = seq(1, 11, by = 2)) %>%
  hline(j = 1) %>%
  hline(j = c(2:11), i = c(6,17)) %>%
  fontsize(part = "all", size = 8) %>%
  padding(part = "all", padding.top = 2, padding.bottom = 2, padding.left = 4, padding.right = 4) %>% 
  fix_border_issues() %>% autofit()

# Tables 5-1, 5-2, and 5-3
save_as_docx(non_eval_rates_3.5M_flex, path = "output_tables/table_rates_low_.docx")
save_as_docx(non_eval_rates_4.5M_flex, path = "output_tables/table_rates_med_.docx")
save_as_docx(non_eval_rates_5.25M_flex, path = "output_tables/table_rates_high_.docx")

#========================================#
# Trips by Monitoring Method and Gear ---- 
#========================================#

# Method and Gear-specific data collection
# for each stratification's effort object, get counts of each trip, splitting those with multiple gear types!

trips_by_gear <- lapply(list(pc_effort.CURRENT, pc_effort.FMP, pc_effort.FIXED_FMP), function(x0) {
  x1 <- unique(copy(x0)[!(STRATUM_COL %like% "ZERO")][, .(ADP, TRIP_ID, GEAR, STRATUM_COL)])
  x1[, GEAR_w := 1/uniqueN(GEAR), keyby =.(TRIP_ID)
  ][, .(GEAR_N = sum(GEAR_w)), keyby = .(ADP, STRATUM_COL, GEAR)]
})
names(trips_by_gear) <- strat_vec

trips_by_gear_sampled_lst <- mapply(
  function(x1, x2) {
    # x1 <- rates[[5]]; x2 <- names(rates)[[5])
    trip_gear_counts <- trips_by_gear[[which(names(trips_by_gear) == gsub("[.].*$", "", x2))]]
    x3 <- trip_gear_counts[x1[, .(FILL, BUDGET, ADP, STRATUM_COL, RATE)], on = .(ADP, STRATUM_COL), allow.cartesian = T]
    x3[, GEAR_n := GEAR_N * RATE]
    x3[, .(GEAR_n = sum(GEAR_n)), keyby = .(ADP, BUDGET, FILL, GEAR)]
  }, 
  x1 = rates, x2 = names(rates), SIMPLIFY = F
)
trips_by_gear_sampled_dt <- rbindlist(trips_by_gear_sampled_lst, idcol = "DESIGN")

trips_by_gear_sampled_dt[, c("Stratification", "Allocation") := tstrsplit(DESIGN, split  = "[.]")][, DESIGN := NULL]
trips_by_gear_sampled_dt[, Allocation := factor(Allocation, levels = allo_vec)]
trips_by_gear_sampled_dt[, Stratification := factor(Stratification, levels = strat_vec)]
trips_by_gear_sampled_dt[, BUDGET := paste0("$", BUDGET/1e6, "M")]
trips_by_gear_sampled_dt[, variable := "n"]

ggplot(trips_by_gear_sampled_dt[ADP == 2022], aes(x = Allocation, y = interaction(GEAR, FILL), label = round(GEAR_n))) + 
  geom_tile(fill = "white", color = "black") + geom_text() + facet_nested(variable + BUDGET ~ Stratification)

# Create table
trips_by_gear_sampled_dt_2022 <- trips_by_gear_sampled_dt[ADP == 2022]
trips_by_gear_sampled_dt_2022[, GEAR_n := formatC(GEAR_n, digits = 0, format = "f")]
trips_by_gear_sampled_dt_2022_wide <- dcast(trips_by_gear_sampled_dt_2022, Stratification + FILL + GEAR ~ BUDGET + Allocation , value.var = "GEAR_n")
setnames(trips_by_gear_sampled_dt_2022_wide, c("FILL", "GEAR"), c("Monitoring Method", "Gear"))
trips_by_gear_sampled_dt_2022_wide[, `Monitoring Method` := factor(`Monitoring Method`, levels = c("OB", "EM_FG", "EM_TRW"))]
setorderv(trips_by_gear_sampled_dt_2022_wide, c("Stratification", "Monitoring Method", "Gear"))
allo_vec <- c("EQUAL", "STATUS_QUO", "CWB", "PROX")
bud_vec <- c("$3.5M", "$4.5M", "$5.25M")
trips_by_gear_sampled_flex <- trips_by_gear_sampled_dt_2022_wide %>% 
  flextable() %>% merge_v(j = c("Stratification", "Monitoring Method")) %>%
  set_header_labels(
    colnames(trips_by_gear_sampled_dt_2022_wide),
    values = c("Stratitfication", "Monitoring Method", "Gear", rep(allo_vec, times = 3))) %>% 
  add_header_row(values = c("", bud_vec), colwidths = c(3, 4, 4, 4)) %>%
  vline(j = c(1,2,3, 7, 11)) %>% 
  hline(i = c(6,12)) %>% 
  hline(i = c(3,5, 9,11, 15,17), j = c(2,3)) %>% 
  align(j = c(4:15), align = "right", part = "body") %>%
  align(align = "center", part = "header") %>%
  fontsize(part = "all", size = 8) %>%
  padding(part = "all", padding.top = 2, padding.bottom = 2, padding.left = 4, padding.right = 4) %>%
  fix_border_issues() %>% autofit()

# Tables for rmarkdown document output [Table 5-5]
trips_by_gear_sampled_flex %>% save_as_docx(path = "output_tables/table_trips_by_gear.docx")


#==================#
# Cost Per Trip ----
#==================#

# split by stratification method first
design_monitoring_cost_rates <- c(
  lapply(rates[names(rates) %like% "CURRENT."], function(x) x[current.allo_lst$effort[STRATA != "ZERO", .(ADP, STRATA, TRP_DUR)], on = c("ADP", STRATUM_COL = "STRATA")]),
  lapply(rates[names(rates) %like% "^FMP."], function(x) x[fmp.allo_lst$effort[!(STRATA %like% "ZERO"), .(ADP, STRATA, TRP_DUR)], on = c("ADP", STRATUM_COL = "STRATA")]),
  lapply(rates[names(rates) %like% "FIXED_FMP."], function(x) x[fixed_fmp.allo_lst$effort[!(STRATA %like% "ZERO"), .(ADP, STRATA, TRP_DUR)], on = c("ADP", STRATUM_COL = "STRATA")])
)

design_monitoring_cost_rates_lst <- lapply(design_monitoring_cost_rates, function(x) {
  # x <- design_monitoring_cost_rates[[1]]
  
  x1 <- copy(x)
  x1[, MON_RATE := RATE]
  split_cols <- c("BUDGET", "ADP")
  x1_split <- split(x1, by =  split_cols)
  
  x1[, MON_GROUP := fcase(STRATA %like% "OB", "OB", STRATA %like% "EM_HAL|EM_POT|EM_FIXED", "EMFG", STRATA %like% "EM_TRW", "EMTRW")]
  mon_mtd_dt <- x1[, .(MON_MTD = weighted.mean(TRP_DUR, w = n)), by = c(split_cols, "MON_GROUP")]
  mon_mtd_dt <- dcast(mon_mtd_dt, BUDGET + ADP ~ MON_GROUP, value.var = "MON_MTD")
  
  x2 <- rbindlist(mapply(
    cbind,
    lapply(x1_split, ob_cost, cost_params),
    lapply(x1_split, emfg_cost, cost_params),
    lapply(x1_split, emtrw_cost, cost_params),
    SIMPLIFY = F
  ), idcol = "BUDGET.ADP")
  
  x2[, (split_cols) := tstrsplit(BUDGET.ADP, split = "[.]")][, BUDGET.ADP := NULL]
  x2[, (split_cols) := lapply(.SD, as.integer), .SDcols =  split_cols]
  x2 <- x2[mon_mtd_dt, on = split_cols]
  
  x2 <- x2[, .(
    # Percentage of budget allocated to each monitoring method
    OB_PERC = round(OB_TOTAL / BUDGET * 100, 1),
    OB_n = OB_DAYS / OB,
    OB_CPT = OB_CPD * OB,
    
    EMFG_PERC = round(EMFG_TOTAL/ BUDGET * 100, 1),
    EMFG_n = EMFG_DAYS / EMFG,
    EMFG_CPT = EMFG_CPD * EMFG,
    
    EMTRW_PERC = round(EMTRW_TOTAL / BUDGET * 100, 1),
    EMTRW_n = EMTRW_PLANT_n,
    EMTRW_CPT = EMTRW_CPT
    
  ), by = .(ADP, BUDGET)]
  x2
})

design_monitoring_cost_rates_dt <- rbindlist(design_monitoring_cost_rates_lst, idcol = "DESIGN")
design_monitoring_cost_rates_dt[, c("Stratification", "Allocation") := tstrsplit(DESIGN, split  = "[.]")
][, DESIGN := NULL
][, Allocation := factor(Allocation, levels = allo_vec)
][, Stratification := factor(Stratification, levels = strat_vec)
][, BUDGET := paste0("$", BUDGET/1e6, "M")]

# format values
perc_cols <- colnames(design_monitoring_cost_rates_dt)[colnames(design_monitoring_cost_rates_dt) %like% c("_PERC$")]
n_cols <- colnames(design_monitoring_cost_rates_dt)[colnames(design_monitoring_cost_rates_dt) %like% c("_n$")]
cpt_cols <- colnames(design_monitoring_cost_rates_dt)[colnames(design_monitoring_cost_rates_dt) %like% c("_CPT$")]

design_monitoring_cost_rates_dt[, (perc_cols) := lapply(.SD, formatC, digits = 1, format = "f" ), .SDcols = perc_cols]
design_monitoring_cost_rates_dt[, (n_cols) := lapply(.SD, formatC, digits = 0, format = "f" ), .SDcols = n_cols]
design_monitoring_cost_rates_dt[, (cpt_cols ) := lapply(.SD, formatC, digits = 0, format = "f", big.mark = "," ), .SDcols = cpt_cols ]

# TODO Put MONITORING METHOD in EM_FG and EM_TRW format (instead of EMFG, EMTRW, and OB), then also factor and set levels
design_monitoring_cost_rates_dt <- rbind(
  cbind("Monitoring Method" = "OB", melt(
    design_monitoring_cost_rates_dt[ADP == 2022], 
    id.vars = c("BUDGET", "Stratification", "Allocation"),
    measure.vars = c("OB_PERC", "OB_n", "OB_CPT"), variable.name = "Metric"
  )),
  cbind("Monitoring Method" = "EMFG", melt(
    design_monitoring_cost_rates_dt[ADP == 2022], 
    id.vars = c("BUDGET", "Stratification", "Allocation"),
    measure.vars = c("EMFG_PERC", "EMFG_n", "EMFG_CPT"), variable.name = "Metric"
  )),
  cbind("Monitoring Method" = "EMTRW", melt(
    design_monitoring_cost_rates_dt[ADP == 2022], 
    id.vars = c("BUDGET", "Stratification", "Allocation"),
    measure.vars = c("EMTRW_PERC", "EMTRW_n", "EMTRW_CPT"), variable.name = "Metric"
  ))
)

# Simplify metric names, removing Monitoring Method
design_monitoring_cost_rates_dt[, Metric := fcase(
  Metric %like% "PERC", "% of budget",
  Metric %like% "n", "n",
  Metric %like% "CPT", "Cost per trip"
)]

design_monitoring_cost_rates_dt_wide <- dcast(design_monitoring_cost_rates_dt, BUDGET +  Stratification + `Monitoring Method` ~ Allocation + Metric, value.var = "value")
design_monitoring_cost_rates_dt_wide[, `Monitoring Method` := factor(`Monitoring Method`, levels = c("OB", "EMFG", "EMTRW"))]
design_monitoring_cost_rates_flex <- design_monitoring_cost_rates_dt_wide %>% flextable() %>%
  add_header_row(values = c("", allo_vec), colwidths = c(3,3,3,3,3)) %>%
  add_header_row(values = c("", "Allocation scheme"), colwidths = c(3, 12)) %>%
  merge_v(j = c(1:2)) %>% hline(j = 1) %>% hline(i = seq(3, 27, by = 3), j = 2:15) %>% vline(j = seq(3, 15, by = 3)) %>% vline(part = "header", i = 1) %>%
  set_header_labels(colanmes(try_this), values = c("Budget", "Stratification", "Monitoring Method", rep(c("% of Budget", "Cost per trip", "n"), times = 4))) %>%
  align(part = "header", align = "center") %>% vline(j = 1:2) %>%
  align(part = "body", align = "right", j = 4:15) %>%
  fontsize(part = "all", size = 8) %>%
  padding(part = "all", padding.top = 2, padding.bottom = 2, padding.left = 4, padding.right = 4) %>% 
  border_outer() %>% fix_border_issues() %>% autofit()

# Table 5-4
design_monitoring_cost_rates_flex  
save_as_docx(design_monitoring_cost_rates_flex, path = "output_tables/table_monitoring_costs.docx")


#=========================#
# Variance in Expenses ----
#=========================#

# For each design, simulate sampling 1000 times and see realized rates affect the distribution of expenses.

# Subset only to 2022 to make things go faster
design_monitoring_cost_rates_2022 <- lapply(design_monitoring_cost_rates, function(x) x[ADP == 2022])

current.trips_days <- unique(pc_effort.CURRENT[!(STRATA %like% "ZERO"), .(ADP, STRATUM_COL, TRIP_ID, DAYS)])
fmp.trips_days <- unique(pc_effort.FMP[!(STRATA %like% "ZERO"), .(ADP, STRATUM_COL, TRIP_ID, DAYS)])
fixed_fmp.trips_days <- unique(pc_effort.FIXED_FMP[!(STRATA %like% "ZERO"), .(ADP, STRATUM_COL, TRIP_ID, DAYS)])

set.seed(12345)        # Warning - this simulation takes ~20 minutes or so to run! Can quick-load the results.
expense_variances_lst <- mapply(
  function(x, design_names) {
    # x <- design_monitoring_cost_rates[[1]]; design_names <- names(design_monitoring_cost_rates_2022)[[1]]
    
    x1 <- copy(x)
    x1[, MON_RATE := RATE]
    split_cols <- c("BUDGET", "ADP")
    
    x1[, MON_GROUP := fcase(STRATA %like% "OB", "OB", STRATA %like% "EM_HAL|EM_POT|EM_FIXED", "EMFG", STRATA %like% "EM_TRW", "EMTRW")]
    mon_mtd_dt <- x1[, .(MON_MTD = weighted.mean(TRP_DUR, w = n)), by = c(split_cols, "MON_GROUP")]
    mon_mtd_dt <- dcast(mon_mtd_dt, BUDGET + ADP ~ MON_GROUP, value.var = "MON_MTD")
    
    iter <- 10000
    iter_lst <- vector(mode = "list", length = iter)
    
    # First, grab the correct trips_days object
    if(design_names %like% "CURRENT.") use_trip_days <- copy(current.trips_days) else {
      if(design_names %like% "^FMP.") use_trip_days <- copy(fmp.trips_days) else {
        if(design_names %like% "^FIXED_FMP.") use_trip_days <- copy(fixed_fmp.trips_days)
      }
    }
    use_trip_days <- use_trip_days[ADP %in% unique(x$ADP)]
    # Merge in sample rates
    
    x1_merge <- x1[, .(BUDGET, ADP, STRATUM_COL, STRATA_N, TRP_DUR, MON_RATE)][use_trip_days, on = .(ADP, STRATUM_COL), allow.cartesian = T]
    
    cat(paste0("Simulating sampling ", iter, " times: "))
    
    for(i in seq_len(iter)) {
      
      if(i %% 100 == 0) cat(paste0(i, ", "))
      
      # Simulate sampling trips! Count trips and days
      
      x2 <- copy(x1_merge)
      x2[, RN := runif(.N)]
      
      x2_split <- split(
        x2[, .(
          n = sum(RN <= MON_RATE),
          d = sum(DAYS[RN <= MON_RATE])
        ), keyby = .(BUDGET, ADP, STRATA = STRATUM_COL, STRATA_N, TRP_DUR)], 
        by = split_cols)
      
      x3 <- rbindlist(mapply(
        cbind,
        lapply(x2_split, ob_cost, cost_params, sim = T),                    # Calculate realized costs of each monitoring method
        lapply(x2_split, emfg_cost, cost_params, sim = T),
        lapply(x2_split, emtrw_cost, cost_params, sim = T),
        SIMPLIFY = F
      ), idcol = "BUDGET.ADP")
      x3[, TOTAL := OB_TOTAL + EMFG_TOTAL + EMTRW_TOTAL]                    # Calculate realized total
      iter_lst[[i]] <- x3
    }
    
    iter_res <- rbindlist(iter_lst, idcol = "ITER")
    cat("\n")
    iter_res
  }, 
  x = design_monitoring_cost_rates_2022, 
  design_names = names(design_monitoring_cost_rates_2022),
  SIMPLIFY = F
)

expense_2022 <- rbindlist(expense_variances_lst, idcol = "DESIGN", fill = T)
expense_2022[, c("BUDGET", "ADP") := tstrsplit(BUDGET.ADP, split = "[.]")][, BUDGET.ADP := NULL]
expense_2022_cv <- expense_variances_2022[, .(EXP_SD = sd(TOTAL), EXP_MAX = max(TOTAL), EXP_MIN = min(TOTAL), EXP_MEAN = mean(TOTAL)), by = .(ADP, BUDGET, DESIGN)]
expense_2022_cv[, c("Stratification", "Allocation") := tstrsplit(DESIGN, split  = "[.]")][, DESIGN := NULL]
expense_2022_cv[, Allocation := factor(Allocation, levels = c("EQUAL", "STATUS_QUO", "CWB", "PROX"))]
expense_2022_cv[, Stratification := factor(Stratification, levels = c("CURRENT", "FMP", "FIXED_FMP"))]
expense_2022_cv[, BUDGET := paste0("$", as.integer(BUDGET)/1e6, "M")]
expense_2022_cv[, BASELINE := rep(EXP_SD[Stratification == "CURRENT" & Allocation == "EQUAL"], times = .N), by = .(ADP, BUDGET)]
expense_2022_cv[, DIFF := -1 *(EXP_SD - BASELINE) / BASELINE]  #Lower variation is better!
expense_2022_cv[, EXP_CV := EXP_SD / EXP_MEAN * 100]
expense_2022_cv[, ALLO_LBL := factor(gsub("[_]", "\n", Allocation), levels = c("EQUAL", "STATUS\nQUO", "CWB", "PROX"))]

expenses_cv_2022_plot <- ggplot(expense_2022_cv, aes(x = ALLO_LBL, y = EXP_CV)) + geom_col() + 
  facet_nested(
    ADP + BUDGET ~ Stratification,
    labeller = labeller(BUDGET = function(x) paste0("Budget: ", x), Stratification = function(x) paste0("Stratification: ", x))) + 
  labs(x = "Allocation method", y = "CV of expenses") 

# Figure 5-1
ggsave(expenses_cv_2022_plot, file = "analyses/evaluation_interspersion/figure_eval_expenses_cv.png", width = 8, height = 6, units = "in", dpi = 300)

# For Scorecard!
scorecard_expense_cv <- expense_2022_cv[, .(BUDGET, Stratification, Allocation, variable = "CV", value = EXP_CV, BASELINE, DIFF)]
save(scorecard_expense_cv, file = "analyses/evaluation_trips_and_costs/scorecard_expense_cv.rdata")
# Saved to google drive: https://drive.google.com/file/d/1RM16ONVHDtWfaIDMnX5znRa4mWxXDr5x/view?usp=drive_link
