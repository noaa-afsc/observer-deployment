# 2024 Draft ADP : Evaluation - [Interspersion]

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

# Load the ADFG statistical area shapefile. '../' is needed for Rmarkdown to climb into parent folders.
stat_area_sf <- st_read(
  dsn = "source_data/ADFG_Stat_Area_shapefile/PVG_Statewide_2001_Present_GCS_WGS1984.shp", quiet = T) %>%
  select(STAT_AREA) %>%
  st_transform(crs = 3467)

# Load the Alaska map sf objects
load("source_data/ak_shp.rdata")      # shp_land, shp_nmfs, and shp_centroids added to global

#===============#
## Constants ----
#===============#

allo_vec <- c("EQUAL", "STATUS_QUO", "CWB", "PROX")
strat_vec <- c("CURRENT", "FMP", "FIXED_FMP")

#===============#
## Functions ----
#===============#

source("analyses/allocation_evaluation/functions.R")

#======================================================================================================================#
# Interspersion --------------------------------------------------------------------------------------------------------
#======================================================================================================================#

# Make a vector of all designs and the stratifications they use
design_stratification <- sapply(names(rates), function(x) unlist(strsplit(x, split = "[.]"))[1] )
# The value is the stratum, the name is the design

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

#=============#
## Current ----
#=============#

# First, set up data donor and acceptor lists lists
# current.box$dmn$strata_dt
current.OB.acceptor_donor_lst <- c(
  rep(list(4:5), times = 2),                # 1-2: EM_HAL and EM_POT 
  list(3),                                  # 3: EM_TRW
  rep(list(4:5), times = 2),                # 4-5: OB Fixed Gear
  list(6),                                  # 6: OB Trawl                         
  list(4:5)                                 # 7: ZERO           
)
current.nonOB.acceptor_donor_lst <- c(
  rep(list(1:2), times = 2),                # 1-2: Fixed-gear EM to itself
  rep(list(NULL), times = 5)                # 4-7: No other donors
)

# Compile all designs with "CURRENT" stratification

current_designs <- names(design_stratification[design_stratification == "CURRENT"])
current_figs <- vector(mode = "list", length = length(current_designs))

for(i in 1:length(current_designs )) {
  
  rates.design <- rates[[ current_designs[[i]] ]]
  rates.design.budget_lst <- split(rates.design, by = "BUDGET")
  names(rates.design.budget_lst) <- paste0("BUD_", unlist(budget_lst)/1e6, "M")
  
  current_figs[[i]] <- lapply(
    rates.design.budget_lst,
    function(x) {
      dmn_interspersion_figs(
        box_def = current.box,
        selection_rates = x,
        ob_adl = current.OB.acceptor_donor_lst,
        nonob_adl = current.nonOB.acceptor_donor_lst
      )
    }
  )
}
names(current_figs) <- current_designs

#=========#
## FMP ----
#=========#

# fmp.box$dmn$strata_dt
fmp.OB.acceptor_donor_lst <- c(
  rep(list(6:9), times = 4),                # 1-4:   EM_HAL and EM_POT 
  list(5),                                  # 5:     EM_TRW
  rep(list(6:9), times = 4),                # 6-9:   OB Fixed Gear
  rep(list(10:11), times = 2),              # 10-11: OB Trawl                         
  rep(list(6:9), times = 2)                 # 12-13: ZERO           
)
fmp.nonOB.acceptor_donor_lst <- c(
  rep(list(1:4), times = 4),               # 1-4: Fixed-gear EM to itself
  rep(list(NULL), times = 9)               # 5-13: No other donors
)

# Compile all designs with "FMP" stratification

fmp_designs <- names(design_stratification[design_stratification == "FMP"])
fmp_figs <- vector(mode = "list", length = length(fmp_designs))

for(i in 1:length(fmp_designs)) {
  
  rates.design <- rates[[ fmp_designs[[i]] ]]
  rates.design.budget_lst <- split(rates.design, by = "BUDGET")
  names(rates.design.budget_lst) <- paste0("BUD_", unlist(budget_lst)/1e6, "M")
  
  fmp_figs[[i]] <- lapply(
    rates.design.budget_lst,
    function(x) {
      dmn_interspersion_figs(
        box_def = fmp.box,
        selection_rates = x,
        ob_adl = fmp.OB.acceptor_donor_lst,
        nonob_adl = fmp.nonOB.acceptor_donor_lst
      )
    }
  )
}
names(fmp_figs) <- fmp_designs

#===============#
## FIXED-FMP ----
#===============#

# fixed_fmp.box$dmn$strata_dt
fixed_fmp.OB.acceptor_donor_lst <- c(
  rep(list(4:5), times = 2),                # 1-2:   EM Fixed Gear
  list(3),                                  # 3:     EM Trawl
  rep(list(4:5), times = 2),                # 4-5:   OB Fixed Gear
  rep(list(6:7), times = 2),                # 6-7:   OB Trawl                         
  rep(list(4:5), times = 2)                 # 8-9:   ZERO           
)

fixed_fmp.nonOB.acceptor_donor_lst <- c(
  rep(list(1:2), times = 2),               # 1-2: Fixed-gear EM to itself
  rep(list(NULL), times = 7)               # 3-9: No other donors
)

# Compile all designs with "FIXED_FMP" stratification

fixed_fmp_designs <- names(design_stratification[design_stratification == "FIXED_FMP"])
fixed_fmp_figs <- vector(mode = "list", length = length(fixed_fmp_designs))

for(i in 1:length(fixed_fmp_designs)) {
  
  rates.design <- rates[[ fixed_fmp_designs[[i]] ]]
  rates.design.budget_lst <- split(rates.design, by = "BUDGET")
  names(rates.design.budget_lst) <- paste0("BUD_", unlist(budget_lst)/1e6, "M")
  
  fixed_fmp_figs[[i]] <- lapply(
    rates.design.budget_lst,
    function(x) {
      dmn_interspersion_figs(
        box_def = fixed_fmp.box,
        selection_rates = x,
        ob_adl = fixed_fmp.OB.acceptor_donor_lst,
        nonob_adl = fixed_fmp.nonOB.acceptor_donor_lst
      )
    }
  )
  
}
names(fixed_fmp_figs) <- fixed_fmp_designs

#=====================#
## Combine Figures ----
#=====================#

# Set identical widths once we get all designs done! First, split all designs by budget!
# unlist so we have a single list with all budgets and designs

all_figs <- c(
  unlist(current_figs, recursive = F),
  unlist(fmp_figs, recursive = F),
  unlist(fixed_fmp_figs, recursive = F)
)

# If everything worked the way it should status quo designs should all be identical!
fsetequal(
  all_figs$CURRENT.STATUS_QUO.BUD_4.5M$DMN_INSP_OB_SMRY$OVERALL[ADP == 2022][order(POOL, GEAR)] ,
  all_figs$FMP.STATUS_QUO.BUD_4.5M$DMN_INSP_OB_SMRY$OVERALL[ADP == 2022][order(POOL, GEAR)]   # FMP and FIXED_FMP match!!
)
fsetequal(
  all_figs$FMP.STATUS_QUO.BUD_4.5M$DMN_INSP_OB_SMRY$OVERALL[ADP == 2022][order(POOL, GEAR)] ,
  all_figs$FIXED_FMP.STATUS_QUO.BUD_4.5M$DMN_INSP_OB_SMRY$OVERALL[ADP == 2022][order(POOL, GEAR)]   # FMP and FIXED_FMP match!!
)

#  Does the interspersion allocated from PROX match the evaluation (for TRW stratum)
all_figs$CURRENT.PROX.BUD_4.5M$DMN_INSP_OB_SMRY$OVERALL[GEAR == "TRW" & POOL == "OB"][, .(ADP, BOX_DMN_w, ISPN = POOL_DMN_INTERSPERSION)]
current.prox[STRATA == "OB_TRW" & BUDGET == 4.5e6][, .(ADP, STRATA_N, ISPN)]  # Perfect! 

all_figs$CURRENT.PROX.BUD_4.5M$DMN_INSP_OB_SMRY$OVERALL[GEAR == "TRW" & POOL == "EM"][, .(ADP, BOX_DMN_w, ISPN = POOL_DMN_INTERSPERSION)]
current.prox[STRATA == "EM_TRW" & BUDGET == 4.5e6][, .(ADP, STRATA_N, ISPN)]  # Also matches!

# Make all figures have the same width!

# Subset all "overall" figures
figs_overall <- lapply(all_figs, "[[", "DMN_PLOT_OVERALL")
figs_overall_grobs <- lapply(figs_overall, ggplotGrob)
figs_overall_widths_lst <- lapply(figs_overall_grobs, function(x) x[["widths"]])
figs_overall_width <- do.call(unit.pmax, figs_overall_widths_lst)
for(i in 1:length(figs_overall_grobs)) figs_overall_grobs[[i]]$widths <- figs_overall_width

figs_fmp <- lapply(all_figs, "[[", "DMN_PLOT_FMP")
figs_fmp_grobs <- lapply(figs_fmp, ggplotGrob)
figs_fmp_widths_lst <- lapply(figs_fmp_grobs, function(x) x[["widths"]])
figs_fmp_width <- do.call(unit.pmax, figs_fmp_widths_lst)
for(i in 1:length(figs_fmp_grobs)) figs_fmp_grobs[[i]]$widths <- figs_fmp_width

# Now combine the figures!
figs_combined_grobs <- mapply(
  FUN = function(overall, fmp) arrangeGrob(overall, fmp, ncol = 1),
  overall = figs_overall_grobs,
  fmp = figs_fmp_grobs
)

# Figures for 2018-2022, separately for each stratification
grid.arrange(figs_combined_grobs$CURRENT.STATUS_QUO.BUD_4.5M)
grid.arrange(figs_combined_grobs$FIXED_FMP.CWB.BUD_4.5M)
grid.arrange(figs_combined_grobs$FIXED_FMP.PROX.BUD_4.5M)

# Make 2022 only figure (y-facet of design), make 2018-2021 figures for appendix
# Adding annotation of actual values or table
interspersion_summary <- function(x, year) {
  # x <- copy(all_figs);  year <- 2022
  
  # for each Design...
  
  mapply(function(y, design_name) {
    # y <- x[[1]]; design_name <- names(x)[[1]]
    
    
    # Method-level summaries
    
    design_budget <- as.data.table(design_name)
    design_budget[
    ][, c("Stratification", "Allocation", "Budget1", "Budget2") := tstrsplit(design_name, split = "[.]")
    ][, Budget := paste0(Budget1, ".", Budget2)
    ][, c("design_name", "Budget1", "Budget2") := NULL]
    
    # Overall
    x1_ob_overall <- copy(y$DMN_INSP_OB_SMRY$OVERALL)[ADP %in% year]
    x1_nonob_overall <- copy(y$DMN_INSP_NONOB_SMRY$OVERALL)[ADP %in% year]
    
    # FMP
    x1_ob_fmp <- copy(y$DMN_INSP_OB_SMRY$BSAI_GOA)[ADP %in% year]
    x1_nonob_fmp <- copy(y$DMN_INSP_NONOB_SMRY$BSAI_GOA)[ADP %in% year]
    
    # Overall summarized over Method
    x1_ob_overall[, METHOD := ifelse(POOL %like% "OB|ZERO", POOL, fcase(GEAR %in% c("HAL", "POT"), "EM_FIXED", GEAR == "TRW", "EM_TRW"))][, METHOD := paste0("OB to ", METHOD)]
    x1_nonob_overall[, METHOD := ifelse(POOL %like% "OB|ZERO", POOL, fcase(GEAR %in% c("HAL", "POT"), "EM_FIXED", GEAR == "TRW", "EM_TRW"))][, METHOD := paste0("EM_FIXED to ", METHOD)]
    
    out_overall <- cbind(
      design_budget,
      rbind(
        x1_ob_overall[, .(METHOD_ISPN = weighted.mean(POOL_DMN_INTERSPERSION, w = BOX_DMN_w)) , keyby = .(ADP, METHOD)],
        x1_nonob_overall[, .(METHOD_ISPN = weighted.mean(POOL_DMN_INTERSPERSION, w = BOX_DMN_w)) , keyby = .(ADP, METHOD)]
      )
    )
    
    # FMP summarized over Method
    x1_ob_fmp[, METHOD := ifelse(POOL %like% "OB|ZERO", POOL, fcase(GEAR %in% c("HAL", "POT"), "EM_FIXED", GEAR == "TRW", "EM_TRW"))][, METHOD := paste0("OB to ", METHOD)]
    x1_nonob_fmp[, METHOD := ifelse(POOL %like% "OB|ZERO", POOL, fcase(GEAR %in% c("HAL", "POT"), "EM_FIXED", GEAR == "TRW", "EM_TRW"))][, METHOD := paste0("EM_FIXED to ", METHOD)]
    out_fmp <- cbind(
      design_budget,
      rbind(
        x1_ob_fmp[, .(METHOD_ISPN = weighted.mean(POOL_DMN_INTERSPERSION, w = BOX_DMN_w)) , keyby = .(ADP, METHOD, BSAI_GOA)],
        x1_nonob_fmp[, .(METHOD_ISPN = weighted.mean(POOL_DMN_INTERSPERSION, w = BOX_DMN_w)) , keyby = .(ADP, METHOD, BSAI_GOA)]
      )
    )
    
    list(OVERALL = out_overall, FMP = out_fmp)

  },
  y = x,
  design_name = names(x),
  SIMPLIFY = F)
  
}

all_ispn_summaries <- interspersion_summary(all_figs, year = 2022)
# Make some data.table versions
all_ispn_overall_dt <- rbindlist(lapply(all_ispn_summaries, "[[", "OVERALL"))
all_ispn_fmp_dt <- rbindlist(lapply(all_ispn_summaries, "[[", "FMP"))

# Next, subset by budgets. 
budget_names <- sapply(budget_lst, function(x) paste0("BUD_", (x/1e6), "M"))
all_ispn_summaries <- setNames(lapply(budget_names, function(x) all_ispn_summaries[names(all_ispn_summaries) %like% x]), budget_names)

all_ispn_overall_lst <- lapply(all_ispn_summaries, function(x) lapply(x, "[[", "OVERALL"))
all_ispn_overall_lst <- lapply(all_ispn_overall_lst, function(x) {
  do.call(cbind, unname(
    mapply(
      function(x1, x2) setNames(x1[, .(METHOD_ISPN)], x2), 
      x1 = x, x2 = names(x), SIMPLIFY = F
    )
  ))
})

all_ispn_fmp_lst <- lapply(all_ispn_summaries, function(x) lapply(x, "[[", "OVERALL"))
all_ispn_fmp_lst <- lapply(all_ispn_fmp_lst, function(x) {
  do.call(cbind, unname(
    mapply(
      function(x1, x2) setNames(x1[, .(METHOD_ISPN)], x2), 
      x1 = x, x2 = names(x), SIMPLIFY = F
    )
  ))
})

# Compare to baseline of CURRENT.EQUAL within each budget and ADP year
all_ispn_overall_plot_dt <- all_ispn_overall_dt[
][, BASELINE := rep(METHOD_ISPN[Stratification == "CURRENT" & Allocation == "EQUAL"], times = .N), by = .(ADP, Budget, METHOD)
][, DIFF := METHOD_ISPN - BASELINE
][, Allocation := factor(Allocation, levels = c("EQUAL","STATUS_QUO", "CWB", "PROX"))
][, Stratification := factor(Stratification, levels = c("CURRENT", "FMP", "FIXED_FMP"))
][, BUDGET := gsub("BUD_", "$", Budget)][]

all_ispn_overall_plot_dt[METHOD == "OB to EM_TRW", METHOD := "EM_TRW to EM_TRW"]
ispn_method_levels <- c("EM_FIXED to EM_FIXED", "EM_TRW to EM_TRW", "OB to ZERO", "OB to EM_FIXED", "OB to OB")
all_ispn_overall_plot_dt[, METHOD := factor(METHOD, levels = ispn_method_levels)]

# AK-wide Interspersion
ggplot(all_ispn_overall_plot_dt[!(Allocation %in% c("EQUAL", "STATUS_QUO") & Stratification %in% c("FMP", "FIXED_FMP"))], aes(x = Allocation, y = METHOD, fill = DIFF)) + 
  facet_nested(ADP ~ Budget + Stratification, scales = "free_x", space = "free_x") + geom_tile() + 
  scale_fill_gradient2() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "bottom", legend.key.width = unit(2, "in")) + 
  geom_text(aes(label = formatC(METHOD_ISPN, digits = 3, format = "f")), size = 3) + 
  labs(fill = "Relative benefit", y = "Comparison")
# Can see that at low budget levels, differences between designs are much more noticeable. At high budget levels
# Note that within a budget, the EQUAL and STATUS_QUO allocation methods are identical across stratification schemes

ispn_method_levels_fmp <- c(rbind(sapply(c("EM_FIXED to EM_FIXED", "EM_TRW to EM_TRW", "OB to ZERO", "OB to EM_FIXED", "OB to OB"), function(x) paste0(x, "\n", c("BSAI", "GOA")))))

all_ispn_fmp_plot_dt <- copy(all_ispn_fmp_dt)[
][, BASELINE := rep(unique(METHOD_ISPN[Stratification == "CURRENT" & Allocation == "EQUAL"]), times = .N), by = .(ADP, Budget, METHOD, BSAI_GOA)
][, DIFF := METHOD_ISPN - BASELINE
][, Allocation := factor(Allocation, levels = c("EQUAL", "STATUS_QUO", "CWB", "PROX"))
][, Stratification := factor(Stratification, levels = c("CURRENT", "FMP", "FIXED_FMP"))
][, Method := paste0(METHOD, " x ", BSAI_GOA)
][, BUDGET := gsub("BUD_", "$", Budget)
][METHOD %like% "OB to EM_TRW", METHOD := "EM_TRW to EM_TRW"
][, METHOD := factor(paste0(METHOD, "\n", BSAI_GOA ), levels = ispn_method_levels_fmp)][]

# FMP-specific Interspersion
ggplot(all_ispn_fmp_plot_dt[!(Allocation %in% c("EQUAL", "STATUS_QUO") & Stratification %in% c("FMP", "FIXED_FMP"))], aes(x = Allocation, y = METHOD, fill = DIFF)) + 
  facet_nested(ADP ~ Budget + Stratification, scales = "free_x", space = "free_x") + geom_tile() + 
  scale_fill_gradient2() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "bottom", legend.key.width = unit(2, "in")) + 
  geom_text(aes(label = formatC(METHOD_ISPN, digits = 3, format = "f")), size = 3) + 
  labs(x = "Allocation Method", y = "Comparison x FMP", fill = "Difference in interspersion relative to `Equal Rates`")
# When considering FMP, both CWB and PROX generally allocate more sample to the BSAI under the FMP and FIXED_FMP stratification schemes.
# PROX tends to sample BSAI more heavily than CWB due to it's goal of protecting from low sample sizes in strata with fewer trips/offloads.

all_ispn_summaries_old <- interspersion_summary(all_figs, year = 2018:2021)
# Make some data.table versions
all_ispn_overall_dt_old <- rbindlist(lapply(all_ispn_summaries_old, "[[", "OVERALL"))
all_ispn_fmp_dt_old <- rbindlist(lapply(all_ispn_summaries_old, "[[", "FMP"))

# Compare to baseline of CURRENT.EQUAL within each budget and ADP year
all_ispn_overall_plot_dt_old <- all_ispn_overall_dt_old[
][!(Allocation %in% c("EQUAL", "STATUS_QUO") & Stratification %in% c("FMP", "FIXED_FMP"))
][, BASELINE := rep(METHOD_ISPN[Stratification == "CURRENT" & Allocation == "EQUAL"], times = .N), by = .(ADP, Budget, METHOD)
][, DIFF := (METHOD_ISPN - BASELINE) / BASELINE
][, Allocation := factor(Allocation, levels = c("EQUAL","STATUS_QUO", "CWB", "PROX"))
][, Stratification := factor(Stratification, levels = c("CURRENT", "FMP", "FIXED_FMP"))]

# 2018-2021 AK-specific Interspersion for all budgets
ggplot(all_ispn_overall_plot_dt_old, aes(x = Allocation, y = METHOD, fill = DIFF)) + 
  facet_nested(ADP ~ Budget + Stratification, scales = "free_x", space = "free_x") + geom_tile() + 
  scale_fill_gradient2() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  geom_text(aes(label = formatC(METHOD_ISPN, digits = 3, format = "f")), size = 2)

#=========================================#
## Interspersion Outputs for Scorecard ----
#=========================================#

interspersion_ak <- all_ispn_overall_plot_dt[ADP == 2022, .(BUDGET, Stratification, Allocation, variable = METHOD, value = METHOD_ISPN, BASELINE, DIFF)]
interspersion_fmp <- all_ispn_fmp_plot_dt[ADP == 2022, .(BUDGET, Stratification, Allocation, variable = METHOD, value = METHOD_ISPN, BASELINE, DIFF)]

save(interspersion_ak, interspersion_fmp, file = "results/scorecard_interspersion.rdata")
# Saved to google drive: https://drive.google.com/file/d/15ImmB1LDkfeZCtfKy94IhSvk5OF9YQqC/view?usp=drive_link


#======================================================#
# Gear-Specific Interspersion for 2022 (Appendix B) ----
#======================================================#

x1 <- all_figs$CURRENT.CWB.BUD_3.5M$DMN_INSP_OB_SMRY$OVERALL[ADP == 2022]
x2 <- all_figs$CURRENT.CWB.BUD_3.5M$DMN_INSP_NONOB_SMRY$OVERALL[ADP == 2022]

dmn_plot_overall <- ggplot(x1, aes(y = GEAR, x = POOL_DMN_INTERSPERSION)) + 
  facet_grid(ADP ~ POOL) + geom_col(aes(fill = I(FILL))) + 
  geom_point(data = x2, shape = 23, fill = "yellow", stroke = 1) + 
  geom_text(aes(label = round(BOX_DMN_w)), x = 0.15, hjust = 1, size = 3, color = "white")

x3 <- all_figs$CURRENT.CWB.BUD_3.5M$DMN_INSP_OB_SMRY$BSAI_GOA[ADP == 2022]
x4 <- all_figs$CURRENT.CWB.BUD_3.5M$DMN_INSP_NONOB_SMRY$BSAI_GOA[ADP == 2022]

dmn_plot_fmp <- ggplot(x3, aes(y = GEAR, x = POOL_DMN_INTERSPERSION)) +
  facet_grid(ADP ~ POOL + BSAI_GOA) + geom_col(aes(fill = I(FILL))) + 
  geom_point(data = x4, shape = 23, fill = "yellow", stroke = 1) + 
  geom_text(aes(label = round(BOX_DMN_w)), x = 0.3, hjust = 1, size = 3, color = "white")

# Make separate figures for OVERALL and FMP

interspersion_2022_lst <- lapply(all_figs, function(x) {
  # x <- copy(all_figs$CURRENT.EQUAL.BUD_3.5M)
  list(
    OVERALL = list(
      OB = x$DMN_INSP_OB_SMRY$OVERALL[ADP == 2022],
      NONOB = x$DMN_INSP_NONOB_SMRY$OVERALL[ADP == 2022]
    ),
    FMP = list(
      OB = x$DMN_INSP_OB_SMRY$BSAI_GOA[ADP == 2022],
      NONOB = x$DMN_INSP_NONOB_SMRY$BSAI_GOA[ADP == 2022]
    )
  )
})

interspersion_2022_lst_AK <- lapply(interspersion_2022_lst, "[[", "OVERALL")
interspersion_2022_lst_AK_OB <- rbindlist(lapply(interspersion_2022_lst_AK, "[[", "OB"), idcol = "DESIGN.BUD")
interspersion_2022_lst_AK_NONOB <- rbindlist(lapply(interspersion_2022_lst_AK, "[[", "NONOB"), idcol = "DESIGN.BUD")
interspersion_2022_lst_AK_OB[, ':=' (
  DESIGN = gsub("[.]BUD.*$", "", DESIGN.BUD),
  BUD = gsub("^.*[_]", "", DESIGN.BUD)
)][, ':=' (
  STRATA = factor(gsub("[.].*$", "", DESIGN), levels = strat_vec),
  ALLO = factor(gsub("^.*[.]", "", DESIGN), levels = allo_vec)
)]
interspersion_2022_lst_AK_NONOB[, ':=' (
  DESIGN = gsub("[.]BUD.*$", "", DESIGN.BUD),
  BUD = gsub("^.*[_]", "", DESIGN.BUD)
)][, ':=' (
  STRATA = factor(gsub("[.].*$", "", DESIGN), levels = strat_vec),
  ALLO = factor(gsub("^.*[.]", "", DESIGN), levels = allo_vec)
)]
# Equal and Status quo are the same across stratifications, so omit those
interspersion_2022_lst_AK_OB <- interspersion_2022_lst_AK_OB[!(STRATA %in% c("FMP", "FIXED_FMP") & (ALLO %in% c("EQUAL", "STATUS_QUO")))]
interspersion_2022_lst_AK_NONOB <- interspersion_2022_lst_AK_NONOB[!(STRATA %in% c("FMP", "FIXED_FMP") & (ALLO %in% c("EQUAL", "STATUS_QUO")))]

dmn_insp_plot_theme_2022 <- list(
  scale_x_continuous(limits = c(0,1), breaks = seq(0, 1, 0.2)),
  scale_y_discrete(limits = rev),
  theme(
    #legend.position = "none",
    strip.text = element_text(margin = margin(t=3, r=3, b=3, l=3))),
  labs(x = "Gear-specific Interspersion", y = "Gear Type")
)

ggplot(interspersion_2022_lst_AK_OB, aes(y = GEAR, x = POOL_DMN_INTERSPERSION)) + 
  facet_nested(STRATA + ALLO ~ BUD + POOL) + geom_col(aes(fill = I(FILL))) + 
  geom_point(data = interspersion_2022_lst_AK_NONOB, shape = 23, aes(fill = I("yellow")), stroke = 1) + 
  geom_text(aes(label = round(BOX_DMN_w)), x = 0.15, hjust = 1, size = 3, color = "white") + 
  dmn_insp_plot_theme_2022

# Do the same but by FMP

interspersion_2022_lst_FMP <- lapply(interspersion_2022_lst, "[[", "FMP")
interspersion_2022_lst_FMP_OB <- rbindlist(lapply(interspersion_2022_lst_FMP, "[[", "OB"), idcol = "DESIGN.BUD")
interspersion_2022_lst_FMP_NONOB <- rbindlist(lapply(interspersion_2022_lst_FMP, "[[", "NONOB"), idcol = "DESIGN.BUD")
interspersion_2022_lst_FMP_OB[, ':=' (
  DESIGN = gsub("[.]BUD.*$", "", DESIGN.BUD),
  BUD = gsub("^.*[_]", "", DESIGN.BUD)
)][, ':=' (
  STRATA = factor(gsub("[.].*$", "", DESIGN), levels = strat_vec),
  ALLO = factor(gsub("^.*[.]", "", DESIGN), levels = allo_vec)
)]
interspersion_2022_lst_FMP_NONOB[, ':=' (
  DESIGN = gsub("[.]BUD.*$", "", DESIGN.BUD),
  BUD = gsub("^.*[_]", "", DESIGN.BUD)
)][, ':=' (
  STRATA = factor(gsub("[.].*$", "", DESIGN), levels = strat_vec),
  ALLO = factor(gsub("^.*[.]", "", DESIGN), levels = allo_vec)
)]
# Equal and Status quo are the same across stratifications, so omit those
interspersion_2022_lst_FMP_OB <- interspersion_2022_lst_FMP_OB[!(STRATA %in% c("FMP", "FIXED_FMP") & (ALLO %in% c("EQUAL", "STATUS_QUO")))]
interspersion_2022_lst_FMP_NONOB <- interspersion_2022_lst_FMP_NONOB[!(STRATA %in% c("FMP", "FIXED_FMP") & (ALLO %in% c("EQUAL", "STATUS_QUO")))]

ggplot(interspersion_2022_lst_FMP_OB, aes(y = GEAR, x = POOL_DMN_INTERSPERSION)) + 
  facet_nested(STRATA + ALLO ~ BUD + POOL + BSAI_GOA) + geom_col(aes(fill = I(FILL))) + 
  geom_point(data = interspersion_2022_lst_FMP_NONOB, shape = 23, fill = "yellow", stroke = 1) + 
  geom_text(aes(label = round(BOX_DMN_w)), x = 0.15, hjust = 1, size = 3, color = "white") + 
  dmn_insp_plot_theme_2022

# Budget-specific figures
ggplot(interspersion_2022_lst_AK_OB[BUD == "4.5M"], aes(y = GEAR, x = POOL_DMN_INTERSPERSION)) + 
  facet_nested(STRATA + ALLO ~ BUD + POOL) + geom_col(aes(fill = I(FILL))) + 
  geom_point(data = interspersion_2022_lst_AK_NONOB[BUD == "4.5M"], shape = 23, fill = "yellow", stroke = 1) + 
  geom_text(aes(label = round(BOX_DMN_w)), x = 0.15, hjust = 1, size = 3, color = "white") + 
  dmn_insp_plot_theme_2022

ggplot(interspersion_2022_lst_FMP_OB[BUD == "4.5M"], aes(y = GEAR, x = POOL_DMN_INTERSPERSION)) + 
  facet_nested(STRATA + ALLO ~ BUD + POOL + BSAI_GOA) + geom_col(aes(fill = I(FILL))) + 
  geom_point(data = interspersion_2022_lst_FMP_NONOB[BUD == "4.5M"], shape = 23, fill = "yellow", stroke = 1) + 
  geom_text(aes(label = round(BOX_DMN_w)), x = 0.15, hjust = 1, size = 3, color = "white") + 
  dmn_insp_plot_theme_2022

gear_insp_legend <- list(
  scale_fill_identity(
    name = "Comparisons",
    breaks = c( "dodgerblue", "chartreuse3", "darkorchid4", "dodgerblue4", "yellow"),
    labels = c("OB to EM_FIXED", "OB to OB", "OB to ZE",  "EM_TRW to EM_TRW", "EM_FIXED to EM_FIXED"),
    guide = "legend"
  ),
  guides(fill = guide_legend(override.aes = list(shape = NA))),
  theme(
    legend.position = "bottom"
  )
)

interspersion_2022_plot_lst <- mapply(
  function(x1, x2, x3, x4) {
    list(
      AK = ggplot(x1, aes(y = GEAR, x = POOL_DMN_INTERSPERSION)) + 
        facet_nested(STRATA + ALLO ~ BUD + POOL, labeller = labeller(BUD = function(x) paste0("Budget: $", x))) + 
        geom_col(aes(fill = I(FILL))) + 
        geom_point(data = x2, shape = 23, aes(fill = I("yellow")), stroke = 1) + 
        geom_text(aes(label = round(BOX_DMN_w)), x = 0.15, hjust = 1, size = 3, color = "white") + 
        dmn_insp_plot_theme_2022 + 
        gear_insp_legend ,
      FMP = ggplot(x3, aes(y = GEAR, x = POOL_DMN_INTERSPERSION)) + 
        facet_nested(STRATA + ALLO ~ BUD + POOL + BSAI_GOA, labeller = labeller(BUD = function(x) paste0("Budget: $", x))) + 
        geom_col(aes(fill = I(FILL))) + 
        geom_point(data = x4, shape = 23, aes(fill = I("yellow")), stroke = 1) + 
        geom_text(aes(label = round(BOX_DMN_w)), x = 0.25, hjust = 1, size = 3, color = "white") + 
        dmn_insp_plot_theme_2022 + 
        gear_insp_legend 
    )
  },
  x1 = split(interspersion_2022_lst_AK_OB, by = "BUD"),
  x2 = split(interspersion_2022_lst_AK_NONOB, by = "BUD"),
  x3 = split(interspersion_2022_lst_FMP_OB, by = "BUD"),
  x4 = split(interspersion_2022_lst_FMP_NONOB, by = "BUD"),
  SIMPLIFY = F
)

# Subset all "overall" figures
figs_insp_2022 <- lapply(interspersion_2022_plot_lst, "[[", "AK")
figs_insp_2022_grobs <- lapply(figs_insp_2022, ggplotGrob)
figs_insp_2022_widths_lst <- lapply(figs_insp_2022_grobs, function(x) x[["widths"]])
figs_insp_2022_width <- do.call(unit.pmax, figs_insp_2022_widths_lst)
for(i in 1:length(figs_insp_2022_grobs)) figs_insp_2022_grobs[[i]]$widths <- figs_insp_2022_width

figs_insp_2022_fmp <- lapply(interspersion_2022_plot_lst, "[[", "FMP")
figs_insp_2022_fmp_grobs <- lapply(figs_insp_2022_fmp, ggplotGrob)
figs_insp_2022_fmp_widths_lst <- lapply(figs_insp_2022_fmp_grobs, function(x) x[["widths"]])
figs_insp_2022_fmp_width <- do.call(unit.pmax, figs_insp_2022_fmp_widths_lst)
for(i in 1:length(figs_insp_2022_fmp_grobs)) figs_insp_2022_fmp_grobs[[i]]$widths <- figs_insp_2022_fmp_width

#================================#
##  [Appendix B] Gear-Specific Interspersion (AK-wide and FMP-specific for each budget)

ggsave(grid.arrange(figs_insp_2022_grobs[["3.5M"]]) ,     file = "output_figures/figure_B_lo_AK.png", width = 12, height = 6, units = "in")
ggsave(grid.arrange(figs_insp_2022_fmp_grobs[["3.5M"]]),  file = "output_figures/figure_B_lo_FMP.png", width = 12, height = 6, units = "in")

ggsave(grid.arrange(figs_insp_2022_grobs[["4.5M"]]),      file = "output_figures/figure_B_md_AK.png", width = 12, height = 6, units = "in")
ggsave(grid.arrange(figs_insp_2022_fmp_grobs[["4.5M"]]),  file = "output_figures/figure_B_md_FMP.png", width = 12, height = 6, units = "in")

ggsave(grid.arrange(figs_insp_2022_grobs[["5.25M"]]),     file = "output_figures/figure_B_hi_AK.png", width = 12, height = 6, units = "in")
ggsave(grid.arrange(figs_insp_2022_fmp_grobs[["5.25M"]]), file = "output_figures/figure_B_hi_FMP.png", width = 12, height = 6, units = "in")
