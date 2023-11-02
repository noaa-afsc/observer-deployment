# effort_prediction_exploration.R
# Author: Geoff Mayhew
# Start Date: 2023-Oct-11
# Revised Date:

# This script is a quick visualization of how partial coverage fishing effort has changed in terms of numbers of trips
# between 2015 and 2022. Its purpose is to fuel discussion on effort predictions for the ADPs and whether/how it can 
# be improved.

library(data.table)
library(ggplot2)
library(ggh4x)      # For facet_nested()
library(grid)       # For combining plots
library(gridExtra)  # For combining plots

#======================================================================================================================#
# Load Data ----
#======================================================================================================================#

# Load pc_effort_dt (Valhalla transformed to 2023 stratum definitions, 2015 to 2023)
load("~/GitHub/2024_ADP/analyses/allocation_evaluation/data_prep_final.Rdata")

# Load the ADFG statistical area shapefile. '../' is needed for Rmarkdown to climb into parent folders.
stat_area_sf <- st_read(
  dsn = "source_data/ADFG_Stat_Area_shapefile/PVG_Statewide_2001_Present_GCS_WGS1984.shp", quiet = T) %>%
  select(STAT_AREA) %>%
  st_transform(crs = 3467)

# Load the Alaska map sf objects
load("source_data/ak_shp.rdata")      # shp_land, shp_nmfs, and shp_centroids added to global

#======================================================================================================================#
# Functions ----
#======================================================================================================================#

# Load 2024 ADP allocation/stratification functions
source("analyses/allocation_evaluation/functions.R")

# This function makes similar plots have the same widths so they look clean when switching tabsets
unify_plot_widths <- function(plot_lst) {
  plot_grobs_lst <- lapply(plot_lst, ggplotGrob)
  figs_widths_lst <- lapply(plot_grobs_lst, function(x) x[["widths"]])
  figs_width_pmax <- do.call(unit.pmax, figs_widths_lst)
  for(i in 1:length(plot_grobs_lst)) plot_grobs_lst[[i]]$widths <- figs_width_pmax 
  # Return list of grobs with unified widths
  plot_grobs_lst
}

#======================================================================================================================#
# SKIP DOWN TO 'RESTARTING' ----
#======================================================================================================================#



#======================================================================================================================#
# Data Prep ----
#======================================================================================================================#

pc_effort_2015_2023 <- pc_effort_dt[!is.na(TARGET) & AGENCY_GEAR_CODE != "JIG", .(ADP, TRIP_ID, TARGET, STRATA, BSAI_GOA, GEAR)]
pc_effort_2015_2023  <- unique(pc_effort_2015_2023 [BSAI_GOA %in% c("BS", "AI"), BSAI_GOA := "BSAI"])
# Some trips have more than one GEAR and TRIP_TARGET
# Group up trip targets, putting all non S, I, C, and P into the 'other' category, 'O'
pc_effort_2015_2023 [, TARGET_2 := fcase(
  TARGET == "B", "P",
  TARGET %in% c("W", "H", "X", "K", "O", "A", "Y", "L"), "O",
  TARGET %in% c("S", "I", "C", "P"), TARGET)]
# Define monitoring method as OB or EM
pc_effort_2015_2023[, MON := fcase(STRATA %like% "EM", "EM", STRATA %like% "OB", "OB", STRATA %like% "ZERO", "ZERO")]

#======================================================================================================================#
# Plots by Gear and FMP, without and with Target ----
#======================================================================================================================#

# Total Effort
pc_effort_2015_2023_count_prop_total <- pc_effort_2015_2023[, .(COUNT = as.numeric(uniqueN(TRIP_ID))), keyby = .(ADP, MON, GEAR, BSAI_GOA, TARGET_2)]
pc_effort_2015_2023_count_prop_total <- pc_effort_2015_2023_count_prop_total[, PROP := COUNT / sum(COUNT), keyby = .(ADP, MON, BSAI_GOA)]
pc_effort_2015_2023_count_prop_total <- melt(pc_effort_2015_2023_count_prop_total, id.vars = c("ADP", "MON", "GEAR", "BSAI_GOA", "TARGET_2"), measure.vars = c("COUNT", "PROP"))

# Gear type only. Proportions by gear type has definitely changed over time.
# in 2016, OB TRW is high, big Cod year.
total_gear <- ggplot(pc_effort_2015_2023_count_prop_total, aes(x = ADP, y = value, fill = GEAR)) + 
  geom_col() +
  facet_nested(variable ~ BSAI_GOA + MON, scales = "free_y") + 
  labs(x = "Year", y = "Value", fill = "Gear") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# So the question is, can we reliably predict changes in counts of fishing trips year to year better than just using
# the previous year's counts? How would we be able to predict inflection points?

# Adding Target as fill color, gear type is line color
total_gear_target <- ggplot(pc_effort_2015_2023_count_prop_total, aes(x = ADP, y = value, color = GEAR, fill = TARGET_2)) + 
  geom_col(size = 1) +
  scale_fill_viridis_d() + 
  facet_nested(variable ~ BSAI_GOA + MON, scales = "free_y") + 
  labs(x = "Year", y = "Value", color = "Gear", fill = "Target") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Split by trip target
pc_effort_2015_2023_count_prop <- pc_effort_2015_2023[, .(COUNT = as.numeric(uniqueN(TRIP_ID))), keyby = .(ADP, MON, GEAR, BSAI_GOA, TARGET_2)]
pc_effort_2015_2023_count_prop[, PROP := COUNT / sum(COUNT), keyby = .(ADP, MON, GEAR, BSAI_GOA)]
pc_effort_2015_2023_count_prop <- melt(pc_effort_2015_2023_count_prop, id.vars = c("ADP", "MON", "GEAR", "BSAI_GOA", "TARGET_2"), measure.vars = c("COUNT", "PROP"))
split_gear <- ggplot(pc_effort_2015_2023_count_prop, aes(x = ADP, y = value, fill = GEAR)) + 
  geom_col(size = 1) +
  facet_nested(variable + GEAR ~ BSAI_GOA + MON, scales = "free_y") + 
  labs(x = "Year", y = "Value", fill = "Gear") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Again, include target as a fill color
split_gear_target <- ggplot(pc_effort_2015_2023_count_prop, aes(x = ADP, y = value, fill = TARGET_2)) + 
  geom_col(size = 1) +
  scale_fill_viridis_d() + 
  facet_nested(variable + GEAR ~ BSAI_GOA + MON, scales = "free_y") + 
  labs(x = "Year", y = "Value", fill = "Target") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Combine plots and unify widths
effort_prediction_exploration_plots <- unify_plot_widths(list(
  GEAR = total_gear, 
  GEAR_TARGET = total_gear_target,
  SPLIT_GEAR = split_gear,
  SPLIT_GEAR_TARGET = split_gear_target))

#grid.arrange(effort_prediction_exploration_plots$GEAR)
#grid.arrange(effort_prediction_exploration_plots$GEAR_TARGET)
#grid.arrange(effort_prediction_exploration_plots$SPLIT_GEAR)
#grid.arrange(effort_prediction_exploration_plots$SPLIT_GEAR_TARGET)

# In the BSAI, most fishing effort via ZERO-HAL, of monitored catch, it's OB-POT gear. Since 2020, ZERO also has POT trips.
# Overall the proportions between fixed-gear EM and OB are very similar between years!
# In recent years, count of HAL has decreased whereas POT has increased
# In Recent years, count of TRW has decreased
# Within HAL, proportions of targets has changed little. Within POT, switched from C to S. Within OB_TRW, no more O.

# Save objects ----
save(effort_prediction_exploration_plots, file = "analyses/effort_prediction/effort_prediction_exploration.rdata")

# Stratify by new stratification definitions? Could also stratify by gear type and monitoring method?

val_2015_2023_dt <- pc_effort_dt[ADP %in% 2015:2023]
val_2015_2023_dt[BSAI_GOA %in% c("BS", "AI"), BSAI_GOA := "BSAI"]
val_2015_2023_dt <- unique(val_2015_2023_dt)

#===============================#
## Prepare trips_melt object ----
#===============================#

# trips_melt has metrics and trip duration for all trips >= 2015. . 
# Add ADP year, STRATA, and DAYS of each TRIP_ID to trips_melt
trips_melt_add_data <- unique(pc_effort_dt[STRATA != "ZERO", .(ADP, TRIP_ID, STRATA, DAYS)])
trips_melt <- trips_melt_add_data[trips_melt, on = .(TRIP_ID)]
setcolorder(trips_melt, c("ADP", "STRATA", "TRIP_ID", "DAYS", "Metric", "Value"))
setkey(trips_melt, ADP, STRATA, TRIP_ID, DAYS, Metric, Value)
# Error-check
if(nrow(trips_melt[, .N, by = .(TRIP_ID)][N != uniqueN(trips_melt$Metric)])) stop("At least one trip has number of metrics != 3!")
# Remove records missing data
trips_melt <- trips_melt[!is.na(ADP)]


#=================================#
## Stratification: GEAR x FMP ----
#=================================#

# Define boxes, splitting trips into strata based on FMP: BSAI and GOA

system.time(fmp.box <- define_boxes_3(
  val_2015_2023_dt, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), geom = T ))
# Re-calculate trip durations
fmp.allo_lst <- update_strata(pc_effort_dt, trips_melt, stratum_cols = c("STRATA", "BSAI_GOA"), focus_years = 2015:2023)

# Assume a 15% sample rate for all strata when calculating interspersion
sample_rate_vec <- 0.15

fmp.prox_insp <- calculate_interspersion(fmp.box, sample_rate_vec)
fmp.prox_insp$ispn_dt[ADP == 2022]
fmp.prox_insp$strata_n_dt[ADP == 2022]

# If you want to break it into months, kind of need to feed it smaller sections
# For now, focus on 2022 and see if you can create monthly totals? Might need to apply smoother?
test_2022 <- fmp.box$dt_out[ADP == 2022]  # 50 weeks in 2022
test_2022[, MONTH := floor((TIME - 1)/4)]




# calculate_interspersion needs params, strata_n_dt, and box_smry, and strata_n_dt needs to be specific to each 'month'
# so that interspersion uses the proper denominator. Can I just use the sum of BOX_w?

# I have to rebuild each box_smry, using BOX_ID to reference WEEK and BSAI_GOA, subset, and sum BOX_w to rebuild STRATA_N_dt

  
insp_by_fmp_x_month <- function(box_def) {
  # box_def <- copy(fmp.box)
  
  # Find all unique fmp, year, and month combinations
  fmp_year_week_dt <- unique(box_def$dt_out[, .(BSAI_GOA, ADP, TIME)])
  fmp_year_week_dt[, MONTH := floor((TIME - 1) / 4) + 1]
  year_week_month_dt <- unique(fmp_year_week_dt[, .(ADP, TIME, MONTH)])
  fmp_year_month_dt <- unique(fmp_year_week_dt[, .(BSAI_GOA, ADP, MONTH)])
  
  # Subset for each fmp_year_month_dt
  split_fmp_month_lst <- apply(fmp_year_month_dt, MARGIN = 1,  function(x) {
    # x <- fmp_year_month_dt[11, ]
    # cat(x, "\n")
    
    # 'x' will become a character when fed through 'apply', so have to rebuild it's data.table structure to use in join.
    x <- setnames(data.table(t(x)), c("BSAI_GOA", "ADP", "MONTH"))
    x[, c("ADP", "MONTH") := lapply(.SD, as.integer), .SDcols = c("ADP", "MONTH")]
    
    fmp_month_sub <- box_def$dt_out[year_week_month_dt[x, on = .(ADP, MONTH)], on = .(ADP, TIME, BSAI_GOA)]
    
    # fmp_month_sub
    
    # Count weight of trips in this FMP x MONTH
    fmp_month_sub[, .(sum_BOX_w = sum(BOX_w)), keyby = .(STRATA, ADP, BSAI_GOA, MONTH)]
    
     #Need to subset box_smry

    #names(box_def$box_smry) # Need to subset each of these, already subset by year_col and stratum_cols
    #box_def$params
    
    # Subset box_smry 
    box_smry_sub <- box_def$box_smry[names(box_def$box_smry) %like% (x$BSAI_GOA) & names(box_def$box_smry) %like% (x$ADP) ]
    box_id_sub <- fmp_month_sub[, unique(BOX_ID)]
    box_smry_sub_out <- lapply(box_smry_sub, function(x) x[x[,"BOX_ID"] %in% box_id_sub, , drop = F])
    
    strata_n_dt_sub <- data.table(YEAR_STRATA_FMP = names(box_smry_sub_out))
    strata_n_dt_sub[, c("ADP", "STRATA", "BSAI_GOA") := tstrsplit(YEAR_STRATA_FMP, split = "[.]")]
    strata_n_dt_sub[, ADP := as.integer(ADP)]
    strata_n_dt_sub[, STRATA_N := sapply(box_smry_sub_out, function(x) sum(x[, "BOX_w"])) ]
    
    list(
      box_smry = box_smry_sub_out,
      strata_n_dt = strata_n_dt_sub,
      params = box_def$params
    )
    
  })
  names(split_fmp_month_lst) <- apply(fmp_year_month_dt, 1, function(x)  paste(trimws(x), collapse = "."))
  
  # output
  split_fmp_month_lst
}


what <- insp_by_fmp_x_month(fmp.box)
fmp.prox_insp_month <- lapply(what, function(x) calculate_interspersion(x, sample_rate_vec))

# subset all interspersion results and collapse
interspersion_fmp_month <- rbindlist(lapply(fmp.prox_insp_month, "[[", "ispn_dt"), idcol = "FMP_YEAR_MONTH")
interspersion_fmp_month[, MONTH := tstrsplit(FMP_YEAR_MONTH, split = "[.]")[[3]]]
interspersion_fmp_month[, MONTH := as.integer(MONTH)]
setorder(interspersion_fmp_month, STRATA, BSAI_GOA, ADP, MONTH)

ggplot(interspersion_fmp_month, aes(x = MONTH, y = ISPN, color = as.character(ADP))) + facet_grid(BSAI_GOA ~ STRATA) + geom_line() + 
  scale_colour_viridis_d(direction = -1) + labs(x = "Month", y = "Proximity Index", color = "Year")


#============================================================
# How has interspersion changed with STRATA_N each year? ####
#============================================================

# Define boxes again split by FMP, but make neighboring gear-specific for the fixed-gear strata
system.time(fixed_fmp.box <- define_boxes_3(
  val_2015_2023_dt, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), geom = T ))
fixed_fmp.allo_lst <- update_strata(val_2015_2023_dt, trips_melt, stratum_cols = c("STRATA", "BSAI_GOA"), focus_years = 2015:2023)

fixed_fmp.prox_insp <- calculate_interspersion(fixed_fmp.box, sample_rate_vec)
fixed_fmp.prox_insp$ispn_dt[, `ISPN/STRATA_N` := ISPN / STRATA_N ]
fixed_fmp.prox_insp$ispn_dt[, `STRATA_N/ISPN` := STRATA_N / ISPN ]
fixed_fmp.prox_insp$ispn_dt[, `logSTRATA_N/ISPN` := log(STRATA_N) / ISPN ]
fixed_fmp.prox_insp$ispn_dt[, `ISPN/logSTRATA_N` := ISPN / log(STRATA_N) ]

#fixed_fmp_plot_data <- melt(fixed_fmp.prox_insp$ispn_dt, id.vars = c("ADP", "STRATA", "BSAI_GOA"), measure.vars = c("ISPN", "STRATA_N", "ISPN/STRATA_N", "STRATA_N/ISPN", "logSTRATA_N/ISPN", "ISPN/logSTRATA_N"))
fixed_fmp_plot_data <- melt(fixed_fmp.prox_insp$ispn_dt, id.vars = c("ADP", "STRATA", "BSAI_GOA"), measure.vars = c("ISPN", "STRATA_N", "ISPN/logSTRATA_N"))

# TODO NOTE THAT THIS IS NOT INTERSPERSION, IT'S THE 'PROXIMITY INDEX'. I HAVE TO FEED THE RATES TO EVALUATION INTERSPERSION THING AS WELL...
# THEREFORE IT IS NOT GEAR SPECIFIC!
ggplot(fixed_fmp_plot_data, aes(x = ADP, y = value)) + 
  facet_nested(variable ~ STRATA + BSAI_GOA, scales = "free_y") + 
  geom_line()
# I don't really know what this is telling me...
# OB_POT_GOA is trending upwards

# overall, Interspersion increases with STRATA_N in a logarithmic (or square root) relationship
ggplot(fixed_fmp.prox_insp$ispn_dt, aes(x = STRATA_N, y = ISPN, color = STRATA, shape = BSAI_GOA, linetype = BSAI_GOA)) + geom_point(size = 3, stroke = 1)  + 
  geom_path(aes(group = interaction(STRATA, BSAI_GOA))) + 
  scale_shape_manual(values = c(BSAI = 16, GOA = 2)) + 
  labs(x = "# Trips in Stratum", y = "Proximity Index", shape = "FMP", linetype = "FMP", color = "Stratum")

# Interestingly, ISPN goes up faster in the BSAI than the GOA as STRATA_N increases. This tells me that fishing is generally more clumped in time and space
# in the BSAI than in the GOA (which admittedly is much larger in space)
ggplot(fixed_fmp.prox_insp$ispn_dt, aes(x = STRATA_N, y = ISPN, color = STRATA, shape = BSAI_GOA, linetype = BSAI_GOA)) + geom_point(size = 3, stroke = 1) + scale_shape_manual(values = c(BSAI = 16, GOA = 2)) + 
  facet_grid(BSAI_GOA ~ STRATA, scales = "free") + geom_path(aes(group = interaction(STRATA, BSAI_GOA))) + 
  labs(x = "# Trips in Stratum", y = "Proximity Index", shape = "FMP", linetype = "FMP", color = "Stratum")

# Here we can see how the values changed year to year, and whether a trend exists
ggplot(fixed_fmp.prox_insp$ispn_dt, aes(x = STRATA_N, y = ISPN, color = ADP, shape = BSAI_GOA)) + geom_point(size = 3, stroke = 1) + scale_shape_manual(values = c(BSAI = 16, GOA = 2)) + 
  facet_grid(BSAI_GOA ~ STRATA, scales = "free") + scale_color_viridis_c(direction = -1)  + geom_path(aes(group = interaction(STRATA, BSAI_GOA))) + 
  labs(x = "# Trips in Stratum", y = "Proximity Index", shape = "FMP", color = "Year")




# WHAT IS ps_cols has GEAR

# Define boxes again split by FMP, but make neighboring gear-specific for the fixed-gear strata

fixed.val_2015_2023_dt <- unique(copy(val_2015_2023_dt)[STRATA %like% "HAL|POT", STRATA := paste0(POOL, "_", "FIXED")])





system.time(fixed_fmp.box <- define_boxes_3(
  fixed.val_2015_2023_dt[ADP == 2022], c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), dmn_cols = c("GEAR"), geom = T ))

# TODO when I calculate interspersion for FIXED stratification, did I keep HAL and POT separate?


# define_boxes_gs() doesn't do anything. Same as define_boxes but with worthless columns
system.time(fixed_fmp.box.gs <- define_boxes_gs(
  fixed.val_2015_2023_dt[ADP == 2022], c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), dmn_cols = c("GEAR"), geom = T, ps_cols = c("GEAR")))

calculate_interspersion(fixed_fmp.box, sample_rate_vec)$ispn_dt
calculate_interspersion_gs(fixed_fmp.box.gs, sample_rate_vec)$ispn_dt 


# However, I can't run the gs-specific interspersion function on the non-gs box definition...
calculate_interspersion_gs(fixed_fmp.box, sample_rate_vec)

# FIXME In analyses/stratification_allocation_test.R, under ## Fixed X FMP ---- I used  define_boxes_gs() and
# calculate_interspersion_gs(). But why am I finding the counts are the same?
# system.time(box_fixed_fmp_gs <- define_boxes_gs(
#   val_fixed, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
#   year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), ps_cols = "GEAR", dmn_cols = c("GEAR"), geom = T))

 
# FUCK EVERYTHING IS WRONG, HAVE TO REDO THE DRAFT RATES SENT OUT.... 



# does define_boxes_gs work the same as define_boxes_3?

system.time(test_og <- define_boxes_3(
  val_2015_2023_dt, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), dmn_cols = "GEAR", geom = T ))

system.time(test_gs <- define_boxes_gs(
  val_2015_2023_dt, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), dmn_cols = "GEAR", geom = T ))

system.time(test_gs_ps <- define_boxes_gs(
  val_2015_2023_dt, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), dmn_cols = "GEAR", ps_cols = c("GEAR"), geom = T ))

fsetequal(
  test_og$dt_out,
  test_gs$dt_out[, -c("PS_ID", "PS")]
)  # Good, so these match
# Interestingly, test_ps_ps doesn't have the "PS" colum that is in test_gs (which is a bunch of NAs anyway)
# TODO REMOVE 'PS' column when ps_cols is NULL
fsetequal(
  test_og$dt_out,
  test_gs_ps$dt_out[, -c("PS_ID", "PS")]
) 

# TODO Now can define_interspersion_gs handle both test_gs and test_gs_ps, and get the same results as test_og with define_interspersion?
fsetequal(
  calculate_interspersion(test_og, sample_rate_vec)$ispn_dt,
  calculate_interspersion_gs(test_gs, sample_rate_vec)$ispn_dt
)  # Cool, so this works
fsetequal(
  calculate_interspersion_gs(test_gs, sample_rate_vec)$ispn_dt,
  calculate_interspersion_gs(test_gs_ps, sample_rate_vec)$ispn_dt,
) # and these differ as they should

# are dmn objects the same between define_boxes_3() and define_boxes_gs()?
fsetequal(
  test_og$dmn$box_dmn_smry_dt,
  test_gs$dmn$box_dmn_smry_dt
) 

# domain objects are the same here, great
fsetequal(
  test_og$dmn$box_dmn_smry_dt,
  test_gs_ps$dmn$box_dmn_smry_dt
) 




define_boxes_gs <- function(data, space, time, year_col, stratum_cols, dmn_cols = NULL, stata_area_sf = stat_area_sf, geom = F, ps_cols = NULL) {
  # data <- copy(fixed.val_2015_2023_dt[ADP == 2022]); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col = "ADP"; stratum_cols = c("STRATA", "BSAI_GOA"); dmn_cols <- "GEAR"; geom <- T
  
  #==================================#
  # NOTE! Remove any jig-gear trips! #
  #==================================# 
  # Jig-gear trips are generally in zero coverage (though sometimes HAL or EM_HAL trips might also fish
  # with jig gear in a trip?). Therefore, from an allocation perspective, it's not important if the rate will be 0.
  # Additionally, from an evaluation perspective, we don't use observed trips to make estimates for jig trips, so
  # we can remove them without issue. MAKE SURE 'data' HAS NO JIG GEAR COMPONENTS!
  
  # Check for jig-gear in the dataset
  check_jig_cols <- unique(c(stratum_cols, dmn_cols, ps_cols))
  check_jig_cols <- apply(data[, ..check_jig_cols], 2, function(x) any(x == "JIG"))
  # Remove jig records
  if(any(check_jig_cols)) {
    jig_cols <- names(which(check_jig_cols))
    for(i in jig_cols) {
      jig_trips <- data[which(data[[jig_cols]] == "JIG"), ]
      cat(paste0("Removing ", nrow(jig_trips), " rows from ", uniqueN(jig_trips$TRIP_ID), " JIG trips from ", i, " column."), "\n")
      data <- fsetdiff(data, jig_trips)
    }
  }
  
  win <- as.integer(time[2])
  
  # Make sure integer TRIP_ID, integer ADFG_STAT_AREA_CODE, and 'dmn_cols' are specified in the data
  if( length( intersect(colnames(data), c("TRIP_ID", "ADFG_STAT_AREA_CODE", dmn_cols))) !=  length(c("TRIP_ID", "ADFG_STAT_AREA_CODE", dmn_cols)) ) {
    stop(paste0("'data' must have columns 'TRIP_ID' and 'ADFG_STAT_AREA_CODE and dmn_cols: ", paste(dmn_cols, collapse = ", ")))
  } else {
    if( all(lapply(data[, c("TRIP_ID", "ADFG_STAT_AREA_CODE")], class) != "integer")) {
      stop ("'TRIP_ID' and 'ADFG_STAT_AREA_CODE' must be of class integer.")
    }
  }
  if(nrow(unique(data[, .(TRIP_ID, STRATA)])) != uniqueN(data$TRIP_ID)) stop("A trip is not in only one stratum!")
  
  # Remove any unused columns
  keep_cols <- unique(c(year_col, stratum_cols, dmn_cols, "ADFG_STAT_AREA_CODE", time[3], time[4], "TRIP_ID", ps_cols))
  data <- unique(subset(data, select = keep_cols))      
  
  #========================#
  # Convert ADFG to HEX_ID #
  #========================#
  
  stat_area_lst <- stat_area_to_hex(space[1], stat_area_sf)
  stat_area_dist_lst <- suppressWarnings(apply(
    X = round(st_distance(st_centroid(stat_area_lst$HEX_GEOMETRY))), 
    MARGIN = 1, 
    FUN = function(x) which(x <= space[2])))
  data <- unique(data.table(stat_area_lst$STAT_AREA_HEX_DF)[
  ][data, on = .(ADFG_STAT_AREA_CODE)
  ][, -"ADFG_STAT_AREA_CODE"])
  setcolorder(data, neworder = unique(c(year_col, stratum_cols, ps_cols, dmn_cols, "HEX_ID", time[3], time[4])))
  setkeyv(data, cols = unique(c(year_col, stratum_cols, ps_cols, dmn_cols, "HEX_ID", time[3], time[4])))
  
  if(nrow(data[is.na(HEX_ID)])) {
    print(data[is.na(HEX_ID)])
    stop("Could not assign a HEX_ID!")
  }
  
  #======================#
  # Define temporal unit #
  #======================#
  
  # First, get all years and a table converting date to week
  if(time[1] != "week") stop("So far this function only works with 'week()' function!")
  dates_lst <- lapply(
    lapply(
      unique(unlist(data[, ..year_col])),
      function(x) as.Date(paste0(x, c("-01-01", "-12-31")))
    ),
    function(x) as.Date(x[1] : x[2], origin = as.POSIXct("1970-01-01", tz = "UTC"))
  )
  dates_mtx <- cbind(unlist(dates_lst), unlist(lapply(dates_lst, get(time[1]))))
  dates_start <- min(dates_mtx[, 1]) - 1  # Get first date and subtract 1. T
  dates_mtx[, 1] <- dates_mtx[, 1] - dates_start  # This makes it so matrix can be reference by row index, much faster
  
  # Grab time columns and define groups based on TRIP_ID and HEX_ID
  time_cols <- time[3:4]
  time_int <- data[, ..time_cols]
  time_int[, (time_cols) := lapply(.SD, as.integer), .SDcols = time_cols]
  setnames(time_int, new = c("S", "E"))
  data_int <- data[, .(TRIP_ID, HEX_ID)][, GRP := .GRP, by = .(TRIP_ID, HEX_ID)]
  # Convert to matrix and split by TRIP_ID and HEX_ID
  time_lst <- as.matrix(cbind(time_int - dates_start, data_int[, .(GRP)]))
  time_lst <- lapply(split(time_lst, time_lst[, "GRP"], drop = F), matrix, ncol = 3)
  # For each TRIP_ID x HEX_ID, identify unique weeks
  time_lst <- lapply(time_lst, function(x) {
    dates_int <- unique(unlist(apply(x, 1, function(y) y[1] : y[2], simplify = F)))  # get unique days
    unique(dates_mtx[dates_int, 2, drop = F])                     # Identify week using dates_mtx
  }) 
  
  # Collapse into a data.table (this is much faster than rbindlist(lapply(data_lst, as.data.table)))
  data_dt <- as.data.table(
    do.call(rbind, Map(function(x1, x2) cbind(x1, x2), x1 = as.list(seq_along(time_lst)), x2 = time_lst)))
  setnames(data_dt, new = c("GRP", "TIME"))
  # Merge
  data_int <- unique(data_int)[data_dt, on = .(GRP)][, - "GRP"]
  keep_cols2 <- c(setdiff(keep_cols, c(time_cols, "ADFG_STAT_AREA_CODE")), "HEX_ID")
  data <- unique(data[, ..keep_cols2])[data_int, on = .(TRIP_ID, HEX_ID)]
  
  #============#
  # Define Box #
  #============#
  
  setkeyv(data, c(year_col, stratum_cols, "TIME", "HEX_ID"))
  setcolorder(data, unique(c(year_col, stratum_cols, ps_cols, "TIME", "HEX_ID")))
  # 'BOX_ID' is defined by HEX_ID and TIME and if specified, ps_cols, and is common to all data. This will be used to determine neighbors.
  # 'stratum_cols' is not included so that we can later compare overlap between strata
  data[, BOX_ID := .GRP, by = c(year_col, "TIME", "HEX_ID")]
  
  # [NOTE] HERE is where this function differs from define_boxes_3()
  if(!is.null(ps_cols)) {
    data[, PS_ID := .GRP, keyby = ps_cols]
    ps_cols_tbl <- unique(subset(data, select = c(ps_cols, "PS_ID")))
  } else {
    ps_cols_tbl <- data.table(PS = "NA", PS_ID = 1L)
    data[, PS_ID := 1L] 
  }
  setkey(data, BOX_ID)
  
  #==================#
  # Define Neighbors #
  #==================#
  
  # For each BOX_ID, find which BOX_IDs are neighboring based on week and spatial cells.
  sub_cols <- c(year_col, "TIME", "HEX_ID", "BOX_ID")
  st_mtx <- as.matrix(unique(data[, ..sub_cols]))
  nbr_lst <- apply(st_mtx, MARGIN = 1, function(x) {
    # x <- st_mtx[1,]
    box_mtx <- st_mtx[st_mtx[,1] == x[1], , drop = F]             # subset by year
    nbr_time <- (x[2] + (-win:win))                               # identify neighboring TIME
    box_mtx <- box_mtx[box_mtx[,2] %in% nbr_time, , drop = F]     # subset by time range
    nbr_hex_id <- stat_area_dist_lst[[x[3]]]                      # identify neighboring HEX_IDs
    box_mtx[box_mtx[, 3] %in% nbr_hex_id, 4]                      # subset by neighboring HEX_IDs, then grab BOX_ID
  }) # 0.5 sec, but should save time when identifying number of neighbors?
  # each element of this list corresponds to BOX_ID and contains all neighboring BOX_IDs
  
  #===================================================#
  # Split up the dataset by year_col and stratum_cols #
  #===================================================#
  
  if(!is.null(dmn_cols)) {
    data_dmn <- copy(data)       # Create a copy that will keep dmn_cols
    data <- unique(data[, -..dmn_cols])  # Remove 'dmn_cols' for now
  }
  
  group_cols  <- c(year_col, stratum_cols)
  setkeyv(data, cols = c(group_cols, "PS_ID", "BOX_ID"))
  data_lst <- lapply(
    X = split(x = subset(data, select = c(group_cols, "BOX_ID", "TRIP_ID", "PS_ID")), by = group_cols, keep.by = F),
    FUN = as.matrix)
  
  # Make the frequency table of each TRIP_ID (so that trips are properly split by HEX_ID, TIME, and if present, ps_cols
  trip_id_mat <- do.call(rbind, lapply(
    data_lst,
    function(p) {
      trip_id_frq <- table(p[, "TRIP_ID"])
      matrix(
        c(as.integer(names(trip_id_frq)), trip_id_frq),
        ncol = 2, dimnames = list(NULL, c("TRIP_ID", "Freq")))
    }))
  trip_id_mat <- trip_id_mat[order(trip_id_mat[,1]), ]
  trip_id_vec <- vector(mode = "integer")
  trip_id_vec[trip_id_mat[, 1]] <- trip_id_mat[, 2]
  
  #==================================================#
  # Identify which TRIP_IDs are neighboring each box #
  #==================================================#
  
  box_smry <- lapply(
    data_lst,
    function(stratum_mtx) {
      # as [1:BOX_ID] [2:TRIP_ID] and if specified, [3:PS_ID]
      # stratum_mtx <- data_lst[[5]]   # 2022.OB_FIXED.GOA
      
      # First, split by PS_ID
      ps_lst <- lapply(split(stratum_mtx[, 1:2], stratum_mtx[,3]), matrix, ncol = 2)
      
      # Now for each PS_ID group, identify neighboring trips
      lapply(ps_lst, function(x) {
        # x <- ps_lst[[1]]    # Here PS_ID = 1 is POT
        
        # Get all unique time and space boxes
        x1 <- unique(x[, 1])
        
        # Now for each BOX_ID listed in 'x1', count the number unique TRIP_IDs in neighboring BOX_IDs
        x2 <- do.call(rbind, lapply(x1, function(y) {
          # y <- x1[1]   # Box 5749
          
          trip_id_centered <- x[x[,1] == y, 2]    # Identify number of trips actually  the box
          # There shouldn't ever be the same trip counted twice in the same box.
          if( length(trip_id_centered) != length(unique(trip_id_centered)) ) stop("There is a duplicate trip_id!")
          
          cbind(
            BOX_ID = y,
            # Count of trips centered in BOX_ID
            BOX_n = length(trip_id_centered) ,
            # Weight of trips centered in BOX_ID
            # FIXME calculating BOX_w is slow!
            BOX_w = sum(1/trip_id_vec[trip_id_centered]), 
            # # Count of unique TRIP_IDs in neighboring BOX_IDs
            BOX_nbr = length(unique(x[(x[,1] %in% nbr_lst[[y]]), 2])) 
          )
          
        }))
      })
    }
  ) 
  
  # Calculate STRATA_N and ensure the sum of weights is equivalent
  strata_N_dt <- data[, .(STRATA_N = uniqueN(TRIP_ID)), by = group_cols]
  
  # Double-check that weights sum to STRATA_N
  if(!(all(
    unname(sapply(box_smry, function(x) sum(sapply(x, function(y) sum(y[, "BOX_w"]))))) == strata_N_dt$STRATA_N
  ))) stop("STRATA_N and sum(BOX_w) are not equal!")
  
  # Get post-strata weights (number of component trips in post-strata)
  ps_W_dt <- data[, .(ps_W = sum(1 / trip_id_vec[TRIP_ID])), by = c(group_cols, "PS_ID")]
  ps_W_dt <- ps_W_dt[ps_cols_tbl, on = .(PS_ID)]  # Merge in ps_cols on PS_ID
  setkeyv(ps_W_dt, c(year_col, stratum_cols, "PS_ID"))
  
  # Create data.table output
  st_dt <- unique(data[, .(BOX_ID, HEX_ID, TIME)])   # Table of BOX_ID, HEX_ID, and TIME
  dt_out <- 
    rbindlist(lapply(box_smry, function(x) rbindlist(lapply(x, as.data.table), idcol = "PS_ID")), idcol = "GROUP_COLS")[
    ][, (group_cols) := tstrsplit(GROUP_COLS, split = "[.]")
    ][, GROUP_COLS := NULL][
    ][st_dt, on = .(BOX_ID)]
  dt_out <- dt_out[
  ][, which(colnames(dt_out) == year_col) := as.integer(dt_out$ADP)
  ][, PS_ID := as.integer(PS_ID)
  ][ps_cols_tbl, on = .(PS_ID)]
  setcolorder(dt_out, c(group_cols, "PS_ID", "BOX_ID", "BOX_n", "BOX_w", "BOX_nbr", ps_cols, "HEX_ID", "TIME"))
  
  # Initialize outputs
  box_res <- list(
    box_smry = box_smry,
    strata_n_dt = strata_N_dt,
    ps_W_dt = ps_W_dt,
    dt_out = dt_out,
    og_data = data,
    nbr_lst = nbr_lst,
    params = list(stratum_cols = stratum_cols, year_col = year_col, dmn_cols = dmn_cols, ps_cols = ps_cols)
  )
  
  #==========================#
  # Handle dmn_cols now (GEAR)
  #==========================#
  
  if(!is.null(dmn_cols)) {
    
    # Get trip weights, splitting by dmn_cols as well
    trip_id_dmn_mat <- as.matrix(data_dmn[, .N, by = .(TRIP_ID)])
    trip_id_dmn_vec <- vector(mode = "integer")
    trip_id_dmn_vec[trip_id_dmn_mat[, 1]] <- trip_id_dmn_mat[, 2]
    
    # Identify all BOX_IDs within a dmn_tbl group
    dmn_tbl <- unique(data_dmn[, ..dmn_cols])
    dmn_lst <- vector(mode = "list", length = nrow(dmn_tbl))
    for(i in 1:nrow(dmn_tbl)) {
      # i <- 1
      
      focus_dmn_dat <- unique(subset(data_dmn[dmn_tbl[i,], on = dmn_cols], select = c(stratum_cols, "BOX_ID", "TRIP_ID")))
      focus_dmn_dat_stratum <- lapply(split(focus_dmn_dat, by = stratum_cols, keep.by = F), as.matrix)
      focus_dmn_box_ids <- unique(focus_dmn_dat$BOX_ID)
      
      # for each stratum...
      focus_dmn_nbr_lst <- lapply(focus_dmn_dat_stratum, function(x) {
        # x <- focus_dmn_dat_stratum[[1]]
        
        # for each BOX_ID
        x1 <- lapply(focus_dmn_box_ids, function(y) {
          # y <- focus_dmn_box_ids[[1]]
          
          trip_id_centered <- unique(x[x[,1] == y, 2])
          
          c(
            BOX_ID = y,
            BOX_DMN_n = length(trip_id_centered),
            BOX_DMN_w = sum(1/trip_id_dmn_vec[trip_id_centered]),
            BOX_DMN_nbr = length(unique(x[x[,1] %in% nbr_lst[[y]], 2]))
          )
          
        })
        as.data.table(do.call(rbind, x1))
        
      })
      focus_dmn_nbr_lst <- rbindlist(focus_dmn_nbr_lst, idcol = "STRATUM_COLS")
      focus_dmn_nbr_lst[, (stratum_cols) := tstrsplit(STRATUM_COLS, split = "[.]")][, STRATUM_COLS := NULL]
      dmn_lst[[i]] <- focus_dmn_nbr_lst
    }
    names(dmn_lst) <- apply(dmn_tbl, 1, paste0, collapse = ".")
    dmn_nbr_dt <- rbindlist(dmn_lst, idcol = "DMN_COLS")
    dmn_nbr_dt[, (dmn_cols) := tstrsplit(DMN_COLS, split = "[.]")][, DMN_COLS := NULL]
    
    box_id_details <- unique(data_dmn[, .(BOX_ID, ADP, HEX_ID, TIME)])
    dmn_nbr_dt <- box_id_details[dmn_nbr_dt, on = .(BOX_ID)]
    setcolorder(dmn_nbr_dt, c(year_col, stratum_cols, dmn_cols, "BOX_ID", "BOX_DMN_n", "BOX_DMN_w", "BOX_DMN_nbr"))
    
    # Calculate Number of trips in each STRATA x dmn_cols. Note that trips that have multiple 
    # 'dmn_cols' were split here!
    strata_dmn_N_dt <- dmn_nbr_dt[, .(STRATA_DMN_N = sum(BOX_DMN_w)), by = c(year_col, stratum_cols, dmn_cols)]
    
    # Double-check that weights sum to STRATA_N
    if(!fsetequal(
      strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)][STRATA_N != 0],
      strata_N_dt
    )) stop("STRATA_N and sum(BOX_DMN_w) are not equal!")
    
    box_res$dmn <- list()
    box_res$dmn$strata_dmn_n_dt <- strata_dmn_N_dt
    box_res$dmn$box_dmn_smry_dt <- dmn_nbr_dt
    box_res$dmn$strata_dt <- setorderv(unique(strata_N_dt[, ..stratum_cols]), cols = stratum_cols)[, STRATUM_ID := .I][]
    
  }
  
  # Create sf object with geometry if requested
  if(geom == T) {
    geom_sf <- merge(stat_area_lst$HEX_GEOMETRY, dt_out, on = .(HEX_ID))
    box_res$geom_sf <- geom_sf
    
    if(!is.null(dmn_cols)) {
      geom_dmn_sf <- merge(stat_area_lst$HEX_GEOMETRY, dmn_nbr_dt, on = .(HEX_ID))
      box_res$dmn$geom_dmn_df <- geom_dmn_sf
    }
  }
  
  # Return results
  box_res
  
}


define_boxes_3 <- function(data, space, time, year_col, stratum_cols, dmn_cols = NULL, stata_area_sf = stat_area_sf, geom = F){
  
  # data <- copy(fixed.val_2015_2023_dt[ADP == 2022]); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col = "ADP"; stratum_cols = c("STRATA", "BSAI_GOA"); dmn_cols <- "GEAR"; geom <- T
  
  
  #==================================#
  # NOTE! Remove any jig-gear trips! #
  #==================================# 
  # Jig-gear trips are generally in zero coverage (though sometimes HAL or EM_HAL trips might also fish
  # with jig gear in a trip?). Therefore, from an allocation perspective, it's not important if the rate will be 0.
  # Additionally, from an evaluation perspective, we don't use observed trips to make estimates for jig trips, so
  # we can remove them without issue. MAKE SURE 'data' HAS NO JIG GEAR COMPONENTS!
  # TODO Do I want to do this here? Current our domains/evaluation doesn't allow OB_HAL to cross with ZE_JIG, and 
  # can't if we remove all JIG trips at the very start.
  
  # Check for jig-gear in the dataset
  check_jig_cols <- unique(c(stratum_cols, dmn_cols))
  check_jig_cols <- apply(data[, ..check_jig_cols], 2, function(x) any(x == "JIG"))
  # Remove jig records
  if(any(check_jig_cols)) {
    jig_cols <- names(which(check_jig_cols))
    for(i in jig_cols) {
      jig_trips <- data[which(data[[jig_cols]] == "JIG"), ]
      cat(paste0("Removing ", nrow(jig_trips), " rows from ", uniqueN(jig_trips$TRIP_ID), " JIG trips from ", i, " column."), "\n")
      data <- fsetdiff(data, jig_trips)
    }
  }
  
  win <- as.integer(time[2])
  
  # Make sure integer TRIP_ID, integer ADFG_STAT_AREA_CODE, and 'dmn_cols' are specified in the data
  if( length( intersect(colnames(data), c("TRIP_ID", "ADFG_STAT_AREA_CODE", dmn_cols))) !=  length(c("TRIP_ID", "ADFG_STAT_AREA_CODE", dmn_cols)) ) {
    stop(paste0("'data' must have columns 'TRIP_ID' and 'ADFG_STAT_AREA_CODE and dmn_cols: ", paste(dmn_cols, collapse = ", ")))
  } else {
    if( all(lapply(data[, c("TRIP_ID", "ADFG_STAT_AREA_CODE")], class) != "integer")) {
      stop ("'TRIP_ID' and 'ADFG_STAT_AREA_CODE' must be of class integer.")
    }
  }
  if(nrow(unique(data[, .(TRIP_ID, STRATA)])) != uniqueN(data$TRIP_ID)) stop("A trip is not in only one stratum!")
  
  # Remove any unused columns
  keep_cols <- c(year_col, stratum_cols, dmn_cols, "ADFG_STAT_AREA_CODE", time[3], time[4], "TRIP_ID")
  data <- unique(subset(data, select = keep_cols))      
  
  #========================#
  # Convert ADFG to HEX_ID #
  #========================#
  
  stat_area_lst <- stat_area_to_hex(space[1], stat_area_sf)
  stat_area_dist_lst <- suppressWarnings(apply(
    X = round(st_distance(st_centroid(stat_area_lst$HEX_GEOMETRY))), 
    MARGIN = 1, 
    FUN = function(x) which(x <= space[2])))
  data <- unique(data.table(stat_area_lst$STAT_AREA_HEX_DF)[
  ][data, on = .(ADFG_STAT_AREA_CODE)
  ][, -"ADFG_STAT_AREA_CODE"])
  setcolorder(data, neworder = c(year_col, stratum_cols, dmn_cols, "HEX_ID", time[3], time[4]))
  setkeyv(data, cols = c(year_col, stratum_cols, dmn_cols, "HEX_ID", time[3], time[4]))
  
  if(nrow(data[is.na(HEX_ID)])) {
    print(data[is.na(HEX_ID)])
    stop("Could not assign a HEX_ID!")
  }
  
  #======================#
  # Define temporal unit #
  #======================#
  
  # First, get all years and a table converting date to week
  if(time[1] != "week") stop("So far this function only works with 'week()' function!")
  dates_lst <- lapply(
    lapply(
      unique(unlist(data[, ..year_col])),
      function(x) as.Date(paste0(x, c("-01-01", "-12-31")))
    ),
    function(x) as.Date(x[1] : x[2], origin = as.POSIXct("1970-01-01", tz = "UTC"))
  )
  dates_mtx <- cbind(unlist(dates_lst), unlist(lapply(dates_lst, get(time[1]))))
  dates_start <- min(dates_mtx[, 1]) - 1  # Get first date and subtract 1. T
  dates_mtx[, 1] <- dates_mtx[, 1] - dates_start  # This makes it so matrix can be reference by row index, much faster
  
  # Grab time columns and define groups based on TRIP_ID and HEX_ID
  time_cols <- time[3:4]
  time_int <- data[, ..time_cols]
  time_int[, (time_cols) := lapply(.SD, as.integer), .SDcols = time_cols]
  setnames(time_int, new = c("S", "E"))
  data_int <- data[, .(TRIP_ID, HEX_ID)][, GRP := .GRP, by = .(TRIP_ID, HEX_ID)]
  # Convert to matrix and split by TRIP_ID and HEX_ID
  time_lst <- as.matrix(cbind(time_int - dates_start, data_int[, .(GRP)]))
  time_lst <- lapply(split(time_lst, time_lst[, "GRP"], drop = F), matrix, ncol = 3)
  # For each TRIP_ID x HEX_ID, identify unique weeks
  time_lst <- lapply(time_lst, function(x) {
    dates_int <- unique(unlist(apply(x, 1, function(y) y[1] : y[2], simplify = F)))  # get unique days
    unique(dates_mtx[dates_int, 2, drop = F])                     # Identify week using dates_mtx
  }) 
  
  # Collapse into a data.table (this is much faster than rbindlist(lapply(data_lst, as.data.table)))
  data_dt <- as.data.table(
    do.call(rbind, Map(function(x1, x2) cbind(x1, x2), x1 = as.list(seq_along(time_lst)), x2 = time_lst)))
  setnames(data_dt, new = c("GRP", "TIME"))
  # Merge
  data_int <- unique(data_int)[data_dt, on = .(GRP)][, - "GRP"]
  keep_cols2 <- c(setdiff(keep_cols, c(time_cols, "ADFG_STAT_AREA_CODE")), "HEX_ID")
  data <- unique(data[, ..keep_cols2])[data_int, on = .(TRIP_ID, HEX_ID)]
  
  #============#
  # Define Box #
  #============#
  
  setkeyv(data, c(year_col, stratum_cols, "TIME", "HEX_ID"))
  setcolorder(data, c(year_col, stratum_cols, "TIME", "HEX_ID"))
  # 'BOX_ID' is defined by HEX_ID and TIME (including year_col), and is common to all data. This will be used to determine neighbors
  data[, BOX_ID := .GRP, by = c(year_col, "TIME", "HEX_ID")]
  setkey(data, BOX_ID)
  
  #==================#
  # Define Neighbors #
  #==================#
  
  # For each BOX_ID, find which BOX_IDs are neighboring based on week and spatial cells.
  sub_cols <- c(year_col, "TIME", "HEX_ID", "BOX_ID")
  st_mtx <- as.matrix(unique(data[, ..sub_cols]))
  nbr_lst <- apply(st_mtx, MARGIN = 1, function(x) {
    # x <- c(2022, 27, 106, 6047)
    box_mtx <- st_mtx[st_mtx[,1] == x[1], , drop = F]
    nbr_time <- (x[2] + (-win:win)) 
    box_mtx <- box_mtx[box_mtx[,2] %in% nbr_time, , drop = F] 
    nbr_hex_id <- stat_area_dist_lst[[x[3]]]
    box_mtx[box_mtx[, 3] %in% nbr_hex_id, 4]
  }) # 0.5 sec, but should save time when identifying number of neighbors?
  # each element of this list corresponds to BOX_ID and contains all neighboring BOX_IDs
  
  #===================================================#
  # Split up the dataset by year_col and stratum_cols #
  #===================================================#
  
  if(!is.null(dmn_cols)) {
    data_dmn <- copy(data)       # Create a copy that will keep dmn_cols
    data <- unique(data[, -..dmn_cols])  # Remove 'dmn_cols' for now
  }
  
  group_cols  <- c(year_col, stratum_cols)
  setkeyv(data, group_cols)
  data_lst <- lapply(
    X = split(x = subset(data, select = c(group_cols, "BOX_ID", "TRIP_ID")), by = group_cols, keep.by = F),
    FUN = as.matrix)
  
  # Make the frequency table of each TRIP_ID (so that trips are properly split by HEX_ID, TIME
  trip_id_mat <- do.call(rbind, lapply(
    data_lst,
    function(p) {
      trip_id_frq <- table(p[, "TRIP_ID"])
      matrix(
        c(as.integer(names(trip_id_frq)), trip_id_frq),
        ncol = 2, dimnames = list(NULL, c("TRIP_ID", "Freq")))
    }))
  trip_id_mat <- trip_id_mat[order(trip_id_mat[,1]), ]
  trip_id_vec <- vector(mode = "integer")
  trip_id_vec[trip_id_mat[, 1]] <- trip_id_mat[, 2]
  
  #==================================================#
  # Identify which TRIP_IDs are neighboring each box #
  #==================================================#
  
  box_smry <- lapply(
    data_lst,
    function(x) {
      # x <- data_lst[[5]] #is OB_FIXED.GOA as [1:BOX_ID] [2:TRIP_ID]
      # Get all unique time and space post-strata
      x1 <- unique(x[, 1])
      
      # Now for each BOX_ID listed in 'x1', count the number unique TRIP_IDs in neighboring BOX_IDs
      x2 <- do.call(rbind, lapply(x1, function(y) {
        # y <- x1[897]  # Corresponds to box 1358 in OB_FIXED.GOA, has two gear types
        
        trip_id_centered <- x[x[,1] == y, 2]
        # There shouldn't ever be the same trip counted twice in the same box.
        if( length(trip_id_centered) != length(unique(trip_id_centered))) stop("There is a duplicate trip_id!")
        
        cbind(
          BOX_ID = y,
          # Count of trips centered in BOX_ID
          BOX_n = length(trip_id_centered) ,
          # Weight of trips centered in BOX_ID
          # FIXME calculating BOX_w is slow!
          BOX_w = sum(1/trip_id_vec[trip_id_centered]), 
          # # Count of unique TRIP_IDs in neighboring BOX_IDs
          BOX_nbr = length(unique(x[(x[,1] %in% nbr_lst[[y]]), 2]))
        )
      }))
    }
  ) 
  
  # Calculate STRATA_N and ensure the sum of weights is equivalent
  strata_N_dt <- data[, .(STRATA_N = uniqueN(TRIP_ID)), keyby = group_cols]
  
  # Double-check that weights sum to STRATA_N
  if(!(all(
    unname(sapply(box_smry, function(x) sum(x[,"BOX_w"]))) == strata_N_dt $STRATA_N
  ))) stop("STRATA_N and sum(BOX_w) are not equal!")
  
  # Create data.table output
  st_dt <- unique(data[, .(BOX_ID, HEX_ID, TIME)])   # Table of BOX_ID, HEX_ID, and TIME
  dt_out <- setcolorder(
    rbindlist(lapply(box_smry, as.data.table), idcol = "GROUP_COLS")[
    ][, (group_cols) := tstrsplit(GROUP_COLS, split = "[.]")
    ][, GROUP_COLS := NULL],
    c(group_cols, "BOX_ID", "BOX_n", "BOX_w", "BOX_nbr"))[
    ][st_dt, on = .(BOX_ID)]
  dt_out[, which(colnames(dt_out) == year_col) := as.integer(dt_out$ADP)]
  
  # Initialize outputs
  box_res <- list(
    box_smry = box_smry,
    strata_n_dt = strata_N_dt,
    dt_out = dt_out,
    og_data = data,
    nbr_lst = nbr_lst,
    params = list(stratum_cols = stratum_cols, year_col = year_col, dmn_cols = dmn_cols)
  )
  
  
  if(!is.null(dmn_cols)) {
    
    # Get trip weights, splitting by dmn_cols as well
    trip_id_dmn_mat <- as.matrix(data_dmn[, .N, by = .(TRIP_ID)])  # For each TRIP_ID, count number of instances (split by group_cols and dmn_cols and BOX_ID)
    trip_id_dmn_vec <- vector(mode = "integer")
    trip_id_dmn_vec[trip_id_dmn_mat[, 1]] <- trip_id_dmn_mat[, 2]  # Have each index be TRIP_ID and each value be the trip's domain weight
    
    # Identify all BOX_IDs within a dmn_tbl group
    
    # in both time and space. Essentially, any spatial or temporal post-strata should be excluded from this list.
    
    dmn_tbl <- setorderv(unique(data_dmn[, ..dmn_cols]), dmn_cols)
    dmn_lst <- vector(mode = "list", length = nrow(dmn_tbl))
    for(i in 1:nrow(dmn_tbl)) {
      # i <- 1  # BSAI x HAL
      # i <- 4  #  GOA x HAL
      
      # Subset the data by domain
      focus_dmn_dat <- unique(subset(data_dmn[dmn_tbl[i,], on = dmn_cols], select = c(stratum_cols, "BOX_ID", "TRIP_ID")))                 # this only has trips within the domain
      if(F) focus_dmn_dat[, sum(1/trip_id_dmn_vec[TRIP_ID])]  # 19670.29 trips in all strata with HAL, 5120.713 in POT,  7221 in TRW
      # Split the data by stratum
      focus_dmn_dat_stratum <- lapply(split(focus_dmn_dat, by = stratum_cols, keep.by = F), as.matrix)
      if(F) sapply(focus_dmn_dat_stratum, function(x) sum(1 / trip_id_dmn_vec[x[, "TRIP_ID"]]))
      
      # Make a list of non-spatial domain columns. Although we might define "BSAI_GOA" as a domain, BOX_ID is used to identify all neighbors.
      # This is so that within a domain that may be defined by space (BSAI_GOA), we can still look outside that domain for spatial neighbors 
      non_spatial_dmn_cols <- setdiff(dmn_cols, c("BSAI_GOA", "BS_AI_GOA"))
      non_spatial_dmn_dat <- unique(subset(data_dmn[dmn_tbl[i, ..non_spatial_dmn_cols], on = non_spatial_dmn_cols], select = c(stratum_cols, "BOX_ID", "TRIP_ID")))
      # Also split it by stratum
      non_spatial_dmn_dat_stratum <- lapply(split(non_spatial_dmn_dat, by = stratum_cols, keep.by = F), as.matrix)
      non_spatial_dmn_dat_stratum <- non_spatial_dmn_dat_stratum[names(focus_dmn_dat_stratum)]   # Make sure the strata names are in the same order
      
      # Get all BOX_IDs in the domain (exclude boxes without any trips)
      focus_dmn_box_ids <- unique(focus_dmn_dat$BOX_ID)
      
      if(!all(names(focus_dmn_dat_stratum) == names(non_spatial_dmn_dat_stratum))) stop("Names of focus_dmn_dat_stratum aren't the same as non_spatial_dmn_dat_stratum!")
      
      # For each stratum...
      focus_dmn_nbr_lst <- mapply(
        function(x1, x2) {
          # x1 <- focus_dmn_dat_stratum[["OB_HAL"]]
          # x2 <- non_spatial_dmn_dat_stratum[["OB_HAL"]]
          
          # x1 <- focus_dmn_dat_stratum[["EM_HAL"]]
          # x2 <- non_spatial_dmn_dat_stratum[["EM_HAL"]]
          
          # for each BOX_ID
          x1.1 <- lapply(focus_dmn_box_ids, function(y) {
            # y <- focus_dmn_box_ids[[1]]
            
            trip_id_centered <- unique(x1[x1[,1] == y, 2])
            
            c(
              BOX_ID = y,
              BOX_DMN_n = length(trip_id_centered),
              BOX_DMN_w = sum(1/trip_id_dmn_vec[trip_id_centered]),
              BOX_DMN_nbr = length(unique(x2[x2[,1] %in% nbr_lst[[y]], 2]))   # Compile all trips in neighborhood, count number of unique TRIP_IDs in neighborhood
            )
            
          })
          
          as.data.table(do.call(rbind, x1.1))
        }, 
        x1 = focus_dmn_dat_stratum, 
        x2 = non_spatial_dmn_dat_stratum,
        SIMPLIFY = F
      )
      
      focus_dmn_nbr_lst <- rbindlist(focus_dmn_nbr_lst, idcol = "STRATUM_COLS")
      focus_dmn_nbr_lst[, (stratum_cols) := tstrsplit(STRATUM_COLS, split = "[.]")][, STRATUM_COLS := NULL]
      dmn_lst[[i]] <- focus_dmn_nbr_lst
    }
    
    names(dmn_lst) <- apply(dmn_tbl, 1, paste0, collapse = ".")
    dmn_nbr_dt <- rbindlist(dmn_lst, idcol = "DMN_COLS")
    dmn_nbr_dt[, (dmn_cols) := tstrsplit(DMN_COLS, split = "[.]")][, DMN_COLS := NULL]
    
    box_id_details <- unique(data_dmn[, .(BOX_ID, ADP, HEX_ID, TIME)])
    dmn_nbr_dt <- box_id_details[dmn_nbr_dt, on = .(BOX_ID)]
    setcolorder(dmn_nbr_dt, c(year_col, stratum_cols, dmn_cols, "BOX_ID", "BOX_DMN_n", "BOX_DMN_w", "BOX_DMN_nbr"))
    
    # Calculate Number of trips in each STRATA x dmn_cols. Note that trips that have multiple 
    # 'dmn_cols' were split here!
    strata_dmn_N_dt <- dmn_nbr_dt[, .(STRATA_DMN_N = sum(BOX_DMN_w)), by = c(year_col, stratum_cols, dmn_cols)]
    
    # Double-check that all weights in strata_N_dt are also in strata_dmn_N_dt. 
    if(!fsetequal(
      strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)][STRATA_N != 0],
      strata_N_dt
    )) stop("STRATA_N and sum(BOX_DMN_w) are not equal!")
    
    if (F) {
      
      # As long as I remove all jig gear trips, total trip counts match up. 
      strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)][, sum(STRATA_N)]
      strata_N_dt[, sum(STRATA_N)] 
      data[, uniqueN(TRIP_ID)]      
      
      fsetdiff(
        strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)],
        strata_N_dt
      )
      fsetdiff(
        strata_N_dt,
        strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)]
      )
      
      a <- strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)]
      a[ADP == 2022 & STRATA == "OB_MIXED"]           # 12 and 218
      strata_N_dt[ADP == 2022 & STRATA == "OB_MIXED"] # 11 and 218: Does a different stratum make up the 1 trip difference?
      
      table(strata_dmn_N_dt$STRATA_DMN_N)  # Are domains all intergers? seems strange to me...
      
      
      
      
      strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = .(year_col, stratum_cols)]
      
      
      
      
      # 2022 BSAI ZERO are 190 and 191, BSAI-OB_MIXED are 11 and 12
      strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)][ADP == 2021 & STRATA == "ZERO"]
      # These are quite far off
      strata_N_dt[ADP == 2021 & STRATA == "ZERO"]
      dmn_cols  # GEAR is used here. 
      
    }
    
    
    
    box_res$dmn <- list()
    box_res$dmn$strata_dmn_n_dt <- strata_dmn_N_dt
    box_res$dmn$box_dmn_smry_dt <- dmn_nbr_dt
    box_res$dmn$strata_dt <- setorderv(unique(strata_N_dt[, ..stratum_cols]), cols = stratum_cols)[, STRATUM_ID := .I][]
    
  }
  
  # Create sf object with geometry if requested
  if(geom == T) {
    geom_sf <- merge(stat_area_lst$HEX_GEOMETRY, dt_out, on = .(HEX_ID))
    box_res$geom_sf <- geom_sf
    
    if(!is.null(dmn_cols)) {
      geom_dmn_sf <- merge(stat_area_lst$HEX_GEOMETRY, dmn_nbr_dt, on = .(HEX_ID))
      box_res$dmn$geom_dmn_df <- geom_dmn_sf
    }
  }
  
  # Return results
  box_res
  
} 


calculate_interspersion_gs <- function(box_res, sample_rate_vec, omit_strata = c(NULL)) {
  # box_res <- copy(fixed_fmp.box); omit_strata <- NULL
  
  group_cols <- c(box_res$params$year_col, box_res$params$stratum_cols)
  ps_cols <- box_res$params$ps_cols
  year_col <- box_res$params$year_col
  
  # Use 'omit_strata' to omit any unmonitored strata
  if(!is.null(omit_strata)) {
    keep_strata <- !apply(sapply(omit_strata, function(x) names(box_res$box_smry) %like% x), 1, any)
  } else {
    keep_strata <- rep(T, times = length(box_res$box_smry))
  }
  
  # For for a range of sample rates, calculate the probably that a post-stratum would be near a sampled neighbor
  # (0-1), and then multiply it by that post-stratum's total weight of component trips centered on the post-stratum.
  
  # For each sample rate...
  ispn_lst <- lapply(
    sample_rate_vec,
    function(x) {
      # x <- 0.15
      
      # For each stratum...
      sapply(
        box_res$box_smry[keep_strata],
        function(y) {
          # y <- box_res$box_smry[keep_strata][[1]]
          
          a <- do.call(rbind, y)
          sum((1 - ((1 - x)^a[,4])) * a[,3])   # Just doing the sum across is the same as the weighted average!
          # FIXME I don't need to split post-strata into additional lists unless I want to weight post-strata separately!
          # I think that within a stratum, we want to weight by each trip's interspersion.
          # We don't want to weight interspersion of HAL and POT post-strata equally regardless of how many trips are in each.
          
        }
      )
    }
  )
  
  # Package the results, converting to data.table
  ispn_lst <- lapply(ispn_lst, function(z)  data.frame(GROUP_COLS = names(z), sum_pw = z))
  names(ispn_lst) <- sample_rate_vec
  ispn_dt <- rbindlist(ispn_lst, idcol = "SAMPLE_RATE")[
  ][, SAMPLE_RATE := as.numeric(SAMPLE_RATE)
  ][, (group_cols) := tstrsplit(GROUP_COLS, split = "[.]")
  ][, GROUP_COLS := NULL]
  ispn_dt[, (year_col) := lapply(.SD, as.integer), .SDcol = year_col]
  
  ispn_dt <- box_res$strata_n_dt[
  ][ispn_dt, on = group_cols
  ][, ISPN := sum_pw / STRATA_N][]
  
  setcolorder(ispn_dt, c("SAMPLE_RATE", group_cols, "ISPN"))
  
  ispn_res <- list(
    ispn_dt = ispn_dt,
    strata_n_dt = box_res$strata_n_dt,
    params = box_res$params
  )
  
  ispn_res
  
}



calculate_interspersion <- function(box_res, sample_rate_vec, omit_strata = c(NULL)) {
  # omit_strata <- NULL
  # omit_strata <- "ZERO"
  # x <- 0.15; y <- box_res$box_smry[[1]]
  
  group_cols <- c(box_res$params$year_col, box_res$params$stratum_cols)
  year_col <- box_res$params$year_col
  
  # Use 'omit_strata' to omit any unmonitored strata
  if(!is.null(omit_strata)) {
    keep_strata <- !apply(sapply(omit_strata, function(x) names(box_res$box_smry) %like% x), 1, any)
  } else {
    keep_strata <- rep(T, times = length(box_res$box_smry))
  }
  
  # For for a range of sample rates, calculate the probably that a post-stratum would be near a sampled neighbor
  # (0-1), and then multiply it by that post-stratum's total weight of component trips centered on the post-stratum.
  
  # For each sample rate...
  ispn_lst <- lapply(
    sample_rate_vec,
    function(x) {
      
      # For each stratum...
      sapply(
        box_res$box_smry[keep_strata],
        function(y) {
          
          # For each stratum's BOX_ID, use BOX_nbr to calculate the probability that the box is sampled,
          # and then multiply that by BOX_w to get the expected number of sampled trips in the box. Sum across
          # all boxes to get expected number of sampled trips in stratum.
          # x is the sample rate, y[,4] is 'BOX_nbr' and y[, 3] is 'BOX_w'. Referencing by column is faster.
          
          sum((1 - ((1 - x)^y[,4])) * y[,3])
          # sum((1 - ((1 - x)^y[,"BOX_nbr"])) * y[,"BOX_w"])
          
        }
      )
    }
  )
  
  # Package the results, converting to data.table
  ispn_lst <- lapply(ispn_lst, function(z)  data.frame(GROUP_COLS = names(z), sum_pw = z))
  names(ispn_lst) <- sample_rate_vec
  ispn_dt <- rbindlist(ispn_lst, idcol = "SAMPLE_RATE")[
  ][, SAMPLE_RATE := as.numeric(SAMPLE_RATE)
  ][, (group_cols) := tstrsplit(GROUP_COLS, split = "[.]")
  ][, GROUP_COLS := NULL]
  ispn_dt[, (year_col) := lapply(.SD, as.integer), .SDcol = year_col]
  
  ispn_dt <- box_res$strata_n_dt[
  ][ispn_dt, on = group_cols
  ][, ISPN := sum_pw / STRATA_N][]
  
  setcolorder(ispn_dt, c("SAMPLE_RATE", group_cols, "ISPN"))
  
  ispn_res <- list(
    ispn_dt = ispn_dt,
    strata_n_dt = box_res$strata_n_dt,
    params = box_res$params
  )
  
  ispn_res
  
}


#======================================================================================================================#
# RESTARTING ----
#======================================================================================================================#






val_2015_2023_dt <- pc_effort_dt[ADP %in% 2015:2023]
val_2015_2023_dt[BSAI_GOA %in% c("BS", "AI"), BSAI_GOA := "BSAI"]
val_2015_2023_dt <- unique(val_2015_2023_dt)

sample_rate_vec <- seq(from = 0.0001, to = 0.9950, by = 0.0001)
sample_rate_15 <- 0.15

#===============================#
## Prepare trips_melt object ----
#===============================#

# trips_melt has metrics and trip duration for all trips >= 2015. . 
# Add ADP year, STRATA, and DAYS of each TRIP_ID to trips_melt
trips_melt_add_data <- unique(pc_effort_dt[STRATA != "ZERO", .(ADP, TRIP_ID, STRATA, DAYS)])
trips_melt <- trips_melt_add_data[trips_melt, on = .(TRIP_ID)]
setcolorder(trips_melt, c("ADP", "STRATA", "TRIP_ID", "DAYS", "Metric", "Value"))
setkey(trips_melt, ADP, STRATA, TRIP_ID, DAYS, Metric, Value)
# Error-check
if(nrow(trips_melt[, .N, by = .(TRIP_ID)][N != uniqueN(trips_melt$Metric)])) stop("At least one trip has number of metrics != 3!")
# Remove records missing data
trips_melt <- trips_melt[!is.na(ADP)]


#======================================#
## Prepare fixed_fmp fishing effort ----
#======================================#

# Combine HAL and POT stratum trips into FIXED strata
fixed.val_2015_2023_dt <- unique(copy(val_2015_2023_dt)[STRATA %like% "HAL|POT", STRATA := paste0(POOL, "_", "FIXED")])

# Define Boxes
fixed_fmp.box <- define_boxes_gs(
  fixed.val_2015_2023_dt, space = c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), dmn_cols = c("GEAR"), geom = T, ps_cols = c("GEAR"))
# Recalculate trip durations of each stratum
fixed_fmp.allo_lst <- update_strata(fixed.val_2015_2023_dt, trips_melt, stratum_cols = c("STRATA", "BSAI_GOA"), focus_years = 2015:2023)

# Calculate monitoring costs

cost_params <- list(
  
  OB = list(
    day_rate_intercept          = 1870.4466666667,    # To calculate the sea day cost
    day_rate_slope              =   -0.2263733333,    # To calculate the sea day cost
    travel_day_rate             =  423.7867560407     # Expected cost of travel per day
  ),
  
  EMFG = list(
    emfg_v                      =  172,               # Size of fixed-gear EM vessel pool
    cost_per_vessel             = 5679.9002264045,
    cost_per_review_day         =  150.3237858616
  ),
  
  EMTRW = list(
    emtrw_goa_v                 =   39,               # Number of EM_TRW vessels that fish exclusively in the GOA. Current count is 39 for 2024.
    trip_to_plant_factor        =    3.8059361492,    # Used to predict plant days from trips
    amortized_equipment_per_VY  = 4100.7124800000,    # Per (Vessel x Year) amortized EM equipment install costs for GOA-only vessels
    equipment_upkeep_per_VY     = 4746.0398955882,    # Per (Vessel x Year) EM equipment maintenance cost for GOA-only vessels
    review_day_rate             =   27.9918948996,    # Per sea day cost for EM compliance review
    plant_day_rate              =  908.2225000000     # Per plant day cost for shoreside monitoring by observers
  )
)

budget_lst <- list(5819000)
max_budget <- 7.00e6                        # This is used as a cutoff for allocation functions.

# Estimate monitoring costs from method-specific cost models for a wide range of sample rates. This table will be used
# by all designs as it is design-agnostic. Takes 2-3 minutes to run.
#system.time(cost_dt <- rates_to_costs_all(fixed_fmp.allo_lst, sample_rate_vec, cost_params, max_budget))
# FIXME : the ob_cost() model should be changed to logistic so that CPD doesn't decline infinitely with volume of days.
# For now, just exclude any instances where the OB_TOTAL decreases relative to the previous instance
#cost_dt <- rbindlist(lapply(split(cost_dt, by = "ADP"), function(x) x[(which(x[-.N, OB_TOTAL] < x[-1, OB_TOTAL]))]))


#=============================================#
# Calculate Proximity at a 15% sample rate ----
#=============================================#

# SKIP down to Allocation using random sampling with replacement, and define allo_prox() function


fixed_fmp.prox <- calculate_interspersion_gs(fixed_fmp.box, sample_rate_15)

# create a new column named proximity (the updated name of this index, where interspersion is saved for evaluation)
fixed_fmp.prox$ispn_dt[, PROX := ISPN]
# Calculate relationship of proximity index / log(stratum size), which overall linearizes the relationship.
fixed_fmp.prox$ispn_dt[, `PROX/logSTRATA_N` := PROX / log(STRATA_N) ]
# Melt the dataset so we can facet on metrics
fixed_fmp.plot_data <- melt(fixed_fmp.prox$ispn_dt, id.vars = c("ADP", "STRATA", "BSAI_GOA"), measure.vars = c("PROX", "STRATA_N", "PROX/logSTRATA_N"))

fixed_fmp.prox$ispn_dt[ADP %in% c(2021, 2022, 2023), LABEL := ADP]

# [NOTE] THAT THIS IS NOT INTERSPERSION, IT'S THE 'PROXIMITY INDEX'. Therefore, fixed-strata (both OB and EM in BSAI and GOA) would have separate
# gear-specific interspersion values
prox3 <- ggplot(fixed_fmp.plot_data, aes(x = ADP, y = value)) + 
  facet_nested(variable ~ STRATA + BSAI_GOA, scales = "free_y") + 
  geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x = "Year", y = "Value")
# Overall, year to year, interspersion changes little for most strata, especially in the GOA. BSAI is much more sensitive
# due to the low number of trips fishing there.
# PROX/log(STRATA_N) tells me how much 'clumpiness' changes, 'correcting' for the size of the stratum. Increasing trends indicate 
# Ex, if I had the same PROX values over two years, where effort differed, 100 trips vs 500 trips, I would see a decreasing PROX/log(STRATA_N),
# meaning that fishing effort would have to have been larger but more diffuse.
# Ex., OB_TRW GOA, PROX and STRATA_N both decline over the time series (Pollock trips stayed constant but fishing for COD and OTHER declined
# between 2015 and 2020), but PROX/log(STRATA_N) is increasing, meaning that overall remaining fishing effort must have become more clumped (less diffuse).

# Relating proximity to STRATA_N
# overall, Interspersion increases with STRATA_N in a logarithmic (or square root) relationship
prox1 <- ggplot(fixed_fmp.prox$ispn_dt, aes(x = STRATA_N, y = PROX, color = STRATA, shape = BSAI_GOA, linetype = BSAI_GOA)) + geom_point(size = 3, stroke = 1)  + 
  geom_path(aes(group = interaction(STRATA, BSAI_GOA))) + 
  scale_shape_manual(values = c(BSAI = 16, GOA = 2)) + 
  labs(x = "# Trips in Stratum", y = "Proximity Index", shape = "FMP", linetype = "FMP", color = "Stratum") + 
  geom_text(aes(label = LABEL), size = 3, color = "black")

prox2 <- ggplot(fixed_fmp.prox$ispn_dt, aes(x = log(STRATA_N), y = PROX, color = STRATA, shape = BSAI_GOA, linetype = BSAI_GOA)) + geom_point(size = 3, stroke = 1)  + 
  geom_path(aes(group = interaction(STRATA, BSAI_GOA))) + 
  scale_shape_manual(values = c(BSAI = 16, GOA = 2)) + 
  labs(x = "log(# Trips in Stratum)", y = "Proximity Index", shape = "FMP", linetype = "FMP", color = "Stratum") + 
  geom_text(aes(label = LABEL), size = 3, color = "black")

# Square-root doesn't quite linearize the relationship
ggplot(fixed_fmp.prox$ispn_dt, aes(x = sqrt(STRATA_N), y = PROX, color = STRATA, shape = BSAI_GOA, linetype = BSAI_GOA)) + geom_point(size = 3, stroke = 1)  + 
  geom_path(aes(group = interaction(STRATA, BSAI_GOA))) + 
  scale_shape_manual(values = c(BSAI = 16, GOA = 2)) + 
  labs(x = "sqrt(# Trips in Stratum)", y = "Proximity Index", shape = "FMP", linetype = "FMP", color = "Stratum")

# Changes over time
# Interestingly, ISPN goes up faster in the BSAI than the GOA as STRATA_N increases. This tells me that fishing is generally more clumped in time and space
# in the BSAI than in the GOA (which admittedly is much larger in space)
ggplot(fixed_fmp.prox$ispn_dt, aes(x = STRATA_N, y = ISPN, color = STRATA, shape = BSAI_GOA, linetype = BSAI_GOA)) + geom_point(size = 3, stroke = 1) + scale_shape_manual(values = c(BSAI = 16, GOA = 2)) + 
  facet_grid(BSAI_GOA ~ STRATA, scales = "free") + geom_path(aes(group = interaction(STRATA, BSAI_GOA))) + 
  labs(x = "# Trips in Stratum", y = "Proximity Index", shape = "FMP", linetype = "FMP", color = "Stratum")
# Here we can see how the values changed year to year, and whether a trend exists
prox4 <- ggplot(fixed_fmp.prox$ispn_dt, aes(x = STRATA_N, y = ISPN, color = ADP, shape = BSAI_GOA)) + geom_point(size = 3, stroke = 1) + scale_shape_manual(values = c(BSAI = 16, GOA = 2)) + 
  facet_grid(BSAI_GOA ~ STRATA, scales = "free") + scale_color_viridis_c(direction = -1)  + geom_path(aes(group = interaction(STRATA, BSAI_GOA))) + 
  labs(x = "# Trips in Stratum", y = "Proximity Index", shape = "FMP", color = "Year")

#======================================================================================================================#
# INTERSPERSION ----
#======================================================================================================================#

# For interspersion, again assume 15% sample rate

# With OB as only donors (including TRW_EM to itself as it is observer-based)
fixed_fmp.box$dmn$strata_dt
acceptor_donor_lst <-  c(
  rep(list(1:2), times = 2),                # 1-2: EM_FIXED to itself (BSAI + GOA)
  list(3),                                  # 3:   EM_TRW donates to itself
  rep(list(4:5), times = 2),                # 4-5: OB_FIXED to itself (BSAI + GOA)
  rep(list(6:7), times = 2),                # 6-7: OB_TRW to itself (BSAI + GOA)
  rep(list(8:9), times = 2)                 # 8-9:   Zero to itself (BSAI + GOA)
)

fixed_fmp.insp <- calculate_dmn_interspersion4(box_def = fixed_fmp.box, selection_rates = fixed_fmp.prox$ispn_dt, acceptor_donor_lst = acceptor_donor_lst)

# Are these the same or not?
fixed_fmp.insp_smry <- dmn_interspersion_smry(fixed_fmp.insp)


insp1 <- ggplot(fixed_fmp.insp_smry$BSAI_GOA, aes(x = ADP, y = POOL_DMN_INTERSPERSION, color = BOX_DMN_w)) + 
  facet_grid(BSAI_GOA ~ POOL + GEAR) + 
  geom_point(size = 2) + 
  geom_path(size = 2) + 
  scale_color_viridis_c(trans = "log")  + 
  labs(x = "Year", y = "INSP/log(# Trips)", color = "log(# Trips)") +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


# here are actual interspersion values, separated by BSAI/GOA and gear type (HAL separate from POT)
insp_fmp <- copy(fixed_fmp.insp_smry$BSAI_GOA)
insp_fmp[, `INSP/log(BOX_DMN_w)` := POOL_DMN_INTERSPERSION / BOX_DMN_w]
insp2 <- ggplot(insp_fmp, aes(x = ADP, y = `INSP/log(BOX_DMN_w)`, color = BOX_DMN_w)) + 
  facet_grid(BSAI_GOA ~ POOL + GEAR) + 
  geom_point(size = 2) + 
  geom_path(size = 2) + 
  scale_color_viridis_c(trans = "log") + 
  labs(x = "Year", y = "INSP/log(# Trips)", color = "log(# Trips)")+  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# Overall all sampled strata are very consistent year to year, but OB_TRW-BSAI can vary a lot, especially with such few trips.

# If we omit zero, can see other groups's trends a little more clearly.
insp3 <- ggplot(insp_fmp[POOL != "ZERO"], aes(x = ADP, y = `INSP/log(BOX_DMN_w)`, color = BOX_DMN_w)) + 
  facet_grid(BSAI_GOA ~ POOL + GEAR, scales = "free") + 
  geom_point(size = 2) + 
  geom_path(size = 2) + 
  scale_color_viridis_c(trans = "log") + 
  labs(x = "Year", y = "INSP/log(# Trips)", color = "log(# Trips)")+  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))






dmn_interspersion_smry(fixed_fmp.insp)$BSAI_GOA[GEAR == "TRW" & BSAI_GOA == "GOA" & POOL == "OB"]
fixed_fmp.prox$ispn_dt[STRATA == "OB_TRW" & BSAI_GOA == "GOA"]
# Not exact... not sure if the small differences are due to some crossover trips?

dmn_interspersion_smry(fixed_fmp.insp)$BSAI_GOA[GEAR == "TRW" & BSAI_GOA == "BSAI" & POOL == "OB"]
fixed_fmp.prox$ispn_dt[STRATA == "OB_TRW" & BSAI_GOA == "BSAI"]
# In the BSAI, PROX and Interspersion can be a lot more different. 2017 has 0.923 in interspersion but prox is only 0.721???
# Must be a lot of overlap between these HEX_IDs?
dmn_interspersion_smry(fixed_fmp.insp)$BSAI_GOA[GEAR == "TRW" & BSAI_GOA == "BSAI" & POOL == "OB"]$POOL_DMN_INTERSPERSION - 
  fixed_fmp.prox$ispn_dt[STRATA == "OB_TRW" & BSAI_GOA == "BSAI", PROX]
plot(dmn_interspersion_smry(fixed_fmp.insp)$BSAI_GOA[GEAR == "TRW" & BSAI_GOA == "BSAI" & POOL == "OB"]$POOL_DMN_INTERSPERSION, ylim = c(0.4, 1), type = "l")
lines(fixed_fmp.prox$ispn_dt[STRATA == "OB_TRW" & BSAI_GOA == "BSAI", PROX], col= "red")
# Interspersion values are less jagged. Interspersion is higher because of some interaction with GOA trips.
test_plots <- dmn_interspersion_plot(fixed_fmp.insp)

# Adding NMFS areas is super slow, but seeing BSAI/GOa boundary would be nice, and keep extent limited to hex_ids, not 
# all NMFs areas
# test_plots$PLOTS$TRW[["2022"]] + geom_sf(data = shp_nmfs %>% st_set_crs(st_crs(fixed_fmp.insp$geom)))
test_plots$PLOTS$TRW[["2022"]]

# how about OB_FG?
fixed_fmp.insp_smry$BSAI_GOA[BSAI_GOA == "BSAI" & POOL == "OB" & GEAR %in% c("HAL", "POT")][order(ADP)]
fixed_fmp.prox$ispn_dt[STRATA == "OB_FIXED" & BSAI_GOA == "BSAI"]

# EM_TRW Matches perfectly
dmn_interspersion_smry(fixed_fmp.insp)$BSAI_GOA[GEAR == "TRW" & BSAI_GOA == "GOA" & POOL == "EM"]
fixed_fmp.prox$ispn_dt[STRATA == "EM_TRW" & BSAI_GOA == "GOA"]

#======================================================================================================================#
# Bootstrapping Methods ----
#======================================================================================================================#

# USE THE LOOP BELOW THAT DOES BOTH PROXIMITY AND ALLOCATION

# Does proximity change a lot when bootstrapping with replacement? What happens if we sample the same
# number of trips each year, just with replacement?

# Split each by year and stratum
fixed.year_lst <- split(fixed.val_2015_2023_dt[order(ADP)], by = c("ADP", "STRATA", "BSAI_GOA"))

# Define number of bootstrap iterations
bootstrap_iter <- 100
bootstrap_lst <- vector(mode = "list", length = bootstrap_iter)

set.seed(12345)
for(i in seq_len(bootstrap_iter)) {
  
  # Create bootstrapped fishing effort, sampling with replacement the same number of trips
  bootstrap.effort <- rbindlist(lapply(fixed.year_lst, function(x) {
    # Sample trips with replacement
    trip_ids <- unique(x$TRIP_ID)
    sampled_trip_ids <- data.table(TRIP_ID = sample(trip_ids, size = length(trip_ids), replace = T))
    sampled_trip_ids[, I := .I]
    bootstrap_sample <- x[sampled_trip_ids, on = .(TRIP_ID), allow.cartesian = T]
  }))
  # Re-assign trip_id so that we can differentiate trips sampled multiple times
  bootstrap.effort[, TRIP_ID := .GRP, keyby = .(ADP, STRATA, BSAI_GOA, TRIP_ID, I)]
  if(uniqueN(bootstrap.effort$TRIP_ID) != uniqueN(fixed.val_2015_2023_dt$TRIP_ID)) stop("Count of TRIP_IDs doesn't match!")
  
  # Define boxes of bootstrapped effort
  bootstrap.box <- define_boxes_gs(
    bootstrap.effort, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
    year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), geom = F, ps_cols = c("GEAR"))
  
  # Calculate proximity at 15% sample rate (function is still called interspersion)
  bootstrap.prox <- calculate_interspersion_gs(bootstrap.box, sample_rate_vec = sample_rate_15)
  bootstrap_lst[[i]] <- bootstrap.prox$ispn_dt
}
bootstrap_results_dt <- rbindlist(bootstrap_lst, idcol = "Bootstrap Iter")

bootstrap_1 <- ggplot(bootstrap_results_dt, aes(x = as.character(ADP), y = ISPN)) +
  facet_grid(BSAI_GOA ~ STRATA) + 
  geom_violin(color = "red", draw_quantiles = 0.5) +
  stat_summary(geom = "point", color = "red", fun = mean, shape = 4) + 
  geom_point(data = fixed_fmp.prox$ispn_dt, shape = 1, fill = NA, stroke = 1) + 
  geom_text(data = fixed_fmp.prox$ispn_dt, aes(label = STRATA_N), size = 3, na.rm = T, nudge_y = -0.05, angle = 90) + 
  labs(x = "Year", y = "Proximity Index")

bootstrap_2 <- ggplot(bootstrap_results_dt, aes(x = as.character(ADP), y = ISPN)) +
  facet_wrap(BSAI_GOA ~ STRATA, scales = "free") + 
  geom_violin(color = "red", draw_quantiles = 0.5) +
  stat_summary(geom = "point", color = "red", fun = mean, shape = 4) + 
  geom_point(data = fixed_fmp.prox$ispn_dt, shape = 1, fill = NA, stroke = 1) + 
  labs(x = "Year", y = "Proximity Index")
# Good news is that the distributions seem to be generally normally distributed and not skewed. 

# What are the relative changes to interspersion compared to original?
dcast(
  rbind(
    cbind("Type" = "Bootstrap", bootstrap_results_dt),
    cbind("Type" = "Original", fixed_fmp.prox$ispn_dt),
    fill = T
  )[, .(
    `Perc Diff vs OG` = round(100 * ((mean(ISPN[Type == "Bootstrap"]) / ISPN[Type == "Original"]) - 1), 3)
    ), by = .(ADP, STRATA, BSAI_GOA)],
  BSAI_GOA + STRATA ~ ADP, value.var = "Perc Diff vs OG")
# Like the figures show, it indicates that bootstrapping using sampling with replacement will result in a positive
# bias of proximity indices, especially in the BSI_GOA where the bias can be between +3 and +9 percent. Biases in the GOA
# are generally low, typically well within +1 percent.

# TODO What if we sample without replacement, and repeat this if we need to sample more trips than prior year?
# TODO What happens if use the prior 3 years to sample without replacemnt from each year? Might come with its own issues with changes in trends, and
# would likely warrant using higher sampling probabilities from more recent years.

# TODO Since we didn't actually allocate, it's hard to say exactly how much the rates would change year to year with 
# a specific budget. However, given that it looks like low effort strata in the BSAI generally have much higher bootstrapped
# proximity, meaning those strata would get allocated to relatively less compared to the original. 

#======================================================================================================================#
# How does sample rate affect proximity ----
#======================================================================================================================#

fixed_fmp.prox_range <- calculate_interspersion_gs(fixed_fmp.box, sample_rate_vec = seq(0.05, 0.25, by = 0.05))
# Increasing sample rate just drags values up towards 1
prox_by_rate_1 <- ggplot(fixed_fmp.prox_range$ispn_dt, aes(x = STRATA_N, y = ISPN, color = STRATA, shape = BSAI_GOA, linetype = BSAI_GOA)) + 
  facet_grid(. ~ SAMPLE_RATE) + 
  geom_point() + geom_path() + labs(x = "# Trips", y = "Proximity", shape = "FMP", linetype = "FMP", color = "Strata") + 
  scale_shape_manual(values = c(BSAI = 16, GOA = 2)) 
# If we allow the y-axes to vary, we see little changes in the overall shape
prox_by_rate_2 <- ggplot(fixed_fmp.prox_range$ispn_dt, aes(x = STRATA_N, y = ISPN, color = STRATA, shape = BSAI_GOA, linetype = BSAI_GOA)) + 
  facet_wrap(. ~ SAMPLE_RATE, scales = "free", nrow = 1) + 
  geom_point() + geom_path() + labs(x = "# Trips", y = "Proximity", shape = "FMP", linetype = "FMP", color = "Strata") + 
  scale_shape_manual(values = c(BSAI = 16, GOA = 2))
# We see the most separation of Proximity at a rate of 5%?


#======================================================================================================================#
# Allocation using random sampling with replacement ----
#======================================================================================================================#

# Get realized proximity of each year
og_prox <- calculate_interspersion_gs(fixed_fmp.box, sample_rate_vec = seq(from = 0.0001, to = 0.9950, by = 0.0001) )

# For each year, calculate what the equal rates allocation would be
# fixed_fmp.equal <- rbindlist(lapply(budget_lst, function(x) cbind(BUDGET = x, allo_equal2(cost_dt, budget = x, fixed_fmp.allo_lst, cost_params))))
# dcast(fixed_fmp.equal, STRATA ~ ADP, value.var = "MON_RATE")
# proximity, indices, and costs.

# This function is a replacemnt for calculate_interspersion_gs(), calculate_index2(), and prox_rates_from_budget2(),
# that runs the allocation faster as it hones in on the budget, rather than doing all and then filtering for any budget.
# TODO NEED THIS FUNCTION FOR BOOTSTRAPPING TO GET RATES
allo_prox <- function(box_def, allo_lst, cost_params, budget, max_budget, index_interval = 0.001, range_var = 1) { 
  # box_def <- copy(fixed_fmp.box); allo_lst <- copy(fixed_fmp.allo_lst);  budget <- budget_lst[[1]]

  # box_def <- copy(bootstrap.box); allo_lst <- copy(bootstrap.allo_lst);  budget <- budget_lst[[1]]; index_interval <- 0.0001; range_var = 0
  
  # box_def <- copy(swor_bootstrap.box); allo_lst <- copy(swor_bootstrap.allo_lst); index_interval <- 0.0001
  
  # Have to do this by year, so feed the data one year at a time
  group_cols <- c(box_def$params$year_col, box_def$params$stratum_cols)
  stratum_cols <- box_def$params$stratum_cols
  year_col <- box_def$params$year_col
  
  year_vec <- unique(box_def$strata_n_dt$ADP)
  
  # TODO FOR NOW, OMIT ZERO and EM_TRW as we will not allocate to either in 2024
  box_def$strata_n_dt <- box_def$strata_n_dt[!(STRATA %like% "ZERO|EM_TRW")]
  box_def$dt_out <- box_def$dt_out[!(STRATA %like% "ZERO|EM_TRW")]
  box_def$box_smry <- box_def$box_smry[!(names(box_def$box_smry) %like% "ZERO|EM_TRW")]
  
  year_res <- vector(mode = "list", length = length(year_vec))
  
  for(i in year_vec) {

    # Quickly calculate proximity for a wide range of sample rates but with low resolution (0.005 instead of 0.0001)
    # makes this 50x faster.
    
    # TODO I Think I should use by = 0.001. the most time consuming part is calculating a huge a huge vector of INDEX, most
    # of which not even close!
    
    box_def_sub.prox.range <- calculate_interspersion_gs(box_def, sample_rate_vec = c(0.0001, seq(0.05, 1, by = 0.001)), omit_strata = "ZERO" )$ispn_dt[ADP == i]
    # Calculate index for each stratum
    box_def_sub.prox.range[
    ][, n := SAMPLE_RATE * STRATA_N
    ][, FPC := (STRATA_N - n) / STRATA_N
    ][, CV_SCALING := sqrt(FPC * (1/n))
    ][, INDEX := ISPN * (1 - CV_SCALING)][is.na(INDEX), INDEX := 0]
    setorderv(box_def_sub.prox.range, c(stratum_cols, "INDEX"))
    # Approximate the costs of a range of indices 
    index_vec <- seq(0, 1, by = 0.025)
    index_vec_rates_costs <- lapply(
      index_vec, 
      function(x) {
        res <- box_def_sub.prox.range[, .SD[findInterval(x, INDEX)], by = c(box_def$params$stratum_cols)]
        stratum_column <- apply(res[, ..stratum_cols], 1, paste, collapse = "-")
        res[, STRATUM_COL := stratum_column]
        res[, INDEX := x]
        index_cost <- calculate_cost_prox(res, cost_params, allo_lst, max_budget) # this is the most this index would cost
        if( nrow(index_cost) > 0 ) res[, INDEX_COST := index_cost$INDEX_COST]
        res
      }
    )
    
    # Omit any indices that go over the maximum budget
    index_costs <- sapply(index_vec_rates_costs, function(x) unique(x$INDEX_COST))
    for(j in seq_along(index_costs)) {
      if( is.null(index_costs[[j]])) {
        index_costs <- unlist(index_costs[1:(j - 1)])
        break()
      } else if (j > 1) {
        if( index_costs[[j]] < index_costs[[j - 1]]) {
          index_costs <- unlist(index_costs[1:(j - 1)])
          break()
        }
      }
    }
    
    # Find the range of indices to explore
    index_near_budget <- findInterval(budget, index_costs)
    index_range <- index_vec[c(index_near_budget - range_var, index_near_budget + 1)]  # I can afford an index somewhere in this range
    # Get all stratum names
    strata_dt <- unique(box_def$strata_n_dt[, ..stratum_cols])
    prox_by_stratum_lst <- vector(mode = "list", length = nrow(strata_dt))
    # Calculate proximity for each stratum using a focused range of sample rates
    for(k in 1:nrow(strata_dt)) {
      # k <- 1
      
      stratum_year <- paste0(i, ".", paste(strata_dt[k], collapse = "." ))
      # Make a new box definition specific to the stratum of focus
      box_stratum <- list(
        box_smry = box_def$box_smry[stratum_year],
        strata_n_dt = box_def$strata_n_dt,
        params = box_def$params
      )
      # Find the stratum's range of sample rates
      sample_range <- sapply(
        index_vec_rates_costs[c(index_near_budget - range_var, index_near_budget + 1)],             # FIXME I have to do +2 instead of +1. findInterval always underestimates?
        function(x) x[strata_dt[k], on = c(box_def$params$stratum_cols), SAMPLE_RATE]
      )
      # box_res <- copy(box_stratum); omit_strata <- NULL; sample_rate_vec <- seq(0.5, 575, by = 0.0001)
      # Now, we go back calculating rates ever 0.0001 here.
      prox_by_stratum <- calculate_interspersion_gs(box_stratum, sample_rate_vec = seq(sample_range[1], sample_range[2], by = 0.0001))$ispn_dt
      prox_by_stratum[
      ][, n := SAMPLE_RATE * STRATA_N
      ][, FPC := (STRATA_N - n) / STRATA_N
      ][, CV_SCALING := sqrt(FPC * (1/n))
      ][, INDEX := ISPN * (1 - CV_SCALING)]
      prox_by_stratum_lst[[k]]  <- prox_by_stratum
      
    }
    prox_by_list_dt <- rbindlist(prox_by_stratum_lst)
    
    # find the common range of indices
    # Find range if INDEX that is common to all strata x ADP
    prox_by_list_dt[, as.list(setNames(range(INDEX), c("MIN", "MAX"))), by = c(stratum_cols)]
    
    index_range_afforded <- prox_by_list_dt[
    ][, .(MIN = min(INDEX), MAX = max(INDEX)), by = group_cols
    ][, .(MIN = max(MIN),  MAX = min(MAX)), by = year_col]
    
    prox_by_list_dt <- prox_by_list_dt[, .SD[between(INDEX, index_range_afforded$MIN, index_range_afforded$MAX )], by = c(stratum_cols)]
    
    # I can set index_interval to 0.0001 to really get the closes to affording the budget. Does take 10x longer...
    prox_index_search <- seq(round(index_range_afforded$MIN,3), round(index_range_afforded$MAX,3), by = index_interval)
    index_costs2 <- lapply(prox_index_search, function(x) {
      x1 <- data.table(INDEX = x)
      x2 <- prox_by_list_dt[, .SD[x1, on = .(INDEX), roll = "nearest"], by = c(stratum_cols)]
      stratum_column <- apply(x2[, ..stratum_cols], 1, paste, collapse = "-")
      x2$STRATUM_COL <- stratum_column
      x2
    })
    # Calculate the cost of each index
    index_costs_vec <- sapply(index_costs2, function(x) {
      calculate_cost_prox(x, cost_params, allo_lst, max_budget)$INDEX_COST
    })
    
    # Find the index that is closest to the budget
    closest_to_budget <- findInterval(budget, unlist(index_costs_vec))
    out <- index_costs2[[closest_to_budget]]
    out[, INDEX_COST := unlist(index_costs_vec)[closest_to_budget]]
    year_res[[which(year_vec == i)]] <- out
  }
  
  # Return the allocated rates, collasping the list of rates by year
  rbindlist(year_res)

}

# Find the rates afforded each year assuming our current 2024 ADP budget
system.time(og_rates <- allo_prox(fixed_fmp.box, fixed_fmp.allo_lst, cost_params, budget_lst[[1]], max_budget, index_interval = 0.0001))
# using sample_rate by = 0.001  and default index_interval = 0.001 takes 53 sec. Changing it to 0.0001 takes 142 sec.
unique(og_rates[, .(ADP, INDEX_COST)])

# Overall, sample rates increase over time because effort overall has decreased
ggplot(og_rates, aes(x = ADP, y = SAMPLE_RATE, fill = STRATUM_COL)) + facet_grid(STRATUM_COL ~ .) + geom_col(color = "black") + labs(x = "Year", y = "Sample Rate", fill = "Stratum")
# When we look at proportions of sample size, there are very few differences
ggplot(og_rates, aes(x = ADP, y = n, fill = STRATUM_COL)) + geom_col(position = "fill", color = "black") + labs(x = "Year", y = "Sample size (n)", fill = "Stratum")

# Now that we can fairly quickly calculate allocated rates, we can do some testing of bootstrapping methods!


#======================================================================================================================#
# Prepare Bootstrapping ----
#======================================================================================================================#

# Define number of bootstrap iterations
bootstrap_iter <- 100

#======================================================================================================================#
# 1-Year Sampling >With< Replacement (current method) ----
#======================================================================================================================#

# Assumptions
# + We will assume we have a perfect prediction of `next year's` fishing effort (STRATA_N)

# We will sample the 'current' year's fishing with replacement knowing the upcoming year's STRATA_N
# We will treat each 

# Initialize list of bootstrapping results
adp_years <- unique(fixed.val_2015_2023_dt[order(ADP), ADP])  # 2015 - 2023
adp_years_to_predict <- adp_years[-1]                     # Will use 2015 to predict 2016, 2016 for 2017, and so on.

swr_1_lst <- vector(mode = "list", length = adp_years_to_predict)
set.seed(12345)
for(j in seq_along(adp_years_to_predict)) {
  # j <- 1
  
  cat("Year", adp_years_to_predict[j], "\n")
  
  # Subset the data to prior year(s)
  adp_years_to_pull <- adp_years_to_predict[j] - (1)
  effort_prior <- fixed.val_2015_2023_dt[ADP %in% adp_years_to_pull]
  effort_prior[, ADP := adp_years_to_predict[j]]            # ADP is the year our prediction is for
  effort_prior_lst <- split(effort_prior[order(STRATA, BSAI_GOA)], by = c("STRATA", "BSAI_GOA"))
  
  # Use actual trip counts as our 'predicted' number of trips to sample to create yearly populations (perfect prediction of effort)
  adp_strata_N <- fixed.val_2015_2023_dt[ADP == adp_years_to_predict[j], .(N = uniqueN(TRIP_ID)), keyby = .(STRATA, BSAI_GOA)]
  adp_strata_N[, STRATUM_COL := paste0(STRATA, ".", BSAI_GOA)] 
  
  # Make sure names and ordering are the same
  if(!identical(names(effort_prior_lst), adp_strata_N$STRATUM_COL)) stop("Stratum names/order are not the same!")
  
  # Initialize bootstrap list
  swr_bootstrap_lst <- vector(mode = "list", length = bootstrap_iter)
  
  # Begin bootstrap sampling WITH replacement
  for(i in seq_len(bootstrap_iter)) {
    
    cat(i, ", ")
    
    # Bootstrap using adp_strata_N to sample each stratum's population size size
    swr_bootstrap.effort <- rbindlist(Map(
      function(prior, strata_N) {
        # prior <- effort_prior_lst[[1]]; strata_N <- adp_strata_N$N[1]
        trip_ids <- unique(prior$TRIP_ID)
        sampled_trip_ids <- data.table(TRIP_ID = sample(trip_ids, size = strata_N, replace = T))
        sampled_trip_ids[, I := .I]
        # Bring in each trip's data
        bootstrap_sample <- prior[sampled_trip_ids, on = .(TRIP_ID), allow.cartesian = T]
      }, 
      prior = effort_prior_lst,
      strata_N = adp_strata_N$N
    ))
 
    # Re-assign trip_id so that we can differentiate trips sampled multiple times
    swr_bootstrap.effort[, TRIP_ID := .GRP, keyby = .(ADP, STRATA, BSAI_GOA, TRIP_ID, I)]
    if(uniqueN(swr_bootstrap.effort$TRIP_ID) !=  sum(adp_strata_N$N)) stop("Count of TRIP_IDs doesn't match!")
    
    # Define boxes of bootstrapped effort
    bootstrap.box <- define_boxes_gs(
      swr_bootstrap.effort, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
      year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), geom = F, ps_cols = c("GEAR"))
  
    bootstrap.allo_lst <- list(effort = unique(swr_bootstrap.effort[, .(ADP, STRATA, BSAI_GOA, TRIP_ID, DAYS)])[
    ][, STRATA := paste0(STRATA, "-", BSAI_GOA)
    ][, BSAI_GOA := NULL
    ][, .(STRATA_N = uniqueN(TRIP_ID), TRP_DUR = mean(DAYS)), keyby = .(ADP, STRATA)])
  
    # Calculate proximity at 15% sample rate (function is still called interspersion)
    bootstrap.prox <- calculate_interspersion_gs(bootstrap.box, sample_rate_vec = sample_rate_15)$ispn_dt
    
    # Calculate rates afforded with a $5.81M budget
    # [NOTE] allo_prox currently omits ZERO and EM_TRW from the allocation, per 2024 ADP. 
    bootstrap.rates <- allo_prox(bootstrap.box, bootstrap.allo_lst, cost_params, budget_lst[[1]], max_budget, index_interval = 0.0001, range_var = 1)
   
    swr_bootstrap_lst[[i]] <- list(prox = bootstrap.prox, rates = bootstrap.rates)
  }
  
  # Capture the results from each year
  swr_1_lst[[j]] <- swr_bootstrap_lst
}

# save(swr_1_lst, file = "analyses/effort_prediction/swr_1_lst.rdata")

swr_1_prox_dt <-  rbindlist(lapply(swr_1_lst, function(x) rbindlist(lapply(x, "[[", "prox"), idcol = "ITER")))
swr_1_rates_dt <-  rbindlist(lapply(swr_1_lst, function(x) rbindlist(lapply(x, "[[", "rates"), idcol = "ITER")))


ggplot(swr_1_rates_dt, aes(x = as.character(ADP), y = SAMPLE_RATE)) + facet_grid(STRATUM_COL ~ ., scales = "free") + 
  geom_violin(color = "red", draw_quantiles = 0.5) + 
  stat_summary(geom = "point", color = "red", fun = mean, shape = 4) + 
  geom_point(data = og_rates, shape = 1, fill = NA, stroke = 1) + 
  labs(x = "Year", y = "Sample Rate")

# As predicted, because proximity is inflated when bootstrapping using sampling with replacement, the smaller BSAI strata
# end up with lower than desired sampling rates, and GOA get higher-than-desired sampling rates.

bootstrap_rates_mean <- bootstrap_rates_dt[, .(SAMPLE_RATE = mean(SAMPLE_RATE)), keyby = .(ADP, STRATUM_COL, STRATA_N)]
bootstrap_rates_mean[, OG_RATE := og_rates[bootstrap_rates_mean, SAMPLE_RATE, on = .(ADP, STRATUM_COL)]]
# Percent difference in rates
dcast(
  bootstrap_rates_mean[, .(PERC_DIFF = round((SAMPLE_RATE - OG_RATE)/OG_RATE * 100,3) ), keyby = .(ADP, STRATUM_COL)],
  STRATUM_COL ~ ADP, value.var = c("PERC_DIFF"))
# Raw difference in rates
dcast(
  bootstrap_rates_mean[, .(RAW_DIFF = round((SAMPLE_RATE - OG_RATE),3) ), keyby = .(ADP, STRATUM_COL)],
  STRATUM_COL ~ ADP, value.var = c("RAW_DIFF"))
# Raw difference in sample size
dcast(
  bootstrap_rates_mean[, .(n_DIFF = round(STRATA_N * (SAMPLE_RATE - OG_RATE),3) ), keyby = .(ADP, STRATUM_COL)],
  STRATUM_COL ~ ADP, value.var = c("n_DIFF"))

# On average, the rates are really not vastly different: on average they end up being within ~ 0.02. In 2022, It mostly
# means 7 more tris for OB_FIXED-GOA and 4.5 fewer for OB_FIXED-BSAI
# Interestingly... OB_TRW-BSAI ends up with more samples just because it is protected by CV_SCALING so much, and
# EM_FIXED-BSAI an OB-FIXED-BSAI are sampled less resulting in more money for sampling other strata.


#======================================================================================================================#
# 3-year Sampling >Without< Replacement ----
#======================================================================================================================#

# [ NOTE ] 
# Assuming we have a perfect prediction of next year's number of fishing trips, how do the sampling methods differ
# in their prediction of Proximity and Sample Rates? The differences are due to how well they predict the spatiotemporal
# arrangement of the fishing.

# If we see what we have above and assume that our estimate for one year would be used to predict the next, we have a 
# baseline level of prediction error, some of which will be due to the bias. If we use sampling without replacement 
# using the prior 3 years of fishing effort, can we resolve this bias issue? And, does it overall have less prediction 
# error?

adp_years <- unique(fixed.val_2015_2023_dt[order(ADP), ADP])  # 2015 - 2023
adp_years_to_predict <- adp_years[-(1:3)]                     # Will use 2015-2017 to predict 2018, and so on.
swor_3_lst <- vector(mode = "list", length = length(adp_years_to_predict))
set.seed(12345)
for(j in seq_along(adp_years_to_predict)) {
  # j <- 1
  
  cat("Year ", adp_years_to_predict[j], "\n")
  
  # Subset the data to prior 3 years
  adp_years_to_pull <- adp_years_to_predict[j] - (3:1)
  effort_prior_3 <- fixed.val_2015_2023_dt[ADP %in% adp_years_to_pull]
  effort_prior_3[, ADP := adp_years_to_predict[j]]
  effort_prior_3_lst <- split(effort_prior_3[order(STRATA, BSAI_GOA)], by = c("STRATA", "BSAI_GOA"))
  
  # Use actual trip counts as our 'predicted' number of trips to sample to create yearly populations
  adp_strata_N <- fixed.val_2015_2023_dt[ADP == adp_years_to_predict[j], .(N = uniqueN(TRIP_ID)), keyby = .(STRATA, BSAI_GOA)]
  adp_strata_N[, STRATUM_COL := paste0(STRATA, ".", BSAI_GOA)] 
  
  # Make sure names and ordering are the same
  if(!identical(names(effort_prior_3_lst), adp_strata_N$STRATUM_COL)) stop("Stratum names/order are not the same!")
  
  # Initialize bootstrap list
  swor_boot_lst <- vector(mode = "list", length = bootstrap_iter)
  
  for(k in seq_len(bootstrap_iter)) {
    # k <- 1
    cat(k, ", ")

    # Bootstrap using adp_strata_N to sample each stratum's population size size
    swor_bootstrap.effort <- rbindlist(Map(
      function(prior, strata_N) {
        # Sample trips WITHOUT replacement
        trip_ids <- unique(prior$TRIP_ID)
        sampled_trip_ids <- data.table(TRIP_ID = sample(trip_ids, size = strata_N, replace = F))
        sampled_trip_ids[, I := .I]
        bootstrap_sample <- prior[sampled_trip_ids, on = .(TRIP_ID), allow.cartesian = T]
      }, 
      prior = effort_prior_3_lst,
      strata_N = adp_strata_N$N
    ))

    # Re-assign trip_id so that we can differentiate trips sampled multiple times
    swor_bootstrap.effort[, TRIP_ID := .GRP, keyby = .(ADP, STRATA, BSAI_GOA, TRIP_ID, I)]
    if(uniqueN(swor_bootstrap.effort$TRIP_ID) != sum(adp_strata_N$N)) stop("Count of TRIP_IDs doesn't match!")
    
    # Adjust the TRIP_TARGET_DATE and LANDING_DATE to be for the current year
    swor_bootstrap.effort[, ':=' (
      TRIP_TARGET_DATE = as.Date(yday(TRIP_TARGET_DATE), origin = paste0(adp_years_to_predict[j], "-01-01")) - 1 ,
      LANDING_DATE =  as.Date(yday(LANDING_DATE), origin = paste0(adp_years_to_predict[j], "-01-01")) - 1
    )]
    # Due to leap years, cram any dates that got pushed to the next to to the previous year, Dec 31
    swor_bootstrap.effort[year(TRIP_TARGET_DATE) == (adp_years_to_predict[j] + 1), TRIP_TARGET_DATE := TRIP_TARGET_DATE - 1 ]

    # Define boxes of bootstrapped effort
    swor_bootstrap.box <- define_boxes_gs(
      swor_bootstrap.effort, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
      year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), geom = F, ps_cols = c("GEAR"))
    
    swor_bootstrap.allo_lst <- list(effort = unique(swor_bootstrap.effort[, .(ADP, STRATA, BSAI_GOA, TRIP_ID, DAYS)])[
    ][, STRATA := paste0(STRATA, "-", BSAI_GOA)
    ][, BSAI_GOA := NULL
    ][, .(STRATA_N = uniqueN(TRIP_ID), TRP_DUR = mean(DAYS)), keyby = .(ADP, STRATA)])
    
    # Calculate proximity at 15% sample rate (function is still called interspersion)
    swor_bootstrap.prox <- calculate_interspersion_gs(swor_bootstrap.box, sample_rate_vec = sample_rate_15)$ispn_dt
    
    # Calculate rates afforded with a $5.81M budget
    swor_bootstrap.rates <- allo_prox(swor_bootstrap.box, swor_bootstrap.allo_lst, cost_params, budget_lst[[1]], max_budget, index_interval = 0.0001)

    # Save iteration results
    swor_boot_lst[[k]] <- list(
      prox = swor_bootstrap.prox, 
      rates = swor_bootstrap.rates
    )

  }
  
  # Capture the results from each year
  swor_3_lst[[j]] <- swor_boot_lst

}

# save(swor_3_lst, file = "analyses/effort_prediction/swor_3_lst.rdata")


#======================================================================================================================#
## Proximity
#======================================================================================================================#

test <- rbindlist(lapply(swor_3_lst, function(x) rbindlist(lapply(x, "[[", "prox"), idcol = "ITER")))
# As a test, using 2015-2017 SWOR (red distribution) to predict 2018 realized (black dot)  
ggplot(test, aes(x = as.character(ADP), y = ISPN)) + 
  facet_grid(BSAI_GOA ~ STRATA, scales = "free") +
  geom_violin(draw_quantiles = 0.5, color = "red", fill = NA) + 
  geom_violin(data = bootstrap_prox_dt[ADP != 2023], aes(x = as.character(ADP + 1)), color = "blue", draw_quantiles = 0.5, fill = NA) +
  geom_point(data = og_prox$ispn_dt[SAMPLE_RATE == 0.15 ], fill = "green", shape = 21, size = 2) +
  geom_text(data = og_prox$ispn_dt[SAMPLE_RATE == 0.15 ], aes(label = STRATA_N), size = 2, nudge_y = -0.01) +
  labs(x = "Year", y = "Proximity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
# RED: SWoR-3; BLUE: SWR-1


# TODO Try bootstrapping just two years?

#======================================================================================================================#
## Allocation
#======================================================================================================================#

test2 <- rbindlist(lapply(swor_3_lst, function(x) rbindlist(lapply(x, "[[", "rates"), idcol = "ITER")))
# As a test, using 2015-2017 SWOR (red distribution) to predict 2018 realized (black dot)  
ggplot(test2, aes(x = as.character(ADP), y = SAMPLE_RATE)) + 
  facet_grid(BSAI_GOA ~ STRATA, scales = "free") +
  geom_violin(draw_quantiles = 0.5, color = "red", fill = NA) + 
  #geom_point(data = og_prox$ispn_dt[SAMPLE_RATE == 0.15 & ADP == 2017], color = "blue") + 
  # Offsetting 1-year SWOR bootstrap by 1 year, e.g., using 2015 to predict 2016
  geom_violin(data = bootstrap_rates_dt[ADP != 2023], aes(x = as.character(ADP + 1)), color = "blue", draw_quantiles = 0.5, fill = NA) + 
  geom_point(data = og_rates, color = "black", size = 2) +
  labs(x = "Year", y = "Sample Rate") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
# Blue is basically just allocating based on previous year (with some biases due to SWR)





# TODO Plot diff relative to OG?


swr_1_diff <- copy(bootstrap_rates_dt)
swr_1_diff[, ADP := ADP + 1]
swr_1_diff <- swr_1_diff[ADP %in% 2018:2023]
swr_1_diff <- swr_1_diff[, .(SWR_1_RATE = mean(SAMPLE_RATE), SWR_1_PROX = mean(ISPN)), keyby = .(ADP, STRATUM_COL)]

swor_3_diff <- test2[, .(SWOR_3_RATE = mean(SAMPLE_RATE), SWOR_3_PROX = mean(ISPN)), keyby = .(ADP, STRATUM_COL)]

compare_rates_prox <- og_rates[ADP %in% c(2018:2023), .(STRATUM_COL, STRATA, BSAI_GOA, ADP, OG_RATE = SAMPLE_RATE, OG_PROX = ISPN)]
compare_rates_prox <- compare_rates_prox[swr_1_diff, on = .(ADP, STRATUM_COL)][swor_3_diff, on = .(ADP, STRATUM_COL)]

compare_rates_prox[, .(
  SWR_1_bias = mean(SWR_1_RATE - OG_RATE),
  SWOR_3_bias = mean(SWOR_3_RATE - OG_RATE),
  SWR_1_total_resid = sum((SWR_1_RATE - OG_RATE)^2),
  SWOR_3_total_resid = sum((SWOR_3_RATE - OG_RATE)^2)
)]

 # Sampling without replacement using only most recent year (and if effort increases, use all trips and then SWR)


#======================================================================================================================#
# 2-year Sampling >Without< Replacement ----
#======================================================================================================================#

# Same thing as before, but using 2 years instead of 3

adp_years <- unique(fixed.val_2015_2023_dt[order(ADP), ADP])  # 2015 - 2023
adp_years_to_predict <- adp_years[-(1:2)]                     # Will use 2015-2017 to predict 2018, and so on.
swor_2_lst <- vector(mode = "list", length = length(adp_years_to_predict))
set.seed(12345)
for(j in seq_along(adp_years_to_predict)) {
  # j <- 1
  
  cat("Year ", adp_years_to_predict[j], "\n")
  
  # Subset the data to prior 3 years
  adp_years_to_pull <- adp_years_to_predict[j] - (2:1)
  effort_prior_3 <- fixed.val_2015_2023_dt[ADP %in% adp_years_to_pull]
  effort_prior_3[, ADP := adp_years_to_predict[j]]
  effort_prior_3_lst <- split(effort_prior_3[order(STRATA, BSAI_GOA)], by = c("STRATA", "BSAI_GOA"))
  
  # Use actual trip counts as our 'predicted' number of trips to sample to create yearly populations
  adp_strata_N <- fixed.val_2015_2023_dt[ADP == adp_years_to_predict[j], .(N = uniqueN(TRIP_ID)), keyby = .(STRATA, BSAI_GOA)]
  adp_strata_N[, STRATUM_COL := paste0(STRATA, ".", BSAI_GOA)] 
  
  # Make sure names and ordering are the same
  if(!identical(names(effort_prior_3_lst), adp_strata_N$STRATUM_COL)) stop("Stratum names/order are not the same!")
  
  # Initialize bootstrap list
  swor_boot_lst <- vector(mode = "list", length = bootstrap_iter)
  
  for(k in seq_len(bootstrap_iter)) {
    # k <- 1
    cat(k, ", ")
    
    # Bootstrap using adp_strata_N to sample each stratum's population size size
    swor_bootstrap.effort <- rbindlist(Map(
      function(prior, strata_N) {
        # Sample trips WITHOUT replacement
        trip_ids <- unique(prior$TRIP_ID)
        sampled_trip_ids <- data.table(TRIP_ID = sample(trip_ids, size = strata_N, replace = F))
        sampled_trip_ids[, I := .I]
        bootstrap_sample <- prior[sampled_trip_ids, on = .(TRIP_ID), allow.cartesian = T]
      }, 
      prior = effort_prior_3_lst,
      strata_N = adp_strata_N$N
    ))
    
    # Re-assign trip_id so that we can differentiate trips sampled multiple times
    swor_bootstrap.effort[, TRIP_ID := .GRP, keyby = .(ADP, STRATA, BSAI_GOA, TRIP_ID, I)]
    if(uniqueN(swor_bootstrap.effort$TRIP_ID) != sum(adp_strata_N$N)) stop("Count of TRIP_IDs doesn't match!")
    
    # Adjust the TRIP_TARGET_DATE and LANDING_DATE to be for the current year
    swor_bootstrap.effort[, ':=' (
      TRIP_TARGET_DATE = as.Date(yday(TRIP_TARGET_DATE), origin = paste0(adp_years_to_predict[j], "-01-01")) - 1 ,
      LANDING_DATE =  as.Date(yday(LANDING_DATE), origin = paste0(adp_years_to_predict[j], "-01-01")) - 1
    )]
    # Due to leap years, cram any dates that got pushed to the next to to the previous year, Dec 31
    swor_bootstrap.effort[year(TRIP_TARGET_DATE) == (adp_years_to_predict[j] + 1), TRIP_TARGET_DATE := TRIP_TARGET_DATE - 1 ]
    
    # Define boxes of bootstrapped effort
    swor_bootstrap.box <- define_boxes_gs(
      swor_bootstrap.effort, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
      year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), geom = F, ps_cols = c("GEAR"))
    
    swor_bootstrap.allo_lst <- list(effort = unique(swor_bootstrap.effort[, .(ADP, STRATA, BSAI_GOA, TRIP_ID, DAYS)])[
    ][, STRATA := paste0(STRATA, "-", BSAI_GOA)
    ][, BSAI_GOA := NULL
    ][, .(STRATA_N = uniqueN(TRIP_ID), TRP_DUR = mean(DAYS)), keyby = .(ADP, STRATA)])
    
    # Calculate proximity at 15% sample rate (function is still called interspersion)
    swor_bootstrap.prox <- calculate_interspersion_gs(swor_bootstrap.box, sample_rate_vec = sample_rate_15)$ispn_dt
    
    # Calculate rates afforded with a $5.81M budget
    swor_bootstrap.rates <- allo_prox(swor_bootstrap.box, swor_bootstrap.allo_lst, cost_params, budget_lst[[1]], max_budget, index_interval = 0.0001)
    
    # Save iteration results
    swor_boot_lst[[k]] <- list(
      prox = swor_bootstrap.prox, 
      rates = swor_bootstrap.rates
    )
    
  }
  
  # Capture the results from each year
  swor_2_lst[[j]] <- swor_boot_lst
  
}

# save(swor_2_lst, file = "analyses/effort_prediction/swor_2_lst.rdata")

#======================================================================================================================#
# 1-year Sampling >Without< Replacement (modified) ----
#======================================================================================================================#

# Here, we use the 'current' 1-year of fishing to predict the next year a fishing, using the next year's STRATA_N (i.e.,
# we have a perfect prediction of the size of the next year of fishing effort.) If we downsample, it's a simple SWoR, and
# if we have to upsample, we sample all trips once (or more if needed), then the remainder with SWoR again.

adp_years <- unique(fixed.val_2015_2023_dt[order(ADP), ADP])  # 2015 - 2023
adp_years_to_predict <- adp_years[-1]                     # Will use 2015-2017 to predict 2018, and so on.
swor_1_lst <- vector(mode = "list", length = length(adp_years_to_predict))
set.seed(12345)
for(j in seq_along(adp_years_to_predict)) {
  # j <- 1
  
  cat("Year ", adp_years_to_predict[j], "\n")
  
  # Subset the data to prior year(s)
  adp_years_to_pull <- adp_years_to_predict[j] - (1)
  effort_prior <- fixed.val_2015_2023_dt[ADP %in% adp_years_to_pull]
  effort_prior[, ADP := adp_years_to_predict[j]]            # ADP is the year our prediction is for
  effort_prior_lst <- split(effort_prior[order(STRATA, BSAI_GOA)], by = c("STRATA", "BSAI_GOA"))
  
  # Use actual trip counts as our 'predicted' number of trips to sample to create yearly populations (perfect prediction of effort)
  adp_strata_N <- fixed.val_2015_2023_dt[ADP == adp_years_to_predict[j], .(N = uniqueN(TRIP_ID)), keyby = .(STRATA, BSAI_GOA)]
  adp_strata_N[, STRATUM_COL := paste0(STRATA, ".", BSAI_GOA)] 
  
  # Make sure names and ordering are the same
  if(!identical(names(effort_prior_lst), adp_strata_N$STRATUM_COL)) stop("Stratum names/order are not the same!")
  
  # Initialize bootstrap list
  swor_boot_lst <- vector(mode = "list", length = bootstrap_iter)
  
  for(k in seq_len(bootstrap_iter)) {
    # k <- 1
    cat(k, ", ")
    
    # Bootstrap using adp_strata_N to sample each stratum's population size size
    swor_bootstrap.effort <- rbindlist(Map(
      function(prior, strata_N) {
        # prior <- effort_prior_lst[[1]]; strata_N <- adp_strata_N$N[1]
        # Sample trips WITHOUT replacement
        trip_ids <- unique(prior$TRIP_ID)
        
        # How many times does prior effort go into future effort?
        prior_vs_future <- floor(strata_N / length(trip_ids))
        # What number of trips should be sampled without replacement?
        swr_n <- strata_N - (length(trip_ids) * prior_vs_future)
        # Create dt of trip_ids
        sampled_trip_ids <- data.table(
          TRIP_ID = c(
            # Repeat trip_id prior_vs_future times
            rep(trip_ids, times = prior_vs_future), 
            # Sample without replacement using swr_n
            sample(trip_ids, size = swr_n, replace = F)
          )
        )
        sampled_trip_ids[, I := .I]
        # Bring in each trip's data
        bootstrap_sample <- prior[sampled_trip_ids, on = .(TRIP_ID), allow.cartesian = T]
      }, 
      prior = effort_prior_lst,
      strata_N = adp_strata_N$N
    ))
    
    # Re-assign trip_id so that we can differentiate trips sampled multiple times
    swor_bootstrap.effort[, TRIP_ID := .GRP, keyby = .(ADP, STRATA, BSAI_GOA, TRIP_ID, I)]
    if(uniqueN(swor_bootstrap.effort$TRIP_ID) != sum(adp_strata_N$N)) stop("Count of TRIP_IDs doesn't match!")
    
    # Adjust the TRIP_TARGET_DATE and LANDING_DATE to be for the current year
    swor_bootstrap.effort[, ':=' (
      TRIP_TARGET_DATE = as.Date(yday(TRIP_TARGET_DATE), origin = paste0(adp_years_to_predict[j], "-01-01")) - 1 ,
      LANDING_DATE =  as.Date(yday(LANDING_DATE), origin = paste0(adp_years_to_predict[j], "-01-01")) - 1
    )]
    # Due to leap years, cram any dates that got pushed to the next to to the previous year, Dec 31
    swor_bootstrap.effort[year(TRIP_TARGET_DATE) == (adp_years_to_predict[j] + 1), TRIP_TARGET_DATE := TRIP_TARGET_DATE - 1 ]
    
    # Define boxes of bootstrapped effort
    swor_bootstrap.box <- define_boxes_gs(
      swor_bootstrap.effort, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
      year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), geom = F, ps_cols = c("GEAR"))
    
    swor_bootstrap.allo_lst <- list(effort = unique(swor_bootstrap.effort[, .(ADP, STRATA, BSAI_GOA, TRIP_ID, DAYS)])[
    ][, STRATA := paste0(STRATA, "-", BSAI_GOA)
    ][, BSAI_GOA := NULL
    ][, .(STRATA_N = uniqueN(TRIP_ID), TRP_DUR = mean(DAYS)), keyby = .(ADP, STRATA)])
    
    # Calculate proximity at 15% sample rate (function is still called interspersion)
    swor_bootstrap.prox <- calculate_interspersion_gs(swor_bootstrap.box, sample_rate_vec = sample_rate_15)$ispn_dt
    
    # Calculate rates afforded with a $5.81M budget
    swor_bootstrap.rates <- allo_prox(swor_bootstrap.box, swor_bootstrap.allo_lst, cost_params, budget_lst[[1]], max_budget, index_interval = 0.0001)
    
    # Why is it taking 4 minutes whereas it took less than one on the original dataset!? That's basically as long as it was before...
    # I had to change the index vector to 0.001 because 0.005 kept breaking (lowest budget was still higher than budget in initial...)
    # In the function, I need to do a better job of initializing the range...
    
    swor_boot_lst[[k]] <- list(
      prox = swor_bootstrap.prox, 
      rates = swor_bootstrap.rates
    )
    
  }
  
  # Capture the results from each year
  swor_1_lst[[j]] <- swor_boot_lst
  
}

# save(swor_1_lst, file = "analyses/effort_prediction/swor_1_lst.rdata")


# og_prox, og_rates, swr_1_lst, swor_3_lst, swor_2_lst, swor_1_lst

# Split bootstrap results into 15% proximity and allocated selection rates

swr_1_prox_dt <-  rbindlist(lapply(swr_1_lst, function(x) rbindlist(lapply(x, "[[", "prox"), idcol = "ITER")))
swr_1_rates_dt <-  rbindlist(lapply(swr_1_lst, function(x) rbindlist(lapply(x, "[[", "rates"), idcol = "ITER")))

swor_3_prox_dt <-  rbindlist(lapply(swor_3_lst, function(x) rbindlist(lapply(x, "[[", "prox"), idcol = "ITER")))
swor_3_rates_dt <-  rbindlist(lapply(swor_3_lst, function(x) rbindlist(lapply(x, "[[", "rates"), idcol = "ITER")))

swor_2_prox_dt <-  rbindlist(lapply(swor_2_lst, function(x) rbindlist(lapply(x, "[[", "prox"), idcol = "ITER")))
swor_2_rates_dt <-  rbindlist(lapply(swor_2_lst, function(x) rbindlist(lapply(x, "[[", "rates"), idcol = "ITER")))

swor_1_prox_dt <-  rbindlist(lapply(swor_1_lst, function(x) rbindlist(lapply(x, "[[", "prox"), idcol = "ITER")))
swor_1_rates_dt <-  rbindlist(lapply(swor_1_lst, function(x) rbindlist(lapply(x, "[[", "rates"), idcol = "ITER")))


#======================================================================================================================#
## Proximity ----
#======================================================================================================================#

# Summarize all results for 2018-2023 (omit 2015-2017 as we can't make comparisons for those years)

# How does Proximity at a 15% selection rate compare vs actual?
plot_prox_raw <- ggplot(swr_1_prox_dt[ADP >= 2018], aes(x = as.character(ADP), y = ISPN)) + 
  facet_grid(BSAI_GOA ~ STRATA, scales = "free") +
  geom_line(data = og_prox$ispn_dt[ADP >= 2018 & SAMPLE_RATE == 0.15], aes(group = interaction(STRATA, BSAI_GOA))) +
  geom_violin(color = "cyan3", draw_quantiles = 0.5, fill = NA) + 
  geom_violin(color = "blue",    draw_quantiles = 0.5, fill = NA, data = swor_3_prox_dt[ADP >= 2018]) +
  geom_violin(color = "purple",  draw_quantiles = 0.5, fill = NA, data = swor_2_prox_dt[ADP >= 2018]) +
  geom_violin(color = "magenta", draw_quantiles = 0.5, fill = NA, data = swor_1_prox_dt[ADP >= 2018]) +
  geom_point(data = og_prox$ispn_dt[ADP >= 2018 & SAMPLE_RATE == 0.15 ], fill = "green", shape = 21, size = 2) +
  geom_text(data = og_prox$ispn_dt[ADP >= 2018 & SAMPLE_RATE == 0.15 ], aes(label = STRATA_N, y = 1), size = 2, vjust = 0) +
  labs(x = "Year", y = "Proximity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
# Cyan and Magenta (swr_1 and swor_1) Are generally very similar and overlapping, swor_1 having a tighter distribution.



prox_res <- rbind(
  #cbind(Method = "Actual", og_prox$ispn_dt[ADP >= 2018 & SAMPLE_RATE == 0.15 ]),
  cbind(Method = "swr_1", swr_1_prox_dt[ADP >= 2018]),
  cbind(Method = "swor_3", swor_3_prox_dt[ADP >= 2018]),
  cbind(Method = "swor_2", swor_2_prox_dt[ADP >= 2018]),
  cbind(Method = "swor_1", swor_1_prox_dt[ADP >= 2018]),
  fill = T
)
rates_res <-rbind(
  #cbind(Method = "Actual", og_rates[ADP >= 2018]),
  cbind(Method = "swr_1", swr_1_rates_dt[ADP >= 2018]),
  cbind(Method = "swor_3", swor_3_rates_dt[ADP >= 2018]),
  cbind(Method = "swor_2", swor_2_rates_dt[ADP >= 2018]),
  cbind(Method = "swor_1", swor_1_rates_dt[ADP >= 2018]),
  fill = T
)[, .(Method, STRATA, BSAI_GOA, STRATUM_COL, ADP, SAMPLE_RATE, n, INDEX, ITER)]

boot_res <- rates_res[prox_res, on = .(Method, ADP, STRATA, BSAI_GOA, ITER)]

# Here, we can see how each method differed relative to the actual (line and point)
plot_prox_res <- ggplot(boot_res, aes(x = as.character(ADP), y = ISPN)) + 
  facet_nested(STRATA + BSAI_GOA ~ Method) +
  geom_line(data = og_prox$ispn_dt[ADP >= 2018 & SAMPLE_RATE == 0.15], aes(group = interaction(STRATA, BSAI_GOA))) +
  geom_violin(aes(color = Method), draw_quantiles = 0.5, fill = NA, position = "identity") + 
  geom_point(data = og_prox$ispn_dt[ADP >= 2018 & SAMPLE_RATE == 0.15]) + 
  scale_color_manual(values = c(swr_1 = "cyan3", swor_3 =  "blue", swor_2 = "purple", swor_1 = "magenta")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x = "Year", y = "Proximity")

plot_prox_res_free_scale <-  ggplot(boot_res, aes(x = as.character(ADP), y = ISPN)) + 
  facet_nested(STRATA + BSAI_GOA ~ Method, scales = "free") +
  geom_line(data = og_prox$ispn_dt[ADP >= 2018 & SAMPLE_RATE == 0.15], aes(group = interaction(STRATA, BSAI_GOA))) +
  geom_violin(aes(color = Method), draw_quantiles = 0.5, fill = NA, position = "identity") + 
  geom_point(data = og_prox$ispn_dt[ADP >= 2018 & SAMPLE_RATE == 0.15]) + 
  scale_color_manual(values = c(swr_1 = "cyan3", swor_3 =  "blue", swor_2 = "purple", swor_1 = "magenta")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x = "Year", y = "Proximity")

plot_prox_lst <- unify_plot_widths(plot_lst = list(original = plot_prox_res, free = plot_prox_res_free_scale))


# Here, we can see how each method differed relative to the actual (line and point)
plot_rates_res <- ggplot(boot_res[!is.na(SAMPLE_RATE)], aes(x = as.character(ADP), y = SAMPLE_RATE)) + 
  facet_nested(STRATA + BSAI_GOA ~ Method) +
  geom_line(data = og_rates, aes(group = interaction(STRATA, BSAI_GOA))) +
  geom_violin(aes(color = Method), draw_quantiles = 0.5, fill = NA, position = "identity", na.rm = T) + 
  geom_point(data = og_rates) + 
  scale_color_manual(values = c(swr_1 = "cyan3", swor_3 =  "blue", swor_2 = "purple", swor_1 = "magenta")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x = "Year", y = "Proximity")

plot_rates_free_scale <-  ggplot(boot_res[!is.na(SAMPLE_RATE)], aes(x = as.character(ADP), y = SAMPLE_RATE)) + 
  facet_nested(STRATA + BSAI_GOA ~ Method, scales = "free") +
  geom_line(data = og_rates, aes(group = interaction(STRATA, BSAI_GOA))) +
  geom_violin(aes(color = Method), draw_quantiles = 0.5, fill = NA, position = "identity", na.rm = T) + 
  geom_point(data = og_rates) + 
  scale_color_manual(values = c(swr_1 = "cyan3", swor_3 =  "blue", swor_2 = "purple", swor_1 = "magenta")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x = "Year", y = "Proximity")

plot_rates_lst <- unify_plot_widths(plot_lst = list(original = plot_rates_res, free = plot_rates_free_scale))

save(plot_prox_lst, plot_rates_lst, file = "analyses/effort_prediction/effort_bootstrap.rdata")
# swor_1 has by far the least variability among iterations (no surprise)
# swor_2 and swor_3 have 

#======================================================================================================================#
## Rates ----
#======================================================================================================================#

# How well do predicted rates match realized rates?
plot_rates_raw <- ggplot(swr_1_rates_dt[ADP >= 2018], aes(x = as.character(ADP), y = SAMPLE_RATE)) + 
  facet_grid(BSAI_GOA ~ STRATA, scales = "free") +
  geom_line(data = og_rates[ADP >= 2018], aes(group = interaction(STRATA, BSAI_GOA))) +
  geom_violin(color = "cyan3", draw_quantiles = 0.5, fill = NA) + 
  geom_violin(color = "blue",    draw_quantiles = 0.5, fill = NA, data = swor_3_rates_dt[ADP >= 2018]) +
  geom_violin(color = "purple",  draw_quantiles = 0.5, fill = NA, data = swor_2_rates_dt[ADP >= 2018]) +
  geom_violin(color = "magenta", draw_quantiles = 0.5, fill = NA, data = swor_1_rates_dt[ADP >= 2018]) +
  geom_point(data = og_rates[ADP >= 2018], fill = "green", shape = 21, size = 2) +
  geom_text(data = og_rates[ADP >= 2018], aes(label = STRATA_N, y = 0), size = 2, vjust = 0) +
  labs(x = "Year", y = "Proximity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
# Overall there are fewer differences between the methods in terms of allocated rates.
# The biggest exceptions are for OB_TRW in the GOA and BSAI in 2021, where all methods overestimated rates relative to 'actual'

#======================================================================================================================#
## Compare ----
#======================================================================================================================#

### Proximity ----

og_prox_dt <- og_prox$ispn_dt[SAMPLE_RATE == 0.15, .(ADP, STRATA, BSAI_GOA, STRATA_N, ISPN)]
boot_prox_dt <- rbind(
  cbind(METHOD = "swr_1", swr_1_prox_dt[, .(ADP, STRATA, BSAI_GOA, PROX = ISPN)]),
  cbind(METHOD = "swor_3", swor_3_prox_dt[, .(ADP, STRATA, BSAI_GOA, PROX = ISPN)]),
  cbind(METHOD = "swor_2", swor_2_prox_dt[, .(ADP, STRATA, BSAI_GOA, PROX = ISPN)]),
  cbind(METHOD = "swor_1", swor_1_prox_dt[, .(ADP, STRATA, BSAI_GOA, PROX = ISPN)]))
boot_prox_dt[, METHOD := factor(METHOD, levels = c("swr_1", "swor_3", "swor_2", "swor_1"))]
boot_prox_dt[, ACTUAL := og_prox_dt[boot_prox_dt, ISPN, on = .(ADP, STRATA, BSAI_GOA)]]
boot_prox_dt[, DIFF := PROX - ACTUAL]
boot_prox_dt[, PERC_DIFF := (PROX - ACTUAL)/ACTUAL]

# Proximity is much more difficult to predict in the BSAI (strata with fewer trips) than in the GOA. 
plot_prox_diff <- ggplot(boot_prox_dt[ADP >= 2018], aes(x = as.character(ADP), y = DIFF)) + 
  facet_grid(BSAI_GOA ~ STRATA) + 
  geom_hline(yintercept = 0) +
  geom_boxplot(aes(color = METHOD), fill = NA) + 
  geom_text(data = og_prox_dt[ADP >= 2018], aes(label = STRATA_N, y = 0.4), size = 2, vjust = 0) +
  scale_color_manual(values = c(swr_1 = "cyan3", swor_3 = "blue", swor_2 = "purple", swor_1 = "magenta")) + 
  labs(x = "Year", y = "Diff in Proximity (Boot - Actual)", color = "Method") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# Raw difference in proximity for well within 5% for GOA strata, but generally within 10% for BSAI strata, but 
# OB_TRW-BSAI is highly variable and can vary +/- 0.3 points! Very small stratum

plot_prox_perc_diff <- ggplot(boot_prox_dt[ADP >= 2018], aes(x = as.character(ADP), y = PERC_DIFF)) + 
  facet_grid(BSAI_GOA ~ STRATA) + 
  geom_hline(yintercept = 0) +
  geom_boxplot(aes(color = METHOD), fill = NA) + 
  geom_text(data = og_prox_dt[ADP >= 2018], aes(label = STRATA_N, y = 0.4), size = 2, vjust = 0) +
  scale_color_manual(values = c(swr_1 = "cyan3", swor_3 = "blue", swor_2 = "purple", swor_1 = "magenta")) + 
  labs(x = "Year", y = "% Diff in Proximity (Boot - Actual)/Actual", color = "Method") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# Percent difference in proximity can be vastly different for some methods in certain years (especially for 1-year methods
# following a very low effort year).
# How these differences in proximity affect allocation remains to be seen, but given that the allocation will prioritize 
# allocation to these very small strata first, it might not have a huge impact.

# Difference in Prox vs 'true', using mean across iterations
ggplot(boot_prox_dt[ADP >= 2018, .(MEAN_DIFF = mean(DIFF)), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)]) + 
  facet_nested(. ~ STRATA + BSAI_GOA) + 
  geom_hline(yintercept = 0) + 
  geom_line(aes(x = as.character(ADP), y = MEAN_DIFF, color = METHOD, group = interaction(METHOD, STRATA, BSAI_GOA)), linewidth = 1) + 
  scale_color_manual(values = c(swr_1 = "cyan3", swor_3 = "blue", swor_2 = "purple", swor_1 = "magenta")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x = "Year", y = "Prox (Boot - Actual)", color = "Method") 

ggplot(boot_prox_dt[ADP >= 2018, .(MEAN_DIFF = mean(PERC_DIFF)), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)]) + 
  facet_nested(. ~ STRATA + BSAI_GOA) + 
  geom_hline(yintercept = 0) + 
  geom_line(aes(x = as.character(ADP), y = MEAN_DIFF, color = METHOD, group = interaction(METHOD, STRATA, BSAI_GOA)), linewidth = 1) + 
  scale_color_manual(values = c(swr_1 = "cyan3", swor_3 = "blue", swor_2 = "purple", swor_1 = "magenta")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x = "Year", y = "Prox (Boot - Actual)", color = "Method") 

### Rates ----

og_rates_dt <- og_rates[, .(ADP, STRATA, BSAI_GOA, STRATA_N, SAMPLE_RATE)]
boot_rates_dt <- rbind(
  cbind(METHOD = "swr_1", swr_1_rates_dt[, .(ADP, STRATA, BSAI_GOA, SAMPLE_RATE)]),
  cbind(METHOD = "swor_3", swor_3_rates_dt[, .(ADP, STRATA, BSAI_GOA, SAMPLE_RATE)]),
  cbind(METHOD = "swor_2", swor_2_rates_dt[, .(ADP, STRATA, BSAI_GOA, SAMPLE_RATE)]),
  cbind(METHOD = "swor_1", swor_1_rates_dt[, .(ADP, STRATA, BSAI_GOA, SAMPLE_RATE)]))
boot_rates_dt[, METHOD := factor(METHOD, levels = c("swr_1", "swor_3", "swor_2", "swor_1"))]
boot_rates_dt[, ACTUAL := og_rates_dt[boot_rates_dt, SAMPLE_RATE, on = .(ADP, STRATA, BSAI_GOA)]]
boot_rates_dt[, DIFF := SAMPLE_RATE - ACTUAL]
boot_rates_dt[, PERC_DIFF := (SAMPLE_RATE - ACTUAL)/ACTUAL]

plot_rates_diff <- ggplot(boot_rates_dt[ADP >= 2018], aes(x = as.character(ADP), y = DIFF)) + 
  facet_grid(BSAI_GOA ~ STRATA) + 
  geom_hline(yintercept = 0) +
  geom_boxplot(aes(color = METHOD), fill = NA) + 
  geom_text(data = og_rates_dt[ADP >= 2018], aes(label = STRATA_N, y = 0.10), size = 2, vjust = 0) +
  scale_color_manual(values = c(swr_1 = "cyan3", swor_3 = "blue", swor_2 = "purple", swor_1 = "magenta")) + 
  labs(x = "Year", y = "Diff in Sample Rate (Boot - Actual)", color = "Method") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# As far as actual rates go, most have medians within 0.05, or 5%. 

plot_rates_perc_diff <- ggplot(boot_rates_dt[ADP >= 2018], aes(x = as.character(ADP), y = PERC_DIFF)) + 
  facet_grid(BSAI_GOA ~ STRATA) + 
  geom_hline(yintercept = 0) +
  geom_boxplot(aes(color = METHOD), fill = NA) + 
  geom_text(data = og_rates_dt[ADP >= 2018], aes(label = STRATA_N, y = 0.40), size = 2, vjust = 0) +
  scale_color_manual(values = c(swr_1 = "cyan3", swor_3 = "blue", swor_2 = "purple", swor_1 = "magenta")) + 
  labs(x = "Year", y = "% Diff in Sample Rate (Boot - Actual)/Actual", color = "Method") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# Percent differences for all methods are generally within 10% (with largest exception for 2021 OB-TRW GOA)

# Difference in Rates vs 'true', using mean across iterations
ggplot(boot_rates_dt[ADP >= 2018, .(MEAN_DIFF = mean(DIFF)), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)]) + 
  facet_nested(. ~ STRATA + BSAI_GOA) + 
  geom_hline(yintercept = 0) + 
  geom_line(aes(x = as.character(ADP), y = MEAN_DIFF, color = METHOD, group = interaction(METHOD, STRATA, BSAI_GOA)), linewidth = 1) + 
  scale_color_manual(values = c(swr_1 = "cyan3", swor_3 = "blue", swor_2 = "purple", swor_1 = "magenta")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x = "Year", y = "Sample Rate (Boot - Actual)", color = "Method") 
# Can see that for most strata, predicted rates are generally with 2.5%, mostly within 5%

ggplot(boot_rates_dt[ADP >= 2018, .(MEAN_DIFF = mean(PERC_DIFF)), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)]) + 
  facet_nested(. ~ STRATA + BSAI_GOA) + 
  geom_hline(yintercept = 0) + 
  geom_line(aes(x = as.character(ADP), y = MEAN_DIFF, color = METHOD, group = interaction(METHOD, STRATA, BSAI_GOA)), linewidth = 1) + 
  scale_color_manual(values = c(swr_1 = "cyan3", swor_3 = "blue", swor_2 = "purple", swor_1 = "magenta")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x = "Year", y = "% Diff in Sample Rate (Boot - Actual)", color = "Method")
# In terms of percent differences, all methods generally prescribe rates within 10% of where they should be, exception
# being OB_TRW-GOA in 2021/2022. 

# [NOTE] Why is 2021 higher than expected for all methods? How is that possible with the same budget?
# How can these cost the same? Literally all strata are higher?
og_rates[ADP == 2021, .(STRATA, BSAI_GOA, STRATA_N, SAMPLE_RATE, n, INDEX_COST)]
swr_1_rates_dt[ADP == 2021, .(SAMPLE_RATE = mean(SAMPLE_RATE), n = mean(n), INDEX_COST = mean(INDEX_COST)), keyby = .(ADP, STRATA, BSAI_GOA, STRATA_N)]
# TODO Adding d to the output of rates would be useful.

# calculate_cost_prox(x, cost_params, allo_lst, max_budget)$INDEX_COST
# Is trip duration vastly different in 2020 vs 2021? I see this pattern in all methods, regardless of # of years used in bootstrapping.
dcast(fixed_fmp.allo_lst$effort, STRATA ~ ADP, value.var = "TRP_DUR")
# 2021 was actually higher than 2020, so 'realized' rates would be lower, leading to overestimates in my bootstrapping
plot_trip_duration <- ggplot(fixed_fmp.allo_lst$effort[!(STRATA %like% "ZERO|EM_TRW")], aes(x = ADP, y = TRP_DUR, color = STRATA)) + geom_line() + geom_point() +
  labs(x = "Year", y = "Average Trip Duration (days)")
# Trip duration definitely jumped sine 2019, but mostly for BSAI trips.
# This kind of phenomenon isn't something we can ever plan for. # of trips is one thing, but change in fishing behavior itself
# is another.

### Bias/Variance in difference in Proximity ----
# boot_prox_dt[ADP >= 2018, mean(DIFF), keyby = .(METHOD)]  # swor_3 had the highest bias. swor_1 had the least
# boot_prox_dt[ADP >= 2018, var(DIFF), keyby = .(METHOD)]   # swor_2 had the least variance. swor_1 and swr_1 had the highest
# 
# boot_prox_dt[ADP >= 2018, mean(PERC_DIFF), keyby = .(METHOD)] # swor_1 best, then swr_1
# boot_prox_dt[ADP >= 2018, var(PERC_DIFF), keyby = .(METHOD)]  # swor_2 best, then swor_3

boot_prox_dt[ADP >= 2018, mean(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, sum(V1), keyby = .(METHOD)]  # swor_1 least bias, worse with more years
boot_prox_dt[ADP >= 2018, var(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, sum(V1), keyby = .(METHOD)]   # swor_1 least variance, gets worse with more years

boot_prox_dt[ADP >= 2018, mean(PERC_DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, sum(V1), keyby = .(METHOD)] # swor_1 best
boot_prox_dt[ADP >= 2018, mean(PERC_DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, sum(V1), keyby = .(METHOD)] # swor_1 best

ggplot(boot_prox_dt[ADP >= 2018, mean(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)], aes(x = interaction(STRATA, BSAI_GOA), y = METHOD, fill = V1)) + 
  geom_tile() + scale_fill_gradient2() + labs(fill = "Bias of Prox") + facet_grid(. ~ ADP) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + scale_y_discrete(limits = rev)
ggplot(boot_prox_dt[ADP >= 2018, var(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)], aes(x = interaction(STRATA, BSAI_GOA), y = METHOD, fill = V1)) + 
  geom_tile() + scale_fill_gradient2() + labs(fill = "Variance of Prox")+ facet_grid(. ~ ADP) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + scale_y_discrete(limits = rev)
# In 2021, Bias of proximity to OB_TRW-BSAI was overestimated by all methods.
# Don't really use 2023, since it's an incomplete year and we're sampling much fewer trips
# All of the variance in difference of proximity is in the BSAI.
# TODO Omit 2023?


# Don't use these, use what is below
# Bias/Variance in raw difference Rates
# boot_rates_dt[ADP >= 2018, mean(DIFF), keyby = .(METHOD)]  # swor_3 had the highest bias. swr_1 actually had the least! swor_1 was better than swor_2 or swor_3
# boot_rates_dt[ADP >= 2018, var(DIFF), keyby = .(METHOD)]   # swor_3 had the least overall variance, followed closely by swor_1. swr_1 had the highest variance
# # The fact that swor_1 hast he least variance is not surprising - It has the fewest number of trips to sample from between iterations, 
# # and because it's not sampling with replacement, 
# boot_rates_dt[ADP >= 2018, mean(PERC_DIFF), keyby = .(METHOD)] # swor_1 best, then swr_1
# boot_rates_dt[ADP >= 2018, var(PERC_DIFF), keyby = .(METHOD)]  # swor_1 best, then swor_3

dcast(boot_rates_dt[ADP >= 2018, round(mean(DIFF), 4), keyby = .(METHOD, ADP)], METHOD ~ ADP, value.var = "V1")
dcast(boot_rates_dt[ADP >= 2018, round(mean(DIFF), 4), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)], METHOD + STRATA + BSAI_GOA ~ ADP, value.var = "V1")

# stratum-specific differences
# Why are all rates in 2021 higher than what was 'idealized'? We should have perfect knowledge of effort...?
ggplot(boot_rates_dt[ADP >= 2018, mean(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)], aes(x = interaction(STRATA, BSAI_GOA), y = METHOD, fill = V1)) + 
  geom_tile() + scale_fill_gradient2() + labs(x = "Strata", y = "Method", fill = "Bias of Rates") + facet_grid(. ~ ADP) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + scale_y_discrete(limits = rev)
# For each year, rates rates are all within 0.08 points, less than 1%. 2022 was actually pretty good , especially for swor_1

# TODO This probably the best plot? Why doesn't it have the least overall variance summarized by method? Cancelled out since the
# variance is calculated across iterations and strata simultanously?
ggplot(boot_rates_dt[ADP >= 2018, var(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)], aes(x = interaction(STRATA, BSAI_GOA), y = METHOD, fill = V1)) + 
  geom_tile() + scale_fill_gradient2() + labs(x= "Strata", y = "Method", fill = "Variance of Rates") + facet_grid(. ~ ADP) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + scale_y_discrete(limits = rev)
# In terms of rates, swor_1 had consistently lower variance of residuals

# Summing variance across strata and years (first taknig aveage across iterations)
boot_rates_dt[ADP >= 2018, mean(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, sum(V1), keyby = .(METHOD)]  # swr_1 least bias, followed by swor_1
boot_rates_dt[ADP >= 2018, var(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, sum(V1), keyby = .(METHOD)]   # swor_1 least variance

boot_rates_dt[ADP >= 2018, mean(PERC_DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, sum(V1), keyby = .(METHOD)] # swr_1 least bias, follwoed by swor_1
boot_rates_dt[ADP >= 2018, var(PERC_DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, sum(V1), keyby = .(METHOD)]  # swor_1 least variance

# TODO summarize these 

cbind(
  boot_prox_dt[ADP >= 2018, mean(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, .(PROX_TOTAL_DIFF = sum(V1)), keyby = .(METHOD)],  # swor_1 least bias, worse with more years
  boot_prox_dt[ADP >= 2018, var(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, .(PROX_VAR_DIFF = sum(V1)), keyby = .(METHOD)][, -"METHOD"],   # swor_1 least variance, gets worse with more years
  boot_prox_dt[ADP >= 2018, mean(PERC_DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, .(PROX_TOTAL_PERC_DIFF = sum(V1)), keyby = .(METHOD)][, -"METHOD"], # swor_1 best
  boot_prox_dt[ADP >= 2018, var(PERC_DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, .(PROX_VAR_PERC_DIFF = sum(V1)), keyby = .(METHOD)][, -"METHOD"], # swor_1 best
  boot_rates_dt[ADP >= 2018, mean(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, .(RATES_TOTAL_DIFF = sum(V1)), keyby = .(METHOD)][, -"METHOD"],  # swr_1 least bias, followed by swor_1
  boot_rates_dt[ADP >= 2018, var(DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, .(RATES_VAR_DIFF = sum(V1)), keyby = .(METHOD)][, -"METHOD"],   # swor_1 least variance
  boot_rates_dt[ADP >= 2018, mean(PERC_DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, .(RATES_TOTAL_PERC_DIFF = sum(V1)), keyby = .(METHOD)][, -"METHOD"], # swr_1 least bias, follwoed by swor_1
  boot_rates_dt[ADP >= 2018, var(PERC_DIFF), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)][, .(RATES_VAR_DIFF = sum(V1)), keyby = .(METHOD)[, -"METHOD"]]  # swor_1 least variance
)

# First, average difference vs actual across iterations (prox and rates)
# Then, across years and strata, calculate the bias (mean difference)
boot_metrics <- rbind(
  melt(
    boot_prox_dt[ADP >= 2018, .(
      METRIC = "PROX", MEAN_DIFF = mean(DIFF), VAR_DIFF = var(DIFF), MEAN_PERC_DIFF = mean(PERC_DIFF), VAR_PERC_DIFF = var(PERC_DIFF)
    ), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)
    ][, lapply(.SD, sum), .SDcols = c("MEAN_DIFF", "VAR_DIFF", "MEAN_PERC_DIFF", "VAR_PERC_DIFF"), keyby = .(METRIC, METHOD)],
    id.vars = c("METRIC", "METHOD")),
  melt(
    boot_rates_dt[ADP >= 2018, .(
      METRIC = "RATES", MEAN_DIFF = mean(DIFF), VAR_DIFF = var(DIFF), MEAN_PERC_DIFF = mean(PERC_DIFF), VAR_PERC_DIFF = var(PERC_DIFF)
    ), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)
    ][, lapply(.SD, sum), .SDcols = c("MEAN_DIFF", "VAR_DIFF", "MEAN_PERC_DIFF", "VAR_PERC_DIFF"), keyby = .(METRIC, METHOD)],
    id.vars = c("METRIC", "METHOD"))
)

# Calculate average difference and percent difference relative to 'actual'
boot_prox_dt[ADP >= 2018, .(
  METRIC = "PROX", MEAN_DIFF = mean(DIFF), MEAN_PERC_DIFF = mean(PERC_DIFF)
), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)
][, .(
  MEAN = mean(MEAN_DIFF),
  MEAN_P = mean(MEAN_PERC_DIFF),
  VAR = var(MEAN_DIFF),
  VAR_P = var(MEAN_PERC_DIFF)), keyby = .(METHOD)]  # swor_1 has the lowest bias but highest variance. Typical bias-variance tradeoff


# TODO USE THESE METRICS
# MSE : sum((estimate - true value)^2) / (n)
boot_prox_dt[
][ADP >= 2018, .(BOOT_MEAN = mean(PROX)), keyby = .(ADP, STRATA, BSAI_GOA, METHOD, ACTUAL)
][, .(
  BIAS = mean(BOOT_MEAN - ACTUAL), 
  MPE = 100  * mean((BOOT_MEAN - ACTUAL)/ACTUAL), 
  MSE = mean((BOOT_MEAN - ACTUAL)^2)),
  keyby = .(METHOD)]
# PROX: swor_1 has the least bias in proximity, but also the highest MSE (but not large difference)


boot_rates_dt[
][ADP >= 2018, .(BOOT_MEAN = mean(SAMPLE_RATE)), keyby = .(ADP, STRATA, BSAI_GOA, METHOD, ACTUAL)
][, .(
  BIAS = mean(BOOT_MEAN - ACTUAL), 
  MPE = 100  * mean((BOOT_MEAN - ACTUAL)/ACTUAL),
  MSE = mean((BOOT_MEAN - ACTUAL)^2)),
  keyby = .(METHOD)]


boot_rates_dt[
][ADP >= 2018, .(BOOT_MEAN = mean(SAMPLE_RATE)), keyby = .(ADP, STRATA, BSAI_GOA, METHOD, ACTUAL)
][, .(BIAS = mean(BOOT_MEAN - ACTUAL), MSE = mean((BOOT_MEAN - ACTUAL)^2)), keyby = .(METHOD)]
# RATES: swr_1 has the least bias by far. swor_1 still has low bias, but also with lower MSE than swr_1



# Mean Percentage Error
# 100% / n * sum(actual - predict)/actual


boot_rates_dt[ADP >= 2018, .(
  METRIC = "PROX", MEAN_DIFF = mean(DIFF), MEAN_PERC_DIFF = mean(PERC_DIFF)
), keyby = .(METHOD, ADP, STRATA, BSAI_GOA)
][, .(
  MEAN = mean(MEAN_DIFF),
  MEAN_P = mean(MEAN_PERC_DIFF),
  VAR = var(MEAN_DIFF),
  VAR_P = var(MEAN_PERC_DIFF)), keyby = .(METHOD)]  # swor_1 has the lowest bias but highest variance. Typical bias-variance tradeoff
# For swor_1, realized rates are on average within 0.35%

# FIXME I DONT THINK THIS IS RIGHT
# Scale weighted value relative to maximum of each metric. 
boot_metrics[, w_value := sign(value) * (abs(value) / max(abs(value))), keyby = .(METRIC, variable)]
boot_metrics[, MEAN_VAR := gsub("_DIFF|_PERC_DIFF", "", variable)]
boot_metrics[, RAW_PERC := ifelse(variable %like% "PERC", "PERC_DIFF", "RAW_DIFF")]
boot_metrics[, RAW_PERC := factor(RAW_PERC, levels = c("RAW_DIFF", "PERC_DIFF"))]
plot_boot_metrics <- ggplot(boot_metrics, aes(x = RAW_PERC, y = METHOD, fill = w_value)) + 
  facet_nested(. ~ METRIC + MEAN_VAR) + geom_tile() + 
  scale_fill_gradient2() + scale_y_discrete(limits = rev) + 
  labs(x = "Total (sum) difference relative to 'Actual'", y = "Method", fill = "Relative Difference") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  geom_text(aes(label = round(value, 3)), size = 3)
# For either metric, zero is the best value, and the metric with the farthest value from zero gets a -1 or 1
plot_boot_metrics
# swor_3 and swor_2 generally have the biggest issues and tend to underestimate proximity. swor_1 had the best predictions
# of proximity, 

#======================================================================================================================#
# Save Outputs ----
#======================================================================================================================#

save(
  effort_prediction_exploration_plots,
  prox1, prox2, prox3, prox4,
  insp1, insp2, insp3, 
  prox_by_rate_1, prox_by_rate_2,
  bootstrap_results_dt, bootstrap_1, bootstrap_2,
  file = "analyses/effort_prediction/effort_prediction_exploration.rdata")


#======================================================================================================================#
# Proportions of fishing effort (trips)_ ----
#======================================================================================================================#

a <- fixed.val_2015_2023_dt[, .(N = uniqueN(TRIP_ID)), keyby = .(ADP, STRATA, BSAI_GOA)]
a[, EM := STRATA %like% "EM"]
a[, STRATUM_COL := paste0(STRATA, "-", BSAI_GOA)]
# With ZERO (large pink portions)
ggplot(a, aes(x = as.character(ADP), y = N, fill = STRATUM_COL)) + 
  geom_col(position = "fill", aes(color = EM)) + 
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "red"))
# without zero
ggplot(a[!(STRATA %like% "ZERO")], aes(x = as.character(ADP), y = N, fill = STRATUM_COL)) + 
  geom_col(position = "fill", aes(color = EM)) + 
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "red"))
# Facetiing, splitting EM out
ggplot(a[!(STRATA %like% "ZERO")], aes(x = as.character(ADP), y = N, fill = STRATUM_COL)) + 
  geom_col(position = "fill", aes(color = EM)) + 
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "red")) + 
  facet_grid(EM ~ .)
# Noticeable increases in proportion of OB_FIXED-GOA since 2019
# without EM_TRW
ggplot(a[!(STRATA %like% "ZERO|EM_TRW")], aes(x = as.character(ADP), y = N, fill = STRATUM_COL)) + 
  geom_col(position = "fill", aes(color = EM)) + 
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "red")) + 
  facet_grid(EM ~ .)
