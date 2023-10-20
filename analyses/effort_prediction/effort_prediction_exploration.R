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

# TODO UPDATE WITH FINAL_ADP_DATA!


# TODO What about by time?
# TODO What does 2023 look like so far?



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

fixed_fmp.box <- define_boxes_gs(
  fixed.val_2015_2023_dt, space = c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = c("STRATA", "BSAI_GOA"), dmn_cols = c("GEAR"), geom = T, ps_cols = c("GEAR"))

sample_rate_vec <- 0.15
fixed_fmp.prox <- calculate_interspersion_gs(fixed_fmp.box, sample_rate_vec)

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



save(
  effort_prediction_exploration_plots,
  prox1, prox2, prox3, prox4,
  insp1, insp2, insp3, 
  file = "analyses/effort_prediction/effort_prediction_exploration.rdata")



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

fixed.val_2015_2023_dt

