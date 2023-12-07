# Craig Faunce
# GOAL - to format power analysis tables for publication in the draft 2024 ADP
# Start 8/25/2023
# End - 

#For reference - from the preliminary rates document:
#Table 1. Preliminary rates and sample sizes from all designs to be considered 
#in the 2024 Draft Annual Deployment Plan. Design is a combination of the 
#stratification and allocation schemes. Method refers to the monitoring method, 
#where Equal is equal rates, SQ is status quo, CWB is cost-weighted boxes, 
#and PROX is proximity. FMP and Gear identify how strata are defined in addition to 
#Method. N is the total number of trips in a stratum, Sample % is the preliminary sample 
#rate given a $4.5M budget, and n is the preliminary sample size 
#(i.e the expected number of monitored trips). 

# We have two analyses 

# Rare species ---------------------------------------------------------------------------------------------------------
load(file = "~/2024 Integrated Analysis/2024_ADP_repo/analyses/power_analyses/Power_analysis_birds_mammals.Rdata")

library(dplyr)
#We want Design, Method, FMP, Gear, N, Sample %, n
names(detect_power_out)
detect_power_out[1:2,]

#Reformat SPECIES_GROUP_CODE https://www.statology.org/dplyr-replace-multiple-values/
detect_power_out <- 
  detect_power_out %>% 
  mutate(Species = recode(SPECIES_GROUP_CODE, 
                          'EJ' = 'Steller SL',
                          'LBRD' = 'Laysan Alb.',
                          'STBR' = 'Short-tailed Alb.'
                          ),
         Bycatch = ceiling(Xbar * STRATA_N)
         )

#Select the order of columns
detect_table <- 
  detect_power_out %>%
  select(Budget = BUDGET, Species, 
         Design = DESIGN, STRATUM_COL, N = STRATA_N, n, 
         BPUE = Xbar, s2, Bycatch, Power = PROBABILITY_TO_DETECT)

#Format the values
detect_table$BPUE <- formatC(detect_table$BPUE, format = "e", digits = 2) #https://stackoverflow.com/questions/39623636/forcing-r-output-to-be-scientific-notation-with-at-most-two-decimals
detect_table$s2 <- formatC(detect_table$s2, format = "e", digits = 2)
detect_table$Power <- round(detect_table$Power, digits = 2)

#Arrange
detect_table <- detect_table %>% arrange(Budget, Species, Design, STRATUM_COL)
detect_table <- rename(detect_table, "Method_Gear-FMP" = STRATUM_COL)
detect_table[1:10,]


## Design Table

#Correct the species
design_detect_power_out <- 
  design_detect_power_out %>% 
  mutate(Species = recode(SPECIES_GROUP_CODE, 
                          'EJ' = 'Steller SL',
                          'LBRD' = 'Laysan Alb.',
                          'STBR' = 'Short-tailed Alb.'
                          ),
         Bycatch = ceiling(Xbar * N)
         )

#Select order of columns
design_detect_table <- 
  design_detect_power_out %>%
  select(Budget = BUDGET, Species, 
         Design = DESIGN, N, n = nt, 
         BPUE = Xbar, s2, Bycatch, Power = PROBABILITY_TO_DETECT)

#Format the values
design_detect_table$BPUE <- formatC(design_detect_table$BPUE, format = "e", digits = 2) #https://stackoverflow.com/questions/39623636/forcing-r-output-to-be-scientific-notation-with-at-most-two-decimals
design_detect_table$s2 <- formatC(design_detect_table$s2, format = "e", digits = 2)
design_detect_table$Power <- round(design_detect_table$Power, digits = 2)

#Arrange
design_detect_table <- design_detect_table %>% arrange(Budget, Species, Design)
design_detect_table[1:10,]

# Monitoring effects ---------------------------------------------------------------------------------------------------
load(file = "~/2024 Integrated Analysis/2024_ADP_repo/analyses/power_analyses/Power_analysis_groundfish.Rdata")

names(monitoring_effects_power_out)
effects_table <- 
  monitoring_effects_power_out %>% 
  select(Budget = BUDGET,
         Metric = METRIC, Design = DESIGN, STRATUM_COL, N = STRATA_N, n, 
         Effect = EFFECT_SIZE, EPM = EFFECT_PCT_OBS, Power = POWER)

#Format the values
effects_table$Effect <- round(effects_table$Effect, digits = 2) 
effects_table$Power <- round(effects_table$Power, digits = 2) 

#Arrange
effects_table <- effects_table %>% arrange(Budget, Design, STRATUM_COL, Metric)
effects_table <- rename(effects_table, "Method_Gear-FMP" = STRATUM_COL)
effects_table[1:10,]

# Cleanup and Save -----------------------------------------------------------------------------------------------------
rm(detect_power_out, design_detect_power_out, monitoring_effects_power_out)

#save.image(file = "~/2024 Integrated Analysis/2024_ADP_repo/analyses/power_analyses/Power_tables.Rdata")

# Export for word ------------------------------------------------------------------------------------------------------

# Following this https://www.rforecology.com/post/exporting-tables-from-r-to-microsoft-word/

#write.csv(design_detect_table,
          "~/2024 Integrated Analysis/2024_ADP_repo/analyses/power_analyses/Power_to_detect_by_design.csv", 
          row.names = F)

#write.csv(effects_table,
          "~/2024 Integrated Analysis/2024_ADP_repo/analyses/power_analyses/Power_effects_by_design.csv", 
          row.names = F)


# Make Figures ---------------------------------------------------------------------------------------------------------
library(ggplot2)
library(stringr)
library(ggh4x) #for facet nested, which I dont even really use.

load(file = "~/2024 Integrated Analysis/2024_ADP_repo/analyses/power_analyses/Power_tables.Rdata")

# Make a figure of the power to detect ---------------------------------------------------------------------------------
for_fig <- detect_table

for_fig <- #Create columns for Allocation and Stratification
  for_fig %>% 
  mutate(Stratification = str_split(Design, "\\.", simplify = T)[,1],
         Allocation = str_split(Design, "\\.", simplify = T)[,2])

for_fig <- #set factor levels
  for_fig %>% mutate(Stratification = factor(Stratification, levels = c("CURRENT", "FMP", "FIXED_FMP")),
                     Allocation = factor(Allocation, levels = c("PROX", "CWB", "STATUS_QUO", "EQUAL"))
  )

myplot.fxn <- function(data_in){
  ggplot(data_in, 
         aes(x = `Method_Gear-FMP`, 
             y = Allocation, 
             fill = Power, 
             label = Bycatch)) + 
    facet_nested(Species ~ Stratification,
                 scales = "free", 
                 space = "free") + 
    #plot all data
    geom_tile() + 
    geom_text() +
    scale_fill_gradient2() + 
    #Just power > 0.8 
    #TODO - I could NOT get these boxes to look right for the mid and high budgets.
#    geom_tile(data = data_in %>% filter(Power >= 0.8),
#              linewidth = 2, colour = 'black', lty = 1, fill = NA) +
    geom_text(data = data_in %>% filter(Power >= 0.8),
              colour = 'white') +
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust = 1, 
                                     vjust = 0.5))}


d1 <- myplot.fxn(for_fig %>% filter(Budget == 3500000))
d1
ggsave(plot = d1, width = 10, height = 5.5, units = "in", filename = "~/2024 Integrated Analysis/2024_ADP_repo/output_figures/detect_low_plot.png")

d2 <- myplot.fxn(for_fig %>% filter(Budget == 4500000))
d2
ggsave(plot = d2, width = 10, height = 5.5, units = "in", filename = "~/2024 Integrated Analysis/2024_ADP_repo/output_figures/detect_mid_plot.png")

d3 <- myplot.fxn(for_fig %>% filter(Budget == 5250000))
ggsave(plot = d3, width = 10, height = 5.5, units = "in", filename = "~/2024 Integrated Analysis/2024_ADP_repo/output_figures/detect_high_plot.png")


# Make a figure of the monitoring effects ------------------------------------------------------------------------------

for_fig <- effects_table

for_fig <- #Create columns for Allocation and Stratification
  for_fig %>% 
  mutate(Stratification = str_split(Design, "\\.", simplify = T)[,1],
         Allocation = str_split(Design, "\\.", simplify = T)[,2])

for_fig <- #set factor levels
  for_fig %>% mutate(Stratification = factor(Stratification, levels = c("CURRENT", "FMP", "FIXED_FMP")),
                     Allocation = factor(Allocation, levels = c("PROX", "CWB", "STATUS_QUO", "EQUAL"))
                     )
  

library(ggh4x) #for facet nested, which I dont even really use.

myplot.fxn <- function(data_in){
ggplot(data_in, 
       aes(x = `Method_Gear-FMP`, 
           y = Allocation, 
           fill = Power, 
           label = round(EPM, digits = 0)),
       lwd = SIZE) + 
  facet_nested(Metric ~ Stratification,
               scales = "free", space = "free") + 
  #plot all data
  geom_tile() + 
  geom_text() +
  scale_fill_gradient2() + 
  #Just power > 0.8
  geom_tile(data = data_in %>% filter(Power >= 0.8),
            linewidth = 2, colour = 'black', lty = 1, fill = NA) +
  geom_text(data = data_in %>% filter(Power >= 0.8),
            colour = 'white') +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = 0.5))}


p1 <- myplot.fxn(for_fig %>% filter(Budget == 3500000))
ggsave(plot = p1, width = 10, height = 5.5, units = "in", filename = "~/2024 Integrated Analysis/2024_ADP_repo/output_figures/effects_low_plot.png")

p2 <- myplot.fxn(for_fig %>% filter(Budget == 4500000))
ggsave(plot = p2, width = 10, height = 5.5, units = "in", filename = "~/2024 Integrated Analysis/2024_ADP_repo/output_figures/effects_mid_plot.png")

p3 <- myplot.fxn(for_fig %>% filter(Budget == 5250000))
ggsave(plot = p3, width = 10, height = 5.5, units = "in", filename = "~/2024 Integrated Analysis/2024_ADP_repo/output_figures/effects_high_plot.png")


