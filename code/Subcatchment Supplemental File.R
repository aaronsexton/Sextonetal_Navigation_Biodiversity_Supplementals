

# Selecting the subcatchment level to use in models. 
# The goal is to control for differences in richness and invasive species 
# between smaller and larger subcatchments in our model. 
# So we would like to include the subcatchment classification level 
# (according to the HydroSHEDS database: link).
# Deciding between levels 3 - 7.

library(tidyverse)

# Read in biodiversity file
full  <- read.csv("../fish_modeldf.csv")
str(full)


# Catchments
catches <- read.csv("../sites_in_catchements.csv")

# Send it wide
catches2 <- pivot_wider(catches, names_from = layer, values_from = SUB_AREA)

# Join them
full_forcat <- select(full, station.x, prop_i, Tax_Rich)
full_forcat <- left_join(full_forcat, catches2)


# Look at histograms of size classes
par(mfrow=c(2,3))

hist(full_forcat$hybas_eu_lev03_v1c)
hist(full_forcat$hybas_eu_lev04_v1c)
hist(full_forcat$hybas_eu_lev05_v1c)
hist(full_forcat$hybas_eu_lev06_v1c)
hist(full_forcat$hybas_eu_lev07_v1c)

# Run regressions between functional richness and catchments
summary(lm(full_forcat$Tax_Rich ~ full_forcat$hybas_eu_lev03_v1c))
summary(lm(full_forcat$Tax_Rich ~ full_forcat$hybas_eu_lev04_v1c))
summary(lm(full_forcat$Tax_Rich ~ full_forcat$hybas_eu_lev05_v1c))
summary(lm(full_forcat$Tax_Rich ~ full_forcat$hybas_eu_lev06_v1c))
summary(lm(full_forcat$Tax_Rich ~ full_forcat$hybas_eu_lev07_v1c))

# Run regressions between proportion community invasive and catchments
summary(lm(full_forcat$prop_i ~ full_forcat$hybas_eu_lev03_v1c))
summary(lm(full_forcat$prop_i ~ full_forcat$hybas_eu_lev04_v1c))
summary(lm(full_forcat$prop_i ~ full_forcat$hybas_eu_lev05_v1c))
summary(lm(full_forcat$prop_i ~ full_forcat$hybas_eu_lev06_v1c))
summary(lm(full_forcat$prop_i ~ full_forcat$hybas_eu_lev07_v1c))



# Subcatchment level 6 best as it has most normal distribution, highest R2, lowest p, 

