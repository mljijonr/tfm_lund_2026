#####################################################
# FINAL ANALYSIS 
#
#
# Relationship between Water Governance - measured through WGI
# With health outcomes - measured through hospitalizations
# In the 221 cantons of Ecuador from 2018-2024
#
# Panel data structure
# Negative Binomial Fixed Effects Model
#####################################################

rm(list = ls())

library(MASS)
library(data.table)
library(readr)
library(stringr)
library(stringi)
library(fixest)
library(janitor)
library(tidyverse) 
library(glmmTMB)   


setwd("/Users/mljijon/Desktop/MEDEG/Lund/TFM")


#####################################################
# DATABASES and MERGE
#####################################################

# Canton and Province ookup File
canton_lookup<-fread("canton_lookup.csv")
canton_lookup <- canton_lookup %>%
  rename(canton = canton_code,
         province = prov_code)

# WGI 
wgi_panel <- fread("WGI_Panel_Long_2018_2024.csv")

# Hospitalizations (gastrointestinal)
# For main outcome variable
egresos_panel <- fread("egresos_gastro_panel_2018_2024.csv")
egresos_panel$province <- NULL

# Hospitalizations (cardiovascular)
# For robustness check - placebo
egresos_cardio_panel <- fread("egresos_cardio_panel_2018_2024.csv")
egresos_cardio_panel$province <- NULL

# Available hospital beds dataset
camas_panel <- fread("camas_panel_2018_2024.csv")

# Controls from the Census
controls_panel <-fread("census_variables_2018_2024.csv")

# Merge into single panel dataset
outcome_panel <- wgi_panel %>%
  left_join(egresos_panel, by = c("canton", "year")) %>%
  left_join(egresos_cardio_panel, by = c("canton", "year")) %>%
  left_join(camas_panel, by = c("canton", "year")) %>%
  left_join(canton_lookup, by = "canton") %>%
  left_join(controls_panel, by = c("canton", "year"))

# Look for NAs in the variables
sum(is.na(outcome_panel$beds_total))
sum(is.na(outcome_panel$beds_available))
sum(is.na(outcome_panel$hosp_gastro))
sum(is.na(outcome_panel$hosp_under5))
sum(is.na(outcome_panel$hosp_out))
sum(is.na(outcome_panel$hosp_cardio_under5))

# Turn hospitalizations into integer (just in case)
outcome_panel$hosp_cardio <- as.integer(outcome_panel$hosp_cardio)

# Change NAs to 0
outcome_panel$beds_total[is.na(outcome_panel$beds_total)] <- 0
outcome_panel$beds_available[is.na(outcome_panel$beds_available)] <- 0
outcome_panel$hosp_gastro[is.na(outcome_panel$hosp_gastro)] <- 0
outcome_panel$hosp_cardio[is.na(outcome_panel$hosp_cardio)] <- 0
outcome_panel$hosp_under5[is.na(outcome_panel$hosp_under5)] <- 0
outcome_panel$hosp_out[is.na(outcome_panel$hosp_out)] <- 0
outcome_panel$hosp_out_cardio[is.na(outcome_panel$hosp_out_cardio)] <- 0
outcome_panel$hosp_cardio_under5[is.na(outcome_panel$hosp_cardio_under5)] <- 0


#####################################################
# NEW VARIABLES
#####################################################

# Available Hospital Beds per 1000 in cantons
outcome_panel <- outcome_panel %>%
  mutate(beds_per_1000 = (beds_available / population) * 1000) # Rate per 1,000 people

# Hospitalization out of canton (percentage)
outcome_panel <- outcome_panel %>%
  mutate(hosp_out_pct = (hosp_out / hosp_gastro) * 100) # Rate per 1,000 people

# Hospitalization out of canton (for cardiovascular diseases) 
outcome_panel <- outcome_panel %>%
  mutate(hosp_out_cardio_pct = (hosp_out_cardio / hosp_cardio) * 100) # Rate per 1,000 people

# Turn missings to 0 (also make sure they are integers)
outcome_panel[is.na(hosp_out_cardio_pct), hosp_out_cardio_pct := 0]
outcome_panel$hosp_out_pct[is.na(outcome_panel$hosp_out_pct)] <- 0
outcome_panel$hosp_out_cardio[is.na(outcome_panel$hosp_out_cardio)] <- 0


# Check that the grouping columns are ready
outcome_panel$year_f <- as.factor(outcome_panel$year)
outcome_panel$canton_f <- as.factor(outcome_panel$canton)


#####################################################
# NEGATIVE BINOMIAL FIXED EFFECTS MODELS
#####################################################

# 1. Base Model
# With WGI PCA 
nb_model1 <- fenegbin(
  hosp_gastro ~ WGI_pca + Urban_pct + Poverty_Rate  + 
    beds_per_1000 + hosp_out_pct +
    offset(log(population)) | canton + year,
  cluster = ~ canton,
  data = outcome_panel
)

summary(nb_model1)

# 2. Add interaction term
# With WGI PCA
nb_model2 <- fenegbin(
  hosp_gastro ~ WGI_pca * beds_per_1000 + Urban_pct 
  + Poverty_Rate  + hosp_out_pct +
    offset(log(population)) | canton + year,
  cluster = ~ canton,
  data = outcome_panel
)

summary(nb_model2)

# Robustness checks using WGI equal weight

# 4. Base WGI equal weight 
nb_model3 <- fenegbin(
  hosp_gastro ~ WGI_equalweight + Urban_pct + Poverty_Rate  + 
    beds_per_1000 + hosp_out_pct +
    offset(log(population)) | canton + year,
  cluster = ~ canton,
  data = outcome_panel
)

summary(nb_model3)

# Add interaction term - WGI equal weight
nb_model4 <- fenegbin(
  hosp_gastro ~ WGI_equalweight * beds_per_1000 + Urban_pct 
  + Poverty_Rate  + hosp_out_pct +
    offset(log(population)) | canton + year,
  cluster = ~ canton,
  data = outcome_panel
)

summary(nb_model4)

#####################################################
# MODELS - BY DIMENSION
#####################################################

# 5. Disaggregation of the WGI dimensions
nb_model5 <- fenegbin(
  hosp_gastro ~ dim1_institutional + dim2_technical + 
    dim3_sanitation + Urban_pct + Poverty_Rate  + 
    beds_per_1000 + hosp_out_pct +
    offset(log(population)) | canton + year,
  cluster = ~ canton,
  data = outcome_panel
)

summary(nb_model5)

# 6. Add interaction term for each dimension
nb_model6 <- fenegbin(
  hosp_gastro ~ dim3_sanitation * beds_per_1000 + dim2_technical * beds_per_1000 
  + dim1_institutional * beds_per_1000 + Urban_pct + Poverty_Rate   
  + hosp_out_pct +
    offset(log(population)) | canton + year,
  cluster = ~ canton,
  data = outcome_panel
)

summary(nb_model6)

#####################################################
# ROBUSTNESS CHECK - PLACEBO
#####################################################
# As a robustness check, change outcome variable to cardiovascular hospitalizations
# This is a placebo test, we expect no relationship

# 1. Base Model
# with WGI PCA
rc_model1 <- fenegbin(
  hosp_cardio ~ WGI_pca + Urban_pct + Poverty_Rate  + 
    beds_per_1000 + hosp_out_cardio_pct +
    offset(log(population)) | canton + year,
  cluster = ~ canton,
  data = outcome_panel
)

summary(rc_model1)

# 2. Add interaction
# with WGI PCA
rc_model2 <- fenegbin(
  hosp_cardio ~ WGI_pca * beds_per_1000 + Urban_pct 
  + Poverty_Rate  + hosp_out_cardio_pct +
    offset(log(population)) | canton + year,
  cluster = ~ canton,
  data = outcome_panel
)

summary(rc_model2)

# 3. Base model with WGI equal weight
rc_model3 <- fenegbin(
  hosp_cardio ~ WGI_equalweight + Urban_pct + Poverty_Rate  + 
    beds_per_1000 + hosp_out_cardio_pct +
    offset(log(population)) | canton + year,
  cluster = ~ canton,
  data = outcome_panel
)

summary(rc_model3)

# 4. Add interaction term
rc_model4 <- fenegbin(
  hosp_cardio ~ WGI_equalweight * beds_per_1000 + Urban_pct 
  + Poverty_Rate  + hosp_out_cardio_pct +
    offset(log(population)) | canton + year,
  cluster = ~ canton,
  data = outcome_panel
)

summary(rc_model4)


# End
