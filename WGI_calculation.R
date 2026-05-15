############################################################
# WATER GOVERNANCE INDEX (WGI) 
#
#
# Index created with Principal Coponent Analysis (PCA)
# Using panel dataset at the canon level - Ecuador
# Using APA datasets (Agua Potable y Alcantarillado) by INEC (2018-2024)
# Available at: https://www.ecuadorencifras.gob.ec/gad-municipales/

# The WGI has three dimensions grounded in the literature
# Dimension 1 : Institutional and Administrative Capacity
# Dimension 2 : Water Quality and Service Continuity (Technical)
# Dimension 3 : Sanitation and Wastewater Management

############################################################

rm(list = ls())

library(data.table)
library(psych)
library(openxlsx)
library(writexl)
library(ggplot2)
library(dplyr)
library(scales) 

setwd("/Users/mljijon/Desktop/MEDEG/Lund/TFM")

#####################################################
# VARIABLES PER DIMENSION
#####################################################

# --- DIMENSION 1: Institutional & Administrative ---
dim1_vars <- c(
  "MA121",   # Updated org structure - water
  "MA122",   # Updated org structure - sewage
  "MA1101",  # Permanent operator training - water
  "MA1102",  # Permanent operator training - sewage
  "MA1111",  # Admin staff trained - water
  "MA1112",  # Admin staff trained - sewage
  "MA171",   # User satisfaction measurement - water
  "MA172",   # User satisfaction measurement - sewage
  "MA181",   # Consumer registry - water
  "MA182",   # Consumer registry - sewage
  "MA131",   # Construction plans - water
  "MA132",   # Construction plans - sewage
  "MA141",   # Automated accounting - water
  "MA142",   # Automated accounting - sewage
  "MA151",   # Automated billing/collection
  "MA191",   # Infrastructure registry - water
  "MA192"    # Infrastructure registry - sewage
)

# --- DIMENSION 2: Water Supply Quality & Continuity ---
dim2_vars <- c(
  "MA225_RP_C_NH_ZU",  # Hours service/day - urban
  "MA3168",            # Water quality INEN 1108 (MA3167NI in 2017-18)
  "MA3161",            # Has water treatment plant
  "MA3181",            # Has water storage tanks
  "MA31",              # System had technical feasibility study
  "MA25",              # Supply meets system demand
  "MA3211",            # Hydraulic sectorization exists
  "MA3212"             # Hydraulic modeling for network
)

# --- DIMENSION 3: Sanitation & Wastewater ---
dim3_vars <- c(
  "MA451",    # Wastewater treated before discharge
  "MA4311",   # Has sanitary sewage system
  "MA4312",   # Has combined sewage system
  "MA441"     # Has sewage pumping stations
)

all_vars <- c(dim1_vars, dim2_vars, dim3_vars)

#####################################################
# LOAD EACH YEAR (2018 - 2024)

# Standardize variable names
#(some variables have changed names)
#####################################################

# Helper function to select only the variables that exist in the file
safe_select <- function(dt, vars) {
  existing <- intersect(vars, names(dt))
  dt[, ..existing]
}

# 2018
apa_2018 <- fread("INEC_APA_GADM_BDD_ 2018.csv", colClasses = "character") %>%
  as.data.frame()

apa_2018 <- apa_2018 %>%
  rename(CANTON   = IDCANTON,
         MA3168   = MA3167NI,
         # In 2018 MA5311 is "Tiempo promedio de instalacion conexion (dias)"
         # Chage for the tariff variable 
         tiempo   = MA5311,
         MA5311   = MA5211,
         MA521    = MA511,
         MA551    = MA542) %>%
  dplyr::select(CANTON, any_of(all_vars)) %>%
  mutate(year = 2018)

# 2019
apa_2019 <- fread("INEC_APA_GADM_BDD_ 2019.csv", colClasses = "character") %>%
  as.data.frame()

apa_2019 <- apa_2019 %>%
  rename(CANTON   = IDCANTON,
         MA5311   = MA5211,
         MA521    = MA511,
         MA551    = MA542) %>%
  dplyr::select(CANTON, any_of(all_vars)) %>%
  mutate(year = 2019)

# 2020
apa_2020 <- fread("INEC_APA_GADM_BDD_ 2020.csv", colClasses = "character") %>%
  as.data.frame() %>%
  rename(CANTON = IDCANTON) %>%
  dplyr::select(CANTON, any_of(all_vars)) %>%
  mutate(year = 2020)

# 2021
apa_2021 <- fread("INEC_APA_GADM_BDD_ 2021.csv", colClasses = "character") %>%
  as.data.frame() %>%
  rename(CANTON = IDCANTON) %>%
  dplyr::select(CANTON, any_of(all_vars)) %>%
  mutate(year = 2021)

# 2022
apa_2022 <- fread("INEC_APA_GADM_BDD_ 2022.csv", colClasses = "character") %>%
  as.data.frame() %>%
  rename(CANTON = IDCANTON) %>%
  dplyr::select(CANTON, any_of(all_vars)) %>%
  mutate(year = 2022)

# 2023
apa_2023 <- fread("INEC_APA_GADM_BDD_ 2023.csv", colClasses = "character") %>%
  as.data.frame() %>%
  rename(CANTON = IDCANTON) %>%
  dplyr::select(CANTON, any_of(all_vars)) %>%
  mutate(year = 2023)

# 2024
apa_2024 <- fread("INEC_APA_GADM_BDD_ 2024.csv", colClasses = "character") %>%
  as.data.frame() %>%
  rename(CANTON = IDCANTON) %>%
  dplyr::select(CANTON, any_of(all_vars)) %>%
  mutate(year = 2024)

#####################################################
# APPEND INTO LONG PANEL
#####################################################

panel_apa <- bind_rows(apa_2018, apa_2019,
                       apa_2020, apa_2021, 
                       apa_2022, apa_2023, 
                       apa_2024)

#####################################################
# DATA CLEANING AND PREPPING
#####################################################

# Convert variables to numeric:
# Canton identification 
# Year
panel_apa <- panel_apa %>%
  mutate(across(-c(CANTON, year),
                ~ as.numeric(gsub(",", ".", .))))

# Recode binary variables (1=Yes -> 1, 2=No -> 0)
# and normalize the continuous ones
binary_vars <- setdiff(
  all_vars,
  c("MA225_RP_C_NH_ZU",
    "MA225_RP_C_NH_ZR")
)

panel_apa <- panel_apa %>%
  mutate(across(all_of(intersect(binary_vars, names(.))),
                ~ case_when(. == 1 ~ 1,
                            . == 2 ~ 0,
                            TRUE   ~ NA_real_))) %>%
  mutate(
    MA225_RP_C_NH_ZU = MA225_RP_C_NH_ZU / 24
  ) %>%
  mutate(across(any_of(c("MA451", "MA4311", "MA4312", "MA411", "MA431161", "MA441")),
                ~ ifelse(is.na(.), 0, .)))

#####################################################
# CALCULATE THE WGI 
# At the Canton-Year Level (panel)
#####################################################

# Dimension scores for each row (canton-year)
# Mean of the variables
panel_apa_wgi <- panel_apa %>%
  mutate(
    dim1_institutional = 100 * (rowMeans(pick(any_of(dim1_vars)), na.rm = TRUE)),
    dim2_technical     = 100 * (rowMeans(pick(any_of(dim2_vars)), na.rm = TRUE)),
    dim3_sanitation    = 100 * (rowMeans(pick(any_of(dim3_vars)), na.rm = TRUE))
  )

# PCA input
pca_input_panel <- panel_apa_wgi %>%
  dplyr::select(dim1_institutional, dim2_technical, dim3_sanitation)

# Identify complete rows
complete_panel <- complete.cases(pca_input_panel)

# PCA! 
pca_panel_res <- principal(pca_input_panel[complete_panel, ],
                           nfactors = 1, rotate = "none", scores = TRUE)

# Assign and rescale PCA scores [0, 100]
panel_apa_wgi$WGI_pca <- NA_real_
raw_panel_scores <- as.numeric(pca_panel_res$scores)

panel_apa_wgi$WGI_pca[complete_panel] <- 100 * (raw_panel_scores - min(raw_panel_scores)) /
  (max(raw_panel_scores) - min(raw_panel_scores))

# Make Equal-weighted composite WGI for comparison
panel_apa_wgi$WGI_equalweight <- rowMeans(
  panel_apa_wgi[, c("dim1_institutional", "dim2_technical", "dim3_sanitation")],
  na.rm = TRUE
)

# Keep only the main variables
panel_apa_wgi <- panel_apa_wgi %>%
  dplyr::select(
    canton = CANTON,
    year,
    dim1_institutional,
    dim2_technical,
    dim3_sanitation,
    WGI_pca,
    WGI_equalweight
  )

# Make a categorical WGI 
# Four groups given the score
panel_apa_wgi <- panel_apa_wgi %>%
  mutate(
    WGI_cat_abs = cut(WGI_pca, 
                      breaks = c(0, 25, 50, 75, 100), 
                      labels = c("Low (0-25)", "Lower-Mid (25-50)", 
                                 "Upper-Mid (50-75)",  "High (75-100)"), 
                      include.lowest = TRUE),
  )

# Check
# Total counts for all years together
table(panel_apa_wgi$WGI_cat_abs)

# Counts specifically for the year 2018
with(panel_apa_wgi[panel_apa_wgi$year == 2018, ], table(WGI_cat_abs))

# Counts specifically for the year 2024
with(panel_apa_wgi[panel_apa_wgi$year == 2024, ], table(WGI_cat_abs))


#####################################################
# EXPORT
#####################################################
write.csv(panel_apa_wgi,      "WGI_Panel_Long_2018_2024.csv",          row.names = FALSE)

# End


