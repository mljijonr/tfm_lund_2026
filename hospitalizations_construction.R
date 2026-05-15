#####################################################
# HOSPTIALIZATIONS DATA
#
#
# Outcome Variable — Gastrointestinal hospitalizations
# from "Camas y Egresos Hospitalarios" database (INEC)
# by canton and year (2018-2024)
# https://www.ecuadorencifras.gob.ec/camas-y-egresos-hospitalarios/
#####################################################

rm(list = ls())

library(data.table)
library(dplyr)
library(openxlsx)
library(dplyr)
library(stringr)
library(stringi)
library(tibble)

setwd("/Users/mljijon/Desktop/MEDEG/Lund/TFM")


################################################################################
# IMPORTANT FUNCTIONS, DATABASES, AND INFO
################################################################################

# Function to normalize texr
# since in some of the DB the cantons/illnesses are in text format, not codified
# The function converts it to lowercase, removes accents, and 
# transliterating characters to plain ASCII
# then it removes the non-alphanumeric characters and extra spaces
# it returns a clean, uniform string suitable for matching 
normalize_text <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("ñ", "n") %>%
    str_replace_all("á", "a") %>%
    str_replace_all("é", "e") %>%
    str_replace_all("í", "i") %>%
    str_replace_all("ó", "o") %>%
    str_replace_all("ú", "u") %>%
    str_replace_all("ü", "u") %>%
    str_replace_all("Á", "a") %>%
    str_replace_all("É", "e") %>%
    str_replace_all("Í", "i") %>%
    str_replace_all("Ó", "o") %>%
    str_replace_all("Ú", "u") %>%
    stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish()
}


# Canton Lookup file
# This DB contains the full list of provinces and cantons with their codes
canton_lookup<-fread("canton_lookup.csv")

# Normalize it
canton_lookup <- canton_lookup %>%
  mutate(
    prov_clean   = normalize_text(prov_name),
    canton_clean = normalize_text(canton_name)
  )


# Focus on Gastrointestinal diseases (waterborne diseases)
# They have ICD-10 codes 
A00 <- "Colera"
A01 <- "Fiebres tifoidea y paratifoidea"
A02 <- "Otras infecciones debidas a Salmonella"
A03 <- "Shigelosis"
A04 <- "Otras infecciones intestinales bacterianas"
A05 <- "Otras intoxicaciones alimentarias bacterianas, no clasificadas en otra parte"
A06 <- "Amebiasis"
A07 <- "Otras enfermedades intestinales debidas a protozoarios"
A08 <- "Infecciones intestinales debidas a virus y otros organismos especificados"
A09 <- "Otras gastroenteritis y colitis de origen infeccioso y no especificado"
A71 <- "Tracoma"
B15 <- "Hepatitis aguda tipo A"
B58 <- "Toxoplasmosis"
B68 <- "Teniasis"
B69 <- "Cisticercosis"
B75 <- "Triquinosis"
B77 <- "Ascariasis"
B78 <- "Estrongiloidiasis"
B79 <- "Tricuriasis"
B80 <- "Enterobiasis"
B81 <- "Otras helmintiasis intestinales, no clasificadas en otra parte"
B82 <- "Parasitosis intestinal, sin otra especificación"

gastro_categories <- c( tolower(A00), tolower(A01), tolower(A02), tolower(A03),
                        tolower(A04), tolower(A05), tolower(A06), tolower(A07),
                        tolower(A08), tolower(A09), tolower(A71), tolower(B15), 
                        tolower(B58), tolower(B68), tolower(B69), tolower(B75),
                        tolower(B77), tolower(B78), tolower(B79), tolower(B80), 
                        tolower(B81), tolower(B82) )

gastro_codes <- c(
  "A00","A01","A02","A03","A04","A05","A06","A07","A08","A09",
  "A71","B15","B58","B68","B69","B75","B77","B78","B79","B80",
  "B81","B82"
)

################################################################################
# READ EACH YEAR'S DATABASE
################################################################################

#######################################
# 2024
#######################################

EH_2024 <- fread("egresos_hospitalarios_2024.csv",
                 select = c("prov_ubi", "cant_ubi",
                            "prov_res", "cant_res",
                            "anio_egr", "anio_ingr",
                            "causa3","con_egrpa", 
                            "cod_edad", "edad"))

# In 2024, causa 3 has the fcodes at the begining of the string
EH_2024 <- EH_2024 %>%
  mutate(
    causa3    = trimws(as.character(causa3)),
    con_egrpa = trimws(as.character(con_egrpa)),
    causa3     = substr(causa3, 1, 3)
  ) %>%
  filter(causa3 %in% gastro_codes)

# Correct typos manually in the cantons list
# remove cases who live outside of the country, i.e. exterior
EH_2024 <- EH_2024 %>%
  filter(prov_res != "Exterior") %>%
  # fix typos 
  mutate(
    cant_res = ifelse(cant_res == "Cotacahi", "Cotacachi", cant_res),
    cant_res = ifelse(cant_res == "La Condordia", "La Concordia", cant_res),
    cant_res = ifelse(cant_res == "Olemdo", "Olmedo", cant_res),
    cant_res = ifelse(cant_res == "Carlos Julio Arrosemena Tola", "Carlos Julio Arosemena Tola", cant_res),
    cant_res = ifelse(cant_res == "Azoques", "Azogues", cant_res),
    cant_res = ifelse(cant_res == "Gnral. Antonio Elizalde", "General Antonio Elizalde", cant_res),
    cant_ubi = ifelse(cant_ubi == "Cotacahi", "Cotacachi", cant_ubi),
    cant_ubi = ifelse(cant_ubi == "La Condordia", "La Concordia", cant_ubi),
    cant_ubi = ifelse(cant_ubi == "Olemdo", "Olmedo", cant_ubi),
    cant_ubi = ifelse(cant_ubi == "Carlos Julio Arrosemena Tola", "Carlos Julio Arosemena Tola", cant_ubi),
    cant_ubi = ifelse(cant_ubi == "Azoques", "Azogues", cant_ubi),
    cant_ubi = ifelse(cant_ubi == "Gnral. Antonio Elizalde", "General Antonio Elizalde", cant_ubi)
  )

# Normalize province and canton
EH_2024 <- EH_2024 %>%
  mutate(
    prov_res_clean   = normalize_text(prov_res),
    canton_res_clean = normalize_text(cant_res),
    prov_ubi_clean   = normalize_text(prov_ubi),
    canton_ubi_clean = normalize_text(cant_ubi)
  )

# Join the residence canton code from the lookup file
EH_2024 <- EH_2024 %>%
  left_join(
    canton_lookup %>% 
      dplyr::select(prov_clean, canton_clean, prov_code, canton_code) %>%
      rename(prov_res_code = prov_code, 
             canton_res_code = canton_code), 
    by = c("prov_res_clean" = "prov_clean", "canton_res_clean" = "canton_clean")
  )

# Join the hospital/clinic canton code from the lookup file
EH_2024 <- EH_2024 %>%
  left_join(
    canton_lookup %>% 
      dplyr::select(prov_clean, canton_clean, prov_code, canton_code) %>%
      rename(prov_ubi_code = prov_code, 
             canton_ubi_code = canton_code), 
    by = c("prov_ubi_clean" = "prov_clean", "canton_ubi_clean" = "canton_clean")
  )


# Create a variable which identifies whether the hospitalization was in the canton of residence or not
# 1 if the resident canton is the same as the hospital canton, 0 otherwise
EH_2024 <- EH_2024 %>% 
  mutate( 
    hosp_out = as.integer(canton_res_code != canton_ubi_code) 
  )

# Create a subset which stores hospitalizations from previous years
# I will then join these to the corresponding year datasets
plus_2023 <- EH_2024 %>%
  filter(anio_ingr == 2023)

# Remove them from this year's data
EH_2024 <- EH_2024 %>%
  filter(anio_ingr != 2023)

# Under 5 patients
EH_2024 <- EH_2024 %>%
  mutate(under5 = ifelse(
    cod_edad %in% c("Días (1 a 28 días de edad)", 
                    "Horas (1 a 23 horas de edad)", 
                    "Meses (1 a 11 meses de edad)") | 
      (cod_edad == "Años (1 a 115 años de edad)" & edad < 5), 
    1, # Result if true
    0  # Result if false
  ))

# Aggregate by canton of residence
# sum the total hospitalizations
# sum the total deaths
# sum the number of hospitalizations out of their residence canton
# sum under 5
egresos_2024 <- EH_2024 %>%
  filter(!is.na(canton_res_code)) %>%
  group_by(canton = canton_res_code,
           province = prov_res_code) %>%       
  summarise(
    hosp_gastro   = n(),
    gastro_deaths = sum(con_egrpa %in% c(
      "Fallecido menos de 48 horas",
      "Fallecido en 48 horas y más"),
      na.rm = TRUE),
    hosp_out = sum(hosp_out),
    under5 = sum(under5),
    .groups = "drop"
  ) %>%
  mutate(year = 2024)

# Turn these variables to integers to seamlessly match later
egresos_2024$canton<-as.integer(egresos_2024$canton)
egresos_2024$province<-as.integer(egresos_2024$province)

# Remove
# rm(EH_2024)

#######################################
# 2023
#######################################

EH_2023 <- fread("egresos_hospitalarios_2023.csv",
                 select = c("prov_ubi", "cant_ubi",
                            "prov_res", "cant_res",
                            "anio_egr", "anio_ingr",
                            "causa3","con_egrpa",
                            "cod_edad", "edad"))

# Recode causa 3 to extract the first 3 characters as the code
EH_2023 <- EH_2023 %>%
  mutate(
    causa3    = trimws(as.character(causa3)),
    con_egrpa = trimws(as.character(con_egrpa)),
    causa3     = substr(causa3, 1, 3)
  ) %>%
  filter(causa3 %in% gastro_codes)

# Correct typos and remove those who live outside the country
EH_2023 <- EH_2023 %>%
  filter(prov_res != "Exterior") %>%
  mutate(
    cant_res = ifelse(cant_res == "Cotacahi", "Cotacachi", cant_res),
    cant_res = ifelse(cant_res == "La Condordia", "La Concordia", cant_res),
    cant_res = ifelse(cant_res == "Olemdo", "Olmedo", cant_res),
    cant_res = ifelse(cant_res == "Carlos Julio Arrosemena Tola", "Carlos Julio Arosemena Tola", cant_res),
    cant_res = ifelse(cant_res == "Azoques", "Azogues", cant_res),
    cant_res = ifelse(cant_res == "Gnral. Antonio Elizalde", "General Antonio Elizalde", cant_res),
    cant_ubi = ifelse(cant_ubi == "Cotacahi", "Cotacachi", cant_ubi),
    cant_ubi = ifelse(cant_ubi == "La Condordia", "La Concordia", cant_ubi),
    cant_ubi = ifelse(cant_ubi == "Olemdo", "Olmedo", cant_ubi),
    cant_ubi = ifelse(cant_ubi == "Carlos Julio Arrosemena Tola", "Carlos Julio Arosemena Tola", cant_ubi),
    cant_ubi = ifelse(cant_ubi == "Azoques", "Azogues", cant_ubi),
    cant_ubi = ifelse(cant_ubi == "Gnral. Antonio Elizalde", "General Antonio Elizalde", cant_ubi)
  )

# Normalize province and canton
EH_2023 <- EH_2023 %>%
  mutate(
    prov_res_clean   = normalize_text(prov_res),
    canton_res_clean = normalize_text(cant_res),
    prov_ubi_clean   = normalize_text(prov_ubi),
    canton_ubi_clean = normalize_text(cant_ubi)
  )

# Join the residence canton code from the lookup file
EH_2023 <- EH_2023 %>%
  left_join(
    canton_lookup %>% 
      dplyr::select(prov_clean, canton_clean, prov_code, canton_code) %>%
      rename(prov_res_code = prov_code, 
             canton_res_code = canton_code), 
    by = c("prov_res_clean" = "prov_clean", "canton_res_clean" = "canton_clean")
  )

# Join the hospital/clinic canton code from the lookup file
EH_2023 <- EH_2023 %>%
  left_join(
    canton_lookup %>% 
      dplyr::select(prov_clean, canton_clean, prov_code, canton_code) %>%
      rename(prov_ubi_code = prov_code, 
             canton_ubi_code = canton_code), 
    by = c("prov_ubi_clean" = "prov_clean", "canton_ubi_clean" = "canton_clean")
  )

# Hospitalizations outside of canton of residence
EH_2023 <- EH_2023 %>%
  mutate(
    hosp_out = as.integer(canton_res_code != canton_ubi_code)
  )

# Subset which stores hospitalizations from previous years to later join
plus_2022 <- EH_2023 %>%
  filter(anio_ingr == 2022)

# Remove them from this year's data
EH_2023 <- EH_2023 %>%
  filter(anio_ingr != 2022)

# Add the cases taken from the 2024 file 
EH_2023 <- bind_rows(EH_2023, plus_2023)

# Under 5 patients
EH_2023 <- EH_2023 %>%
  mutate(under5 = ifelse(
    cod_edad %in% c("Días (1 a 28 días de edad)", 
                    "Horas (1 a 23 horas de edad)", 
                    "Meses (1 a 11 meses de edad)") | 
      (cod_edad == "Años (1 a 115 años de edad)" & edad < 5), 
    1, # Result if true
    0  # Result if false
  ))

# Aggregate by canton of residence
egresos_2023 <- EH_2023 %>%
  filter(!is.na(canton_res_code)) %>%
  group_by(canton = canton_res_code,
           province = prov_res_code) %>%       
  summarise(
    hosp_gastro   = n(),
    gastro_deaths = sum(con_egrpa %in% c(
      "Fallecido menos de 48 horas",
      "Fallecido en 48 horas y más"),
      na.rm = TRUE),
    hosp_out = sum(hosp_out),
    under5 = sum(under5),
    .groups = "drop"
  ) %>%
  mutate(year = 2023)

# Turn these variables to integers 
egresos_2023$canton<-as.integer(egresos_2023$canton)
egresos_2023$province<-as.integer(egresos_2023$province)

# Remove
# rm(EH_2023)

#######################################
# 2022
#######################################

EH_2022 <- fread("egresos_hospitalarios_2022.csv",
                 select = c("prov_ubi", "cant_ubi",
                            "prov_res", "cant_res",
                            "anio_egr", "anio_ingr",
                            "causa3","con_egrpa",
                            "cod_edad", "edad"))

# Recode causa 3 to extract the first 3 characters as the code
EH_2022 <- EH_2022 %>%
  mutate(
    causa3    = trimws(as.character(causa3)),
    con_egrpa = trimws(as.character(con_egrpa)),
    causa3     = substr(causa3, 1, 3)
  ) %>%
  filter(causa3 %in% gastro_codes)

# Correct typos and remove those who live outside the country
EH_2022 <- EH_2022 %>%
  filter(prov_res != "Exterior") %>%
  mutate(
    cant_res = ifelse(cant_res == "Cotacahi", "Cotacachi", cant_res),
    cant_res = ifelse(cant_res == "La Condordia", "La Concordia", cant_res),
    cant_res = ifelse(cant_res == "Olemdo", "Olmedo", cant_res),
    cant_res = ifelse(cant_res == "Carlos Julio Arrosemena Tola", "Carlos Julio Arosemena Tola", cant_res),
    cant_res = ifelse(cant_res == "Azoques", "Azogues", cant_res),
    cant_res = ifelse(cant_res == "Gnral. Antonio Elizalde", "General Antonio Elizalde", cant_res),
    cant_ubi = ifelse(cant_ubi == "Cotacahi", "Cotacachi", cant_ubi),
    cant_ubi = ifelse(cant_ubi == "La Condordia", "La Concordia", cant_ubi),
    cant_ubi = ifelse(cant_ubi == "Olemdo", "Olmedo", cant_ubi),
    cant_ubi = ifelse(cant_ubi == "Carlos Julio Arrosemena Tola", "Carlos Julio Arosemena Tola", cant_ubi),
    cant_ubi = ifelse(cant_ubi == "Azoques", "Azogues", cant_ubi),
    cant_ubi = ifelse(cant_ubi == "Gnral. Antonio Elizalde", "General Antonio Elizalde", cant_ubi)
  )

# Normalize province and canton
EH_2022 <- EH_2022 %>%
  mutate(
    prov_res_clean   = normalize_text(prov_res),
    canton_res_clean = normalize_text(cant_res),
    prov_ubi_clean   = normalize_text(prov_ubi),
    canton_ubi_clean = normalize_text(cant_ubi)
  )

# Join the residence canton code from the lookup file
EH_2022 <- EH_2022 %>%
  left_join(
    canton_lookup %>% 
      dplyr::select(prov_clean, canton_clean, prov_code, canton_code) %>%
      rename(prov_res_code = prov_code, 
             canton_res_code = canton_code), 
    by = c("prov_res_clean" = "prov_clean", "canton_res_clean" = "canton_clean")
  )

# Join the hospital/clinic canton code from the lookup file
EH_2022 <- EH_2022 %>%
  left_join(
    canton_lookup %>% 
      dplyr::select(prov_clean, canton_clean, prov_code, canton_code) %>%
      rename(prov_ubi_code = prov_code, 
             canton_ubi_code = canton_code), 
    by = c("prov_ubi_clean" = "prov_clean", "canton_ubi_clean" = "canton_clean")
  )

# Hospitalizations outside of canton of residence
EH_2022 <- EH_2022 %>%
  mutate(
    hosp_out = as.integer(canton_res_code != canton_ubi_code)
  )

# Subset which stores hospitalizations from previous years to later join
plus_2021 <- EH_2022 %>%
  filter(anio_ingr == 2021)

# Remove them from this year's data
EH_2022 <- EH_2022 %>%
  filter(anio_ingr != 2021)

# Add the cases taken from the previous year file 
EH_2022 <- bind_rows(EH_2022, plus_2022)

# Under 5 patients
EH_2022 <- EH_2022 %>%
  mutate(under5 = ifelse(
    cod_edad %in% c("Días (1 a 28 días de edad)", 
                    "Horas (1 a 23 horas de edad)", 
                    "Meses (1 a 11 meses de edad)") | 
      (cod_edad == "Años (1 a 115 años de edad)" & edad < 5), 
    1, # Result if true
    0  # Result if false
  ))

# Aggregate by canton of residence
egresos_2022 <- EH_2022 %>%
  filter(!is.na(canton_res_code)) %>%
  group_by(canton = canton_res_code,
           province = prov_res_code) %>%       
  summarise(
    hosp_gastro   = n(),
    gastro_deaths = sum(con_egrpa %in% c(
      "Fallecido menos de 48 horas",
      "Fallecido en 48 horas y más"),
      na.rm = TRUE),
    hosp_out = sum(hosp_out),
    under5 = sum(under5),
    .groups = "drop"
  ) %>%
  mutate(year = 2022)

# Turn these variables to integers 
egresos_2022$canton<-as.integer(egresos_2022$canton)
egresos_2022$province<-as.integer(egresos_2022$province)

# Remove
# rm(EH_2022)

#######################################
# 2021
#######################################

EH_2021 <- fread("egresos_hospitalarios_2021.csv",
                 select = c("prov_ubi", "cant_ubi",
                            "prov_res", "cant_res",
                            "anio_egr", "anio_ingr",
                            "causa3","con_egrpa",
                            "cod_edad", "edad"))

# Get the gastrointestinal diseases from the gastro_categories list
# This year doesn't have the initial codes, so find the literal names
# (defined in the begining)
EH_2021 <- EH_2021 %>%
  mutate(causa3    = trimws(tolower(as.character(causa3))),
         con_egrpa = trimws(as.character(con_egrpa)))

EH_2021 <- EH_2021 %>%
  filter(causa3 %in% tolower(gastro_categories))

# Correct typos and remove those who live outside the country
EH_2021 <- EH_2021 %>%
  filter(prov_res != "Exterior") %>%
  mutate(
    cant_res = ifelse(cant_res == "Cotacahi", "Cotacachi", cant_res),
    cant_res = ifelse(cant_res == "La Condordia", "La Concordia", cant_res),
    cant_res = ifelse(cant_res == "Olemdo", "Olmedo", cant_res),
    cant_res = ifelse(cant_res == "Carlos Julio Arrosemena Tola", "Carlos Julio Arosemena Tola", cant_res),
    cant_res = ifelse(cant_res == "Azoques", "Azogues", cant_res),
    cant_res = ifelse(cant_res == "Gnral. Antonio Elizalde", "General Antonio Elizalde", cant_res),
    cant_ubi = ifelse(cant_ubi == "Cotacahi", "Cotacachi", cant_ubi),
    cant_ubi = ifelse(cant_ubi == "La Condordia", "La Concordia", cant_ubi),
    cant_ubi = ifelse(cant_ubi == "Olemdo", "Olmedo", cant_ubi),
    cant_ubi = ifelse(cant_ubi == "Carlos Julio Arrosemena Tola", "Carlos Julio Arosemena Tola", cant_ubi),
    cant_ubi = ifelse(cant_ubi == "Azoques", "Azogues", cant_ubi),
    cant_ubi = ifelse(cant_ubi == "Gnral. Antonio Elizalde", "General Antonio Elizalde", cant_ubi)
  )

# Normalize province and canton
EH_2021 <- EH_2021 %>%
  mutate(
    prov_res_clean   = normalize_text(prov_res),
    canton_res_clean = normalize_text(cant_res),
    prov_ubi_clean   = normalize_text(prov_ubi),
    canton_ubi_clean = normalize_text(cant_ubi)
  )

# Join the residence canton code from the lookup file
EH_2021 <- EH_2021 %>%
  left_join(
    canton_lookup %>% 
      dplyr::select(prov_clean, canton_clean, prov_code, canton_code) %>%
      rename(prov_res_code = prov_code, 
             canton_res_code = canton_code), 
    by = c("prov_res_clean" = "prov_clean", "canton_res_clean" = "canton_clean")
  )

# Join the hospital/clinic canton code from the lookup file
EH_2021 <- EH_2021 %>%
  left_join(
    canton_lookup %>% 
      dplyr::select(prov_clean, canton_clean, prov_code, canton_code) %>%
      rename(prov_ubi_code = prov_code, 
             canton_ubi_code = canton_code), 
    by = c("prov_ubi_clean" = "prov_clean", "canton_ubi_clean" = "canton_clean")
  )

# Hospitalizations outside of canton of residence
EH_2021 <- EH_2021 %>%
  mutate(
    hosp_out = as.integer(canton_res_code != canton_ubi_code)
  )

# Subset which stores hospitalizations from previous years to later join
plus_2020 <- EH_2021 %>%
  filter(anio_ingr == 2020)

# Remove them from this year's data
EH_2021 <- EH_2021 %>%
  filter(anio_ingr != 2020)

# Add the cases taken from the previous year file 
EH_2021 <- bind_rows(EH_2021, plus_2021)

# Under 5 patients
EH_2021 <- EH_2021 %>%
  mutate(under5 = ifelse(
    cod_edad %in% c("Días (1 a 28 días de edad)", 
                    "Horas (1 a 23 horas de edad)", 
                    "Meses (1 a 11 meses de edad)") | 
      (cod_edad == "Años (1 a 115 años de edad)" & edad < 5), 
    1, # Result if true
    0  # Result if false
  ))

# Aggregate by canton of residence
egresos_2021 <- EH_2021 %>%
  filter(!is.na(canton_res_code)) %>%
  group_by(canton = canton_res_code,
           province = prov_res_code) %>%       
  summarise(
    hosp_gastro   = n(),
    gastro_deaths = sum(con_egrpa %in% c(
      "Fallecido menos de 48 horas",
      "Fallecido en 48 horas y más"),
      na.rm = TRUE),
    hosp_out = sum(hosp_out),
    under5 = sum(under5),
    .groups = "drop"
  ) %>%
  mutate(year = 2021)

# Turn to integer
egresos_2021$canton<-as.integer(egresos_2021$canton)
egresos_2021$province<-as.integer(egresos_2021$province)

# Remove
# rm(EH_2021)

#######################################
# 2020
#######################################

EH_2020 <- fread("egresos_hospitalarios_2020.csv",
                 select = c("prov_ubi", "cant_ubi",
                            "prov_res", "cant_res",
                            "anio_egr", "anio_ingr",
                            "causa3","con_egrpa",
                            "cod_edad", "edad"))

# Recode causa 3 to extract the first 3 characters as the code
EH_2020 <- EH_2020 %>%
  mutate(
    causa3 = toupper(trimws(as.character(causa3))),
    causa3  = substr(causa3, 1, 3)
  ) %>%
  filter(causa3 %in% gastro_codes)

# Hospitalizations outside of canton of residence
EH_2020 <- EH_2020 %>%
  mutate(
    hosp_out = as.integer(cant_res != cant_ubi)
  )

# Subset which stores hospitalizations from previous years to later join
plus_2019 <- EH_2020 %>%
  filter(anio_ingr == 2019)

# Remove them from this year's data
EH_2020 <- EH_2020 %>%
  filter(anio_ingr != 2019)

# From the plus_2020 remove the "character" columns to match properly
plus_2020$prov_ubi <- NULL
plus_2020$cant_ubi <- NULL
plus_2020$prov_res <- NULL
plus_2020$cant_res <- NULL

plus_2020$prov_res_clean <- NULL
plus_2020$canton_res_clean <- NULL
plus_2020$prov_ubi_clean <- NULL
plus_2020$canton_ubi_clean <- NULL

plus_2020 <- plus_2020 %>%
  rename(prov_res = prov_res_code,
         cant_res = canton_res_code,
         prov_ubi = prov_ubi_code,
         cant_ubi = canton_ubi_code)

# Recode the condition of leaving the hospital from character to integer
plus_2020 <- plus_2020 %>%
  mutate(
    con_egrpa = trimws(tolower(as.character(con_egrpa))),
    con_egrpa = case_when(
      con_egrpa == "vivo" ~ 1L,
      con_egrpa == "fallecido menos de 48 horas" ~ 2L,
      con_egrpa == "fallecido en 48 horas y más" ~ 3L,
      TRUE ~ NA_integer_
    )
  )

# Convert the variable type
EH_2020$cod_edad <- as.character(EH_2020$cod_edad)

# Add the cases taken from the 2024 file 
EH_2020 <- bind_rows(EH_2020, plus_2020)

# Under 5 patients
EH_2020 <- EH_2020 %>%
  mutate(under5 = ifelse(
    cod_edad %in% c("2", # dias
                    "Meses (1 a 11 meses de edad)",
                    "3", # meses
                    "1") | # horas
      ((cod_edad == "4" | cod_edad =="Años (1 a 115 años de edad)") & edad < 5), 
    1, # Result if true
    0  # Result if false
  ))

# Aggregate by canton of residence
egresos_2020 <- EH_2020 %>%
  filter(!is.na(cant_res)) %>%
  group_by(canton = cant_res,
           province = prov_res) %>%       
  summarise(
    hosp_gastro   = n(),
    gastro_deaths = sum(con_egrpa %in% c("2", "3"),
                        na.rm = TRUE),
    hosp_out = sum(hosp_out),
    under5 = sum(under5),
    .groups = "drop"
  ) %>%
  mutate(year = 2020)

# Remove
# rm(EH_2020)

#######################################
# 2019 
#######################################

EH_2019 <- fread("egresos_hospitalarios_2019.csv",
                 select = c("prov_ubi", "cant_ubi",
                            "prov_res", "cant_res",
                            "anio_egr", "anio_ingr",
                            "causa3","con_egrpa",
                            "cod_edad", "edad"))

# Recode causa 3 to extract the first 3 characters as the code
EH_2019 <- EH_2019 %>%
  mutate(
    causa3 = toupper(trimws(as.character(causa3))),
    causa3  = substr(causa3, 1, 3)
  ) %>%
  filter(causa3 %in% gastro_codes)

# Hospitalizations outside of canton of residence
EH_2019 <- EH_2019 %>%
  mutate(
    hosp_out = as.integer(cant_res != cant_ubi)
  )

# Subset which stores hospitalizations from previous years to later join
plus_2018 <- EH_2019 %>%
  filter(anio_ingr == 2018)

# Remove them from this year's data
EH_2019 <- EH_2019 %>%
  filter(anio_ingr != 2018)

# Convert the variable type
EH_2019$cod_edad <- as.character(EH_2019$cod_edad)
plus_2019$cod_edad <- as.character(plus_2019$cod_edad)

# Add the cases taken from the previous year file
EH_2019 <- bind_rows(EH_2019, plus_2019)

# Under 5 patients
EH_2019 <- EH_2019 %>%
  mutate(under5 = ifelse(
    cod_edad %in% c("2", # dias
                    "3", # meses
                    "1") | # horas
      ((cod_edad == "4") & edad < 5), 
    1, # Result if true
    0  # Result if false
  ))

# Aggregate by canton of residence
egresos_2019 <- EH_2019 %>%
  filter(!is.na(cant_res)) %>%
  group_by(canton = cant_res,
           province = prov_res) %>%       
  summarise(
    hosp_gastro   = n(),
    gastro_deaths = sum(con_egrpa %in% c("2", "3"),
                        na.rm = TRUE),
    hosp_out = sum(hosp_out),
    under5 = sum(under5),
    .groups = "drop"
  ) %>%
  mutate(year = 2019)

# Remove
# rm(EH_2019)

#######################################
# 2018 
#######################################

EH_2018 <- fread("egresos_hospitalarios_2018.csv",
                 select = c("prov_ubi", "cant_ubi",
                            "prov_res", "cant_res",
                            "anio_egr", "anio_ingr",
                            "causa3","con_egrpa",
                            "cod_edad", "edad"))

# Recode causa 3 to extract the first 3 characters as the code
EH_2018 <- EH_2018 %>%
  mutate(
    causa3 = toupper(trimws(as.character(causa3))),
    causa3  = substr(causa3, 1, 3)
  ) %>%
  filter(causa3 %in% gastro_codes)


# Hospitalizations outside of canton of residence
EH_2018 <- EH_2018 %>%
  mutate(
    hosp_out = as.integer(cant_res != cant_ubi)
  )

# Subset which stores hospitalizations from previous years to later join
plus_2017 <- EH_2018 %>%
  filter(anio_ingr == 2017)

# Remove them from this year's data
EH_2018 <- EH_2018 %>%
  filter(anio_ingr != 2017)

# Convert the variable type
EH_2018$cod_edad <- as.character(EH_2018$cod_edad)
plus_2018$cod_edad <- as.character(plus_2018$cod_edad)

# Add the cases taken from the previous year
EH_2018 <- bind_rows(EH_2018, plus_2018)

# Under 5 patients
EH_2018 <- EH_2018 %>%
  mutate(under5 = ifelse(
    cod_edad %in% c("2", # dias
                    "Meses (1 a 11 meses de edad)",
                    "3", # meses
                    "1") | # horas
      ((cod_edad == "4" | cod_edad =="Años (1 a 115 años de edad)") & edad < 5), 
    1, # Result if true
    0  # Result if false
  ))


# Aggregate by canton of residence
egresos_2018 <- EH_2018 %>%
  filter(!is.na(cant_res)) %>%
  group_by(canton = cant_res,
           province = prov_res) %>%       
  summarise(
    hosp_gastro   = n(),
    gastro_deaths = sum(con_egrpa %in% c("2", "3"),
                        na.rm = TRUE),
    hosp_out = sum(hosp_out),
    under5 = sum(under5),
    .groups = "drop"
  ) %>%
  mutate(year = 2018)

# Remove 
# rm(EH_2018)



#####################################################
# APPEND INTO A SINGLE FILE
#####################################################

# Panel database - exclude 2017 
egresos_panel <- bind_rows(egresos_2018,
                           egresos_2019, egresos_2020,
                           egresos_2021, egresos_2022,
                           egresos_2023, egresos_2024)

# Summary Statistics
cat("\nCases by year:\n")
egresos_panel %>%
  group_by(year) %>%
  summarise(
    cantons      = n(),
    total_cases  = sum(hosp_gastro,   na.rm = TRUE),
    total_under5 = sum(under5, na.rm = TRUE),
    total_hosp_out = sum(hosp_out, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# Rename
egresos_panel <- egresos_panel %>%
  rename(hosp_under5 = under5)

# Keep only the main variables 
egresos_panel <- egresos_panel %>%
  dplyr::select(
    canton,  
    province,
    hosp_gastro, 
    hosp_under5,
    hosp_out, 
    year
  )

#####################################################
# EXPORT
#####################################################

# Save
fwrite(egresos_panel, "egresos_gastro_panel_2018_2024.csv")


# End