# Water Governance and Health Outcomes in Ecuador

## Relationship Between Municipal Water Governance and Gastrointestinal Hospitalizations in Ecuador (2018–2024)

**Second Year Master’s Thesis — MSc in Economic Development and Growth (MEDEG)**  
Lund University  
May 2026

**Author:** Maria Laura Jijon

---

## Overview

This repository contains the complete codebase for my second-year master’s thesis examining the relationship between municipal water governance and gastrointestinal hospitalization rates in Ecuador between 2018 and 2024.

The project develops a multidimensional **Water Governance Index (WGI)** using administrative data from Ecuador’s 221 cantons and evaluates its association with waterborne disease outcomes using panel econometric methods.

The analysis combines:
- index construction,
- panel data econometrics,
- public health outcomes,
- and local governance indicators.

---

## Research Question

> Does improved municipal water governance reduce gastrointestinal disease burden in Ecuador?

The thesis evaluates whether stronger local institutional capacity, better service continuity, and improved sanitation management are associated with lower rates of gastrointestinal hospitalizations.

---

## Main Contributions

### 1. Construction of a Novel Water Governance Index (WGI)

The WGI is constructed using:
- **Principal Component Analysis (PCA)**
- **Equal-weight aggregation methods**

The index includes three dimensions:

| Dimension | Description |
|---|---|
| **Dimension 1** | Institutional & Administrative Capacity |
| **Dimension 2** | Water Quality & Service Continuity |
| **Dimension 3** | Sanitation & Wastewater Management |

---

### 2. Panel Econometric Analysis

The empirical strategy uses:
- Canton-year panel data (2018–2024)
- Fixed effects estimation
- Negative Binomial regression models for count outcomes

Main outcome:
- Gastrointestinal hospitalizations

Robustness check:
- Placebo analysis using cardiovascular hospitalizations

---

## Main Findings

The results suggest that:

- Improvements in municipal water governance are associated with reductions in gastrointestinal hospitalization rates.
- The strongest and most consistent effects are found for:
  - **Sanitation infrastructure**
  - **Wastewater management**
  - **Service continuity indicators**

These findings highlight the importance of local governance quality in public health outcomes.

# Data Sources

All data originate from the Instituto Nacional de Estadística y Censos (INEC), Ecuador.

### Administrative Sources

#### APA Surveys
Agua Potable y Alcantarillado (APA) surveys provide:
- water service indicators,
- sanitation coverage,
- institutional management variables,
- wastewater treatment indicators.

#### Camas y Egresos Hospitalarios
Hospital discharge data used to construct:
- gastrointestinal hospitalization rates,
- placebo cardiovascular hospitalization measures.

#### Census 2022
Used for:
- population denominators,
- demographic controls,
- canton-level characteristics.

---

# Methodology

## Water Governance Index (WGI)

The WGI is constructed annually for all 221 Ecuadorian cantons.

Two aggregation strategies are implemented:
1. PCA-based index construction
2. Equal-weight composite indices

The final panel dataset includes:
- overall WGI scores,
- dimension-specific scores,
- yearly governance variation across cantons.

---

## Econometric Specification

The main empirical models use:
- Negative Binomial Fixed Effects estimators,
- canton fixed effects,
- year fixed effects,
- clustered standard errors.

Outcome variables are measured as:
- counts of gastrointestinal hospitalizations,
- hospitalization rates per population.

---

# Replication

The project was developed in **R**.

Recommended packages include:

```r
tidyverse
fixest
MASS
plm
psych
FactoMineR
sandwich
lmtest
readr
dplyr
ggplot2

---

# Repository Structure

```text
tfm_lund_2026/
│
├── README.md
│
├── WGI_calculation.R
│   # Constructs Water Governance Index (PCA + dimensions)
│
├── hospitalizations_construction.R
│   # Builds gastrointestinal hospitalization panel dataset
│
├── Negative Binomial FE Analysis.R
│   # Main econometric estimations
│
├── hosp_cardiac_check.R
│   # Placebo analysis using cardiovascular hospitalizations
│
├── canton_lookup.csv
│   # Canton identifiers and matching table
│
├── WGI_Panel_Long_2018_2024.csv
│   # Generated WGI panel dataset
│
├── egresos_gastro_panel_2018_2024.csv
│   # Gastrointestinal hospitalization panel
│
├── egresos_cardio_panel_2018_2024.csv
│   # Cardiovascular hospitalization panel
│
└── data/
    # Raw input datasets (not included in repository)
    




