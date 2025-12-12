# City-level Effectiveness of NCAP in India

![Language](https://img.shields.io/badge/Language-R-276DC3)
![Status](https://img.shields.io/badge/Status-Replication_Ready-success)

This repository contains the replication materials for the Master Thesis: **"City-level Effectiveness of NCAP in India"**. The analysis evaluates the causal impact of India's flagship air pollution policy on PM2.5 concentrations across 129 treated units using Difference-in-Differences (DiD) and Synthetic Difference-in-Differences (SDiD) estimators.

---

## Quick Start: How to Replicate

**Prerequisites:** You need R (v4.0+) installed. Refer [[#🛠 Dependencies]] for required packages. 

### 1. Get the Code
Clone this repository to your local machine.

### 2. Get the Data
Because the processed panel datasets are large, they are hosted externally.
1.  Download the `Data` folder from **[Google Drive Link](https://drive.google.com/drive/folders/1N5TkNHlr5DuTbk1Qatg7qzC3bD15c7NF?usp=share_link)**.
2.  Unzip and paste the `Data` folder directly into the **root directory** of this repository.

*Your folder structure should look like this:*
```text
/Repo_Root
   ├── Data/                          <-- Paste downloaded folder here
   │    ├── Treated_PM25_Pop_Area.csv
   │    ├── Untreated_PM25_Pop_Area.csv
   │    ├── funds_data.csv
   │    └── ...
   ├── 01_Data_Construction/
   ├── 02_Validation/
   ├── 03_Descriptive_Analysis/
   ├── 04_Main_Analysis/
   └── 05_Robustness/
```

### 3. Run the Analysis
Once the data is placed, you can immediately run the scripts in **Folder 04 (Main Analysis)** without reprocessing raw satellite files.

---

## 📂 Repository Structure

The repository is organised logically to follow the thesis workflow, from raw data processing to robustness checks.

### 1. Data Construction (`/01_Data_Construction`)
> **Note:** Requires raw NetCDF files (see Data Access below). Run in this exact order:

1.  `processing_monthly_satellite_data.R`: Extract raw pollution data for Indian subdistricts.
2.  `yearly_csv_merge.R`: Consolidate chunks into a national timeseries.
3.  `matching_w_ID.R`: Generate keys linking NCAP city names to Census IDs.
4.  `pm25+pop+IDmatching.R`: Merge Timeseries, Population, and Keys to create the Master Panel.
5.  `treated.R` & `untreated.R`: Split Master Panel into final `Treated` and `Untreated` CSVs.

### 2. Validation (`/02_Validation`)
* `ch3_sat_ground_corr.R`: Validates satellite-derived PM2.5 against CPCB ground monitoring station data (Result: $R \approx 0.83$).

### 3. Descriptive Analysis (`/03_Descriptive_Analysis`)
* `ch4_summarystats.R`: Generates main summary statistics tables.
* `ch4_maps.R`: Visualizes PM2.5 distribution and NCAP city locations.
* `yearlyavg_statewise.R`: Computes state-level pollution averages.

### 4. Main Econometric Analysis (`/04_Main_Analysis`)
* **Standard DiD:**
    * `c1.R`: Baseline TWFE (Control Group 1: Urban).
    * `c2.R`: TWFE (Control Group 2: Density Matched).
    * `het1_did.R`: Regional heterogeneity (Standard DiD).
* **Synthetic DiD (SDiD):**
    * `sDiDc1.R`: National-level SDiD estimation.
    * `het1_sdid.R`: Regional-level SDiD estimation.
    * `sDiD_citylevel + rmspe.R`: **(Primary Script)** Granular city-level SDiD with RMSPE diagnostics.
* **Mechanisms:**
    * `metal_analysis_funds.R`: Meta-regression on fund utilization vs. treatment effects.

### 5. Robustness Checks (`/05_Robustness`)
* `placebo_twfe_c1.R`: Placebo tests for Standard TWFE.
* `placebo_regional.R`: Placebo tests for Regional SDiD.
---

## 🛠 Dependencies

You can install all required packages with the following command:

```r
install.packages(c(
  # Core Data & Plotting
  "tidyverse", "lubridate", "stringr", "patchwork", 
  "ggridges", "ggrepel", "ggpubr", "viridis", "hrbrthemes",
  
  # Econometrics
  "fixest", "synthdid", "broom",
  
  # Spatial Analysis
  "sf", "terra", "raster", "spdep", "exactextractr",
  
  # Tables & Formatting
  "kableExtra", "showtext", "ragg"
))
```