# ==============================================================================
# Regional Heterogeneity Analysis: Synthetic DiD (SDiD)
# Master Thesis - Main Econometric Analysis
# ==============================================================================
# Description: 
#   Estimates SDiD models separately for 5 distinct Airsheds (Regions).
#   - Control Group: C1 (Untreated, Pop > 100k, Density > 400)
#   - Diagnostics: Detrended RMSPE, Effective Sample Size (N_eff), Top Time Weights.
# ==============================================================================

# 1. Setup & Libraries ----------------------------------------------------

# Automatically set working directory
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
message("📂 Working Directory: ", getwd())

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(synthdid)
library(purrr)
library(showtext)

# Create Output Directory
OUTPUT_DIR <- "Output_Het1_SDiD"
dir.create(OUTPUT_DIR, showWarnings = FALSE)

# 2. Helper Functions (Paths & Fonts) -------------------------------------

# A. Robust File Finder
find_data_file <- function(filename) {
  candidates <- c(
    filename,
    file.path("Data", filename),
    file.path("..", "Data", filename),
    file.path("..", "..", "Data", filename)
  )
  path_found <- candidates[file.exists(candidates)][1]
  if (is.na(path_found)) stop(paste("❌ Critical File Not Found:", filename))
  return(normalizePath(path_found))
}

# B. Font Setup (Times New Roman)
tryCatch({
  font_add("Times", "/System/Library/Fonts/Times.ttc") # MacOS
  # font_add("Times", "C:/Windows/Fonts/times.ttf")    # Windows
  showtext_auto()
}, error = function(e) {
  message("⚠️ Font 'Times' not found. Using default sans-serif.")
})

# 3. Data Loading & Construction ------------------------------------------

message("\n📥 Loading Data...")
treated_path   <- find_data_file("Treated_PM25_Pop_Area.csv")
untreated_path <- find_data_file("Untreated_PM25_Pop_Area.csv")

treated_df   <- read_csv(treated_path, show_col_types = FALSE)
untreated_df <- read_csv(untreated_path, show_col_types = FALSE)

# Combine and Clean
df_monthly <- bind_rows(treated_df, untreated_df) %>%
  filter(area_km2 > 0) %>%
  mutate(
    year_month  = ym(year_month),
    year        = year(year_month),
    pop_density = population / area_km2,
    group       = ifelse(treated == 1, "Treated", "Untreated")
  )

# Aggregate to Annual Level (Preserve Region Info)
df_annual <- df_monthly %>%
  group_by(ID, year, treated, population, area_km2, group, 
           state_name, district_name, sd_name) %>%
  summarise(
    mean_pm25   = mean(mean_pm25, na.rm = TRUE),
    pop_density = mean(pop_density, na.rm = TRUE),
    .groups     = "drop"
  )

# 4. Airshed Classification -----------------------------------------------

message("\n🌍 Classifying Airsheds...")

df_classified <- df_annual %>%
  mutate(
    airshed_region = case_when(
      # 1. Indo-Gangetic Plain
      state_name %in% c("Punjab", "Haryana", "Chandigarh", "NCT Of Delhi", 
                        "Uttar Pradesh", "Bihar", "West Bengal", "Jharkhand", 
                        "Uttarakhand") ~ "IGP",
      # 2. Arid & Western
      state_name %in% c("Rajasthan", "Gujarat", "Daman and Diu", 
                        "Dadra and Nagar Haveli") ~ "West",
      # 3. Central India
      state_name %in% c("Madhya Pradesh", "Chhattisgarh", "Maharashtra") ~ "Central",
      # 4. South India
      state_name %in% c("Andhra Pradesh", "Telangana", "Karnataka", "Tamil Nadu", 
                        "Kerala", "Puducherry", "Goa", "Lakshadweep", 
                        "Andaman and Nicobar Islands") ~ "South",
      # 5. North-East & East Coast
      state_name %in% c("Odisha", "Assam", "Nagaland", "Meghalaya", "Manipur", 
                        "Mizoram", "Tripura", "Sikkim", "Arunachal Pradesh") ~ "NorthEast_East",
      TRUE ~ "Other"
    )
  )

# Filter Control Group 1 (C1: Urban)
df_analysis <- df_classified %>%
  filter(
    year >= 1998,
    treated == 1 | (treated == 0 & population > 100000 & pop_density > 400)
  ) %>%
  rename(treatment = treated)

cat("\nSample Distribution by Region:\n")
print(table(df_analysis$airshed_region, df_analysis$treatment))

# 5. Regional SDiD Function -----------------------------------------------

run_regional_sdid <- function(region_name) {
  
  cat(paste0("\n========================================\n"))
  cat(paste0(" 🔄 PROCESSING REGION: ", region_name, "\n"))
  cat(paste0("========================================\n"))
  
  # A. Subset Data & Setup W Matrix
  df_sub <- df_analysis %>%
    filter(airshed_region == region_name) %>%
    mutate(
      W = ifelse(treatment == 1 & year >= 2019, 1, 0)
    )
  
  # Check sample size
  n_treat <- n_distinct(df_sub$ID[df_sub$treatment == 1])
  if (n_treat == 0) {
    message("⚠️  Skipping: No treated units found in ", region_name)
    return(NULL)
  }
  
  # B. Balance Panel (SDiD requirement)
  all_years <- sort(unique(df_sub$year))
  df_bal <- df_sub %>%
    group_by(ID) %>%
    filter(n_distinct(year) == length(all_years)) %>%
    ungroup()
  
  # C. Build Matrices
  pm <- panel.matrices(
    as.data.frame(df_bal),
    unit      = "ID",
    time      = "year",
    outcome   = "mean_pm25",
    treatment = "W"
  )
  
  # D. Estimate SDiD
  tau_hat <- synthdid_estimate(pm$Y, pm$N0, pm$T0)
  
  # E. Diagnostics: Detrended RMSPE
  # Extract weights and series
  Y  <- pm$Y; N0 <- pm$N0; T0 <- pm$T0
  w_list <- attr(tau_hat, "weights")
  omega  <- as.numeric(w_list$omega)
  omega_ctrl <- omega[1:N0] / sum(omega[1:N0]) # Normalize control weights
  
  # Construct Series
  treated_series   <- colMeans(Y[(N0 + 1):nrow(Y), , drop = FALSE])
  synthetic_series <- as.numeric(t(omega_ctrl) %*% Y[1:N0, , drop = FALSE])
  
  # Detrending (Pre-treatment only)
  pre_idx     <- 1:T0
  res_treated <- resid(lm(treated_series[pre_idx] ~ pre_idx))
  res_synth   <- resid(lm(synthetic_series[pre_idx] ~ pre_idx))
  
  # Calculate RMSPE
  rmspe_trend <- sqrt(mean((res_treated - res_synth)^2, na.rm = TRUE))
  cat("   Pre-treatment Detrended RMSPE:", round(rmspe_trend, 4), "\n")
  
  # F. Inference (Jackknife)
  att_value <- as.numeric(tau_hat)
  att_se    <- sqrt(vcov(tau_hat, method = "jackknife"))
  
  # G. Plotting
  p_base <- plot(tau_hat, overlay = 1, se.method = "jackknife")
  
  p_custom <- p_base +
    geom_vline(xintercept = 2019, linetype = "longdash", color = "black", linewidth = 0.6) +
    labs(
      title = paste("Region:", region_name),
      subtitle = paste("RMSPE:", round(rmspe_trend, 2)),
      x = "Year",
      y = expression(paste("Annual Mean ", PM[2.5]))
    ) +
    theme_minimal(base_family = "Times") +
    theme(
      text = element_text(size = 14),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    scale_x_continuous(breaks = seq(1998, 2023, 5)) +
    scale_color_manual(values = c("black", "blue"), labels = c("Synthetic", "Treated")) +
    scale_linetype_manual(values = c("dashed", "solid"), labels = c("Synthetic", "Treated"))
  
  # Save Plot
  ggsave(file.path(OUTPUT_DIR, paste0("SDiD_Plot_", region_name, ".pdf")), 
         p_custom, width = 7, height = 4, device = cairo_pdf)
  
  # H. Extract Top Predictors (Time Weights)
  lambda_vals <- w_list$lambda
  names(lambda_vals) <- colnames(pm$Y)[1:length(lambda_vals)]
  top_years <- paste(names(sort(lambda_vals, decreasing = TRUE)[1:3]), collapse = ", ")
  
  # Return Summary
  return(data.frame(
    Region      = region_name,
    N_Treated   = n_treat,
    Estimate    = round(att_value, 3),
    SE          = round(att_se, 3),
    P_Value     = round(2 * (1 - pnorm(abs(att_value / att_se))), 4),
    Significance = ifelse(abs(att_value/att_se) > 2.58, "***", 
                          ifelse(abs(att_value/att_se) > 1.96, "**", 
                                 ifelse(abs(att_value/att_se) > 1.65, "*", ""))),
    RMSPE_Trend = round(rmspe_trend, 3),
    Top_Years   = top_years
  ))
}

# 6. Execution & Export ---------------------------------------------------

regions_list <- c("IGP", "West", "Central", "South", "NorthEast_East")

# Run the function for all regions
sdid_summary <- map_dfr(regions_list, run_regional_sdid)

# Print and Save the correct variable
print(sdid_summary)
write_csv(sdid_summary, file.path(OUTPUT_DIR, "Regional_SDiD_Summary.csv"))

message("\n✅ Regional SDiD Analysis Complete. Results saved to: ", OUTPUT_DIR)