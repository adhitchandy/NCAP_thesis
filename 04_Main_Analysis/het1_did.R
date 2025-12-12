# ==============================================================================
# Regional Heterogeneity Analysis: Standard DiD (TWFE)
# Master Thesis - Main Econometric Analysis
# ==============================================================================
# Description: 
#   Estimates the impact of NCAP separately for 5 distinct Airsheds (Regions).
#   - Control Group: C1 (Untreated, Pop > 100k, Density > 400)
#   - Output: 5 Event Study Plots + 1 Summary Table
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
library(fixest)
library(ggplot2)
library(broom)
library(purrr)
library(showtext)

# Create Output Directory
OUTPUT_DIR <- "Output_DiD_Regional"
dir.create(OUTPUT_DIR, showWarnings = FALSE)

# 2. Helper Functions -----------------------------------------------------

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

# Merge and Pre-process
df_main <- bind_rows(treated_df, untreated_df) %>%
  filter(area_km2 > 0) %>%
  mutate(
    year_month  = ym(year_month),
    year        = year(year_month),
    group       = ifelse(treated == 1, "Treated", "Untreated"),
    pop_density = population / area_km2
  )

# 4. Airshed Classification -----------------------------------------------

message("\n🌍 Classifying Airsheds...")

# Classification Logic
df_annual <- df_main %>%
  # Aggregate to Annual Level
  group_by(ID, year, treated, population, area_km2, group, 
           state_name, district_name, sd_name) %>%
  summarise(
    mean_pm25   = mean(mean_pm25, na.rm = TRUE),
    pop_density = mean(pop_density, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  mutate(
    post_treatment = ifelse(year >= 2019, 1, 0),
    airshed_region = case_when(
      # 1. Indo-Gangetic Plain (The Critical Zone)
      state_name %in% c("Punjab", "Haryana", "Chandigarh", "NCT Of Delhi", 
                        "Uttar Pradesh", "Bihar", "West Bengal", "Jharkhand", 
                        "Uttarakhand") ~ "IGP",
      
      # 2. Arid & Western (The Dust Zone)
      state_name %in% c("Rajasthan", "Gujarat", "Daman and Diu", 
                        "Dadra and Nagar Haveli") ~ "West",
      
      # 3. Central India (The Buffer)
      state_name %in% c("Madhya Pradesh", "Chhattisgarh", "Maharashtra") ~ "Central",
      
      # 4. South India (The Maritime Zone)
      state_name %in% c("Andhra Pradesh", "Telangana", "Karnataka", "Tamil Nadu", 
                        "Kerala", "Puducherry", "Goa", "Lakshadweep", 
                        "Andaman and Nicobar Islands") ~ "South",
      
      # 5. North-East & East Coast
      state_name %in% c("Odisha", "Assam", "Nagaland", "Meghalaya", "Manipur", 
                        "Mizoram", "Tripura", "Sikkim", "Arunachal Pradesh") ~ "NorthEast_East",
      
      # Others (Himalayan North etc.)
      TRUE ~ "Other"
    )
  )

# Apply Control Group 1 Filter (C1)
df_analysis <- df_annual %>%
  filter(
    year >= 2010,
    # Treated OR (Untreated & Urban Criteria)
    treated == 1 | (treated == 0 & population > 100000 & pop_density > 400)
  ) %>%
  rename(treatment = treated)

# Print Distribution
cat("\nSample Distribution by Region:\n")
print(table(df_analysis$airshed_region, df_analysis$treatment))

# 5. Regional Analysis Function -------------------------------------------

analyze_region_did <- function(region_name) {
  
  cat(paste0("\n--- Analyzing Region: ", region_name, " ---\n"))
  
  # A. Subset Data
  df_reg <- df_analysis %>% filter(airshed_region == region_name)
  
  n_treat <- n_distinct(df_reg$ID[df_reg$treatment == 1])
  n_ctrl  <- n_distinct(df_reg$ID[df_reg$treatment == 0])
  
  if (n_treat == 0) {
    message("⚠️  Skipping: No treated units found.")
    return(NULL)
  }
  
  # B. TWFE DiD Model (Statistics)
  twfe_mod <- feols(mean_pm25 ~ post_treatment * treatment | ID + year, 
                    data = df_reg, cluster = ~ID)
  
  # Extract Stats
  est      <- coef(twfe_mod)["post_treatment:treatment"]
  se       <- se(twfe_mod)["post_treatment:treatment"]
  pval     <- pvalue(twfe_mod)["post_treatment:treatment"]
  ci       <- confint(twfe_mod)
  
  # C. Event Study Model (Plotting)
  es_mod <- feols(mean_pm25 ~ i(year, treatment, ref = 2018) | ID + year, 
                  data = df_reg, cluster = ~ID)
  
  # Prepare Plot Data
  es_res <- broom::tidy(es_mod, conf.int = TRUE) %>%
    filter(grepl("year::", term)) %>%
    mutate(year = as.numeric(gsub(".*year::(\\d+):treatment.*", "\\1", term)))
  
  # D. Generate Plot (Publication Style)
  p <- ggplot(es_res, aes(x = year, y = estimate)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_vline(xintercept = 2018.5, linetype = "dashed", color = "#D55E00", linewidth = 0.8) +
    
    # Confidence Intervals
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  width = 0.2, linewidth = 0.6, color = "grey40") +
    
    # Points
    geom_point(size = 3, color = "black", fill = "white", shape = 21, stroke = 1.2) +
    
    # Styling
    labs(
      title = paste("Region:", region_name),
      y = expression(paste(Delta, " PM"[2.5])),
      x = NULL
    ) +
    theme_minimal(base_family = "Times", base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      plot.title = element_text(face = "bold", hjust = 0.5)
    ) +
    scale_x_continuous(breaks = seq(min(es_res$year), max(es_res$year), 2))
  
  # Save Plot
  plot_file <- file.path(OUTPUT_DIR, paste0("EventStudy_", region_name, ".pdf"))
  ggsave(plot_file, p, width = 6, height = 4, device = cairo_pdf)
  
  # Return Summary Row
  return(data.frame(
    Region      = region_name,
    N_Treated   = n_treat,
    N_Control   = n_ctrl,
    Estimate    = round(est, 3),
    SE          = round(se, 3),
    P_Value     = round(pval, 4),
    Significance = ifelse(pval < 0.01, "***", 
                          ifelse(pval < 0.05, "**", 
                                 ifelse(pval < 0.1, "*", ""))),
    RMSE        = round(sigma(twfe_mod), 2),
    Obs         = nobs(twfe_mod)
  ))
}

# 6. Execution Loop -------------------------------------------------------

regions_list <- c("IGP", "West", "Central", "South", "NorthEast_East")
regional_summary <- map_dfr(regions_list, analyze_region_did)

# 7. Export Summary Table -------------------------------------------------

message("\n📊 Regional Analysis Complete.")
print(regional_summary)

write_csv(regional_summary, file.path(OUTPUT_DIR, "Regional_TWFE_Summary.csv"))
message("✅ Results saved to: ", OUTPUT_DIR)