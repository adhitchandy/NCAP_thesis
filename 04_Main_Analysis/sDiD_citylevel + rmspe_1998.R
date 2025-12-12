# ==============================================================================
# City-Level Synthetic DiD & Robustness Check
# Master Thesis - Main Econometric Analysis
# ==============================================================================
# Description: 
#   1. Classifies cities into Airsheds.
#   2. Runs SDiD for *each* treated city using regional donor pools.
#   3. Diagnostics: Bootstrapped Inference + Detrended RMSPE (Trend Fit).
#   4. Visualization: Ridge plot of heterogeneous effects.
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
library(ggridges)
library(showtext)

# Create Output Directory
OUTPUT_DIR <- "Output_CityLevel_SDiD"
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

# Combine and Clean
df_monthly <- bind_rows(treated_df, untreated_df) %>%
  filter(area_km2 > 0) %>%
  mutate(
    year_month  = ym(year_month),
    year        = year(year_month),
    pop_density = population / area_km2,
    group       = ifelse(treated == 1, "Treated", "Untreated")
  )

# Aggregate to Annual Level
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
      state_name %in% c("Punjab", "Haryana", "Chandigarh", "NCT Of Delhi", 
                        "Uttar Pradesh", "Bihar", "West Bengal", "Jharkhand", 
                        "Uttarakhand") ~ "IGP",
      state_name %in% c("Rajasthan", "Gujarat", "Daman and Diu", 
                        "Dadra and Nagar Haveli") ~ "West",
      state_name %in% c("Madhya Pradesh", "Chhattisgarh", "Maharashtra") ~ "Central",
      state_name %in% c("Andhra Pradesh", "Telangana", "Karnataka", "Tamil Nadu", 
                        "Kerala", "Puducherry", "Goa", "Lakshadweep", 
                        "Andaman and Nicobar Islands") ~ "South",
      state_name %in% c("Odisha", "Assam", "Nagaland", "Meghalaya", "Manipur", 
                        "Mizoram", "Tripura", "Sikkim", "Arunachal Pradesh") ~ "NorthEast_East",
      TRUE ~ "Other"
    )
  )

# 5. Define Analysis Pools ------------------------------------------------

# A. Treated Cities (NCAP)
treated_cities <- df_classified %>%
  filter(treated == 1) %>%
  distinct(ID, sd_name, district_name, state_name, airshed_region) %>%
  mutate(
    final_name = coalesce(sd_name, district_name, state_name, paste0("City_", ID))
  ) %>%
  dplyr::select(ID, Name = final_name, Region = airshed_region)

# B. Control Pool (Urban Criteria)
control_pool <- df_classified %>%
  filter(treated == 0 & population > 100000 & pop_density > 400)

cat(paste("✅ Found", nrow(treated_cities), "treated cities to analyze.\n"))

# 6. City-Level SDiD Function ---------------------------------------------

run_city_sdid <- function(target_id, target_name, target_region) {
  
  # A. Setup Data
  target_data <- df_classified %>% filter(ID == target_id)
  donor_data  <- control_pool %>% filter(airshed_region == target_region)
  
  if(nrow(donor_data) == 0) {
    return(data.frame(ID = target_id, Name = target_name, Region = target_region, Status = "No Donors"))
  }
  
  run_data <- bind_rows(target_data, donor_data) %>%
    filter(year >= 1998 & year <= 2023) %>%
    mutate(W = ifelse(ID == target_id & year >= 2019, 1, 0))
  
  # B. Balance Panel
  all_years <- sort(unique(run_data$year))
  run_data_bal <- run_data %>%
    group_by(ID) %>%
    filter(n_distinct(year) == length(all_years)) %>%
    ungroup()
  
  if(nrow(run_data_bal) == 0) return(data.frame(ID = target_id, Name = target_name, Status = "Unbalanced"))
  
  # C. Estimation & Diagnostics
  tryCatch({
    # Matrix Setup
    setup <- panel.matrices(as.data.frame(run_data_bal), unit="ID", time="year", outcome="mean_pm25", treatment="W")
    
    # Estimate
    tau_hat <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
    att_val <- as.numeric(tau_hat)
    
    # --- RMSPE (Trend Fit) ---
    Y <- setup$Y; N0 <- setup$N0; T0 <- setup$T0
    
    # Weights
    w_list <- attr(tau_hat, "weights")
    omega  <- as.numeric(w_list$omega)[1:N0]
    omega  <- omega / sum(omega)
    
    # Series
    treated_series   <- colMeans(Y[(N0 + 1):nrow(Y), , drop = FALSE])
    synthetic_series <- as.numeric(t(omega) %*% Y[1:N0, , drop = FALSE])
    
    # Detrending (Pre-treatment)
    pre_idx     <- 1:T0
    res_treated <- resid(lm(treated_series[pre_idx] ~ pre_idx))
    res_synth   <- resid(lm(synthetic_series[pre_idx] ~ pre_idx))
    rmspe_trend <- sqrt(mean((res_treated - res_synth)^2))
    
    # --- Bootstrap Inference ---
    n_donors <- n_distinct(donor_data$ID)
    se_hat <- NA
    ci_lower <- NA; ci_upper <- NA
    
    if(n_donors >= 3) {
      boots <- replicate(50, { # Reduced to 50 for speed, increase for final
        tryCatch({
          boot_donors <- sample(unique(donor_data$ID), replace = TRUE)
          boot_dat <- bind_rows(
            run_data_bal %>% filter(ID == target_id),
            run_data_bal %>% filter(ID %in% boot_donors)
          )
          b_setup <- panel.matrices(as.data.frame(boot_dat), unit="ID", time="year", outcome="mean_pm25", treatment="W")
          as.numeric(synthdid_estimate(b_setup$Y, b_setup$N0, b_setup$T0))
        }, error = function(e) NA)
      })
      boots <- boots[!is.na(boots)]
      if(length(boots) > 10) {
        se_hat <- sd(boots)
        ci_lower <- att_val - 1.96 * se_hat
        ci_upper <- att_val + 1.96 * se_hat
      }
    }
    
    return(data.frame(
      ID = target_id, Name = target_name, Region = target_region,
      ATT = att_val, CI_lower = ci_lower, CI_upper = ci_upper, SE = se_hat,
      RMSPE_trend = rmspe_trend, N_Donors = n_donors, Status = "Success"
    ))
    
  }, error = function(e) {
    return(data.frame(ID = target_id, Name = target_name, Region = target_region, Status = "Error"))
  })
}

# 7. Execution Loop -------------------------------------------------------

message("\n🚀 Starting iteration through ", nrow(treated_cities), " treated cities...")

set.seed(12345)
city_results <- pmap_dfr(
  list(treated_cities$ID, treated_cities$Name, treated_cities$Region),
  run_city_sdid
)

# Filter Successes
final_results <- city_results %>%
  filter(Status == "Success" & !is.na(ATT))

message("✅ Estimated ", nrow(final_results), " cities successfully.")

# 8. Visualization: Ridge Plot --------------------------------------------

message("\n📊 Generating Ridge Plot...")

# Prepare Data for Plotting
plot_data <- final_results %>%
  mutate(Region = ifelse(Region == "NorthEast_East", "NEE", Region)) %>% # Shorten name
  filter(Region != "Other") %>% # Remove sparse region
  mutate(Region = factor(Region, levels = c("South", "Central", "West", "NEE", "IGP"))) # Order

# Calculate smart breaks
att_range <- range(plot_data$ATT, na.rm = TRUE)
plot_breaks <- pretty(att_range, n = 5)

p_ridge <- ggplot(plot_data, aes(x = ATT, y = Region, fill = Region)) +
  geom_density_ridges(
    scale = 1.3, rel_min_height = 0.01,
    quantile_lines = TRUE, quantiles = 2,
    alpha = 0.8, color = "white"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  scale_fill_brewer(palette = "RdBu", direction = -1) +
  labs(
    x = expression(paste("Estimated Effect on ", PM[2.5], " (", mu, "g/", m^3, ")")),
    y = NULL,
    title = "Distribution of NCAP Effects by Region",
    subtitle = "City-level Synthetic DiD Estimates"
  ) +
  theme_minimal(base_family = "Times") +
  theme(
    legend.position = "none",
    text = element_text(size = 12),
    axis.text.y = element_text(face = "bold", size = 11),
    panel.grid.major.x = element_line(color = "grey90")
  ) +
  scale_x_continuous(breaks = plot_breaks)

# Save Plot
ggsave(file.path(OUTPUT_DIR, "Figure_CityLevel_Heterogeneity.pdf"), 
       p_ridge, width = 7, height = 5, device = cairo_pdf)

# 9. Summary & Export -----------------------------------------------------

message("\n💾 Exporting Results...")

# A. Regional Summary
summary_stats <- final_results %>%
  group_by(Region) %>%
  summarise(
    N = n(),
    Mean_ATT = mean(ATT),
    Median_ATT = median(ATT),
    Pct_Negative = mean(ATT < 0) * 100
  )
write_csv(summary_stats, file.path(OUTPUT_DIR, "Summary_Stats_By_Region.csv"))

# B. Detailed Results (with Significance)
all_results <- city_results %>%
  mutate(
    t_stat = ATT / SE,
    significance = case_when(
      abs(t_stat) > 1.96 ~ "**",
      abs(t_stat) > 1.65 ~ "*",
      TRUE ~ "NS"
    )
  )
write_csv(all_results, file.path(OUTPUT_DIR, "All_City_Results.csv"))

# C. High Quality Matches (Low RMSPE)
hq_results <- all_results %>%
  filter(RMSPE_trend < 1) %>% # Strict fit criterion
  arrange(RMSPE_trend)

write_csv(hq_results, file.path(OUTPUT_DIR, "High_Quality_Estimates.csv"))

message("✅ Analysis Complete. Results saved to: ", OUTPUT_DIR)