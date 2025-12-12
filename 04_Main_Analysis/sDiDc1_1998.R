# ==============================================================================
# National SDiD Analysis: Control Group 1 (Urban)
# Master Thesis - Main Econometric Analysis
# ==============================================================================
# Description: 
#   Estimates the National Average Treatment Effect using Synthetic Difference-in-Differences (SDiD).
#   - Control Group: C1 (Untreated, Pop > 100k, Density > 400)
#   - Time Window: 1998 - 2023 (Annual Panel)
#   - Diagnostics: Jackknife Inference, Unit Weights, Time Weights, Detrended RMSPE.
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
library(showtext)

# Define Output Directory
OUTPUT_DIR <- "Output_SDiD_C1"
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

# Construct Monthly DataFrame
df_monthly <- bind_rows(treated_df, untreated_df) %>%
  filter(area_km2 > 0) %>%
  mutate(
    year_month  = ym(year_month),
    year        = year(year_month),
    pop_density = population / area_km2,
    group       = ifelse(treated == 1, "Treated", "Untreated")
  )

# Aggregate to ANNUAL Panel
df_annual <- df_monthly %>%
  group_by(ID, year, treated, population, area_km2, group) %>%
  summarise(
    mean_pm25   = mean(mean_pm25, na.rm = TRUE),
    pop_density = mean(pop_density, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  mutate(
    post_treatment = ifelse(year >= 2019, 1, 0)
  )

# Filter Control Group 1 (Urban)
df_analysis <- df_annual %>%
  filter(
    year >= 1998,
    treated == 1 | (treated == 0 & population > 100000 & pop_density > 400)
  ) %>%
  rename(treatment = treated)

cat("\n=== Analysis Sample Summary ===\n")
print(table(df_analysis$treatment))

# 4. SDiD Estimation ------------------------------------------------------

message("\n📊 Estimating Synthetic DiD...")

# A. Setup Panel Variables
df_sdid <- df_analysis %>%
  filter(year >= 1998 & year <= 2023) %>%
  mutate(
    # Treatment Matrix W: 1 only for treated units in post-2019
    W = ifelse(treatment == 1 & year >= 2019, 1, 0)
  )

# B. Balance Panel (SDiD requires balanced panel)
all_years <- sort(unique(df_sdid$year))
df_sdid_bal <- df_sdid %>%
  group_by(ID) %>%
  filter(n_distinct(year) == length(all_years)) %>%
  ungroup()

# C. Build Matrices
pm <- panel.matrices(
  as.data.frame(df_sdid_bal),
  unit      = "ID",
  time      = "year",
  outcome   = "mean_pm25",
  treatment = "W"
)

# D. Estimate Tau
tau_hat <- synthdid_estimate(pm$Y, pm$N0, pm$T0)

# E. Inference (Jackknife)
message("   Running Jackknife Inference (may take a moment)...")
att_value <- as.numeric(tau_hat)
att_se    <- sqrt(vcov(tau_hat, method = "jackknife"))
att_ci_l  <- att_value - 1.96 * att_se
att_ci_u  <- att_value + 1.96 * att_se

# Print Results
cat("\n------------------------------------------------\n")
cat("SDiD RESULTS (National C1)\n")
cat("------------------------------------------------\n")
cat("ATT Estimate:   ", round(att_value, 4), "\n")
cat("Standard Error: ", round(att_se, 4), "\n")
cat("95% CI:         [", round(att_ci_l, 4), ", ", round(att_ci_u, 4), "]\n")

# Save Results CSV
results_df <- data.frame(
  Metric = c("ATT", "SE", "CI_Lower", "CI_Upper"),
  Value = c(att_value, att_se, att_ci_l, att_ci_u)
)
write_csv(results_df, file.path(OUTPUT_DIR, "SDiD_Results_Summary.csv"))

# 5. Visualization --------------------------------------------------------

message("\n📈 Generating Plots...")

# Base Plot Object (with overlay)
p_base <- plot(tau_hat, overlay = 1, se.method = "jackknife")

# Customizing Plot
p_custom <- p_base +
  geom_vline(xintercept = 2018.5, linetype = "longdash", color = "black", linewidth = 0.6) +
  scale_x_continuous(breaks = c(seq(1998, 2018, 4), 2019, 2023)) +
  labs(
    x = "Year",
    y = expression(paste("Annual Mean ", PM[2.5], " (", mu, "g/", m^3, ")")),
    title = NULL
  ) +
  theme_minimal(base_family = "Times") +
  theme(
    text = element_text(size = 14, family = "Times"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_color_manual(values = c("black", "#0072B2"), labels = c("Synthetic Control", "Treated")) +
  scale_linetype_manual(values = c("dashed", "solid"), labels = c("Synthetic Control", "Treated"))

# Save Plot
ggsave(file.path(OUTPUT_DIR, "SDiD_Plot_National.pdf"), p_custom, width = 8, height = 5, dpi = 300)

# 6. Diagnostics: RMSPE & Detrending --------------------------------------

message("\n🔍 Calculating RMSPE (Trend Fit)...")

# Setup for manual calculation
Y  <- pm$Y
N0 <- pm$N0
T0 <- pm$T0

# Extract Weights
w_list <- attr(tau_hat, "weights")
omega  <- w_list$omega
# Normalize Omega
omega <- omega / sum(omega)

# Construct Series
treated_series   <- colMeans(Y[(N0 + 1):nrow(Y), , drop = FALSE])
synthetic_series <- as.numeric(t(omega) %*% Y[1:N0, , drop = FALSE])

# Pre-treatment Detrending
pre_idx       <- 1:T0
time_pre      <- pre_idx
res_treated   <- resid(lm(treated_series[pre_idx] ~ time_pre))
res_synth     <- resid(lm(synthetic_series[pre_idx] ~ time_pre))

# Calculate RMSPE
rmspe_trend <- sqrt(mean((res_treated - res_synth)^2))

cat("Pre-treatment RMSPE (Detrended): ", round(rmspe_trend, 4), "\n")

# 7. Extract Weights (Donor Analysis) -------------------------------------

message("\n⚖️  Extracting Donor Weights...")

# A. Unit Weights (Omega)
unit_ids <- rownames(Y)[1:N0]
omega_df <- data.frame(ID = unit_ids, weight = as.numeric(omega)) %>%
  filter(weight > 0.001) %>% # Filter negligible weights
  arrange(desc(weight))

# Add Metadata
unit_info <- df_monthly %>%
  dplyr::select(ID, sd_name, district_name, state_name) %>%
  distinct(ID, .keep_all = TRUE)

omega_full <- omega_df %>% left_join(unit_info, by = "ID")

# Save Top Donors
write_csv(omega_full, file.path(OUTPUT_DIR, "SDiD_Donor_Weights.csv"))

# B. Time Weights (Lambda)
lambda_vals <- w_list$lambda
time_weights_df <- data.frame(
  Year = as.numeric(colnames(Y))[1:length(lambda_vals)],
  Weight = as.numeric(lambda_vals)
) %>%
  filter(Weight > 0.001) %>%
  arrange(desc(Weight))

write_csv(time_weights_df, file.path(OUTPUT_DIR, "SDiD_Time_Weights.csv"))

message("✅ SDiD Analysis Complete. Results saved to: ", OUTPUT_DIR)