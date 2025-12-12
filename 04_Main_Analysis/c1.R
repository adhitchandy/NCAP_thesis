# ==============================================================================
# Control Group 1: Untreated, pop > 100k, density > 400 (ANNUAL PANEL)
# Master Thesis - Main Econometric Analysis
# ==============================================================================
# Description: 
#   Estimates the standard Two-Way Fixed Effects (TWFE) model using 
#   Control Group 1 (Urban centers).
#   - Filter: Population > 100,000 AND Density > 400
#   - Time Window: 2010 - 2023
#   - Treatment Start: 2019
# ==============================================================================

# 1. Setup & Libraries ----------------------------------------------------

# Automatically set working directory to the script's location
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
library(showtext)

# Define Control Group ID for file naming
CONTROL_GROUP_ID <- "c1"

# Create output directory
OUTPUT_DIR <- paste0("Output_", CONTROL_GROUP_ID)
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

# B. Font Setup
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

# Merge and Pre-process Monthly Data
df_monthly <- bind_rows(treated_df, untreated_df) %>%
  filter(area_km2 > 0) %>%                        # Drop invalid areas
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
    mean_pm25   = mean(mean_pm25, na.rm = TRUE),   # Annual average PM2.5
    pop_density = mean(pop_density, na.rm = TRUE), # Safe aggregation
    .groups     = "drop"
  ) %>%
  mutate(
    post_treatment = ifelse(year >= 2019, 1, 0),
    time_id        = year                          # Time Fixed Effect
  )

# 4. Apply Control Group 1 Filter -----------------------------------------

# Filter: Year >= 2010 AND (Treated OR (Untreated & Urban Criteria))
df_analysis <- df_annual %>%
  filter(
    year >= 2010,
    treated == 1 | (treated == 0 & population > 100000 & pop_density > 400)
  ) %>%
  rename(treatment = treated)

# Print Dataset Dimensions
cat("\n=== Dataset Summary (C1 Analysis) ===\n")
cat("Observations:", nrow(df_analysis), "\n")
cat("Unique Cities:", n_distinct(df_analysis$ID), "\n")
cat("  - Treated:", n_distinct(df_analysis$ID[df_analysis$treatment == 1]), "\n")
cat("  - Control:", n_distinct(df_analysis$ID[df_analysis$treatment == 0]), "\n")
cat("Time Window:", min(df_analysis$year), "to", max(df_analysis$year), "\n")

# 5. TWFE Difference-in-Differences Model ---------------------------------

message("\n📊 Running TWFE DiD Model...")

twfe_main <- feols(
  mean_pm25 ~ post_treatment * treatment | ID + time_id,
  data    = df_analysis,
  cluster = ~ ID
)

print(summary(twfe_main))

# Extract Key Stats
ate_val  <- coef(twfe_main)["post_treatment:treatment"]
ate_se   <- se(twfe_main)["post_treatment:treatment"]
ate_pval <- pvalue(twfe_main)["post_treatment:treatment"]

# 6. Event Study Model ----------------------------------------------------

message("\n📈 Running Event Study Model...")

eventstudy_model <- feols(
  mean_pm25 ~ i(year, treatment, ref = 2018) | ID + year,
  data    = df_analysis,
  cluster = ~ ID
)

# 7. Visualization: Event Study Plot --------------------------------------

# Extract coefficients
event_results <- broom::tidy(eventstudy_model, conf.int = TRUE) %>%
  filter(grepl("year::", term)) %>%
  mutate(
    year = as.numeric(gsub(".*year::(\\d+):treatment.*", "\\1", term))
  ) %>%
  dplyr::select(year, estimate, conf.low, conf.high)

# Plot
p_event <- ggplot(event_results, aes(x = year, y = estimate)) +
  # Zero line
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.5) +
  # Treatment line
  geom_vline(xintercept = 2018.5, linetype = "dashed", colour = "#D55E00", linewidth = 0.8) +
  # Confidence Intervals
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, linewidth = 0.6, colour = "grey30") +
  # Points
  geom_point(size = 3, colour = "#0072B2", fill = "white", shape = 21, stroke = 1.5) +
  # Labels and Theme
  labs(
    x = "Year",
    y = expression(paste("Estimate effect on ", PM[2.5], " (", mu, "g/", m^3, ")")),
    title = "Event Study: Impact of NCAP on PM2.5",
    subtitle = "Control Group 1: Urban (Pop > 100k, Density > 400)"
  ) +
  theme_minimal(base_family = "Times") +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey90", linetype = "dashed"),
    axis.line.x = element_line(colour = "black")
  ) +
  scale_x_continuous(breaks = seq(2010, 2023, 2))

# 8. Export Results -------------------------------------------------------

message("\n💾 Exporting Results to: ", OUTPUT_DIR)

# A. Save Tables (TXT/CSV)
etable(
  twfe_main, eventstudy_model,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
  digits = 3,
  file = file.path(OUTPUT_DIR, "Regression_Table_C1.txt"),
  replace = TRUE
)

# B. Save Plot
ggsave(
  filename = file.path(OUTPUT_DIR, "Event_Study_Plot_C1.pdf"),
  plot = p_event,
  width = 8, height = 5, dpi = 300
)

# C. Save Balance Table (Console Output)
pre_treatment_bal <- df_analysis %>%
  filter(post_treatment == 0) %>%
  group_by(treatment) %>%
  summarise(
    Mean_PM25 = mean(mean_pm25, na.rm = TRUE),
    Pop_Density = mean(pop_density, na.rm = TRUE),
    Population = mean(population, na.rm = TRUE),
    N_Obs = n()
  )
write_csv(pre_treatment_bal, file.path(OUTPUT_DIR, "Pre_Treatment_Balance_C1.csv"))

message("✅ Analysis Complete.")