# ==============================================================================
# Control Group 2: Density-Matched Untreated Units (ANNUAL PANEL)
# Master Thesis - Main Econometric Analysis
# ==============================================================================
# Description: 
#   Estimates the TWFE model using Control Group 2.
#   - Methodology: Dynamically finds a population density cutoff for the control 
#     group that minimizes the difference in mean density vs. the treated group.
#   - Time Window: 2010 - 2023
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
library(tibble)
library(fixest)
library(ggplot2)
library(broom)
library(showtext)

# Define Control Group ID
CONTROL_GROUP_ID <- "c2"
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

# Merge and Pre-process
df_main <- bind_rows(treated_df, untreated_df) %>%
  filter(area_km2 > 0) %>%
  mutate(
    year_month  = ym(year_month),
    year        = year(year_month),
    group       = ifelse(treated == 1, "Treated", "Untreated"),
    pop_density = population / area_km2
  )

# 4. Density Matching Algorithm -------------------------------------------

message("\n🔍 Running Density Matching Grid Search...")

# A. Calculate Treated Mean Density (Pre-2019)
treated_mean_density <- df_main %>%
  filter(treated == 1, year < 2019) %>%
  summarise(avg = mean(pop_density, na.rm = TRUE)) %>%
  pull(avg)

cat("   Target Density (Treated):", round(treated_mean_density, 2), "\n")

# B. Grid Search for Best Cutoff
candidate_cuts <- seq(500, 6000, by = 100)

density_match_df <- bind_rows(lapply(candidate_cuts, function(cutoff) {
  control_subset <- df_main %>%
    filter(year < 2019, treated == 0, pop_density > cutoff)
  
  if(nrow(control_subset) == 0) return(NULL)
  
  tibble(
    cutoff = cutoff,
    n_control = n_distinct(control_subset$ID),
    mean_density = mean(control_subset$pop_density, na.rm = TRUE)
  )
})) %>%
  mutate(diff = abs(mean_density - treated_mean_density)) %>%
  arrange(diff)

# C. Select Best Cutoff
best_cutoff <- density_match_df$cutoff[1]
cat("   ✅ Best Cutoff Found:", best_cutoff, "(Diff:", round(density_match_df$diff[1], 2), ")\n")

# 5. Filter Data & Aggregate ----------------------------------------------

# Filter Monthly Data based on Cutoff
df_c2_monthly <- df_main %>%
  filter(
    year >= 2010,
    treated == 1 | (treated == 0 & pop_density > best_cutoff)
  ) %>%
  mutate(treatment = treated)

# Aggregate to ANNUAL Panel
df_annual <- df_c2_monthly %>%
  group_by(ID, treatment, year) %>%
  summarise(
    mean_pm25      = mean(mean_pm25, na.rm = TRUE),
    population     = first(population),
    area_km2       = first(area_km2),
    pop_density    = first(pop_density),
    .groups        = "drop"
  ) %>%
  mutate(
    post_treatment = ifelse(year >= 2019, 1, 0)
  )

# 6. TWFE Difference-in-Differences Model ---------------------------------

message("\n📊 Running TWFE DiD Model...")

twfe_main <- feols(
  mean_pm25 ~ post_treatment * treatment | ID + year,
  data    = df_annual,
  cluster = ~ ID
)

print(summary(twfe_main))

# 7. Event Study Model ----------------------------------------------------

message("\n📈 Running Event Study Model...")

eventstudy_model <- feols(
  mean_pm25 ~ i(year, treatment, ref = 2018) | ID + year,
  data    = df_annual,
  cluster = ~ ID
)

# 8. Visualization: Event Study Plot --------------------------------------

# Extract coefficients
event_results <- broom::tidy(eventstudy_model, conf.int = TRUE) %>%
  filter(grepl("year::", term)) %>%
  mutate(
    year = as.numeric(gsub(".*year::(\\d+):treatment.*", "\\1", term))
  )

# Plot
p_event <- ggplot(event_results, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.5) +
  geom_vline(xintercept = 2018.5, linetype = "dashed", colour = "#D55E00", linewidth = 0.8) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, linewidth = 0.6, colour = "grey30") +
  geom_point(size = 3, colour = "#009E73", fill = "white", shape = 21, stroke = 1.5) +
  labs(
    x = "Year",
    y = expression(paste("Estimate effect on ", PM[2.5], " (", mu, "g/", m^3, ")")),
    title = "Event Study: Density Matched Control Group",
    subtitle = paste("Cutoff >", best_cutoff, "pop/km²")
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

# 9. Export Results -------------------------------------------------------

message("\n💾 Exporting Results to: ", OUTPUT_DIR)

# A. Save Tables
etable(
  twfe_main, eventstudy_model,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
  digits = 3,
  file = file.path(OUTPUT_DIR, "Regression_Table_C2.txt"),
  replace = TRUE
)

# B. Save Plot
ggsave(
  filename = file.path(OUTPUT_DIR, "Event_Study_Plot_C2.pdf"),
  plot = p_event,
  width = 8, height = 5, dpi = 300
)

# C. Save Balance Table
pre_treatment_bal <- df_annual %>%
  filter(post_treatment == 0) %>%
  group_by(treatment) %>%
  summarise(
    Mean_PM25 = mean(mean_pm25, na.rm = TRUE),
    Pop_Density = mean(pop_density, na.rm = TRUE),
    Population = mean(population, na.rm = TRUE),
    N_Obs = n()
  )
write_csv(pre_treatment_bal, file.path(OUTPUT_DIR, "Pre_Treatment_Balance_C2.csv"))

message("✅ Analysis C2 Complete.")