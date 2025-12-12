# ==========================================
# Satellite vs Ground PM2.5 Validation Analysis
# Master Thesis - Air Quality Assessment
# ==========================================
# Description: This script performs correlation and regression analysis
# between satellite-derived and ground-measured PM2.5 concentrations.
# Outputs: Two publication-ready figures (corrpan1.pdf, corrpan2.pdf)
# saved directly in this folder.
# ==========================================

# 1. Setup & Libraries ----

# Automatically set working directory to the script's location (if in RStudio)
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(showtext)
library(ragg)
library(ggrastr)
library(patchwork)

# 2. Robust Data Loading ----

# Helper function: recursively searches up to 3 levels up/down for the file
find_data_file <- function(filename) {
  # List of possible relative paths to check
  candidates <- c(
    filename,                           # Same folder
    file.path("Data", filename),        # Down in Data/
    file.path("..", "Data", filename),  # Up one level then Data/
    file.path("..", "..", "Data", filename), # Up two levels then Data/
    file.path("..", filename)           # Up one level
  )
  
  # Check which one exists
  path_found <- candidates[file.exists(candidates)][1]
  
  if (is.na(path_found)) {
    stop(paste("❌ Could not find file:", filename, 
               "\n   Checked in:", paste(getwd(), "and parent directories.")))
  }
  
  # Return the absolute path
  return(normalizePath(path_found))
}

# Locate files dynamically
cat("🔍 Searching for data files...\n")
ground_path <- find_data_file("monthly_city_averages.csv")
sat_path    <- find_data_file("NCAP_PM25_Timeseries_1998_2023.csv")

cat("✅ Found Ground Data:", ground_path, "\n")
cat("✅ Found Sat Data:   ", sat_path, "\n\n")

# Load Data
ground_data    <- read_csv(ground_path, show_col_types = FALSE)
satellite_data <- read_csv(sat_path, show_col_types = FALSE)

# 3. Data Preprocessing ----

# Clean ground monitoring data
ground_data <- ground_data %>%
  mutate(
    city = str_to_lower(str_trim(city)),
    date = as.Date(paste(year, month, "01", sep = "-"))
  )

# Clean satellite data
satellite_data <- satellite_data %>%
  mutate(
    city = str_to_lower(str_trim(NCAP_city))
  ) %>%
  separate(year_month, into = c("year", "month"), sep = "-", remove = FALSE) %>%
  mutate(
    year = as.integer(year),
    month = as.integer(month),
    date = as.Date(paste(year, month, "01", sep = "-"))
  )

# Merge datasets
merged_data <- inner_join(
  ground_data, 
  satellite_data,
  by = c("city", "date"),
  suffix = c("_ground", "_sat")
) %>%
  mutate(
    year = year(date),
    month = month(date),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Summer",
      month %in% c(6, 7, 8) ~ "Monsoon",
      TRUE ~ "Post-Monsoon"
    )
  )

# Filter for valid observations
merged_data_clean <- merged_data %>%
  filter(
    !is.na(avg_PM2.5),
    !is.na(mean_pm25),
    avg_PM2.5 > 0,
    mean_pm25 > 0
  )

# 4. Statistical Analysis ----

# Monthly correlation
pearson_monthly <- cor(merged_data_clean$avg_PM2.5, merged_data_clean$mean_pm25, use = "complete.obs")

# Annual aggregation and correlation
yearly_data <- merged_data_clean %>%
  group_by(city, year) %>%
  summarise(
    ground_pm25_yearly = mean(avg_PM2.5, na.rm = TRUE),
    sat_pm25_yearly = mean(mean_pm25, na.rm = TRUE),
    .groups = "drop"
  )

pearson_yearly <- cor(yearly_data$ground_pm25_yearly, yearly_data$sat_pm25_yearly, use = "complete.obs")

# OLS regression
ols_model <- lm(avg_PM2.5 ~ mean_pm25, data = merged_data_clean)
ols_sum <- summary(ols_model)

# Seasonal correlations
seasonal_corrs <- merged_data_clean %>%
  group_by(season) %>%
  summarise(
    n_obs = n(),
    pearson_corr = cor(avg_PM2.5, mean_pm25, use = "complete.obs"),
    .groups = "drop"
  )

# 5. Visualization Setup ----

# Font loading with fallback
tryCatch({
  font_add("Times", "/System/Library/Fonts/Times.ttc") # MacOS
  # font_add("Times", "C:/Windows/Fonts/times.ttf")    # Windows (uncomment if needed)
  showtext_auto()
}, error = function(e) {
  message("⚠️ Font 'Times' not found. Using default fonts.")
})

theme_pub <- function() {
  theme_minimal(base_family = "Times") +
    theme(
      text = element_text(family = "Times", size = 12),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 11),
      panel.grid.major = element_line(colour = "grey85", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 12, face = "bold")
    )
}

# 6. Figure Generation (Condensed) ----

col_sat <- "#2C7BB6"
col_line <- "#D7191C"
col_yearly <- "#4D9221"

# Figure 1: Monthly/Annual
p1 <- ggplot(merged_data_clean, aes(x = mean_pm25, y = avg_PM2.5)) +
  geom_point_rast(alpha = 0.4, colour = col_sat, size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, colour = col_line) +
  labs(title = paste("Monthly (r =", round(pearson_monthly, 2), ")"), 
       x = "Satellite PM2.5", y = "Ground PM2.5") + theme_pub()

p2 <- ggplot(yearly_data, aes(x = sat_pm25_yearly, y = ground_pm25_yearly)) +
  geom_point(alpha = 0.7, colour = col_yearly, size = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = col_line) +
  labs(title = paste("Annual (r =", round(pearson_yearly, 2), ")"), 
       x = "Annual Satellite PM2.5", y = NULL) + theme_pub()

ggsave("corr.pdf", p1 | p2, width = 8, height = 4)

# Figure 2: Seasonal (Loop)
plot_list <- lapply(unique(merged_data_clean$season), function(s) {
  df <- filter(merged_data_clean, season == s)
  r <- round(cor(df$avg_PM2.5, df$mean_pm25, use="complete.obs"), 2)
  ggplot(df, aes(x = mean_pm25, y = avg_PM2.5)) +
    geom_point_rast(alpha = 0.4) +
    geom_smooth(method="lm", se=FALSE, colour=col_line) +
    labs(subtitle = paste(s, "r =", r), x=NULL, y=NULL) +
    theme_pub()
})

combined_season <- wrap_plots(plot_list, ncol=2) + 
  plot_annotation(caption = "Satellite PM2.5 (ug/m3)")

ggsave("seasonal_corr.pdf", combined_season, width = 7, height = 6)

message("✅ Validation Analysis Complete. Figures saved.")