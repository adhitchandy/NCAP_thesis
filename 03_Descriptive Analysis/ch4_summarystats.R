# ==========================================
# Chapter 4: Descriptive Statistics & Maps
# Master Thesis - Air Quality Assessment
# ==========================================
# Description: Generates summary statistics tables, time-series plots,
# seasonality heatmaps, and spatial clustering maps (Moran's I/LISA).
# ==========================================

# 1. Setup & Libraries ----------------------------------------------------

# Automatically set working directory to the script's location
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
message("Working Directory: ", getwd())

# Load Libraries
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(knitr)
library(kableExtra)
library(tidyr)
library(sf)
library(spdep)
library(showtext)
library(patchwork)

# 2. Helper Functions (Paths & Fonts) -------------------------------------

# A. Robust File Finder
find_data_file <- function(filename) {
  # Search candidates: current dir, parent dirs, and 'Data' subfolders
  candidates <- c(
    filename,
    file.path("Data", filename),
    file.path("..", "Data", filename),
    file.path("..", "..", "Data", filename),
    file.path("..", filename)
  )
  
  # specific check for the shapefile folder structure
  if(grepl(".shp", filename)) {
    candidates <- c(candidates, 
                    file.path("..", "Data", "shrug-pc11subdist-poly-shp", filename))
  }
  
  path_found <- candidates[file.exists(candidates)][1]
  
  if (is.na(path_found)) {
    warning(paste("⚠️ File not found:", filename))
    return(NULL)
  }
  return(normalizePath(path_found))
}

# B. Font Setup (Cross-Platform)
tryCatch({
  font_add("Times", "/System/Library/Fonts/Times.ttc") # MacOS
  # font_add("Times", "C:/Windows/Fonts/times.ttf")    # Windows (Enable if needed)
  showtext_auto()
}, error = function(e) {
  message("⚠️ Font 'Times' not found. Using default fonts.")
})

# C. Base Theme
base_theme <- theme_bw(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    text = element_text(family = "Times"),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

# 3. Load & Prepare Data --------------------------------------------------

message("DATA LOADING...")
treated_path   <- find_data_file("Treated_PM25_Pop_Area.csv")
untreated_path <- find_data_file("Untreated_PM25_Pop_Area.csv")

if(is.null(treated_path) | is.null(untreated_path)) stop("Critical data missing.")

treated_df   <- read_csv(treated_path, show_col_types = FALSE)
untreated_df <- read_csv(untreated_path, show_col_types = FALSE)

treated_df$treatment   <- 1
untreated_df$treatment <- 0

# Merge and Clean
full_df <- bind_rows(treated_df, untreated_df) %>%
  mutate(
    year_month     = ym(year_month),
    year           = year(year_month),
    month          = month(year_month, label = TRUE, abbr = TRUE),
    post_treatment = ifelse(year >= 2019, 1, 0),
    group          = ifelse(treatment == 1, "Treated", "Untreated"),
    pop_density    = population / area_km2
  ) %>%
  # Handle infinite/NA values centrally
  mutate(
    mean_pm25   = ifelse(is.finite(mean_pm25),   mean_pm25,   NA_real_),
    population  = ifelse(is.finite(population),  population,  NA_real_),
    area_km2    = ifelse(is.finite(area_km2),    area_km2,    NA_real_),
    pop_density = ifelse(is.finite(pop_density), pop_density, NA_real_)
  )

message("✅ Data Loaded: ", nrow(full_df), " observations.")

# 4. Summary Statistics Tables --------------------------------------------

make_summary <- function(df) {
  # Base stats
  base_stats <- df %>%
    summarise(
      pm25_mean   = mean(mean_pm25, na.rm = TRUE),
      pm25_sd     = sd(mean_pm25, na.rm = TRUE),
      pm25_median = median(mean_pm25, na.rm = TRUE),
      pop_mean    = mean(population, na.rm = TRUE),
      pop_sd      = sd(population, na.rm = TRUE),
      area_mean   = mean(area_km2, na.rm = TRUE),
      area_sd     = sd(area_km2, na.rm = TRUE)
    )
  
  # Density stats (filter NA)
  density_stats <- df %>%
    filter(is.finite(pop_density)) %>%
    summarise(
      density_mean = mean(pop_density, na.rm = TRUE),
      density_sd   = sd(pop_density, na.rm = TRUE)
    )
  
  bind_cols(base_stats, density_stats) %>%
    mutate(across(everything(), ~ round(.x, 2)))
}

# Generate stats
sum_treated   <- make_summary(filter(full_df, group == "Treated"))
sum_untreated <- make_summary(filter(full_df, group == "Untreated"))
sum_full      <- make_summary(full_df)

# Output LaTeX to console
cat("\n--- Table: Treated Summary ---\n")
print(kable(sum_treated, format="latex", booktabs=TRUE))

cat("\n--- Table: Untreated Summary ---\n")
print(kable(sum_untreated, format="latex", booktabs=TRUE))

# 5. Visualization: Annual PM2.5 Trends -----------------------------------

annual_group <- full_df %>%
  group_by(year, group) %>%
  summarise(mean_pm25 = mean(mean_pm25, na.rm = TRUE), .groups = "drop")

annual_combined <- full_df %>%
  group_by(year) %>%
  summarise(mean_pm25 = mean(mean_pm25, na.rm = TRUE), .groups = "drop")

y_max <- round(max(c(annual_group$mean_pm25, annual_combined$mean_pm25), na.rm = TRUE), 1)

annual_plot <- ggplot() +
  geom_col(data = annual_combined, aes(x = year, y = mean_pm25, fill = "Combined"), 
           colour = NA, alpha = 0.7, width = 0.8) +
  geom_line(data = annual_group, aes(x = year, y = mean_pm25, colour = group), linewidth = 0.8) +
  geom_point(data = annual_group, aes(x = year, y = mean_pm25, colour = group), size = 1.2) +
  geom_vline(xintercept = 2019, linetype = "dashed", colour = "black") +
  scale_x_continuous(breaks = c(seq(1998, 2019, 3), 2023), expand = expansion(mult = c(0.005, 0.005))) +
  labs(x = "Year", y = expression(paste("Annual Mean PM"[2.5]))) +
  scale_colour_manual(name = NULL, values = c("Treated" = "blue", "Untreated" = "red")) +
  scale_fill_manual(name = NULL, values = c("Combined" = "grey80")) +
  coord_cartesian(ylim = c(0, y_max * 1.05)) +
  base_theme

ggsave("annual_pm25_plot.png", annual_plot, width = 6, height = 4, dpi = 300)
message("Saved: annual_pm25_plot.png")

# 6. Visualization: Seasonality Heatmaps ----------------------------------

# Heatmap 1: Years (X) vs Months (Y)
season_df <- full_df %>%
  filter(year >= 1998, year <= 2023) %>%
  group_by(group, year, month) %>%
  summarise(mean_pm25 = mean(mean_pm25, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    month = factor(month, levels = month.abb, ordered = TRUE),
    year = factor(year)
  )

pm_colors <- c("#2DC937", "#99C140", "#E7B416", "#CC3232", "#55007F", "#000000")

season_heatmap <- ggplot(season_df, aes(x = year, y = month, fill = mean_pm25)) +
  geom_tile(color = "white", linewidth = 0.3) +
  facet_wrap(~ group, ncol = 1) +
  scale_fill_gradientn(colours = pm_colors, name = expression(paste("PM"[2.5]))) +
  labs(x = "Year", y = "Month") +
  theme_minimal(base_family = "Times") +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
    panel.grid = element_blank()
  )

ggsave("seasonality_heatmap_years_x_months_y.png", season_heatmap, width = 8, height = 6, dpi = 300)
message("Saved: seasonality_heatmap_years_x_months_y.png")

# 7. Visualization: Distribution & Boxplots -------------------------------

# Distribution Plot
pm_dist_plot <- ggplot(full_df, aes(x = mean_pm25, fill = group)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.4, bins = 60, position = "identity") +
  geom_density(aes(colour = group), linewidth = 1) +
  scale_fill_manual(values = c("Treated" = "blue", "Untreated" = "red")) +
  scale_colour_manual(values = c("Treated" = "blue", "Untreated" = "red")) +
  labs(x = expression(paste("PM"[2.5])), y = "Density") +
  base_theme + theme(legend.position = "top")

ggsave("pm25_distribution_hist_kde.png", pm_dist_plot, width = 6, height = 4, dpi = 300)

# Decile Boxplots
decile_long <- full_df %>%
  mutate(
    pop_decile  = ntile(population,  10),
    area_decile = ntile(area_km2,    10),
    dens_decile = ntile(pop_density, 10)
  ) %>%
  select(mean_pm25, pop_decile, area_decile, dens_decile) %>%
  pivot_longer(cols = ends_with("decile"), names_to = "var", values_to = "decile") %>%
  mutate(
    var = factor(var, labels = c("Area", "Pop Density", "Population")),
    decile = factor(decile)
  )

decile_box_plot <- ggplot(decile_long, aes(x = decile, y = mean_pm25)) +
  geom_boxplot(outlier.alpha = 0.2) +
  facet_wrap(~ var, nrow = 1, scales = "free_x") +
  labs(x = "Decile (1=Low, 10=High)", y = expression(paste("PM"[2.5]))) +
  base_theme

ggsave("pm25_decile_boxplots.png", decile_box_plot, width = 6, height = 4, dpi = 300)

# 8. Spatial Analysis (Moran's I & LISA) ----------------------------------
# Note: Requires 'subdistrict.shp' in Data/shrug-pc11subdist-poly-shp/

cat("\n--- Starting Spatial Analysis ---\n")
shp_path <- find_data_file("subdistrict.shp")

if (!is.null(shp_path)) {
  
  # A. Load Shapefile
  india_sf <- st_read(shp_path, quiet = TRUE)
  
  # B. Prepare Data
  # Aggregate PM2.5 by subdistrict (sd_name)
  pm_agg <- full_df %>% 
    group_by(sd_name) %>% 
    summarise(mean_pm25 = mean(mean_pm25, na.rm = TRUE))
  
  moran_df <- india_sf %>%
    left_join(pm_agg, by = "sd_name") %>%
    filter(is.finite(mean_pm25)) # Remove NAs for spatial weights
  
  # C. Global Moran's I
  nb <- poly2nb(moran_df, queen = TRUE, snap = 1e-5)
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  moran_result <- moran.test(moran_df$mean_pm25, lw, zero.policy = TRUE, na.action = na.exclude)
  print(moran_result)
  
  # D. Local Moran's I (LISA)
  lisa <- localmoran(moran_df$mean_pm25, lw, zero.policy = TRUE, na.action = na.exclude)
  
  moran_df$lisa_cluster <- "Not significant"
  
  # Determine clusters
  m_dev <- moran_df$mean_pm25 - mean(moran_df$mean_pm25)
  lag_dev <- lag.listw(lw, m_dev, zero.policy = TRUE)
  sig <- lisa[, "Pr(z != E(Ii))"] <= 0.05
  
  moran_df$lisa_cluster[sig & m_dev > 0 & lag_dev > 0] <- "High-High"
  moran_df$lisa_cluster[sig & m_dev < 0 & lag_dev < 0] <- "Low-Low"
  moran_df$lisa_cluster[sig & m_dev > 0 & lag_dev < 0] <- "High-Low"
  moran_df$lisa_cluster[sig & m_dev < 0 & lag_dev > 0] <- "Low-High"
  
  moran_df$lisa_cluster <- factor(moran_df$lisa_cluster, 
                                  levels = c("High-High", "Low-Low", "High-Low", "Low-High", "Not significant"))
  
  # E. Plot Map
  lisa_map <- ggplot(moran_df) +
    geom_sf(aes(fill = lisa_cluster), colour = NA) +
    scale_fill_manual(values = c(
      "High-High" = "#a50f15", "Low-Low" = "#08519c",
      "High-Low" = "#fc9272", "Low-High" = "#9ecae1",
      "Not significant" = "grey90"
    )) +
    theme_minimal(base_family = "Times") +
    theme(axis.text = element_blank(), panel.grid = element_blank())
  
  ggsave("lisa_pm25_clusters.png", lisa_map, width = 6, height = 5, dpi = 300)
  message("Saved: lisa_pm25_clusters.png")
  
} else {
  message("⚠️ Shapefile not found. Skipping Spatial Analysis.")
}

message("\n=== Script Complete ===")