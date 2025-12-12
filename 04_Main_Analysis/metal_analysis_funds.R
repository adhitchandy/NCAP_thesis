# ==============================================================================
# Mechanism Analysis: Meta-Regression (Funds vs. Effectiveness)
# Master Thesis - Main Econometric Analysis
# ==============================================================================
# Description: 
#   Regresses the estimated City-Level SDiD Treatment Effects (ATT) on the 
#   Fund Utilization Ratio.
#   - Model: Weighted Least Squares (WLS) using Inverse Variance Weighting.
#   - Visual: Scatter plot with regression line and outliers capped.
# ==============================================================================

# 1. Setup & Libraries ----------------------------------------------------

# Automatically set working directory
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
message("📂 Working Directory: ", getwd())

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(showtext)

# Create Output Directory
OUTPUT_DIR <- "Output_Mechanisms"
dir.create(OUTPUT_DIR, showWarnings = FALSE)

# 2. Helper Functions (Paths & Fonts) -------------------------------------

# A. Robust File Finder
find_data_file <- function(filename) {
  candidates <- c(
    filename,
    file.path("Data", filename),
    file.path("..", "Data", filename),
    file.path("..", "..", "Data", filename),
    # Look for the results from the PREVIOUS script (Folder 04)
    file.path("Output_CityLevel_SDiD", filename),
    file.path("..", "Output_CityLevel_SDiD", filename)
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

# 3. Data Loading ---------------------------------------------------------

message("\n📥 Loading Data...")

# A. Load Matching Key & Funds Data
matched_path <- find_data_file("NCAP_city_names_matched_with_ID.csv")
funds_path   <- find_data_file("funds_data.csv")

funds_raw  <- read_csv(funds_path, show_col_types = FALSE)
matched_df <- read_csv(matched_path, show_col_types = FALSE)

# B. Load SDiD Results (From the previous script's output folder)
results_path <- find_data_file("All_City_Results.csv")
sdid_results <- read_csv(results_path, show_col_types = FALSE)

cat("   Loaded SDiD Results for", nrow(sdid_results), "cities.\n")

# 4. Data Cleaning & Merging ----------------------------------------------

message("\n🧹 Cleaning & Linking Data...")

# A. Clean Funds Data
funds_clean <- funds_raw %>%
  mutate(
    match_name_temp = str_trim(tolower(City)),
    # Manual Fixes for Mismatched Names
    match_name = case_when(
      match_name_temp == "pathankot/dera baba" ~ "pathankot/ dera baba",
      match_name_temp == "kanpur" ~ "kanpur nagar",
      TRUE ~ match_name_temp
    ),
    # Clean Currency Strings
    released_val = as.numeric(str_remove_all(`Funds Released`, "[^0-9.]")),
    utilized_val = as.numeric(str_remove_all(`Funds Utilized`, "[^0-9.]")),
    # Calculate Ratio
    utilization_ratio = ifelse(released_val == 0, 0, utilized_val / released_val)
  )

# B. Link Funds -> IDs
funds_with_ids <- funds_clean %>%
  left_join(matched_df, by = c("match_name" = "input_city")) %>%
  dplyr::select(City, utilization_ratio, ID, everything())

# C. Link Funds (ID) -> SDiD Results (ID)
analysis_df <- funds_with_ids %>%
  filter(!is.na(ID)) %>%
  inner_join(sdid_results, by = "ID") %>%
  mutate(
    # Create Precision Weights for WLS (Inverse Variance)
    # Adding small constant 1e-6 to avoid division by zero
    precision_weight = 1 / (SE^2 + 1e-6),
    is_significant   = abs(ATT / SE) > 1.96
  )

cat("   ✅ Final Analysis Dataset:", nrow(analysis_df), "cities linked.\n")

# 5. Weighted Least Squares Regression ------------------------------------

message("\n📊 Running Weighted Least Squares (WLS)...")

model_wls <- lm(ATT ~ utilization_ratio, 
                data = analysis_df, 
                weights = precision_weight)

print(summary(model_wls))

# Save Regression Summary
capture.output(summary(model_wls), file = file.path(OUTPUT_DIR, "Meta_Regression_Summary.txt"))

# 6. Visualization --------------------------------------------------------

message("\n📈 Generating Publication Plot...")

# A. Prepare Plot Data (Capping outliers for visual clarity)
analysis_plot_df <- analysis_df %>%
  mutate(
    plot_ratio = ifelse(utilization_ratio > 1, 1.05, utilization_ratio),
    is_outlier = utilization_ratio > 1,
    is_reliable = RMSPE_trend < 0.5, # Only prioritize labeling reliable fits
    # Rank for labeling (High Utilization + High Effect)
    priority_score = case_when(
      !is_reliable ~ 0,
      TRUE ~ utilization_ratio + (abs(ATT) / 20)
    )
  ) %>%
  arrange(desc(priority_score)) %>%
  mutate(
    rank_id = ifelse(row_number() <= 10, as.character(row_number()), NA),
    should_label = !is.na(rank_id)
  )

# B. Generate Caption Key
top_cities <- analysis_plot_df %>% 
  filter(should_label) %>% 
  arrange(as.numeric(rank_id)) %>%
  mutate(label_entry = paste0(rank_id, ". ", City, " (", round(ATT, 1), ")"))

key_string <- paste(top_cities$label_entry, collapse = "  ")
key_string_formatted <- str_wrap(key_string, width = 90)

# C. Create Plot
p_thesis <- ggplot(analysis_plot_df, aes(x = plot_ratio, y = ATT)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  
  # Points
  geom_point(aes(size = precision_weight, fill = is_significant, color = is_outlier), 
             shape = 21, alpha = 0.8, stroke = 0.8) +
  
  # Regression Line (Weighted, utilizing full data range)
  geom_smooth(data = analysis_df, aes(x = utilization_ratio, y = ATT, weight = precision_weight), 
              method = "lm", color = "black", alpha = 0.15, se = TRUE) +
  
  # Labels
  geom_text_repel(
    aes(label = rank_id),
    size = 4, fontface = "bold",
    box.padding = 0.5, point.padding = 0.3, min.segment.length = 0,
    segment.color = "grey50", bg.color = "white", bg.r = 0.15
  ) +
  
  # Scales
  scale_fill_manual(values = c("grey80", "#D55E00"), labels = c("No", "Yes"), name = "Significant?") +
  scale_color_manual(values = c("white", "black"), guide = "none") +
  scale_size_continuous(range = c(2, 9), guide = "none") + 
  scale_x_continuous(
    breaks = seq(0, 1.2, by = 0.1), 
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.1), 
    name = "Fund Utilisation Ratio"
  ) +
  
  # Aesthetics
  labs(
    title = NULL,
    y = expression(paste("Est. Change in ", PM[2.5], " (", mu, "g/", m^3, ")")),
    caption = paste0("Key to Labeled Cities (Top 10 by Relevance):\n", key_string_formatted)
  ) +
  theme_minimal(base_family = "Times") +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = -5),
    plot.caption = element_text(hjust = 0, size = 9, face = "italic", margin = margin(t = 15)),
    axis.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )

# D. Save Plot
ggsave(file.path(OUTPUT_DIR, "Figure_Funds_Thesis_Final.pdf"), 
       p_thesis, width = 7, height = 5, device = cairo_pdf)

message("✅ Analysis Complete. Results saved to: ", OUTPUT_DIR)