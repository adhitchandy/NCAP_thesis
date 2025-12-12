# -----------------------------
# Load necessary libraries
# -----------------------------
library(dplyr)
library(readr)
library(stringr)

# -----------------------------
# 1️⃣ Load data
# -----------------------------
# --- ADJUST YOUR FILE PATHS HERE ---
df_file <- "/Users/adhitchandy/Library/CloudStorage/OneDrive-M365UniversitätHamburg/MT/Data/Satellite_Data/PM25_Pop_Area_Merged_wID.csv"
matches_file <- "/Users/adhitchandy/Library/CloudStorage/OneDrive-M365UniversitätHamburg/MT/NCAP/NCAP_city_names_matched_with_ID.csv"

# Read the data, ensuring the ID in matches is read as a character
df <- read_csv(df_file)
matches <- read_csv(matches_file, col_types = cols(ID = col_character()))

# -----------------------------
# 2️⃣ Preprocess and extract ID levels
# -----------------------------
df <- df %>%
  mutate(
    full_id     = str_pad(as.character(full_id), 10, pad = "0"),
    district_id = substr(full_id, 1, 5),
    state_id    = substr(full_id, 1, 2),
    pop_density = population / area_km2,
    year        = as.numeric(substr(year_month, 1, 4)),
    treated     = ifelse(treatment == 1, 1, 0)
  )

# Extract the different levels of IDs from the matches file
subdistrict_ids <- matches$ID[nchar(matches$ID) == 10]
district_ids    <- substr(matches$ID[nchar(matches$ID) == 5], 1, 5)
state_ids       <- substr(matches$ID[nchar(matches$ID) == 2], 1, 2)

# -----------------------------
# 3️⃣ Create unique name mappings (1 row per ID)
# -----------------------------
# This is an efficient way to get the correct name for each ID level, including parent names
id_map_sd <- df %>%
  dplyr::select(full_id, sd_name, district_name, state_name) %>%
  distinct(full_id, .keep_all = TRUE)

id_map_district <- df %>%
  dplyr::select(district_id, district_name, state_name) %>%
  distinct(district_id, .keep_all = TRUE)

id_map_state <- df %>%
  dplyr::select(state_id, state_name) %>%
  distinct(state_id, .keep_all = TRUE)

# -----------------------------
# 4️⃣ Aggregation function
# -----------------------------
# This function aggregates data for a given set of IDs at a specific administrative level
aggregate_by_id <- function(df, id_vec, id_col) {
  df %>%
    filter(.data[[id_col]] %in% id_vec, treated == 1) %>%
    group_by(.data[[id_col]], year_month, year) %>%
    summarise(
      mean_pm25   = mean(mean_pm25, na.rm = TRUE),
      population  = sum(unique(population), na.rm = TRUE),
      area_km2    = sum(unique(area_km2), na.rm = TRUE),
      treated     = 1,
      .groups     = "drop"
    )
}

# -----------------------------
# 5️⃣ Run aggregations and join all relevant names
# -----------------------------
# The key is to join back the relevant names for each level
treated_subdistrict <- aggregate_by_id(df, subdistrict_ids, "full_id") %>%
  rename(ID = full_id) %>%
  left_join(id_map_sd, by = c("ID" = "full_id"))

treated_district <- aggregate_by_id(df, district_ids, "district_id") %>%
  rename(ID = district_id) %>%
  left_join(id_map_district, by = c("ID" = "district_id"))

treated_state <- aggregate_by_id(df, state_ids, "state_id") %>%
  rename(ID = state_id) %>%
  left_join(id_map_state, by = c("ID" = "state_id"))

# -----------------------------
# 6️⃣ Combine and finalize
# -----------------------------
# bind_rows will now correctly stack the data, creating NA for non-matching columns
treated_df <- bind_rows(treated_subdistrict, treated_district, treated_state) %>%
  mutate(
    post_treatment = ifelse(year < 2019, 0, 1)
  )

# -----------------------------
# 7️⃣ Final Summary
# -----------------------------
cat("✅ Number of unique treated units:", n_distinct(treated_df$ID), "\n")
cat("✅ Number of rows in final treated_df:", nrow(treated_df), "\n")

# -----------------------------
# 5️⃣ Save untreated_df to file
# -----------------------------
write_csv(
  treated_df,
  "/Users/adhitchandy/Library/CloudStorage/OneDrive-M365UniversitätHamburg/MT/Data/Satellite_Data/Treated_PM25_Pop_Area.csv"
)