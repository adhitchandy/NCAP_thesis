# -----------------------------
# Load necessary libraries
# -----------------------------
library(dplyr)
library(readr)
library(stringr)

# -----------------------------
# 1️⃣ Load main data file
# -----------------------------
# --- ADJUST YOUR FILE PATH HERE ---
df_file <- "/Users/adhitchandy/Library/CloudStorage/OneDrive-M365UniversitätHamburg/MT/Data/Satellite_Data/PM25_Pop_Area_Merged_wID.csv"

# Read the data
df <- read_csv(df_file)

# -----------------------------
# 2️⃣ Preprocess the data
# -----------------------------
# This step is crucial to ensure the 'treated' column is correctly defined
df_processed <- df %>%
  mutate(
    full_id     = str_pad(as.character(full_id), 10, pad = "0"),
    district_id = substr(full_id, 1, 5),
    state_id    = substr(full_id, 1, 2),
    pop_density = population / area_km2,
    year        = as.numeric(substr(year_month, 1, 4)),
    treated     = ifelse(treatment == 1, 1, 0), # Ensure treated is 1 or 0
    post_treatment = ifelse(year < 2019, 0, 1)
  )

# -----------------------------
# 3️⃣ Create the untreated dataframe with matching structure
# -----------------------------
# Filter for control units and then select/rename columns to match treated_df structure
untreated_df <- df_processed %>%
  filter(treated == 0) %>%
  rename(ID = full_id) %>% # Standardize the ID column to match treated_df
  dplyr::select(
    ID,
    year_month,
    year,
    mean_pm25,
    population,
    area_km2,
    treated,
    sd_name,
    district_name,
    state_name,
    post_treatment
  )

# -----------------------------
# 4️⃣ Final Summary
# -----------------------------
cat("✅ Untreated DataFrame created successfully.\n")
cat("✅ Number of rows in untreated_df:", nrow(untreated_df), "\n")
cat("✅ Number of unique untreated subdistricts:", n_distinct(untreated_df$ID), "\n") 

# -----------------------------
# 5️⃣ Save untreated_df to file
# -----------------------------
write_csv(
  untreated_df,
  "/Users/adhitchandy/Library/CloudStorage/OneDrive-M365UniversitätHamburg/MT/Final Data/Untreated_PM25_Pop_Area.csv"
)
