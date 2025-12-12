# -------------------------------------------
# 📚 Load packages
# -------------------------------------------
library(readr)
library(dplyr)
library(stringr)

# -------------------------------------------
# 1️⃣ File paths
# -------------------------------------------
pm25_file <- "/Users/adhitchandy/Library/CloudStorage/OneDrive-M365UniversitätHamburg/MT/Data/Satellite_Data/monthly_data/PM25_Subdistrict_TimeSeries_1998_2023.csv"
pop_area_file <- "/Users/adhitchandy/Library/CloudStorage/OneDrive-M365UniversitätHamburg/MT/Data/Satellite_Data/shrug-pc11subdist-wt-csv/subdist_pc11_pop_area_key.csv"
match_file <- "/Users/adhitchandy/Library/CloudStorage/OneDrive-M365UniversitätHamburg/MT/NCAP/NCAP_city_names_matched_with_ID.csv"
output_file <- "/Users/adhitchandy/Library/CloudStorage/OneDrive-M365UniversitätHamburg/MT/Data/Satellite_Data/PM25_Pop_Area_Merged_wID.csv"

# -------------------------------------------
# 2️⃣ Load & clean PM2.5 dataset
# -------------------------------------------
pm25 <- read_csv(pm25_file) %>%
  mutate(
    year = as.numeric(substr(year_month, 1, 4)),
    post_treatment = ifelse(year >= 2019, 1, 0),
    treatment = NA
  ) %>%
  filter(!pc11_sd_id %in% c("00000", "99998"))

# -------------------------------------------
# 3️⃣ Load population & area with district ID
# -------------------------------------------
pop_area <- read_csv(pop_area_file) %>%
  mutate(
    population = sd_pc11_pca_tot_p,
    area_km2 = sd_pc11_land_area
  ) %>%
  dplyr::select(pc11_district_id, pc11_subdistrict_id, population, area_km2)

# ✅ Check for duplicates
duplicates <- pop_area %>%
  count(pc11_district_id, pc11_subdistrict_id) %>%
  filter(n > 1)

if (nrow(duplicates) > 0) {
  cat("⚠️ Duplicates found in pop_area. Check manually!\n")
  print(duplicates)
}

# -------------------------------------------
# 4️⃣ Merge PM2.5 + population/area
# -------------------------------------------
final_big <- pm25 %>%
  left_join(
    pop_area,
    by = c(
      "pc11_d_id" = "pc11_district_id",
      "pc11_sd_id" = "pc11_subdistrict_id"
    )
  ) %>%
  dplyr::select(
    pc11_s_id,  # ✅ ADD THIS
    pc11_d_id, pc11_sd_id,
    state_name, district_name, sd_name,
    year_month, mean_pm25,
    population, area_km2,
    treatment, post_treatment
  ) %>%
  arrange(pc11_sd_id, year_month)
# -------------------------------------------
# 5️⃣ Load NCAP matches and apply treatment using hierarchical ID prefix
# -------------------------------------------
matches <- read_csv(match_file) %>%
  mutate(ID = as.character(ID))

# ✅ Separate IDs by level
id_state     <- matches$ID[nchar(matches$ID) == 2]     # State-level
id_district  <- matches$ID[nchar(matches$ID) == 5]     # District-level
id_subdist   <- matches$ID[nchar(matches$ID) == 10]    # Subdistrict-level

# ✅ Pad IDs to ensure uniform structure
# ✅ Pad all IDs to ensure proper format
final_big <- final_big %>%
  mutate(
    pc11_s_id  = str_pad(as.character(pc11_s_id), 2, pad = "0"),
    pc11_d_id  = str_pad(as.character(pc11_d_id), 3, pad = "0"),
    pc11_sd_id = str_pad(as.character(pc11_sd_id), 5, pad = "0")
  )

# ✅ Construct full hierarchical IDs
final_big <- final_big %>%
  mutate(
    full_id     = paste0(pc11_s_id, pc11_d_id, pc11_sd_id),  # 10-digit
    district_id = paste0(pc11_s_id, pc11_d_id),              # 5-digit
    state_id    = pc11_s_id                                  # 2-digit
  )

# ✅ Apply treatment flag based on hierarchical prefix matching
final_big <- final_big %>%
  mutate(
    treatment = case_when(
      full_id %in% id_subdist ~ 1,
      district_id %in% id_district ~ 1,
      state_id %in% id_state ~ 1,
      TRUE ~ 0
    )
  )
# -------------------------------------------
# 6️⃣ Save final output
# -------------------------------------------
write_csv(final_big, output_file)
cat("✅ Final merged PM2.5 + Pop + Area file saved:\n", output_file, "\n")

# ✅ Quick checks
cat("Number of unique subdistricts:", length(unique(final_big$pc11_sd_id)), "\n")
cat("Number of unique districts:", length(unique(final_big$pc11_d_id)), "\n")
cat("Number of treated subdistricts:", sum(final_big$treatment == 1), "\n")