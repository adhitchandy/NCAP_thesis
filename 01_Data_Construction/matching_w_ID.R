library(readr)
library(readxl)
library(dplyr)
library(stringr)

# ------------------------------------------
# ✅ 1) Load Data
# ------------------------------------------
ncap_path <- "/Users/adhitchandy/Library/CloudStorage/OneDrive-M365UniversitätHamburg/MT/NCAP/NCAP_city_names.xlsx"
pm25_path <- "/Users/adhitchandy/Library/CloudStorage/OneDrive-M365UniversitätHamburg/MT/Data/Satellite_Data/monthly_data/sd_monthly_2000.csv"

ncap <- read_xlsx(ncap_path, sheet = "Sheet1")
pm25 <- read_csv(pm25_path)

# ------------------------------------------
# ✅ 2) Lowercase and prepare reference
# ------------------------------------------
ncap <- ncap %>%
  mutate(
    input_city = str_to_lower(input_city),
    pc11_town_name = str_to_lower(pc11_town_name),
    pc11_subdistrict = str_to_lower(pc11_subdistrict),
    pc11_district = str_to_lower(pc11_district),
    pc11_state = str_to_lower(pc11_state)
  )

ref_df <- pm25 %>%
  dplyr::select(pc11_s_id, pc11_d_id, pc11_sd_id, sd_name, district_name, state_name) %>%
  distinct() %>%
  mutate(across(c(state_name, district_name, sd_name), str_to_lower))

# ------------------------------------------
# ✅ 3) Matching logic with ID creation
# ------------------------------------------
ncap <- ncap %>%
  rowwise() %>%
  mutate(
    state_df = list(ref_df %>% filter(state_name == pc11_state)),
    
    # MATCH LEVEL
    found_in = case_when(
      input_city %in% state_df$sd_name ~ "subdistrict_name",
      pc11_town_name %in% state_df$sd_name ~ "subdistrict_name",
      pc11_subdistrict %in% state_df$sd_name ~ "subdistrict_name",
      pc11_district %in% state_df$sd_name ~ "subdistrict_name",
      
      input_city %in% state_df$district_name ~ "district_name",
      pc11_town_name %in% state_df$district_name ~ "district_name",
      pc11_subdistrict %in% state_df$district_name ~ "district_name",
      pc11_district %in% state_df$district_name ~ "district_name",
      
      pc11_state %in% state_df$state_name ~ "state_name",
      TRUE ~ NA_character_
    ),
    
    # SOURCE FIELD
    found_from = case_when(
      found_in == "subdistrict_name" & input_city %in% state_df$sd_name ~ "input_city",
      found_in == "subdistrict_name" & pc11_town_name %in% state_df$sd_name ~ "pc11_town_name",
      found_in == "subdistrict_name" & pc11_subdistrict %in% state_df$sd_name ~ "pc11_subdistrict",
      found_in == "subdistrict_name" & pc11_district %in% state_df$sd_name ~ "pc11_district",
      
      found_in == "district_name" & input_city %in% state_df$district_name ~ "input_city",
      found_in == "district_name" & pc11_town_name %in% state_df$district_name ~ "pc11_town_name",
      found_in == "district_name" & pc11_subdistrict %in% state_df$district_name ~ "pc11_subdistrict",
      found_in == "district_name" & pc11_district %in% state_df$district_name ~ "pc11_district",
      
      found_in == "state_name" ~ "pc11_state",
      TRUE ~ NA_character_
    ),
    
    # ID creation with ambiguity check only for subdistricts
    ID = {
      if (found_in == "subdistrict_name") {
        code_row <- state_df %>%
          filter(district_name == pc11_district, sd_name == pc11_subdistrict)
        if (nrow(code_row) == 1) {
          paste0(code_row$pc11_s_id, code_row$pc11_d_id, code_row$pc11_sd_id)
        } else if (nrow(code_row) > 1) {
          message("⚠️ Ambiguous subdistrict: ", pc11_subdistrict, ", ", pc11_district, ", ", pc11_state)
          NA_character_
        } else {
          message("❓ No subdistrict match: ", pc11_subdistrict, ", ", pc11_district, ", ", pc11_state)
          NA_character_
        }
        
      } else if (found_in == "district_name") {
        code_row <- state_df %>% filter(district_name == pc11_district)
        if (nrow(code_row) >= 1) {
          paste0(code_row$pc11_s_id[1], code_row$pc11_d_id[1])  # ✅ Pick first, skip warning
        } else {
          message("❓ No district match: ", pc11_district, ", ", pc11_state)
          NA_character_
        }
        
      } else if (found_in == "state_name") {
        code_row <- state_df
        if (nrow(code_row) >= 1) {
          as.character(code_row$pc11_s_id[1])
        } else {
          message("❓ No state match: ", pc11_state)
          NA_character_
        }
        
      } else {
        NA_character_
      }
    }
  ) %>%
  ungroup() %>%
  dplyr::select(-state_df)

# ------------------------------------------
# ✅ 4) Summary
# ------------------------------------------
cat("===========================================\n")
cat("🎯 TOTAL cities in NCAP:   ", nrow(ncap), "\n")
cat("✅ Matched cities:         ", sum(!is.na(ncap$found_from)), "\n")
cat("🚫 NOT matched:            ", sum(is.na(ncap$found_from)), "\n")
cat("-------------------------------------------\n")
cat("📌 Breakdown by found_in:\n")
print(table(ncap$found_in, useNA = "always"))
cat("📌 Breakdown by found_from:\n")
print(table(ncap$found_from, useNA = "always"))
cat("📌 Rows with missing or ambiguous ID: ", sum(is.na(ncap$ID)), "\n")

# ------------------------------------------
# ✅ 5) Save Output
# ------------------------------------------
write_csv(
  ncap,
  "/Users/adhitchandy/Library/CloudStorage/OneDrive-M365UniversitätHamburg/MT/NCAP/NCAP_city_names_matched_with_ID.csv"
)