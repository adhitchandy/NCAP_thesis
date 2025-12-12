# ===============================
# PM2.5 Subdistrict Extraction
# for Multiple Years
# ===============================

library(terra)
library(raster)
library(sf)
library(exactextractr)
library(dplyr)
library(stringr)

# -------------------------------
# ✅ 1. Define years to loop
# -------------------------------
YEARS <- 2002:2023

# -------------------------------
# ✅ 2. Static paths
# -------------------------------
subdist_shp <- "/Users/adhitchandy/Library/CloudStorage/OneDrive-M365UniversitätHamburg/MT/Data/Satellite_Data/shrug shapes/shrug-pc11subdist-poly-shp/subdistrict.shp"

state_shp_path <- "/Users/adhitchandy/Library/CloudStorage/OneDrive-M365UniversitätHamburg/MT/Data/Satellite_Data/shrug shapes/shrug-pc11state-poly-shp/state.shp"
dist_shp_path  <- "/Users/adhitchandy/Library/CloudStorage/OneDrive-M365UniversitätHamburg/MT/Data/Satellite_Data/shrug shapes/shrug-pc11dist-poly-shp/district.shp"

# -------------------------------
# ✅ 3. Load static shapefiles once
# -------------------------------
subdist_sf <- st_read(subdist_shp) |> st_transform(4326)

state_sf <- st_read(state_shp_path) |> st_transform(4326)
dist_sf  <- st_read(dist_shp_path)  |> st_transform(4326)

# ✅ Create state & district lookup tables (safe select)
state_lookup <- state_sf %>%
  st_drop_geometry() %>%
  dplyr::select(pc11_s_id, state_name = s_name) %>%
  mutate(
    pc11_s_id = as.character(pc11_s_id),
    pc11_s_id = str_pad(pc11_s_id, width = 2, pad = "0")
  )

dist_lookup <- dist_sf %>%
  st_drop_geometry() %>%
  dplyr::select(pc11_d_id, pc11_s_id, district_name = d_name) %>%
  mutate(
    pc11_d_id = as.character(pc11_d_id),
    pc11_s_id = as.character(pc11_s_id),
    pc11_s_id = str_pad(pc11_s_id, width = 2, pad = "0")
  )

# -------------------------------
# ✅ 4. Loop over each year
# -------------------------------
for (TARGET_YEAR in YEARS) {
  
  cat("\n============================================\n")
  cat("🎯 Starting extraction for:", TARGET_YEAR, "\n")
  cat("============================================\n")
  
  # ---- Define year-specific paths ----
  pm25_folder <- paste0("/Users/adhitchandy/Library/CloudStorage/OneDrive-M365UniversitätHamburg/MT/Data/Satellite_Data/AS/Monthly/", TARGET_YEAR, "/")
  
  output_csv <- paste0("/Users/adhitchandy/Library/CloudStorage/OneDrive-M365UniversitätHamburg/MT/Data/Satellite_Data/monthly_data/sd_monthly_", TARGET_YEAR, ".csv")
  
  # ---- List NetCDF files ----
  nc_files <- list.files(pm25_folder, pattern = "\\.nc$", full.names = TRUE)
  
  cat("🔍 Found", length(nc_files), "NetCDF files for", TARGET_YEAR, "\n")
  
  if (length(nc_files) == 0) {
    cat("⚠️ Skipping year", TARGET_YEAR, " — no files found.\n")
    next
  }
  
  # ---- Process each file ----
  all_results <- list()
  
  for (f in nc_files) {
    cat("📆 Processing:", basename(f), "\n")
    
    r <- terra::rast(f)[[1]]
    r_raster <- raster::raster(r)
    
    vals <- exactextractr::exact_extract(r_raster, subdist_sf, 'mean', force_df = TRUE)
    
    date_str <- str_extract(basename(f), "\\d{6}")
    year_month <- format(as.Date(paste0(date_str, "01"), "%Y%m%d"), "%Y-%m")
    
    df <- subdist_sf %>%
      st_drop_geometry() %>%
      bind_cols(mean_pm25 = vals$mean) %>%
      mutate(year_month = year_month)
    
    all_results[[f]] <- df
  }
  
  final_df <- bind_rows(all_results)
  
  cat("✅ Extracted PM2.5 for", nrow(final_df), "rows.\n")
  
  # ---- Add IDs & join ----
  final_df <- final_df %>%
    mutate(
      pc11_d_id = as.character(pc11_d_id),
      pc11_s_id = as.character(pc11_s_id),
      pc11_s_id = str_pad(pc11_s_id, width = 2, pad = "0")
    )
  
  final_df_joined <- final_df %>%
    left_join(dist_lookup, by = "pc11_d_id")
  
  final_df_ready <- final_df_joined %>%
    dplyr::select(-pc11_s_id.x) %>%
    rename(pc11_s_id = pc11_s_id.y)
  
  final_df_with_names <- final_df_ready %>%
    left_join(state_lookup, by = "pc11_s_id")
  
  # ---- Checks ----
  cat("🔍 Missing district names:\n")
  print(table(is.na(final_df_with_names$district_name)))
  
  cat("🔍 Missing state names:\n")
  print(table(is.na(final_df_with_names$state_name)))
  
  # ---- Save ----
  write.csv(final_df_with_names, output_csv, row.names = FALSE)
  cat("✅ Saved for year", TARGET_YEAR, "➡️", output_csv, "\n")
}

cat("\n🎉 All years done!\n")