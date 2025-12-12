# ===============================
# Merge yearly PM2.5 CSVs
# into a single time series
# ===============================

library(dplyr)
library(readr)

# -------------------------------
# ✅ 1. Define your folder and pattern
# -------------------------------
# Set the folder where all yearly CSVs are saved:
csv_folder <- "/Users/adhitchandy/Library/CloudStorage/OneDrive-M365UniversitätHamburg/MT/Data/Satellite_Data/monthly_data/"

# Get all relevant CSV files:
csv_files <- list.files(csv_folder, pattern = "^sd_monthly_\\d{4}\\.csv$", full.names = TRUE)

cat("📁 Found", length(csv_files), "files:\n")
print(basename(csv_files))

# -------------------------------
# ✅ 2. Read and combine all files
# -------------------------------
all_data <- lapply(csv_files, read_csv)

final_time_series <- bind_rows(all_data)

cat("✅ Combined rows:", nrow(final_time_series), "\n")

# -------------------------------
# ✅ Filtering
# -------------------------------

final_time_series_clean <- final_time_series %>%
  filter(
    pc11_sd_id != "99998",
    pc11_d_id != "000"
  )

# -------------------------------
# ✅ 3. Save final merged CSV
# -------------------------------
output_path <- paste0(csv_folder, "PM25_Subdistrict_TimeSeries_1998_2023.csv")

write_csv(final_time_series, output_path)

cat("🎉 Final time series saved to:\n", output_path, "\n")