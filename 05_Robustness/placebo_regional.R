# ============================================================
# FUNCTION: REGIONAL SDID + PLACEBO TESTS (run after running regional sdid code)
# ============================================================

run_regional_sdid <- function(region_name,
                              placebo_years = c(2010, 2012, 2014, 2015, 2016, 2017)) {
  
  cat(paste0("\n========================================\n"))
  cat(paste0("      PROCESSING REGION: ", region_name, "\n"))
  cat(paste0("========================================\n"))
  
  # -----------------------------
  # A. SUBSET DATA FOR REGION
  # -----------------------------
  
  df_sub <- df_analysis %>%
    filter(airshed_region == region_name)
  
  n_treat <- n_distinct(df_sub$ID[df_sub$treatment == 1])
  if (n_treat == 0) {
    cat("Skipping region: no treated units.\n")
    return(NULL)
  }
  
  all_years <- sort(unique(df_sub$year))
  
  # ============================================================
  # HELPER FUNCTION FOR SDID ESTIMATION (REAL OR PLACEBO)
  # ============================================================
  
  estimate_sdid <- function(treat_start_year) {
    
    df_tmp <- df_sub %>%
      mutate(
        W = ifelse(treatment == 1 & year >= treat_start_year, 1, 0)
      )
    
    # Balance panel
    df_bal <- df_tmp %>%
      group_by(ID) %>%
      filter(n_distinct(year) == length(all_years)) %>%
      ungroup()
    
    if (nrow(df_bal) == 0) {
      cat("WARNING: No balanced panel in region:", region_name, "\n")
      return(c(ATT = NA, SE = NA, CI_L = NA, CI_U = NA))
    }
    
    # Panel matrices
    pm <- panel.matrices(
      as.data.frame(df_bal),
      unit      = "ID",
      time      = "year",
      outcome   = "mean_pm25",
      treatment = "W"
    )
    
    # SDiD estimate
    tau_hat <- synthdid_estimate(pm$Y, pm$N0, pm$T0)
    
    # Inference (jackknife)
    att     <- as.numeric(tau_hat)
    se      <- sqrt(vcov(tau_hat, method = "jackknife"))
    ci_l    <- att - 1.96 * se
    ci_u    <- att + 1.96 * se
    
    return(c(ATT = att, SE = se, CI_L = ci_l, CI_U = ci_u))
  }
  
  # ============================================================
  # B. REAL SDID (START YEAR = 2019)
  # ============================================================
  
  real_res <- estimate_sdid(2019)
  
  cat("\nREAL SDiD ATT FOR REGION ", region_name, ":\n", sep = "")
  cat("ATT =", round(real_res["ATT"], 3),
      ", SE =", round(real_res["SE"], 3),
      ", 95% CI [",
      round(real_res["CI_L"], 3), ", ",
      round(real_res["CI_U"], 3), "]\n", sep = "")
  
  # ============================================================
  # C. PLACEBO SDID LOOP
  # ============================================================
  
  placebo_output <- list()
  
  cat("\nRunning placebo SDiD tests...\n")
  
  for (py in placebo_years) {
    cat(" Placebo year:", py, "\n")
    
    pl <- estimate_sdid(py)
    
    placebo_output[[as.character(py)]] <- data.frame(
      Region = region_name,
      Year = py,
      ATT = round(pl["ATT"], 3),
      SE  = round(pl["SE"], 3),
      CI_L = round(pl["CI_L"], 3),
      CI_U = round(pl["CI_U"], 3),
      Type = "Placebo"
    )
  }
  
  # Convert placebo list to data frame
  placebo_df <- bind_rows(placebo_output)
  
  # ============================================================
  # D. COMBINE REAL + PLACEBO RESULTS
  # ============================================================
  
  real_df <- data.frame(
    Region = region_name,
    Year = 2019,
    ATT = round(real_res["ATT"], 3),
    SE  = round(real_res["SE"], 3),
    CI_L = round(real_res["CI_L"], 3),
    CI_U = round(real_res["CI_U"], 3),
    Type = "Real"
  )
  
  final_results <- bind_rows(real_df, placebo_df)
  
  return(final_results)
}

regional_placebo_results <- map_dfr(regions_list, run_regional_sdid)

print(regional_placebo_results)

write_csv(regional_placebo_results, "Regional_SDiD_Placebo_Results.csv")