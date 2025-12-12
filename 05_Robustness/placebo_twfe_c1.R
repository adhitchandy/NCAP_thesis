
### DiD Robustness Check - Placebo Test (Run after running C1)

placebo_years <- c(2015, 2016, 2017)

for (p in placebo_years) {
  df_placebo <- df %>% mutate(post_placebo = ifelse(year >= p, 1, 0))
  
  model_placebo <- feols(
    mean_pm25 ~ post_placebo * treatment | ID + time_id,
    data = df_placebo,
    cluster = ~ ID
  )
  
  cat("\n=== Placebo Treatment Year:", p, "===\n")
  print(summary(model_placebo))
}