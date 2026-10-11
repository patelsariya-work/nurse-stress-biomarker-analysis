# feature_engineering.R — Part 2: Feature Engineering
# Creates derived features from the 1Hz physiological data.
# Depends on: data_loading.R (physio_data.rds)

setwd("D:/Sanius")

suppressPackageStartupMessages({
  library(tidyverse)
  library(zoo)
  library(lubridate)
})

PROCESSED_DIR <- file.path("data", "processed")
dir.create(file.path("output", "tables"), recursive = TRUE, showWarnings = FALSE)

message("Loading physio data...")
physio_df <- readRDS(file.path(PROCESSED_DIR, "physio_data.rds"))
message("  ", format(nrow(physio_df), big.mark = ","), " rows, ",
        n_distinct(physio_df$subject_id), " participants")

# Feature 1: 60s trailing mean of EDA — smooths motion artifacts
message("Computing EDA rolling mean (60s)...")
physio_df <- physio_df %>%
  group_by(subject_id, session_id) %>%
  arrange(timestamp) %>%
  mutate(
    eda_rolling_60s = rollapply(eda, width = 60, FUN = mean,
                                fill = NA, align = "right", na.rm = TRUE)
  ) %>%
  ungroup()

# Feature 2: 5-min rolling SD of HR — proxy for heart rate variability (HRV)
message("Computing HR variability (5-min SD)...")
physio_df <- physio_df %>%
  group_by(subject_id, session_id) %>%
  arrange(timestamp) %>%
  mutate(
    hr_variability = rollapply(hr, width = 300, FUN = sd,
                               fill = NA, align = "right", na.rm = TRUE)
  ) %>%
  ungroup()

# Feature 3: shift phase based on hour of day
message("Segmenting by time of day...")
physio_df <- physio_df %>%
  mutate(
    hour_of_day = hour(timestamp),
    time_segment = case_when(
      hour_of_day >= 6  & hour_of_day < 10 ~ "Early Shift",
      hour_of_day >= 10 & hour_of_day < 14 ~ "Mid Shift",
      hour_of_day >= 14 & hour_of_day < 18 ~ "Late Shift",
      TRUE ~ "Night"
    ),
    time_segment = factor(time_segment,
                          levels = c("Early Shift", "Mid Shift",
                                     "Late Shift", "Night"))
  )

# Feature 4: 60s trailing mean of accelerometer magnitude
message("Computing ACC rolling mean (60s)...")
physio_df <- physio_df %>%
  group_by(subject_id, session_id) %>%
  arrange(timestamp) %>%
  mutate(
    acc_rolling_60s = rollapply(acc_magnitude, width = 60, FUN = mean,
                                fill = NA, align = "right", na.rm = TRUE)
  ) %>%
  ungroup()

# Summary
new_feats <- c("eda_rolling_60s", "hr_variability", "acc_rolling_60s")
for (f in new_feats) {
  coverage <- round(100 * mean(!is.na(physio_df[[f]])), 1)
  message("  ", f, ": ", coverage, "% coverage")
}

message("\nTime segment distribution:")
physio_df %>% count(time_segment) %>% mutate(pct = round(100 * n / sum(n), 1)) %>% print()

saveRDS(physio_df, file.path(PROCESSED_DIR, "physio_features.rds"))
message("Saved: ", file.path(PROCESSED_DIR, "physio_features.rds"))
