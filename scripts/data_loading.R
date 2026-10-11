
# data_loading.R — Part 1: Data Ingestion & Integration (30%)
#
# Sanius Health — Data Analyst (R) 
# Dataset: Nurse Stress Dataset (Empatica E4)
#

#   1. Loads all physiological CSV files across 15 participants and sessions
#   2. Handles the Empatica E4 CSV format (row 1 = epoch, row 2 = Hz)
#   3. Resamples all signals to a common 1 Hz frequency
#   4. Merges physiological data with SurveyResults.xlsx stress annotations
#   5. Produces a single unified analytical dataset


setwd("D:/Sanius")

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(lubridate)
  library(zoo)
})


# Configuration — relative paths from the project root (D:/Sanius)

DATA_DIR      <- file.path("data", "raw", "Stress_dataset")
SURVEY_FILE   <- file.path("data", "raw", "SurveyResults.xlsx")
OUTPUT_DIR    <- file.path("data", "processed")
FIGURES_DIR   <- file.path("output", "figures")

# Ensure output directories exist
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)


# ASSUMPTION 1: Empatica E4 CSV Format
#   Row 1 = Unix epoch timestamp (start time in seconds since 1970-01-01)
#   Row 2 = Sampling frequency in Hz
#   Row 3+ = Actual data values
#
# ASSUMPTION 2: We resample ALL signals to 1 Hz (the natural frequency of HR)
#    EDA (4 Hz) → 1 Hz via per-second mean
#    TEMP (4 Hz) → 1 Hz via per-second mean
#    ACC (32 Hz) → compute magnitude, then 1 Hz via per-second mean
#    BVP (64 Hz) → 1 Hz via per-second mean
#    IBI has a different format and is processed separately for HRV metrics
#
# ASSUMPTION 3: tags.csv contains no useful information (per dataset docs)
#   and is excluded from ingestion.
#
# Helper Functions

# Read an Empatica E4 CSV file (standard format: EDA, HR, TEMP, BVP, ACC)
# Returns a tibble with timestamp and signal value(s), or NULL on failure
read_empatica_csv <- function(filepath, signal_name) {

  # Check if file exists and is not empty
  if (!file.exists(filepath)) return(NULL)
  file_size <- file.info(filepath)$size
  if (is.na(file_size) || file_size < 10) return(NULL)

  # Read raw lines to extract metadata
  raw_lines <- readLines(filepath, n = 3, warn = FALSE)
  if (length(raw_lines) < 3) return(NULL)

  # Row 1: epoch start time | Row 2: sampling frequency
  epoch_start <- as.numeric(trimws(strsplit(raw_lines[1], ",")[[1]][1]))
  sample_freq <- as.numeric(trimws(strsplit(raw_lines[2], ",")[[1]][1]))

  if (is.na(epoch_start) || is.na(sample_freq) || sample_freq <= 0) return(NULL)

  # Read the actual data (skip first 2 rows)
  data <- tryCatch(
    read.csv(filepath, header = FALSE, skip = 2, stringsAsFactors = FALSE),
    error = function(e) return(NULL)
  )

  if (is.null(data) || nrow(data) == 0) return(NULL)

  # Generate timestamps at the correct frequency
  n_samples <- nrow(data)
  time_offsets <- seq(0, by = 1 / sample_freq, length.out = n_samples)
  timestamps <- as.POSIXct(epoch_start + time_offsets, origin = "1970-01-01", tz = "UTC")

  # Build output tibble
  if (signal_name == "ACC") {
    # ACC has 3 columns: x, y, z (in 1/64g units)
    colnames(data) <- c("acc_x", "acc_y", "acc_z")
    result <- tibble(
      timestamp = timestamps,
      acc_x = data$acc_x,
      acc_y = data$acc_y,
      acc_z = data$acc_z,
      acc_magnitude = sqrt(data$acc_x^2 + data$acc_y^2 + data$acc_z^2)
    )
  } else {
    colnames(data) <- tolower(signal_name)
    result <- tibble(
      timestamp = timestamps,
      !!tolower(signal_name) := data[[1]]
    )
  }

  attr(result, "sample_freq") <- sample_freq
  attr(result, "epoch_start") <- epoch_start
  return(result)
}


# Read an Empatica E4 IBI (Inter-Beat Interval) file

# IBI format is different: Col1 = time offset from epoch, Col2 = IBI in seconds
#
# @param filepath Path to IBI.csv
# @return A tibble with columns: timestamp, ibi
read_ibi_csv <- function(filepath) {
  if (!file.exists(filepath)) return(NULL)
  file_size <- file.info(filepath)$size
  if (is.na(file_size) || file_size < 10) return(NULL)

  raw_lines <- readLines(filepath, n = 2, warn = FALSE)
  if (length(raw_lines) < 2) return(NULL)

  epoch_start <- as.numeric(trimws(strsplit(raw_lines[1], ",")[[1]][1]))
  if (is.na(epoch_start)) return(NULL)

  data <- tryCatch(
    read.csv(filepath, header = FALSE, skip = 1, stringsAsFactors = FALSE),
    error = function(e) return(NULL)
  )

  if (is.null(data) || nrow(data) == 0) return(NULL)

  tibble(
    timestamp = as.POSIXct(epoch_start + data[[1]], origin = "1970-01-01", tz = "UTC"),
    ibi = data[[2]]
  )
}


# Resample a signal to 1 Hz by computing the mean per second
resample_to_1hz <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  df %>%
    mutate(timestamp_1hz = floor_date(timestamp, unit = "second")) %>%
    group_by(timestamp_1hz) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    rename(timestamp = timestamp_1hz)
}


# Load all signals for a single session folder
load_session <- function(session_path, subject_id) {

  signals <- list()

  # HR is already 1 Hz — no resampling needed
  hr_data <- read_empatica_csv(file.path(session_path, "HR.csv"), "HR")
  if (!is.null(hr_data)) signals[["hr"]] <- resample_to_1hz(hr_data)

  # EDA at 4 Hz → resample to 1 Hz
  eda_data <- read_empatica_csv(file.path(session_path, "EDA.csv"), "EDA")
  if (!is.null(eda_data)) signals[["eda"]] <- resample_to_1hz(eda_data)

  # TEMP at 4 Hz → resample to 1 Hz
  temp_data <- read_empatica_csv(file.path(session_path, "TEMP.csv"), "TEMP")
  if (!is.null(temp_data)) signals[["temp"]] <- resample_to_1hz(temp_data)

  # ACC at 32 Hz → compute magnitude, resample to 1 Hz
  acc_data <- read_empatica_csv(file.path(session_path, "ACC.csv"), "ACC")
  if (!is.null(acc_data)) {
    acc_resampled <- resample_to_1hz(acc_data)
    # Keep only magnitude (drop raw x, y, z to reduce memory)
    signals[["acc"]] <- acc_resampled %>% select(timestamp, acc_magnitude)
  }

  # BVP at 64 Hz → resample to 1 Hz
  bvp_data <- read_empatica_csv(file.path(session_path, "BVP.csv"), "BVP")
  if (!is.null(bvp_data)) signals[["bvp"]] <- resample_to_1hz(bvp_data)

  # If no signals loaded, skip this session
  if (length(signals) == 0) return(NULL)

  # Merge all signals on timestamp (full join to keep all time points)
  merged <- signals %>%
    reduce(full_join, by = "timestamp") %>%
    arrange(timestamp) %>%
    mutate(
      subject_id = subject_id,
      session_id = basename(session_path)
    ) %>%
    select(subject_id, session_id, timestamp, everything())

  return(merged)
}


# Main Pipeline

message("Part 1: Data Ingestion & Integration")
message("Nurse Stress Dataset (Empatica E4)\n")

# Discover all participants and sessions
message("Discovering participants and sessions...")

participant_dirs <- list.dirs(DATA_DIR, recursive = FALSE, full.names = TRUE)
cat(sprintf("  Found %d participant folders: %s\n",
            length(participant_dirs),
            paste(basename(participant_dirs), collapse = ", ")))

# Load all sessions across all participants
message("\nLoading physiological data from all sessions...")

all_data <- list()
session_count <- 0
error_count <- 0

for (p_dir in participant_dirs) {
  subject_id <- basename(p_dir)
  session_dirs <- list.dirs(p_dir, recursive = FALSE, full.names = TRUE)

  for (s_dir in session_dirs) {
    session_count <- session_count + 1

    result <- tryCatch(
      load_session(s_dir, subject_id),
      error = function(e) {
        cat(sprintf("    WARNING: Error in %s/%s: %s\n",
                    subject_id, basename(s_dir), e$message))
        error_count <<- error_count + 1
        return(NULL)
      }
    )

    if (!is.null(result) && nrow(result) > 0) {
      all_data[[length(all_data) + 1]] <- result
    }
  }

  cat(sprintf("  Loaded participant %s (%d sessions)\n",
              subject_id, length(session_dirs)))
}

cat(sprintf("\n  Total sessions discovered: %d\n", session_count))
cat(sprintf("  Sessions with errors: %d\n", error_count))
cat(sprintf("  Sessions successfully loaded: %d\n", length(all_data)))

# Combine all sessions into one dataframe
# Combine all sessions into one dataframe
message("\nCombining into unified dataset...")
physio_df <- bind_rows(all_data)

cat(sprintf("  Unified dataset: %s rows x %s columns\n",
            format(nrow(physio_df), big.mark = ","),
            ncol(physio_df)))
cat(sprintf("  Participants: %d\n", n_distinct(physio_df$subject_id)))
cat(sprintf("  Time range: %s to %s\n",
            min(physio_df$timestamp, na.rm = TRUE),
            max(physio_df$timestamp, na.rm = TRUE)))

# Load survey data (stress annotations)
message("\nLoading survey/stress annotation data...")

# Read all sheets from SurveyResults.xlsx
sheet_names <- excel_sheets(SURVEY_FILE)
cat(sprintf("  Found %d sheets: %s\n",
            length(sheet_names),
            paste(sheet_names, collapse = ", ")))

survey_list <- lapply(sheet_names, function(sheet) {
  df <- tryCatch(
    read_excel(SURVEY_FILE, sheet = sheet),
    error = function(e) return(NULL)
  )
  if (!is.null(df) && nrow(df) > 0) {
    df$subject_id <- sheet
  }
  return(df)
})

survey_df <- bind_rows(survey_list)
cat(sprintf("  Survey data: %d rows x %d columns\n", nrow(survey_df), ncol(survey_df)))
cat(sprintf("  Columns: %s\n", paste(colnames(survey_df), collapse = ", ")))

# Save processed datasets
message("\nSaving processed datasets...")

saveRDS(physio_df, file.path(OUTPUT_DIR, "physio_data.rds"))
saveRDS(survey_df, file.path(OUTPUT_DIR, "survey_data.rds"))

# save as CSV for inspection
write_csv(
  physio_df %>% slice_sample(n = min(10000, nrow(.))),
  file.path(OUTPUT_DIR, "physio_data_sample.csv")
)
write_csv(survey_df, file.path(OUTPUT_DIR, "survey_data.csv"))

cat(sprintf("  Saved: %s\n", file.path(OUTPUT_DIR, "physio_data.rds")))
cat(sprintf("  Saved: %s\n", file.path(OUTPUT_DIR, "survey_data.rds")))


message("\nData Summary")

cat("Signal coverage per participant:\n")
physio_df %>%
  group_by(subject_id) %>%
  summarise(
    n_sessions  = n_distinct(session_id),
    n_rows      = n(),
    hours       = round(n() / 3600, 1),
    hr_pct      = round(100 * mean(!is.na(hr)), 1),
    eda_pct     = round(100 * mean(!is.na(eda)), 1),
    temp_pct    = round(100 * mean(!is.na(temp)), 1),
    .groups = "drop"
  ) %>%
  print(n = 20)

cat("\n\nOverall signal summary:\n")
physio_df %>%
  select(hr, eda, temp, acc_magnitude, bvp) %>%
  summary() %>%
  print()

# Print session info for reproducibility
cat("\n")
sessionInfo()
