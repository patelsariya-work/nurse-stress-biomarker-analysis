# correlation_analysis.R — Part 3: Correlation Matrix Heatmap (15%)
# Computes Spearman correlation matrix and generates heatmaps.
#
# Correlation Method: Spearman (rank-based)
# Justification:
#   - Physiological signals often have non-linear relationships
#   - Spearman is robust to outliers common in wearable sensor data
#   - No assumption of normality required
#
# Depends on: feature_engineering.R (produces physio_features.rds)

setwd("D:/Sanius")

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggcorrplot)
  library(corrplot)
})

PROCESSED_DIR <- file.path("data", "processed")
FIGURES_DIR   <- file.path("output", "figures")
dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)

# Load feature-enriched data
message("Loading feature-enriched dataset...")
physio_df <- readRDS(file.path(PROCESSED_DIR, "physio_features.rds"))
message("  ", format(nrow(physio_df), big.mark = ","), " rows loaded")

# Select numeric features for correlation
# BVP excluded: at 1Hz resolution it shows near-zero correlation (<0.01)
# with all other signals — its information is better captured via HR
cor_vars <- c(
  "hr",                # Heart Rate (bpm)
  "eda",               # Electrodermal Activity
  "temp",              # Skin Temperature
  "acc_magnitude",     # Accelerometer magnitude
  "eda_rolling_60s",   # Engineered: smoothed EDA
  "hr_variability",    # Engineered: HR SD (HRV proxy)
  "acc_rolling_60s"    # Engineered: smoothed activity
)

available_vars <- cor_vars[cor_vars %in% colnames(physio_df)]
message("  Using ", length(available_vars), " variables: ",
        paste(available_vars, collapse = ", "))

# Prepare correlation data
cor_data <- physio_df %>%
  select(all_of(available_vars)) %>%
  drop_na()

message("  Complete cases: ", format(nrow(cor_data), big.mark = ","),
        " (", round(100 * nrow(cor_data) / nrow(physio_df), 1), "% of total)")

# Compute Spearman correlation matrix
message("\nComputing Spearman correlation matrix...")

cor_matrix <- cor(cor_data, method = "spearman", use = "pairwise.complete.obs")

# Clean variable names for display
display_names <- c(
  hr              = "Heart Rate",
  eda             = "EDA (Raw)",
  temp            = "Skin Temp",
  acc_magnitude   = "ACC Magnitude",
  eda_rolling_60s = "EDA (60s Mean)",
  hr_variability  = "HR Variability",
  acc_rolling_60s = "ACC (60s Mean)"
)

rownames(cor_matrix) <- display_names[rownames(cor_matrix)]
colnames(cor_matrix) <- display_names[colnames(cor_matrix)]

cat("\nSpearman Correlation Matrix:\n")
print(round(cor_matrix, 3))

# Heatmap 1: ggcorrplot (primary)
message("\nGenerating heatmap visualisations...")

p1 <- ggcorrplot(
  cor_matrix,
  method   = "square",
  type     = "lower",
  lab      = TRUE,
  lab_size = 3.5,
  colors   = c("#2166AC", "white", "#B2182B"),
  title    = "Spearman Correlation — Physiological Signals",
  ggtheme  = theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
)

ggsave(
  file.path(FIGURES_DIR, "correlation_heatmap.png"),
  plot = p1, width = 10, height = 8, dpi = 300
)
message("  Saved: ", file.path(FIGURES_DIR, "correlation_heatmap.png"))

# Heatmap 2: corrplot (clustered)
png(file.path(FIGURES_DIR, "correlation_heatmap_corrplot.png"),
    width = 2400, height = 2000, res = 300)

corrplot(
  cor_matrix,
  method     = "color",
  type       = "lower",
  order      = "hclust",
  addCoef.col = "black",
  number.cex = 0.7,
  tl.col     = "black",
  tl.srt     = 45,
  tl.cex     = 0.8,
  col        = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200),
  title      = "Spearman Correlation — Hierarchically Clustered",
  mar        = c(0, 0, 2, 0)
)

dev.off()
message("  Saved: ", file.path(FIGURES_DIR, "correlation_heatmap_corrplot.png"))

# Top correlations
cor_flat <- as.data.frame(as.table(cor_matrix)) %>%
  filter(Var1 != Var2) %>%
  mutate(abs_cor = abs(Freq)) %>%
  arrange(desc(abs_cor)) %>%
  distinct(abs_cor, .keep_all = TRUE) %>%
  head(10)

message("\nTop 10 strongest correlations:")
for (i in 1:nrow(cor_flat)) {
  message("  ", cor_flat$Var1[i], " <-> ", cor_flat$Var2[i],
          " : r = ", round(cor_flat$Freq[i], 3))
}

message("\nInterpretation:")
message("  1. Strong correlation between raw signals and their rolling averages")
message("     validates the feature engineering approach.")
message("  2. EDA-HR correlation reflects sympathetic co-activation during stress.")
message("  3. ACC magnitude helps disambiguate physical vs psychological stress.")
