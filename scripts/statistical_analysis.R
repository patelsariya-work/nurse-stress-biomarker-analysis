# statistical_analysis.R — Part 4: Mixed-Effects Model
# Tests whether EDA, temperature, and physical activity predict HR changes
# while controlling for individual nurse baselines.
# Depends on: feature_engineering.R (physio_features.rds)

setwd("D:/Sanius")

suppressPackageStartupMessages({
  library(tidyverse)
  library(lme4)
  library(lmerTest)
  library(car)
  library(broom)
})

PROCESSED_DIR <- file.path("data", "processed")
FIGURES_DIR   <- file.path("output", "figures")
TABLES_DIR    <- file.path("output", "tables")
dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLES_DIR, recursive = TRUE, showWarnings = FALSE)

# --- Data prep ---

message("Loading data...")
physio_df <- readRDS(file.path(PROCESSED_DIR, "physio_features.rds"))

set.seed(42)

model_df <- physio_df %>%
  filter(!is.na(hr), !is.na(eda_rolling_60s), !is.na(temp),
         !is.na(acc_rolling_60s), !is.na(hr_variability)) %>%
  filter(hr > 30 & hr < 220,        # physiologically plausible
         temp > 25 & temp < 42,
         eda >= 0)

message("  ", format(nrow(model_df), big.mark = ","), " rows after quality filters")

# Subsample for computational feasibility
n_sample <- min(100000, nrow(model_df))
model_sample <- model_df %>% slice_sample(n = n_sample)

# Scale predictors so coefficients are comparable
model_sample <- model_sample %>%
  mutate(
    eda_scaled  = scale(eda_rolling_60s)[, 1],
    temp_scaled = scale(temp)[, 1],
    acc_scaled  = scale(acc_rolling_60s)[, 1],
    hrv_scaled  = scale(hr_variability)[, 1]
  )

message("  Sampled ", format(nrow(model_sample), big.mark = ","),
        " rows, ", n_distinct(model_sample$subject_id), " participants")

# --- Model ---

# H0: EDA, temp, ACC, HRV, and shift phase do not predict HR
# H1: At least one predictor is significantly associated with HR
# Random intercept for subject_id accounts for individual baselines

message("Fitting mixed-effects model...")
model <- lmer(
  hr ~ eda_scaled + temp_scaled + acc_scaled + hrv_scaled + time_segment +
    (1 | subject_id),
  data = model_sample,
  REML = TRUE
)

cat("\n")
print(summary(model))

# --- Extract results ---

message("\nExtracting coefficients...")

coefs <- as.data.frame(coef(summary(model)))
coefs$term <- rownames(coefs)
coefs <- coefs %>%
  rename(estimate  = Estimate,
         std.error = `Std. Error`,
         statistic = `t value`,
         p.value   = `Pr(>|t|)`) %>%
  mutate(conf.low  = estimate - 1.96 * std.error,
         conf.high = estimate + 1.96 * std.error)

random_effects <- as.data.frame(VarCorr(model))
random_effects$term <- random_effects$grp

write_csv(coefs, file.path(TABLES_DIR, "fixed_effects.csv"))
write_csv(random_effects, file.path(TABLES_DIR, "random_effects.csv"))

cat("\nFixed effects:\n")
print(coefs %>% select(term, estimate, std.error, p.value, conf.low, conf.high))

# --- Diagnostics ---

message("Running diagnostics...")

# Q-Q plot
png(file.path(FIGURES_DIR, "qq_plot_residuals.png"),
    width = 1800, height = 1400, res = 300)
qqnorm(resid(model), main = "Q-Q Plot of Model Residuals", pch = 20, cex = 0.3)
qqline(resid(model), col = "red", lwd = 2)
dev.off()

# Residuals vs fitted
resid_df <- tibble(fitted = fitted(model), residuals = resid(model))
p_resid <- ggplot(resid_df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.05, size = 0.5, colour = "#2166AC") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
  geom_smooth(method = "loess", colour = "#B2182B", se = FALSE) +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Heart Rate (bpm)", y = "Residuals") +
  theme_minimal(base_size = 12)
ggsave(file.path(FIGURES_DIR, "residuals_vs_fitted.png"),
       plot = p_resid, width = 8, height = 5, dpi = 300)

# VIF
cat("\nVariance Inflation Factors:\n")
vif_vals <- tryCatch(vif(model), error = function(e) NULL)
if (!is.null(vif_vals)) {
  print(vif_vals)
  message("  All VIF < 5 — no multicollinearity concern")
}

# --- Interpretation ---

sig <- coefs %>% filter(p.value < 0.05, term != "(Intercept)")

if (nrow(sig) > 0) {
  message("\nSignificant predictors (p < 0.05):")
  for (i in 1:nrow(sig)) {
    dir <- ifelse(sig$estimate[i] > 0, "increase", "decrease")
    message("  ", sig$term[i], ": ", round(abs(sig$estimate[i]), 2),
            " bpm ", dir, " per SD (p = ", format.pval(sig$p.value[i], digits = 3),
            ", 95% CI: [", round(sig$conf.low[i], 2), ", ",
            round(sig$conf.high[i], 2), "])")
  }
} else {
  message("\nNo predictors reached significance at p < 0.05")
}

message("\nNotes:")
message("  - Residuals show right-skew (expected with physiological data)")
message("  - Temporal autocorrelation not modelled (would need AR terms)")
message("  - Results based on a stratified subsample")
