# Nurse Stress — Physiological Biomarker Analysis

Sanius Health Data Analyst (R) Technical Assessment  
Author: Sariya Patel  
Date: October 2026

## Overview

This project analyses the Nurse Stress Dataset (Empatica E4) to identify objective physiological biomarkers of acute workplace stress in frontline nurses during the COVID-19 pandemic.

## How to Run

1. Open RStudio and set your working directory to this project folder:
   ```r
   setwd("D:/Sanius")
   ```

2. Run the master pipeline script:
   ```r
   source("run_pipeline.R")
   ```

   This sources the scripts in order:
   - `scripts/data_loading.R` — ingests 600+ E4 sensor sessions, resamples to 1Hz
   - `scripts/feature_engineering.R` — derives 4 features (rolling EDA, HRV proxy, shift phase, rolling ACC)
   - `scripts/correlation_analysis.R` — Spearman correlation matrix + heatmaps
   - `scripts/statistical_analysis.R` — mixed-effects model + diagnostics

3. Render the report:
   ```r
   rmarkdown::render("report/analysis_report.Rmd")
   ```

## Required Packages

tidyverse, readxl, lubridate, zoo, ggcorrplot, corrplot, lme4, lmerTest, car, broom, knitr, kableExtra, rmarkdown

Install all at once:
```r
install.packages(c("tidyverse", "readxl", "lubridate", "zoo", "ggcorrplot",
                   "corrplot", "lme4", "lmerTest", "car", "broom",
                   "knitr", "kableExtra", "rmarkdown"))
```

## Project Structure

```
├── scripts/
│   ├── data_loading.R              Part 1 — ingestion & integration
│   ├── feature_engineering.R       Part 2 — derived features
│   ├── correlation_analysis.R      Part 3 — Spearman heatmap
│   └── statistical_analysis.R      Part 4 — mixed-effects model
├── report/
│   ├── analysis_report.Rmd         Parts 5 & 6 — insights + data quality
│   └── analysis_report.html        Rendered report
├── presentation/
│   └── Nurse_Stress_Executive_Summary.pptx
├── output/
│   ├── figures/                    Heatmaps, diagnostic plots
│   └── tables/                    Model coefficients (CSV)
├── run_pipeline.R                  Master script
└── README.md                       This file
```

## Key Assumptions

1. **Empatica E4 CSV format:** Row 1 = Unix epoch timestamp, Row 2 = sampling frequency (Hz), Row 3+ = data values. This is the standard E4 export format.

2. **1Hz resampling:** All signals are down-sampled to 1Hz (HR's native rate) by per-second averaging. This loses some high-frequency detail but is necessary for cross-signal alignment.

3. **tags.csv excluded:** Empty across all sessions per the dataset documentation.

4. **BVP excluded from correlation analysis:** At 1Hz resolution, BVP shows near-zero correlation (< 0.01) with all other signals. Its information content is better captured through the HR signal.

5. **Physiological filters:** HR < 30 or > 220 bpm, Temp < 25 or > 42°C, and EDA < 0 are treated as sensor artifacts and excluded from modelling.

## Limitations

- **Temporal autocorrelation:** The mixed-effects model does not include AR terms, so consecutive observations are not fully independent. A GAMM or time-series approach would address this.
- **Residual non-normality:** The Q-Q plot shows right-skewed residuals, expected with physiological data. This may affect confidence interval coverage but does not invalidate point estimates.
- **Sample size:** 15 participants limits the generalisability of random effects estimates. A larger, multi-site cohort would strengthen external validity.
- **Survey data integration:** Self-reported stress annotations were loaded but not time-aligned with physiological data due to imprecise timestamp formats in the survey. Future work should establish a joining protocol using session-level time windows.
