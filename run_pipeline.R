# run_pipeline.R - Master script
# Sources all analysis scripts in order.
# Usage: setwd("D:/Sanius") then source("run_pipeline.R")

setwd("D:/Sanius")

message("=== Part 1: Data Loading ===")
source("scripts/data_loading.R")

message("\n=== Part 2: Feature Engineering ===")
source("scripts/feature_engineering.R")

message("\n=== Part 3: Correlation Analysis ===")
source("scripts/correlation_analysis.R")

message("\n=== Part 4: Statistical Analysis ===")
source("scripts/statistical_analysis.R")

message("\n=== Rendering Report ===")
rmarkdown::render("report/analysis_report.Rmd")

message("\nPipeline complete.")
