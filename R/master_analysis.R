# =============================================================================
# MASTER ANALYSIS SCRIPT - Complete Workflow
# =============================================================================
#
# This script runs the complete analysis pipeline for LLM classification
# stability and confidence-gated verification protocol.
#
# Usage:
#   MODEL <- "gpt-4o"  # or "gpt-4o-mini"
#   source("master_analysis.R")
#
# =============================================================================

library(tidyverse)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Set model if not already set
if (!exists("MODEL")) {
  MODEL <- "gpt-4o"
}

# Set working directory
setwd("~/Desktop/Reproduciblity/R")

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("  MASTER ANALYSIS PIPELINE\n")
cat(sprintf("  Model: %s\n", MODEL))
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# =============================================================================
# STEP 1: LOAD AND PROCESS DATA
# =============================================================================

cat("STEP 1: Loading and processing data...\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# Load stability analysis (parses all 50 runs)
source("stability_analysis.R")
# Note: run_stability_analysis() is called automatically when sourced

# Load confidence analysis (extracts logprobs)
source("confidence_analysis.R")
conf_results <- run_confidence_analysis(results, force_reload = FALSE)

cat("\n✓ Data loaded successfully\n")
cat(sprintf("  Items: %d\n", nrow(results$stability)))
cat(sprintf("  Runs: %d\n", max(results$stability$n_runs)))
cat(sprintf("  Unstable: %d (%.1f%%)\n", 
            sum(!results$stability$is_stable),
            mean(!results$stability$is_stable) * 100))

# =============================================================================
# STEP 2: VALIDATE CORE FINDING (LEAKAGE-FREE)
# =============================================================================

cat("\n")
cat("STEP 2: Validating core finding (leakage-free)...\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")

source("leakage_free_analysis.R")

# Main leakage-free analysis
lf <- leakage_free_analysis(results, conf_results)

# Cross-validation across runs
cv <- cross_validate_runs(results, conf_results, max_runs = 10)

# Summary for paper
cat("\n")
reliability_summary(cv)

# =============================================================================
# STEP 3: PARETO FRONTIER (GATED VS UNIFORM)
# =============================================================================

cat("\n")
cat("STEP 3: Building Pareto frontier...\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")

source("final_analysis.R")
final <- run_final_analysis(results, conf_results)

# =============================================================================
# STEP 4: CALIBRATION ANALYSIS (OPTIONAL - SLOW)
# =============================================================================

# Uncomment to run full calibration grid search (takes 15-30 min)
# 
# cat("\n")
# cat("STEP 4: Running calibration grid search...\n")
# cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")
# 
# source("calibration_grid_search_v2.R")
# grid <- run_grid_search(results, conf_results,
#                         pct_range = seq(0.05, 0.50, by = 0.01),
#                         K_range = 2:20,
#                         methods = c("random", "low_confidence", "stratified"),
#                         target_instability_rate = 0.01,
#                         n_bootstrap = 20)
# 
# saveRDS(grid, sprintf("~/Desktop/Reproduciblity/calibration_grid_%s.rds", MODEL))
# analyze_grid_results(grid)

# Quick grid search (faster, for testing)
cat("\n")
cat("STEP 4: Quick calibration grid search...\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")

source("calibration_grid_search_v2.R")
grid_quick <- quick_grid_search(results, conf_results, 
                                 target_instability_rate = 0.01)

cat("\nTop configurations by τ accuracy:\n")
print(grid_quick %>%
        arrange(tau_abs_error) %>%
        select(method, pct, K, cost, tau_mean, oracle_instab_mean, flagged_mean) %>%
        head(10) %>%
        mutate(
          pct = sprintf("%.0f%%", pct * 100),
          cost = sprintf("%.0f%%", cost * 100),
          tau_mean = sprintf("%.2f", tau_mean),
          oracle_instab_mean = sprintf("%.2f%%", oracle_instab_mean * 100),
          flagged_mean = sprintf("%.1f%%", flagged_mean * 100)
        ))

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("  ANALYSIS COMPLETE\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("Objects available:\n")
cat("  results      - Stability analysis (parsed, stability, task_summary)\n")
cat("  conf_results - Confidence analysis (confidence_raw, merged)\n")
cat("  lf           - Leakage-free analysis results\n")
cat("  cv           - Cross-validation results\n")
cat("  final        - Final analysis (data, frontier, bin_stats)\n")
cat("  grid_quick   - Quick calibration grid search\n")

cat("\nKey findings stored in:\n")
cat("  final$frontier      - Pareto frontier data\n")
cat("  lf$auc              - Leakage-free AUC\n")
cat("  cv                  - Cross-run reliability\n")

cat("\nTo save results:\n")
cat(sprintf("  saveRDS(final, '~/Desktop/Reproduciblity/final_%s.rds')\n", MODEL))
cat(sprintf("  saveRDS(grid_quick, '~/Desktop/Reproduciblity/grid_%s.rds')\n", MODEL))
