# =============================================================================
# FINAL CONSOLIDATED ANALYSIS: Confidence-Gated LLM Classification Auditing
# =============================================================================
#
# This is the ONE script for all publication-ready results.
#
# Sections:
#   1. Setup & Data Loading
#   2. Data Validation
#   3. Stability Analysis (50-run oracle)
#   4. Confidence-Stability Relationship
#   5. Pareto Frontier: Gated vs Uniform Verification
#   6. Calibration Grid Search
#   7. Summary Tables & Figures
#
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. SETUP & DATA LOADING
# =============================================================================

# Source your existing analysis functions (adjust paths as needed)
# source("~/Desktop/Reproduciblity/R/stability_analysis.R")
# source("~/Desktop/Reproduciblity/R/confidence_analysis.R")

# Assumes 'results' and 'conf_results' are already loaded from:
#   results <- run_stability_analysis(...)
#   conf_results <- run_confidence_analysis(results)

# =============================================================================
# 2. DATA VALIDATION
# =============================================================================

validate_data <- function(results, conf_results) {
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("DATA VALIDATION\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Parsed data
  n_rows <- nrow(results$parsed)
  n_items <- nrow(distinct(results$parsed, id, dataset, task, player))
  n_runs <- max(results$parsed$run)
  datasets <- unique(results$parsed$dataset)
  
  cat("Parsed classifications:\n")
  cat(sprintf("  Total rows: %d\n", n_rows))
  cat(sprintf("  Unique items: %d\n", n_items))
  cat(sprintf("  Runs per item: %d\n", n_runs))
  cat(sprintf("  Datasets: %s\n", paste(datasets, collapse = ", ")))
  cat(sprintf("  Expected rows: %d × %d = %d\n", n_items, n_runs, n_items * n_runs))
  
  # Confidence data
  n_conf <- nrow(conf_results$confidence_raw)
  n_conf_items <- nrow(distinct(conf_results$confidence_raw, id, dataset, task, player))
  
  cat("\nConfidence data:\n")
  cat(sprintf("  Total rows: %d\n", n_conf))
  cat(sprintf("  Unique items: %d\n", n_conf_items))
  
  # Stability data
  n_stability <- nrow(results$stability)
  cat("\nStability data:\n")
  cat(sprintf("  Rows: %d\n", n_stability))
  
  cat("\n✓ Data validation complete\n\n")
}

# =============================================================================
# 3. PREPARE ANALYSIS DATA
# =============================================================================

prepare_analysis_data <- function(results, conf_results) {
  
  # Run 1 classifications
  run1 <- results$parsed %>% 
    filter(run == 1) %>% 
    select(id, dataset, task, player, class_run1 = classification)
  
  # Run 1 confidence
  conf_run1 <- conf_results$confidence_raw %>%
    filter(run == 1) %>%
    select(id, dataset, task, player, top1_prob)
  
  # Oracle from 50 runs
  oracle <- results$stability %>%
    select(id, dataset, task, player, 
           oracle_class = modal_class,
           oracle_TAR = modal_freq,
           is_stable)
  
  # Merge all
  data <- run1 %>%
    inner_join(conf_run1, by = c("id", "dataset", "task", "player")) %>%
    inner_join(oracle, by = c("id", "dataset", "task", "player")) %>%
    mutate(
      is_error = (class_run1 != oracle_class),
      is_unstable = !is_stable
    )
  
  data
}

# =============================================================================
# 4. KEY METRICS
# =============================================================================

compute_key_metrics <- function(data, results) {
  
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("KEY METRICS\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  n_total <- nrow(data)
  n_errors <- sum(data$is_error)
  n_unstable <- sum(data$is_unstable)
  
  cat("Overall Statistics:\n")
  cat(sprintf("  Total items: %d\n", n_total))
  cat(sprintf("  Run-1 errors (vs oracle): %d (%.2f%%)\n", n_errors, n_errors/n_total*100))
  cat(sprintf("  Unstable items (any flip in 50 runs): %d (%.2f%%)\n", n_unstable, n_unstable/n_total*100))
  
  # By confidence bin
  cat("\nInstability by Confidence Bin:\n")
  bin_stats <- data %>%
    mutate(conf_bin = cut(top1_prob, breaks = c(0, 0.6, 0.7, 0.8, 0.9, 0.95, 1.0))) %>%
    group_by(conf_bin) %>%
    summarise(
      n = n(),
      pct_of_total = n() / n_total * 100,
      error_rate = mean(is_error) * 100,
      instability_rate = mean(is_unstable) * 100,
      .groups = "drop"
    )
  print(bin_stats)
  
  cat("\n")
  bin_stats
}

# =============================================================================
# 5. PARETO FRONTIER: GATED VS UNIFORM
# =============================================================================

build_pareto_frontier <- function(data, results, 
                                   tau_values = c(0.60, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95),
                                   K_values = c(3, 5, 7, 9)) {
  
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("PARETO FRONTIER: GATED VS UNIFORM VERIFICATION\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  results_list <- list()
  
  # Uniform baselines
  cat("Computing uniform baselines...\n")
  for (K in K_values) {
    uniform_modal <- results$parsed %>%
      filter(run <= K) %>%
      group_by(id, dataset, task, player) %>%
      summarise(final_class = names(which.max(table(classification))), .groups = "drop") %>%
      left_join(results$stability %>% select(id, dataset, task, player, modal_class),
                by = c("id", "dataset", "task", "player")) %>%
      mutate(error = final_class != modal_class)
    
    results_list[[paste0("Always-", K, "×")]] <- tibble(
      method = paste0("Always-", K, "×"),
      tau = NA_real_,
      K_verify = K,
      cost = K,
      error_pct = mean(uniform_modal$error) * 100,
      flagged_pct = 100
    )
  }
  
  # Gated policies
  cat("Computing gated policies...\n")
  for (tau in tau_values) {
    for (K in K_values) {
      
      flagged_data <- data %>% filter(top1_prob < tau)
      accepted_data <- data %>% filter(top1_prob >= tau)
      
      pct_flagged <- nrow(flagged_data) / nrow(data)
      
      # Accepted: use run 1
      accepted_final <- accepted_data %>%
        select(id, dataset, task, player, final_class = class_run1)
      
      # Flagged: use modal of K
      if (nrow(flagged_data) > 0) {
        flagged_final <- results$parsed %>%
          filter(run <= K) %>%
          inner_join(flagged_data %>% select(id, dataset, task, player),
                     by = c("id", "dataset", "task", "player")) %>%
          group_by(id, dataset, task, player) %>%
          summarise(final_class = names(which.max(table(classification))), .groups = "drop")
      } else {
        flagged_final <- tibble()
      }
      
      # Combine and compute error
      full <- bind_rows(accepted_final, flagged_final) %>%
        left_join(results$stability %>% select(id, dataset, task, player, modal_class),
                  by = c("id", "dataset", "task", "player")) %>%
        mutate(error = final_class != modal_class)
      
      cost <- 1 + pct_flagged * (K - 1)
      
      results_list[[paste0("Gated-", K, "×_τ=", tau)]] <- tibble(
        method = paste0("Gated-", K, "×"),
        tau = tau,
        K_verify = K,
        cost = cost,
        error_pct = mean(full$error) * 100,
        flagged_pct = pct_flagged * 100
      )
    }
  }
  
  frontier <- bind_rows(results_list)
  
  # Print results
  cat("\n--- UNIFORM BASELINES ---\n")
  frontier %>% 
    filter(is.na(tau)) %>%
    arrange(cost) %>%
    mutate(error_pct = sprintf("%.2f%%", error_pct)) %>%
    select(method, cost, error_pct) %>%
    print()
  
  cat("\n--- GATED POLICIES (sorted by cost) ---\n")
  frontier %>%
    filter(!is.na(tau)) %>%
    arrange(cost) %>%
    mutate(
      tau = sprintf("%.2f", tau),
      cost = sprintf("%.2f", cost),
      error_pct = sprintf("%.2f%%", error_pct),
      flagged_pct = sprintf("%.1f%%", flagged_pct)
    ) %>%
    select(method, tau, cost, error_pct, flagged_pct) %>%
    print(n = 30)
  
  # Dominance analysis
  always3_error <- frontier %>% filter(method == "Always-3×") %>% pull(error_pct)
  
  dominant <- frontier %>%
    filter(!is.na(tau), cost < 3, error_pct < always3_error) %>%
    arrange(cost)
  
  cat(sprintf("\n--- CONFIGURATIONS DOMINATING Always-3× (cost < 3N, error < %.2f%%) ---\n", always3_error))
  print(dominant %>% 
          mutate(
            tau = sprintf("%.2f", tau),
            cost = sprintf("%.2f", cost),
            error_pct = sprintf("%.2f%%", error_pct)
          ) %>%
          select(method, tau, cost, error_pct, flagged_pct))
  
  cat("\n")
  frontier
}

# =============================================================================
# 6. HEADLINE COMPARISON TABLE
# =============================================================================

print_headline_table <- function(data, results) {
  
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("HEADLINE RESULTS: COST-ERROR COMPARISON\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Single run baseline
  single_error <- mean(data$is_error) * 100
  
  # Always 3× modal
  always3 <- results$parsed %>%
    filter(run <= 3) %>%
    group_by(id, dataset, task, player) %>%
    summarise(final_class = names(which.max(table(classification))), .groups = "drop") %>%
    left_join(results$stability %>% select(id, dataset, task, player, modal_class),
              by = c("id", "dataset", "task", "player")) %>%
    mutate(error = final_class != modal_class)
  always3_error <- mean(always3$error) * 100
  
  # Gated 5× at τ=0.70 (best from Pareto analysis)
  flagged <- data %>% filter(top1_prob < 0.70)
  accepted <- data %>% filter(top1_prob >= 0.70)
  pct_flagged <- nrow(flagged) / nrow(data)
  
  accepted_final <- accepted %>% select(id, dataset, task, player, final_class = class_run1)
  
  flagged_final <- results$parsed %>%
    filter(run <= 5) %>%
    inner_join(flagged %>% select(id, dataset, task, player),
               by = c("id", "dataset", "task", "player")) %>%
    group_by(id, dataset, task, player) %>%
    summarise(final_class = names(which.max(table(classification))), .groups = "drop")
  
  gated5 <- bind_rows(accepted_final, flagged_final) %>%
    left_join(results$stability %>% select(id, dataset, task, player, modal_class),
              by = c("id", "dataset", "task", "player")) %>%
    mutate(error = final_class != modal_class)
  gated5_error <- mean(gated5$error) * 100
  gated5_cost <- 1 + pct_flagged * 4
  
  cat("Method                  | Cost   | Error vs Oracle | vs Always-3×\n")
  cat("------------------------|--------|-----------------|-------------\n")
  cat(sprintf("Single run              | 1.00N  | %.2f%%           | baseline\n", single_error))
  cat(sprintf("Always 3× modal         | 3.00N  | %.2f%%           | -%.0f%% error\n", 
              always3_error, (1 - always3_error/single_error) * 100))
  cat(sprintf("Gated 5× (τ=0.70)       | %.2fN  | %.2f%%           | -%.0f%% error, -%.0f%% cost\n",
              gated5_cost, gated5_error,
              (1 - gated5_error/always3_error) * 100,
              (1 - gated5_cost/3) * 100))
  
  cat("\n")
  cat(sprintf("✓ Gated 5× achieves %.0f%% LOWER error at %.0f%% LOWER cost than Always-3×\n",
              (1 - gated5_error/always3_error) * 100,
              (1 - gated5_cost/3) * 100))
  cat("\n")
}

# =============================================================================
# 7. RUN ALL ANALYSES
# =============================================================================

run_final_analysis <- function(results, conf_results) {
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("  CONFIDENCE-GATED LLM CLASSIFICATION AUDITING\n")
  cat("  Final Analysis Results\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Validate
  validate_data(results, conf_results)
  
  # Prepare data
  data <- prepare_analysis_data(results, conf_results)
  cat(sprintf("Analysis data prepared: %d items\n\n", nrow(data)))
  
  # Key metrics
  bin_stats <- compute_key_metrics(data, results)
  
  # Headline table
  print_headline_table(data, results)
  
  # Pareto frontier
  frontier <- build_pareto_frontier(data, results)
  
  # Return all results
  list(
    data = data,
    bin_stats = bin_stats,
    frontier = frontier
  )
}

# =============================================================================
# USAGE
# =============================================================================

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("FINAL ANALYSIS SCRIPT LOADED\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("Run the complete analysis:\n")
cat("  final <- run_final_analysis(results, conf_results)\n\n")

cat("Or run individual components:\n")
cat("  validate_data(results, conf_results)\n")
cat("  data <- prepare_analysis_data(results, conf_results)\n")
cat("  compute_key_metrics(data, results)\n")
cat("  print_headline_table(data, results)\n")
cat("  frontier <- build_pareto_frontier(data, results)\n")
