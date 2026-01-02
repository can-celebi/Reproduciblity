# =============================================================================
# IMPROVED PROTOCOL VALIDATION
# =============================================================================
#
# Key improvements over previous version:
#   1. Task-specific calibration (not pooled)
#   2. X% of sample (not fixed N)
#   3. Compare calibrated τ to oracle τ (from all 50 runs)
#   4. Full cost breakdown
#   5. Error-based objective (not TAR-based)
#
# =============================================================================

library(tidyverse)

# =============================================================================
# PREPARE DATA
# =============================================================================

prepare_protocol_data <- function(results, conf_results) {
  
  # Run 1 classifications
  run1_class <- results$parsed %>% 
    filter(run == 1) %>% 
    select(id, dataset, task, player, class_run1 = classification)
  
  # Run 1 confidence
  conf_run1 <- conf_results$confidence_raw %>%
    filter(run == 1) %>%
    select(id, dataset, task, player, top1_prob_run1 = top1_prob)
  
  # Oracle: modal class from ALL 50 runs (ground truth)
  oracle <- results$stability %>%
    select(id, dataset, task, player, 
           oracle_class = modal_class, 
           oracle_TAR = modal_freq,
           oracle_flip_rate = flip_rate)
  
  # Merge
  data <- run1_class %>%
    inner_join(conf_run1, by = c("id", "dataset", "task", "player")) %>%
    inner_join(oracle, by = c("id", "dataset", "task", "player")) %>%
    mutate(
      is_error = (class_run1 != oracle_class),
      is_unstable = (oracle_TAR < 1.0)
    )
  
  data
}

# =============================================================================
# FIND ORACLE τ (from full 50-run data)
# =============================================================================

#' Find oracle τ - the best threshold given full knowledge
#' @param data Output from prepare_protocol_data
#' @param target_error_recall Target % of errors to catch (e.g., 0.95)
#' @param by_task If TRUE, find τ per task; if FALSE, find global τ
find_oracle_tau <- function(data, target_error_recall = 0.95, by_task = TRUE) {
  
  find_tau_for_data <- function(df, target) {
    errors <- df %>% filter(is_error)
    if (nrow(errors) == 0) return(list(tau = 0, recall = 1, flagged = 0))
    
    # Try thresholds from low to high
    thresholds <- sort(unique(df$top1_prob_run1))
    
    for (tau in thresholds) {
      recall <- mean(errors$top1_prob_run1 < tau)
      if (recall >= target) {
        flagged <- mean(df$top1_prob_run1 < tau)
        return(list(tau = tau, recall = recall, flagged = flagged))
      }
    }
    
    # If can't achieve target, use max threshold
    tau <- max(thresholds)
    return(list(
      tau = tau,
      recall = mean(errors$top1_prob_run1 < tau),
      flagged = mean(df$top1_prob_run1 < tau)
    ))
  }
  
  if (by_task) {
    tasks <- data %>% distinct(dataset, task)
    
    result <- map_dfr(1:nrow(tasks), function(i) {
      d <- tasks$dataset[i]
      t <- tasks$task[i]
      task_data <- data %>% filter(dataset == d, task == t)
      oracle <- find_tau_for_data(task_data, target_error_recall)
      
      tibble(
        dataset = d,
        task = t,
        n = nrow(task_data),
        n_errors = sum(task_data$is_error),
        error_rate = mean(task_data$is_error) * 100,
        oracle_tau = oracle$tau,
        oracle_recall = oracle$recall * 100,
        oracle_flagged = oracle$flagged * 100
      )
    })
    
    return(result)
    
  } else {
    oracle <- find_tau_for_data(data, target_error_recall)
    return(tibble(
      oracle_tau = oracle$tau,
      oracle_recall = oracle$recall * 100,
      oracle_flagged = oracle$flagged * 100
    ))
  }
}

# =============================================================================
# CALIBRATION PROTOCOL (percentage-based, task-specific)
# =============================================================================

#' Simulate calibration with X% of sample
#' @param data Output from prepare_protocol_data
#' @param results Full results object (for multi-run calibration)
#' @param calib_pct Percentage of data to use for calibration (e.g., 0.10 = 10%)
#' @param K_calib Number of runs for calibration items
#' @param target_error_recall Target error recall
#' @param by_task Task-specific or global calibration
#' @param n_bootstrap Number of bootstrap iterations
calibration_protocol <- function(data, results, 
                                  calib_pct = 0.10,
                                  K_calib = 10,
                                  target_error_recall = 0.95,
                                  by_task = TRUE,
                                  n_bootstrap = 50) {
  
  # Get multi-run data for calibration "ground truth"
  get_calib_truth <- function(ids_to_check, K) {
    results$parsed %>%
      filter(id %in% ids_to_check, run >= 2, run <= K + 1) %>%
      group_by(id, dataset, task, player) %>%
      summarise(
        calib_modal = {
          tbl <- table(classification)
          names(tbl)[which.max(tbl)]
        },
        .groups = "drop"
      )
  }
  
  # Function to learn τ from calibration sample
  learn_tau <- function(calib_data, target) {
    errors <- calib_data %>% filter(is_calib_error)
    if (nrow(errors) == 0) return(min(calib_data$top1_prob_run1))
    
    thresholds <- sort(unique(calib_data$top1_prob_run1))
    
    for (tau in thresholds) {
      recall <- mean(errors$top1_prob_run1 < tau)
      if (recall >= target) return(tau)
    }
    
    return(max(thresholds))
  }
  
  run_single_bootstrap <- function(task_data, task_d, task_t, K) {
    n_task <- nrow(task_data)
    n_calib <- max(5, ceiling(n_task * calib_pct))  # At least 5 items
    
    # Stratified sample: 50% from bottom 30% confidence, 50% from rest
    low_conf <- task_data %>% 
      filter(top1_prob_run1 < quantile(top1_prob_run1, 0.3))
    high_conf <- task_data %>% 
      filter(top1_prob_run1 >= quantile(top1_prob_run1, 0.3))
    
    n_low <- min(ceiling(n_calib * 0.5), nrow(low_conf))
    n_high <- n_calib - n_low
    
    if (n_low > 0 && nrow(low_conf) > 0) {
      low_sample <- low_conf %>% slice_sample(n = n_low, replace = nrow(low_conf) < n_low)
    } else {
      low_sample <- tibble()
    }
    
    if (n_high > 0 && nrow(high_conf) > 0) {
      high_sample <- high_conf %>% slice_sample(n = n_high, replace = nrow(high_conf) < n_high)
    } else {
      high_sample <- tibble()
    }
    
    calib_sample <- bind_rows(low_sample, high_sample)
    
    if (nrow(calib_sample) == 0) {
      return(tibble(
        dataset = task_d, task = task_t,
        tau_learned = NA, recall_achieved = NA, pct_flagged = NA
      ))
    }
    
    # Get "calibration ground truth" from K runs
    calib_truth <- get_calib_truth(calib_sample$id, K)
    
    calib_sample <- calib_sample %>%
      left_join(calib_truth %>% select(id, calib_modal), by = "id") %>%
      mutate(is_calib_error = (class_run1 != calib_modal))
    
    # Learn τ
    tau_learned <- learn_tau(calib_sample, target_error_recall)
    
    # Evaluate on FULL task data using ORACLE truth
    errors_full <- task_data %>% filter(is_error)
    recall_achieved <- if (nrow(errors_full) > 0) {
      mean(errors_full$top1_prob_run1 < tau_learned) * 100
    } else 100
    
    pct_flagged <- mean(task_data$top1_prob_run1 < tau_learned) * 100
    
    tibble(
      dataset = task_d,
      task = task_t,
      n_calib = nrow(calib_sample),
      tau_learned = tau_learned,
      recall_achieved = recall_achieved,
      pct_flagged = pct_flagged
    )
  }
  
  if (by_task) {
    tasks <- data %>% distinct(dataset, task)
    
    all_results <- map_dfr(1:nrow(tasks), function(i) {
      d <- tasks$dataset[i]
      t <- tasks$task[i]
      task_data <- data %>% filter(dataset == d, task == t)
      
      # Bootstrap
      map_dfr(1:n_bootstrap, function(b) {
        run_single_bootstrap(task_data, d, t, K_calib) %>%
          mutate(bootstrap = b)
      })
    })
    
    # Summarize
    summary <- all_results %>%
      group_by(dataset, task) %>%
      summarise(
        n_calib_mean = mean(n_calib, na.rm = TRUE),
        tau_mean = mean(tau_learned, na.rm = TRUE),
        tau_sd = sd(tau_learned, na.rm = TRUE),
        recall_mean = mean(recall_achieved, na.rm = TRUE),
        recall_sd = sd(recall_achieved, na.rm = TRUE),
        flagged_mean = mean(pct_flagged, na.rm = TRUE),
        flagged_sd = sd(pct_flagged, na.rm = TRUE),
        .groups = "drop"
      )
    
    return(list(raw = all_results, summary = summary))
    
  } else {
    # Global calibration
    all_results <- map_dfr(1:n_bootstrap, function(b) {
      run_single_bootstrap(data, "all", "all", K_calib) %>%
        mutate(bootstrap = b)
    })
    
    summary <- all_results %>%
      summarise(
        n_calib_mean = mean(n_calib, na.rm = TRUE),
        tau_mean = mean(tau_learned, na.rm = TRUE),
        tau_sd = sd(tau_learned, na.rm = TRUE),
        recall_mean = mean(recall_achieved, na.rm = TRUE),
        recall_sd = sd(recall_achieved, na.rm = TRUE),
        flagged_mean = mean(pct_flagged, na.rm = TRUE),
        flagged_sd = sd(pct_flagged, na.rm = TRUE)
      )
    
    return(list(raw = all_results, summary = summary))
  }
}

# =============================================================================
# COMPARE CALIBRATED τ TO ORACLE τ
# =============================================================================

#' Compare calibrated threshold to oracle threshold
compare_to_oracle <- function(oracle_results, calib_results, data) {
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("CALIBRATED τ vs ORACLE τ COMPARISON\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Merge oracle and calibration results
  comparison <- oracle_results %>%
    left_join(calib_results$summary, by = c("dataset", "task")) %>%
    mutate(
      tau_diff = tau_mean - oracle_tau,
      recall_diff = recall_mean - oracle_recall,
      flagged_diff = flagged_mean - oracle_flagged
    )
  
  cat("Per-Task Comparison:\n\n")
  print(comparison %>% 
          select(dataset, task, n, n_errors,
                 oracle_tau, tau_mean, tau_sd, tau_diff,
                 oracle_recall, recall_mean, recall_diff) %>%
          mutate(across(where(is.numeric), ~round(., 3))))
  
  cat("\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("SUMMARY:\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  cat(sprintf("Mean τ difference (calibrated - oracle): %.3f (SD: %.3f)\n",
              mean(comparison$tau_diff, na.rm = TRUE),
              sd(comparison$tau_diff, na.rm = TRUE)))
  cat(sprintf("Mean recall difference: %.1f%% (SD: %.1f%%)\n",
              mean(comparison$recall_diff, na.rm = TRUE),
              sd(comparison$recall_diff, na.rm = TRUE)))
  cat(sprintf("Mean flagged difference: %.1f%% (SD: %.1f%%)\n",
              mean(comparison$flagged_diff, na.rm = TRUE),
              sd(comparison$flagged_diff, na.rm = TRUE)))
  
  # Check if calibration is "good enough"
  if (mean(abs(comparison$tau_diff), na.rm = TRUE) < 0.1) {
    cat("\n✓ Calibration achieves τ within ±0.1 of oracle on average\n")
  } else {
    cat("\n⚠ Calibration τ differs from oracle by > 0.1 on average\n")
  }
  
  invisible(comparison)
}

# =============================================================================
# FULL COST ANALYSIS
# =============================================================================

#' Complete cost analysis comparing all strategies
#' @param data Output from prepare_protocol_data
#' @param tau Threshold to use
#' @param K_verify Number of runs for verification
cost_analysis <- function(data, tau, K_verify = 5, K_calib = 10, calib_pct = 0.10) {
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat(sprintf("COST ANALYSIS (τ = %.2f, K_verify = %d)\n", tau, K_verify))
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  n_total <- nrow(data)
  n_flagged <- sum(data$top1_prob_run1 < tau)
  pct_flagged <- n_flagged / n_total * 100
  
  # Strategy costs (API calls)
  cost_baseline <- n_total  # 1 run each
  cost_verify_all <- n_total * K_verify  # K runs each
  cost_protocol <- n_total + n_flagged * K_verify  # 1 + K for flagged
  cost_calibration <- ceiling(n_total * calib_pct) * K_calib  # One-time cost
  
  # Error performance
  errors <- data %>% filter(is_error)
  n_errors <- nrow(errors)
  errors_caught <- sum(errors$top1_prob_run1 < tau)
  error_recall <- if (n_errors > 0) errors_caught / n_errors * 100 else 100
  
  # Create comparison table
  strategies <- tibble(
    Strategy = c(
      "A: Single run (baseline)",
      sprintf("B: Verify all (%d runs)", K_verify),
      sprintf("C: Protocol (τ=%.2f)", tau)
    ),
    API_Calls = c(cost_baseline, cost_verify_all, cost_protocol),
    Cost_Multiplier = c(1, K_verify, cost_protocol / cost_baseline),
    Errors_Remaining = c(
      n_errors,  # All errors remain
      0,  # All caught (assuming perfect verification)
      n_errors - errors_caught  # Uncaught errors
    ),
    Error_Rate = c(
      n_errors / n_total * 100,
      0,
      (n_errors - errors_caught) / n_total * 100
    )
  )
  
  cat("STRATEGY COMPARISON:\n\n")
  print(strategies %>% mutate(across(where(is.numeric), ~round(., 2))))
  
  cat("\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("PROTOCOL DETAILS:\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  cat(sprintf("Total items: %d\n", n_total))
  cat(sprintf("Items flagged: %d (%.1f%%)\n", n_flagged, pct_flagged))
  cat(sprintf("Total errors in data: %d (%.2f%%)\n", n_errors, n_errors/n_total*100))
  cat(sprintf("Errors caught by flagging: %d (%.1f%% recall)\n", errors_caught, error_recall))
  cat(sprintf("Errors remaining: %d (%.2f%% residual error rate)\n", 
              n_errors - errors_caught, (n_errors - errors_caught)/n_total*100))
  
  cat("\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("COST BREAKDOWN:\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  cat(sprintf("Calibration (one-time): %d calls (%.1f%% × %d items × %d runs)\n",
              cost_calibration, calib_pct * 100, n_total, K_calib))
  cat(sprintf("Protocol application: %d calls\n", cost_protocol))
  cat(sprintf("  - Initial run (all): %d calls\n", n_total))
  cat(sprintf("  - Verification (flagged): %d × %d = %d calls\n", 
              n_flagged, K_verify, n_flagged * K_verify))
  
  cat("\n")
  cat(sprintf("Protocol cost: %.2f× baseline\n", cost_protocol / cost_baseline))
  cat(sprintf("Protocol cost: %.1f%% of verify-all\n", cost_protocol / cost_verify_all * 100))
  cat(sprintf("Savings vs verify-all: %.0f%% (%.0f calls saved)\n",
              (1 - cost_protocol / cost_verify_all) * 100,
              cost_verify_all - cost_protocol))
  
  invisible(list(
    strategies = strategies,
    details = list(
      n_total = n_total,
      n_flagged = n_flagged,
      n_errors = n_errors,
      errors_caught = errors_caught,
      error_recall = error_recall,
      cost_calibration = cost_calibration,
      cost_protocol = cost_protocol,
      cost_verify_all = cost_verify_all
    )
  ))
}

# =============================================================================
# MASTER FUNCTION: RUN ALL PROTOCOL VALIDATION
# =============================================================================

#' Run complete protocol validation
#' @param results Output from run_stability_analysis()
#' @param conf_results Output from run_confidence_analysis()
#' @param calib_pct Percentage of data for calibration
#' @param K_calib Runs for calibration
#' @param K_verify Runs for verification
#' @param target_error_recall Target error recall
run_protocol_validation <- function(results, conf_results,
                                     calib_pct = 0.10,
                                     K_calib = 10,
                                     K_verify = 5,
                                     target_error_recall = 0.95,
                                     n_bootstrap = 50) {
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("PROTOCOL VALIDATION\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat(sprintf("Calibration: %.0f%% of sample × %d runs\n", calib_pct * 100, K_calib))
  cat(sprintf("Verification: %d runs for flagged items\n", K_verify))
  cat(sprintf("Target: Catch %.0f%% of errors\n", target_error_recall * 100))
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Prepare data
  cat("Preparing data...\n")
  data <- prepare_protocol_data(results, conf_results)
  
  cat(sprintf("  Total instances: %d\n", nrow(data)))
  cat(sprintf("  Run-1 errors: %d (%.2f%%)\n", 
              sum(data$is_error), mean(data$is_error) * 100))
  
  # Find oracle τ (task-specific)
  cat("\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("STEP 1: Finding Oracle τ (from all 50 runs)\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  oracle <- find_oracle_tau(data, target_error_recall, by_task = TRUE)
  print(oracle %>% mutate(across(where(is.numeric), ~round(., 3))))
  
  # Run calibration (task-specific)
  cat("\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat(sprintf("STEP 2: Calibration (%.0f%% sample × %d runs, %d bootstraps)\n",
              calib_pct * 100, K_calib, n_bootstrap))
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  calib <- calibration_protocol(data, results, calib_pct, K_calib, 
                                 target_error_recall, by_task = TRUE, n_bootstrap)
  print(calib$summary %>% mutate(across(where(is.numeric), ~round(., 3))))
  
  # Compare to oracle
  comparison <- compare_to_oracle(oracle, calib, data)
  
  # Cost analysis using mean calibrated τ (weighted by task size)
  weighted_tau <- sum(oracle$n * calib$summary$tau_mean) / sum(oracle$n)
  cat(sprintf("\nUsing weighted average τ = %.3f for cost analysis\n", weighted_tau))
  
  cost <- cost_analysis(data, weighted_tau, K_verify, K_calib, calib_pct)
  
  # Summary for paper
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("SUMMARY FOR PAPER\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  cat("THE PROTOCOL:\n")
  cat("  1. Run classification ONCE on all N items with logprobs\n")
  cat(sprintf("  2. CALIBRATE: Select %.0f%% of items (stratified by confidence)\n", calib_pct * 100))
  cat(sprintf("     Run %d times on calibration set, learn τ to catch %.0f%% errors\n", K_calib, target_error_recall * 100))
  cat(sprintf("  3. APPLY: If confidence ≥ τ → accept; else → verify with %d runs\n", K_verify))
  cat("  4. REPORT: flagged %, error recall, cost\n")
  
  cat("\nEMPIRICAL RESULTS:\n")
  cat(sprintf("  - Calibration cost: %.0f%% × N × %d = %.1f%% of verify-all\n",
              calib_pct * 100, K_calib, calib_pct * K_calib / K_verify * 100))
  cat(sprintf("  - Learned τ: %.2f ± %.2f (oracle: %.2f)\n",
              mean(calib$summary$tau_mean), mean(calib$summary$tau_sd),
              mean(oracle$oracle_tau)))
  cat(sprintf("  - Error recall: %.1f%% (target: %.0f%%)\n",
              mean(calib$summary$recall_mean), target_error_recall * 100))
  cat(sprintf("  - Items flagged: %.1f%%\n", mean(calib$summary$flagged_mean)))
  cat(sprintf("  - Protocol cost: %.2f× baseline\n", cost$details$cost_protocol / cost$details$n_total))
  cat(sprintf("  - Savings vs verify-all: %.0f%%\n", 
              (1 - cost$details$cost_protocol / cost$details$cost_verify_all) * 100))
  
  invisible(list(
    data = data,
    oracle = oracle,
    calibration = calib,
    comparison = comparison,
    cost = cost
  ))
}

# =============================================================================
# SWEEP: TEST DIFFERENT CALIBRATION PERCENTAGES
# =============================================================================

#' Test how calibration quality varies with sample percentage
sweep_calibration_pct <- function(results, conf_results,
                                   pcts = c(0.05, 0.10, 0.15, 0.20, 0.30),
                                   K_calib = 10,
                                   target_error_recall = 0.95,
                                   n_bootstrap = 30) {
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("CALIBRATION PERCENTAGE SWEEP\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  data <- prepare_protocol_data(results, conf_results)
  oracle <- find_oracle_tau(data, target_error_recall, by_task = TRUE)
  
  sweep_results <- map_dfr(pcts, function(pct) {
    cat(sprintf("Testing %.0f%%...\n", pct * 100))
    
    calib <- calibration_protocol(data, results, pct, K_calib,
                                   target_error_recall, by_task = TRUE, n_bootstrap)
    
    # Compute summary statistics
    tibble(
      calib_pct = pct * 100,
      tau_mean = mean(calib$summary$tau_mean, na.rm = TRUE),
      tau_sd = mean(calib$summary$tau_sd, na.rm = TRUE),
      tau_vs_oracle = mean(calib$summary$tau_mean - oracle$oracle_tau, na.rm = TRUE),
      recall_mean = mean(calib$summary$recall_mean, na.rm = TRUE),
      recall_sd = mean(calib$summary$recall_sd, na.rm = TRUE),
      flagged_mean = mean(calib$summary$flagged_mean, na.rm = TRUE),
      calib_cost = pct * nrow(data) * K_calib
    )
  })
  
  cat("\n")
  cat("Results:\n\n")
  print(sweep_results %>% mutate(across(where(is.numeric), ~round(., 3))))
  
  cat("\n")
  cat("INTERPRETATION:\n")
  cat(sprintf("  - Oracle τ (mean across tasks): %.3f\n", mean(oracle$oracle_tau)))
  
  # Find minimum pct that achieves τ within 0.05 of oracle
  good_pct <- sweep_results %>%
    filter(abs(tau_vs_oracle) < 0.05) %>%
    slice(1)
  
  if (nrow(good_pct) > 0) {
    cat(sprintf("  - %.0f%% calibration achieves τ within 0.05 of oracle\n", good_pct$calib_pct))
  }
  
  invisible(sweep_results)
}

# =============================================================================
# USAGE
# =============================================================================

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("IMPROVED PROTOCOL VALIDATION LOADED\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("Run full validation:\n")
cat("  pv <- run_protocol_validation(results, conf_results,\n")
cat("                                 calib_pct = 0.10,\n")
cat("                                 K_calib = 10,\n")
cat("                                 K_verify = 5,\n")
cat("                                 target_error_recall = 0.95)\n\n")

cat("Test different calibration percentages:\n")
cat("  sweep <- sweep_calibration_pct(results, conf_results)\n\n")

cat("Quick cost analysis at specific τ:\n")
cat("  data <- prepare_protocol_data(results, conf_results)\n")
cat("  cost_analysis(data, tau = 0.75, K_verify = 5)\n")
