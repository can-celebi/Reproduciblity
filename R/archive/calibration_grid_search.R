# =============================================================================
# CALIBRATION GRID SEARCH
# =============================================================================
#
# Goal: Find cheapest (X%, K, selection) that approximates oracle τ
#
# Search space:
#   X% ∈ {5, 6, 7, ..., 50}  (46 values)
#   K  ∈ {2, 3, 4, ..., 20}  (19 values)
#   Selection ∈ {random, low_confidence, stratified}
#
# Oracle: τ from 100% of data × 50 runs
#
# Output: τ error, error recall, cost for each combination
#
# =============================================================================

library(tidyverse)
library(parallel)

# =============================================================================
# SETUP
# =============================================================================

#' Prepare data for calibration experiment
prepare_calibration_data <- function(results, conf_results) {
  
  # Run 1 classifications and confidence
  run1 <- results$parsed %>% 
    filter(run == 1) %>% 
    select(id, dataset, task, player, 
           class_run1 = classification,
           top1_prob = logprobs) %>%
    select(-top1_prob)  # Remove, we'll get from conf_results
  
  conf_run1 <- conf_results$confidence_raw %>%
    filter(run == 1) %>%
    select(id, dataset, task, player, top1_prob)
  
  run1 <- results$parsed %>% 
    filter(run == 1) %>% 
    select(id, dataset, task, player, class_run1 = classification) %>%
    inner_join(conf_run1, by = c("id", "dataset", "task", "player"))
  
  # Oracle = modal of all 50 runs
  oracle <- results$stability %>%
    select(id, dataset, task, player, 
           oracle_class = modal_class,
           oracle_TAR = modal_freq)
  
  # Merge
  data <- run1 %>%
    inner_join(oracle, by = c("id", "dataset", "task", "player")) %>%
    mutate(is_error = (class_run1 != oracle_class))
  
  data
}

#' Compute oracle τ (from full data)
#' @param data Prepared data
#' @param target_recall Target error recall (e.g., 0.95)
compute_oracle_tau <- function(data, target_recall = 0.95) {
  
  errors <- data %>% filter(is_error)
  
  if (nrow(errors) == 0) {
    return(list(tau = 0, recall = 1, flagged_pct = 0))
  }
  
  # Find τ that catches target_recall of errors
  thresholds <- sort(unique(data$top1_prob))
  
  for (tau in thresholds) {
    recall <- mean(errors$top1_prob < tau)
    if (recall >= target_recall) {
      flagged_pct <- mean(data$top1_prob < tau)
      return(list(tau = tau, recall = recall, flagged_pct = flagged_pct))
    }
  }
  
  # If can't achieve target, return max
  tau <- max(thresholds)
  return(list(
    tau = tau, 
    recall = mean(errors$top1_prob < tau),
    flagged_pct = mean(data$top1_prob < tau)
  ))
}

# =============================================================================
# SELECTION METHODS
# =============================================================================

#' Select calibration sample
#' @param data Full dataset
#' @param pct Percentage to select (0-1)
#' @param method Selection method: "random", "low_confidence", "stratified"
select_calibration_sample <- function(data, pct, method = "stratified") {
  
  n_select <- max(5, ceiling(nrow(data) * pct))  # At least 5
  
  if (method == "random") {
    # Pure random sample
    sample_ids <- data %>% 
      slice_sample(n = min(n_select, nrow(data))) %>%
      pull(id)
    
  } else if (method == "low_confidence") {
    # Bottom X% by confidence
    sample_ids <- data %>%
      arrange(top1_prob) %>%
      head(n_select) %>%
      pull(id)
    
  } else if (method == "stratified") {
    # 50% from bottom 30% confidence, 50% from rest
    low_conf <- data %>% filter(top1_prob < quantile(top1_prob, 0.3))
    high_conf <- data %>% filter(top1_prob >= quantile(top1_prob, 0.3))
    
    n_low <- min(ceiling(n_select * 0.5), nrow(low_conf))
    n_high <- n_select - n_low
    
    if (n_low > 0 && nrow(low_conf) > 0) {
      low_sample <- low_conf %>% slice_sample(n = n_low)
    } else {
      low_sample <- tibble()
    }
    
    if (n_high > 0 && nrow(high_conf) > 0) {
      high_sample <- high_conf %>% slice_sample(n = min(n_high, nrow(high_conf)))
    } else {
      high_sample <- tibble()
    }
    
    sample_ids <- bind_rows(low_sample, high_sample) %>% pull(id)
  }
  
  sample_ids
}

# =============================================================================
# CALIBRATION SIMULATION
# =============================================================================

#' Simulate calibration with given parameters
#' @param data Prepared data (full dataset with run-1 info)
#' @param results Full results object (for multi-run data)
#' @param pct Percentage of data to use for calibration
#' @param K Number of runs for calibration
#' @param method Selection method
#' @param target_recall Target recall of UNSTABLE items (not errors)
simulate_calibration <- function(data, results, pct, K, method, target_recall = 0.95) {
  
  # Select calibration sample
  calib_ids <- select_calibration_sample(data, pct, method)
  n_calib <- length(calib_ids)
  
  if (n_calib == 0) {
    return(list(tau = NA, recall = NA, n_calib = 0))
  }
  
  # Get K-run data for calibration items
  # Use runs 1 to K (include run 1 in the stability assessment)
  calib_runs <- results$parsed %>%
    filter(id %in% calib_ids, run <= K) %>%
    group_by(id, dataset, task, player) %>%
    summarise(
      n_unique = n_distinct(classification),
      n_runs_actual = n(),
      calib_TAR = {
        tbl <- table(classification)
        max(tbl) / sum(tbl)
      },
      .groups = "drop"
    ) %>%
    # UNSTABLE = any disagreement among K runs
    mutate(is_calib_unstable = (n_unique > 1))
  
  # Merge with run-1 confidence
  calib_data <- data %>%
    filter(id %in% calib_ids) %>%
    left_join(calib_runs %>% select(id, is_calib_unstable, calib_TAR), by = "id")
  
  # Handle missing (if K runs don't exist for some items)
  calib_data <- calib_data %>% filter(!is.na(is_calib_unstable))
  
  if (nrow(calib_data) == 0) {
    return(list(tau = NA, recall = NA, n_calib = 0, n_calib_unstable = 0))
  }
  
  # Learn τ from calibration sample
  # Goal: find τ such that we catch target_recall of UNSTABLE items
  calib_unstable <- calib_data %>% filter(is_calib_unstable)
  
  if (nrow(calib_unstable) == 0) {
    # No unstable items found - use a conservative threshold
    # Return the 10th percentile of confidence (flag bottom 10%)
    tau_learned <- quantile(calib_data$top1_prob, 0.10)
  } else {
    # Find τ that catches target_recall of unstable items
    thresholds <- sort(unique(calib_data$top1_prob))
    tau_learned <- max(thresholds)  # Default
    
    for (tau in thresholds) {
      recall <- mean(calib_unstable$top1_prob < tau)
      if (recall >= target_recall) {
        tau_learned <- tau
        break
      }
    }
  }
  
  # Evaluate on FULL data using ORACLE errors
  full_errors <- data %>% filter(is_error)
  if (nrow(full_errors) > 0) {
    achieved_recall <- mean(full_errors$top1_prob < tau_learned)
  } else {
    achieved_recall <- 1
  }
  
  flagged_pct <- mean(data$top1_prob < tau_learned)
  
  list(
    tau = tau_learned,
    recall = achieved_recall,
    flagged_pct = flagged_pct,
    n_calib = nrow(calib_data),
    n_calib_unstable = nrow(calib_unstable)
  )
}

# =============================================================================
# GRID SEARCH
# =============================================================================

#' Run full grid search
#' @param results Output from run_stability_analysis()
#' @param conf_results Output from run_confidence_analysis()
#' @param pct_range Range of percentages (default 5-50)
#' @param K_range Range of K values (default 2-20)
#' @param methods Selection methods to test
#' @param target_recall Target error recall
#' @param n_bootstrap Bootstrap iterations per combination
run_grid_search <- function(results, conf_results,
                            pct_range = seq(0.05, 0.50, by = 0.01),
                            K_range = 2:20,
                            methods = c("random", "low_confidence", "stratified"),
                            target_recall = 0.95,
                            n_bootstrap = 20) {
  
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("CALIBRATION GRID SEARCH\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Prepare data
  cat("Preparing data...\n")
  data <- prepare_calibration_data(results, conf_results)
  n_total <- nrow(data)
  n_errors <- sum(data$is_error)
  
  cat(sprintf("  Total items: %d\n", n_total))
  cat(sprintf("  Errors: %d (%.2f%%)\n", n_errors, n_errors/n_total*100))
  
  # Compute oracle
  cat("\nComputing oracle τ...\n")
  oracle <- compute_oracle_tau(data, target_recall)
  cat(sprintf("  Oracle τ = %.4f (catches %.1f%% of errors, flags %.1f%%)\n",
              oracle$tau, oracle$recall * 100, oracle$flagged_pct * 100))
  
  # Grid search
  n_pct <- length(pct_range)
  n_K <- length(K_range)
  n_methods <- length(methods)
  total_combos <- n_pct * n_K * n_methods
  
  cat(sprintf("\nGrid search: %d pct × %d K × %d methods = %d combinations\n",
              n_pct, n_K, n_methods, total_combos))
  cat(sprintf("Bootstrap iterations: %d\n", n_bootstrap))
  cat(sprintf("Total simulations: %d\n\n", total_combos * n_bootstrap))
  
  cat("Running search (this may take a few minutes)...\n")
  
  # Progress tracking
  pb <- txtProgressBar(min = 0, max = total_combos, style = 3)
  combo_count <- 0
  
  # Run grid search
  grid_results <- map_dfr(methods, function(method) {
    map_dfr(pct_range, function(pct) {
      map_dfr(K_range, function(K) {
        
        # Bootstrap
        boot_results <- map_dfr(1:n_bootstrap, function(b) {
          result <- simulate_calibration(data, results, pct, K, method, target_recall)
          tibble(
            bootstrap = b,
            tau = result$tau,
            recall = result$recall,
            flagged_pct = result$flagged_pct,
            n_calib = result$n_calib,
            n_calib_unstable = result$n_calib_unstable
          )
        })
        
        # Update progress
        combo_count <<- combo_count + 1
        setTxtProgressBar(pb, combo_count)
        
        # Summarize
        tibble(
          method = method,
          pct = pct,
          K = K,
          cost = pct * K,  # Relative cost
          tau_mean = mean(boot_results$tau, na.rm = TRUE),
          tau_sd = sd(boot_results$tau, na.rm = TRUE),
          tau_error = mean(boot_results$tau, na.rm = TRUE) - oracle$tau,
          tau_abs_error = abs(mean(boot_results$tau, na.rm = TRUE) - oracle$tau),
          recall_mean = mean(boot_results$recall, na.rm = TRUE),
          recall_sd = sd(boot_results$recall, na.rm = TRUE),
          flagged_mean = mean(boot_results$flagged_pct, na.rm = TRUE),
          n_calib_mean = mean(boot_results$n_calib, na.rm = TRUE),
          n_calib_unstable_mean = mean(boot_results$n_calib_unstable, na.rm = TRUE)
        )
      })
    })
  })
  
  close(pb)
  
  cat("\nSearch complete!\n\n")
  
  # Store oracle info
  attr(grid_results, "oracle") <- oracle
  attr(grid_results, "n_total") <- n_total
  attr(grid_results, "n_errors") <- n_errors
  attr(grid_results, "target_recall") <- target_recall
  
  grid_results
}

# =============================================================================
# ANALYSIS FUNCTIONS
# =============================================================================

#' Find best combinations by different criteria
analyze_grid_results <- function(grid_results, max_tau_error = 0.05) {
  
  oracle <- attr(grid_results, "oracle")
  n_total <- attr(grid_results, "n_total")
  target_recall <- attr(grid_results, "target_recall")
  
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("GRID SEARCH RESULTS\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  cat(sprintf("Oracle τ: %.4f\n", oracle$tau))
  cat(sprintf("Target recall: %.0f%%\n", target_recall * 100))
  cat(sprintf("Acceptable τ error: ±%.2f\n\n", max_tau_error))
  
  # Filter to acceptable τ error
  good_results <- grid_results %>%
    filter(tau_abs_error <= max_tau_error)
  
  cat(sprintf("Combinations with |τ - oracle| ≤ %.2f: %d / %d (%.1f%%)\n\n",
              max_tau_error, nrow(good_results), nrow(grid_results),
              nrow(good_results)/nrow(grid_results)*100))
  
  if (nrow(good_results) == 0) {
    cat("No combinations meet the τ error criterion!\n")
    cat("Showing best by τ error instead:\n\n")
    good_results <- grid_results %>% 
      arrange(tau_abs_error) %>%
      head(20)
  }
  
  # Best by cost (among acceptable)
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("CHEAPEST COMBINATIONS (among acceptable τ error):\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  cheapest <- good_results %>%
    arrange(cost) %>%
    head(10)
  
  print(cheapest %>% 
          select(method, pct, K, cost, tau_mean, tau_error, recall_mean) %>%
          mutate(
            pct = sprintf("%.0f%%", pct * 100),
            cost = sprintf("%.1f%%", cost * 100),
            tau_mean = sprintf("%.3f", tau_mean),
            tau_error = sprintf("%+.3f", tau_error),
            recall_mean = sprintf("%.1f%%", recall_mean * 100)
          ))
  
  # Best by recall (among acceptable)
  cat("\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("HIGHEST RECALL (among acceptable τ error):\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  best_recall <- good_results %>%
    arrange(desc(recall_mean)) %>%
    head(10)
  
  print(best_recall %>% 
          select(method, pct, K, cost, tau_mean, tau_error, recall_mean) %>%
          mutate(
            pct = sprintf("%.0f%%", pct * 100),
            cost = sprintf("%.1f%%", cost * 100),
            tau_mean = sprintf("%.3f", tau_mean),
            tau_error = sprintf("%+.3f", tau_error),
            recall_mean = sprintf("%.1f%%", recall_mean * 100)
          ))
  
  # Best by method
  cat("\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("BEST BY SELECTION METHOD:\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  by_method <- good_results %>%
    group_by(method) %>%
    arrange(cost) %>%
    slice(1) %>%
    ungroup()
  
  print(by_method %>%
          select(method, pct, K, cost, tau_mean, tau_error, recall_mean) %>%
          mutate(
            pct = sprintf("%.0f%%", pct * 100),
            cost = sprintf("%.1f%%", cost * 100),
            tau_mean = sprintf("%.3f", tau_mean),
            tau_error = sprintf("%+.3f", tau_error),
            recall_mean = sprintf("%.1f%%", recall_mean * 100)
          ))
  
  # Summary recommendation
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("RECOMMENDATION\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  best <- cheapest[1, ]
  
  cat(sprintf("Cheapest acceptable combination:\n"))
  cat(sprintf("  Selection: %s\n", best$method))
  cat(sprintf("  Sample: %.0f%% of data\n", best$pct * 100))
  cat(sprintf("  Runs: %d\n", best$K))
  cat(sprintf("  Cost: %.1f%% of (100%% × 50 runs)\n", best$cost / 0.50 * 100))
  cat(sprintf("  Learned τ: %.3f (oracle: %.3f, error: %+.3f)\n", 
              best$tau_mean, oracle$tau, best$tau_error))
  cat(sprintf("  Error recall: %.1f%% (target: %.0f%%)\n", 
              best$recall_mean * 100, target_recall * 100))
  
  invisible(list(
    full = grid_results,
    acceptable = good_results,
    cheapest = cheapest,
    best = best,
    oracle = oracle
  ))
}

#' Plot cost vs τ error tradeoff
plot_cost_tau_tradeoff <- function(grid_results) {
  
  oracle <- attr(grid_results, "oracle")
  
  p <- ggplot(grid_results, aes(x = cost * 100, y = tau_abs_error, color = method)) +
    geom_point(alpha = 0.5, size = 1) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
    annotate("text", x = max(grid_results$cost * 100) * 0.9, y = 0.055, 
             label = "Acceptable error", color = "red", size = 3) +
    labs(
      title = "Calibration Cost vs Threshold Error",
      x = "Calibration Cost (% of data × K runs)",
      y = "|τ_calibrated - τ_oracle|",
      color = "Selection\nMethod"
    ) +
    theme_minimal() +
    scale_color_brewer(palette = "Set1")
  
  print(p)
  invisible(p)
}

#' Summarize by K (averaging over pct)
summarize_by_K <- function(grid_results) {
  
  grid_results %>%
    group_by(method, K) %>%
    summarise(
      tau_error_mean = mean(tau_abs_error, na.rm = TRUE),
      tau_error_sd = sd(tau_abs_error, na.rm = TRUE),
      recall_mean = mean(recall_mean, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(method, K)
}

#' Summarize by pct (averaging over K)
summarize_by_pct <- function(grid_results) {
  
  grid_results %>%
    group_by(method, pct) %>%
    summarise(
      tau_error_mean = mean(tau_abs_error, na.rm = TRUE),
      tau_error_sd = sd(tau_abs_error, na.rm = TRUE),
      recall_mean = mean(recall_mean, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(method, pct)
}

# =============================================================================
# QUICK VERSION (faster for testing)
# =============================================================================

#' Quick grid search with fewer combinations
quick_grid_search <- function(results, conf_results,
                              pct_range = seq(0.05, 0.30, by = 0.05),
                              K_range = c(3, 5, 10, 15, 20),
                              methods = c("stratified"),
                              target_recall = 0.95,
                              n_bootstrap = 10) {
  
  run_grid_search(results, conf_results, 
                  pct_range, K_range, methods, 
                  target_recall, n_bootstrap)
}

# =============================================================================
# USAGE
# =============================================================================

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("CALIBRATION GRID SEARCH LOADED\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("Quick search (for testing):\n")
cat("  grid <- quick_grid_search(results, conf_results)\n")
cat("  analysis <- analyze_grid_results(grid)\n\n")

cat("Full search:\n")
cat("  grid <- run_grid_search(results, conf_results,\n")
cat("                          pct_range = seq(0.05, 0.50, by = 0.01),\n")
cat("                          K_range = 2:20,\n")
cat("                          methods = c('random', 'low_confidence', 'stratified'),\n")
cat("                          n_bootstrap = 20)\n")
cat("  analysis <- analyze_grid_results(grid, max_tau_error = 0.05)\n\n")

cat("Visualize:\n")
cat("  plot_cost_tau_tradeoff(grid)\n")
cat("  summarize_by_K(grid)\n")
cat("  summarize_by_pct(grid)\n")
