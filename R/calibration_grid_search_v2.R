# =============================================================================
# CALIBRATION GRID SEARCH - FIXED VERSION
# =============================================================================
#
# Key fix: Use (id, dataset, task, player) as composite key, not just id
#
# =============================================================================

library(tidyverse)

# =============================================================================
# SETUP
# =============================================================================

prepare_calibration_data <- function(results, conf_results) {
  
  # Run 1 classifications and confidence
  run1 <- results$parsed %>% 
    filter(run == 1) %>% 
    select(id, dataset, task, player, class_run1 = classification)
  
  conf_run1 <- conf_results$confidence_raw %>%
    filter(run == 1) %>%
    select(id, dataset, task, player, top1_prob)
  
  run1 <- run1 %>%
    inner_join(conf_run1, by = c("id", "dataset", "task", "player"))
  
  # Oracle = modal of all 50 runs + stability info
  oracle <- results$stability %>%
    select(id, dataset, task, player, 
           oracle_class = modal_class,
           oracle_TAR = modal_freq,
           is_stable)
  
  # Merge
  data <- run1 %>%
    inner_join(oracle, by = c("id", "dataset", "task", "player")) %>%
    mutate(
      is_error = (class_run1 != oracle_class),
      is_unstable = !is_stable,
      # Create unique row key
      row_key = paste(id, dataset, task, player, sep = "_")
    )
  
  data
}

#' Compute oracle τ
compute_oracle_tau <- function(data, target_instability_rate = 0.05) {
  # Find lowest τ where instability rate ABOVE τ is <= target
  for (tau in seq(0.40, 0.99, by = 0.01)) {
    above <- data %>% filter(top1_prob >= tau)
    if (nrow(above) >= 10) {
      rate <- mean(above$is_unstable)
      if (rate <= target_instability_rate) {
        return(list(tau = tau, rate = rate, n_above = nrow(above)))
      }
    }
  }
  return(list(tau = 0.99, rate = mean(data$is_unstable[data$top1_prob >= 0.99]), n_above = sum(data$top1_prob >= 0.99)))
}

# =============================================================================
# SELECTION METHODS (now returns row indices, not ids)
# =============================================================================

select_calibration_sample <- function(data, pct, method = "stratified") {
  
  n_select <- max(5, ceiling(nrow(data) * pct))
  
  if (method == "random") {
    sample_rows <- data %>% slice_sample(n = min(n_select, nrow(data)))
    
  } else if (method == "low_confidence") {
    sample_rows <- data %>%
      arrange(top1_prob) %>%
      head(n_select)
    
  } else if (method == "stratified") {
    # IMPROVED STRATIFICATION v3
    # 1. Ensure minimum per decile (for stable estimates everywhere)
    # 2. Oversample low-confidence deciles (where instability lives)
    
    min_per_decile <- 10
    oversample_deciles <- 1:3  # Bottom 30%
    oversample_share <- 0.6    # 60% of remaining budget to low-confidence
    
    data_with_decile <- data %>% mutate(conf_decile = ntile(top1_prob, 10))
    n_target <- n_select
    
    # Base coverage: min per decile (or all if decile has fewer)
    base_list <- list()
    for (d in 1:10) {
      decile_data <- data_with_decile %>% filter(conf_decile == d)
      n_to_sample <- min(min_per_decile, nrow(decile_data))
      if (n_to_sample > 0) {
        base_list[[d]] <- decile_data %>% slice_sample(n = n_to_sample)
      }
    }
    base <- bind_rows(base_list)
    
    # If base already exceeds target, return it
    if (nrow(base) >= n_target) {
      sample_rows <- base %>% select(-conf_decile)
    } else {
      # Allocate remaining budget
      n_left <- n_target - nrow(base)
      n_over <- ceiling(n_left * oversample_share)
      n_rest <- n_left - n_over
      
      pool_over <- data_with_decile %>%
        filter(conf_decile %in% oversample_deciles) %>%
        anti_join(base %>% select(id, dataset, task, player), 
                  by = c("id", "dataset", "task", "player"))
      
      pool_rest <- data_with_decile %>%
        filter(!conf_decile %in% oversample_deciles) %>%
        anti_join(base %>% select(id, dataset, task, player), 
                  by = c("id", "dataset", "task", "player"))
      
      # Sample from pools
      n_sample_over <- min(n_over, nrow(pool_over))
      n_sample_rest <- min(n_rest, nrow(pool_rest))
      
      add_over <- if (n_sample_over > 0) {
        pool_over %>% slice_sample(n = n_sample_over)
      } else tibble()
      
      add_rest <- if (n_sample_rest > 0) {
        pool_rest %>% slice_sample(n = n_sample_rest)
      } else tibble()
      
      sample_rows <- bind_rows(base, add_over, add_rest) %>% select(-conf_decile)
    }
  }
  
  sample_rows
}

# =============================================================================
# CALIBRATION SIMULATION (FIXED)
# =============================================================================

simulate_calibration <- function(data, results, pct, K, method, 
                                  target_instability_rate = 0.05) {
  
  # Select calibration sample (returns actual rows with all keys)
  calib_sample <- select_calibration_sample(data, pct, method)
  n_calib <- nrow(calib_sample)
  
  if (n_calib == 0) {
    return(list(tau = NA, oracle_recall = NA, n_calib = 0))
  }
  
  # Get K-run stability for calibration items using FULL key
  calib_keys <- calib_sample %>% select(id, dataset, task, player)
  
  calib_runs <- results$parsed %>%
    inner_join(calib_keys, by = c("id", "dataset", "task", "player")) %>%
    filter(run <= K) %>%
    group_by(id, dataset, task, player) %>%
    summarise(
      n_runs = n(),
      n_unique = n_distinct(classification),
      # Binary "any flip" - actually works better because it over-detects
      # instability, pushing τ higher (conservative in the right direction)
      is_calib_unstable = (n_unique > 1),
      .groups = "drop"
    )
  
  # Merge with confidence
  calib_data <- calib_sample %>%
    left_join(calib_runs %>% select(id, dataset, task, player, is_calib_unstable),
              by = c("id", "dataset", "task", "player"))
  
  calib_data <- calib_data %>% filter(!is.na(is_calib_unstable))
  
  if (nrow(calib_data) == 0) {
    return(list(tau = NA, oracle_recall = NA, n_calib = 0))
  }
  
  # Learn τ: find lowest τ where instability rate above τ is <= target
  tau_learned <- 0.99  # Default
  
  for (tau in seq(0.40, 0.99, by = 0.01)) {
    above <- calib_data %>% filter(top1_prob >= tau)
    if (nrow(above) >= 5) {
      rate <- mean(above$is_calib_unstable)
      if (rate <= target_instability_rate) {
        tau_learned <- tau
        break
      }
    }
  }
  
  # Evaluate: What's the ORACLE error recall at this τ?
  oracle_errors <- data %>% filter(is_error)
  if (nrow(oracle_errors) > 0) {
    oracle_recall <- mean(oracle_errors$top1_prob < tau_learned)
  } else {
    oracle_recall <- 1
  }
  
  # Also: oracle instability rate above τ
  above_tau <- data %>% filter(top1_prob >= tau_learned)
  oracle_instab_rate <- mean(above_tau$is_unstable)
  
  flagged_pct <- mean(data$top1_prob < tau_learned)
  
  list(
    tau = tau_learned,
    oracle_recall = oracle_recall,
    oracle_instab_rate = oracle_instab_rate,
    flagged_pct = flagged_pct,
    n_calib = nrow(calib_data),
    n_calib_unstable = sum(calib_data$is_calib_unstable)
  )
}

# =============================================================================
# GRID SEARCH
# =============================================================================

run_grid_search <- function(results, conf_results,
                            pct_range = seq(0.05, 0.50, by = 0.01),
                            K_range = 2:20,
                            methods = c("random", "low_confidence", "stratified"),
                            target_instability_rate = 0.05,
                            n_bootstrap = 20) {
  
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("CALIBRATION GRID SEARCH (FIXED)\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Prepare data
  cat("Preparing data...\n")
  data <- prepare_calibration_data(results, conf_results)
  n_total <- nrow(data)
  n_errors <- sum(data$is_error)
  n_unstable <- sum(data$is_unstable)
  
  cat(sprintf("  Total items: %d\n", n_total))
  cat(sprintf("  Errors (run1 ≠ modal): %d (%.2f%%)\n", n_errors, n_errors/n_total*100))
  cat(sprintf("  Unstable (any flip in 50 runs): %d (%.2f%%)\n", n_unstable, n_unstable/n_total*100))
  
  # Compute oracle
  cat("\nComputing oracle τ...\n")
  oracle <- compute_oracle_tau(data, target_instability_rate)
  cat(sprintf("  Oracle τ = %.2f (instability rate above: %.2f%%, n=%d)\n",
              oracle$tau, oracle$rate * 100, oracle$n_above))
  
  # What's error recall at oracle τ?
  oracle_errors <- data %>% filter(is_error)
  oracle_recall <- mean(oracle_errors$top1_prob < oracle$tau)
  cat(sprintf("  Error recall at oracle τ: %.1f%%\n", oracle_recall * 100))
  
  # Grid search
  n_combos <- length(pct_range) * length(K_range) * length(methods)
  cat(sprintf("\nGrid: %d pct × %d K × %d methods = %d combinations\n",
              length(pct_range), length(K_range), length(methods), n_combos))
  cat(sprintf("Bootstraps: %d → Total: %d simulations\n\n", n_bootstrap, n_combos * n_bootstrap))
  
  pb <- txtProgressBar(min = 0, max = n_combos, style = 3)
  combo_count <- 0
  
  grid_results <- map_dfr(methods, function(method) {
    map_dfr(pct_range, function(pct) {
      map_dfr(K_range, function(K) {
        
        boot_results <- map_dfr(1:n_bootstrap, function(b) {
          res <- simulate_calibration(data, results, pct, K, method, target_instability_rate)
          tibble(
            bootstrap = b,
            tau = res$tau,
            oracle_recall = res$oracle_recall,
            oracle_instab_rate = res$oracle_instab_rate,
            flagged_pct = res$flagged_pct,
            n_calib = res$n_calib,
            n_calib_unstable = res$n_calib_unstable
          )
        })
        
        combo_count <<- combo_count + 1
        setTxtProgressBar(pb, combo_count)
        
        tibble(
          method = method,
          pct = pct,
          K = K,
          cost = pct * K,
          tau_mean = mean(boot_results$tau, na.rm = TRUE),
          tau_sd = sd(boot_results$tau, na.rm = TRUE),
          tau_error = mean(boot_results$tau, na.rm = TRUE) - oracle$tau,
          tau_abs_error = abs(mean(boot_results$tau, na.rm = TRUE) - oracle$tau),
          oracle_recall_mean = mean(boot_results$oracle_recall, na.rm = TRUE),
          oracle_instab_mean = mean(boot_results$oracle_instab_rate, na.rm = TRUE),
          flagged_mean = mean(boot_results$flagged_pct, na.rm = TRUE),
          n_calib_mean = mean(boot_results$n_calib),
          n_calib_unstable_mean = mean(boot_results$n_calib_unstable)
        )
      })
    })
  })
  
  close(pb)
  cat("\n\nDone!\n")
  
  attr(grid_results, "oracle") <- oracle
  attr(grid_results, "oracle_recall") <- oracle_recall
  attr(grid_results, "n_total") <- n_total
  attr(grid_results, "target_instability_rate") <- target_instability_rate
  
  grid_results
}

# =============================================================================
# ANALYSIS
# =============================================================================

analyze_grid_results <- function(grid_results, max_tau_error = 0.05) {
  
  oracle <- attr(grid_results, "oracle")
  oracle_recall <- attr(grid_results, "oracle_recall")
  target_rate <- attr(grid_results, "target_instability_rate")
  
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("GRID SEARCH RESULTS\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  cat(sprintf("Target: Instability rate ≤ %.0f%% above threshold\n", target_rate * 100))
  cat(sprintf("Oracle τ: %.2f (error recall: %.1f%%)\n\n", oracle$tau, oracle_recall * 100))
  
  # Filter acceptable
  good <- grid_results %>% filter(tau_abs_error <= max_tau_error)
  
  cat(sprintf("Combinations with |τ - oracle| ≤ %.2f: %d / %d (%.1f%%)\n\n",
              max_tau_error, nrow(good), nrow(grid_results), 
              nrow(good)/nrow(grid_results)*100))
  
  if (nrow(good) == 0) {
    cat("No combinations meet criterion. Showing top 20 by τ error:\n\n")
    good <- grid_results %>% arrange(tau_abs_error) %>% head(20)
  }
  
  # Cheapest
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("CHEAPEST ACCEPTABLE COMBINATIONS:\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  cheapest <- good %>% arrange(cost) %>% head(15)
  print(cheapest %>%
          select(method, pct, K, cost, tau_mean, tau_error, oracle_recall_mean) %>%
          mutate(
            pct = sprintf("%.0f%%", pct * 100),
            cost = sprintf("%.1f%%", cost * 100),
            tau_mean = sprintf("%.2f", tau_mean),
            tau_error = sprintf("%+.2f", tau_error),
            oracle_recall_mean = sprintf("%.1f%%", oracle_recall_mean * 100)
          ))
  
  # Best by method
  cat("\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("CHEAPEST BY SELECTION METHOD:\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  by_method <- good %>%
    group_by(method) %>%
    arrange(cost) %>%
    slice(1) %>%
    ungroup()
  
  print(by_method %>%
          select(method, pct, K, cost, tau_mean, tau_error, oracle_recall_mean) %>%
          mutate(
            pct = sprintf("%.0f%%", pct * 100),
            cost = sprintf("%.1f%%", cost * 100),
            tau_mean = sprintf("%.2f", tau_mean),
            tau_error = sprintf("%+.2f", tau_error),
            oracle_recall_mean = sprintf("%.1f%%", oracle_recall_mean * 100)
          ))
  
  # Recommendation
  best <- cheapest[1, ]
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("RECOMMENDATION\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  cat(sprintf("Selection method: %s\n", best$method))
  cat(sprintf("Sample size: %.0f%% of data\n", best$pct * 100))
  cat(sprintf("Runs per item: %d\n", best$K))
  cat(sprintf("Calibration cost: %.1f%% of full (100%% × 50)\n", best$cost / 0.50 * 100))
  cat(sprintf("Learned τ: %.2f (oracle: %.2f, error: %+.2f)\n",
              best$tau_mean, oracle$tau, best$tau_error))
  cat(sprintf("Error recall: %.1f%% (oracle: %.1f%%)\n",
              best$oracle_recall_mean * 100, oracle_recall * 100))
  
  invisible(list(
    grid = grid_results,
    good = good,
    cheapest = cheapest,
    best = best,
    oracle = oracle
  ))
}

# =============================================================================
# QUICK VERSION
# =============================================================================

quick_grid_search <- function(results, conf_results,
                              pct_range = seq(0.05, 0.30, by = 0.05),
                              K_range = c(3, 5, 10, 15, 20),
                              methods = c("random", "stratified"),
                              target_instability_rate = 0.05,
                              n_bootstrap = 10) {
  
  run_grid_search(results, conf_results, pct_range, K_range, methods,
                  target_instability_rate, n_bootstrap)
}

# =============================================================================
# USAGE
# =============================================================================

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("CALIBRATION GRID SEARCH (FIXED) LOADED\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("Quick search:\n")
cat("  grid <- quick_grid_search(results, conf_results)\n")
cat("  analyze_grid_results(grid)\n\n")

cat("Full search:\n")
cat("  grid <- run_grid_search(results, conf_results,\n")
cat("            pct_range = seq(0.05, 0.50, by = 0.01),\n")
cat("            K_range = 2:20,\n")
cat("            methods = c('random', 'low_confidence', 'stratified'),\n")
cat("            target_instability_rate = 0.05,\n")
cat("            n_bootstrap = 20)\n")
cat("  analyze_grid_results(grid, max_tau_error = 0.05)\n")
