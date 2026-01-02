# =============================================================================
# CALIBRATED AUDITING PROTOCOL - VALIDATION SUITE
# =============================================================================
# 
# The protocol is NOT "use τ=0.80 forever"
# The protocol IS "here's how to learn τ cheaply for your model+task"
#
# Key questions this script answers:
#   1. How many calibration samples do you need? (n = 50? 100? 500?)
#   2. How many runs per calibration sample? (K = 5? 10? 20?)
#   3. Does τ transfer across tasks? (leave-one-out)
#   4. Does τ transfer across models? (4o → 4o-mini)
#   5. What's the cost-benefit of the full protocol?
#
# =============================================================================

library(tidyverse)

# =============================================================================
# TEST 1: POLICY SIMULATION
# =============================================================================
# 
# Simulate how the protocol would perform if we only had run 1
# Use runs 2-50 as "oracle" for verification
#
# =============================================================================

#' Simulate the full auditing policy
#' @param results Output from run_stability_analysis()
#' @param conf_results Output from run_confidence_analysis()
#' @param threshold τ - flag items with confidence below this
#' @param K_verify Number of additional runs for verification (default: 5)
policy_simulation <- function(results, conf_results, threshold, K_verify = 5) {
  
  # Get run 1 data
  run1 <- results$parsed %>%
    filter(run == 1) %>%
    select(id, dataset, task, player, class_run1 = classification)
  
  conf_run1 <- conf_results$confidence_raw %>%
    filter(run == 1) %>%
    select(id, dataset, task, player, top1_prob)
  
  # Get "verification" = modal of runs 2 to K_verify+1
  verification <- results$parsed %>%
    filter(run >= 2, run <= K_verify + 1) %>%
    group_by(id, dataset, task, player) %>%
    summarise(
      verification_modal = {
        tbl <- table(classification)
        names(tbl)[which.max(tbl)]
      },
      .groups = "drop"
    )
  
  # Get "oracle" = modal of all 50 runs (ground truth for evaluation)
  oracle <- results$stability %>%
    select(id, dataset, task, player, 
           oracle_class = modal_class,
           oracle_TAR = modal_freq)
  
  # Merge
  data <- run1 %>%
    inner_join(conf_run1, by = c("id", "dataset", "task", "player")) %>%
    inner_join(verification, by = c("id", "dataset", "task", "player")) %>%
    inner_join(oracle, by = c("id", "dataset", "task", "player")) %>%
    mutate(
      # Policy decision
      flagged = top1_prob < threshold,
      
      # Final classification under policy
      policy_class = ifelse(flagged, verification_modal, class_run1),
      
      # Evaluation metrics
      run1_correct = (class_run1 == oracle_class),
      policy_correct = (policy_class == oracle_class)
    )
  
  n_total <- nrow(data)
  n_flagged <- sum(data$flagged)
  
  # Calculate results
  list(
    threshold = threshold,
    K_verify = K_verify,
    n_total = n_total,
    n_flagged = n_flagged,
    pct_flagged = n_flagged / n_total * 100,
    
    # Accuracy (vs oracle = modal of 50)
    run1_accuracy = mean(data$run1_correct) * 100,
    policy_accuracy = mean(data$policy_correct) * 100,
    accuracy_gain = mean(data$policy_correct) * 100 - mean(data$run1_correct) * 100,
    
    # TAR among accepted (non-flagged) items
    accepted_TAR = mean(data$oracle_TAR[!data$flagged]) * 100,
    
    # Residual instability among accepted
    accepted_instability = mean(1 - data$oracle_TAR[!data$flagged]) * 100,
    
    # Cost
    avg_api_calls = (n_flagged * (1 + K_verify) + (n_total - n_flagged)) / n_total,
    cost_vs_always_verify = ((n_flagged * (1 + K_verify) + (n_total - n_flagged)) / n_total) / (1 + K_verify) * 100
  )
}

#' Run policy simulation across multiple thresholds
#' @param results Output from run_stability_analysis()
#' @param conf_results Output from run_confidence_analysis()
#' @param thresholds Vector of thresholds to test
#' @param K_verify Number of verification runs
policy_simulation_sweep <- function(results, conf_results, 
                                     thresholds = seq(0.5, 0.99, by = 0.05),
                                     K_verify = 5) {
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat(sprintf("TEST 1: POLICY SIMULATION (K_verify = %d)\n", K_verify))
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  results_df <- map_dfr(thresholds, function(t) {
    as_tibble(policy_simulation(results, conf_results, t, K_verify))
  })
  
  cat("Policy Performance by Threshold:\n\n")
  print(results_df %>% 
          select(threshold, pct_flagged, run1_accuracy, policy_accuracy, 
                 accuracy_gain, accepted_TAR, avg_api_calls) %>%
          mutate(across(where(is.numeric), ~round(., 2))))
  
  # Find key operating points
  cat("\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("KEY OPERATING POINTS:\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Target: 98% TAR among accepted
  tar_98 <- results_df %>% filter(accepted_TAR >= 98) %>% 
    filter(pct_flagged == min(pct_flagged)) %>% slice(1)
  if(nrow(tar_98) > 0) {
    cat(sprintf("For TAR ≥ 98%% among accepted:\n"))
    cat(sprintf("  τ = %.2f, flag %.1f%%, accuracy %.2f%% → %.2f%%, cost %.2f×\n",
                tar_98$threshold, tar_98$pct_flagged, tar_98$run1_accuracy,
                tar_98$policy_accuracy, tar_98$avg_api_calls))
  }
  
  # Target: 99% TAR among accepted
  tar_99 <- results_df %>% filter(accepted_TAR >= 99) %>% 
    filter(pct_flagged == min(pct_flagged)) %>% slice(1)
  if(nrow(tar_99) > 0) {
    cat(sprintf("\nFor TAR ≥ 99%% among accepted:\n"))
    cat(sprintf("  τ = %.2f, flag %.1f%%, accuracy %.2f%% → %.2f%%, cost %.2f×\n",
                tar_99$threshold, tar_99$pct_flagged, tar_99$run1_accuracy,
                tar_99$policy_accuracy, tar_99$avg_api_calls))
  }
  
  # Best efficiency
  results_df <- results_df %>%
    mutate(efficiency = accuracy_gain / pmax(avg_api_calls - 1, 0.01))
  best_eff <- results_df %>% filter(efficiency == max(efficiency)) %>% slice(1)
  cat(sprintf("\nBest efficiency (accuracy gain per extra call):\n"))
  cat(sprintf("  τ = %.2f, flag %.1f%%, gain +%.2f%%, cost %.2f×\n",
              best_eff$threshold, best_eff$pct_flagged, 
              best_eff$accuracy_gain, best_eff$avg_api_calls))
  
  invisible(results_df)
}

# =============================================================================
# TEST 2: MINIMAL CALIBRATION SIZE
# =============================================================================
#
# KEY QUESTION: How many samples and runs do you need to learn τ?
#
# =============================================================================

#' Test how stable τ is with different calibration sample sizes
#' @param results Output from run_stability_analysis()
#' @param conf_results Output from run_confidence_analysis()
#' @param sample_sizes Vector of calibration sample sizes to test
#' @param K_calib Number of runs for calibration (used to define instability)
#' @param target_TAR Target TAR for accepted items (default: 0.98)
#' @param n_bootstrap Number of bootstrap iterations
calibration_sample_size_test <- function(results, conf_results,
                                          sample_sizes = c(50, 100, 200, 500, 1000),
                                          K_calib = 10,
                                          target_TAR = 0.98,
                                          n_bootstrap = 50) {
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat(sprintf("TEST 2a: CALIBRATION SAMPLE SIZE (K=%d runs, target TAR=%.0f%%)\n", 
              K_calib, target_TAR * 100))
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Prepare data
  run1 <- results$parsed %>%
    filter(run == 1) %>%
    select(id, dataset, task, player, class_run1 = classification)
  
  conf_run1 <- conf_results$confidence_raw %>%
    filter(run == 1) %>%
    select(id, dataset, task, player, top1_prob)
  
  # "Ground truth" stability from K_calib runs (simulating calibration)
  calib_stability <- results$parsed %>%
    filter(run >= 2, run <= K_calib + 1) %>%
    group_by(id, dataset, task, player) %>%
    summarise(
      calib_TAR = {
        tbl <- table(classification)
        max(tbl) / sum(tbl)
      },
      .groups = "drop"
    )
  
  # Oracle stability (for evaluation)
  oracle <- results$stability %>%
    select(id, dataset, task, player, oracle_TAR = modal_freq)
  
  # Full dataset
  full_data <- run1 %>%
    inner_join(conf_run1, by = c("id", "dataset", "task", "player")) %>%
    inner_join(calib_stability, by = c("id", "dataset", "task", "player")) %>%
    inner_join(oracle, by = c("id", "dataset", "task", "player"))
  
  n_total <- nrow(full_data)
  
  # Function to learn τ from a calibration sample
  learn_threshold <- function(calib_data, target_TAR) {
    # Try thresholds and find one that achieves target TAR
    thresholds <- sort(unique(calib_data$top1_prob))
    
    for (t in thresholds) {
      accepted <- calib_data %>% filter(top1_prob >= t)
      if (nrow(accepted) > 0) {
        achieved_TAR <- mean(accepted$calib_TAR)
        if (achieved_TAR >= target_TAR) {
          return(t)
        }
      }
    }
    return(max(thresholds))  # If can't achieve target, use max threshold
  }
  
  # Bootstrap for each sample size
  results_list <- map_dfr(sample_sizes, function(n_calib) {
    
    # Can't sample more than we have
    n_calib_actual <- min(n_calib, n_total)
    
    bootstrap_results <- map_dfr(1:n_bootstrap, function(b) {
      
      # Stratified sample (oversample low confidence)
      # Take 50% from bottom 20% confidence, 50% from rest
      low_conf <- full_data %>% filter(top1_prob < quantile(top1_prob, 0.2))
      high_conf <- full_data %>% filter(top1_prob >= quantile(top1_prob, 0.2))
      
      n_low <- min(floor(n_calib_actual * 0.5), nrow(low_conf))
      n_high <- n_calib_actual - n_low
      
      calib_sample <- bind_rows(
        low_conf %>% slice_sample(n = n_low, replace = TRUE),
        high_conf %>% slice_sample(n = n_high, replace = TRUE)
      )
      
      # Learn τ from calibration sample
      tau_learned <- learn_threshold(calib_sample, target_TAR)
      
      # Evaluate on FULL dataset
      achieved_TAR <- mean(full_data$oracle_TAR[full_data$top1_prob >= tau_learned])
      pct_flagged <- mean(full_data$top1_prob < tau_learned) * 100
      
      tibble(
        bootstrap = b,
        n_calib = n_calib_actual,
        tau_learned = tau_learned,
        achieved_TAR = achieved_TAR * 100,
        pct_flagged = pct_flagged
      )
    })
    
    bootstrap_results
  })
  
  # Summarize
  summary_df <- results_list %>%
    group_by(n_calib) %>%
    summarise(
      tau_mean = mean(tau_learned),
      tau_sd = sd(tau_learned),
      tau_min = min(tau_learned),
      tau_max = max(tau_learned),
      TAR_mean = mean(achieved_TAR),
      TAR_sd = sd(achieved_TAR),
      TAR_min = min(achieved_TAR),
      flagged_mean = mean(pct_flagged),
      flagged_sd = sd(pct_flagged),
      .groups = "drop"
    )
  
  cat("Calibration Sample Size → Threshold Stability:\n\n")
  print(summary_df %>% mutate(across(where(is.numeric), ~round(., 3))))
  
  cat("\n")
  cat("INTERPRETATION:\n")
  
  # Find minimum sample size for stable τ
  stable_size <- summary_df %>%
    filter(tau_sd < 0.05) %>%  # τ varies by < 0.05
    slice(1)
  
  if (nrow(stable_size) > 0) {
    cat(sprintf("  - τ becomes stable (SD < 0.05) at n ≈ %d samples\n", 
                stable_size$n_calib))
  }
  
  cat(sprintf("  - With n=100: τ = %.2f ± %.2f, achieved TAR = %.1f%% ± %.1f%%\n",
              summary_df$tau_mean[summary_df$n_calib == min(summary_df$n_calib[summary_df$n_calib >= 100])],
              summary_df$tau_sd[summary_df$n_calib == min(summary_df$n_calib[summary_df$n_calib >= 100])],
              summary_df$TAR_mean[summary_df$n_calib == min(summary_df$n_calib[summary_df$n_calib >= 100])],
              summary_df$TAR_sd[summary_df$n_calib == min(summary_df$n_calib[summary_df$n_calib >= 100])]))
  
  invisible(list(results = results_list, summary = summary_df))
}

#' Test how stable τ is with different K (runs per calibration item)
#' @param results Output from run_stability_analysis()
#' @param conf_results Output from run_confidence_analysis()
#' @param K_values Vector of K values to test
#' @param n_calib Fixed calibration sample size
#' @param target_TAR Target TAR
calibration_K_test <- function(results, conf_results,
                                K_values = c(3, 5, 10, 20, 30),
                                n_calib = 200,
                                target_TAR = 0.98,
                                n_bootstrap = 50) {
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat(sprintf("TEST 2b: CALIBRATION K (n=%d samples, target TAR=%.0f%%)\n", 
              n_calib, target_TAR * 100))
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Prepare data
  run1 <- results$parsed %>%
    filter(run == 1) %>%
    select(id, dataset, task, player, class_run1 = classification)
  
  conf_run1 <- conf_results$confidence_raw %>%
    filter(run == 1) %>%
    select(id, dataset, task, player, top1_prob)
  
  # Oracle (full 50 runs)
  oracle <- results$stability %>%
    select(id, dataset, task, player, oracle_TAR = modal_freq)
  
  full_data <- run1 %>%
    inner_join(conf_run1, by = c("id", "dataset", "task", "player")) %>%
    inner_join(oracle, by = c("id", "dataset", "task", "player"))
  
  n_total <- nrow(full_data)
  n_calib_actual <- min(n_calib, n_total)
  
  # Function to learn τ
  learn_threshold <- function(calib_data, target_TAR) {
    thresholds <- sort(unique(calib_data$top1_prob))
    for (t in thresholds) {
      accepted <- calib_data %>% filter(top1_prob >= t)
      if (nrow(accepted) > 0 && mean(accepted$calib_TAR) >= target_TAR) {
        return(t)
      }
    }
    return(max(thresholds))
  }
  
  # Test each K
  results_list <- map_dfr(K_values, function(K) {
    
    # Get stability with K runs
    calib_stability <- results$parsed %>%
      filter(run >= 2, run <= K + 1) %>%
      group_by(id, dataset, task, player) %>%
      summarise(
        calib_TAR = {
          tbl <- table(classification)
          max(tbl) / sum(tbl)
        },
        .groups = "drop"
      )
    
    full_with_K <- full_data %>%
      inner_join(calib_stability, by = c("id", "dataset", "task", "player"))
    
    # Bootstrap
    bootstrap_results <- map_dfr(1:n_bootstrap, function(b) {
      
      # Stratified sample
      low_conf <- full_with_K %>% filter(top1_prob < quantile(top1_prob, 0.2))
      high_conf <- full_with_K %>% filter(top1_prob >= quantile(top1_prob, 0.2))
      
      n_low <- min(floor(n_calib_actual * 0.5), nrow(low_conf))
      n_high <- n_calib_actual - n_low
      
      calib_sample <- bind_rows(
        low_conf %>% slice_sample(n = n_low, replace = TRUE),
        high_conf %>% slice_sample(n = n_high, replace = TRUE)
      )
      
      tau_learned <- learn_threshold(calib_sample, target_TAR)
      
      # Evaluate on full dataset using ORACLE TAR
      achieved_TAR <- mean(full_with_K$oracle_TAR[full_with_K$top1_prob >= tau_learned])
      pct_flagged <- mean(full_with_K$top1_prob < tau_learned) * 100
      
      tibble(
        bootstrap = b,
        K = K,
        tau_learned = tau_learned,
        achieved_TAR = achieved_TAR * 100,
        pct_flagged = pct_flagged
      )
    })
    
    bootstrap_results
  })
  
  # Summarize
  summary_df <- results_list %>%
    group_by(K) %>%
    summarise(
      tau_mean = mean(tau_learned),
      tau_sd = sd(tau_learned),
      TAR_mean = mean(achieved_TAR),
      TAR_sd = sd(achieved_TAR),
      flagged_mean = mean(pct_flagged),
      .groups = "drop"
    ) %>%
    mutate(
      calib_cost = K * n_calib  # Total calibration API calls
    )
  
  cat("Calibration K (runs per item) → Threshold Stability:\n\n")
  print(summary_df %>% mutate(across(where(is.numeric), ~round(., 3))))
  
  cat("\n")
  cat("INTERPRETATION:\n")
  cat(sprintf("  - K=5 achieves τ = %.2f ± %.2f (cost: %d calls)\n",
              summary_df$tau_mean[summary_df$K == 5],
              summary_df$tau_sd[summary_df$K == 5],
              summary_df$calib_cost[summary_df$K == 5]))
  cat(sprintf("  - K=10 achieves τ = %.2f ± %.2f (cost: %d calls)\n",
              summary_df$tau_mean[summary_df$K == 10],
              summary_df$tau_sd[summary_df$K == 10],
              summary_df$calib_cost[summary_df$K == 10]))
  
  invisible(list(results = results_list, summary = summary_df))
}

# =============================================================================
# TEST 3: LEAVE-ONE-DATASET-OUT (Cross-Task Generalization)
# =============================================================================

#' Test if τ learned from some tasks transfers to held-out task
#' @param results Output from run_stability_analysis()
#' @param conf_results Output from run_confidence_analysis()
#' @param K_calib Number of runs for calibration
#' @param target_TAR Target TAR
leave_one_dataset_out <- function(results, conf_results, 
                                   K_calib = 10, 
                                   target_TAR = 0.98) {
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("TEST 3: LEAVE-ONE-DATASET-OUT (Cross-Task Generalization)\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Prepare data
  run1 <- results$parsed %>%
    filter(run == 1) %>%
    select(id, dataset, task, player, class_run1 = classification)
  
  conf_run1 <- conf_results$confidence_raw %>%
    filter(run == 1) %>%
    select(id, dataset, task, player, top1_prob)
  
  calib_stability <- results$parsed %>%
    filter(run >= 2, run <= K_calib + 1) %>%
    group_by(id, dataset, task, player) %>%
    summarise(
      calib_TAR = {
        tbl <- table(classification)
        max(tbl) / sum(tbl)
      },
      .groups = "drop"
    )
  
  oracle <- results$stability %>%
    select(id, dataset, task, player, oracle_TAR = modal_freq)
  
  full_data <- run1 %>%
    inner_join(conf_run1, by = c("id", "dataset", "task", "player")) %>%
    inner_join(calib_stability, by = c("id", "dataset", "task", "player")) %>%
    inner_join(oracle, by = c("id", "dataset", "task", "player"))
  
  # Get unique dataset-task combinations
  tasks <- full_data %>% distinct(dataset, task)
  
  # Leave-one-out
  loo_results <- map_dfr(1:nrow(tasks), function(i) {
    held_out_dataset <- tasks$dataset[i]
    held_out_task <- tasks$task[i]
    
    # Train on other tasks
    train_data <- full_data %>%
      filter(!(dataset == held_out_dataset & task == held_out_task))
    
    # Test on held-out task
    test_data <- full_data %>%
      filter(dataset == held_out_dataset, task == held_out_task)
    
    # Learn τ from training data
    learn_threshold <- function(data, target) {
      thresholds <- sort(unique(data$top1_prob))
      for (t in thresholds) {
        accepted <- data %>% filter(top1_prob >= t)
        if (nrow(accepted) > 0 && mean(accepted$calib_TAR) >= target) {
          return(t)
        }
      }
      return(max(thresholds))
    }
    
    tau_train <- learn_threshold(train_data, target_TAR)
    
    # Also learn τ on test data (oracle, for comparison)
    tau_test <- learn_threshold(test_data, target_TAR)
    
    # Apply tau_train to test data
    achieved_TAR <- mean(test_data$oracle_TAR[test_data$top1_prob >= tau_train]) * 100
    pct_flagged <- mean(test_data$top1_prob < tau_train) * 100
    
    # What if we used the "right" τ for this task?
    oracle_TAR <- mean(test_data$oracle_TAR[test_data$top1_prob >= tau_test]) * 100
    oracle_flagged <- mean(test_data$top1_prob < tau_test) * 100
    
    tibble(
      held_out = paste(held_out_dataset, held_out_task, sep = "_"),
      n_test = nrow(test_data),
      tau_from_others = tau_train,
      tau_optimal = tau_test,
      tau_diff = tau_train - tau_test,
      achieved_TAR = achieved_TAR,
      oracle_TAR = oracle_TAR,
      TAR_gap = oracle_TAR - achieved_TAR,
      pct_flagged = pct_flagged,
      oracle_flagged = oracle_flagged
    )
  })
  
  cat("Leave-One-Out Results:\n\n")
  print(loo_results %>% mutate(across(where(is.numeric), ~round(., 2))))
  
  cat("\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("SUMMARY:\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  cat(sprintf("Mean τ difference (learned - optimal): %.3f (SD: %.3f)\n",
              mean(loo_results$tau_diff), sd(loo_results$tau_diff)))
  cat(sprintf("Mean TAR gap: %.2f%% (SD: %.2f%%)\n",
              mean(loo_results$TAR_gap), sd(loo_results$TAR_gap)))
  
  if (mean(abs(loo_results$TAR_gap)) < 2) {
    cat("\n✓ Threshold transfers well across tasks (TAR gap < 2%)\n")
  } else {
    cat("\n⚠ Threshold varies by task - task-specific calibration recommended\n")
  }
  
  invisible(loo_results)
}

# =============================================================================
# TEST 4: CROSS-MODEL TRANSFER
# =============================================================================

#' Test if τ learned on model A transfers to model B
#' Requires both models' results loaded
#' @param results_A Results from model A
#' @param conf_results_A Confidence results from model A
#' @param results_B Results from model B
#' @param conf_results_B Confidence results from model B
#' @param K_calib Number of calibration runs
#' @param target_TAR Target TAR
cross_model_transfer <- function(results_A, conf_results_A,
                                  results_B, conf_results_B,
                                  model_A_name = "Model A",
                                  model_B_name = "Model B",
                                  K_calib = 10,
                                  target_TAR = 0.98) {
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat(sprintf("TEST 4: CROSS-MODEL TRANSFER (%s ↔ %s)\n", model_A_name, model_B_name))
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Helper to prepare data
  prepare_data <- function(results, conf_results, K) {
    run1 <- results$parsed %>%
      filter(run == 1) %>%
      select(id, dataset, task, player, class_run1 = classification)
    
    conf_run1 <- conf_results$confidence_raw %>%
      filter(run == 1) %>%
      select(id, dataset, task, player, top1_prob)
    
    calib_stability <- results$parsed %>%
      filter(run >= 2, run <= K + 1) %>%
      group_by(id, dataset, task, player) %>%
      summarise(
        calib_TAR = {
          tbl <- table(classification)
          max(tbl) / sum(tbl)
        },
        .groups = "drop"
      )
    
    oracle <- results$stability %>%
      select(id, dataset, task, player, oracle_TAR = modal_freq)
    
    run1 %>%
      inner_join(conf_run1, by = c("id", "dataset", "task", "player")) %>%
      inner_join(calib_stability, by = c("id", "dataset", "task", "player")) %>%
      inner_join(oracle, by = c("id", "dataset", "task", "player"))
  }
  
  # Learn τ function
  learn_threshold <- function(data, target) {
    thresholds <- sort(unique(data$top1_prob))
    for (t in thresholds) {
      accepted <- data %>% filter(top1_prob >= t)
      if (nrow(accepted) > 0 && mean(accepted$calib_TAR) >= target) {
        return(t)
      }
    }
    return(max(thresholds))
  }
  
  # Prepare both models
  data_A <- prepare_data(results_A, conf_results_A, K_calib)
  data_B <- prepare_data(results_B, conf_results_B, K_calib)
  
  # Learn τ for each model
  tau_A <- learn_threshold(data_A, target_TAR)
  tau_B <- learn_threshold(data_B, target_TAR)
  
  cat(sprintf("τ learned on %s: %.3f\n", model_A_name, tau_A))
  cat(sprintf("τ learned on %s: %.3f\n", model_B_name, tau_B))
  cat(sprintf("Difference: %.3f\n\n", abs(tau_A - tau_B)))
  
  # Cross-application
  results_df <- tibble(
    Scenario = c(
      sprintf("%s τ → %s data", model_A_name, model_A_name),
      sprintf("%s τ → %s data", model_A_name, model_B_name),
      sprintf("%s τ → %s data", model_B_name, model_B_name),
      sprintf("%s τ → %s data", model_B_name, model_A_name)
    ),
    tau_used = c(tau_A, tau_A, tau_B, tau_B),
    achieved_TAR = c(
      mean(data_A$oracle_TAR[data_A$top1_prob >= tau_A]) * 100,
      mean(data_B$oracle_TAR[data_B$top1_prob >= tau_A]) * 100,
      mean(data_B$oracle_TAR[data_B$top1_prob >= tau_B]) * 100,
      mean(data_A$oracle_TAR[data_A$top1_prob >= tau_B]) * 100
    ),
    pct_flagged = c(
      mean(data_A$top1_prob < tau_A) * 100,
      mean(data_B$top1_prob < tau_A) * 100,
      mean(data_B$top1_prob < tau_B) * 100,
      mean(data_A$top1_prob < tau_B) * 100
    )
  )
  
  cat("Cross-Model Application:\n\n")
  print(results_df %>% mutate(across(where(is.numeric), ~round(., 2))))
  
  cat("\n")
  cat("INTERPRETATION:\n")
  
  # Check if transfer works
  same_model_TAR <- mean(c(results_df$achieved_TAR[1], results_df$achieved_TAR[3]))
  cross_model_TAR <- mean(c(results_df$achieved_TAR[2], results_df$achieved_TAR[4]))
  
  cat(sprintf("  - Same-model τ achieves: %.1f%% TAR\n", same_model_TAR))
  cat(sprintf("  - Cross-model τ achieves: %.1f%% TAR\n", cross_model_TAR))
  cat(sprintf("  - Performance gap: %.1f%%\n", same_model_TAR - cross_model_TAR))
  
  if (abs(same_model_TAR - cross_model_TAR) < 2) {
    cat("\n✓ Threshold transfers well across models\n")
  } else {
    cat("\n⚠ Threshold needs model-specific calibration\n")
    cat("   (but confidence RANKING likely transfers - check AUC)\n")
  }
  
  invisible(results_df)
}

# =============================================================================
# FULL PROTOCOL SUMMARY
# =============================================================================

#' Generate complete protocol summary for paper
#' @param results Output from run_stability_analysis()
#' @param conf_results Output from run_confidence_analysis()
#' @param target_TAR Target TAR for the protocol
protocol_summary <- function(results, conf_results, target_TAR = 0.98) {
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("CALIBRATED AUDITING PROTOCOL - SUMMARY\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  cat("THE PROTOCOL:\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("1. Run classification ONCE on all items with logprobs\n")
  cat("2. Extract top1_prob (normalized over valid classes)\n")
  cat("3. CALIBRATION: Select ~200 items (oversample low-confidence),\n")
  cat("   run K=10 times, learn τ to achieve target TAR\n")
  cat("4. APPLY: If confidence ≥ τ → accept; else → verify with K runs\n")
  cat("5. REPORT: Share flagged %, achieved TAR, cost\n\n")
  
  # Run key tests
  cat("EMPIRICAL VALIDATION:\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Policy simulation at learned τ
  # First, learn τ
  run1 <- results$parsed %>% filter(run == 1) %>%
    select(id, dataset, task, player, class_run1 = classification)
  conf_run1 <- conf_results$confidence_raw %>% filter(run == 1) %>%
    select(id, dataset, task, player, top1_prob)
  calib_stability <- results$parsed %>%
    filter(run >= 2, run <= 11) %>%
    group_by(id, dataset, task, player) %>%
    summarise(calib_TAR = max(table(classification)) / n(), .groups = "drop")
  oracle <- results$stability %>%
    select(id, dataset, task, player, oracle_TAR = modal_freq)
  
  full_data <- run1 %>%
    inner_join(conf_run1, by = c("id", "dataset", "task", "player")) %>%
    inner_join(calib_stability, by = c("id", "dataset", "task", "player")) %>%
    inner_join(oracle, by = c("id", "dataset", "task", "player"))
  
  # Learn τ
  thresholds <- sort(unique(full_data$top1_prob))
  tau_learned <- NA
  for (t in thresholds) {
    accepted <- full_data %>% filter(top1_prob >= t)
    if (nrow(accepted) > 0 && mean(accepted$calib_TAR) >= target_TAR) {
      tau_learned <- t
      break
    }
  }
  
  cat(sprintf("Learned τ (for TAR ≥ %.0f%%): %.3f\n\n", target_TAR * 100, tau_learned))
  
  # Evaluate
  policy_result <- policy_simulation(results, conf_results, tau_learned, K_verify = 5)
  
  cat("Protocol Performance:\n")
  cat(sprintf("  - Items flagged for verification: %.1f%%\n", policy_result$pct_flagged))
  cat(sprintf("  - Baseline accuracy (run 1 only): %.2f%%\n", policy_result$run1_accuracy))
  cat(sprintf("  - Protocol accuracy: %.2f%%\n", policy_result$policy_accuracy))
  cat(sprintf("  - Accuracy gain: +%.2f%%\n", policy_result$accuracy_gain))
  cat(sprintf("  - TAR among accepted items: %.1f%%\n", policy_result$accepted_TAR))
  cat(sprintf("  - Average API calls: %.2f× (vs 1× baseline, 6× always-verify)\n", 
              policy_result$avg_api_calls))
  cat(sprintf("  - Cost reduction vs always-verify: %.0f%%\n", 
              100 - policy_result$cost_vs_always_verify))
  
  cat("\n")
  cat("FOR PAPER:\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat('"The calibrated auditing protocol achieves TAR ≥ ', sprintf("%.0f%%", policy_result$accepted_TAR),
      ' among accepted\n', sep = "")
  cat('classifications while flagging only ', sprintf("%.1f%%", policy_result$pct_flagged),
      ' of items for verification,\n', sep = "")
  cat('reducing API costs by ', sprintf("%.0f%%", 100 - policy_result$cost_vs_always_verify),
      ' compared to verifying all items."\n', sep = "")
  
  invisible(list(tau = tau_learned, performance = policy_result))
}

# =============================================================================
# USAGE
# =============================================================================

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("PROTOCOL VALIDATION MODULE LOADED\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("Run these tests:\n\n")

cat("TEST 1 - Policy Simulation:\n")
cat("  policy_simulation_sweep(results, conf_results)\n\n")

cat("TEST 2 - Minimal Calibration Size:\n")
cat("  calibration_sample_size_test(results, conf_results)\n")
cat("  calibration_K_test(results, conf_results)\n\n")

cat("TEST 3 - Cross-Task Generalization:\n")
cat("  leave_one_dataset_out(results, conf_results)\n\n")

cat("TEST 4 - Cross-Model Transfer (after 4o-mini done):\n")
cat("  cross_model_transfer(results_4o, conf_4o, results_mini, conf_mini)\n\n")

cat("FULL SUMMARY:\n")
cat("  protocol_summary(results, conf_results)\n")
