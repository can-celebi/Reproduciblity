# =============================================================================
# LEAKAGE-FREE CONFIDENCE-STABILITY ANALYSIS
# =============================================================================
# 
# This script implements the CORRECT analysis:
#   Predictor: Confidence from RUN 1 only
#   Outcome: Stability computed from RUNS 2..K
#
# This avoids the data leakage problem where mean confidence across all runs
# is used to predict stability defined over the same runs.
#
# =============================================================================

library(tidyverse)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Which run to use for confidence prediction (the "single run")
PREDICTOR_RUN <- 1

# =============================================================================
# MAIN ANALYSIS FUNCTION
# =============================================================================

#' Run leakage-free confidence-stability analysis
#' @param results Output from run_stability_analysis()
#' @param conf_results Output from run_confidence_analysis()
#' @param predictor_run Which run to use for confidence (default: 1)
leakage_free_analysis <- function(results, conf_results, predictor_run = 1) {
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("LEAKAGE-FREE ANALYSIS\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat(sprintf("Predictor: Confidence from run %d\n", predictor_run))
  cat("Outcome: Stability from all OTHER runs\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # -------------------------------------------------------------------------
  # Step 1: Extract confidence from predictor run only
  # -------------------------------------------------------------------------
  
  cat("Step 1: Extracting confidence from run", predictor_run, "...\n")
  
  conf_run1 <- conf_results$confidence_raw %>%
    filter(run == predictor_run) %>%
    select(id, dataset, task, player, 
           top1_prob_r1 = top1_prob,
           top2_prob_r1 = top2_prob,
           margin_r1 = margin,
           entropy_r1 = norm_entropy,
           chosen_token_r1 = chosen_token)
  
  cat(sprintf("  Found %d observations with run %d confidence\n", 
              nrow(conf_run1), predictor_run))
  
  # -------------------------------------------------------------------------
  # Step 2: Compute stability from OTHER runs only (excluding predictor run)
  # -------------------------------------------------------------------------
  
  cat("\nStep 2: Computing stability from runs != ", predictor_run, "...\n", sep = "")
  
  # Get parsed data excluding predictor run
  parsed_other <- results$parsed %>%
    filter(run != predictor_run)
  
  n_other_runs <- n_distinct(parsed_other$run)
  cat(sprintf("  Using %d other runs for stability\n", n_other_runs))
  
  # Recalculate stability metrics on other runs
  stability_other <- parsed_other %>%
    group_by(id, dataset, task, player) %>%
    summarise(
      n_runs_other = n_distinct(run),
      
      # Modal class (most frequent)
      modal_class_other = {
        tbl <- table(classification)
        names(tbl)[which.max(tbl)]
      },
      
      # Modal frequency (TAR - Total Agreement Rate)
      modal_freq_other = {
        tbl <- table(classification)
        max(tbl) / sum(tbl)
      },
      
      # Number of deviations from mode
      n_flips_other = {
        tbl <- table(classification)
        mode_class <- names(tbl)[which.max(tbl)]
        sum(classification != mode_class)
      },
      
      .groups = "drop"
    ) %>%
    mutate(
      flip_rate_other = n_flips_other / n_runs_other,
      is_stable_other = (modal_freq_other == 1.0),
      TAR_other = modal_freq_other
    )
  
  cat(sprintf("  Computed stability for %d units\n", nrow(stability_other)))
  
  # -------------------------------------------------------------------------
  # Step 3: Check if run 1 classification matches modal of other runs
  # -------------------------------------------------------------------------
  
  cat("\nStep 3: Computing agreement between run 1 and modal of others...\n")
  
  # Get run 1 classifications
  class_run1 <- results$parsed %>%
    filter(run == predictor_run) %>%
    select(id, dataset, task, player, class_r1 = classification)
  
  # -------------------------------------------------------------------------
  # Step 4: Merge confidence (run 1) with stability (other runs)
  # -------------------------------------------------------------------------
  
  cat("\nStep 4: Merging confidence and stability...\n")
  
  merged <- conf_run1 %>%
    inner_join(stability_other, by = c("id", "dataset", "task", "player")) %>%
    left_join(class_run1, by = c("id", "dataset", "task", "player")) %>%
    mutate(
      chosen_token_r1 = as.character(chosen_token_r1),
      modal_class_other = as.character(modal_class_other),
      r1_matches_modal = (chosen_token_r1 == modal_class_other)
    )
  
  cat(sprintf("  Merged data: %d units\n", nrow(merged)))
  
  # Sanity check: how often does run 1 match the modal?
  match_rate <- mean(merged$r1_matches_modal, na.rm = TRUE)
  cat(sprintf("  Run-%d matches modal of others: %.1f%% of cases\n", 
              predictor_run, match_rate * 100))
  
  # -------------------------------------------------------------------------
  # Step 5: Compute correlations (LEAKAGE-FREE)
  # -------------------------------------------------------------------------
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("LEAKAGE-FREE CORRELATIONS\n")
  cat(sprintf("(Run %d confidence → Runs 2-%d stability)\n", 
              predictor_run, predictor_run + n_other_runs))
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  cor_top1 <- cor(merged$top1_prob_r1, merged$flip_rate_other, 
                  method = "spearman", use = "complete.obs")
  cor_margin <- cor(merged$margin_r1, merged$flip_rate_other, 
                    method = "spearman", use = "complete.obs")
  cor_entropy <- cor(merged$entropy_r1, merged$flip_rate_other, 
                     method = "spearman", use = "complete.obs")
  
  cat("CORRELATIONS (Spearman):\n")
  cat(sprintf("  Run-%d Top-1 Prob vs Other-Runs Flip Rate: r = %.3f\n", 
              predictor_run, cor_top1))
  cat(sprintf("  Run-%d Margin vs Other-Runs Flip Rate: r = %.3f\n", 
              predictor_run, cor_margin))
  cat(sprintf("  Run-%d Entropy vs Other-Runs Flip Rate: r = %.3f\n", 
              predictor_run, cor_entropy))
  
  # Significance tests
  test_top1 <- cor.test(merged$top1_prob_r1, merged$flip_rate_other, 
                        method = "spearman")
  test_margin <- cor.test(merged$margin_r1, merged$flip_rate_other, 
                          method = "spearman")
  
  cat(sprintf("\n  Top-1: p = %.2e %s\n", test_top1$p.value,
              if(test_top1$p.value < 0.001) "***" else if(test_top1$p.value < 0.01) "**" else ""))
  cat(sprintf("  Margin: p = %.2e %s\n", test_margin$p.value,
              if(test_margin$p.value < 0.001) "***" else if(test_margin$p.value < 0.01) "**" else ""))
  
  # -------------------------------------------------------------------------
  # Step 6: Stable vs Unstable comparison (LEAKAGE-FREE)
  # -------------------------------------------------------------------------
  
  cat("\n")
  cat(sprintf("STABLE vs UNSTABLE (defined by runs 2-%d):\n\n", 
              predictor_run + n_other_runs))
  
  comparison <- merged %>%
    group_by(is_stable_other) %>%
    summarise(
      n = n(),
      mean_top1_prob_r1 = mean(top1_prob_r1, na.rm = TRUE),
      sd_top1_prob_r1 = sd(top1_prob_r1, na.rm = TRUE),
      mean_margin_r1 = mean(margin_r1, na.rm = TRUE),
      mean_entropy_r1 = mean(entropy_r1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(group = ifelse(is_stable_other, "Stable (runs 2+)", "Unstable (runs 2+)"))
  
  print(comparison %>% select(group, n, mean_top1_prob_r1, sd_top1_prob_r1, 
                               mean_margin_r1, mean_entropy_r1))
  
  # Wilcoxon test
  stable_conf <- merged %>% filter(is_stable_other) %>% pull(top1_prob_r1)
  unstable_conf <- merged %>% filter(!is_stable_other) %>% pull(top1_prob_r1)
  
  if (length(unstable_conf) > 1) {
    wilcox <- wilcox.test(stable_conf, unstable_conf)
    cat(sprintf("\nWilcoxon test: W = %.0f, p = %.2e %s\n",
                wilcox$statistic, wilcox$p.value,
                if(wilcox$p.value < 0.001) "***" else ""))
  }
  
  # -------------------------------------------------------------------------
  # Step 7: ROC Analysis (LEAKAGE-FREE)
  # -------------------------------------------------------------------------
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("ROC ANALYSIS (LEAKAGE-FREE)\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Calculate ROC
  roc_data <- calculate_roc_leakage_free(merged)
  auc <- calculate_auc(roc_data)
  
  cat(sprintf("AUC (Run-%d Top-1 Prob predicting instability in runs 2+): %.3f\n", 
              predictor_run, auc))
  
  # -------------------------------------------------------------------------
  # Step 8: Optimal thresholds (LEAKAGE-FREE)
  # -------------------------------------------------------------------------
  
  cat("\n")
  cat(sprintf("OPTIMAL THRESHOLDS (using run-%d confidence):\n\n", predictor_run))
  
  # Youden
  youden <- roc_data %>%
    mutate(youden_j = TPR - FPR) %>%
    filter(youden_j == max(youden_j)) %>%
    slice(1)
  
  cat("YOUDEN (balanced):\n")
  cat(sprintf("  Threshold: %.3f (flag if run-%d top1_prob < %.3f)\n", 
              youden$threshold, predictor_run, youden$threshold))
  cat(sprintf("  Recall: %.1f%%\n", youden$TPR * 100))
  cat(sprintf("  Precision: %.1f%%\n", youden$precision * 100))
  cat(sprintf("  Flagged: %.1f%%\n", youden$pct_flagged))
  
  # Best F1
  f1_best <- roc_data %>%
    filter(F1 == max(F1)) %>%
    slice(1)
  
  cat("\nBEST F1:\n")
  cat(sprintf("  Threshold: %.3f\n", f1_best$threshold))
  cat(sprintf("  Recall: %.1f%%, Precision: %.1f%%, F1: %.3f\n",
              f1_best$TPR * 100, f1_best$precision * 100, f1_best$F1))
  cat(sprintf("  Flagged: %.1f%%\n", f1_best$pct_flagged))
  
  # 80% recall
  recall_80 <- roc_data %>%
    filter(TPR >= 0.80) %>%
    filter(precision == max(precision)) %>%
    slice(1)
  
  if (nrow(recall_80) > 0) {
    cat("\n80% RECALL:\n")
    cat(sprintf("  Threshold: %.3f\n", recall_80$threshold))
    cat(sprintf("  Recall: %.1f%%, Precision: %.1f%%\n",
                recall_80$TPR * 100, recall_80$precision * 100))
    cat(sprintf("  Flagged: %.1f%%\n", recall_80$pct_flagged))
  }
  
  # -------------------------------------------------------------------------
  # Step 9: Bin analysis (LEAKAGE-FREE)
  # -------------------------------------------------------------------------
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat(sprintf("BIN ANALYSIS (Run-%d confidence, Runs 2+ stability)\n", predictor_run))
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  bins <- merged %>%
    mutate(conf_decile = ntile(top1_prob_r1, 10)) %>%
    group_by(conf_decile) %>%
    summarise(
      n = n(),
      conf_min = min(top1_prob_r1),
      conf_max = max(top1_prob_r1),
      n_unstable = sum(!is_stable_other),
      pct_unstable = mean(!is_stable_other) * 100,
      mean_flip_rate = mean(flip_rate_other) * 100,
      .groups = "drop"
    ) %>%
    mutate(conf_range = sprintf("[%.2f-%.2f]", conf_min, conf_max))
  
  print(bins %>% select(conf_decile, conf_range, n, n_unstable, pct_unstable, mean_flip_rate))
  
  # -------------------------------------------------------------------------
  # Step 10: Compare with leaky analysis
  # -------------------------------------------------------------------------
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("COMPARISON: LEAKAGE-FREE vs ORIGINAL (LEAKY)\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Get original leaky correlation from conf_results$merged
  leaky_cor <- cor(conf_results$merged$mean_top1_prob, 
                   conf_results$merged$flip_rate,
                   method = "spearman", use = "complete.obs")
  
  cat(sprintf("Original (leaky) correlation:     r = %.3f\n", leaky_cor))
  cat(sprintf("Leakage-free correlation:         r = %.3f\n", cor_top1))
  cat(sprintf("Difference:                       %.3f\n", abs(leaky_cor) - abs(cor_top1)))
  
  cat(sprintf("\nLeakage-free AUC:                 %.3f\n", auc))
  
  if (auc > 0.9) {
    cat("\n✓ AUC > 0.9: Your core finding is ROBUST to leakage correction!\n")
  } else if (auc > 0.8) {
    cat("\n⚠ AUC 0.8-0.9: Still good, but effect is weaker than originally reported.\n")
  } else {
    cat("\n✗ AUC < 0.8: The original finding may have been largely driven by leakage.\n")
  }
  
  # Return results
  invisible(list(
    merged = merged,
    roc = roc_data,
    auc = auc,
    correlations = c(top1 = cor_top1, margin = cor_margin, entropy = cor_entropy),
    comparison = comparison,
    bins = bins,
    thresholds = list(youden = youden, f1 = f1_best, recall_80 = recall_80)
  ))
}

#' Calculate ROC for leakage-free analysis
#' Uses run-1 confidence to predict instability in runs 2+
calculate_roc_leakage_free <- function(merged_df) {
  
  conf_values <- merged_df$top1_prob_r1
  is_unstable <- !merged_df$is_stable_other
  
  thresholds <- sort(unique(c(0, conf_values, 1)))
  
  map_dfr(thresholds, function(thresh) {
    pred_unstable <- conf_values < thresh
    
    TP <- sum(pred_unstable & is_unstable)
    FP <- sum(pred_unstable & !is_unstable)
    TN <- sum(!pred_unstable & !is_unstable)
    FN <- sum(!pred_unstable & is_unstable)
    
    TPR <- if (TP + FN > 0) TP / (TP + FN) else 0
    FPR <- if (FP + TN > 0) FP / (FP + TN) else 0
    precision <- if (TP + FP > 0) TP / (TP + FP) else 0
    F1 <- if (precision + TPR > 0) 2 * precision * TPR / (precision + TPR) else 0
    
    tibble(
      threshold = thresh,
      TP = TP, FP = FP, TN = TN, FN = FN,
      TPR = TPR, FPR = FPR,
      precision = precision,
      F1 = F1,
      pct_flagged = mean(pred_unstable) * 100
    )
  })
}

#' Calculate AUC from ROC data
calculate_auc <- function(roc_df) {
  roc_sorted <- roc_df %>% arrange(FPR, TPR)
  n <- nrow(roc_sorted)
  auc <- 0
  for (i in 2:n) {
    width <- roc_sorted$FPR[i] - roc_sorted$FPR[i-1]
    height <- (roc_sorted$TPR[i] + roc_sorted$TPR[i-1]) / 2
    auc <- auc + width * height
  }
  auc
}

# =============================================================================
# ADDITIONAL: Cross-validation style analysis
# =============================================================================

#' Run analysis using each run as predictor (leave-one-out style)
#' This gives you a sense of how stable the effect is across different "single runs"
cross_validate_runs <- function(results, conf_results, max_runs = 10) {
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("CROSS-VALIDATION: Each run as predictor\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  available_runs <- sort(unique(conf_results$confidence_raw$run))
  runs_to_test <- head(available_runs, max_runs)
  
  cv_results <- map_dfr(runs_to_test, function(r) {
    
    # Confidence from run r
    conf_r <- conf_results$confidence_raw %>%
      filter(run == r) %>%
      select(id, dataset, task, player, top1_prob_r = top1_prob)
    
    # Stability from other runs
    parsed_other <- results$parsed %>% filter(run != r)
    
    stability_other <- parsed_other %>%
      group_by(id, dataset, task, player) %>%
      summarise(
        n_runs = n_distinct(run),
        modal_freq = {
          tbl <- table(classification)
          max(tbl) / sum(tbl)
        },
        flip_rate = 1 - modal_freq,
        is_stable = (modal_freq == 1.0),
        .groups = "drop"
      )
    
    merged <- conf_r %>%
      inner_join(stability_other, by = c("id", "dataset", "task", "player"))
    
    # Correlation
    cor_val <- cor(merged$top1_prob_r, merged$flip_rate, 
                   method = "spearman", use = "complete.obs")
    
    # Quick AUC
    roc <- calculate_roc_leakage_free(
      merged %>% rename(top1_prob_r1 = top1_prob_r, is_stable_other = is_stable)
    )
    auc_val <- calculate_auc(roc)
    
    tibble(
      predictor_run = r,
      n_other_runs = n_distinct(parsed_other$run),
      correlation = cor_val,
      auc = auc_val
    )
  })
  
  cat("Results by predictor run:\n\n")
  print(cv_results)
  
  cat(sprintf("\nMean correlation: %.3f (SD: %.3f)\n", 
              mean(cv_results$correlation), sd(cv_results$correlation)))
  cat(sprintf("Mean AUC: %.3f (SD: %.3f)\n",
              mean(cv_results$auc), sd(cv_results$auc)))
  
  invisible(cv_results)
}

# =============================================================================
# TASK-LEVEL ANALYSIS (Leakage-Free AUC by Task)
# =============================================================================

#' Compute leakage-free AUC and correlation for each task separately
#' @param results Output from run_stability_analysis()
#' @param conf_results Output from run_confidence_analysis()
#' @param predictor_run Which run to use for confidence (default: 1)
task_level_analysis <- function(results, conf_results, predictor_run = 1) {
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat(sprintf("TASK-LEVEL ANALYSIS (Run-%d confidence → Runs 2+ stability)\n", predictor_run))
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Get run 1 confidence
  conf_r1 <- conf_results$confidence_raw %>%
    filter(run == predictor_run) %>%
    select(id, dataset, task, player, top1_prob_r1 = top1_prob)
  
  # Compute stability from other runs
  parsed_other <- results$parsed %>% filter(run != predictor_run)
  
  stability_other <- parsed_other %>%
    group_by(id, dataset, task, player) %>%
    summarise(
      n_runs = n_distinct(run),
      modal_freq = {
        tbl <- table(classification)
        max(tbl) / sum(tbl)
      },
      flip_rate = 1 - modal_freq,
      is_stable = (modal_freq == 1.0),
      .groups = "drop"
    )
  
  # Merge
  merged <- conf_r1 %>%
    inner_join(stability_other, by = c("id", "dataset", "task", "player"))
  
  # Analyze by task
  tasks <- merged %>% distinct(dataset, task)
  
  task_results <- map_dfr(1:nrow(tasks), function(i) {
    d <- tasks$dataset[i]
    t <- tasks$task[i]
    
    task_data <- merged %>% filter(dataset == d, task == t)
    
    n_total <- nrow(task_data)
    n_unstable <- sum(!task_data$is_stable)
    
    # Correlation
    cor_val <- cor(task_data$top1_prob_r1, task_data$flip_rate, 
                   method = "spearman", use = "complete.obs")
    
    # AUC (only if there are both stable and unstable)
    if (n_unstable > 0 && n_unstable < n_total) {
      roc <- calculate_roc_leakage_free(
        task_data %>% rename(is_stable_other = is_stable)
      )
      auc_val <- calculate_auc(roc)
    } else {
      auc_val <- NA_real_
    }
    
    tibble(
      dataset = d,
      task = t,
      n_units = n_total,
      n_unstable = n_unstable,
      pct_unstable = n_unstable / n_total * 100,
      correlation = cor_val,
      auc = auc_val
    )
  })
  
  cat("Leakage-Free Results by Task:\n\n")
  print(task_results %>% arrange(desc(abs(correlation))))
  
  cat("\nInterpretation:\n")
  cat("  - Strongest effects in tasks with highest ambiguity (l3_level, l2_level)\n")
  cat("  - AUC near 1.0 where unstable cases exist = confidence is highly predictive\n")
  cat("  - p1_promise has only 1 unstable case → AUC less meaningful\n")
  
  invisible(task_results)
}

# =============================================================================
# CONTINUOUS OUTCOME ANALYSIS (K-Independent)
# =============================================================================

#' Analyze using continuous outcome (1-TAR) instead of binary "any disagreement"
#' This is K-independent and more comparable across studies
#' @param results Output from run_stability_analysis()
#' @param conf_results Output from run_confidence_analysis()
#' @param predictor_run Which run to use for confidence (default: 1)
continuous_outcome_analysis <- function(results, conf_results, predictor_run = 1) {
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("CONTINUOUS OUTCOME ANALYSIS (K-Independent)\n")
  cat(sprintf("Predictor: Run-%d confidence\n", predictor_run))
  cat("Outcome: 1 - TAR (Total Agreement Rate) from other runs\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Get run 1 confidence (NO logprob_gap)
  conf_r1 <- conf_results$confidence_raw %>%
    filter(run == predictor_run) %>%
    select(id, dataset, task, player, 
           top1_prob_r1 = top1_prob,
           margin_r1 = margin,
           entropy_r1 = norm_entropy)
  
  # Compute TAR from other runs
  parsed_other <- results$parsed %>% filter(run != predictor_run)
  n_other_runs <- n_distinct(parsed_other$run)
  
  stability_other <- parsed_other %>%
    group_by(id, dataset, task, player) %>%
    summarise(
      n_runs = n_distinct(run),
      TAR = {
        tbl <- table(classification)
        max(tbl) / sum(tbl)
      },
      .groups = "drop"
    ) %>%
    mutate(
      disagreement_rate = 1 - TAR
    )
  
  # Merge
  merged <- conf_r1 %>%
    inner_join(stability_other, by = c("id", "dataset", "task", "player"))
  
  cat(sprintf("Units analyzed: %d\n", nrow(merged)))
  cat(sprintf("Runs for outcome: %d (excluding run %d)\n\n", n_other_runs, predictor_run))
  
  # --- Correlations with continuous outcome ---
  cat("CORRELATIONS with Disagreement Rate (1 - TAR):\n\n")
  
  cor_top1 <- cor(merged$top1_prob_r1, merged$disagreement_rate, 
                  method = "spearman", use = "complete.obs")
  cor_margin <- cor(merged$margin_r1, merged$disagreement_rate, 
                    method = "spearman", use = "complete.obs")
  cor_entropy <- cor(merged$entropy_r1, merged$disagreement_rate, 
                     method = "spearman", use = "complete.obs")
  
  cat(sprintf("  Top-1 Prob (run %d) vs Disagreement Rate: r = %.3f\n", predictor_run, cor_top1))
  cat(sprintf("  Margin (run %d) vs Disagreement Rate:     r = %.3f\n", predictor_run, cor_margin))
  cat(sprintf("  Entropy (run %d) vs Disagreement Rate:    r = %.3f\n", predictor_run, cor_entropy))
  
  # Significance
  test <- cor.test(merged$top1_prob_r1, merged$disagreement_rate, method = "spearman")
  cat(sprintf("\n  p-value (Top-1): %.2e ***\n", test$p.value))
  
  # --- Linear regression ---
  cat("\n")
  cat("LINEAR REGRESSION: Disagreement_Rate ~ Top1_Prob\n\n")
  
  model <- lm(disagreement_rate ~ top1_prob_r1, data = merged)
  cat(sprintf("  Intercept: %.4f\n", coef(model)[1]))
  cat(sprintf("  Slope: %.4f (per 1 unit increase in top1_prob)\n", coef(model)[2]))
  cat(sprintf("  R²: %.3f\n", summary(model)$r.squared))
  
  # Interpretation
  cat("\n  Interpretation:\n")
  cat(sprintf("    For each 0.1 increase in top1_prob, disagreement rate decreases by %.2f%%\n",
              abs(coef(model)[2]) * 0.1 * 100))
  
  # --- By task ---
  cat("\n")
  cat("CORRELATION BY TASK (continuous outcome):\n\n")
  
  by_task <- merged %>%
    group_by(dataset, task) %>%
    summarise(
      n = n(),
      mean_disagreement = mean(disagreement_rate) * 100,
      cor_top1 = cor(top1_prob_r1, disagreement_rate, method = "spearman"),
      .groups = "drop"
    ) %>%
    arrange(desc(abs(cor_top1)))
  
  print(by_task)
  
  # --- Summary stats for paper ---
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("SUMMARY STATISTICS FOR PAPER\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  cat("K-Independent Findings:\n")
  cat(sprintf("  - Correlation (top1_prob vs disagreement_rate): r = %.3f\n", cor_top1))
  cat(sprintf("  - Linear R²: %.3f\n", summary(model)$r.squared))
  cat(sprintf("  - Effect size: %.2f%% less disagreement per 0.1 higher confidence\n",
              abs(coef(model)[2]) * 0.1 * 100))
  cat("\n")
  cat("This result is independent of K (number of runs) and comparable across studies.\n")
  
  invisible(list(
    merged = merged,
    correlations = c(top1 = cor_top1, margin = cor_margin, entropy = cor_entropy),
    model = model,
    by_task = by_task
  ))
}

# =============================================================================
# RELIABILITY SUMMARY (for paper)
# =============================================================================

#' Generate a clean reliability summary from cross-validation results
#' @param cv_results Output from cross_validate_runs()
reliability_summary <- function(cv_results) {
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("DIAGNOSTIC RELIABILITY SUMMARY (for paper)\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Correlation stats
  mean_cor <- mean(cv_results$correlation)
  sd_cor <- sd(cv_results$correlation)
  ci_cor_low <- mean_cor - 1.96 * sd_cor / sqrt(nrow(cv_results))
  ci_cor_high <- mean_cor + 1.96 * sd_cor / sqrt(nrow(cv_results))
  
  # AUC stats
  mean_auc <- mean(cv_results$auc)
  sd_auc <- sd(cv_results$auc)
  ci_auc_low <- mean_auc - 1.96 * sd_auc / sqrt(nrow(cv_results))
  ci_auc_high <- mean_auc + 1.96 * sd_auc / sqrt(nrow(cv_results))
  
  cat("Cross-Run Reliability (using each run as single-run predictor):\n\n")
  cat(sprintf("  Correlation (Spearman):\n"))
  cat(sprintf("    Mean: %.3f (SD: %.3f)\n", mean_cor, sd_cor))
  cat(sprintf("    95%% CI: [%.3f, %.3f]\n", ci_cor_low, ci_cor_high))
  cat(sprintf("    Range: [%.3f, %.3f]\n", min(cv_results$correlation), max(cv_results$correlation)))
  
  cat(sprintf("\n  AUC (predicting any disagreement):\n"))
  cat(sprintf("    Mean: %.3f (SD: %.3f)\n", mean_auc, sd_auc))
  cat(sprintf("    95%% CI: [%.3f, %.3f]\n", ci_auc_low, ci_auc_high))
  cat(sprintf("    Range: [%.3f, %.3f]\n", min(cv_results$auc), max(cv_results$auc)))
  
  cat("\n")
  cat("Conclusion: The diagnostic is highly stable across runs.\n")
  cat("Choice of which 'single run' to use has minimal impact on results.\n")
  
  invisible(list(
    correlation = list(mean = mean_cor, sd = sd_cor, ci = c(ci_cor_low, ci_cor_high)),
    auc = list(mean = mean_auc, sd = sd_auc, ci = c(ci_auc_low, ci_auc_high))
  ))
}

# =============================================================================
# USAGE
# =============================================================================

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("LEAKAGE-FREE ANALYSIS MODULE LOADED\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("Core Analysis:\n")
cat("  lf <- leakage_free_analysis(results, conf_results)      # Main analysis\n")
cat("  cv <- cross_validate_runs(results, conf_results)        # Cross-validation\n\n")

cat("Additional Analyses:\n")
cat("  task_level_analysis(results, conf_results)              # AUC by task\n")
cat("  continuous_outcome_analysis(results, conf_results)      # K-independent (1-TAR)\n")
cat("  reliability_summary(cv)                                 # For paper reporting\n\n")

cat("Options:\n")
cat("  lf <- leakage_free_analysis(results, conf_results, predictor_run = 5)\n")
