library(dplyr)
library(purrr)
library(tibble)

# Generic ROC builder for any score
# direction:
#   "low_flags"  -> predict unstable if score < threshold  (e.g., top1_prob, margin, logprob_gap)
#   "high_flags" -> predict unstable if score > threshold  (e.g., entropy)
roc_generic <- function(score, unstable, direction = c("low_flags", "high_flags")) {
  direction <- match.arg(direction)
  
  # remove NAs safely
  ok <- !is.na(score) & !is.na(unstable)
  score <- score[ok]
  unstable <- unstable[ok]
  
  thresholds <- sort(unique(c(min(score), score, max(score))))
  
  map_dfr(thresholds, function(t) {
    pred_unstable <- if (direction == "low_flags") score < t else score > t
    
    TP <- sum(pred_unstable & unstable)
    FP <- sum(pred_unstable & !unstable)
    TN <- sum(!pred_unstable & !unstable)
    FN <- sum(!pred_unstable & unstable)
    
    TPR <- if ((TP + FN) > 0) TP / (TP + FN) else 0
    FPR <- if ((FP + TN) > 0) FP / (FP + TN) else 0
    precision <- if ((TP + FP) > 0) TP / (TP + FP) else 0
    recall <- TPR
    F1 <- if ((precision + recall) > 0) 2 * precision * recall / (precision + recall) else 0
    
    tibble(threshold = t, TPR = TPR, FPR = FPR, precision = precision, recall = recall, F1 = F1)
  })
}

# trapezoid AUC on ROC curve
auc_from_roc <- function(roc_df) {
  roc_sorted <- roc_df %>% arrange(FPR, TPR)
  if (nrow(roc_sorted) < 2) return(NA_real_)
  sum(diff(roc_sorted$FPR) * (head(roc_sorted$TPR, -1) + tail(roc_sorted$TPR, -1)) / 2)
}

# leakage-free merged data (run r confidence -> stability of all other runs)
make_leakage_free_merged <- function(results, conf_results, predictor_run = 1) {
  
  # confidence from run r only
  conf_r <- conf_results$confidence_raw %>%
    filter(run == predictor_run) %>%
    transmute(
      id, dataset, task, player,
      top1_prob_r = top1_prob,
      margin_r = margin,
      entropy_r = norm_entropy
    )
  
  # stability from other runs only
  parsed_other <- results$parsed %>% filter(run != predictor_run)
  
  stability_other <- parsed_other %>%
    group_by(id, dataset, task, player) %>%
    summarise(
      n_runs_other = n_distinct(run),
      modal_freq_other = {
        tbl <- table(classification)
        max(tbl) / sum(tbl)
      },
      flip_rate_other = 1 - modal_freq_other,
      is_stable_other = (modal_freq_other == 1),
      .groups = "drop"
    )
  
  merged <- conf_r %>%
    inner_join(stability_other, by = c("id", "dataset", "task", "player")) %>%
    mutate(unstable_other = !is_stable_other)
  
  merged
}

# compare metrics (top1 vs margin vs entropy) leakage-free
compare_metrics_leakage_free <- function(results, conf_results, predictor_run = 1) {
  
  merged <- make_leakage_free_merged(results, conf_results, predictor_run)
  
  # Define scores + directions
  metrics <- tibble::tribble(
    ~name,      ~score_col,      ~direction,
    "top1_prob","top1_prob_r",   "low_flags",
    "margin",   "margin_r",      "low_flags",
    "entropy",  "entropy_r",     "high_flags"
  )
  
  out <- purrr::pmap_dfr(metrics, function(name, score_col, direction) {
    score <- merged[[score_col]]
    unstable <- merged$unstable_other
    
    roc <- roc_generic(score, unstable, direction = direction)
    auc <- auc_from_roc(roc)
    
    # best F1 threshold (optional)
    best <- roc %>% filter(F1 == max(F1, na.rm = TRUE)) %>% slice(1)
    
    tibble(
      predictor_run = predictor_run,
      metric = name,
      auc = auc,
      best_threshold = best$threshold,
      best_F1 = best$F1,
      best_precision = best$precision,
      best_recall = best$recall
    )
  })
  
  list(summary = out, merged = merged)
}


cmp1 <- compare_metrics_leakage_free(results, conf_results, predictor_run = 1)
cmp1$summary




runs_to_test <- 1:10

cmp_all <- purrr::map_dfr(runs_to_test, function(r) {
  compare_metrics_leakage_free(results, conf_results, predictor_run = r)$summary
})

cmp_all %>%
  group_by(metric) %>%
  summarise(
    mean_auc = mean(auc, na.rm = TRUE),
    sd_auc = sd(auc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_auc))
