# =============================================================================
# MASTER ANALYSIS SCRIPT
# =============================================================================
# Run this single script to perform complete stability + confidence analysis
# 
# Usage: 
#   MODEL <- "gpt-4o"      # or "gpt-4o-mini"
#   source("run_analysis.R")
# =============================================================================

library(tidyverse)
library(jsonlite)

# =============================================================================
# MODEL SELECTION
# =============================================================================

# Set which model to analyze
# Options: "gpt-4o", "gpt-4o-mini"
if (!exists("MODEL")) {
  MODEL <- "gpt-4o"
}

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat(sprintf("ANALYZING MODEL: %s\n", MODEL))
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")

# Set working directory (adjust if needed)
setwd("~/Desktop/Reproduciblity/R")

# =============================================================================
# STEP 1: Load stability analysis
# =============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("STEP 1: STABILITY ANALYSIS\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

source("stability_analysis.R")

# =============================================================================
# STEP 2: Load confidence analysis and extract metrics
# =============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("STEP 2: CONFIDENCE ANALYSIS\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

source("confidence_analysis.R")
conf_results <- run_confidence_analysis(results)

# =============================================================================
# STEP 3: Threshold optimization
# =============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("STEP 3: THRESHOLD OPTIMIZATION\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")

thresh <- threshold_analysis(conf_results$merged)

# =============================================================================
# STEP 4: Bin analysis
# =============================================================================

bins <- bin_analysis(conf_results$merged)

# =============================================================================
# STEP 5: Generate plots
# =============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("STEP 5: GENERATING PLOTS\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# ROC curve
p_roc <- plot_roc(thresh$roc_top1)
print(p_roc)

# Precision-Recall curve
p_pr <- plot_precision_recall(thresh$roc_top1)
print(p_pr)

# Confidence vs Flip Rate scatter
p_scatter <- ggplot(conf_results$merged, aes(x = mean_top1_prob, y = flip_rate)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  geom_vline(xintercept = thresh$optimal$recall_80$threshold, 
             linetype = "dashed", color = "blue") +
  annotate("text", x = thresh$optimal$recall_80$threshold - 0.05, y = 0.4,
           label = sprintf("Threshold = %.2f", thresh$optimal$recall_80$threshold),
           angle = 90, color = "blue") +
  labs(
    title = "Confidence vs Flip Rate",
    subtitle = sprintf("r = %.3f, p < 0.001", 
                       cor(conf_results$merged$mean_top1_prob, 
                           conf_results$merged$flip_rate, 
                           use = "complete.obs")),
    x = "Mean Top-1 Probability",
    y = "Flip Rate"
  ) +
  theme_minimal()
print(p_scatter)

# Bin analysis plot
p_bins <- ggplot(bins, aes(x = conf_bin, y = pct_unstable)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_line(aes(y = mean_flip_rate * 10), color = "red", size = 1) +
  geom_point(aes(y = mean_flip_rate * 10), color = "red", size = 2) +
  scale_y_continuous(
    name = "% Unstable",
    sec.axis = sec_axis(~./10, name = "Mean Flip Rate (%)")
  ) +
  labs(
    title = "Instability by Confidence Decile",
    subtitle = "Lower confidence → Higher instability",
    x = "Confidence Decile (1 = lowest)"
  ) +
  theme_minimal()
print(p_bins)

# =============================================================================
# STEP 6: Summary
# =============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("ANALYSIS COMPLETE\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("Objects available:\n")
cat("  results       - Stability analysis results\n")
cat("  conf_results  - Confidence analysis results\n")
cat("  thresh        - Threshold optimization results\n")
cat("  bins          - Bin analysis results\n\n")

cat("Key findings:\n")
cat(sprintf("  - Total units: %d\n", nrow(conf_results$merged)))
cat(sprintf("  - Unstable: %d (%.1f%%)\n", 
            sum(!conf_results$merged$is_stable),
            mean(!conf_results$merged$is_stable) * 100))
cat(sprintf("  - AUC (Top-1 Prob): %.3f\n", thresh$auc["top1"]))
cat(sprintf("  - Optimal threshold (80%% recall): %.3f\n", 
            thresh$optimal$recall_80$threshold))
cat(sprintf("  - At this threshold: flag %.1f%% for review, catch %.0f%% of unstable\n",
            thresh$optimal$recall_80$pct_flagged,
            thresh$optimal$recall_80$TPR * 100))

cat("\nPlots displayed. To save:\n")
cat("  ggsave('roc_curve.png', p_roc, width = 8, height = 6)\n")
cat("  ggsave('precision_recall.png', p_pr, width = 8, height = 6)\n")
cat("  ggsave('confidence_vs_flip.png', p_scatter, width = 8, height = 6)\n")
cat("  ggsave('bin_analysis.png', p_bins, width = 8, height = 6)\n")