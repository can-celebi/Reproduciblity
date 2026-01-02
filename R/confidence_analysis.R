# =============================================================================
# LLM Classification Confidence Analysis
# =============================================================================
# Extracts and analyzes logprobs to calculate confidence metrics
# and investigate relationship between confidence and stability

library(tidyverse)
library(jsonlite)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Use same model/directory as stability_analysis.R
# If MODEL is already set in environment, use it; otherwise default to gpt-4o
if (!exists("MODEL")) {
  MODEL <- "gpt-4o"
}

base_output_dir <- "~/Desktop/Reproduciblity/output"
output_dir <- file.path(base_output_dir, MODEL)

# Fall back to base directory if model subdirectory doesn't exist
if (!dir.exists(output_dir) && dir.exists(base_output_dir)) {
  output_dir <- base_output_dir
}

# Valid classification tokens by task
# These are the ONLY tokens we care about for probability calculation
VALID_TOKENS <- list(
  p1_promise = c("0", "1"),
  p2_promise = c("0", "1", "na"),  # Same for p1, p2, p3 players
  l1_level = c("0", "1", "2", "3"),
  l2_level = c("0", "1", "2", "3", "4", "5"),
  l2_belief = c("0", "1", "2", "3"),
  l3_level = c("0", "1", "2", "3", "4", "5", "eq", "na"),
  l3_belief = c("0", "16", "26", "36", "46", "56", "66", "76", "na")
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Convert logprob to probability
logprob_to_prob <- function(lp) exp(lp)

#' Calculate normalized entropy over a probability distribution
#' Returns value in [0, 1] where 0 = certain, 1 = max uncertainty
#' @param probs Named vector of probabilities (must sum to 1)
#' @param n_classes Total number of possible classes (for proper normalization)
normalized_entropy <- function(probs, n_classes = NULL) {
  # Remove zeros to avoid log(0)
  probs <- probs[probs > 0]
  if (length(probs) <= 1) return(0)
  
  # Shannon entropy
  H <- -sum(probs * log2(probs))
  
 # Max entropy: use n_classes if provided, otherwise use observed classes
  if (is.null(n_classes)) {
    n_classes <- length(probs)
  }
  H_max <- log2(n_classes)
  
  # Normalize
  if (H_max == 0) return(0)
  min(H / H_max, 1)  # Cap at 1
}

# =============================================================================
# DIAGNOSTIC FUNCTION - Run this first to understand logprobs structure
# =============================================================================

#' Inspect logprobs structure from a sample of records
#' Run this first to understand how the data is organized
diagnose_logprobs <- function(parsed_df, n_samples = 5) {
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
  cat("LOGPROBS STRUCTURE DIAGNOSIS\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
  
  # Sample different task types
  tasks <- parsed_df %>% 
    distinct(dataset, task) %>%
    head(n_samples)
  
  for (i in 1:nrow(tasks)) {
    d <- tasks$dataset[i]
    t <- tasks$task[i]
    
    sample_row <- parsed_df %>%
      filter(dataset == d, task == t) %>%
      slice(1)
    
    cat(sprintf("\n--- %s_%s ---\n", d, t))
    cat("Player:", sample_row$player %||% "NA", "\n")
    cat("Classification:", sample_row$classification, "\n")
    
    lp <- sample_row$logprobs[[1]]
    
    if (is.null(lp)) {
      cat("Logprobs: NULL\n")
      next
    }
    
    cat("Logprobs type:", class(lp), "\n")
    
    if (is.data.frame(lp)) {
      cat("Logprobs dim:", nrow(lp), "x", ncol(lp), "\n")
      cat("Columns:", paste(names(lp), collapse = ", "), "\n")
      cat("\nTokens in response:\n")
      for (j in 1:min(nrow(lp), 10)) {
        tok <- lp$token[j]
        logp <- lp$logprob[j]
        cat(sprintf("  [%d] '%s' (logprob: %.3f)\n", j, tok, logp))
      }
      
      # Check top_logprobs structure
      if ("top_logprobs" %in% names(lp)) {
        cat("\ntop_logprobs structure at position 1:\n")
        top_lp <- lp$top_logprobs[[1]]
        cat("  Type:", class(top_lp), "\n")
        if (is.data.frame(top_lp)) {
          cat("  Dim:", nrow(top_lp), "x", ncol(top_lp), "\n")
          cat("  Columns:", paste(names(top_lp), collapse = ", "), "\n")
          cat("  First 5 alternatives:\n")
          for (k in 1:min(nrow(top_lp), 5)) {
            cat(sprintf("    '%s': %.3f\n", top_lp$token[k], top_lp$logprob[k]))
          }
        } else if (is.list(top_lp)) {
          cat("  Length:", length(top_lp), "\n")
          cat("  First element:", str(top_lp[[1]]), "\n")
        }
      }
    }
  }
  
  cat("\n" %>% rep(2) %>% paste(collapse = ""))
  cat("Diagnosis complete. Check output format to verify extraction logic.\n")
}

# =============================================================================
# LOGPROBS EXTRACTION - SIMPLIFIED ROBUST VERSION
# =============================================================================

#' Extract confidence metrics from a single observation
#' Handles the nested logprobs structure from Azure OpenAI
#' @param logprobs_data The logprobs list/dataframe from one record
#' @param valid_tokens Vector of valid classification tokens to look for
#' @param player For p2_promise, which player (p1/p2/p3) to extract. NULL for other tasks.
#' @return Tibble with confidence metrics or NULL if extraction fails
extract_confidence_single <- function(logprobs_data, valid_tokens, player = NULL) {
  
  # Handle NULL/empty
  if (is.null(logprobs_data) || length(logprobs_data) == 0) {
    return(NULL)
  }
  
  # Convert to dataframe if needed
  if (!is.data.frame(logprobs_data)) {
    logprobs_data <- tryCatch(bind_rows(logprobs_data), error = function(e) NULL)
    if (is.null(logprobs_data)) return(NULL)
  }
  
  # Must have token and top_logprobs columns
  if (!all(c("token", "top_logprobs") %in% names(logprobs_data))) {
    return(NULL)
  }
  
  tokens <- logprobs_data$token
  n_tokens <- length(tokens)
  
  # Find the position with a valid classification token
  target_pos <- NULL
  
  if (!is.null(player)) {
    # For p2_promise: tokens are split like 'p', '1', '":"', 'VALUE'
    # Need to find 'p' followed by player number, then find value after '":"'
    player_num <- substr(player, 2, 2)  # "p1" -> "1"
    
    for (i in 1:(n_tokens - 3)) {
      # Look for pattern: 'p' -> player_num -> '":"' -> VALUE
      if (tokens[i] == "p" && 
          i + 1 <= n_tokens && tokens[i + 1] == player_num &&
          i + 2 <= n_tokens && tokens[i + 2] == '":"') {
        # The value should be at position i + 3
        value_pos <- i + 3
        if (value_pos <= n_tokens) {
          val <- tokens[value_pos]
          if (val %in% valid_tokens) {
            target_pos <- value_pos
            break
          }
        }
      }
    }
  } else {
    # For single classification tasks: {"classification":"X"}
    # Classification value is at position 4 (token index)
    # But let's be more robust - find '":"' then check next token
    for (i in 1:(n_tokens - 1)) {
      if (tokens[i] == '":"') {
        next_tok <- tokens[i + 1]
        if (next_tok %in% valid_tokens) {
          target_pos <- i + 1
          break
        }
      }
    }
  }
  
  if (is.null(target_pos) || target_pos > n_tokens) {
    return(NULL)
  }
  
  # Extract top_logprobs at target position
  top_lp <- logprobs_data$top_logprobs[[target_pos]]
  chosen_token <- tokens[target_pos]
  chosen_logprob <- logprobs_data$logprob[target_pos]
  
  # Handle different top_logprobs structures
  if (is.null(top_lp) || length(top_lp) == 0) {
    # No alternatives, just the chosen token
    return(tibble(
      chosen_token = chosen_token,
      top1_token = chosen_token,
      top1_prob = logprob_to_prob(chosen_logprob),
      top2_token = NA_character_,
      top2_prob = 0,
      margin = logprob_to_prob(chosen_logprob),
      top1_logprob = chosen_logprob,
      top2_logprob = -Inf,
      logprob_gap = Inf,  # No competition
      norm_entropy = 0,
      n_valid_in_top20 = 1L
    ))
  }
  
  # Extract tokens and logprobs from top_logprobs
  if (is.data.frame(top_lp)) {
    alt_tokens <- top_lp$token
    alt_logprobs <- top_lp$logprob
  } else if (is.list(top_lp)) {
    # Try to extract from list structure
    alt_tokens <- sapply(top_lp, function(x) x$token %||% NA_character_)
    alt_logprobs <- sapply(top_lp, function(x) x$logprob %||% NA_real_)
  } else {
    return(NULL)
  }
  
  # Filter to ONLY valid classification tokens
  valid_mask <- alt_tokens %in% valid_tokens
  
  if (sum(valid_mask) == 0) {
    # No valid tokens in top 20 - use chosen only
    return(tibble(
      chosen_token = chosen_token,
      top1_token = chosen_token,
      top1_prob = logprob_to_prob(chosen_logprob),
      top2_token = NA_character_,
      top2_prob = 0,
      margin = logprob_to_prob(chosen_logprob),
      top1_logprob = chosen_logprob,
      top2_logprob = -Inf,
      logprob_gap = Inf,  # No competition
      norm_entropy = 0,
      n_valid_in_top20 = 1L
    ))
  }
  
  valid_tokens_found <- alt_tokens[valid_mask]
  valid_logprobs <- alt_logprobs[valid_mask]
  
  # Sort by RAW logprobs first (for logprob gap calculation)
  logprob_order <- order(valid_logprobs, decreasing = TRUE)
  sorted_tokens_by_lp <- valid_tokens_found[logprob_order]
  sorted_logprobs <- valid_logprobs[logprob_order]
  
  # Get top1 and top2 raw logprobs
  top1_logprob <- sorted_logprobs[1]
  top2_logprob <- if(length(sorted_logprobs) > 1) sorted_logprobs[2] else -Inf
  logprob_gap <- top1_logprob - top2_logprob  # Higher = more confident
  
  # Convert logprobs to probabilities
  valid_probs <- logprob_to_prob(valid_logprobs)
  
  # Normalize so they sum to 1 (renormalize over valid tokens only)
  prob_sum <- sum(valid_probs)
  if (prob_sum == 0) prob_sum <- 1  # Avoid division by zero
  valid_probs_norm <- valid_probs / prob_sum
  
  # Create sorted probability distribution (by normalized prob)
  prob_order <- order(valid_probs_norm, decreasing = TRUE)
  sorted_tokens <- valid_tokens_found[prob_order]
  sorted_probs <- valid_probs_norm[prob_order]
  
  # Calculate metrics
  top1_token <- sorted_tokens[1]
  top1_prob <- sorted_probs[1]
  top2_token <- if(length(sorted_tokens) > 1) sorted_tokens[2] else NA_character_
  top2_prob <- if(length(sorted_probs) > 1) sorted_probs[2] else 0
  margin <- top1_prob - top2_prob
  
  # Normalized entropy over ALL possible valid classes (not just observed)
  n_possible_classes <- length(valid_tokens)
  ent <- normalized_entropy(sorted_probs, n_classes = n_possible_classes)
  
  tibble(
    chosen_token = chosen_token,
    top1_token = top1_token,
    top1_prob = top1_prob,
    top2_token = top2_token,
    top2_prob = top2_prob,
    margin = margin,
    top1_logprob = top1_logprob,
    top2_logprob = top2_logprob,
    logprob_gap = logprob_gap,  # NEW: raw logprob difference
    norm_entropy = ent,
    n_valid_in_top20 = length(sorted_tokens)
  )
}

# =============================================================================
# MAIN EXTRACTION FUNCTION
# =============================================================================

#' Extract confidence metrics from all records
#' @param parsed_df Output from parse_classifications() with logprobs column
#' @return Dataframe with confidence metrics for each observation
extract_confidence_metrics <- function(parsed_df) {
  
  cat("Extracting confidence metrics from logprobs...\n")
  cat("Total observations:", nrow(parsed_df), "\n\n")
  
  results <- list()
  n_success <- 0
  n_fail <- 0
  
  pb <- txtProgressBar(min = 0, max = nrow(parsed_df), style = 3)
  
  for (i in 1:nrow(parsed_df)) {
    row <- parsed_df[i, ]
    
    # Determine valid tokens for this task
    task_key <- paste0(row$dataset, "_", row$task)
    valid_tokens <- VALID_TOKENS[[task_key]]
    
    if (is.null(valid_tokens)) {
      n_fail <- n_fail + 1
      setTxtProgressBar(pb, i)
      next
    }
    
    # Get logprobs data
    logprobs_data <- row$logprobs[[1]]
    
    # Determine player (for p2_promise)
    player <- if (row$dataset == "p2" && row$task == "promise") row$player else NULL
    
    # Extract confidence
    conf <- extract_confidence_single(logprobs_data, valid_tokens, player)
    
    if (is.null(conf)) {
      n_fail <- n_fail + 1
    } else {
      conf$id <- row$id
      conf$run <- row$run
      conf$dataset <- row$dataset
      conf$task <- row$task
      conf$player <- row$player %||% NA_character_
      conf$classification <- row$classification
      results[[length(results) + 1]] <- conf
      n_success <- n_success + 1
    }
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  cat(sprintf("\nExtracted: %d successful (%.1f%%), %d failed\n", 
              n_success, n_success/nrow(parsed_df)*100, n_fail))
  
  if (length(results) == 0) {
    warning("No confidence metrics extracted!")
    return(NULL)
  }
  
  # Combine and reorder columns
  bind_rows(results) %>%
    select(id, run, dataset, task, player, classification, 
           chosen_token, top1_token, top1_prob, top2_token, top2_prob,
           margin, norm_entropy, n_valid_in_top20)
}

# =============================================================================
# AGGREGATE CONFIDENCE BY UNIT
# =============================================================================

#' Aggregate confidence metrics across runs for each classification unit
#' @param confidence_df Output from extract_confidence_metrics()
#' @return Dataframe with mean confidence metrics per unit
aggregate_confidence <- function(confidence_df) {
  
  confidence_df %>%
    group_by(id, dataset, task, player) %>%
    summarise(
      n_runs = n(),
      
      # Mean confidence metrics
      mean_top1_prob = mean(top1_prob, na.rm = TRUE),
      sd_top1_prob = sd(top1_prob, na.rm = TRUE),
      min_top1_prob = min(top1_prob, na.rm = TRUE),
      max_top1_prob = max(top1_prob, na.rm = TRUE),
      
      mean_margin = mean(margin, na.rm = TRUE),
      sd_margin = sd(margin, na.rm = TRUE),
      
      mean_entropy = mean(norm_entropy, na.rm = TRUE),
      sd_entropy = sd(norm_entropy, na.rm = TRUE),
      
      # Logprob gap (exclude Inf values)
      mean_logprob_gap = mean(logprob_gap[is.finite(logprob_gap)], na.rm = TRUE),
      sd_logprob_gap = sd(logprob_gap[is.finite(logprob_gap)], na.rm = TRUE),
      
      # Confidence variability across runs
      conf_variability = sd(top1_prob, na.rm = TRUE),
      
      .groups = "drop"
    )
}

# =============================================================================
# MERGE WITH STABILITY DATA
# =============================================================================

#' Merge confidence metrics with stability metrics
#' @param confidence_agg Output from aggregate_confidence()
#' @param stability_df Stability metrics from stability_analysis.R
#' @return Merged dataframe for analysis
merge_confidence_stability <- function(confidence_agg, stability_df) {
  
  # Join on id, dataset, task, player
  merged <- stability_df %>%
    left_join(
      confidence_agg,
      by = c("id", "dataset", "task", "player")
    )
  
  merged
}

# =============================================================================
# STATISTICAL ANALYSIS
# =============================================================================

#' Analyze relationship between confidence and stability
analyze_confidence_stability <- function(merged_df) {
  
  cat("\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
  cat("CONFIDENCE-STABILITY RELATIONSHIP\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
  
  # Filter to units with both confidence and stability data
  analysis_df <- merged_df %>%
    filter(!is.na(mean_top1_prob), !is.na(flip_rate))
  
  cat("Units with complete data:", nrow(analysis_df), "\n\n")
  
  # --- Correlations ---
  cat("CORRELATIONS:\n")
  
  cor_top1 <- cor(analysis_df$mean_top1_prob, analysis_df$flip_rate, 
                   use = "complete.obs", method = "spearman")
  cor_margin <- cor(analysis_df$mean_margin, analysis_df$flip_rate, 
                    use = "complete.obs", method = "spearman")
  cor_entropy <- cor(analysis_df$mean_entropy, analysis_df$flip_rate, 
                     use = "complete.obs", method = "spearman")
  
  cat(sprintf("  Top-1 Probability vs Flip Rate: r = %.3f\n", cor_top1))
  cat(sprintf("  Margin vs Flip Rate: r = %.3f\n", cor_margin))
  cat(sprintf("  Entropy vs Flip Rate: r = %.3f\n", cor_entropy))
  
  # --- Test significance ---
  cat("\nSIGNIFICANCE TESTS (Spearman):\n")
  
  test_top1 <- cor.test(analysis_df$mean_top1_prob, analysis_df$flip_rate, 
                        method = "spearman")
  test_margin <- cor.test(analysis_df$mean_margin, analysis_df$flip_rate, 
                          method = "spearman")
  test_entropy <- cor.test(analysis_df$mean_entropy, analysis_df$flip_rate, 
                           method = "spearman")
  
  cat(sprintf("  Top-1: p = %.2e %s\n", test_top1$p.value, 
              if(test_top1$p.value < 0.001) "***" else if(test_top1$p.value < 0.01) "**" else if(test_top1$p.value < 0.05) "*" else ""))
  cat(sprintf("  Margin: p = %.2e %s\n", test_margin$p.value,
              if(test_margin$p.value < 0.001) "***" else if(test_margin$p.value < 0.01) "**" else if(test_margin$p.value < 0.05) "*" else ""))
  cat(sprintf("  Entropy: p = %.2e %s\n", test_entropy$p.value,
              if(test_entropy$p.value < 0.001) "***" else if(test_entropy$p.value < 0.01) "**" else if(test_entropy$p.value < 0.05) "*" else ""))
  
  # --- Group comparison: Stable vs Unstable ---
  cat("\n")
  cat("STABLE vs UNSTABLE COMPARISON:\n\n")
  
  comparison <- analysis_df %>%
    group_by(is_stable) %>%
    summarise(
      n = n(),
      mean_top1_prob = mean(mean_top1_prob, na.rm = TRUE),
      sd_top1_prob = sd(mean_top1_prob, na.rm = TRUE),
      mean_margin = mean(mean_margin, na.rm = TRUE),
      mean_entropy = mean(mean_entropy, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(group = ifelse(is_stable, "Stable", "Unstable"))
  
  print(comparison %>% select(group, n, mean_top1_prob, sd_top1_prob, mean_margin, mean_entropy))
  
  # Wilcoxon test for difference
  stable_conf <- analysis_df %>% filter(is_stable) %>% pull(mean_top1_prob)
  unstable_conf <- analysis_df %>% filter(!is_stable) %>% pull(mean_top1_prob)
  
  if (length(unstable_conf) > 1) {
    wilcox_test <- wilcox.test(stable_conf, unstable_conf)
    cat(sprintf("\nWilcoxon test (Top-1 Prob): W = %.0f, p = %.2e %s\n", 
                wilcox_test$statistic, wilcox_test$p.value,
                if(wilcox_test$p.value < 0.001) "***" else if(wilcox_test$p.value < 0.01) "**" else if(wilcox_test$p.value < 0.05) "*" else ""))
  }
  
  # --- By task ---
  cat("\n")
  cat("CORRELATION BY TASK:\n\n")
  
  by_task <- analysis_df %>%
    group_by(dataset, task) %>%
    summarise(
      n = n(),
      n_unstable = sum(!is_stable),
      cor_top1_flip = cor(mean_top1_prob, flip_rate, use = "complete.obs", method = "spearman"),
      cor_margin_flip = cor(mean_margin, flip_rate, use = "complete.obs", method = "spearman"),
      .groups = "drop"
    )
  
  print(by_task)
  
  invisible(analysis_df)
}

# =============================================================================
# POWER ANALYSIS FOR 50 RUNS
# =============================================================================

power_analysis_50runs <- function(stability_df, confidence_agg = NULL) {
  
  cat("\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
  cat("STATISTICAL POWER ANALYSIS (Projecting to 50 Runs)\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
  
  k_current <- max(stability_df$n_runs, na.rm = TRUE)
  k_target <- 50
  
  # Current counts
  n_total <- nrow(stability_df)
  n_unstable <- sum(!stability_df$is_stable)
  pct_unstable <- mean(!stability_df$is_stable) * 100
  mean_flip_rate <- mean(stability_df$flip_rate, na.rm = TRUE)
  
  cat("CURRENT STATE (k =", k_current, "runs):\n")
  cat(sprintf("  Total classification units: %d\n", n_total))
  cat(sprintf("  Unstable units (any flip): %d (%.1f%%)\n", n_unstable, pct_unstable))
  cat(sprintf("  Mean flip rate: %.2f%%\n", mean_flip_rate * 100))
  
  # --- Projection to 50 runs ---
  cat("\n")
  cat("PROJECTION TO 50 RUNS:\n\n")
  
  # Model: Each run pair has independent flip probability p
  # P(stable after k runs) ≈ (1-p)^(k-1) [need k-1 consecutive non-flips]
  # This is approximate since flips aren't truly independent
  
  # Better model: use observed flip rate per unit
  # P(at least 1 flip in k runs) = 1 - P(modal every time)
  # If modal_freq = m, then P(stable) ≈ m^k (roughly)
  
  # Empirical approach: extrapolate from current data
  # At k=7, we see pct_unstable. What's the per-run "flip discovery rate"?
  
  # Simple binomial model
  p_flip_per_run <- mean_flip_rate
  p_stable_50 <- (1 - p_flip_per_run)^(k_target - 1)
  p_unstable_50 <- 1 - p_stable_50
  expected_unstable_50 <- n_total * p_unstable_50
  
  cat("Binomial Model (assumes independent flips):\n")
  cat(sprintf("  P(flip) per run: %.3f\n", p_flip_per_run))
  cat(sprintf("  P(stable after 50 runs): %.1f%%\n", p_stable_50 * 100))
  cat(sprintf("  P(unstable after 50 runs): %.1f%%\n", p_unstable_50 * 100))
  cat(sprintf("  Expected unstable units: ~%.0f of %d\n", expected_unstable_50, n_total))
  
  # Alternative: use modal frequency
  mean_modal_freq <- mean(stability_df$modal_freq, na.rm = TRUE)
  p_stable_modal <- mean_modal_freq^(k_target)
  expected_unstable_modal <- n_total * (1 - p_stable_modal)
  
  cat("\nModal Frequency Model:\n")
  cat(sprintf("  Mean modal agreement: %.1f%%\n", mean_modal_freq * 100))
  cat(sprintf("  P(modal all 50 runs): %.1f%%\n", p_stable_modal * 100))
  cat(sprintf("  Expected unstable: ~%.0f of %d\n", expected_unstable_modal, n_total))
  
  # --- Sample size adequacy ---
  cat("\n")
  cat("SAMPLE SIZE ADEQUACY:\n\n")
  
  # Use more conservative estimate
  n_unstable_projected <- min(expected_unstable_50, expected_unstable_modal)
  
  cat(sprintf("Projected unstable units (conservative): ~%.0f\n", n_unstable_projected))
  
  # For correlation analysis
  cat("\nFor Correlation Analysis:\n")
  for (r in c(0.1, 0.2, 0.3, 0.5)) {
    # Sample size needed for 80% power at α=0.05
    # Using formula: n = ((z_α + z_β) / arctanh(r))^2 + 3
    z_alpha <- 1.96
    z_beta <- 0.84  # 80% power
    n_needed <- ceiling(((z_alpha + z_beta) / atanh(r))^2 + 3)
    
    can_detect <- n_unstable_projected >= n_needed
    cat(sprintf("  r = %.1f (%.0f%% effect): need n ≥ %d → %s\n",
                r, r*100, n_needed,
                if(can_detect) "✓ Detectable" else "✗ Underpowered"))
  }
  
  # For threshold/binning analysis
  cat("\nFor Threshold Analysis (quintile bins):\n")
  n_bins <- 5
  n_per_bin <- n_unstable_projected / n_bins
  cat(sprintf("  %d bins → ~%.0f unstable units per bin\n", n_bins, n_per_bin))
  cat(sprintf("  Rule of thumb (30+ per bin): %s\n", 
              if(n_per_bin >= 30) "✓ Sufficient" else if(n_per_bin >= 20) "⚠ Marginal" else "✗ Use fewer bins"))
  
  # For confidence interval precision
  cat("\nConfidence Interval Precision:\n")
  
  # CI for proportion (unstable rate)
  p_hat <- n_unstable_projected / n_total
  se_prop <- sqrt(p_hat * (1 - p_hat) / n_total)
  ci_width_prop <- 2 * 1.96 * se_prop
  cat(sprintf("  95%% CI for unstable rate: ±%.1f percentage points\n", ci_width_prop * 100))
  
  # CI for mean flip rate
  # Assuming similar SD to current
  sd_flip <- sd(stability_df$flip_rate, na.rm = TRUE)
  se_flip <- sd_flip / sqrt(n_total)
  ci_width_flip <- 2 * 1.96 * se_flip
  cat(sprintf("  95%% CI for mean flip rate: ±%.2f percentage points\n", ci_width_flip * 100))
  
  # --- Recommendations ---
  cat("\n")
  cat("RECOMMENDATIONS:\n\n")
  
  if (n_unstable_projected >= 200) {
    cat("✓ Excellent: Projected ~", round(n_unstable_projected), " unstable units\n", sep = "")
    cat("  - Can detect small correlations (r ≥ 0.2)\n")
    cat("  - Can use 5+ confidence bins for threshold analysis\n")
    cat("  - Can compute narrow confidence intervals\n")
    cat("  - Can do subgroup analysis by task\n")
  } else if (n_unstable_projected >= 100) {
    cat("✓ Good: Projected ~", round(n_unstable_projected), " unstable units\n", sep = "")
    cat("  - Can detect medium correlations (r ≥ 0.3)\n")
    cat("  - Use 3-4 confidence bins for threshold analysis\n")
    cat("  - Consider pooling across tasks\n")
  } else if (n_unstable_projected >= 50) {
    cat("⚠ Marginal: Projected ~", round(n_unstable_projected), " unstable units\n", sep = "")
    cat("  - Can only detect large effects (r ≥ 0.4)\n")
    cat("  - Use 2-3 bins or median split\n")
    cat("  - Pool all tasks together\n")
  } else {
    cat("✗ Underpowered: Projected ~", round(n_unstable_projected), " unstable units\n", sep = "")
    cat("  - Consider: more runs, different analysis approach\n")
    cat("  - Focus on descriptive rather than inferential statistics\n")
  }
  
  # Return projections for use elsewhere
  invisible(list(
    k_current = k_current,
    k_target = k_target,
    n_total = n_total,
    n_unstable_current = n_unstable,
    n_unstable_projected = n_unstable_projected,
    mean_flip_rate = mean_flip_rate
  ))
}

# =============================================================================
# MAIN ANALYSIS RUNNER
# =============================================================================

run_confidence_analysis <- function(results_from_stability, force_reload = FALSE) {
  
  # Check if conf_results already exist
  if (!force_reload && exists("conf_results", envir = .GlobalEnv)) {
    existing <- get("conf_results", envir = .GlobalEnv)
    if (is.list(existing) && all(c("confidence_raw", "merged") %in% names(existing))) {
      cat("Using cached conf_results (", nrow(existing$merged), " units)\n", sep = "")
      cat("To reload, use: conf_results <- run_confidence_analysis(results, force_reload = TRUE)\n\n")
      return(invisible(existing))
    }
  }
  
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
  cat("CONFIDENCE METRIC EXTRACTION\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
  
  # Extract confidence metrics from parsed data
  confidence <- extract_confidence_metrics(results_from_stability$parsed)
  
  if (is.null(confidence)) {
    stop("Failed to extract confidence metrics")
  }
  
  cat("\nSample confidence data:\n")
  print(head(confidence, 10))
  
  # Aggregate by unit
  cat("\nAggregating confidence by classification unit...\n")
  confidence_agg <- aggregate_confidence(confidence)
  
  # Merge with stability
  cat("Merging with stability metrics...\n")
  merged <- merge_confidence_stability(confidence_agg, results_from_stability$stability)
  
  # Run statistical analysis
  analysis_df <- analyze_confidence_stability(merged)
  
  # Power analysis
  power_analysis_50runs(results_from_stability$stability, confidence)
  
  # Return everything
  invisible(list(
    confidence_raw = confidence,
    confidence_agg = confidence_agg,
    merged = merged,
    analysis_df = analysis_df
  ))
}

# =============================================================================
# THRESHOLD OPTIMIZATION & ROC ANALYSIS
# =============================================================================

#' Calculate ROC curve for predicting instability from confidence
#' @param merged_df Output from merge_confidence_stability()
#' @param confidence_col Column name for confidence metric (default: mean_top1_prob)
#' @return Dataframe with threshold, TPR, FPR, precision, recall, F1
calculate_roc <- function(merged_df, confidence_col = "mean_top1_prob") {
  
  # Remove NAs
  df <- merged_df %>%
    filter(!is.na(.data[[confidence_col]]), !is.na(is_stable))
  
  # For instability prediction: LOW confidence = predict unstable
  # So we invert: predict unstable if confidence < threshold
  
  conf_values <- df[[confidence_col]]
  is_unstable <- !df$is_stable  # TRUE = actually unstable
  
  # Get unique thresholds
  thresholds <- sort(unique(c(0, conf_values, 1)))
  
  results <- map_dfr(thresholds, function(thresh) {
    # Predict unstable if confidence < threshold
    pred_unstable <- conf_values < thresh
    
    # Confusion matrix
    TP <- sum(pred_unstable & is_unstable)   # Correctly predicted unstable
    FP <- sum(pred_unstable & !is_unstable)  # Incorrectly predicted unstable (actually stable)
    TN <- sum(!pred_unstable & !is_unstable) # Correctly predicted stable
    FN <- sum(!pred_unstable & is_unstable)  # Missed unstable (predicted stable)
    
    # Metrics
    TPR <- if (TP + FN > 0) TP / (TP + FN) else 0  # Sensitivity/Recall
    FPR <- if (FP + TN > 0) FP / (FP + TN) else 0  # False positive rate
    precision <- if (TP + FP > 0) TP / (TP + FP) else 0
    specificity <- if (TN + FP > 0) TN / (TN + FP) else 0
    
    # F1 score
    F1 <- if (precision + TPR > 0) 2 * precision * TPR / (precision + TPR) else 0
    
    # Fraction flagged for review
    pct_flagged <- mean(pred_unstable) * 100
    
    tibble(
      threshold = thresh,
      TP = TP, FP = FP, TN = TN, FN = FN,
      TPR = TPR,           # Recall: what % of unstable do we catch?
      FPR = FPR,           # What % of stable do we incorrectly flag?
      precision = precision, # Of those we flag, what % are actually unstable?
      specificity = specificity,
      F1 = F1,
      pct_flagged = pct_flagged
    )
  })
  
  results
}

#' Calculate AUC (Area Under ROC Curve)
#' @param roc_df Output from calculate_roc()
#' @return Numeric AUC value
calculate_auc <- function(roc_df) {
  # Sort by FPR
  roc_sorted <- roc_df %>% arrange(FPR, TPR)
  
  # Trapezoidal integration
  n <- nrow(roc_sorted)
  auc <- 0
  for (i in 2:n) {
    width <- roc_sorted$FPR[i] - roc_sorted$FPR[i-1]
    height <- (roc_sorted$TPR[i] + roc_sorted$TPR[i-1]) / 2
    auc <- auc + width * height
  }
  
  auc
}

#' Find optimal threshold using various criteria
#' @param roc_df Output from calculate_roc()
#' @param criterion One of: "youden" (max TPR-FPR), "f1" (max F1), "recall_90" (90% recall), 
#'                  "precision_50" (50% precision), "cost" (custom cost function)
#' @param fp_cost Cost of false positive (for "cost" criterion)
#' @param fn_cost Cost of false negative (for "cost" criterion)
find_optimal_threshold <- function(roc_df, criterion = "youden", fp_cost = 1, fn_cost = 1) {
  
  if (criterion == "youden") {
    # Maximize Youden's J = TPR - FPR (equivalently, sensitivity + specificity - 1)
    roc_df %>%
      mutate(youden_j = TPR - FPR) %>%
      filter(youden_j == max(youden_j)) %>%
      slice(1)
    
  } else if (criterion == "f1") {
    # Maximize F1 score
    roc_df %>%
      filter(F1 == max(F1)) %>%
      slice(1)
    
  } else if (criterion == "recall_90") {
    # Find threshold that gives at least 90% recall with best precision
    roc_df %>%
      filter(TPR >= 0.90) %>%
      filter(precision == max(precision)) %>%
      slice(1)
    
  } else if (criterion == "recall_80") {
    # Find threshold that gives at least 80% recall with best precision
    roc_df %>%
      filter(TPR >= 0.80) %>%
      filter(precision == max(precision)) %>%
      slice(1)
    
  } else if (criterion == "precision_50") {
    # Find threshold that gives at least 50% precision with best recall
    roc_df %>%
      filter(precision >= 0.50) %>%
      filter(TPR == max(TPR)) %>%
      slice(1)
    
  } else if (criterion == "cost") {
    # Minimize total cost = fp_cost * FP + fn_cost * FN
    roc_df %>%
      mutate(total_cost = fp_cost * FP + fn_cost * FN) %>%
      filter(total_cost == min(total_cost)) %>%
      slice(1)
    
  } else {
    stop("Unknown criterion: ", criterion)
  }
}

#' Run full threshold analysis
#' @param merged_df Merged confidence + stability data
#' @return List with ROC data, optimal thresholds, and summary
threshold_analysis <- function(merged_df) {
  
  cat("\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
  cat("THRESHOLD OPTIMIZATION ANALYSIS\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
  
  # Calculate ROC for top1_prob
  cat("Calculating ROC curve for top1_prob...\n")
  roc_top1 <- calculate_roc(merged_df, "mean_top1_prob")
  auc_top1 <- calculate_auc(roc_top1)
  cat(sprintf("AUC (Top-1 Probability): %.3f\n", auc_top1))
  
  # Calculate ROC for margin
  cat("Calculating ROC curve for margin...\n")
  roc_margin <- calculate_roc(merged_df, "mean_margin")
  auc_margin <- calculate_auc(roc_margin)
  cat(sprintf("AUC (Margin): %.3f\n", auc_margin))
  
  # Calculate ROC for entropy (note: HIGH entropy = unstable, so invert)
  cat("Calculating ROC curve for entropy...\n")
  # For entropy, we predict unstable if entropy > threshold (opposite direction)
  merged_inv <- merged_df %>%
    mutate(inv_entropy = 1 - mean_entropy)  # Invert so low = uncertain
  roc_entropy <- calculate_roc(merged_inv, "inv_entropy")
  auc_entropy <- calculate_auc(roc_entropy)
  cat(sprintf("AUC (Entropy): %.3f\n\n", auc_entropy))
  
  # Find optimal thresholds
  cat("OPTIMAL THRESHOLDS (using Top-1 Probability):\n\n")
  
  criteria <- c("youden", "f1", "recall_90", "recall_80")
  optimal_thresholds <- list()
  
  for (crit in criteria) {
    opt <- find_optimal_threshold(roc_top1, crit)
    optimal_thresholds[[crit]] <- opt
    
    cat(sprintf("%s:\n", toupper(crit)))
    cat(sprintf("  Threshold: %.3f (flag if top1_prob < %.3f)\n", opt$threshold, opt$threshold))
    cat(sprintf("  Recall: %.1f%% (catch this %% of unstable)\n", opt$TPR * 100))
    cat(sprintf("  Precision: %.1f%% (of flagged, this %% are unstable)\n", opt$precision * 100))
    cat(sprintf("  Flagged for review: %.1f%% of all instances\n", opt$pct_flagged))
    cat(sprintf("  F1: %.3f\n\n", opt$F1))
  }
  
  # Cost-benefit summary
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
  cat("PRACTICAL RECOMMENDATIONS\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
  
  n_total <- nrow(merged_df)
  n_unstable <- sum(!merged_df$is_stable)
  base_rate <- n_unstable / n_total
  
  cat(sprintf("Base rate of instability: %.1f%% (%d / %d)\n\n", 
              base_rate * 100, n_unstable, n_total))
  
  # Compare strategies
  cat("Strategy Comparison:\n\n")
  
  # Strategy 1: No filtering (accept all as-is)
  cat("1. ACCEPT ALL (no multi-run verification):\n")
  cat(sprintf("   - Cost: 0 extra API calls\n"))
  cat(sprintf("   - Unstable classifications: %d (%.1f%%)\n\n", n_unstable, base_rate * 100))
  
  # Strategy 2: Verify everything (50 runs)
  cat("2. VERIFY ALL (run everything 50x):\n")
  cat(sprintf("   - Cost: 49 extra runs × %d instances = %d extra API calls\n", 
              n_total, 49 * n_total))
  cat(sprintf("   - Catches: 100%% of unstable\n\n"))
  
  # Strategy 3: Selective verification using threshold
  opt_80 <- optimal_thresholds$recall_80
  n_flagged_80 <- round(n_total * opt_80$pct_flagged / 100)
  n_caught_80 <- round(n_unstable * opt_80$TPR)
  
  cat("3. SELECTIVE (flag low-confidence for verification):\n")
  cat(sprintf("   Threshold: top1_prob < %.3f\n", opt_80$threshold))
  cat(sprintf("   - Flagged: %d instances (%.1f%%)\n", n_flagged_80, opt_80$pct_flagged))
  cat(sprintf("   - Extra cost: 49 runs × %d = %d extra API calls\n", 
              n_flagged_80, 49 * n_flagged_80))
  cat(sprintf("   - Catches: %.0f%% of unstable (%d / %d)\n", 
              opt_80$TPR * 100, n_caught_80, n_unstable))
  cat(sprintf("   - Cost reduction: %.0f%% vs verify-all\n\n", 
              (1 - n_flagged_80 / n_total) * 100))
  
  invisible(list(
    roc_top1 = roc_top1,
    roc_margin = roc_margin,
    roc_entropy = roc_entropy,
    auc = c(top1 = auc_top1, margin = auc_margin, entropy = auc_entropy),
    optimal = optimal_thresholds
  ))
}

#' Plot ROC curve (requires ggplot2)
#' @param roc_df Output from calculate_roc()
#' @param title Plot title
plot_roc <- function(roc_df, title = "ROC Curve: Predicting Classification Instability") {
  
  auc <- calculate_auc(roc_df)
  
  ggplot(roc_df, aes(x = FPR, y = TPR)) +
    geom_line(color = "steelblue", size = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    annotate("text", x = 0.7, y = 0.2, 
             label = sprintf("AUC = %.3f", auc), size = 5) +
    labs(
      title = title,
      x = "False Positive Rate (1 - Specificity)",
      y = "True Positive Rate (Sensitivity/Recall)"
    ) +
    theme_minimal() +
    coord_equal()
}

#' Plot precision-recall curve
#' @param roc_df Output from calculate_roc()
plot_precision_recall <- function(roc_df, title = "Precision-Recall Curve") {
  
  ggplot(roc_df %>% filter(TPR > 0), aes(x = TPR, y = precision)) +
    geom_line(color = "darkgreen", size = 1) +
    geom_point(data = roc_df %>% filter(F1 == max(F1)), 
               aes(x = TPR, y = precision), color = "red", size = 3) +
    labs(
      title = title,
      x = "Recall (True Positive Rate)",
      y = "Precision"
    ) +
    theme_minimal() +
    xlim(0, 1) + ylim(0, 1)
}

#' Analyze confidence bins and flip rates
#' @param merged_df Merged confidence + stability data
#' @param n_bins Number of bins (default 10 for deciles)
#' @param conf_col Confidence column to use
bin_analysis <- function(merged_df, n_bins = 10, conf_col = "mean_top1_prob") {
  
  cat("\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
  cat("CONFIDENCE BIN ANALYSIS\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
  
  # Create bins
  binned <- merged_df %>%
    filter(!is.na(.data[[conf_col]])) %>%
    mutate(
      conf_bin = ntile(.data[[conf_col]], n_bins),
      conf_bin_label = sprintf("Q%d", conf_bin)
    )
  
  # Summarize by bin
  bin_summary <- binned %>%
    group_by(conf_bin) %>%
    summarise(
      n = n(),
      conf_min = min(.data[[conf_col]], na.rm = TRUE),
      conf_max = max(.data[[conf_col]], na.rm = TRUE),
      conf_mean = mean(.data[[conf_col]], na.rm = TRUE),
      n_unstable = sum(!is_stable),
      pct_unstable = mean(!is_stable) * 100,
      mean_flip_rate = mean(flip_rate, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    mutate(
      conf_range = sprintf("[%.2f-%.2f]", conf_min, conf_max)
    )
  
  cat(sprintf("Binned by %s into %d groups:\n\n", conf_col, n_bins))
  
  print(bin_summary %>% 
          select(conf_bin, conf_range, n, n_unstable, pct_unstable, mean_flip_rate) %>%
          rename(
            Bin = conf_bin,
            `Conf Range` = conf_range,
            N = n,
            `N Unstable` = n_unstable,
            `% Unstable` = pct_unstable,
            `Mean Flip %` = mean_flip_rate
          ))
  
  # Test for trend
  cat("\n")
  cor_test <- cor.test(binned[[conf_col]], binned$flip_rate, method = "spearman")
  cat(sprintf("Trend test (Spearman): r = %.3f, p = %.2e\n", 
              cor_test$estimate, cor_test$p.value))
  
  invisible(bin_summary)
}

# =============================================================================
# USAGE
# =============================================================================

cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("CONFIDENCE ANALYSIS MODULE LOADED\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

cat("STEP 1: Run stability analysis first (if not already done):\n")
cat("  source('stability_analysis.R')\n")
cat("  results <- run_stability_analysis()  # Caches automatically\n\n")

cat("STEP 2: Diagnose logprobs structure (recommended first time):\n")
cat("  diagnose_logprobs(results$parsed)\n\n")

cat("STEP 3: Run confidence analysis:\n")
cat("  source('confidence_analysis.R')\n")
cat("  conf_results <- run_confidence_analysis(results)  # Caches automatically\n\n")

cat("STEP 4: Threshold optimization:\n")
cat("  thresh <- threshold_analysis(conf_results$merged)\n")
cat("  bin_analysis(conf_results$merged)  # Decile breakdown\n\n")

cat("STEP 5: Visualization (optional):\n")
cat("  plot_roc(thresh$roc_top1)\n")
cat("  plot_precision_recall(thresh$roc_top1)\n\n")

cat("Objects returned:\n")
cat("  conf_results$confidence_raw  - Per-observation confidence (all runs)\n")
cat("  conf_results$confidence_agg  - Aggregated by classification unit\n")
cat("  conf_results$merged          - Merged with stability metrics\n\n")

cat("Threshold analysis returns:\n")
cat("  thresh$roc_top1    - ROC data for top1_prob\n")
cat("  thresh$auc         - AUC values\n")
cat("  thresh$optimal     - Optimal thresholds by criterion\n\n")

cat("Note: Results are cached. Use force_reload=TRUE to refresh.\n")
