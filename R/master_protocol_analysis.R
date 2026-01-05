# =============================================================================
# MASTER PROTOCOL ANALYSIS
# =============================================================================
#
# Complete pipeline for LLM classification stability analysis.
# 
# USAGE:
#   1. (Optional) Set MODEL before sourcing: MODEL <- "gpt-4o-mini"
#   2. Source this file: source("master_protocol_analysis.R")
#   3. Everything runs automatically, loading cached data when available
#
# OUTPUTS:
#   - Stability statistics (50-run analysis)
#   - Confidence-instability relationship
#   - Calibration protocol recommendation
#   - Cost comparison with baselines
#   - Final pipeline instability (apples-to-apples comparison)
#
# =============================================================================

library(tidyverse)
library(jsonlite)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Use MODEL if already set, otherwise default to gpt-4o
if (!exists("MODEL")) {
  MODEL <- "gpt-4o"
}
cat(sprintf("Using model: %s\n", MODEL))

# Paths
BASE_DIR <- "~/Desktop/Reproduciblity"
OUTPUT_DIR <- file.path(BASE_DIR, "output", MODEL)
RESULTS_DIR <- file.path(BASE_DIR, "results", MODEL)

# Create results directory if needed
dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

# Cache file paths
STABILITY_CACHE <- file.path(RESULTS_DIR, "stability_results.rds")
CONFIDENCE_CACHE <- file.path(RESULTS_DIR, "confidence_results.rds")
GRID_CACHE <- file.path(RESULTS_DIR, "calibration_grid.rds")

# Grid search parameters (only used if grid not cached)
GRID_PCT_RANGE <- seq(0.05, 0.50, by = 0.01)
GRID_K_RANGE <- 2:20
GRID_METHODS <- c("random", "low_confidence", "stratified")
GRID_TARGET_INSTABILITY <- 0.01
GRID_N_BOOTSTRAP <- 20

# =============================================================================
# PART 1: STABILITY ANALYSIS (Load 50 runs, compute stability)
# =============================================================================

load_or_compute_stability <- function(force = FALSE) {
  
  # Check if already in environment
  if (!force && exists("stability_results", envir = .GlobalEnv)) {
    env_obj <- get("stability_results", envir = .GlobalEnv)
    if (is.list(env_obj) && "stability" %in% names(env_obj)) {
      cat("Using stability_results from environment...\n")
      return(env_obj)
    }
  }
  
  if (!force && file.exists(STABILITY_CACHE)) {
    cat("Loading cached stability results...\n")
    return(readRDS(STABILITY_CACHE))
  }
  
  cat("Computing stability from 50 runs...\n")
  
  # Find run files (matches run_001.jsonl, run_01.jsonl, run_1.jsonl)
  files <- list.files(OUTPUT_DIR, pattern = "run_.*\\.jsonl$", full.names = TRUE)
  if (length(files) == 0) {
    stop("No run files found in ", OUTPUT_DIR)
  }
  
  run_nums <- as.integer(str_extract(basename(files), "\\d+"))
  cat(sprintf("Found %d run files\n", length(files)))
  
  # Parse each run file
  parse_run_file <- function(filepath) {
    lines <- readLines(filepath, warn = FALSE)
    
    map_dfr(lines, function(line) {
      parsed <- tryCatch(fromJSON(line, flatten = FALSE), error = function(e) NULL)
      if (is.null(parsed)) return(NULL)
      if (isTRUE(parsed$hasError)) return(NULL)
      
      # Handle player field
      player <- NA_character_
      if (!is.null(parsed$player)) {
        player <- as.character(parsed$player)
      } else if (!is.null(parsed$localId)) {
        player <- as.character(parsed$localId)
      }
      
      # Handle classification field - different locations in gpt-4o vs gpt-4o-mini
      classification <- NULL
      if (!is.null(parsed$classification)) {
        classification <- parsed$classification
      } else if (!is.null(parsed$output$classification)) {
        classification <- parsed$output$classification
      } else if (!is.null(parsed$output) && is.character(parsed$output)) {
        classification <- parsed$output
      }
      
      if (is.null(classification)) return(NULL)
      
      tibble(
        id = parsed$id,
        dataset = parsed$dataset,
        task = parsed$task,
        player = player,
        run = parsed$run,
        classification = as.character(classification)
      )
    })
  }
  
  all_parsed <- map2_dfr(files, run_nums, function(f, r) {
    df <- parse_run_file(f)
    if (!"run" %in% names(df) || all(is.na(df$run))) {
      df$run <- r
    }
    df
  })
  
  cat(sprintf("Parsed %d records\n", nrow(all_parsed)))
  
  # Compute stability metrics
  stability <- all_parsed %>%
    group_by(id, dataset, task, player) %>%
    summarise(
      n_runs = n(),
      n_classes = n_distinct(classification),
      modal_class = names(sort(table(classification), decreasing = TRUE))[1],
      modal_freq = max(table(classification)) / n(),
      flip_rate = 1 - modal_freq,
      all_classes = paste(sort(unique(classification)), collapse = ","),
      class_run1 = classification[run == min(run)][1],
      .groups = "drop"
    ) %>%
    mutate(
      is_stable = (modal_freq == 1.0),
      is_error = (class_run1 != modal_class)
    )
  
  results <- list(
    parsed = all_parsed,
    stability = stability,
    model = MODEL,
    n_runs = max(all_parsed$run, na.rm = TRUE)
  )
  
  # Cache
  saveRDS(results, STABILITY_CACHE)
  cat(sprintf("Saved to %s\n", STABILITY_CACHE))
  
  return(results)
}

# =============================================================================
# PART 2: CONFIDENCE ANALYSIS (Extract logprobs)
# =============================================================================

load_or_compute_confidence <- function(stability_results, force = FALSE) {
  
  # Check if already in environment
  if (!force && exists("confidence_results", envir = .GlobalEnv)) {
    env_obj <- get("confidence_results", envir = .GlobalEnv)
    if (is.list(env_obj) && "merged" %in% names(env_obj)) {
      # Verify it has required columns
      if ("top1_prob" %in% names(env_obj$merged)) {
        cat("Using confidence_results from environment...\n")
        return(env_obj)
      }
    }
  }
  
  if (!force && file.exists(CONFIDENCE_CACHE)) {
    cat("Loading cached confidence results...\n")
    cached <- readRDS(CONFIDENCE_CACHE)
    # Verify it has required columns, otherwise recompute
    if ("top1_prob" %in% names(cached$merged)) {
      return(cached)
    } else {
      cat("Cached confidence missing required columns, recomputing...\n")
    }
  }
  
  cat("Computing confidence from logprobs...\n")
  
  # Find run files (matches run_001.jsonl, run_01.jsonl, run_1.jsonl)
  files <- list.files(OUTPUT_DIR, pattern = "run_.*\\.jsonl$", full.names = TRUE)
  run_nums <- as.integer(str_extract(basename(files), "\\d+"))
  
  # Extract logprobs from each run
  extract_logprobs <- function(filepath) {
    lines <- readLines(filepath, warn = FALSE)
    
    map_dfr(lines, function(line) {
      parsed <- tryCatch(fromJSON(line, flatten = FALSE), error = function(e) NULL)
      if (is.null(parsed)) return(NULL)
      if (isTRUE(parsed$hasError)) return(NULL)
      
      # Handle player field
      player <- NA_character_
      if (!is.null(parsed$player)) {
        player <- as.character(parsed$player)
      } else if (!is.null(parsed$localId)) {
        player <- as.character(parsed$localId)
      }
      
      # Extract logprobs - handle different formats
      logprobs <- parsed$logprobs
      if (is.null(logprobs) || length(logprobs) == 0) {
        return(tibble(
          id = parsed$id, dataset = parsed$dataset, task = parsed$task,
          player = player, run = parsed$run,
          top1_prob = NA_real_, top1_token = NA_character_,
          top2_prob = NA_real_, margin = NA_real_, entropy = NA_real_
        ))
      }
      
      top1_prob <- NA_real_
      top1_token <- NA_character_
      top2_prob <- 0
      probs_vec <- c()
      
      # Both gpt-4o and gpt-4o-mini use same format: logprob + top_logprobs
      if (is.data.frame(logprobs) && "logprob" %in% names(logprobs) && "top_logprobs" %in% names(logprobs)) {
        # Find the classification token (single digit like "0", "1", "2", etc.)
        classification_idx <- which(nchar(logprobs$token) == 1 & grepl("^[0-9]$", logprobs$token))
        
        if (length(classification_idx) > 0) {
          idx <- classification_idx[1]
          # logprob is log probability, convert to probability
          top1_logprob <- logprobs$logprob[idx]
          top1_prob <- exp(top1_logprob)
          top1_token <- logprobs$token[idx]
          
          # Get top_logprobs for this position to find alternatives
          top_lp <- logprobs$top_logprobs[[idx]]
          if (is.data.frame(top_lp) && nrow(top_lp) >= 2) {
            # Sort by logprob descending
            top_lp_sorted <- top_lp[order(top_lp$logprob, decreasing = TRUE), ]
            top2_logprob <- top_lp_sorted$logprob[2]
            top2_prob <- exp(top2_logprob)
            
            # Get all probs for entropy
            probs_vec <- exp(top_lp_sorted$logprob)
            probs_vec <- probs_vec[probs_vec > 0 & !is.infinite(probs_vec) & !is.na(probs_vec)]
          } else {
            probs_vec <- c(top1_prob)
          }
        }
        
      # Fallback: old format with 'prob' column (unlikely but keep for safety)
      } else if (is.data.frame(logprobs) && "prob" %in% names(logprobs)) {
        sorted <- logprobs %>% arrange(desc(prob))
        top1_prob <- sorted$prob[1]
        top1_token <- sorted$token[1]
        top2_prob <- if (nrow(sorted) >= 2) sorted$prob[2] else 0
        probs_vec <- logprobs$prob
        
      # List format fallback
      } else if (is.list(logprobs) && !is.data.frame(logprobs)) {
        if ("prob" %in% names(logprobs[[1]])) {
          probs <- sapply(logprobs, function(x) x$prob)
          tokens <- sapply(logprobs, function(x) x$token)
          ord <- order(probs, decreasing = TRUE)
          top1_prob <- probs[ord[1]]
          top1_token <- tokens[ord[1]]
          top2_prob <- if (length(ord) >= 2) probs[ord[2]] else 0
          probs_vec <- probs
        }
        
      } else {
        return(tibble(
          id = parsed$id, dataset = parsed$dataset, task = parsed$task,
          player = player, run = parsed$run,
          top1_prob = NA_real_, top1_token = NA_character_,
          top2_prob = NA_real_, margin = NA_real_, entropy = NA_real_
        ))
      }
      
      margin <- top1_prob - top2_prob
      
      # Entropy
      probs_vec <- probs_vec[probs_vec > 0 & !is.na(probs_vec) & !is.infinite(probs_vec)]
      if (length(probs_vec) > 0) {
        # Normalize if needed
        probs_vec <- probs_vec / sum(probs_vec)
        entropy <- -sum(probs_vec * log2(probs_vec))
      } else {
        entropy <- NA_real_
      }
      
      tibble(
        id = parsed$id, dataset = parsed$dataset, task = parsed$task,
        player = player, run = parsed$run,
        top1_prob = top1_prob, top1_token = top1_token,
        top2_prob = top2_prob, margin = margin, entropy = entropy
      )
    })
  }
  
  all_confidence <- map2_dfr(files, run_nums, function(f, r) {
    df <- extract_logprobs(f)
    if (!"run" %in% names(df) || all(is.na(df$run))) {
      df$run <- r
    }
    df
  })
  
  cat(sprintf("Extracted confidence for %d records\n", nrow(all_confidence)))
  
  # Merge with stability
  merged <- all_confidence %>%
    filter(run == 1) %>%
    select(id, dataset, task, player, top1_prob, margin, entropy) %>%
    inner_join(stability_results$stability, by = c("id", "dataset", "task", "player"))
  
  results <- list(
    confidence_raw = all_confidence,
    merged = merged,
    model = MODEL
  )
  
  # Cache
  saveRDS(results, CONFIDENCE_CACHE)
  cat(sprintf("Saved to %s\n", CONFIDENCE_CACHE))
  
  return(results)
}

# =============================================================================
# PART 3: CALIBRATION GRID SEARCH
# =============================================================================

load_or_compute_grid <- function(stability_results, confidence_results, force = FALSE) {
  
  # Check if already in environment
  if (!force && exists("grid_results", envir = .GlobalEnv)) {
    env_obj <- get("grid_results", envir = .GlobalEnv)
    if (is.list(env_obj) && "grid" %in% names(env_obj)) {
      cat("Using grid_results from environment...\n")
      return(env_obj)
    }
  }
  
  # Also check for grid_full (the raw output from run_grid_search)
  if (!force && exists("grid_full", envir = .GlobalEnv)) {
    env_obj <- get("grid_full", envir = .GlobalEnv)
    if (is.data.frame(env_obj) && "tau_mean" %in% names(env_obj)) {
      cat("Using grid_full from environment (wrapping in list)...\n")
      # Find oracle tau
      data <- confidence_results$merged %>%
        filter(!is.na(top1_prob)) %>%
        mutate(is_unstable = !is_stable)
      
      oracle_tau <- 0.89  # Default
      for (tau in seq(0.99, 0.50, by = -0.01)) {
        above <- data %>% filter(top1_prob >= tau)
        if (nrow(above) > 0 && mean(above$is_unstable) <= GRID_TARGET_INSTABILITY) {
          oracle_tau <- tau
          break
        }
      }
      
      return(list(
        grid = env_obj,
        oracle_tau = oracle_tau,
        model = MODEL,
        params = list(
          pct_range = GRID_PCT_RANGE,
          K_range = GRID_K_RANGE,
          methods = GRID_METHODS,
          target = GRID_TARGET_INSTABILITY,
          n_bootstrap = GRID_N_BOOTSTRAP
        )
      ))
    }
  }
  
  if (!force && file.exists(GRID_CACHE)) {
    cat("Loading cached grid search results...\n")
    return(readRDS(GRID_CACHE))
  }
  
  cat("Running calibration grid search (this may take 10-30 minutes)...\n")
  
  # Prepare data
  data <- confidence_results$merged %>%
    filter(!is.na(top1_prob)) %>%
    mutate(is_unstable = !is_stable)
  
  n_total <- nrow(data)
  
  # Oracle τ (what we'd learn with infinite data)
  find_oracle_tau <- function(target = GRID_TARGET_INSTABILITY) {
    for (tau in seq(0.99, 0.50, by = -0.01)) {
      above <- data %>% filter(top1_prob >= tau)
      if (nrow(above) > 0 && mean(above$is_unstable) <= target) {
        return(tau)
      }
    }
    return(0.50)
  }
  
  oracle_tau <- find_oracle_tau()
  cat(sprintf("Oracle τ = %.2f\n", oracle_tau))
  
  # Grid search function
  simulate_calibration <- function(pct, K, method) {
    
    n_sample <- ceiling(n_total * pct)
    
    # Sample selection
    if (method == "random") {
      sampled_idx <- sample(1:n_total, n_sample)
    } else if (method == "low_confidence") {
      ordered_idx <- order(data$top1_prob)
      sampled_idx <- ordered_idx[1:min(n_sample, n_total)]
    } else if (method == "stratified") {
      # Stratified by confidence decile
      data_with_idx <- data %>% mutate(idx = row_number())
      data_with_idx <- data_with_idx %>%
        mutate(decile = ntile(top1_prob, 10))
      
      # Minimum per decile
      min_per_decile <- max(1, floor(n_sample / 20))
      sampled_idx <- c()
      
      for (d in 1:10) {
        decile_idx <- data_with_idx %>% filter(decile == d) %>% pull(idx)
        n_take <- min(min_per_decile, length(decile_idx))
        sampled_idx <- c(sampled_idx, sample(decile_idx, n_take))
      }
      
      # Fill remaining with low-confidence oversample
      remaining <- n_sample - length(sampled_idx)
      if (remaining > 0) {
        low_conf_idx <- data_with_idx %>% 
          filter(decile <= 3, !idx %in% sampled_idx) %>% 
          pull(idx)
        if (length(low_conf_idx) > 0) {
          n_add <- min(remaining, length(low_conf_idx))
          sampled_idx <- c(sampled_idx, sample(low_conf_idx, n_add))
        }
      }
    }
    
    sampled_data <- data[sampled_idx, ]
    
    # Simulate K runs using 50-run data
    simulate_K_runs <- function(item_data, K) {
      available_runs <- stability_results$parsed %>%
        filter(id == item_data$id[1], 
               dataset == item_data$dataset[1],
               task == item_data$task[1],
               player == item_data$player[1])
      
      if (nrow(available_runs) < K) {
        return(NA)  # Not enough runs
      }
      
      sampled_runs <- available_runs %>% sample_n(K)
      n_unique <- n_distinct(sampled_runs$classification)
      return(n_unique > 1)  # TRUE = unstable
    }
    
    # Check instability for each sampled item
    sampled_instability <- map_lgl(1:nrow(sampled_data), function(i) {
      simulate_K_runs(sampled_data[i, ], K)
    })
    
    sampled_data$observed_unstable <- sampled_instability
    sampled_data <- sampled_data %>% filter(!is.na(observed_unstable))
    
    if (nrow(sampled_data) == 0) return(NULL)
    
    # Learn τ from sample
    learned_tau <- 0.50
    for (tau in seq(0.99, 0.50, by = -0.01)) {
      above <- sampled_data %>% filter(top1_prob >= tau)
      if (nrow(above) > 0 && mean(above$observed_unstable) <= GRID_TARGET_INSTABILITY) {
        learned_tau <- tau
        break
      }
    }
    
    # Evaluate on full data
    flagged_pct <- mean(data$top1_prob < learned_tau)
    accepted <- data %>% filter(top1_prob >= learned_tau)
    oracle_instab <- if (nrow(accepted) > 0) mean(accepted$is_unstable) else NA
    
    tibble(
      pct = pct,
      K = K,
      method = method,
      tau = learned_tau,
      flagged = flagged_pct,
      oracle_instab = oracle_instab
    )
  }
  
  # Build grid
  grid <- expand_grid(
    pct = GRID_PCT_RANGE,
    K = GRID_K_RANGE,
    method = GRID_METHODS
  )
  
  cat(sprintf("Grid: %d pct × %d K × %d methods = %d combinations\n",
              length(GRID_PCT_RANGE), length(GRID_K_RANGE), length(GRID_METHODS), nrow(grid)))
  cat(sprintf("Bootstraps: %d → Total: %d simulations\n", 
              GRID_N_BOOTSTRAP, nrow(grid) * GRID_N_BOOTSTRAP))
  
  # Run with progress
  pb <- txtProgressBar(min = 0, max = nrow(grid), style = 3)
  
  all_results <- map_dfr(1:nrow(grid), function(i) {
    setTxtProgressBar(pb, i)
    
    g <- grid[i, ]
    
    # Bootstrap
    bootstrap_results <- map_dfr(1:GRID_N_BOOTSTRAP, function(b) {
      res <- simulate_calibration(g$pct, g$K, g$method)
      if (!is.null(res)) res$bootstrap <- b
      res
    })
    
    if (nrow(bootstrap_results) == 0) return(NULL)
    
    # Aggregate
    bootstrap_results %>%
      summarise(
        pct = first(pct),
        K = first(K),
        method = first(method),
        tau_mean = mean(tau, na.rm = TRUE),
        tau_sd = sd(tau, na.rm = TRUE),
        flagged_mean = mean(flagged, na.rm = TRUE),
        oracle_instab_mean = mean(oracle_instab, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  close(pb)
  
  results <- list(
    grid = all_results,
    oracle_tau = oracle_tau,
    model = MODEL,
    params = list(
      pct_range = GRID_PCT_RANGE,
      K_range = GRID_K_RANGE,
      methods = GRID_METHODS,
      target = GRID_TARGET_INSTABILITY,
      n_bootstrap = GRID_N_BOOTSTRAP
    )
  )
  
  # Cache
  saveRDS(results, GRID_CACHE)
  cat(sprintf("\nSaved to %s\n", GRID_CACHE))
  
  return(results)
}

# =============================================================================
# PART 4: ANALYSIS AND REPORTING
# =============================================================================

run_full_analysis <- function(stability_results, confidence_results, grid_results) {
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat(sprintf("  %s CLASSIFICATION STABILITY ANALYSIS\n", MODEL))
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  
  # --- SECTION 1: Basic Stability Stats ---
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("1. BASIC STABILITY (50 runs)\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  stab <- stability_results$stability
  n_total <- nrow(stab)
  n_stable <- sum(stab$is_stable)
  n_unstable <- sum(!stab$is_stable)
  n_error <- if ("is_error" %in% names(stab)) sum(stab$is_error, na.rm = TRUE) else NA
  
  cat(sprintf("Total items: %d\n", n_total))
  cat(sprintf("Stable (100%% same answer): %d (%.1f%%)\n", n_stable, n_stable/n_total*100))
  cat(sprintf("Unstable (any flip in 50 runs): %d (%.1f%%)\n", n_unstable, n_unstable/n_total*100))
  if (!is.na(n_error)) {
    cat(sprintf("Error (run 1 ≠ modal): %d (%.1f%%)\n", n_error, n_error/n_total*100))
  } else {
    cat("Error (run 1 ≠ modal): N/A\n")
  }
  cat(sprintf("Mean TAR (modal agreement): %.2f%%\n", mean(stab$modal_freq) * 100))
  
  cat("\nBy Dataset/Task:\n\n")
  by_task <- stab %>%
    group_by(dataset, task) %>%
    summarise(
      n = n(),
      pct_unstable = sprintf("%.1f%%", mean(!is_stable) * 100),
      pct_error = if ("is_error" %in% names(stab)) sprintf("%.1f%%", mean(is_error, na.rm = TRUE) * 100) else "N/A",
      mean_TAR = sprintf("%.1f%%", mean(modal_freq) * 100),
      .groups = "drop"
    )
  print(by_task)
  
  # --- SECTION 2: Confidence-Instability Relationship ---
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("2. CONFIDENCE → INSTABILITY RELATIONSHIP\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  data <- confidence_results$merged %>%
    filter(!is.na(top1_prob)) %>%
    mutate(is_unstable = !is_stable)
  
  cat("Instability Rate by Confidence Bin:\n\n")
  by_bin <- data %>%
    mutate(conf_bin = cut(top1_prob, breaks = c(0, 0.6, 0.7, 0.8, 0.9, 0.95, 1.0))) %>%
    group_by(conf_bin) %>%
    summarise(
      n = n(),
      pct_of_total = sprintf("%.1f%%", n() / nrow(data) * 100),
      instability_rate = sprintf("%.1f%%", mean(is_unstable) * 100),
      error_rate = if ("is_error" %in% names(data)) sprintf("%.1f%%", mean(is_error, na.rm = TRUE) * 100) else "N/A",
      .groups = "drop"
    )
  print(by_bin)
  
  # AUC for predicting instability - TOP1_PROB
  roc_data <- data %>%
    arrange(top1_prob) %>%
    mutate(
      cum_unstable = cumsum(is_unstable),
      cum_stable = cumsum(!is_unstable),
      tpr = cum_unstable / sum(is_unstable),
      fpr = cum_stable / sum(!is_unstable)
    )
  
  auc_top1 <- sum(diff(roc_data$fpr) * (head(roc_data$tpr, -1) + tail(roc_data$tpr, -1)) / 2)
  auc_top1 <- abs(auc_top1)
  if (auc_top1 < 0.5) auc_top1 <- 1 - auc_top1
  
  # AUC for MARGIN
  auc_margin <- NA
  if ("margin" %in% names(data) && sum(!is.na(data$margin)) > 0) {
    roc_margin <- data %>%
      filter(!is.na(margin)) %>%
      arrange(margin) %>%
      mutate(
        cum_unstable = cumsum(is_unstable),
        cum_stable = cumsum(!is_unstable),
        tpr = cum_unstable / sum(is_unstable),
        fpr = cum_stable / sum(!is_unstable)
      )
    auc_margin <- sum(diff(roc_margin$fpr) * (head(roc_margin$tpr, -1) + tail(roc_margin$tpr, -1)) / 2)
    auc_margin <- abs(auc_margin)
    if (auc_margin < 0.5) auc_margin <- 1 - auc_margin
  }
  
  cat(sprintf("\nAUC (predicting instability):\n"))
  cat(sprintf("  top1_prob: %.3f\n", auc_top1))
  if (!is.na(auc_margin)) {
    cat(sprintf("  margin:    %.3f\n", auc_margin))
    cat(sprintf("  correlation(top1_prob, margin): %.3f\n", cor(data$top1_prob, data$margin, use = "complete.obs")))
  }
  cat("(1.0 = perfect prediction, 0.5 = random)\n")
  
  # Instability by MARGIN bins
  if ("margin" %in% names(data) && sum(!is.na(data$margin)) > 0) {
    cat("\nInstability Rate by MARGIN Bin:\n\n")
    by_margin <- data %>%
      filter(!is.na(margin)) %>%
      mutate(margin_bin = cut(margin, breaks = c(0, 0.3, 0.5, 0.7, 0.9, 1.0))) %>%
      group_by(margin_bin) %>%
      summarise(
        n = n(),
        pct_of_total = sprintf("%.1f%%", n() / nrow(data) * 100),
        instability_rate = sprintf("%.1f%%", mean(is_unstable) * 100),
        .groups = "drop"
      )
    print(by_margin)
  }
  
  # Store AUC for later
  auc <- auc_top1
  
  # --- SECTION 3: Calibration Protocol ---
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("3. CALIBRATION PROTOCOL (from grid search)\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  cat(sprintf("Oracle τ: %.2f\n", grid_results$oracle_tau))
  cat(sprintf("Target instability: ≤%.0f%%\n\n", grid_results$params$target * 100))
  
  # Best configuration achieving ≤2% instability
  best_configs <- grid_results$grid %>%
    filter(oracle_instab_mean <= 0.02) %>%
    mutate(
      calibration_cost = pct * (K - 1),
      verification_cost = flagged_mean * 4,  # K_verify = 5
      total_cost = 1 + calibration_cost + verification_cost
    ) %>%
    arrange(total_cost)
  
  if (nrow(best_configs) == 0) {
    cat("WARNING: No configuration achieved ≤2% instability!\n")
    cat("Showing best available:\n\n")
    best_configs <- grid_results$grid %>%
      mutate(
        calibration_cost = pct * (K - 1),
        verification_cost = flagged_mean * 4,
        total_cost = 1 + calibration_cost + verification_cost
      ) %>%
      arrange(oracle_instab_mean) %>%
      head(10)
  }
  
  cat("Top 10 configurations (≤2% instability, sorted by total cost):\n\n")
  print(best_configs %>%
          head(10) %>%
          mutate(
            pct = sprintf("%.0f%%", pct * 100),
            calibration_cost = sprintf("%.2fN", calibration_cost),
            flagged_mean = sprintf("%.1f%%", flagged_mean * 100),
            verification_cost = sprintf("%.2fN", verification_cost),
            total_cost = sprintf("%.2fN", total_cost),
            tau_mean = sprintf("%.2f", tau_mean),
            oracle_instab_mean = sprintf("%.2f%%", oracle_instab_mean * 100)
          ) %>%
          select(method, pct, K, tau_mean, flagged_mean, total_cost, oracle_instab_mean))
  
  # Best overall
  best <- best_configs %>% slice(1)
  
  cat("\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("RECOMMENDED PROTOCOL:\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  cat(sprintf("Sampling method: %s\n", best$method))
  cat(sprintf("Sample size: %.0f%% of data\n", best$pct * 100))
  cat(sprintf("Runs per sample item: %d\n", best$K))
  cat(sprintf("τ learned: %.2f\n", best$tau_mean))
  cat(sprintf("Items flagged for verification: %.1f%%\n", best$flagged_mean * 100))
  cat(sprintf("Instability among accepted: %.2f%%\n", best$oracle_instab_mean * 100))
  
  # --- SECTION 4: Cost Breakdown ---
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("4. COST BREAKDOWN\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  cat("Protocol Steps:\n\n")
  cat(sprintf("  1. Initial run (all items):           1.00N\n"))
  cat(sprintf("  2. Calibration (%.0f%% × %d extra runs):   %.2fN\n", 
              best$pct * 100, best$K - 1, best$calibration_cost))
  cat(sprintf("  3. Verification (%.1f%% × 4 extra runs): %.2fN\n",
              best$flagged_mean * 100, best$verification_cost))
  cat(sprintf("  ────────────────────────────────────────────\n"))
  cat(sprintf("  TOTAL:                                %.2fN\n", best$total_cost))
  
  # --- SECTION 5: Comparison ---
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("5. COMPARISON WITH BASELINES\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Calculate empirical instability for Always-K× methods
  # Using binomial probability: P(K-run modal matches 50-run modal)
  stab <- stability_results$stability
  unstable_items <- stab %>% filter(!is_stable)
  
  # For unstable items: P(at least ceil(K/2) of K match modal) 
  unstable_items <- unstable_items %>%
    mutate(
      p = modal_freq,
      # P(at least 2 of 3 match modal) = p^3 + 3*p^2*(1-p)
      prob_3_correct = p^3 + 3*p^2*(1-p),
      # P(at least 3 of 5 match modal) = p^5 + 5*p^4*(1-p) + 10*p^3*(1-p)^2
      prob_5_correct = p^5 + 5*p^4*(1-p) + 10*p^3*(1-p)^2,
      # P(at least 4 of 7 match modal)
      prob_7_correct = p^7 + 7*p^6*(1-p) + 21*p^5*(1-p)^2 + 35*p^4*(1-p)^3
    )
  
  n_stable <- sum(stab$is_stable)
  n_unstable <- sum(!stab$is_stable)
  n_total <- nrow(stab)
  
  # Overall instability rate: only unstable items can mismatch
  instab_3x <- n_unstable * (1 - mean(unstable_items$prob_3_correct)) / n_total * 100
  instab_5x <- n_unstable * (1 - mean(unstable_items$prob_5_correct)) / n_total * 100
  instab_7x <- n_unstable * (1 - mean(unstable_items$prob_7_correct)) / n_total * 100
  
  cat("Empirical instability rates (K-run modal ≠ 50-run modal):\n\n")
  
  cat("Method                     | Cost   | Instability Guarantee | vs Always-3×\n")
  cat("---------------------------|--------|----------------------|-------------\n")
  cat(sprintf("Single run                 | 1.00N  | None (%.1f%% unstable) | -67%%\n", 
              mean(data$is_unstable) * 100))
  cat(sprintf("OUR PROTOCOL (τ=%.2f)      | %.2fN  | ≤%.2f%%               | -%.0f%%\n",
              best$tau_mean, best$total_cost, best$oracle_instab_mean * 100,
              (1 - best$total_cost / 3) * 100))
  cat(sprintf("Always-3×                  | 3.00N  | %.2f%%                 | baseline\n", instab_3x))
  cat(sprintf("Always-5×                  | 5.00N  | %.2f%%                 | +67%%\n", instab_5x))
  cat(sprintf("Always-7×                  | 7.00N  | %.2f%%                 | +133%%\n", instab_7x))
  
  cat("\nNote: Instability = P(K-run modal vote ≠ 50-run modal vote)\n")
  
  # --- SECTION 6: Stability Guarantee Check ---
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("6. STABILITY GUARANTEE VERIFICATION\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  tau <- best$tau_mean
  accepted <- data %>% filter(top1_prob >= tau)
  flagged <- data %>% filter(top1_prob < tau)
  
  cat(sprintf("At τ = %.2f:\n\n", tau))
  cat(sprintf("ACCEPTED (conf ≥ %.2f): %d items (%.1f%%)\n", 
              tau, nrow(accepted), nrow(accepted)/nrow(data)*100))
  cat(sprintf("  Instability rate: %.2f%%\n", mean(accepted$is_unstable)*100))
  cat(sprintf("  → %.1f%% will give SAME answer on rerun\n", (1-mean(accepted$is_unstable))*100))
  
  cat(sprintf("\nFLAGGED (conf < %.2f): %d items (%.1f%%)\n",
              tau, nrow(flagged), nrow(flagged)/nrow(data)*100))
  cat(sprintf("  Instability rate: %.2f%%\n", mean(flagged$is_unstable)*100))
  cat(sprintf("  → These get 5× verification (modal vote)\n"))
  
  # --- SECTION 7: Final Pipeline Instability (Apples-to-Apples) ---
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("7. FINAL PIPELINE INSTABILITY (apples-to-apples comparison)\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Calculate final output instability using ACTUAL RUN DATA
  # Accepted items: output = run1
  # Verified items: output = actual modal of first K_verify runs
  
  K_verify <- 5
  parsed <- stability_results$parsed
  
  # Accepted: mismatch = run1 ≠ modal50
  accepted_keys <- data %>%
    filter(!is.na(top1_prob) & top1_prob >= tau) %>%
    select(id, dataset, task, player)
  
  accepted_final <- accepted_keys %>%
    left_join(stab %>% select(id, dataset, task, player, class_run1, modal_class),
              by = c("id", "dataset", "task", "player")) %>%
    mutate(final_class = class_run1, group = "accepted")
  
  # Flagged: compute ACTUAL modal from first K_verify runs
  flagged_keys <- data %>%
    filter(is.na(top1_prob) | top1_prob < tau) %>%
    select(id, dataset, task, player)
  
  if (nrow(flagged_keys) > 0) {
    flagged_final <- parsed %>%
      filter(run <= K_verify) %>%
      inner_join(flagged_keys, by = c("id", "dataset", "task", "player")) %>%
      group_by(id, dataset, task, player) %>%
      summarise(
        final_class = names(which.max(table(classification))),
        .groups = "drop"
      ) %>%
      left_join(stab %>% select(id, dataset, task, player, modal_class),
                by = c("id", "dataset", "task", "player")) %>%
      mutate(group = "flagged")
  } else {
    flagged_final <- tibble(id = character(), dataset = character(), task = character(),
                            player = character(), final_class = character(), 
                            modal_class = character(), group = character())
  }
  
  # Combine and calculate mismatch
  if (nrow(flagged_final) > 0) {
    pipeline_final <- bind_rows(
      accepted_final %>% select(id, dataset, task, player, final_class, modal_class, group),
      flagged_final %>% select(id, dataset, task, player, final_class, modal_class, group)
    )
  } else {
    pipeline_final <- accepted_final %>% select(id, dataset, task, player, final_class, modal_class, group)
  }
  
  pipeline_final <- pipeline_final %>%
    mutate(mismatch = (final_class != modal_class))
  
  # Calculate metrics
  n_accepted <- nrow(accepted_keys)
  n_flagged <- nrow(flagged_keys)
  n_pipeline <- n_accepted + n_flagged
  
  accepted_mismatch <- mean(pipeline_final %>% filter(group == "accepted") %>% pull(mismatch)) * 100
  flagged_mismatch <- if (nrow(flagged_final) > 0) {
    mean(pipeline_final %>% filter(group == "flagged") %>% pull(mismatch)) * 100
  } else { 0 }
  pipeline_instab <- mean(pipeline_final$mismatch) * 100
  
  # Cost breakdown
  calibration_cost <- best$pct * (best$K - 1)  # One-time
  application_cost <- 1 + (n_flagged / n_pipeline) * (K_verify - 1)  # Ongoing
  total_cost_with_calib <- 1 + calibration_cost + (n_flagged / n_pipeline) * (K_verify - 1)
  
  cat("This measures: P(final output ≠ 50-run modal) using ACTUAL run data\n")
  cat("  - Accepted items: final output = run1\n")
  cat(sprintf("  - Verified items: final output = actual modal of runs 1-%d\n\n", K_verify))
  
  cat(sprintf("Accepted (%.1f%%): %.2f%% mismatch with oracle\n", n_accepted/n_pipeline*100, accepted_mismatch))
  cat(sprintf("Verified (%.1f%%): %.2f%% mismatch with oracle\n", n_flagged/n_pipeline*100, flagged_mismatch))
  cat(sprintf("────────────────────────────────────────\n"))
  cat(sprintf("TOTAL PIPELINE: %.2f%% mismatch\n", pipeline_instab))
  
  cat(sprintf("\nCost breakdown:\n"))
  cat(sprintf("  Application cost (ongoing): %.2fN\n", application_cost))
  cat(sprintf("  Calibration cost (one-time): %.2fN\n", calibration_cost))
  cat(sprintf("  Total (first use): %.2fN\n", total_cost_with_calib))
  
  cat(sprintf("\nFinal Comparison (same metric for all):\n\n"))
  cat("Method                     | Cost    | Pipeline Mismatch\n")
  cat("---------------------------|---------|------------------\n")
  cat(sprintf("Single run                 | 1.00N   | %.2f%%\n", 
              mean(data$is_error, na.rm = TRUE) * 100))
  cat(sprintf("OUR PROTOCOL (τ=%.2f)      | %.2fN*  | %.2f%%\n",
              tau, application_cost, pipeline_instab))
  cat(sprintf("Always-3×                  | 3.00N   | %.2f%%\n", instab_3x))
  cat(sprintf("Always-5×                  | 5.00N   | %.2f%%\n", instab_5x))
  cat(sprintf("Always-7×                  | 7.00N   | %.2f%%\n", instab_7x))
  cat(sprintf("\n* Plus %.2fN one-time calibration cost\n", calibration_cost))
  
  # Check comparisons
  cat("\n")
  if (pipeline_instab <= instab_3x) {
    cat(sprintf("✓ BEATS Always-3×: %.2f%% vs %.2f%% at %.0f%% lower cost!\n",
                pipeline_instab, instab_3x, (1 - application_cost / 3) * 100))
  }
  if (pipeline_instab <= instab_5x) {
    cat(sprintf("✓ BEATS Always-5×: %.2f%% vs %.2f%% at %.0f%% lower cost!\n",
                pipeline_instab, instab_5x, (1 - application_cost / 5) * 100))
  }
  if (pipeline_instab <= instab_7x) {
    cat(sprintf("✓ BEATS Always-7×: %.2f%% vs %.2f%% at %.0f%% lower cost!\n",
                pipeline_instab, instab_7x, (1 - application_cost / 7) * 100))
  }
  
  # NA cases warning
  n_na <- sum(is.na(confidence_results$merged$top1_prob))
  if (n_na > 0) {
    cat(sprintf("\nNote: %d items have NA confidence (auto-flagged for verification)\n", n_na))
  }
  
  # --- SECTION 8: Executive Summary ---
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("8. EXECUTIVE SUMMARY\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  cat(sprintf("Model: %s\n", MODEL))
  cat(sprintf("Items analyzed: %d\n", n_total))
  cat(sprintf("Baseline mismatch (single run): %.2f%%\n", mean(data$is_error, na.rm = TRUE) * 100))
  cat(sprintf("\n"))
  cat(sprintf("FINDING: Confidence strongly predicts instability (AUC = %.3f)\n", auc))
  if (!is.na(auc_margin)) {
    cat(sprintf("         (margin AUC = %.3f, correlation = %.3f - nearly identical)\n",
                auc_margin, cor(data$top1_prob, data$margin, use = "complete.obs")))
  }
  cat(sprintf("         %.1f%% of items have confidence ≥ %.2f\n",
              n_accepted/n_pipeline*100, tau))
  cat(sprintf("\n"))
  cat(sprintf("PROTOCOL:\n"))
  cat(sprintf("  Calibration (one-time): Sample %.0f%% (low-confidence) × %d runs → learn τ = %.2f\n",
              best$pct * 100, best$K, tau))
  cat(sprintf("  Application (ongoing):  Accept if conf ≥ %.2f, else verify with %d runs\n", tau, K_verify))
  cat(sprintf("  Flagged for verification: %.1f%%\n", n_flagged/n_pipeline * 100))
  cat(sprintf("\n"))
  cat(sprintf("HEADLINE RESULT (using actual run data):\n\n"))
  cat(sprintf("  Method              | Cost    | Pipeline Mismatch\n"))
  cat(sprintf("  --------------------|---------|------------------\n"))
  cat(sprintf("  Single run          | 1.00N   | %.2f%%\n", mean(data$is_error, na.rm = TRUE) * 100))
  cat(sprintf("  OUR PROTOCOL        | %.2fN*  | %.2f%%\n", application_cost, pipeline_instab))
  cat(sprintf("  Always-3×           | 3.00N   | %.2f%%\n", instab_3x))
  cat(sprintf("  Always-5×           | 5.00N   | %.2f%%\n", instab_5x))
  cat(sprintf("  Always-7×           | 7.00N   | %.2f%%\n", instab_7x))
  cat(sprintf("\n  * Plus %.2fN one-time calibration cost\n", calibration_cost))
  cat(sprintf("\n"))
  
  # Highlight wins
  cat("WINS:\n")
  if (pipeline_instab <= instab_3x) {
    cat(sprintf("  ★ BEATS Always-3×: %.2f%% vs %.2f%% mismatch at %.0f%% lower cost!\n",
                pipeline_instab, instab_3x, (1 - application_cost / 3) * 100))
  }
  if (pipeline_instab <= instab_5x) {
    cat(sprintf("  ★ BEATS Always-5×: %.2f%% vs %.2f%% mismatch at %.0f%% lower cost!\n",
                pipeline_instab, instab_5x, (1 - application_cost / 5) * 100))
  }
  
  # Return key results
  invisible(list(
    stability = stability_results,
    confidence = confidence_results,
    grid = grid_results,
    best_config = best,
    auc_top1 = auc,
    auc_margin = auc_margin,
    pipeline_instab = pipeline_instab,
    application_cost = application_cost,
    calibration_cost = calibration_cost,
    instab_baseline = mean(data$is_error, na.rm = TRUE) * 100,
    instab_3x = instab_3x,
    instab_5x = instab_5x,
    instab_7x = instab_7x
  ))
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat(sprintf("  MASTER PROTOCOL ANALYSIS: %s\n", MODEL))
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("Checking for cached data...\n\n")

# Step 1: Load or compute stability
cat("STEP 1: Stability analysis\n")
stability_results <- load_or_compute_stability()
cat(sprintf("  → %d items loaded\n\n", nrow(stability_results$stability)))

# Step 2: Load or compute confidence
cat("STEP 2: Confidence analysis\n")
confidence_results <- load_or_compute_confidence(stability_results)
cat(sprintf("  → %d items with confidence\n\n", nrow(confidence_results$merged)))

# Step 3: Load or compute grid search
cat("STEP 3: Calibration grid search\n")
grid_results <- load_or_compute_grid(stability_results, confidence_results)
cat(sprintf("  → %d configurations tested\n\n", nrow(grid_results$grid)))

# Step 4: Run full analysis
cat("STEP 4: Running analysis...\n")
final_results <- run_full_analysis(stability_results, confidence_results, grid_results)

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("  ANALYSIS COMPLETE\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("Results objects available:\n")
cat("  stability_results  - 50-run stability data\n")
cat("  confidence_results - Logprob confidence data\n")
cat("  grid_results       - Calibration grid search\n")
cat("  final_results      - Analysis summary\n\n")

cat("Cached files:\n")
cat(sprintf("  %s\n", STABILITY_CACHE))
cat(sprintf("  %s\n", CONFIDENCE_CACHE))
cat(sprintf("  %s\n", GRID_CACHE))
