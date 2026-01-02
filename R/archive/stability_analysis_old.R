# =============================================================================
# LLM Classification Stability Analysis
# =============================================================================
# Analyzes classification consistency across multiple runs
# Calculates flip rates, modal classifications, and stability metrics

library(tidyverse)
library(jsonlite)

# =============================================================================
# CONFIGURATION
# =============================================================================

output_dir <- "~/Desktop/Reproduciblity/output"

# Known content-filtered instances (permanently excluded)
CONTENT_FILTER_IDS <- c(10, 1311, 1836, 2055, 2162, 2687, 2906)

# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

`%||%` <- function(x, y) if (is.null(x)) y else x

load_single_run <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  
  records <- map(lines, function(line) {
    parsed <- fromJSON(line, flatten = FALSE)
    
    prompt_tokens <- parsed$numInputTokens %||% parsed$promptTokens %||% NA_integer_
    completion_tokens <- parsed$numOutputTokens %||% parsed$completionTokens %||% NA_integer_
    
    output_val <- parsed$output
    if (is.list(output_val)) {
      output_val <- toJSON(output_val, auto_unbox = TRUE)
    }
    
    tibble(
      id = parsed$id,
      localId = parsed$localId,
      dataset = parsed$dataset,
      task = parsed$task,
      run = parsed$run %||% NA_integer_,
      output = as.character(output_val),
      hasError = parsed$hasError %||% FALSE,
      promptTokens = as.integer(prompt_tokens),
      completionTokens = as.integer(completion_tokens),
      logprobs = list(parsed$logprobs)
    )
  })
  
  bind_rows(records)
}

#' Load all available runs into a single dataframe
#' @param runs Optional vector of run numbers to load (e.g., 1:6). NULL = load all.
#' @return Dataframe with all runs stacked
load_all_runs <- function(runs = NULL) {
  files <- list.files(output_dir, pattern = "run_\\d+\\.jsonl$", full.names = TRUE)
  
  if (length(files) == 0) {
    stop("No run files found in ", output_dir)
  }
  
  # Extract run numbers from filenames
  run_nums <- as.integer(str_extract(basename(files), "\\d+"))
  
  # Filter to requested runs
 if (!is.null(runs)) {
    keep <- run_nums %in% runs
    files <- files[keep]
    run_nums <- run_nums[keep]
  }
  
  cat("Loading", length(files), "run files...\n")
  
  all_data <- map2_dfr(files, run_nums, function(f, r) {
    cat("  Loading run", r, "...")
    df <- load_single_run(f)
    cat(" done (", nrow(df), " records)\n", sep = "")
    df
  })
  
  cat("Total records:", nrow(all_data), "\n")
  cat("Runs loaded:", paste(sort(unique(all_data$run)), collapse = ", "), "\n\n")
  
  all_data
}

# =============================================================================
# PARSE CLASSIFICATIONS INTO LONG FORMAT
# =============================================================================

#' Extract classification values from JSON output
#' Handles both single-value tasks and p2_promise (3 values per instance)
#' @return Long-format dataframe with one row per classification
parse_classifications <- function(df) {
  
  # Helper to safely parse JSON
  safe_parse <- function(json_str) {
    tryCatch(fromJSON(json_str), error = function(e) NULL)
  }
  
  # Process each task type differently
  results <- list()
  
  # --- Single classification tasks ---
  single_tasks <- df %>%
    filter(!(dataset == "p2" & task == "promise")) %>%
    filter(hasError == FALSE)
  
  if (nrow(single_tasks) > 0) {
    single_parsed <- single_tasks %>%
      mutate(
        parsed = map(output, safe_parse),
        classification = map_chr(parsed, ~ .x$classification %||% NA_character_)
      ) %>%
      select(id, localId, dataset, task, run, classification, logprobs) %>%
      mutate(player = NA_character_)  # No player for non-p2 tasks
    
    results$single <- single_parsed
  }
  
  # --- p2_promise: 3 classifications per instance ---
  p2_data <- df %>%
    filter(dataset == "p2", task == "promise", hasError == FALSE)
  
  if (nrow(p2_data) > 0) {
    p2_long <- p2_data %>%
      mutate(parsed = map(output, safe_parse)) %>%
      mutate(
        p1 = map_chr(parsed, ~ .x$p1 %||% NA_character_),
        p2 = map_chr(parsed, ~ .x$p2 %||% NA_character_),
        p3 = map_chr(parsed, ~ .x$p3 %||% NA_character_)
      ) %>%
      select(id, localId, dataset, task, run, p1, p2, p3, logprobs) %>%
      pivot_longer(
        cols = c(p1, p2, p3),
        names_to = "player",
        values_to = "classification"
      )
    
    results$p2 <- p2_long
  }
  
  # Combine
  bind_rows(results) %>%
    arrange(id, player, run)
}

# =============================================================================
# STABILITY METRICS
# =============================================================================

#' Calculate stability metrics for each classification unit
#' A "unit" is: (id) for single tasks, (id, player) for p2_promise
#' @param parsed_df Output from parse_classifications()
#' @return Dataframe with one row per unit, containing stability metrics
calculate_stability <- function(parsed_df) {
  
  # Group by the appropriate unit
  stability <- parsed_df %>%
    group_by(id, localId, dataset, task, player) %>%
    summarise(
      n_runs = n(),
      n_unique = n_distinct(classification, na.rm = TRUE),
      
      # Modal classification (most frequent)
      modal_class = {
        counts <- table(classification)
        if (length(counts) > 0) names(which.max(counts)) else NA_character_
      },
      
      # Modal count and frequency
      modal_count = {
        counts <- table(classification)
        if (length(counts) > 0) max(counts) else 0L
      },
      modal_freq = modal_count / n_runs,
      
      # All classifications as comma-separated string
      all_classes = paste(classification, collapse = ","),
      
      # Number of flips (changes from modal)
      n_flips = sum(classification != modal_class, na.rm = TRUE),
      
      # Flip rate
      flip_rate = n_flips / n_runs,
      
      # Is this unit "stable"? (100% agreement)
      is_stable = n_unique == 1,
      
      # Entropy (uncertainty measure)
      entropy = {
        counts <- table(classification)
        probs <- counts / sum(counts)
        -sum(probs * log2(probs + 1e-10))
      },
      
      .groups = "drop"
    )
  
  stability
}

#' Summarize stability by task
summarize_by_task <- function(stability_df) {
  stability_df %>%
    group_by(dataset, task) %>%
    summarise(
      n_units = n(),
      n_runs = first(n_runs),
      
      # Stability rates
      pct_stable = mean(is_stable) * 100,
      pct_unstable = mean(!is_stable) * 100,
      
      # Flip statistics
      mean_flip_rate = mean(flip_rate) * 100,
      median_flip_rate = median(flip_rate) * 100,
      max_flip_rate = max(flip_rate) * 100,
      
      # Modal agreement
      mean_modal_freq = mean(modal_freq) * 100,
      
      # Entropy
      mean_entropy = mean(entropy),
      
      .groups = "drop"
    )
}

# =============================================================================
# ANALYSIS FUNCTIONS
# =============================================================================

#' Get the most unstable instances
get_unstable <- function(stability_df, top_n = 20) {
  stability_df %>%
    filter(!is_stable) %>%
    arrange(desc(flip_rate), desc(entropy)) %>%
    head(top_n) %>%
    select(id, localId, dataset, task, player, n_runs, modal_class, 
           modal_freq, flip_rate, entropy, all_classes)
}

#' Compare stability across consecutive run pairs
pairwise_flip_analysis <- function(parsed_df) {
  runs <- sort(unique(parsed_df$run))
  
  if (length(runs) < 2) {
    stop("Need at least 2 runs for pairwise analysis")
  }
  
  pairs <- tibble(
    run1 = runs[-length(runs)],
    run2 = runs[-1]
  )
  
  results <- map2_dfr(pairs$run1, pairs$run2, function(r1, r2) {
    df1 <- parsed_df %>% filter(run == r1)
    df2 <- parsed_df %>% filter(run == r2)
    
    comparison <- df1 %>%
      select(id, player, dataset, task, class1 = classification) %>%
      inner_join(
        df2 %>% select(id, player, class2 = classification),
        by = c("id", "player")
      ) %>%
      mutate(flipped = class1 != class2)
    
    comparison %>%
      group_by(dataset, task) %>%
      summarise(
        run_pair = paste0(r1, "→", r2),
        n = n(),
        flips = sum(flipped, na.rm = TRUE),
        flip_rate = mean(flipped, na.rm = TRUE) * 100,
        .groups = "drop"
      )
  })
  
  results
}

#' Cumulative stability: how does flip rate change as we add more runs?
cumulative_stability <- function(parsed_df) {
  runs <- sort(unique(parsed_df$run))
  
  results <- map_dfr(2:length(runs), function(k) {
    subset <- parsed_df %>% filter(run %in% runs[1:k])
    stability <- calculate_stability(subset)
    
    stability %>%
      group_by(dataset, task) %>%
      summarise(
        k_runs = k,
        pct_stable = mean(is_stable) * 100,
        mean_flip_rate = mean(flip_rate) * 100,
        mean_modal_freq = mean(modal_freq) * 100,
        .groups = "drop"
      )
  })
  
  results
}

# =============================================================================
# MAIN ANALYSIS
# =============================================================================

run_stability_analysis <- function(runs = NULL, force_reload = FALSE) {
  
  # Check if results already exist and we're not forcing reload
  if (!force_reload && exists("results", envir = .GlobalEnv)) {
    existing <- get("results", envir = .GlobalEnv)
    if (is.list(existing) && all(c("parsed", "stability") %in% names(existing))) {
      cat("Using cached results (", nrow(existing$stability), " units from ", 
          max(existing$stability$n_runs), " runs)\n", sep = "")
      cat("To reload, use: results <- run_stability_analysis(force_reload = TRUE)\n\n")
      return(invisible(existing))
    }
  }
  
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
  cat("LLM CLASSIFICATION STABILITY ANALYSIS\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
  
  # Load data
  all_runs <- load_all_runs(runs)
  
  # Parse into long format
  cat("Parsing classifications...\n")
  parsed <- parse_classifications(all_runs)
  cat("Total classification units:", n_distinct(paste(parsed$id, parsed$player)), "\n")
  cat("Total observations:", nrow(parsed), "\n\n")
  
  # Calculate stability
  cat("Calculating stability metrics...\n\n")
  stability <- calculate_stability(parsed)
  
  # Summary by task
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
  cat("STABILITY BY TASK\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
  
  task_summary <- summarize_by_task(stability)
  print(task_summary %>% 
          select(dataset, task, n_units, n_runs, pct_stable, mean_flip_rate, mean_modal_freq))
  
  # Overall stability
  cat("\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
  cat("OVERALL STABILITY\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
  
  overall <- stability %>%
    summarise(
      total_units = n(),
      n_runs = first(n_runs),
      stable = sum(is_stable),
      unstable = sum(!is_stable),
      pct_stable = mean(is_stable) * 100,
      mean_flip_rate = mean(flip_rate) * 100,
      mean_modal_freq = mean(modal_freq) * 100
    )
  
  cat("Total classification units:", overall$total_units, "\n")
  cat("Runs analyzed:", overall$n_runs, "\n")
  cat("Stable (100% agreement):", overall$stable, 
      sprintf("(%.1f%%)", overall$pct_stable), "\n")
  cat("Unstable (any disagreement):", overall$unstable,
      sprintf("(%.1f%%)", 100 - overall$pct_stable), "\n")
  cat("Mean flip rate:", sprintf("%.2f%%", overall$mean_flip_rate), "\n")
  cat("Mean modal agreement:", sprintf("%.1f%%", overall$mean_modal_freq), "\n")
  
  # Most unstable instances
  cat("\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
  cat("MOST UNSTABLE INSTANCES (Top 15)\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
  
  unstable <- get_unstable(stability, 15)
  print(unstable %>% select(-all_classes))
  
  # Pairwise analysis
  cat("\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
  cat("PAIRWISE FLIP RATES (consecutive runs)\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
  
  pairwise <- pairwise_flip_analysis(parsed)
  
  # Summarize across tasks
  pairwise_summary <- pairwise %>%
    group_by(run_pair) %>%
    summarise(
      total_units = sum(n),
      total_flips = sum(flips),
      flip_rate = sum(flips) / sum(n) * 100,
      .groups = "drop"
    )
  print(pairwise_summary)
  
  # Return objects for further analysis
  invisible(list(
    all_runs = all_runs,
    parsed = parsed,
    stability = stability,
    task_summary = task_summary,
    pairwise = pairwise
  ))
}

# =============================================================================
# RUN ANALYSIS
# =============================================================================

# Check if results already exist before running
if (exists("results") && is.list(results) && "stability" %in% names(results)) {
  cat("Results already loaded (", nrow(results$stability), " units from ",
      max(results$stability$n_runs), " runs)\n", sep = "")
  cat("To reload: results <- run_stability_analysis(force_reload = TRUE)\n\n")
} else {
  # Run the analysis (loads all available runs)
  results <- run_stability_analysis()
}

# Access the data for further exploration:
# results$stability     - per-unit stability metrics
# results$parsed        - long-format classifications
# results$task_summary  - summary by task
# results$pairwise      - consecutive run comparisons

cat("\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("OBJECTS AVAILABLE FOR FURTHER ANALYSIS\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
cat("results$stability     - Per-unit stability metrics\n")
cat("results$parsed        - Long-format classifications (all runs)\n")
cat("results$task_summary  - Summary statistics by task\n")
cat("results$pairwise      - Pairwise flip rates between consecutive runs\n")
cat("\nExample explorations:\n")
cat("  results$stability %>% filter(flip_rate > 0.3)  # High flip instances\n")
cat("  results$stability %>% filter(dataset == 'l2', task == 'level')  # Specific task\n")
cat("  cumulative_stability(results$parsed)  # How stability changes with k\n")
