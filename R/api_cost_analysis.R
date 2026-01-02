# =============================================================================
# API COST AND ERROR ANALYSIS
# =============================================================================
#
# Analyzes:
#   1. Token usage and estimated costs per run
#   2. Time/latency patterns
#   3. Content filter errors and patterns
#   4. Which instances consistently fail
#
# =============================================================================

library(tidyverse)
library(jsonlite)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Pricing (per 1M tokens, as of 2024)
PRICING <- list(
  "gpt-4o" = list(input = 2.50, output = 10.00),
  "gpt-4o-mini" = list(input = 0.15, output = 0.60)
)

# Set model
if (!exists("MODEL")) {
  MODEL <- "gpt-4o"
}

base_output_dir <- "~/Desktop/Reproduciblity/output"
output_dir <- file.path(base_output_dir, MODEL)

# =============================================================================
# LOAD RAW DATA WITH METADATA
# =============================================================================

#' Load run data with full metadata including errors
load_run_with_metadata <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  
  map_dfr(lines, function(line) {
    parsed <- tryCatch(fromJSON(line, flatten = FALSE), error = function(e) NULL)
    if (is.null(parsed)) return(NULL)
    
    tibble(
      id = parsed$id %||% NA_integer_,
      localId = parsed$localId %||% NA_character_,
      dataset = parsed$dataset %||% NA_character_,
      task = parsed$task %||% NA_character_,
      run = parsed$run %||% NA_integer_,
      hasError = parsed$hasError %||% FALSE,
      errorType = parsed$errorType %||% NA_character_,
      errorMessage = parsed$errorMessage %||% NA_character_,
      promptTokens = parsed$numInputTokens %||% parsed$promptTokens %||% NA_integer_,
      completionTokens = parsed$numOutputTokens %||% parsed$completionTokens %||% NA_integer_,
      # Try to get timing if available
      latency_ms = parsed$latency_ms %||% parsed$duration_ms %||% NA_real_
    )
  })
}

#' Load all runs with metadata
load_all_runs_metadata <- function() {
  files <- list.files(output_dir, pattern = "run_\\d+\\.jsonl$", full.names = TRUE)
  
  if (length(files) == 0) {
    stop("No run files found in ", output_dir)
  }
  
  run_nums <- as.integer(str_extract(basename(files), "\\d+"))
  
  cat("Loading", length(files), "run files with metadata...\n")
  
  all_data <- map2_dfr(files, run_nums, function(f, r) {
    df <- load_run_with_metadata(f)
    if (!"run" %in% names(df) || all(is.na(df$run))) {
      df$run <- r
    }
    df
  })
  
  cat("Total records:", nrow(all_data), "\n\n")
  all_data
}

# =============================================================================
# TOKEN AND COST ANALYSIS
# =============================================================================

analyze_token_usage <- function(data) {
  
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("TOKEN USAGE ANALYSIS\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Overall stats
  total_input <- sum(data$promptTokens, na.rm = TRUE)
  total_output <- sum(data$completionTokens, na.rm = TRUE)
  total_tokens <- total_input + total_output
  
  cat("OVERALL TOKEN USAGE:\n")
  cat(sprintf("  Total input tokens:  %s\n", format(total_input, big.mark = ",")))
  cat(sprintf("  Total output tokens: %s\n", format(total_output, big.mark = ",")))
  cat(sprintf("  Total tokens:        %s\n", format(total_tokens, big.mark = ",")))
  
  # Per-run stats
  cat("\nPER-RUN STATISTICS:\n")
  per_run <- data %>%
    group_by(run) %>%
    summarise(
      n_calls = n(),
      n_success = sum(!hasError),
      n_errors = sum(hasError),
      input_tokens = sum(promptTokens, na.rm = TRUE),
      output_tokens = sum(completionTokens, na.rm = TRUE),
      total_tokens = input_tokens + output_tokens,
      .groups = "drop"
    )
  
  print(per_run %>% head(10))
  if (nrow(per_run) > 10) {
    cat(sprintf("  ... and %d more runs\n", nrow(per_run) - 10))
  }
  
  # Average per call
  cat("\nAVERAGE PER API CALL:\n")
  avg_input <- mean(data$promptTokens, na.rm = TRUE)
  avg_output <- mean(data$completionTokens, na.rm = TRUE)
  cat(sprintf("  Input tokens:  %.1f\n", avg_input))
  cat(sprintf("  Output tokens: %.1f\n", avg_output))
  cat(sprintf("  Total:         %.1f\n", avg_input + avg_output))
  
  # By task
  cat("\nBY TASK:\n")
  by_task <- data %>%
    group_by(dataset, task) %>%
    summarise(
      n_calls = n(),
      avg_input = mean(promptTokens, na.rm = TRUE),
      avg_output = mean(completionTokens, na.rm = TRUE),
      total_input = sum(promptTokens, na.rm = TRUE),
      total_output = sum(completionTokens, na.rm = TRUE),
      .groups = "drop"
    )
  print(by_task)
  
  invisible(list(
    total_input = total_input,
    total_output = total_output,
    per_run = per_run,
    by_task = by_task
  ))
}

# =============================================================================
# COST ESTIMATION
# =============================================================================

estimate_costs <- function(data, model = MODEL) {
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat(sprintf("COST ESTIMATION (%s)\n", model))
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  prices <- PRICING[[model]]
  if (is.null(prices)) {
    cat("Unknown model pricing. Using gpt-4o prices.\n")
    prices <- PRICING[["gpt-4o"]]
  }
  
  cat(sprintf("Pricing (per 1M tokens): Input = $%.2f, Output = $%.2f\n\n", 
              prices$input, prices$output))
  
  # Total cost
  total_input <- sum(data$promptTokens, na.rm = TRUE)
  total_output <- sum(data$completionTokens, na.rm = TRUE)
  
  input_cost <- total_input / 1e6 * prices$input
  output_cost <- total_output / 1e6 * prices$output
  total_cost <- input_cost + output_cost
  
  cat("TOTAL COST:\n")
  cat(sprintf("  Input cost:  $%.2f\n", input_cost))
  cat(sprintf("  Output cost: $%.2f\n", output_cost))
  cat(sprintf("  Total cost:  $%.2f\n", total_cost))
  
  # Per run
  n_runs <- n_distinct(data$run)
  cat(sprintf("\nPER RUN (n=%d runs):\n", n_runs))
  cat(sprintf("  Cost per run: $%.2f\n", total_cost / n_runs))
  
  # Per instance
  n_instances <- data %>%
    filter(run == 1) %>%
    nrow()
  cat(sprintf("\nPER INSTANCE (n=%d instances):\n", n_instances))
  cat(sprintf("  Cost per instance (single run): $%.4f\n", total_cost / n_runs / n_instances))
  cat(sprintf("  Cost per instance (50 runs):    $%.4f\n", total_cost / n_instances))
  
  # Protocol cost comparison
  cat("\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("PROTOCOL COST COMPARISON (for N instances):\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  cost_per_call <- total_cost / nrow(data)
  
  cat(sprintf("Cost per API call: $%.5f\n\n", cost_per_call))
  
  # Different strategies
  strategies <- tibble(
    Strategy = c(
      "Single run",
      "Always 3×",
      "Always 5×",
      "Gated 3× (10% flagged)",
      "Gated 5× (10% flagged)",
      "Gated 5× + calibration"
    ),
    Multiplier = c(
      1.0,
      3.0,
      5.0,
      1 + 0.10 * 2,  # 1 + flagged × (K-1)
      1 + 0.10 * 4,
      1 + 0.10 * 4 + 0.10 * 10  # + calibration
    )
  ) %>%
    mutate(
      `Cost per 1K instances` = sprintf("$%.2f", Multiplier * 1000 * cost_per_call),
      `Cost per 10K instances` = sprintf("$%.2f", Multiplier * 10000 * cost_per_call),
      `vs Always-3×` = sprintf("%.0f%%", Multiplier / 3.0 * 100)
    )
  
  print(strategies)
  
  invisible(list(
    total_cost = total_cost,
    cost_per_call = cost_per_call,
    cost_per_run = total_cost / n_runs,
    strategies = strategies
  ))
}

# =============================================================================
# ERROR ANALYSIS
# =============================================================================

analyze_errors <- function(data) {
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("ERROR ANALYSIS\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Overall error rate
  n_total <- nrow(data)
  n_errors <- sum(data$hasError)
  
  cat("OVERALL:\n")
  cat(sprintf("  Total API calls: %d\n", n_total))
  cat(sprintf("  Errors: %d (%.3f%%)\n", n_errors, n_errors / n_total * 100))
  
  if (n_errors == 0) {
    cat("\n✓ No errors found!\n")
    return(invisible(NULL))
  }
  
  # Error types
  cat("\nERROR TYPES:\n")
  error_types <- data %>%
    filter(hasError) %>%
    count(errorType, sort = TRUE) %>%
    mutate(pct = n / sum(n) * 100)
  print(error_types)
  
  # Errors by run
  cat("\nERRORS BY RUN:\n")
  errors_by_run <- data %>%
    group_by(run) %>%
    summarise(
      n_calls = n(),
      n_errors = sum(hasError),
      error_rate = mean(hasError) * 100,
      .groups = "drop"
    ) %>%
    filter(n_errors > 0)
  
  if (nrow(errors_by_run) > 0) {
    print(errors_by_run)
  } else {
    cat("  No errors in any run\n")
  }
  
  # Which instances have errors?
  cat("\nINSTANCES WITH ERRORS:\n")
  error_instances <- data %>%
    filter(hasError) %>%
    group_by(id, dataset, task) %>%
    summarise(
      n_errors = n(),
      runs_with_errors = paste(run, collapse = ", "),
      error_types = paste(unique(errorType), collapse = ", "),
      .groups = "drop"
    ) %>%
    arrange(desc(n_errors))
  
  cat(sprintf("  Unique instances with at least 1 error: %d\n", nrow(error_instances)))
  
  if (nrow(error_instances) > 0) {
    cat("\nMost error-prone instances:\n")
    print(error_instances %>% head(20))
  }
  
  # Are errors consistent across runs?
  cat("\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("ERROR CONSISTENCY ANALYSIS:\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # For each instance, how many runs have errors?
  error_consistency <- data %>%
    group_by(id, dataset, task) %>%
    summarise(
      n_runs = n(),
      n_errors = sum(hasError),
      error_rate = mean(hasError),
      .groups = "drop"
    )
  
  # Distribution of error rates per instance
  cat("Distribution of per-instance error rates:\n")
  error_dist <- error_consistency %>%
    mutate(error_category = case_when(
      n_errors == 0 ~ "No errors",
      error_rate == 1 ~ "Always fails",
      error_rate >= 0.5 ~ "Fails often (≥50%)",
      error_rate >= 0.1 ~ "Fails sometimes (10-50%)",
      TRUE ~ "Rare failures (<10%)"
    )) %>%
    count(error_category) %>%
    mutate(pct = n / sum(n) * 100)
  print(error_dist)
  
  # Content filter analysis
  cat("\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("CONTENT FILTER ANALYSIS:\n")
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  content_filter_errors <- data %>%
    filter(hasError, str_detect(tolower(errorType %||% ""), "content|filter|moderation"))
  
  if (nrow(content_filter_errors) > 0) {
    cat(sprintf("Content filter errors: %d\n", nrow(content_filter_errors)))
    
    cf_instances <- content_filter_errors %>%
      distinct(id, dataset, task) %>%
      nrow()
    cat(sprintf("Unique instances affected: %d\n", cf_instances))
    
    # Are they always the same instances?
    cf_by_instance <- content_filter_errors %>%
      group_by(id, dataset, task) %>%
      summarise(
        n_cf_errors = n(),
        runs_affected = paste(sort(run), collapse = ", "),
        .groups = "drop"
      )
    
    cat("\nContent-filtered instances:\n")
    print(cf_by_instance)
  } else {
    cat("No content filter errors detected\n")
  }
  
  invisible(list(
    error_types = error_types,
    error_instances = error_instances,
    error_consistency = error_consistency
  ))
}

# =============================================================================
# LATENCY ANALYSIS (if available)
# =============================================================================

analyze_latency <- function(data) {
  
  if (all(is.na(data$latency_ms))) {
    cat("\nNo latency data available.\n")
    return(invisible(NULL))
  }
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("LATENCY ANALYSIS\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  latency_data <- data %>% filter(!is.na(latency_ms))
  
  cat(sprintf("Calls with latency data: %d / %d\n\n", nrow(latency_data), nrow(data)))
  
  cat("LATENCY STATISTICS (ms):\n")
  cat(sprintf("  Mean:   %.1f\n", mean(latency_data$latency_ms)))
  cat(sprintf("  Median: %.1f\n", median(latency_data$latency_ms)))
  cat(sprintf("  SD:     %.1f\n", sd(latency_data$latency_ms)))
  cat(sprintf("  Min:    %.1f\n", min(latency_data$latency_ms)))
  cat(sprintf("  Max:    %.1f\n", max(latency_data$latency_ms)))
  
  # Percentiles
  cat("\nPERCENTILES:\n")
  percentiles <- quantile(latency_data$latency_ms, probs = c(0.5, 0.9, 0.95, 0.99))
  for (i in seq_along(percentiles)) {
    cat(sprintf("  P%s: %.1f ms\n", names(percentiles)[i], percentiles[i]))
  }
  
  # By task
  cat("\nBY TASK:\n")
  by_task <- latency_data %>%
    group_by(dataset, task) %>%
    summarise(
      n = n(),
      mean_latency = mean(latency_ms),
      median_latency = median(latency_ms),
      .groups = "drop"
    )
  print(by_task)
  
  # Total time
  total_time_ms <- sum(latency_data$latency_ms)
  cat(sprintf("\nTOTAL TIME:\n"))
  cat(sprintf("  Total: %.1f seconds (%.1f minutes)\n", 
              total_time_ms / 1000, total_time_ms / 60000))
  cat(sprintf("  Per run: %.1f seconds\n", 
              total_time_ms / 1000 / n_distinct(latency_data$run)))
  
  invisible(latency_data)
}

# =============================================================================
# MAIN ANALYSIS FUNCTION
# =============================================================================

run_api_analysis <- function(model = MODEL) {
  
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat(sprintf("  API COST AND ERROR ANALYSIS: %s\n", model))
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Load data
  data <- load_all_runs_metadata()
  
  # Run analyses
  token_stats <- analyze_token_usage(data)
  cost_stats <- estimate_costs(data, model)
  error_stats <- analyze_errors(data)
  latency_stats <- analyze_latency(data)
  
  # Summary
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("SUMMARY\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  cat(sprintf("Model: %s\n", model))
  cat(sprintf("Total API calls: %s\n", format(nrow(data), big.mark = ",")))
  cat(sprintf("Total runs: %d\n", n_distinct(data$run)))
  cat(sprintf("Total cost: $%.2f\n", cost_stats$total_cost))
  cat(sprintf("Error rate: %.3f%%\n", mean(data$hasError) * 100))
  
  invisible(list(
    data = data,
    tokens = token_stats,
    costs = cost_stats,
    errors = error_stats,
    latency = latency_stats
  ))
}

# =============================================================================
# USAGE
# =============================================================================

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("API COST AND ERROR ANALYSIS LOADED\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("Run full analysis:\n")
cat("  MODEL <- 'gpt-4o'  # or 'gpt-4o-mini'\n")
cat("  api_stats <- run_api_analysis(MODEL)\n\n")

cat("Or run individual components:\n")
cat("  data <- load_all_runs_metadata()\n")
cat("  analyze_token_usage(data)\n")
cat("  estimate_costs(data, MODEL)\n")
cat("  analyze_errors(data)\n")
cat("  analyze_latency(data)\n")
