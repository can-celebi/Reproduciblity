# =============================================================================
# LLM Classification Stability Study - Data Validation & Analysis
# =============================================================================
# Run this script after completing classification runs to validate output
# and generate summary statistics.

library(tidyverse)
library(jsonlite)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# Set paths - adjust as needed
output_dir <- "~/Desktop/Reproduciblity/output"
data_dir <- "~/Desktop/Reproduciblity/data"

# Known content-filtered instances (permanently excluded)
CONTENT_FILTER_IDS <- c(10, 1311, 1836, 2055, 2162, 2687, 2906)

# ============================================================================
# FILE SELECTION
# ============================================================================
# Option 1: Specify file directly
# output_file <- "~/Desktop/Reproduciblity/output/run_004.jsonl"

# Option 2: Specify run number
# run_number <- 4

# Option 3: Auto-select (will list all and pick most recent)

# List all available files
output_files <- list.files(output_dir, pattern = "run_.*\\.jsonl$", full.names = TRUE)
if (length(output_files) == 0) {
  stop("No output files found in ", output_dir)
}

cat("Available output files:\n")
for (i in seq_along(output_files)) {
  size_mb <- round(file.info(output_files[i])$size / 1e6, 2)
  cat(sprintf("  [%d] %s (%.2f MB)\n", i, basename(output_files[i]), size_mb))
}

# Select file
if (exists("output_file")) {
  cat("\nUsing specified file:", output_file, "\n")
} else if (exists("run_number")) {
  # Find file matching run number
  pattern <- sprintf("run_%03d", run_number)
  matches <- output_files[grepl(pattern, output_files)]
  if (length(matches) == 0) {
    stop("No file found for run ", run_number)
  }
  output_file <- matches[1]
  cat("\nSelected run", run_number, ":", basename(output_file), "\n")
} else {
  # Auto-select: most recent by modification time
  file_info <- file.info(output_files)
  output_file <- output_files[which.max(file_info$mtime)]
  cat("\nAuto-selected (most recent):", basename(output_file), "\n")
}
cat("\n")

# Load the classification output (JSONL format)
# Handle nested structures by not auto-flattening
load_jsonl <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  
  # Parse each line and extract flat fields
  records <- map(lines, function(line) {
    parsed <- fromJSON(line, flatten = FALSE)
    
    # Handle tokens - check all possible field names
    prompt_tokens <- NA_integer_
    completion_tokens <- NA_integer_
    
    # Try different field names used in run.js
    if (!is.null(parsed$numInputTokens)) {
      prompt_tokens <- as.integer(parsed$numInputTokens)
    } else if (!is.null(parsed$promptTokens)) {
      prompt_tokens <- as.integer(parsed$promptTokens)
    } else if (!is.null(parsed$usage$prompt_tokens)) {
      prompt_tokens <- as.integer(parsed$usage$prompt_tokens)
    }
    
    if (!is.null(parsed$numOutputTokens)) {
      completion_tokens <- as.integer(parsed$numOutputTokens)
    } else if (!is.null(parsed$completionTokens)) {
      completion_tokens <- as.integer(parsed$completionTokens)
    } else if (!is.null(parsed$usage$completion_tokens)) {
      completion_tokens <- as.integer(parsed$usage$completion_tokens)
    }
    
    # Handle output - might be string or already parsed
    output_val <- parsed$output
    if (is.list(output_val)) {
      output_val <- toJSON(output_val, auto_unbox = TRUE)
    }
    
    # Extract only the flat fields we need
    tibble(
      id = parsed$id,
      localId = parsed$localId,
      dataset = parsed$dataset,
      task = parsed$task,
      run = parsed$run %||% NA_integer_,
      input = parsed$input,
      output = as.character(output_val),
      hasError = parsed$hasError %||% FALSE,
      errorType = parsed$errorType %||% NA_character_,
      errorMessage = parsed$errorMessage %||% parsed$error %||% NA_character_,
      seed = parsed$seed %||% NA_integer_,
      promptTokens = prompt_tokens,
      completionTokens = completion_tokens,
      resourceName = parsed$resource %||% parsed$resourceName %||% NA_character_,
      fingerprint = parsed$fingerprint %||% NA_character_,
      duration = parsed$duration %||% NA_real_,
      timestamp = parsed$timestamp %||% NA_integer_,
      # Store logprobs as a list column (don't flatten)
      logprobs = list(parsed$logprobs)
    )
  })
  
  bind_rows(records)
}

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Load the data
cat("Loading:", output_file, "\n")

df <- load_jsonl(output_file)
cat("Loaded", nrow(df), "records\n")
cat("Unique IDs:", n_distinct(df$id), "\n")

# Check for multiple runs
if ("run" %in% names(df) && !all(is.na(df$run))) {
  cat("Runs in file:", paste(sort(unique(df$run)), collapse = ", "), "\n")
  cat("Records per run:\n")
  print(table(df$run, useNA = "ifany"))
}

if (nrow(df) != n_distinct(df$id)) {
  cat("\nNote: Multiple records per ID detected\n")
  cat("Records per ID distribution:\n")
  print(table(table(df$id)))
}

# Check first few records to understand structure
cat("\nSample output values (first 3):\n")
head(df$output, 3) %>% print()
cat("\n")

# =============================================================================
# 2. BASIC VALIDATION
# =============================================================================

cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("BASIC VALIDATION\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Check for expected columns
expected_cols <- c("id", "localId", "dataset", "task", "input", "output", 
                   "hasError", "seed", "promptTokens", "completionTokens")
actual_cols <- names(df)
missing_cols <- setdiff(expected_cols, actual_cols)
if (length(missing_cols) > 0) {
  cat("⚠️  Missing columns:", paste(missing_cols, collapse = ", "), "\n")
} else {
  cat("✓ All expected columns present\n")
}
cat("  Available columns:", paste(actual_cols, collapse = ", "), "\n")

# Check for errors
error_count <- sum(df$hasError == TRUE, na.rm = TRUE)
cat("✓ Total records:", nrow(df), "\n")
cat("✓ Successful:", sum(df$hasError == FALSE, na.rm = TRUE), "\n")
cat("⚠️  Errors:", error_count, "\n\n")

# List errored instances
if (error_count > 0) {
  cat("Errored instances:\n")
  df %>%
    filter(hasError == TRUE) %>%
    select(id, localId, dataset, task, errorType) %>%
    print()
  cat("\n")
}

# =============================================================================
# 3. DATASET/TASK BREAKDOWN
# =============================================================================

cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("DATASET/TASK BREAKDOWN\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Expected counts (full dataset)
# Note: 7 instances are permanently excluded due to content filter:
#   - p1_promise: 1 (id=10)
#   - l2_level: 3 (ids=1311, 1836, 2055)
#   - l2_belief: 3 (ids=2162, 2687, 2906)
expected_counts <- tibble(
  dataset = c("p1", "p2", "l1", "l2", "l2", "l3", "l3"),
  task = c("promise", "promise", "level", "level", "belief", "level", "belief"),
  full_expected = c(38, 719, 493, 851, 851, 78, 78),
  content_filtered = c(1, 0, 0, 3, 3, 0, 0)  # Excluded by Azure content filter
) %>%
  mutate(expected = full_expected - content_filtered)

task_summary <- df %>%
  group_by(dataset, task) %>%
  summarise(
    n = n(),
    unique_ids = n_distinct(id),
    errors = sum(hasError == TRUE, na.rm = TRUE),
    success_rate = mean(hasError == FALSE, na.rm = TRUE) * 100,
    avg_prompt_tokens = mean(promptTokens, na.rm = TRUE),
    avg_completion_tokens = mean(completionTokens, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(expected_counts, by = c("dataset", "task")) %>%
  mutate(
    records_per_id = n / unique_ids,
    matches_expected = unique_ids == expected
  )

print(task_summary %>% select(dataset, task, n, unique_ids, expected, records_per_id, errors, success_rate))

# Flag real issues (not the known content filter exclusions)
real_issues <- task_summary %>% filter(records_per_id != 1 | !matches_expected)
if (nrow(real_issues) > 0) {
  cat("\n⚠️  UNEXPECTED ISSUES:\n")
  if (any(task_summary$records_per_id > 1, na.rm = TRUE)) {
    cat("  - Some tasks have multiple records per ID (possibly multiple runs in file)\n")
  }
  mismatched <- task_summary %>% filter(!matches_expected)
  if (nrow(mismatched) > 0) {
    cat("  - Task counts don't match expected (after accounting for content filter):\n")
    for (i in 1:nrow(mismatched)) {
      cat(sprintf("    %s_%s: got %d, expected %d\n", 
                  mismatched$dataset[i], mismatched$task[i],
                  mismatched$unique_ids[i], mismatched$expected[i]))
    }
  }
} else {
  cat("\n✓ All task counts match expected (accounting for 7 content-filtered instances)\n")
}
cat("\n")

# =============================================================================
# 4. OUTPUT VALIDATION BY TASK
# =============================================================================

cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("OUTPUT VALIDATION BY TASK\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Filter to successful only
df_success <- df %>% filter(hasError == FALSE)

# Helper function to safely extract classification from output
# Handles both string JSON and already-parsed objects
safe_extract <- function(output_val, field = "classification") {
  tryCatch({
    if (is.character(output_val)) {
      # It's a JSON string, parse it
      parsed <- fromJSON(output_val)
    } else if (is.list(output_val)) {
      # Already parsed
      parsed <- output_val
    } else {
      return(NA_character_)
    }
    
    if (field %in% names(parsed)) {
      return(as.character(parsed[[field]]))
    } else {
      return(NA_character_)
    }
  }, error = function(e) {
    return(NA_character_)
  })
}

# --- p1_promise ---
cat("p1_promise:\n")
p1_data <- df_success %>% filter(dataset == "p1", task == "promise")
if (nrow(p1_data) > 0) {
  p1_parsed <- p1_data %>%
    mutate(classification = map_chr(output, ~ safe_extract(.x, "classification")))
  
  cat("  Valid values: 0, 1\n")
  cat("  Distribution:\n")
  print(table(p1_parsed$classification, useNA = "ifany"))
  
  invalid <- p1_parsed %>% filter(!classification %in% c("0", "1") & !is.na(classification))
  if (nrow(invalid) > 0) {
    cat("  ⚠️  Invalid values found:", nrow(invalid), "\n")
  } else {
    cat("  ✓ All values valid\n")
  }
}
cat("\n")

# --- p2_promise ---
cat("p2_promise:\n")
p2_data <- df_success %>% filter(dataset == "p2", task == "promise")
if (nrow(p2_data) > 0) {
  p2_parsed <- p2_data %>%
    mutate(
      p1 = map_chr(output, ~ safe_extract(.x, "p1")),
      p2 = map_chr(output, ~ safe_extract(.x, "p2")),
      p3 = map_chr(output, ~ safe_extract(.x, "p3"))
    )
  
  cat("  Valid values: 0, 1, na\n")
  cat("  P1 distribution:\n")
  print(table(p2_parsed$p1, useNA = "ifany"))
  cat("  P2 distribution:\n")
  print(table(p2_parsed$p2, useNA = "ifany"))
  cat("  P3 distribution:\n")
  print(table(p2_parsed$p3, useNA = "ifany"))
  
  valid_vals <- c("0", "1", "na")
  invalid <- p2_parsed %>%
    filter((!p1 %in% valid_vals & !is.na(p1)) | 
             (!p2 %in% valid_vals & !is.na(p2)) | 
             (!p3 %in% valid_vals & !is.na(p3)))
  if (nrow(invalid) > 0) {
    cat("  ⚠️  Invalid values found:", nrow(invalid), "\n")
  } else {
    cat("  ✓ All values valid\n")
  }
}
cat("\n")

# --- l1_level ---
cat("l1_level:\n")
l1_level_data <- df_success %>% filter(dataset == "l1", task == "level")
if (nrow(l1_level_data) > 0) {
  l1_parsed <- l1_level_data %>%
    mutate(classification = map_chr(output, ~ safe_extract(.x, "classification")))
  
  cat("  Valid values: 0, 1, 2, 3\n")
  cat("  Distribution:\n")
  print(table(l1_parsed$classification, useNA = "ifany"))
  
  invalid <- l1_parsed %>% filter(!classification %in% c("0", "1", "2", "3") & !is.na(classification))
  if (nrow(invalid) > 0) {
    cat("  ⚠️  Invalid values found:", nrow(invalid), "\n")
  } else {
    cat("  ✓ All values valid\n")
  }
}
cat("\n")

# --- l2_level ---
cat("l2_level:\n")
l2_level_data <- df_success %>% filter(dataset == "l2", task == "level")
if (nrow(l2_level_data) > 0) {
  l2_parsed <- l2_level_data %>%
    mutate(classification = map_chr(output, ~ safe_extract(.x, "classification")))
  
  cat("  Valid values: 0, 1, 2, 3, 4, 5\n")
  cat("  Distribution:\n")
  print(table(l2_parsed$classification, useNA = "ifany"))
  
  invalid <- l2_parsed %>% filter(!classification %in% as.character(0:5) & !is.na(classification))
  if (nrow(invalid) > 0) {
    cat("  ⚠️  Invalid values found:", nrow(invalid), "\n")
  } else {
    cat("  ✓ All values valid\n")
  }
}
cat("\n")

# --- l2_belief ---
cat("l2_belief:\n")
l2_belief_data <- df_success %>% filter(dataset == "l2", task == "belief")
if (nrow(l2_belief_data) > 0) {
  l2b_parsed <- l2_belief_data %>%
    mutate(classification = map_chr(output, ~ safe_extract(.x, "classification")))
  
  cat("  Valid values: 0, 1, 2, 3\n")
  cat("  Distribution:\n")
  print(table(l2b_parsed$classification, useNA = "ifany"))
  
  invalid <- l2b_parsed %>% filter(!classification %in% c("0", "1", "2", "3") & !is.na(classification))
  if (nrow(invalid) > 0) {
    cat("  ⚠️  Invalid values found:", nrow(invalid), "\n")
  } else {
    cat("  ✓ All values valid\n")
  }
}
cat("\n")

# --- l3_level ---
cat("l3_level:\n")
l3_level_data <- df_success %>% filter(dataset == "l3", task == "level")
if (nrow(l3_level_data) > 0) {
  l3_parsed <- l3_level_data %>%
    mutate(classification = map_chr(output, ~ safe_extract(.x, "classification")))
  
  cat("  Valid values: 0, 1, 2, 3, 4, 5, eq, na\n")
  cat("  Distribution:\n")
  print(table(l3_parsed$classification, useNA = "ifany"))
  
  valid_vals <- c(as.character(0:5), "eq", "na")
  invalid <- l3_parsed %>% filter(!classification %in% valid_vals & !is.na(classification))
  if (nrow(invalid) > 0) {
    cat("  ⚠️  Invalid values found:", nrow(invalid), "\n")
  } else {
    cat("  ✓ All values valid\n")
  }
}
cat("\n")

# --- l3_belief ---
cat("l3_belief:\n")
l3_belief_data <- df_success %>% filter(dataset == "l3", task == "belief")
if (nrow(l3_belief_data) > 0) {
  l3b_parsed <- l3_belief_data %>%
    mutate(classification = map_chr(output, ~ safe_extract(.x, "classification")))
  
  valid_beliefs <- c("0", "16", "26", "36", "46", "56", "66", "76", "na")
  cat("  Valid values:", paste(valid_beliefs, collapse = ", "), "\n")
  cat("  Distribution:\n")
  print(table(l3b_parsed$classification, useNA = "ifany"))
  
  invalid <- l3b_parsed %>% filter(!classification %in% valid_beliefs & !is.na(classification))
  if (nrow(invalid) > 0) {
    cat("  ⚠️  Invalid values found:", nrow(invalid), "\n")
    print(invalid %>% select(id, localId, classification))
  } else {
    cat("  ✓ All values valid\n")
  }
}
cat("\n")

# =============================================================================
# 5. TOKEN STATISTICS
# =============================================================================

cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("TOKEN STATISTICS\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

token_stats <- df_success %>%
  summarise(
    total_prompt = sum(promptTokens, na.rm = TRUE),
    total_completion = sum(completionTokens, na.rm = TRUE),
    total = total_prompt + total_completion,
    avg_prompt = mean(promptTokens, na.rm = TRUE),
    avg_completion = mean(completionTokens, na.rm = TRUE),
    min_prompt = min(promptTokens, na.rm = TRUE),
    max_prompt = max(promptTokens, na.rm = TRUE)
  )

cat("Total prompt tokens:", format(token_stats$total_prompt, big.mark = ","), "\n")
cat("Total completion tokens:", format(token_stats$total_completion, big.mark = ","), "\n")
cat("Total tokens:", format(token_stats$total, big.mark = ","), "\n")
cat("Avg prompt tokens:", round(token_stats$avg_prompt, 1), "\n")
cat("Avg completion tokens:", round(token_stats$avg_completion, 1), "\n")
cat("Prompt token range:", token_stats$min_prompt, "-", token_stats$max_prompt, "\n\n")

# Cost estimates
gpt4o_input_price <- 2.50 / 1e6
gpt4o_output_price <- 10.00 / 1e6
gpt4o_mini_input_price <- 0.15 / 1e6
gpt4o_mini_output_price <- 0.60 / 1e6

cost_4o <- token_stats$total_prompt * gpt4o_input_price + 
  token_stats$total_completion * gpt4o_output_price
cost_mini <- token_stats$total_prompt * gpt4o_mini_input_price + 
  token_stats$total_completion * gpt4o_mini_output_price

cat("Cost estimates (this run):\n")
cat("  GPT-4o: $", round(cost_4o, 2), "\n")
cat("  GPT-4o-mini: $", round(cost_mini, 2), "\n\n")

cat("Projected cost (50 runs):\n")
cat("  GPT-4o: $", round(cost_4o * 50, 2), "\n")
cat("  GPT-4o-mini: $", round(cost_mini * 50, 2), "\n")
cat("  Both models: $", round(cost_4o * 50 + cost_mini * 50, 2), "\n")

# =============================================================================
# 6. RESPONSE TIME ANALYSIS
# =============================================================================

if ("duration" %in% names(df) && !all(is.na(df$duration))) {
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
  cat("RESPONSE TIME ANALYSIS\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
  
  cat("Note: Duration is per-request time. With parallel batching, wall clock time is much shorter.\n\n")
  
  time_stats <- df_success %>%
    summarise(
      total_request_time = sum(duration, na.rm = TRUE),
      mean_time = mean(duration, na.rm = TRUE),
      median_time = median(duration, na.rm = TRUE),
      max_time = max(duration, na.rm = TRUE),
      min_time = min(duration, na.rm = TRUE)
    )
  
  cat("Total request time:", round(time_stats$total_request_time / 60, 2), "min\n")
  cat("Mean request time:", round(time_stats$mean_time, 3), "s\n")
  cat("Median request time:", round(time_stats$median_time, 3), "s\n")
  cat("Range:", round(time_stats$min_time, 3), "-", round(time_stats$max_time, 3), "s\n\n")
  
  # Estimate wall clock time (batch_size concurrent, ~35 parallel)
  batch_size <- 35
  estimated_wall_time <- time_stats$total_request_time / batch_size
  cat("Estimated wall clock time (~", batch_size, " parallel):", round(estimated_wall_time / 60, 2), "min\n")
  
  # Per-task breakdown
  cat("\nPer-task timing:\n")
  task_timing <- df_success %>%
    group_by(dataset, task) %>%
    summarise(
      n = n(),
      mean_s = round(mean(duration, na.rm = TRUE), 3),
      median_s = round(median(duration, na.rm = TRUE), 3),
      max_s = round(max(duration, na.rm = TRUE), 3),
      .groups = "drop"
    )
  print(task_timing)
}

# =============================================================================
# 6b. RESOURCE USAGE ANALYSIS
# =============================================================================

if ("resourceName" %in% names(df) && !all(is.na(df$resourceName))) {
  cat("\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
  cat("RESOURCE USAGE\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
  
  resource_stats <- df %>%
    group_by(resourceName) %>%
    summarise(
      requests = n(),
      tokens = sum(promptTokens + completionTokens, na.rm = TRUE),
      errors = sum(hasError == TRUE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(resourceName)
  
  print(resource_stats)
}

# =============================================================================
# 7. SUMMARY
# =============================================================================

cat("\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("SUMMARY\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Get run number from data
run_in_file <- unique(df$run)
run_label <- if (length(run_in_file) == 1 && !is.na(run_in_file)) {
  paste("Run", run_in_file)
} else {
  basename(output_file)
}

cat(run_label, "validation complete.\n")
cat("File:", basename(output_file), "\n")
cat("Total instances:", nrow(df), "\n")
cat("Success rate:", round(mean(df$hasError == FALSE, na.rm = TRUE) * 100, 2), "%\n")
cat("Ready for 50-run production phase:", 
    ifelse(error_count < 20 && mean(df$hasError == FALSE, na.rm = TRUE) > 0.99, "YES ✓", "REVIEW ERRORS"), "\n")

# =============================================================================
# 8. SAVE PARSED DATA (Optional)
# =============================================================================

# Uncomment to save a clean CSV version
# df_clean <- df_success %>%
#   select(id, localId, dataset, task, output, promptTokens, completionTokens)
# write_csv(df_clean, file.path(output_dir, "run_1_clean.csv"))
# cat("\nSaved clean data to run_1_clean.csv\n")

# =============================================================================
# 9. HELPER FUNCTIONS FOR INTERACTIVE USE
# =============================================================================

#' Load a specific run by number
#' @example load_run(5)
load_run <- function(run_num) {
  pattern <- sprintf("run_%03d", run_num)
  files <- list.files(output_dir, pattern = "\\.jsonl$", full.names = TRUE)
  matches <- files[grepl(pattern, files)]
  if (length(matches) == 0) {
    stop("No file found for run ", run_num)
  }
  cat("Loading", basename(matches[1]), "\n")
  load_jsonl(matches[1])
}

#' Quick comparison of classifications between two runs
#' @example compare_runs(1, 2)
compare_runs <- function(run1, run2) {
  df1 <- load_run(run1) %>% filter(hasError == FALSE)
  df2 <- load_run(run2) %>% filter(hasError == FALSE)
  
  # Join on id
  comparison <- df1 %>%
    select(id, dataset, task, output1 = output) %>%
    inner_join(
      df2 %>% select(id, output2 = output),
      by = "id"
    ) %>%
    mutate(match = output1 == output2)
  
  cat(sprintf("\nComparing Run %d vs Run %d:\n", run1, run2))
  cat(sprintf("  Total compared: %d instances\n", nrow(comparison)))
  cat(sprintf("  Matching: %d (%.1f%%)\n", 
              sum(comparison$match), 
              mean(comparison$match) * 100))
  cat(sprintf("  Different: %d (%.1f%%)\n", 
              sum(!comparison$match), 
              mean(!comparison$match) * 100))
  
  # Per-task breakdown
  cat("\nPer-task flip rate:\n")
  task_flips <- comparison %>%
    group_by(dataset, task) %>%
    summarise(
      n = n(),
      flips = sum(!match),
      flip_rate = mean(!match) * 100,
      .groups = "drop"
    )
  print(task_flips)
  
  invisible(comparison)
}

#' List instances that flip between runs
#' @example show_flips(1, 2)
show_flips <- function(run1, run2, max_show = 20) {
  comparison <- compare_runs(run1, run2)
  
  flips <- comparison %>%
    filter(!match) %>%
    head(max_show)
  
  if (nrow(flips) > 0) {
    cat(sprintf("\nFirst %d flipping instances:\n", min(max_show, nrow(flips))))
    print(flips %>% select(id, dataset, task, output1, output2))
  }
  
  invisible(flips)
}

cat("\n---\nHelper functions available:\n")
cat("  load_run(5)        - Load run 5 into a dataframe\n")
cat("  compare_runs(1,2)  - Compare classifications between runs\n")
cat("  show_flips(1,2)    - Show instances that changed between runs\n")