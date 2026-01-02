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

setwd("~/Desktop/Reproduciblity")

# Set paths - adjust as needed
output_dir <- "~/Desktop/Reproduciblity/output"
data_dir <- "~/Desktop/Reproduciblity/data"

# Load the classification output (JSONL format)
load_jsonl <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  map_dfr(lines, ~ fromJSON(.x))
}

# Find the output file (most recent run_1 file)
output_files <- list.files(output_dir, pattern = "run_001.jsonl", full.names = TRUE)
if (length(output_files) == 0) {
  stop("No output file found. Check the output directory.")
}
output_file <- output_files[length(output_files)]  # Most recent
cat("Loading:", output_file, "\n")

df <- load_jsonl(output_file)
cat("Loaded", nrow(df), "records\n\n")

# =============================================================================
# 2. BASIC VALIDATION
# =============================================================================

cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("BASIC VALIDATION\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Check for expected columns
expected_cols <- c("id", "localId", "dataset", "task", "input", "output", 
                   "hasError", "seed", "promptTokens", "completionTokens")
missing_cols <- setdiff(expected_cols, names(df))
if (length(missing_cols) > 0) {
  cat("⚠️  Missing columns:", paste(missing_cols, collapse = ", "), "\n")
} else {
  cat("✓ All expected columns present\n")
}

# Check for errors
error_count <- sum(df$hasError, na.rm = TRUE)
cat("✓ Total records:", nrow(df), "\n")
cat("✓ Successful:", sum(!df$hasError, na.rm = TRUE), "\n")
cat("⚠️  Errors:", error_count, "\n\n")

# List errored instances
if (error_count > 0) {
  cat("Errored instances:\n")
  df %>%
    filter(hasError) %>%
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

task_summary <- df %>%
  group_by(dataset, task) %>%
  summarise(
    n = n(),
    errors = sum(hasError, na.rm = TRUE),
    success_rate = mean(!hasError, na.rm = TRUE) * 100,
    avg_prompt_tokens = mean(promptTokens, na.rm = TRUE),
    avg_completion_tokens = mean(completionTokens, na.rm = TRUE),
    .groups = "drop"
  )

print(task_summary)
cat("\n")

# =============================================================================
# 4. OUTPUT VALIDATION BY TASK
# =============================================================================

cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("OUTPUT VALIDATION BY TASK\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Filter to successful only
df_success <- df %>% filter(!hasError)

# Parse JSON outputs
parse_output <- function(output_str, task) {
  tryCatch({
    parsed <- fromJSON(output_str)
    return(parsed)
  }, error = function(e) {
    return(NULL)
  })
}

# Validate each task type
validate_task <- function(data, task_name, valid_values) {
  task_data <- data %>% filter(task == task_name)
  if (nrow(task_data) == 0) {
    cat("  No data for", task_name, "\n")
    return(NULL)
  }
  
  # Parse outputs
  parsed <- task_data %>%
    mutate(parsed = map(output, ~ fromJSON(.x)))
  
  # Extract classification values based on task type
  if (task_name %in% c("level", "belief")) {
    # Single classification field
    parsed <- parsed %>%
      mutate(classification = map_chr(parsed, ~ .x$classification))
  } else if (task_name == "promise" && unique(task_data$dataset) == "p2") {
    # p2_promise has p1, p2, p3
    parsed <- parsed %>%
      mutate(
        p1 = map_chr(parsed, ~ .x$p1),
        p2 = map_chr(parsed, ~ .x$p2),
        p3 = map_chr(parsed, ~ .x$p3)
      )
  } else {
    # p1_promise single classification
    parsed <- parsed %>%
      mutate(classification = map_chr(parsed, ~ .x$classification))
  }
  
  return(parsed)
}

# --- p1_promise ---
cat("p1_promise:\n")
p1_data <- df_success %>% filter(dataset == "p1", task == "promise")
if (nrow(p1_data) > 0) {
  p1_parsed <- p1_data %>%
    mutate(classification = map_chr(output, ~ fromJSON(.x)$classification))
  
  cat("  Valid values: 0, 1\n")
  cat("  Distribution:\n")
  print(table(p1_parsed$classification))
  
  invalid <- p1_parsed %>% filter(!classification %in% c("0", "1"))
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
      parsed = map(output, ~ fromJSON(.x)),
      p1 = map_chr(parsed, ~ .x$p1),
      p2 = map_chr(parsed, ~ .x$p2),
      p3 = map_chr(parsed, ~ .x$p3)
    )
  
  cat("  Valid values: 0, 1, na\n")
  cat("  P1 distribution:\n")
  print(table(p2_parsed$p1))
  cat("  P2 distribution:\n")
  print(table(p2_parsed$p2))
  cat("  P3 distribution:\n")
  print(table(p2_parsed$p3))
  
  valid_vals <- c("0", "1", "na")
  invalid <- p2_parsed %>%
    filter(!p1 %in% valid_vals | !p2 %in% valid_vals | !p3 %in% valid_vals)
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
    mutate(classification = map_chr(output, ~ fromJSON(.x)$classification))
  
  cat("  Valid values: 0, 1, 2, 3\n")
  cat("  Distribution:\n")
  print(table(l1_parsed$classification))
  
  invalid <- l1_parsed %>% filter(!classification %in% c("0", "1", "2", "3"))
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
    mutate(classification = map_chr(output, ~ fromJSON(.x)$classification))
  
  cat("  Valid values: 0, 1, 2, 3, 4, 5\n")
  cat("  Distribution:\n")
  print(table(l2_parsed$classification))
  
  invalid <- l2_parsed %>% filter(!classification %in% as.character(0:5))
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
    mutate(classification = map_chr(output, ~ fromJSON(.x)$classification))
  
  cat("  Valid values: 0, 1, 2, 3\n")
  cat("  Distribution:\n")
  print(table(l2b_parsed$classification))
  
  invalid <- l2b_parsed %>% filter(!classification %in% c("0", "1", "2", "3"))
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
    mutate(classification = map_chr(output, ~ fromJSON(.x)$classification))
  
  cat("  Valid values: 0, 1, 2, 3, 4, 5, eq, na\n")
  cat("  Distribution:\n")
  print(table(l3_parsed$classification))
  
  valid_vals <- c(as.character(0:5), "eq", "na")
  invalid <- l3_parsed %>% filter(!classification %in% valid_vals)
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
    mutate(classification = map_chr(output, ~ fromJSON(.x)$classification))
  
  valid_beliefs <- c("0", "16", "26", "36", "46", "56", "66", "76", "na")
  cat("  Valid values:", paste(valid_beliefs, collapse = ", "), "\n")
  cat("  Distribution:\n")
  print(table(l3b_parsed$classification))
  
  invalid <- l3b_parsed %>% filter(!classification %in% valid_beliefs)
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
# 6. RESPONSE TIME ANALYSIS (if available)
# =============================================================================

if ("responseTime" %in% names(df)) {
  cat("\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
  cat("RESPONSE TIME ANALYSIS\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
  
  time_stats <- df_success %>%
    group_by(dataset, task) %>%
    summarise(
      mean_time = mean(responseTime, na.rm = TRUE),
      median_time = median(responseTime, na.rm = TRUE),
      max_time = max(responseTime, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(time_stats)
}

# =============================================================================
# 7. SUMMARY
# =============================================================================

cat("\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("SUMMARY\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

cat("Run 1 validation complete.\n")
cat("Total instances:", nrow(df), "\n")
cat("Success rate:", round(mean(!df$hasError) * 100, 2), "%\n")
cat("Ready for 50-run production phase:", 
    ifelse(error_count < 20 && mean(!df$hasError) > 0.99, "YES ✓", "REVIEW ERRORS"), "\n")

# =============================================================================
# 8. SAVE PARSED DATA (Optional)
# =============================================================================

# Uncomment to save a clean CSV version
# df_clean <- df_success %>%
#   select(id, localId, dataset, task, output, promptTokens, completionTokens)
# write_csv(df_clean, file.path(output_dir, "run_1_clean.csv"))
# cat("\nSaved clean data to run_1_clean.csv\n")
