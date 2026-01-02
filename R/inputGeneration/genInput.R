# =============================================================================
# Build instances.csv for LLM Classification Study
# =============================================================================

library(tidyverse)

# =============================================================================
# LOAD DATA
# =============================================================================

setwd("~/Desktop/Reproduciblity/data")
p1 <- read_csv("p1.csv", show_col_types = FALSE)
p2 <- read_csv("p2.csv", show_col_types = FALSE)
l1 <- read_csv("l1.csv", show_col_types = FALSE)
l2 <- read_csv("l2.csv", show_col_types = FALSE)
l3 <- read_csv("l3.csv", show_col_types = FALSE)

# =============================================================================
# PREPARE EACH DATASET
# =============================================================================

# P1: promise classification (38 rows)
p1_instances <- p1 %>%
  transmute(
    localId = messageId,
    dataset = "p1",
    task = "promise",
    input = Message
  )

cat("p1:", nrow(p1_instances), "instances\n")

# P2: promise classification (719 rows)
p2_instances <- p2 %>%
  transmute(
    localId = as.character(teamID),
    dataset = "p2",
    task = "promise",
    input = CombinedChat
  )

cat("p2:", nrow(p2_instances), "instances\n")

# L1: level classification (493 rows)
l1_instances <- l1 %>%
  transmute(
    localId = id,
    dataset = "l1",
    task = "level",
    input = message
  )

cat("l1:", nrow(l1_instances), "instances\n")

# L2: TWO tasks - level AND belief (851 rows × 2 = 1702 instances)
l2_level <- l2 %>%
  transmute(
    localId = paste(subject.id, message.id, sep = "_"),
    dataset = "l2",
    task = "level",
    input = input
  )

l2_belief <- l2 %>%
  transmute(
    localId = paste(subject.id, message.id, sep = "_"),
    dataset = "l2",
    task = "belief",
    input = input
  )

cat("l2_level:", nrow(l2_level), "instances\n")
cat("l2_belief:", nrow(l2_belief), "instances\n")

# L3: TWO tasks - level AND belief (78 rows × 2 = 156 instances)
l3_level <- l3 %>%
  transmute(
    localId = as.character(message.id),
    dataset = "l3",
    task = "level",
    input = message
  )

l3_belief <- l3 %>%
  transmute(
    localId = as.character(message.id),
    dataset = "l3",
    task = "belief",
    input = message
  )

cat("l3_level:", nrow(l3_level), "instances\n")
cat("l3_belief:", nrow(l3_belief), "instances\n")

# =============================================================================
# COMBINE ALL
# =============================================================================

all_instances <- bind_rows(
  p1_instances,
  p2_instances,
  l1_instances,
  l2_level,
  l2_belief,
  l3_level,
  l3_belief
) %>%
  mutate(id = row_number()) %>%
  select(id, localId, dataset, task, input)

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n========================================\n")
cat("TOTAL INSTANCES:", nrow(all_instances), "\n")
cat("========================================\n\n")

cat("Breakdown:\n")
all_instances %>%
  count(dataset, task) %>%
  print()

# Check for any NA inputs
na_count <- sum(is.na(all_instances$input))
cat("\nNA inputs:", na_count, "\n")

# Check for empty inputs
empty_count <- sum(all_instances$input == "" | is.na(all_instances$input))
cat("Empty inputs:", empty_count, "\n")

# =============================================================================
# SAVE
# =============================================================================

write_csv(all_instances, "instances.csv")
cat("\nSaved to instances.csv\n")

# Preview
cat("\nFirst 10 rows:\n")
print(head(all_instances, 10))

cat("\nLast 10 rows:\n")
print(tail(all_instances, 10))


# =============================================================================
# CREATE TEST SUBSET 
# =============================================================================

# Sample ~10 instances from each dataset_task combination
set.seed(42)  # For reproducibility

test_instances <- all_instances %>%
  group_by(dataset, task) %>%
  slice_sample(n = 10) %>%  # 10 from each group
  ungroup() %>%
  mutate(id = row_number()) %>%  # Re-number IDs
  select(id, localId, dataset, task, input)

cat("\n========================================\n")
cat("TEST SUBSET\n")
cat("========================================\n\n")

cat("Total test instances:", nrow(test_instances), "\n\n")

test_instances %>%
  count(dataset, task) %>%
  print()

# Save test file
write_csv(test_instances, "instances_test.csv")
cat("\nSaved to instances_test.csv\n")

# Preview
cat("\nPreview:\n")
test_instances %>%
  group_by(dataset, task) %>%
  slice_head(n = 2) %>%
  select(id, localId, dataset, task, input = input) %>%
  mutate(input = substr(input, 1, 50)) %>%
  print(n = 20)
