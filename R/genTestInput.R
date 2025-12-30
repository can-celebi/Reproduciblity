# =============================================================================
# CREATE TEST SUBSET (add this to the end of build_instances.R)
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