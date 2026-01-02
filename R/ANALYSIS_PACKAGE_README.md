# LLM Classification Stability Analysis Package

## Overview

This package analyzes the stability and reproducibility of LLM classifications across multiple runs, and develops a confidence-gated verification protocol that reduces costs while maintaining reliability guarantees.

## Quick Start

```r
# Step 1: Load and analyze data
MODEL <- "gpt-4o"  # or "gpt-4o-mini"
source("stability_analysis.R")
results <- run_stability_analysis(force_reload = TRUE)

source("confidence_analysis.R")
conf_results <- run_confidence_analysis(results, force_reload = TRUE)

# Step 2: Validate core finding (leakage-free)
source("leakage_free_analysis.R")
lf <- leakage_free_analysis(results, conf_results)
cv <- cross_validate_runs(results, conf_results, max_runs = 10)

# Step 3: Run headline analysis (Pareto frontier)
source("final_analysis.R")
final <- run_final_analysis(results, conf_results)

# Step 4: Calibration grid search (learn τ cheaply)
source("calibration_grid_search_v2.R")
grid <- run_grid_search(results, conf_results,
                        target_instability_rate = 0.01,
                        n_bootstrap = 20)
analyze_grid_results(grid)
```

---

## Script Reference

### ESSENTIAL (Always Needed)

| Script | Purpose | Key Functions |
|--------|---------|---------------|
| `stability_analysis.R` | Load data, parse classifications, compute 50-run stability | `run_stability_analysis()` |
| `confidence_analysis.R` | Extract logprobs, compute confidence metrics | `run_confidence_analysis()` |

### ANALYSIS (For Paper)

| Script | Purpose | Key Functions |
|--------|---------|---------------|
| `leakage_free_analysis.R` | Validate confidence→stability relationship without data leakage | `leakage_free_analysis()`, `cross_validate_runs()` |
| `final_analysis.R` | Pareto frontier: Gated vs Uniform verification | `run_final_analysis()`, `build_pareto_frontier()` |
| `calibration_grid_search_v2.R` | Find cheapest (X%, K) to learn τ | `run_grid_search()`, `quick_grid_search()` |

### DEPRECATED (Do Not Use)

| Script | Reason |
|--------|--------|
| `protocol_validation_v2.R` | Uses ERROR metric; we switched to INSTABILITY |
| `protocol_calibration.R` | Older version, superseded by grid search |
| `run_analysis_v2.R` | Outdated master script |

---

## Data Flow

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           DATA PIPELINE                                  │
└─────────────────────────────────────────────────────────────────────────┘

    output/{model}/run_*.jsonl          # Raw API responses (50 runs)
              │
              ▼
    ┌─────────────────────┐
    │ stability_analysis.R │
    └─────────────────────┘
              │
              ├─► results$parsed      # Long-format: (id, dataset, task, player, run, classification)
              ├─► results$stability   # Per-item: modal_class, modal_freq, flip_rate, is_stable
              └─► results$task_summary
              │
              ▼
    ┌─────────────────────┐
    │ confidence_analysis.R│
    └─────────────────────┘
              │
              ├─► conf_results$confidence_raw  # Per-run confidence (top1_prob, margin, entropy)
              ├─► conf_results$confidence_agg  # Aggregated by item
              └─► conf_results$merged          # Merged with stability
              │
              ▼
    ┌─────────────────────────────────────────────────────────────────────┐
    │                         ANALYSIS SCRIPTS                             │
    ├─────────────────────────────────────────────────────────────────────┤
    │                                                                      │
    │  leakage_free_analysis.R     final_analysis.R     calibration_grid  │
    │  ─────────────────────────   ─────────────────    ─────────────────  │
    │  • Validates AUC ~0.98       • Pareto frontier    • Learn τ cheaply  │
    │  • Cross-run reliability     • Gated vs Uniform   • (X%, K) search   │
    │  • For PAPER validation      • For PAPER results  • For PROTOCOL     │
    │                                                                      │
    └─────────────────────────────────────────────────────────────────────┘
```

---

## Key Objects

### `results` (from stability_analysis.R)

```r
results$parsed      # tibble: id, dataset, task, player, run, classification, logprobs
results$stability   # tibble: id, dataset, task, player, modal_class, modal_freq, flip_rate, is_stable
results$task_summary # Summary statistics by task
results$model       # Which model was analyzed
```

### `conf_results` (from confidence_analysis.R)

```r
conf_results$confidence_raw  # Per-run: id, run, top1_prob, margin, norm_entropy
conf_results$confidence_agg  # Aggregated: mean_top1_prob, sd_top1_prob, etc.
conf_results$merged          # Joined with stability metrics
```

---

## Key Metrics

| Metric | Definition | Used For |
|--------|------------|----------|
| `is_stable` | Modal frequency = 1.0 (no flips in 50 runs) | Binary stability |
| `is_unstable` | Any flip in 50 runs | Calibration target |
| `modal_freq` (TAR) | Fraction of runs matching modal | Continuous stability |
| `flip_rate` | 1 - modal_freq | Continuous instability |
| `top1_prob` | Normalized probability of top prediction | Confidence score |
| `is_error` | Run 1 ≠ modal of 50 | Accuracy metric |

---

## The Protocol

### Goal
Guarantee ≤X% instability among accepted classifications while minimizing API costs.

### Steps

```
1. INITIAL RUN: Classify all N items once (get confidence scores)
   Cost: N

2. CALIBRATE: Sample X% of items stratified by confidence
   - Run each K times
   - Learn τ such that instability rate above τ ≤ target
   Cost: X% × K × N

3. APPLY: 
   - If confidence ≥ τ → ACCEPT
   - If confidence < τ → VERIFY with K_verify runs, take modal
   Cost: N + (flagged%) × K_verify × N

Total: N × (1 + X%×K + flagged%×K_verify)
```

### Headline Result

| Method | Cost | Error vs Oracle |
|--------|------|-----------------|
| Single run | 1.00N | 1.41% |
| Gated 3× (τ=0.80) | 1.22N | 0.86% |
| **Gated 5× (τ=0.70)** | **1.27N** | **0.73%** |
| Always 3× | 3.00N | 0.84% |

**Gated 5× achieves 13% lower error at 58% lower cost than Always-3×.**

---

## Workflow for New Model

```r
# 1. Set model
MODEL <- "gpt-4o-mini"

# 2. Load data (assumes output/{MODEL}/run_*.jsonl exists)
source("stability_analysis.R")
results <- run_stability_analysis(force_reload = TRUE)

source("confidence_analysis.R")
conf_results <- run_confidence_analysis(results, force_reload = TRUE)

# 3. Validate core finding
source("leakage_free_analysis.R")
lf <- leakage_free_analysis(results, conf_results)

# 4. Run Pareto analysis
source("final_analysis.R")
final <- run_final_analysis(results, conf_results)

# 5. (Optional) Full calibration grid search
source("calibration_grid_search_v2.R")
grid <- run_grid_search(results, conf_results,
                        target_instability_rate = 0.01,
                        n_bootstrap = 20)
saveRDS(grid, sprintf("calibration_grid_%s.rds", MODEL))
```

---

## Future: Human Accuracy Benchmarks

When ready to compare with human annotations:

```r
# TODO: Load human annotations from CSV
# human_labels <- read_csv("human_annotations.csv")

# Join with our data
# accuracy <- results$stability %>%
#   left_join(human_labels, by = c("id", "dataset", "task", "player")) %>%
#   mutate(
#     llm_correct = (modal_class == human_label),
#     run1_correct = (class_run1 == human_label)
#   )

# Then we can compute:
# - LLM accuracy vs human ground truth
# - Does confidence predict accuracy?
# - Error rate in confident vs uncertain regions
```

---

## Files in This Package

```
R/
├── stability_analysis.R        # ESSENTIAL: Data loading & stability
├── confidence_analysis.R       # ESSENTIAL: Confidence extraction
├── leakage_free_analysis.R     # PAPER: Validates core finding
├── final_analysis.R            # PAPER: Pareto frontier
├── calibration_grid_search_v2.R # PROTOCOL: Learn τ cheaply
└── ANALYSIS_PACKAGE_README.md  # This file

output/
├── gpt-4o/
│   └── run_*.jsonl             # Raw data (50 runs)
└── gpt-4o-mini/
    └── run_*.jsonl             # Raw data (50 runs)

results/
├── calibration_grid_full.rds   # Saved grid search results
├── pareto_frontier.rds         # Saved Pareto data
└── figures/                    # Publication figures
```
