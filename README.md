# LLM Classification Stability Study

## Directory Structure

```
project/
├── js/
│   ├── run.js              # Main execution script
│   ├── viewer.html         # Diagnostic viewer for results
│   ├── package.json        # Dependencies
│   ├── keys/               # (gitignored)
│   │   └── secret.js       # Azure API credentials
│   ├── prompts/
│   │   └── prompts.js      # Dataset-specific prompts
│   └── schemas/
│       └── schemas.js      # Dataset-specific JSON schemas
├── data/
│   ├── instances.csv       # Main input data
│   └── instances_test.csv  # Test data (70 instances)
└── output/
    ├── run_001.jsonl       # Results from run 1
    ├── run_002.jsonl       # Results from run 2
    └── ...
```

## Setup

```bash
cd js
npm install openai csv-parse
```

Then configure:
1. `keys/secret.js` - Add Azure resource endpoints and API keys
2. `prompts/prompts.js` - Add prompts for each dataset_task combination
3. `schemas/schemas.js` - Define JSON schemas for each dataset_task combination

## Multi-Resource Configuration

The script supports multiple Azure OpenAI resources for parallel processing. Each resource has independent rate limits (150K TPM, 900 RPM).

**Current setup: 7 resources = 1,050K TPM total capacity**

Edit `keys/secret.js`:
```javascript
const resources = [
    {
        name: "resource-1",
        endpoint: "https://your-resource-1.openai.azure.com/",
        apiKey: "your-api-key-1",
        apiVersion: "2024-12-01-preview"
    },
    // ... add more resources
];
module.exports = { resources };
```

## Input Data Format

`data/instances.csv` must have these columns:
- `id` - Unique integer identifier (1, 2, 3, ...)
- `localId` - Original ID from source dataset (for linking back to benchmarks)
- `dataset` - Dataset type: p1, p2, l1, l2, l3
- `task` - Classification task: promise, level, belief
- `input` - The text/content to classify

```csv
id,localId,dataset,task,input
1,subject_001,p1,promise,"I promise to cooperate..."
2,apc_301,l2,level,"The payoff matrix shows..."
3,apc_301,l2,belief,"I think they'll pick..."
4,bc_401,l3,level,"I'll pick 33 because..."
```

**Note:** The combination `dataset_task` determines which prompt and schema to use.

### Dataset/Task Combinations

| Key | Dataset | Task | Description |
|-----|---------|------|-------------|
| `p1_promise` | p1 | promise | Single-person promise classification |
| `p2_promise` | p2 | promise | Multi-player chat promise classification |
| `l1_level` | l1 | level | Strategic voting level (0-3) |
| `l2_level` | l2 | level | Asymmetric coordination level (0-5) |
| `l2_belief` | l2 | belief | Asymmetric coordination belief categories |
| `l3_level` | l3 | level | Beauty contest level |
| `l3_belief` | l3 | belief | Beauty contest belief bins |

## Running

### Test run (verify all 7 resources work)
```bash
# Set inputFile: "instances_test.csv" in run.js CONFIG
node run.js 1
# Expected: 70/70 completed, ~10 requests per resource, 0 errors
```

### Single run
```bash
node run.js 1
```

### Multiple runs (50 runs for stability study)
```bash
node run.js 1 50
```

### Resume interrupted run
Just run the same command. The script automatically:
- Reads existing output file
- Skips completed instances  
- Retries instances that had errors
- Continues from where it stopped

```bash
# If interrupted during run 25:
node run.js 25 50
# Resumes run 25, then continues to 50
```

## Output Format (JSONL)

Each line is a JSON object:

```json
{
  "id": 1,
  "localId": "subject_001",
  "dataset": "p1",
  "task": "promise",
  "run": 1,
  "input": "I promise to cooperate...",
  "output": {"classification": "1"},
  "timestamp": 1735123456,
  "model": "gpt-4o-2024-08-06",
  "seed": 31,
  "temperature": 0,
  "fingerprint": "fp_abc123",
  "resource": "resource-3",
  "duration": 0.823,
  "numInputTokens": 456,
  "numOutputTokens": 12,
  "logprobs": [...],
  "hasError": false,
  "error": null
}
```

## Diagnostic Viewer

Open `viewer.html` in a browser:
- Drag & drop multiple JSONL files
- Filter by dataset, task, status, ID
- View token usage and costs
- Inspect logprobs and alternatives for key classification token
- Configurable key token position

## Reading Output in R

```r
library(jsonlite)
library(dplyr)

# Read single run
run1 <- stream_in(file("output/run_001.jsonl"))

# Read all runs
files <- list.files("output", pattern = "run_.*\\.jsonl", full.names = TRUE)
all_runs <- bind_rows(lapply(files, function(f) stream_in(file(f))))

# Extract classification (for single-output tasks)
all_runs$classification <- sapply(all_runs$output, function(x) x$classification)

# For p2_promise (multi-output)
p2_data <- all_runs %>% filter(dataset == "p2")
p2_data$p1_class <- sapply(p2_data$output, function(x) x$p1)
p2_data$p2_class <- sapply(p2_data$output, function(x) x$p2)
p2_data$p3_class <- sapply(p2_data$output, function(x) x$p3)
```

## Configuration

Edit `CONFIG` in `run.js`:

```javascript
const CONFIG = {
    // Model Settings
    deployment: "gpt-4o",
    seed: 31,
    maxTokens: 50,
    temperature: 0,
    topLogprobs: 20,
    
    // Execution Settings (for 7 resources)
    batchSize: 35,              // 5 per resource × 7 resources
    delayBetweenBatches: 1200,  // 1.2 seconds between batches
    
    // Retry Settings
    maxRetries: 5,
    initialRetryDelay: 2000,
    longPauseDelay: 600000,     // 10 min pause after many errors
    rateLimitPause: 60000,      // 1 min pause on rate limit
    
    // Paths
    inputFile: "instances.csv", // or "instances_test.csv" for testing
};
```

## Error Handling

1. **Per-call retry**: 5 attempts with exponential backoff (2s, 4s, 8s, 16s, 32s)
2. **Resource rotation**: On rate limit, immediately switches to different resource
3. **Error recording**: Failed calls saved with `hasError: true`
4. **Auto-retry on resume**: Errors automatically retried when script restarts
5. **Long pause**: 10-minute pause after 20 consecutive errors

## Time Estimates

**With 7 resources (1,050K TPM capacity):**

| Scenario | Time/run | 50 runs |
|----------|----------|---------|
| Current L2 prompt (3210 words) | ~8 min | ~7 hours |
| Shortened L2 prompt (~1500 words) | ~5 min | ~4 hours |

## Checklist Before Running

- [ ] `keys/secret.js` has all 7 resources with correct endpoints and API keys
- [ ] All resources have gpt-4o deployed
- [ ] Content filters disabled/minimized on Azure resources
- [ ] `prompts/prompts.js` has prompts for all 7 dataset_task combinations
- [ ] `schemas/schemas.js` has schemas for all 7 dataset_task combinations  
- [ ] `data/instances.csv` has columns: id, localId, dataset, task, input
- [ ] Test with `instances_test.csv` first (expect ~10 requests per resource)
- [ ] Switch `inputFile` to `"instances.csv"` for production run
- [ ] Run: `node run.js 1 50`

## Cost Estimate

GPT-4o pricing: $2.50/1M input tokens, $10/1M output tokens

Estimate for 3,087 instances × 50 runs: **~$1,000** (varies with prompt lengths)
