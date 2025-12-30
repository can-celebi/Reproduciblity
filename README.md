# LLM Classification Stability Study

## Directory Structure

```
project/
├── js/
│   ├── run.js              # Main execution script
│   ├── package.json        # Dependencies
│   ├── keys/
│   │   └── secret.js       # Azure API credentials (DO NOT COMMIT)
│   ├── prompts/
│   │   └── prompts.js      # Dataset-specific prompts
│   └── schemas/
│       └── schemas.js      # Dataset-specific JSON schemas
├── data/
│   └── instances.csv       # Input data (id, dataset, text)
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

Then edit:
1. `keys/secret.js` - Add your Azure credentials
2. `prompts/prompts.js` - Add your prompts for each dataset
3. `schemas/schemas.js` - Define classification categories for each dataset

## Input Data Format

`data/instances.csv` must have these columns:
- `id` - Unique integer identifier (1, 2, 3, ...)
- `dataset` - Dataset type (bc, p1, p2, l1, l2, l3)
- `text` - The text to classify

```csv
id,dataset,text
1,bc,"i think most people will go for 50?"
2,bc,"The equilibrium is 0"
3,p1,"I will choose A because..."
```

The `dataset` value determines which prompt and schema are used.

## Running

### Single run
```bash
node run.js 1
```

### Multiple runs
```bash
node run.js 1 20    # Runs 1 through 20
```

### Resume interrupted run
Just run the same command. The script:
1. Reads existing output file
2. Skips completed instances
3. Retries instances that had errors
4. Continues from where it stopped

```bash
# If interrupted during run 5:
node run.js 5 20
# Resumes run 5, then continues to 20
```

## Output Format (JSONL)

Each line is a JSON object:

```json
{
  "id": 1,
  "dataset": "bc",
  "run": 1,
  "text": "i think most people will go for 50?",
  "output": {"level": "1", "belief": "46"},
  "timestamp": 1735123456,
  "model": "gpt-4o-2024-08-06",
  "seed": 31,
  "temperature": 0,
  "fingerprint": "fp_abc123",
  "duration": 0.823,
  "numInputTokens": 456,
  "numOutputTokens": 12,
  "logprobs": [...],
  "hasError": false,
  "error": null
}
```

## Reading Output in R

```r
library(jsonlite)

# Read single run
run1 <- stream_in(file("output/run_001.jsonl"))

# Read all runs
files <- list.files("output", pattern = "run_.*\\.jsonl", full.names = TRUE)
all_runs <- do.call(rbind, lapply(files, function(f) stream_in(file(f))))

# Extract classification
all_runs$level <- sapply(all_runs$output, function(x) x$level)
all_runs$belief <- sapply(all_runs$output, function(x) x$belief)
```

## Configuration

Edit `CONFIG` in `run.js`:

```javascript
const CONFIG = {
    deployment: "gpt-4o",
    seed: 31,
    temperature: 0,
    topLogprobs: 20,
    
    batchSize: 5,              // Concurrent API calls
    delayBetweenBatches: 1000, // ms between batches
    
    maxRetries: 5,             // Retries per failed call
    initialRetryDelay: 2000,   // Exponential backoff start
    longPauseDelay: 600000,    // 10 min pause after many errors
};
```

## Error Handling

The script handles errors robustly:

1. **Per-call retry**: Each API call retries up to 5 times with exponential backoff (2s, 4s, 8s, 16s, 32s)

2. **Error recording**: Failed calls are saved with `hasError: true`

3. **Error retry on resume**: When you restart, instances with errors are automatically retried

4. **Rate limit protection**: If 10+ consecutive calls fail, pauses for 10 minutes

## Time Estimates

With `batchSize: 5` and ~1.5s average response time:

| Instances | Runs | Approximate Time |
|-----------|------|------------------|
| 3,087 | 1 | ~15 min |
| 3,087 | 10 | ~2.5 hours |
| 3,087 | 20 | ~5 hours |
| 3,087 | 30 | ~7.5 hours |

## Checklist Before Running

- [ ] `keys/secret.js` has correct Azure credentials
- [ ] `prompts/prompts.js` has prompts for all datasets in your CSV
- [ ] `schemas/schemas.js` has schemas for all datasets in your CSV
- [ ] `data/instances.csv` has columns: id, dataset, text
- [ ] All dataset values in CSV have matching prompt and schema
- [ ] Test with `node run.js 1` on small dataset first
