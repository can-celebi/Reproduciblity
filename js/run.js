const fs = require("fs");
const path = require("path");
const { parse } = require("csv-parse/sync");
const { AzureOpenAI } = require("openai");

// =============================================================================
// IMPORTS
// =============================================================================

const { resources } = require("./keys/secret");  // Now exports array of resources
const { prompts } = require("./prompts/prompts");
const { schemas } = require("./schemas/schemas");

// =============================================================================
// CONFIGURATION
// =============================================================================

const CONFIG = {
    // Model Settings - can be overridden with MODEL env variable
    // Options: "gpt-4o", "gpt-4o-mini"
    deployment: process.env.MODEL || "gpt-4o",
    seed: 31,
    maxTokens: 50,
    temperature: 0,
    topLogprobs: 20,
    
    // Execution Settings
    // With 7 resources at 150K TPM each = 1,050K TPM total
    // Batch size = 5 per resource × 7 resources = 35
    // batchSize: 35,
    // delayBetweenBatches: 1200,  // 1.2 seconds - conservative for rate limits
    
    // gpt-4o-mini can handle more aggressive settings
    batchSize: 70, // 10 per resource × 7 resources = 70
    delayBetweenBatches: 1000,  // 1 second - more aggressive settings
    
    // Retry Settings
    maxRetries: 5,
    initialRetryDelay: 2000,
    longPauseDelay: 600000,     // 10 minutes
    consecutiveErrorsBeforePause: 20,
    rateLimitPause: 60000,      // 1 minute on rate limit
    
    // Output Settings
    verbose: process.env.VERBOSE === "true",  // Set VERBOSE=true for detailed logs
    
    // Paths - output goes to model-specific subdirectory
    dataDir: "../data",
    get outputDir() {
        // Create model-specific output directory
        // gpt-4o -> ../output/gpt-4o/
        // gpt-4o-mini -> ../output/gpt-4o-mini/
        return `../output/${this.deployment}`;
    },
    // Support DATASET env variable for dashboard, default to instances_test.csv
    inputFile: "instances.csv",
};

// =============================================================================
// MULTI-RESOURCE CLIENT POOL
// =============================================================================

class ClientPool {
    constructor(resourceConfigs) {
        this.clients = resourceConfigs.map((config, index) => ({
            id: index,
            name: config.name || `resource-${index}`,
            client: new AzureOpenAI({
                endpoint: config.endpoint,
                apiKey: config.apiKey,
                apiVersion: config.apiVersion || "2024-08-01-preview",
                deployment: CONFIG.deployment
            }),
            requestCount: 0,
            tokenCount: 0,
            lastUsed: 0,
            errorCount: 0
        }));
        
        this.currentIndex = 0;
        console.log(`Initialized ${this.clients.length} Azure OpenAI resources:`);
        this.clients.forEach(c => console.log(`  - ${c.name}: ${resourceConfigs[c.id].endpoint}`));
    }
    
    // Round-robin selection
    getNextClient() {
        const client = this.clients[this.currentIndex];
        this.currentIndex = (this.currentIndex + 1) % this.clients.length;
        return client;
    }
    
    // Get client with least recent usage (for better distribution)
    getLeastRecentClient() {
        return this.clients.reduce((min, c) => c.lastUsed < min.lastUsed ? c : min);
    }
    
    // Get client with lowest error count
    getHealthiestClient() {
        return this.clients.reduce((min, c) => c.errorCount < min.errorCount ? c : min);
    }
    
    recordUsage(clientWrapper, tokens) {
        clientWrapper.requestCount++;
        clientWrapper.tokenCount += tokens;
        clientWrapper.lastUsed = Date.now();
    }
    
    recordError(clientWrapper) {
        clientWrapper.errorCount++;
    }
    
    getStats() {
        return this.clients.map(c => ({
            name: c.name,
            requests: c.requestCount,
            tokens: c.tokenCount,
            errors: c.errorCount
        }));
    }
    
    printStats() {
        console.log("\n--- Resource Usage ---");
        for (const c of this.clients) {
            console.log(`  ${c.name}: ${c.requestCount} requests, ${c.tokenCount.toLocaleString()} tokens, ${c.errorCount} errors`);
        }
    }
}

// Initialize client pool
const clientPool = new ClientPool(resources);

// =============================================================================
// FILE UTILITIES
// =============================================================================

/**
 * Load instances from CSV
 * Expected columns: id, localId, dataset, task, input
 */
function loadInstances(filePath) {
    const content = fs.readFileSync(filePath, "utf-8");
    const records = parse(content, {
        columns: true,
        skip_empty_lines: true,
        trim: true
    });
    
    console.log(`Loaded ${records.length} instances from ${filePath}`);
    
    if (records.length > 0) {
        const firstRow = records[0];
        const required = ["id", "localId", "dataset", "task", "input"];
        const missing = required.filter(col => !(col in firstRow));
        if (missing.length > 0) {
            throw new Error(`CSV missing required columns: ${missing.join(", ")}`);
        }
    }
    
    return records;
}

/**
 * Load existing results from JSONL file
 */
function loadExistingResults(filePath) {
    const completed = new Set();
    const errors = new Set();
    const all = [];
    
    if (!fs.existsSync(filePath)) {
        console.log(`No existing output file. Starting fresh.`);
        return { completed, errors, all };
    }
    
    const content = fs.readFileSync(filePath, "utf-8");
    const lines = content.trim().split("\n").filter(line => line.length > 0);
    
    for (const line of lines) {
        try {
            const record = JSON.parse(line);
            all.push(record);
            
            if (record.hasError) {
                errors.add(record.id);
            } else {
                completed.add(record.id);
            }
        } catch (e) {
            console.warn(`Could not parse line: ${line.substring(0, 50)}...`);
        }
    }
    
    console.log(`Loaded ${completed.size} completed, ${errors.size} errors from existing file`);
    return { completed, errors, all };
}

function writeAllResults(filePath, results) {
    const content = results.map(r => JSON.stringify(r)).join("\n") + "\n";
    fs.writeFileSync(filePath, content);
}

function appendResult(filePath, result) {
    const line = JSON.stringify(result) + "\n";
    fs.appendFileSync(filePath, line);
}

// =============================================================================
// API CALL WITH RETRY AND RESOURCE ROTATION
// =============================================================================

function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

/**
 * Detect error types
 */
function getErrorType(error) {
    // Content filter - 400 error with specific message
    if (error.status === 400 && 
        (error.message?.toLowerCase().includes('content') ||
         error.message?.toLowerCase().includes('filter') ||
         error.message?.toLowerCase().includes('policy'))) {
        return 'content_filter';
    }
    
    // Rate limit
    if (error.status === 429 || 
        error.code === 'rate_limit_exceeded' ||
        error.message?.toLowerCase().includes('rate')) {
        return 'rate_limit';
    }
    
    // Transient/server errors
    if (error.status >= 500 || error.code === 'ECONNRESET' || error.code === 'ETIMEDOUT') {
        return 'transient';
    }
    
    return 'other';
}

/**
 * Make API call with smart retry logic:
 * - Content filter errors: Try each resource once (no waiting), then fail fast
 * - Rate limit errors: Switch resource + brief pause
 * - Transient errors: Exponential backoff + retry
 */
async function callWithRetry(systemPrompt, userText, schema, instanceId, dataset, task) {
    let lastError;
    let clientWrapper = clientPool.getNextClient();
    const triedResources = new Set();
    let retryStarted = false;
    
    // For content filter, we try each resource once
    // For other errors, we use maxRetries
    const maxAttempts = Math.max(CONFIG.maxRetries, clientPool.clients.length);
    
    for (let attempt = 1; attempt <= maxAttempts; attempt++) {
        try {
            const response = await clientWrapper.client.chat.completions.create({
                messages: [
                    { role: "system", content: systemPrompt },
                    { role: "user", content: userText }
                ],
                seed: CONFIG.seed,
                max_tokens: CONFIG.maxTokens,
                temperature: CONFIG.temperature,
                logprobs: true,
                top_logprobs: CONFIG.topLogprobs,
                response_format: schema
            });
            
            // Record successful usage
            const totalTokens = response.usage.prompt_tokens + response.usage.completion_tokens;
            clientPool.recordUsage(clientWrapper, totalTokens);
            
            // If we had retries, show resolution
            if (retryStarted) {
                console.log(`  └─ ✅ [${instanceId}] RESOLVED on attempt ${attempt} via ${clientWrapper.name}`);
            }
            
            return { success: true, response, resourceName: clientWrapper.name };
            
        } catch (error) {
            lastError = error;
            clientPool.recordError(clientWrapper);
            triedResources.add(clientWrapper.name);
            
            const errorType = getErrorType(error);
            
            // Visual grouping for retry sequences
            if (!retryStarted) {
                console.log(`  ┌─ ⚠️  [${instanceId}] ${dataset}_${task} ERROR SEQUENCE`);
                retryStarted = true;
            }
            console.log(`  │  Attempt ${attempt} on ${clientWrapper.name}: ${errorType}`);
            console.log(`  │  ${error.message.substring(0, 80)}`);
            
            if (errorType === 'content_filter') {
                // Content filter: try another resource WITHOUT waiting
                // If we've tried all resources, fail immediately
                if (triedResources.size >= clientPool.clients.length) {
                    console.log(`  └─ ❌ [${instanceId}] FAILED: Content filter on all ${triedResources.size} resources`);
                    return { success: false, error: lastError, errorType: getErrorType(lastError) };
                }
                // Find a resource we haven't tried yet
                const untried = clientPool.clients.find(c => !triedResources.has(c.name));
                if (untried) {
                    clientWrapper = untried;
                    console.log(`  │  → Trying ${clientWrapper.name}...`);
                } else {
                    console.log(`  └─ ❌ [${instanceId}] FAILED: No more resources to try`);
                    return { success: false, error: lastError, errorType: getErrorType(lastError) };
                }
                // No sleep - try immediately
                
            } else if (errorType === 'rate_limit') {
                if (attempt >= CONFIG.maxRetries) {
                    console.log(`  └─ ❌ [${instanceId}] FAILED: Rate limit after ${attempt} attempts`);
                    break;
                }
                // Rate limit: switch resource + brief pause
                console.log(`  │  → Rate limit, switching resource + 1s pause...`);
                clientWrapper = clientPool.getHealthiestClient();
                await sleep(1000);
                
            } else {
                if (attempt >= CONFIG.maxRetries) {
                    console.log(`  └─ ❌ [${instanceId}] FAILED: ${errorType} after ${attempt} attempts`);
                    break;
                }
                // Transient/other errors: exponential backoff
                const delay = CONFIG.initialRetryDelay * Math.pow(2, attempt - 1);
                console.log(`  │  → Waiting ${delay / 1000}s before retry...`);
                await sleep(delay);
                clientWrapper = clientPool.getNextClient();
            }
        }
    }
    
    return { success: false, error: lastError, errorType: getErrorType(lastError) };
}

// =============================================================================
// CLASSIFICATION
// =============================================================================

async function classifyInstance(instance, runNumber) {
    const { id, localId, dataset, task, input } = instance;
    const key = `${dataset}_${task}`;
    
    const systemPrompt = prompts[key];
    const schema = schemas[key];
    
    if (!systemPrompt) {
        console.error(`  ❌ Missing PROMPT for key: "${key}" (instance ${id})`);
        console.error(`     Available: ${Object.keys(prompts).join(", ")}`);
    }
    if (!schema) {
        console.error(`  ❌ Missing SCHEMA for key: "${key}" (instance ${id})`);
        console.error(`     Available: ${Object.keys(schemas).join(", ")}`);
    }
    
    if (!systemPrompt || !schema) {
        return {
            id: parseInt(id),
            localId,
            dataset,
            task,
            run: runNumber,
            input,
            output: null,
            hasError: true,
            errorType: 'missing_config',
            error: `Missing ${!systemPrompt ? "prompt" : ""}${!systemPrompt && !schema ? " and " : ""}${!schema ? "schema" : ""} for key: ${key}`
        };
    }
    
    const startTime = Date.now();
    const { success, response, error, errorType, resourceName } = await callWithRetry(systemPrompt, input, schema, id, dataset, task);
    const endTime = Date.now();
    
    if (!success) {
        return {
            id: parseInt(id),
            localId,
            dataset,
            task,
            run: runNumber,
            input,
            output: null,
            timestamp: Math.floor(Date.now() / 1000),
            model: CONFIG.deployment,
            seed: CONFIG.seed,
            temperature: CONFIG.temperature,
            fingerprint: null,
            resource: null,
            duration: (endTime - startTime) / 1000,
            numInputTokens: null,
            numOutputTokens: null,
            logprobs: null,
            hasError: true,
            errorType: errorType || 'unknown',
            error: error?.message || "Unknown error"
        };
    }
    
    const choice = response.choices[0];
    let outputParsed;
    let parseError = null;
    
    try {
        outputParsed = JSON.parse(choice.message.content);
    } catch (e) {
        console.error(`  JSON parse error for instance ${id}: ${e.message}`);
        console.error(`  Raw content: ${choice.message.content}`);
        outputParsed = choice.message.content;
        parseError = e.message;
    }
    
    return {
        id: parseInt(id),
        localId,
        dataset,
        task,
        run: runNumber,
        input,
        output: outputParsed,
        timestamp: response.created,
        model: response.model,
        seed: CONFIG.seed,
        temperature: CONFIG.temperature,
        fingerprint: response.system_fingerprint,
        resource: resourceName,
        duration: parseFloat(((endTime - startTime) / 1000).toFixed(3)),
        numInputTokens: response.usage.prompt_tokens,
        numOutputTokens: response.usage.completion_tokens,
        logprobs: choice.logprobs.content,
        hasError: parseError !== null,
        errorType: parseError ? 'parse_error' : null,
        error: parseError
    };
}

// =============================================================================
// BATCH PROCESSING
// =============================================================================

async function processBatch(instances, runNumber, outputPath, existingResults) {
    const tasks = instances.map(async (instance) => {
        const result = await classifyInstance(instance, runNumber);
        
        const existingErrorIndex = existingResults.all.findIndex(
            r => r.id === result.id && r.hasError
        );
        
        if (existingErrorIndex >= 0) {
            existingResults.all[existingErrorIndex] = result;
            writeAllResults(outputPath, existingResults.all);
            console.log(`  [${result.id}] Replaced error -> ${result.hasError ? "ERROR" : "OK"}`);
        } else {
            appendResult(outputPath, result);
            existingResults.all.push(result);
            // Only log errors or if verbose mode is on
            if (result.hasError || CONFIG.verbose) {
                const status = result.hasError ? "ERROR" : "OK";
                console.log(`  [${result.id}] ${result.dataset}_${result.task} ${status} (${result.duration}s) [${result.resource || '-'}]`);
            }
        }
        
        if (result.hasError) {
            existingResults.errors.add(result.id);
            existingResults.completed.delete(result.id);
        } else {
            existingResults.completed.add(result.id);
            existingResults.errors.delete(result.id);
        }
        
        return result;
    });
    
    return await Promise.all(tasks);
}

// =============================================================================
// MAIN EXECUTION
// =============================================================================

async function executeRun(instances, runNumber, outputPath, errorsOnly = false) {
    console.log(`\n${"=".repeat(60)}`);
    console.log(`RUN ${runNumber}${errorsOnly ? ' (ERRORS ONLY)' : ''}`);
    console.log(`${"=".repeat(60)}`);
    
    const existingResults = loadExistingResults(outputPath);
    
    let toProcess;
    if (errorsOnly) {
        // Only process instances that had errors
        toProcess = instances.filter(inst => {
            const id = parseInt(inst.id);
            return existingResults.errors.has(id);
        });
        console.log(`\nMode: Retrying errors only`);
    } else {
        // Normal mode: process new instances + retry errors
        toProcess = instances.filter(inst => {
            const id = parseInt(inst.id);
            return !existingResults.completed.has(id) || existingResults.errors.has(id);
        });
    }
    
    console.log(`\nTotal instances: ${instances.length}`);
    console.log(`Already completed: ${existingResults.completed.size}`);
    console.log(`Previous errors: ${existingResults.errors.size}`);
    console.log(`To process this session: ${toProcess.length}\n`);
    
    if (toProcess.length === 0) {
        if (errorsOnly) {
            console.log(`No errors to retry in run ${runNumber}!`);
        } else {
            console.log(`Run ${runNumber} already complete!`);
        }
        return;
    }
    
    let consecutiveErrors = 0;
    const totalBatches = Math.ceil(toProcess.length / CONFIG.batchSize);
    const runStartTime = Date.now();
    
    for (let i = 0; i < toProcess.length; i += CONFIG.batchSize) {
        const batchNum = Math.floor(i / CONFIG.batchSize) + 1;
        const batch = toProcess.slice(i, i + CONFIG.batchSize);
        
        // Progress estimate
        const elapsed = (Date.now() - runStartTime) / 1000 / 60;
        const rate = i > 0 ? i / elapsed : 0;
        const remaining = rate > 0 ? (toProcess.length - i) / rate : 0;
        
        console.log(`\n--- Batch ${batchNum}/${totalBatches} (${batch.length} instances) | ~${remaining.toFixed(1)} min remaining ---`);
        
        const results = await processBatch(batch, runNumber, outputPath, existingResults);
        
        const batchErrors = results.filter(r => r.hasError).length;
        const batchOK = results.length - batchErrors;
        
        // Show batch summary
        if (batchErrors > 0) {
            console.log(`  ✓ ${batchOK} OK, ✗ ${batchErrors} errors`);
        } else {
            console.log(`  ✓ ${batchOK}/${batch.length} completed`);
        }
        
        if (batchErrors === batch.length) {
            consecutiveErrors += batchErrors;
            console.warn(`\n⚠️  All ${batchErrors} in batch failed!`);
            
            if (consecutiveErrors >= CONFIG.consecutiveErrorsBeforePause) {
                console.warn(`\n🛑 ${consecutiveErrors} consecutive errors. Pausing for 10 minutes...`);
                await sleep(CONFIG.longPauseDelay);
                consecutiveErrors = 0;
            }
        } else {
            consecutiveErrors = 0;
        }
        
        if (i + CONFIG.batchSize < toProcess.length) {
            await sleep(CONFIG.delayBetweenBatches);
        }
    }
    
    // Final summary
    const finalState = loadExistingResults(outputPath);
    
    let totalInputTokens = 0;
    let totalOutputTokens = 0;
    const errorTypes = {};
    for (const r of finalState.all) {
        if (r.numInputTokens) totalInputTokens += r.numInputTokens;
        if (r.numOutputTokens) totalOutputTokens += r.numOutputTokens;
        if (r.hasError && r.errorType) {
            errorTypes[r.errorType] = (errorTypes[r.errorType] || 0) + 1;
        }
    }
    const avgTokens = finalState.completed.size > 0 
        ? Math.round((totalInputTokens + totalOutputTokens) / finalState.completed.size)
        : 0;
    
    console.log(`\n--- Run ${runNumber} Summary ---`);
    console.log(`Completed: ${finalState.completed.size}/${instances.length}`);
    console.log(`Errors: ${finalState.errors.size}`);
    if (Object.keys(errorTypes).length > 0) {
        console.log(`Error breakdown: ${Object.entries(errorTypes).map(([k,v]) => `${k}=${v}`).join(', ')}`);
    }
    console.log(`Tokens: ${totalInputTokens.toLocaleString()} in + ${totalOutputTokens.toLocaleString()} out = ${(totalInputTokens + totalOutputTokens).toLocaleString()} total`);
    console.log(`Avg tokens/request: ${avgTokens}`);
    
    clientPool.printStats();
}

async function main() {
    const args = process.argv.slice(2);
    
    // Parse flags
    const errorsOnly = args.includes('--errors-only') || args.includes('-e');
    const filteredArgs = args.filter(a => !a.startsWith('-'));
    
    const startRun = parseInt(filteredArgs[0]) || 1;
    const endRun = parseInt(filteredArgs[1]) || startRun;
    
    console.log(`\n${"=".repeat(60)}`);
    console.log(`LLM CLASSIFICATION STABILITY STUDY`);
    console.log(`${"=".repeat(60)}`);
    console.log(`Model: ${CONFIG.deployment}`);
    console.log(`Temperature: ${CONFIG.temperature}`);
    console.log(`Seed: ${CONFIG.seed}`);
    console.log(`Batch size: ${CONFIG.batchSize}`);
    console.log(`Resources: ${clientPool.clients.length}`);
    console.log(`Output: ${CONFIG.outputDir}`);
    console.log(`Runs: ${startRun} to ${endRun}`);
    if (errorsOnly) {
        console.log(`Mode: ERRORS ONLY (retrying previous errors)`);
    }
    console.log(`Start time: ${new Date().toISOString()}`);
    
    const scriptDir = __dirname;
    const dataPath = path.join(scriptDir, CONFIG.dataDir, CONFIG.inputFile);
    const outputDir = path.join(scriptDir, CONFIG.outputDir);
    
    if (!fs.existsSync(outputDir)) {
        fs.mkdirSync(outputDir, { recursive: true });
    }
    
    const instances = loadInstances(dataPath);
    
    // Show breakdown
    const breakdown = {};
    for (const inst of instances) {
        const key = `${inst.dataset}_${inst.task}`;
        breakdown[key] = (breakdown[key] || 0) + 1;
    }
    console.log(`\nDataset/Task breakdown:`);
    for (const [key, count] of Object.entries(breakdown).sort()) {
        console.log(`  ${key}: ${count} instances`);
    }
    
    const startTime = Date.now();
    
    for (let run = startRun; run <= endRun; run++) {
        const outputPath = path.join(outputDir, `run_${String(run).padStart(3, "0")}.jsonl`);
        await executeRun(instances, run, outputPath, errorsOnly);
    }
    
    const totalMinutes = (Date.now() - startTime) / 1000 / 60;
    
    console.log(`\n${"=".repeat(60)}`);
    console.log(`ALL DONE`);
    console.log(`End time: ${new Date().toISOString()}`);
    console.log(`Total time: ${totalMinutes.toFixed(1)} minutes`);
    clientPool.printStats();
    console.log(`${"=".repeat(60)}\n`);
}

main().catch(err => {
    console.error("Fatal error:", err);
    process.exit(1);
});
