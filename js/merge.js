const fs = require("fs");
const path = require("path");

/**
 * Merge all JSONL run files into a single JSON array or CSV
 * 
 * Usage: 
 *   node merge.js json   -> outputs combined.json
 *   node merge.js csv    -> outputs combined.csv
 */

const OUTPUT_DIR = path.join(__dirname, "../output");

function loadAllResults() {
    const results = [];
    
    const files = fs.readdirSync(OUTPUT_DIR)
        .filter(f => f.startsWith("run_") && f.endsWith(".jsonl"))
        .sort();
    
    console.log(`Found ${files.length} run files`);
    
    for (const file of files) {
        const filePath = path.join(OUTPUT_DIR, file);
        const content = fs.readFileSync(filePath, "utf-8");
        const lines = content.trim().split("\n").filter(l => l.length > 0);
        
        for (const line of lines) {
            try {
                results.push(JSON.parse(line));
            } catch (e) {
                console.warn(`Could not parse line in ${file}`);
            }
        }
    }
    
    console.log(`Loaded ${results.length} total results`);
    return results;
}

function exportJSON(results) {
    const outputPath = path.join(OUTPUT_DIR, "combined.json");
    fs.writeFileSync(outputPath, JSON.stringify(results, null, 2));
    console.log(`Exported to ${outputPath}`);
}

function exportCSV(results) {
    // Flatten classification and extract key logprob info
    const rows = results.map(r => {
        // Get top logprob for first token (the classification)
        let topLogprob = null;
        let topToken = null;
        if (r.logprobs && r.logprobs.length > 0) {
            const first = r.logprobs[0];
            topToken = first.token;
            topLogprob = first.logprob;
        }
        
        return {
            instanceId: r.instanceId,
            dataset: r.dataset,
            run: r.run,
            level: r.classification?.level || "",
            belief: r.classification?.belief || "",
            timestamp: r.timestamp,
            fingerprint: r.fingerprint,
            durationMs: r.durationMs,
            promptTokens: r.promptTokens,
            completionTokens: r.completionTokens,
            topToken: topToken,
            topLogprob: topLogprob,
            hasError: r.hasError
        };
    });
    
    // Create CSV
    const headers = Object.keys(rows[0]);
    const csvLines = [
        headers.join(","),
        ...rows.map(row => headers.map(h => {
            const val = row[h];
            // Quote strings that might contain commas
            if (typeof val === "string" && val.includes(",")) {
                return `"${val}"`;
            }
            return val ?? "";
        }).join(","))
    ];
    
    const outputPath = path.join(OUTPUT_DIR, "combined.csv");
    fs.writeFileSync(outputPath, csvLines.join("\n"));
    console.log(`Exported to ${outputPath}`);
}

function exportLogprobsDetailed(results) {
    // Export full logprobs for detailed analysis
    const rows = [];
    
    for (const r of results) {
        if (!r.logprobs) continue;
        
        for (let tokenIdx = 0; tokenIdx < r.logprobs.length; tokenIdx++) {
            const tokenData = r.logprobs[tokenIdx];
            
            // Top token
            rows.push({
                instanceId: r.instanceId,
                run: r.run,
                tokenPosition: tokenIdx,
                token: tokenData.token,
                logprob: tokenData.logprob,
                rank: 0
            });
            
            // Alternative tokens
            if (tokenData.top_logprobs) {
                for (let rank = 0; rank < tokenData.top_logprobs.length; rank++) {
                    const alt = tokenData.top_logprobs[rank];
                    rows.push({
                        instanceId: r.instanceId,
                        run: r.run,
                        tokenPosition: tokenIdx,
                        token: alt.token,
                        logprob: alt.logprob,
                        rank: rank + 1
                    });
                }
            }
        }
    }
    
    const headers = ["instanceId", "run", "tokenPosition", "token", "logprob", "rank"];
    const csvLines = [
        headers.join(","),
        ...rows.map(row => headers.map(h => {
            const val = row[h];
            if (typeof val === "string" && (val.includes(",") || val.includes('"'))) {
                return `"${val.replace(/"/g, '""')}"`;
            }
            return val ?? "";
        }).join(","))
    ];
    
    const outputPath = path.join(OUTPUT_DIR, "logprobs_detailed.csv");
    fs.writeFileSync(outputPath, csvLines.join("\n"));
    console.log(`Exported detailed logprobs to ${outputPath}`);
}

// Main
const format = process.argv[2] || "json";
const results = loadAllResults();

if (format === "csv") {
    exportCSV(results);
} else if (format === "logprobs") {
    exportLogprobsDetailed(results);
} else {
    exportJSON(results);
}
