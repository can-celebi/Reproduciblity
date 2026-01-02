const http = require("http");
const fs = require("fs");
const path = require("path");
const { spawn } = require("child_process");
const WebSocket = require("ws");

// =============================================================================
// CONFIGURATION
// =============================================================================

const PORT = 3000;
const HTML_FILE = path.join(__dirname, "dashboard.html");

// =============================================================================
// STATE
// =============================================================================

let currentProcess = null;
let runState = {
    status: "idle", // idle, running, completed, error
    currentRun: 0,
    totalRuns: 0,
    startRun: 0,
    endRun: 0,
    instancesTotal: 0,
    instancesCompleted: 0,
    instancesErrors: 0,
    currentBatch: 0,
    totalBatches: 0,
    startTime: null,
    runHistory: [],
    errors: [],
    tokensIn: 0,
    tokensOut: 0,
    logs: [],
    dataset: "instances_test.csv",
    lastErrorTask: null  // Track task name for error reporting
};

// WebSocket clients
let wsClients = new Set();

// =============================================================================
// BROADCAST TO ALL CLIENTS
// =============================================================================

function broadcast(data) {
    const message = JSON.stringify(data);
    wsClients.forEach(client => {
        if (client.readyState === WebSocket.OPEN) {
            client.send(message);
        }
    });
}

function addLog(text, type = "info") {
    const entry = {
        time: new Date().toISOString().substr(11, 8),
        text,
        type
    };
    runState.logs.push(entry);
    if (runState.logs.length > 500) runState.logs.shift();
    broadcast({ type: "log", entry });
}

// =============================================================================
// PARSE RUN.JS OUTPUT
// =============================================================================

function parseOutput(line) {
    // Run header - match "RUN 1" on its own line (=== are separate lines)
    const runMatch = line.match(/^RUN\s+(\d+)/);
    if (runMatch) {
        runState.currentRun = parseInt(runMatch[1]);
        // Reset per-run counters for this run
        runState.instancesCompleted = 0;
        runState.currentBatch = 0;
        broadcast({ type: "state", state: runState });
        return;
    }

    // Batch progress
    const batchMatch = line.match(/Batch (\d+)\/(\d+)/);
    if (batchMatch) {
        runState.currentBatch = parseInt(batchMatch[1]);
        runState.totalBatches = parseInt(batchMatch[2]);
        broadcast({ type: "state", state: runState });
        return;
    }

    // NEW: Batch summary - "✓ 35/35 completed" or "✓ 33 OK, ✗ 2 errors"
    const batchCompleteMatch = line.match(/✓\s+(\d+)\/(\d+)\s+completed/);
    if (batchCompleteMatch) {
        runState.instancesCompleted += parseInt(batchCompleteMatch[1]);
        broadcast({ type: "state", state: runState });
        return;
    }
    
    const batchMixedMatch = line.match(/✓\s+(\d+)\s+OK,\s+✗\s+(\d+)\s+errors/);
    if (batchMixedMatch) {
        runState.instancesCompleted += parseInt(batchMixedMatch[1]);
        // Errors are tracked separately via FAILED lines
        broadcast({ type: "state", state: runState });
        return;
    }

    // LEGACY: Instance completion - count successful completions (for verbose mode)
    const okMatch = line.match(/\[(\d+)\] \w+ OK/);
    if (okMatch) {
        runState.instancesCompleted++;
        broadcast({ type: "state", state: runState });
        return;
    }

    // Instance FINAL error - only match the definitive FAILED line
    // Pattern: └─ ❌ [52] FAILED: ...
    const finalErrorMatch = line.match(/└─ ❌ \[(\d+)\] FAILED/);
    if (finalErrorMatch) {
        const instanceId = parseInt(finalErrorMatch[1]);
        runState.instancesErrors++;
        runState.errors.push({
            id: instanceId,
            task: runState.lastErrorTask || "unknown",
            run: runState.currentRun
        });
        runState.lastErrorTask = null; // Clear after use
        broadcast({ type: "state", state: runState });
        return;
    }
    
    // Track the task name when error sequence starts (for labeling)
    const errorSeqMatch = line.match(/⚠️\s*\[(\d+)\]\s+(\w+)\s+ERROR SEQUENCE/);
    if (errorSeqMatch) {
        runState.lastErrorTask = errorSeqMatch[2];
        return;
    }

    // Run summary - extract from "Completed: X/Y"
    const summaryMatch = line.match(/^Completed: (\d+)\/(\d+)/);
    if (summaryMatch) {
        const completed = parseInt(summaryMatch[1]);
        const total = parseInt(summaryMatch[2]);
        const errors = total - completed;
        runState.runHistory.push({
            run: runState.currentRun,
            completed,
            total,
            errors,
            time: new Date().toISOString()
        });
        broadcast({ type: "state", state: runState });
        return;
    }

    // Token summary
    const tokenMatch = line.match(/Tokens: ([\d,]+) in \+ ([\d,]+) out/);
    if (tokenMatch) {
        runState.tokensIn += parseInt(tokenMatch[1].replace(/,/g, ""));
        runState.tokensOut += parseInt(tokenMatch[2].replace(/,/g, ""));
        broadcast({ type: "state", state: runState });
        return;
    }

    // Total instances loaded
    const loadedMatch = line.match(/Loaded (\d+) instances/);
    if (loadedMatch) {
        runState.instancesTotal = parseInt(loadedMatch[1]);
        broadcast({ type: "state", state: runState });
        return;
    }

    // All done
    if (line.includes("ALL DONE")) {
        runState.status = "completed";
        broadcast({ type: "state", state: runState });
        return;
    }
}

// =============================================================================
// START RUN
// =============================================================================

function startRun(startRun, endRun, dataset) {
    if (currentProcess) {
        return { error: "A run is already in progress" };
    }

    // Reset state
    runState = {
        status: "running",
        currentRun: startRun,
        totalRuns: endRun - startRun + 1,
        startRun,
        endRun,
        instancesTotal: 0,
        instancesCompleted: 0,
        instancesErrors: 0,
        currentBatch: 0,
        totalBatches: 0,
        startTime: Date.now(),
        runHistory: [],
        errors: [],
        tokensIn: 0,
        tokensOut: 0,
        logs: [],
        dataset,
        lastErrorTask: null
    };

    addLog(`Starting runs ${startRun} to ${endRun} with dataset ${dataset}`, "info");

    // Spawn the process
    currentProcess = spawn("node", ["run.js", startRun.toString(), endRun.toString()], {
        cwd: __dirname,
        env: { ...process.env, DATASET: dataset }
    });

    currentProcess.stdout.on("data", (data) => {
        const lines = data.toString().split("\n");
        lines.forEach(line => {
            if (line.trim()) {
                addLog(line, "stdout");
                parseOutput(line);
            }
        });
    });

    currentProcess.stderr.on("data", (data) => {
        const lines = data.toString().split("\n");
        lines.forEach(line => {
            if (line.trim()) {
                addLog(line, "stderr");
            }
        });
    });

    currentProcess.on("close", (code) => {
        addLog(`Process exited with code ${code}`, code === 0 ? "info" : "error");
        runState.status = code === 0 ? "completed" : "error";
        currentProcess = null;
        broadcast({ type: "state", state: runState });
    });

    currentProcess.on("error", (err) => {
        addLog(`Process error: ${err.message}`, "error");
        runState.status = "error";
        currentProcess = null;
        broadcast({ type: "state", state: runState });
    });

    broadcast({ type: "state", state: runState });
    return { success: true };
}

function stopRun() {
    if (currentProcess) {
        currentProcess.kill("SIGTERM");
        addLog("Run stopped by user", "warning");
        runState.status = "stopped";
        currentProcess = null;
        broadcast({ type: "state", state: runState });
        return { success: true };
    }
    return { error: "No run in progress" };
}

// =============================================================================
// HTTP SERVER
// =============================================================================

const server = http.createServer((req, res) => {
    // Serve dashboard HTML
    if (req.url === "/" || req.url === "/dashboard") {
        fs.readFile(HTML_FILE, (err, data) => {
            if (err) {
                res.writeHead(500);
                res.end("Error loading dashboard");
                return;
            }
            res.writeHead(200, { "Content-Type": "text/html" });
            res.end(data);
        });
        return;
    }

    // API: Start run
    if (req.url === "/api/start" && req.method === "POST") {
        let body = "";
        req.on("data", chunk => body += chunk);
        req.on("end", () => {
            try {
                const { startRun: sr, endRun: er, dataset } = JSON.parse(body);
                const result = startRun(sr || 1, er || 1, dataset || "instances_test.csv");
                res.writeHead(200, { "Content-Type": "application/json" });
                res.end(JSON.stringify(result));
            } catch (e) {
                res.writeHead(400, { "Content-Type": "application/json" });
                res.end(JSON.stringify({ error: e.message }));
            }
        });
        return;
    }

    // API: Stop run
    if (req.url === "/api/stop" && req.method === "POST") {
        const result = stopRun();
        res.writeHead(200, { "Content-Type": "application/json" });
        res.end(JSON.stringify(result));
        return;
    }

    // API: Get state
    if (req.url === "/api/state") {
        res.writeHead(200, { "Content-Type": "application/json" });
        res.end(JSON.stringify(runState));
        return;
    }

    // 404
    res.writeHead(404);
    res.end("Not found");
});

// =============================================================================
// WEBSOCKET SERVER
// =============================================================================

const wss = new WebSocket.Server({ server });

wss.on("connection", (ws) => {
    wsClients.add(ws);
    // Send current state to new client
    ws.send(JSON.stringify({ type: "state", state: runState }));
    
    ws.on("close", () => {
        wsClients.delete(ws);
    });
});

// =============================================================================
// START SERVER
// =============================================================================

server.listen(PORT, () => {
    console.log(`
╔════════════════════════════════════════════════════════════╗
║           LLM CLASSIFICATION STABILITY DASHBOARD           ║
╠════════════════════════════════════════════════════════════╣
║  Open in browser: http://localhost:${PORT}                    ║
║  Press Ctrl+C to stop the server                           ║
╚════════════════════════════════════════════════════════════╝
`);
});
