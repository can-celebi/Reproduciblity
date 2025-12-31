/**
 * Dataset-specific JSON schemas for structured output
 * 
 * Key format: "dataset_task"
 * 
 * Datasets & Tasks:
 *   p1_promise  - Promise classification (single person) -> 0/1
 *   p2_promise  - Promise classification (chat with up to 3 people) -> p1/p2/p3: 0/1
 *   l1_level    - Strategic voting level (0-3)
 *   l2_level    - Coordination game level (0-5)
 *   l2_belief   - Coordination game belief (payoff + label salience)
 *   l3_level    - Beauty contest level (0-5, eq, na)
 *   l3_belief   - Beauty contest belief bins (0-76, na)
 */

const schemas = {

    // =========================================================================
    // P1: Promise Classification (single text)
    // 0 = empty talk, 1 = promise
    // =========================================================================
    p1_promise: {
        type: "json_schema",
        json_schema: {
            name: "promise_classification",
            strict: true,
            schema: {
                type: "object",
                properties: {
                    classification: {
                        type: "string",
                        enum: ["0", "1"]
                    }
                },
                required: ["classification"],
                additionalProperties: false
            }
        }
    },

    // =========================================================================
    // P2: Promise Classification (chat with up to 3 players)
    // 0 = empty talk, 1 = promise, per player
    // =========================================================================
    p2_promise: {
        type: "json_schema",
        json_schema: {
            name: "chat_promise_classification",
            strict: true,
            schema: {
                type: "object",
                properties: {
                    p1: {
                        type: "string",
                        enum: ["0", "1"]
                    },
                    p2: {
                        type: "string",
                        enum: ["0", "1"]
                    },
                    p3: {
                        type: "string",
                        enum: ["0", "1"]
                    }
                },
                required: ["p1", "p2", "p3"],
                additionalProperties: false
            }
        }
    },

    // =========================================================================
    // L1: Strategic Voting Level (0-3)
    // =========================================================================
    l1_level: {
        type: "json_schema",
        json_schema: {
            name: "voting_level_classification",
            strict: true,
            schema: {
                type: "object",
                properties: {
                    classification: {
                        type: "string",
                        enum: ["0", "1", "2", "3"]
                    }
                },
                required: ["classification"],
                additionalProperties: false
            }
        }
    },

    // =========================================================================
    // L2: Coordination Game - Level (0-5)
    // =========================================================================
    l2_level: {
        type: "json_schema",
        json_schema: {
            name: "coordination_level_classification",
            strict: true,
            schema: {
                type: "object",
                properties: {
                    classification: {
                        type: "string",
                        enum: ["0", "1", "2", "3", "4", "5"]
                    }
                },
                required: ["classification"],
                additionalProperties: false
            }
        }
    },

    // =========================================================================
    // L2: Coordination Game - Belief (Salience Type)
    // 0 = payoff salience only
    // 1 = label salience only
    // 2 = both payoff and label salience
    // 3 = no salience indicated
    // =========================================================================
    l2_belief: {
        type: "json_schema",
        json_schema: {
            name: "coordination_belief_classification",
            strict: true,
            schema: {
                type: "object",
                properties: {
                    classification: {
                        type: "string",
                        enum: ["0", "1", "2", "3"]
                    }
                },
                required: ["classification"],
                additionalProperties: false
            }
        }
    },

    // =========================================================================
    // L3: Beauty Contest - Level (0-5, eq, na)
    // 0-5 = level of reasoning
    // eq = equilibrium thinker
    // na = cannot determine
    // =========================================================================
    l3_level: {
        type: "json_schema",
        json_schema: {
            name: "bc_level_classification",
            strict: true,
            schema: {
                type: "object",
                properties: {
                    classification: {
                        type: "string",
                        enum: ["0", "1", "2", "3", "4", "5", "eq", "na"]
                    }
                },
                required: ["classification"],
                additionalProperties: false
            }
        }
    },

    // =========================================================================
    // L3: Beauty Contest - Belief (level-0 belief mean bins)
    // 0 = [0-15], 16 = [16-25], 26 = [26-35], 36 = [36-45]
    // 46 = [46-55], 56 = [56-65], 66 = [66-75], 76 = [76-100]
    // na = no belief stated
    // =========================================================================
    l3_belief: {
        type: "json_schema",
        json_schema: {
            name: "bc_belief_classification",
            strict: true,
            schema: {
                type: "object",
                properties: {
                    classification: {
                        type: "string",
                        enum: ["0", "16", "26", "36", "46", "56", "66", "76", "na"]
                    }
                },
                required: ["classification"],
                additionalProperties: false
            }
        }
    },

};

module.exports = { schemas };
