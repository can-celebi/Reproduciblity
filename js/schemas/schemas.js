/**
 * Dataset-specific JSON schemas for structured output
 * 
 * Key format: "dataset_task"
 * 
 * Datasets & Tasks:
 *   p1_promise  - Promise classification (single person)
 *   p2_promise  - Promise classification (chat with up to 3 people)
 *   l1_level    - Strategic voting level (0-3)
 *   l2_level    - Asymmetric payoff coordination level (0-5)
 *   l2_belief   - APC belief categories (0-8)
 *   l3_level    - Beauty contest level
 *   l3_belief   - Beauty contest belief bins
 * 
 * Note: Using "classification" as the standard field name for single-output tasks.
 * For p2_promise which has multiple outputs, we use p1, p2, p3.
 */

const schemas = {

    // =========================================================================
    // P1: Promise Classification (single text)
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
                        enum: ["0", "1", "NA"]
                        // 0 = not promise, 1 = promise, NA = cannot determine
                    }
                },
                required: ["classification"],
                additionalProperties: false
            }
        }
    },

    // =========================================================================
    // P2: Promise Classification (chat with up to 3 players)
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
                        enum: ["0", "1", "NA"]
                    },
                    p2: {
                        type: "string",
                        enum: ["0", "1", "NA"]
                    },
                    p3: {
                        type: "string",
                        enum: ["0", "1", "NA"]
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
    // L2: Asymmetric Payoff Coordination - Level (0-5)
    // =========================================================================
    l2_level: {
        type: "json_schema",
        json_schema: {
            name: "apc_level_classification",
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
    // L2: Asymmetric Payoff Coordination - Belief Categories
    // Categories (adjust labels as needed):
    //   0 = payoff_high
    //   1 = payoff_low  
    //   2 = no_payoff
    //   3 = label_x
    //   4 = label_y
    //   5 = label_hash (#)
    //   6 = label_para (¶)
    //   7 = label_dollar ($)
    //   8 = no_label
    // =========================================================================
    l2_belief: {
        type: "json_schema",
        json_schema: {
            name: "apc_belief_classification",
            strict: true,
            schema: {
                type: "object",
                properties: {
                    classification: {
                        type: "string",
                        enum: ["0", "1", "2", "3", "4", "5", "6", "7", "8"]
                    }
                },
                required: ["classification"],
                additionalProperties: false
            }
        }
    },

    // =========================================================================
    // L3: Beauty Contest - Level
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
                        enum: ["0", "1", "2", "3", "4", "eq", "na"]
                        // 0-4 = level of reasoning
                        // eq = equilibrium thinker
                        // na = cannot determine
                    }
                },
                required: ["classification"],
                additionalProperties: false
            }
        }
    },

    // =========================================================================
    // L3: Beauty Contest - Belief (mean of level-0 distribution)
    // Bins labeled by lower bound:
    //   0  = [0, 16)
    //   16 = [16, 26)
    //   26 = [26, 36)
    //   ... etc
    //   76 = [76, 100]
    //   none = no belief stated
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
                        enum: ["0", "16", "26", "36", "46", "56", "66", "76", "none"]
                    }
                },
                required: ["classification"],
                additionalProperties: false
            }
        }
    },

};

module.exports = { schemas };
