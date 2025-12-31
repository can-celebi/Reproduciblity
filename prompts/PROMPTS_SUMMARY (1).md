# Standardized Prompts Summary (v2)

## Unified Structure

All 7 prompts follow this consistent structure:

```
# General Task
# Role Persona
# Context
  ## Experiment
  ## Theory (l1-l3)
  ## Input Format (l2 only)
# Classification Task
# Classification Coding
# Constraints
# Examples
```

## Prompt Overview

| Prompt | Task | Output Schema | Categories |
|--------|------|---------------|------------|
| `p1_promise` | Promise detection in trust game | `{"classification": "0"/"1"}` | 0=empty talk, 1=promise |
| `p2_promise` | Promise detection in public goods | `{"p1":"0"/"1", "p2":"0"/"1", "p3":"0"/"1"}` | 0=empty talk, 1=promise |
| `l1_level` | Strategic level in voting game | `{"classification": "0"/"1"/"2"/"3"}` | Levels 0-3 |
| `l2_level` | Strategic level in coordination | `{"classification": "0"-"5"}` | Levels 0-5 |
| `l2_belief` | Salience type in coordination | `{"classification": "payoff"/"label"/"both"/"neither"}` | 4 salience types |
| `l3_level` | Strategic level in beauty contest | `{"classification": "0"-"5"/"eq"/"na"}` | Levels 0-5, eq, na |
| `l3_belief` | Level-0 belief in beauty contest | `{"classification": "0"/"16"/"26"/..."76"/"na"}` | Belief bins |

## Key Updates in v2

1. **Classification Coding Format**: Changed from `"x" = description` to `Code X as "Y"` style
2. **l2 Payoff Tables**: Added complete payoff tables for all X-Y and Pie game variations
3. **l2_belief Simplified**: Now single-token output with 4 categories: payoff, label, both, neither
4. **Classification Task Intro**: Added "Classify the participant's level of strategic reasoning using the guidance below:" to level prompts
5. **Minor wording**: "trust game" and "public goods game" in task descriptions

## Payoff Tables (l2)

### X-Y Game
| Game | X (π1, π2) | Y (π1, π2) |
|------|------------|------------|
| SL   | 5, 5       | 5, 5       |
| ASL  | 5, 5.1     | 5.1, 5     |
| AML  | 5, 6       | 6, 5       |
| ALL  | 5, 10      | 10, 5      |

### Pie Game
| Game | $ (π1, π2) | # (π1, π2) | § (π1, π2) |
|------|------------|------------|------------|
| S1   | 5, 5       | 5, 5       | 5, 5       |
| S2   | 6, 6       | 6, 6       | 5, 5       |
| AM2  | 5, 6       | 6, 5       | 6, 5       |
| AM4  | 6, 7       | 7, 6       | 7, 5       |

## Files

```
prompts/
├── p1_promise.md     # Trust game promise (78 lines)
├── p2_promise.md     # Public goods promise (98 lines)
├── l1_level.md       # Voting game level (137 lines)
├── l2_level.md       # Coordination level (178 lines)
├── l2_belief.md      # Coordination belief (155 lines)
├── l3_level.md       # Beauty contest level (140 lines)
└── l3_belief.md      # Beauty contest belief (127 lines)

js/
├── prompts/prompts.js   # All prompts embedded
└── schemas/schemas.js   # JSON schemas for structured output
```

## Instance Counts

| Dataset | Task | Count |
|---------|------|-------|
| p1 | promise | 38 |
| p2 | promise | 719 |
| l1 | level | 493 |
| l2 | level | 851 |
| l2 | belief | 851 |
| l3 | level | 78 |
| l3 | belief | 78 |
| **Total** | | **3,108** |
