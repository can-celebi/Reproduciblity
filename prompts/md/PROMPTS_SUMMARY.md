# Standardized Prompts Summary

## Unified Structure

All 7 prompts follow this consistent structure:

```
# General Task
# Role Persona
# Context
## Experiment
## Theory (l1-l3 only)
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
| `l2_belief` | Salience belief in coordination | `{"payoff":"H"/"L"/"no", "label":"X"/"Y"/"$"/"#"/"§"/"no"}` | Payoff + label |
| `l3_level` | Strategic level in beauty contest | `{"classification": "0"-"5"/"eq"/"na"}` | Levels 0-5, eq, na |
| `l3_belief` | Level-0 belief in beauty contest | `{"classification": "0"/"16"/"26"/..."76"/"na"}` | Belief bins |

## Key Design Decisions

### Based on OpenAI GPT-4.1 Prompting Guide:
1. **Clear headers**: Markdown H1/H2 hierarchy for sections
2. **Explicit constraints**: Dedicated section for output requirements  
3. **Structured examples**: Consistent `### Message` → `### Classification` format
4. **Role specification**: Brief "Role Persona" section

### Based on Few-Shot Prompting Best Practices:
1. **Diverse examples**: Cover edge cases and typical cases
2. **Consistent format**: All examples follow same structure
3. **Output demonstration**: Show exact JSON format expected
4. **Minimal explanation**: Examples show, don't tell

### Alignment Across Prompts:
1. **Same section order** in all prompts
2. **Same terminology**: "General Task", "Role Persona", "Classification Task", etc.
3. **Same example format**: `### Message` followed by `### Classification`
4. **Consistent JSON key**: `"classification"` for single-output (except p2, l2_belief)

## Theory Sections

All level-k prompts (l1, l2, l3) include a brief Theory section covering:
- Level-0 belief definition
- Level-k best response concept  
- Game-specific level interpretations

## Files

```
prompts/
├── p1_promise.md     # Trust game promise
├── p2_promise.md     # Public goods promise  
├── l1_level.md       # Voting game level
├── l2_level.md       # Coordination level
├── l2_belief.md      # Coordination belief
├── l3_level.md       # Beauty contest level
└── l3_belief.md      # Beauty contest belief

js/
├── prompts/prompts.js   # All prompts embedded
└── schemas/schemas.js   # JSON schemas for structured output
```

## Instance Counts (from instances.csv)

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

## Next Steps

1. Copy prompts to your project's `prompts/` directory
2. Update `prompts.js` path if needed
3. Verify schemas match your expected output format
4. Run test with `instances_test.csv` (70 instances)
5. Review outputs before full 50-run production
