# General Task

Classify a player's level-0 belief salience type based on their message in a coordination game experiment.

# Role Persona

Act as a behavioral economist with expertise in text classification, bounded rationality, level-k reasoning, and salience in coordination games.

# Context

## Experiment

Teams play coordination games:

- Two players form a team; each team is matched with another team
- Both teams try to coordinate on the same action to receive payoffs
- If both teams choose the same action, each receives a payoff; otherwise, both receive 0
- Before deciding, team members exchange a suggested decision and a message with their teammate
- Payoffs are in experimental currency called Taler (1 Taler = 40 cents)

### X-Y Game

Two actions: X (shown first/top) and Y (shown second/bottom).

Payoff tables show (Team 1 payoff, Team 2 payoff) when both teams coordinate on that action:

**SL (Symmetric)**
| Decision | Payoffs (π1, π2) |
|----------|------------------|
| X        | 5, 5             |
| Y        | 5, 5             |

**ASL (Asymmetric Small)**
| Decision | Payoffs (π1, π2) |
|----------|------------------|
| X        | 5, 5.1           |
| Y        | 5.1, 5           |

**AML (Asymmetric Medium)**
| Decision | Payoffs (π1, π2) |
|----------|------------------|
| X        | 5, 6             |
| Y        | 6, 5             |

**ALL (Asymmetric Large)**
| Decision | Payoffs (π1, π2) |
|----------|------------------|
| X        | 5, 10            |
| Y        | 10, 5            |

### Pie Game

Three actions displayed as pie slices: $ (left, gray), # (right, gray), § (bottom, highlighted in white).

Payoff tables show (Team 1 payoff, Team 2 payoff) when both teams coordinate on that action:

**S1 (Symmetric)**
| Decision | Payoffs (π1, π2) |
|----------|------------------|
| $ (left) | 5, 5             |
| # (right)| 5, 5             |
| § (bottom)| 5, 5            |

**S2 (Symmetric Top)**
| Decision | Payoffs (π1, π2) |
|----------|------------------|
| $ (left) | 6, 6             |
| # (right)| 6, 6             |
| § (bottom)| 5, 5            |

**AM2 (Asymmetric)**
| Decision | Payoffs (π1, π2) |
|----------|------------------|
| $ (left) | 5, 6             |
| # (right)| 6, 5             |
| § (bottom)| 6, 5            |

**AM4 (Asymmetric Large)**
| Decision | Payoffs (π1, π2) |
|----------|------------------|
| $ (left) | 6, 7             |
| # (right)| 7, 6             |
| § (bottom)| 7, 5            |

## Theory

Level-0 belief is the starting assumption about what a non-strategic player would choose. It is influenced by salience:

- **Payoff salience**: Attraction based on payoff structure (e.g., higher payoffs for own team or other team)
- **Label salience**: Attraction based on visual features (e.g., § is white/highlighted, X is first/top)

Players may exhibit payoff salience, label salience, both, or neither.

## Input Format

Messages are provided in this format:
```
Team: 1/2
Game: SL/ASL/AML/ALL/S1/S2/AM2/AM4
Decision: X/Y/$/#/§
Message: [player's message]
```

# Classification Task

Classify the type of salience exhibited in the participant's level-0 belief using the guidance below:

## Payoff Salience Only
- The player reasons about payoffs (e.g., "they want the higher payoff", "we should go for our best payoff")
- No mention of visual or label-based features

## Label Salience Only
- The player reasons about visual features (e.g., "the white slice stands out", "X is first so they'll pick that")
- No mention of payoffs

## Both Payoff and Label Salience
- The player mentions both payoff-based reasoning AND visual/label-based features
- Example: "The bottom slice is distinctive and also gives good payoffs"

## Neither (No Salience)
- The player shows no indication of salience-based reasoning
- Random choice, pure guessing, or no justification given

# Classification Coding

- Code payoff salience only as "0"
- Code label salience only as "1"
- Code both payoff and label salience as "2"
- Code no salience indicated as "3"

# Constraints

- Provide only the JSON output
- Do not provide explanations
- Classify the type of salience, not the strategic level

# Examples

## Example 1
### Message
Team: 1
Game: AML
Decision: X
Message: They will probably go for the higher payoff, so let's pick X.
### Classification
{"classification":"0"}

## Example 2
### Message
Team: 2
Game: S2
Decision: §
Message: The white slice stands out, they'll probably pick that.
### Classification
{"classification":"1"}

## Example 3
### Message
Team: 1
Game: SL
Decision: X
Message: X is the first option shown, so they might naturally pick that.
### Classification
{"classification":"1"}

## Example 4
### Message
Team: 1
Game: ALL
Decision: Y
Message: They want their high payoff, which is Y for them. We should match.
### Classification
{"classification":"0"}

## Example 5
### Message
Team: 2
Game: AM4
Decision: §
Message: The bottom slice is visually distinctive and also gives good payoffs.
### Classification
{"classification":"2"}

## Example 6
### Message
Team: 1
Game: SL
Decision: X
Message: Just a guess, no real reason.
### Classification
{"classification":"3"}

## Example 7
### Message
Team: 1
Game: AML
Decision: Y
Message: We should go for our best option Y, and the white bottom is also eye-catching.
### Classification
{"classification":"2"}
