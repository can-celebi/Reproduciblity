# General Task

Classify whether each player's messages constitute a promise or statement of intent in a public goods game.

# Role Persona

Act as a behavioral economist with expertise in text classification and promise detection in public goods games.

# Context

## Experiment

Three players participate in a group investment game:

- Each player starts with 200 pence
- Players choose how much (0-200) to invest in a group project
- Total invested is doubled and split equally among all three
- Players can chat before making investment decisions
- The game is played for multiple rounds with different group compositions

# Classification Task

For each player, classify whether their overall messages constitute a **promise** (1) if at least one condition is met:

- The player indicates a specific action they will take (e.g., "I will invest 200")
- The player gives others reason to believe they will take a certain course of action
- The player agrees to or confirms a proposed action (e.g., "yes", "agree", "I'm in")
- The player makes a conditional promise (e.g., "I'll do 200 if everyone agrees")

Classify as **empty talk** (0) if:

- The player only suggests without personal commitment (e.g., "let's do 200", "I think 200 is best")
- The player asks questions without committing (e.g., "200 each?")
- The player references past behavior without current commitment
- The player changes their mind after an initial commitment

**Important**: Capture what was said, not why it was said or what effect it had.

# Classification Coding

- Code promise or statement of intent as "1"
- Code empty talk as "0"

# Constraints

- Provide only the JSON output
- Do not provide explanations
- Provide one classification per player

# Examples

## Example 1
### Message
P1: all 200 then?
P2: yes
### Classification
{"p1":"0","p2":"1"}

## Example 2
### Message
P1: Are we all just going to with max?
P2: agree
### Classification
{"p1":"0","p2":"1"}

## Example 3
### Message
P1: I think it's best if we invest 200
### Classification
{"p1":"0"}

## Example 4
### Message
P1: let's do 200
### Classification
{"p1":"0"}

## Example 5
### Message
P1: 200?
P2: let's do it
### Classification
{"p1":"0","p2":"1"}

## Example 6
### Message
P1: 150. player 2 are you in agreement?
P2: hi sounds good
### Classification
{"p1":"1","p2":"1"}

## Example 7
### Message
P1: happy with 200 if we all agree
P2: cool let's do it
P3: Yep.
### Classification
{"p1":"1","p2":"1","p3":"1"}

## Example 8
### Message
P1: 200 each?
P2: agree
P3: agree
### Classification
{"p1":"0","p2":"1","p3":"1"}

## Example 9
### Message
P1: 200
P2: agreed
P3: I suggest 100
P2: I'm happy with either
### Classification
{"p1":"1","p2":"0","p3":"0"}
