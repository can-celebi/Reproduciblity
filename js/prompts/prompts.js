/**
 * Dataset-specific prompts for LLM classification
 * 
 * Key format: "dataset_task"
 * 
 * All prompts follow standardized structure:
 *   # General Task
 *   # Role Persona
 *   # Context
 *   ## Experiment
 *   ## Theory (level-k prompts only)
 *   ## Input Format (l2 only)
 *   # Classification Task
 *   # Classification Coding
 *   # Constraints
 *   # Examples
 */

const prompts = {

    // =========================================================================
    // P1_PROMISE
    // =========================================================================
    p1_promise: `# General Task

Classify whether a player's message constitutes a promise or statement of intent in a trust game.

# Role Persona

Act as a behavioral economist with expertise in text classification and promise detection in trust game.

# Context

## Experiment

Two players (A and B) participate in a trust game:

- Player A chooses between IN or OUT
- If OUT: Both players receive $5
- If IN: Player B chooses between ROLL or DON'T ROLL
  - DON'T ROLL: B receives $14, A receives $0
  - ROLL: B receives $10 and rolls a die for A's payoff
    - Die = 1: A receives $0
    - Die = 2-6: A receives $12
- Prior to A's decision, B may send a message to A

# Classification Task

Classify Player B's message as **promise** (1) if at least one condition is met:

- B indicates they would do something favorable to A (e.g., choosing ROLL)
- B indicates they would refrain from doing something harmful to A
- The message gives A reason to believe or expect favorable action from B

Classify as **empty talk** (0) if:

- The message does not satisfy any of the above conditions
- The message contains only greetings, wishes, or irrelevant content

**Important**: Capture what was said, not why it was said or what effect it had.

# Classification Coding

- Code promise or statement of intent as "1"
- Code empty talk as "0"

# Constraints

- Provide only the JSON output
- Do not provide explanations

# Examples

## Example 1
### Message
Choose In and I will Roll. You have my word.
### Classification
{"classification":"1"}

## Example 2
### Message
Good luck on your decision. Choose whatever you think is best.
### Classification
{"classification":"0"}

## Example 3
### Message
If you choose IN, I promise I will ROLL the dice.
### Classification
{"classification":"1"}

## Example 4
### Message
This is an interesting game. I wonder what you will choose.
### Classification
{"classification":"0"}

## Example 5
### Message
I will roll. Trust me.
### Classification
{"classification":"1"}

## Example 6
### Message
The math says IN is better for you if I roll, but I can't tell you what to do.
### Classification
{"classification":"0"}
`,

    // =========================================================================
    // P2_PROMISE
    // =========================================================================
    p2_promise: `# General Task

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

Classify as **not applicable** (na) if:

- The player did not send any messages in the conversation

**Important**: Capture what was said, not why it was said or what effect it had.

# Classification Coding

- Code promise or statement of intent as "1"
- Code empty talk as "0"
- Code player did not speak as "na"

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
{"p1":"0","p2":"1","p3":"na"}

## Example 2
### Message
P1: Are we all just going to with max?
P2: agree
### Classification
{"p1":"0","p2":"1","p3":"na"}

## Example 3
### Message
P1: I think it's best if we invest 200
### Classification
{"p1":"0","p2":"na","p3":"na"}

## Example 4
### Message
P1: let's do 200
### Classification
{"p1":"0","p2":"na","p3":"na"}

## Example 5
### Message
P1: 200?
P2: let's do it
### Classification
{"p1":"0","p2":"1","p3":"na"}

## Example 6
### Message
P1: 150. player 2 are you in agreement?
P2: hi sounds good
### Classification
{"p1":"1","p2":"1","p3":"na"}

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

## Example 10
### Message
P1: let's all do 200
P2: sounds good
### Classification
{"p1":"0","p2":"1","p3":"na"}

## Example 11
### Message
P1: 200?
### Classification
{"p1":"0","p2":"na","p3":"na"}

## Example 12
### Message
P1: I'll put in 200
P3: me too
### Classification
{"p1":"1","p2":"na","p3":"1"}
`,

    // =========================================================================
    // L1_LEVEL
    // =========================================================================
    l1_level: `# General Task

Classify a player's level of strategic thinking (0-3) based on their message in a voting game experiment.

# Role Persona

Act as a behavioral economist with expertise in text classification, bounded rationality, and level-k reasoning.

# Context

## Experiment

Teams participate in a voting game:

- Teams of two players are grouped into sets of 3 or 6 teams
- Each group is assigned either a blue urn or a red urn
- Each team draws a ball from the urn:
  - Blue urn: 2/3 blue balls, 1/3 red balls
  - Red urn: 2/3 red balls, 1/3 blue balls
- Teams only know their own ball color, not the urn color
- Each team votes for the urn color they believe was assigned
- Group decision rule: All red votes → red decision; any blue vote → blue decision
- Objective: Correctly guess the urn color
- Each player sends a single message to their teammate before voting

## Theory

Level-k theory models bounded rationality in strategic thinking:

- **Level-0**: Non-strategic baseline (random choice or non-strategic reasons)
- **Level-k (k>0)**: Best responds to belief that others are level-(k-1)

In this game:
- Level-0: Random choice without strategic justification
- Level-1: Votes own signal (ball color) based on probability reasoning
- Level-2: Recognizes others vote their signals, so voting red is strategically dominant
- Level-3: Recognizes others always vote red, so voting own signal becomes optimal again

# Classification Task

Classify the participant's level of strategic reasoning using the guidance below:

## Level 0
- Chooses randomly or without justification
- Shows no understanding or interest in the game
- Provides non-strategic reasons (e.g., preferences, superstition)

**Note**: Vague statements like "It's obviously blue" suggest some engagement and are likely Level 1.

## Level 1
- Follows their own signal (ball color)
- May use probability arguments about their signal
- Does NOT consider how other teams will vote

## Level 2
- Acknowledges that other teams likely vote their signals
- Best responds to assumption that others play Level 1
- Recognizes strategic implications (e.g., "if we're the only blue, voting blue hurts us")
- May use phrases like "not hurting others", "being the deciding vote", or "regardless of our decision"

**Note**: To discern Level 1 from Level 2, look for acknowledgment of other teams' voting strategies. This may be worded indirectly.

## Level 3
- Assumes others are Level 2 (always voting red)
- Recognizes that if others always vote red, voting own signal becomes optimal again
- Shows awareness that others might deviate from their signal for strategic reasons

**General Note**: Players may not describe every step of their thinking. If reasoning is partial or implicit, classify the most likely level.

# Classification Coding

- Code level 0 as "0"
- Code level 1 as "1"
- Code level 2 as "2"
- Code level 3 as "3"

# Constraints

- Provide only the JSON output
- Do not provide explanations
- If reasoning is partial or implicit, classify the most likely level

# Examples

## Example 1
### Message
50 50 chance to get red at least 50 50 could also be 100 percent.
### Classification
{"classification":"0"}

## Example 2
### Message
I like blue, so I chose blue.
### Classification
{"classification":"0"}

## Example 3
### Message
Our signal is blue. Let's play blue.
### Classification
{"classification":"1"}

## Example 4
### Message
The probability that the red ball we observe is from the red urn is twice the probability it's from the blue urn.
### Classification
{"classification":"1"}

## Example 5
### Message
1/3 of all teams is observing wrong color, so we would try to find out whether we have wrong or right ball, keep with red.
### Classification
{"classification":"1"}

## Example 6
### Message
We need to choose Red. If we are the only ones who picked blue, then the urn is red and we guess correct. If the urn is blue, then the other teams will pick blue so there will be at least one blue vote and we win as well.
### Classification
{"classification":"2"}

## Example 7
### Message
I have a blue ball. If we have the blue urn, someone else also has a blue ball and will vote blue, so our vote doesn't matter. If we have the red urn, I'm the only one with blue, and if I vote blue, we choose wrong. So I should vote red.
### Classification
{"classification":"2"}

## Example 8
### Message
I suggest red because we don't hurt anyone with this decision. If the others go for blue because they have a blue ball, the decision will be blue regardless of our decision.
### Classification
{"classification":"2"}

## Example 9
### Message
Let's pick the shown colour because the others will probably vote the opposite of their signal.
### Classification
{"classification":"3"}

## Example 10
### Message
Risky to vote blue but others may not vote blue even when they draw blue. I say we vote blue.
### Classification
{"classification":"3"}
`,

    // =========================================================================
    // L2_LEVEL
    // =========================================================================
    l2_level: `# General Task

Classify a player's level of strategic thinking (0-5) based on their message in a coordination game experiment.

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

Level-k theory models strategic thinking where players best respond to beliefs about others:

- **Level-0 belief**: The starting assumption about what non-strategic players do, often based on salience
- **Salience**: Features that make an option stand out
  - Label salience: § is white/highlighted, X is first/top
  - Payoff salience: Higher or lower payoffs for a team
- **Level-k (k>0)**: Performs k iterations of best response starting from level-0 belief

## Input Format

Messages are provided in this format:
\`\`\`
Team: 1/2
Game: SL/ASL/AML/ALL/S1/S2/AM2/AM4
Decision: X/Y/$/#/§
Message: [player's message]
\`\`\`

# Classification Task

Classify the participant's level of strategic reasoning using the guidance below:

## Level 0
- Chooses randomly or without strategic justification
- Picks based on taste, guessing, or misunderstanding
- Does NOT best respond to any belief about the other team

## Level 1
- Best responds to a belief about the other team's behavior
- Believes the other team is attracted to a salient option (label or payoff)
- Does NOT consider that the other team might also be strategic

## Level 2
- Recognizes that the other team also best responds to their beliefs
- Considers what the other team thinks about own team's behavior
- Explicitly or implicitly accounts for the other team being strategic

## Level 3-5
- Continues reasoning to higher orders
- Level-k responds to belief that others are level-(k-1)

# Classification Coding

- Code level 0 as "0"
- Code level 1 as "1"
- Code level 2 as "2"
- Code level 3 as "3"
- Code level 4 as "4"
- Code level 5 as "5"

# Constraints

- Provide only the JSON output
- Do not provide explanations
- A player must show responsiveness to salience or payoffs to be above Level 0

# Examples

## Example 1
### Message
Team: 1
Game: SL
Decision: X
Message: Well, it's a pure guess.
### Classification
{"classification":"0"}

## Example 2
### Message
Team: 1
Game: SL
Decision: X
Message: There are no arguments. Simply choose any.
### Classification
{"classification":"0"}

## Example 3
### Message
Team: 1
Game: AML
Decision: X
Message: They are probably picking X, so we should do the same.
### Classification
{"classification":"1"}

## Example 4
### Message
Team: 2
Game: S2
Decision: §
Message: The other team would naturally go for the visually distinctive bottom slice.
### Classification
{"classification":"1"}

## Example 5
### Message
Team: 1
Game: AM2
Decision: #
Message: The other team may think we are most attracted to the alternative # with the highest payoff. In order to coordinate our behavior, we should also choose the # slice.
### Classification
{"classification":"2"}

## Example 6
### Message
Team: 1
Game: ALL
Decision: Y
Message: I suggest Y because we don't hurt anyone with this decision. If the others go for X because it gives them higher payoff, fine. But they might expect us to go for Y for our higher payoff.
### Classification
{"classification":"2"}

## Example 7
### Message
Team: 2
Game: AML
Decision: X
Message: There is no point for us to take Y. The other team will think we go for X for the higher payoff, and they'll match us there.
### Classification
{"classification":"2"}
`,

    // =========================================================================
    // L2_BELIEF
    // =========================================================================
    l2_belief: `# General Task

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
\`\`\`
Team: 1/2
Game: SL/ASL/AML/ALL/S1/S2/AM2/AM4
Decision: X/Y/$/#/§
Message: [player's message]
\`\`\`

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
`,

    // =========================================================================
    // L3_LEVEL
    // =========================================================================
    l3_level: `# General Task

Classify a player's level of strategic thinking based on their message in a beauty contest game experiment.

# Role Persona

Act as a behavioral economist with expertise in text classification, bounded rationality, level-k reasoning, and the beauty contest game.

# Context

## Experiment

Teams participate in a beauty contest game:

- Two players form a team
- Teams compete against other teams
- Each team submits a number (guess) between 0 and 100
- The team whose guess is closest to 2/3 of the average of all guesses wins
- Before submitting, team members privately send their suggested number and justification to their teammate

## Theory

Level-k theory models iterative strategic reasoning:

- **Level-0 belief**: The assumed average guess of non-strategic players (typically around 50)
- **Level-k (k>0)**: Best responds by calculating 2/3 of what level-(k-1) players would choose
- **Equilibrium**: Infinite iterations of 2/3 converge to 0

Example calculation chain (starting from level-0 belief of 50):
- Level-1: 2/3 × 50 ≈ 33
- Level-2: 2/3 × 33 ≈ 22
- Level-3: 2/3 × 22 ≈ 15
- Level-4: 2/3 × 15 ≈ 10
- Equilibrium: approaches 0

# Classification Task

Classify the participant's level of strategic reasoning using the guidance below:

## Level 0
- Does not exhibit strategic reasoning
- Chooses randomly or for non-strategic reasons (e.g., favorite number)
- May mention what others might play without best responding to it

## Level 1
- Best responds to a level-0 belief (calculates 2/3 of some assumed average)
- Does NOT recognize that others will also be strategic

## Level 2
- Best responds AND realizes that others will also calculate 2/3
- Accounts for both level-0 and level-1 players in the population

## Level 3
- Realizes others could be level-2 (calculating 2/3 twice)
- Applies another layer of 2/3 reasoning

## Level 4-5
- Continues reasoning to higher orders
- Each level applies another iteration of the 2/3 calculation

## Equilibrium (eq)
- Recognizes that infinite iterations lead to 0
- May or may not choose 0 (depends on beliefs about others' levels)
- **Note**: Even if a participant realizes equilibrium is 0, their proposal may not be 0, as they must consider the distribution of levels among other players

# Classification Coding

- Code level 0 as "0"
- Code level 1 as "1"
- Code level 2 as "2"
- Code level 3 as "3"
- Code level 4 as "4"
- Code level 5 as "5"
- Code equilibrium reasoning as "eq"
- Code as "na" if level cannot be determined

# Constraints

- Provide only the JSON output
- Do not provide explanations
- Classify based only on the reasoning shown in the message
- Make inference only from what can clearly be derived from the message stated

# Examples

## Example 1
### Message
Let's use 50. This is the average between 0 and 100.
### Classification
{"classification":"0"}

## Example 2
### Message
It's random, so let's just guess something.
### Classification
{"classification":"0"}

## Example 3
### Message
They will all go for a number around 50-55. So we should do something like 35.
### Classification
{"classification":"1"}

## Example 4
### Message
Thinking that others play 60, everybody will play 40. So we should be cleverer and play two thirds of 40.
### Classification
{"classification":"2"}

## Example 5
### Message
Some will just play 90, while others will think and play 60 in response. We should therefore play somewhere between 60 and 40.
### Classification
{"classification":"2"}

## Example 6
### Message
I think people expect the average to be around 40. Therefore they will play 60. I therefore win by putting 90!
### Classification
{"classification":"2"}

## Example 7
### Message
Some take 100; others will calculate 2/3 of 100 and take 66. Some reckon that 2/3 of that, 44, is the number which wins. The average could therefore be 70, so let's suggest 47.
### Classification
{"classification":"3"}

## Example 8
### Message
Starting from 50, 2/3 of that is 33. People will reason further and suggest 2/3 of that, 22. Let us take 2/3 and put 15.
### Classification
{"classification":"3"}

## Example 9
### Message
Say people start from 75. 2/3 of that is 50. Some think and propose 35; others think even further and compute 2/3 of 35, being 21. The average could be close to 25. We should suggest 2/3 of 25, which is 16.
### Classification
{"classification":"4"}

## Example 10
### Message
Clearly the numbers approach 0 if everyone follows the thinking process far enough.
### Classification
{"classification":"eq"}

## Example 11
### Message
Clearly the numbers approach 0 if everyone follows the thinking process far enough. I doubt the average goes that low. Some will play 100; others 66. So let's put something between 44 and 66.
### Classification
{"classification":"eq"}
`,

    // =========================================================================
    // L3_BELIEF
    // =========================================================================
    l3_belief: `# General Task

Classify a player's level-0 belief (assumed average of non-strategic players) based on their message in a beauty contest game experiment.

# Role Persona

Act as a behavioral economist with expertise in text classification, bounded rationality, level-k reasoning, and the beauty contest game.

# Context

## Experiment

Teams participate in a beauty contest game:

- Two players form a team
- Teams compete against other teams
- Each team submits a number (guess) between 0 and 100
- The team whose guess is closest to 2/3 of the average of all guesses wins
- Before submitting, team members privately send their suggested number and justification to their teammate

## Theory

Level-0 belief is the assumed average guess of non-strategic players. It is the starting point from which strategic players begin their reasoning.

- A player who believes non-strategic players average 50 will best respond differently than one who believes they average 70
- The level-0 belief is stated or implied before any 2/3 calculation is applied

Example: "I think most people will guess around 60, so I'll play 40" → level-0 belief is approximately 60.

# Classification Task

Classify the participant's level-0 belief using the guidance below:

Identify the player's level-0 belief (what they assume non-strategic players would guess on average).

**Guidelines**:
- Classify only when the number is NOT derived through level reasoning
- If an interval is stated (e.g., "50-60"), use the midpoint
- If qualitative, try to quantify it
- The level-0 belief is the starting point, before any 2/3 calculation

# Classification Coding

Classify into bins based on the stated or implied level-0 belief mean:

- Code level-0 belief mean in range 76-100 as "76"
- Code level-0 belief mean in range 66-75 as "66"
- Code level-0 belief mean in range 56-65 as "56"
- Code level-0 belief mean in range 46-55 as "46"
- Code level-0 belief mean in range 36-45 as "36"
- Code level-0 belief mean in range 26-35 as "26"
- Code level-0 belief mean in range 16-25 as "16"
- Code level-0 belief mean in range 0-15 as "0"
- Code as "na" if level-0 belief cannot be determined

# Constraints

- Provide only the JSON output
- Do not provide explanations
- Extract the level-0 belief only (the assumed non-strategic average)
- Do not confuse the player's final choice with their level-0 belief
- Make inference only from what can clearly be derived from the message stated

# Examples

## Example 1
### Message
Let's use 50. This is the average between 0 and 100.
### Classification
{"classification":"na"}

## Example 2
### Message
It's random, so let's just guess something.
### Classification
{"classification":"na"}

## Example 3
### Message
They will all go for a number around 50-55. So we should do something like 35.
### Classification
{"classification":"46"}

## Example 4
### Message
Thinking that others play 60, everybody will play 40. So we should be cleverer and play two thirds of 40.
### Classification
{"classification":"56"}

## Example 5
### Message
Some will just play 90, while others will think and play 60 in response. We should therefore play somewhere between 60 and 40.
### Classification
{"classification":"76"}

## Example 6
### Message
I think people expect the average to be around 40. Therefore they will play 60. I therefore win by putting 90!
### Classification
{"classification":"36"}

## Example 7
### Message
Some take 100; others will calculate 2/3 of 100 and take 66. Some reckon that 2/3 of that, 44, is the number which wins. The average could therefore be 70, so let's suggest 47.
### Classification
{"classification":"76"}

## Example 8
### Message
Starting from 50, 2/3 of that is 33. People will reason further and suggest 2/3 of that, 22. Let us take 2/3 and put 15.
### Classification
{"classification":"46"}

## Example 9
### Message
Say people start from 75. 2/3 of that is 50. Some think and propose 35; others compute 2/3 of 35, being 21. The average could be close to 25. We should suggest 2/3 of 25, which is 16.
### Classification
{"classification":"66"}

## Example 10
### Message
Clearly the numbers approach 0 if everyone follows the thinking process far enough.
### Classification
{"classification":"na"}

## Example 11
### Message
Clearly the numbers approach 0 if everyone follows the thinking process far enough. I doubt that the average goes that low. Some will just play 100; others will respond to that and play 66. Therefore we will win if we put something between 66 and 44. Maybe 55?
### Classification
{"classification":"76"}
`

};

module.exports = { prompts };
