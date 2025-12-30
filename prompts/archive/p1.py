prompt_neutral = """# General Task: Classify a player's message in an economic experiment game to determine if it constitutes a promise.

# Context
- Players: Two players, A and B.
- Game Mechanics: 
  + First, player A chooses between the two decisions 'IN' and 'OUT'.
  + If player A chooses 'OUT', each player receives $5.
  + If player A chooses 'IN', player B chooses between the two decisions 'ROLL' or 'DON'T ROLL' (a die).
  + If player A chooses 'IN' and player B chooses 'DON'T ROLL', then player B receives $14 and A receives $0.
  + If player A chooses 'IN', and player B chooses 'ROLL', player B receives $10 and rolls a six-sided die to determine player A's payoff. If the die comes up 1 (1/6 likelihood), player A receives $0; if the die comes up 2-6 (5/6 likelihood), player A receives $12.
  + In addition, each player receives $5 show-up fee for participating in the experiment.
- Communication: Prior to Player A's decision to 'IN' or 'OUT', player B has the option to send a message to player A.
- Task: Classify whether the message sent by player B constitutes a promise or not.

# Classification Guidelines
- Classify a message as a "promise or a statement of intent" if at least one of the following conditions is satisfied:
  + Player B indicates in the message he would do something favorable to player A or refrain from doing something that hurts player A
  + The message gives player A reasons to believe or expect that player B would do something favorable to player A or refrain from doing something that hurts player A.
- If the message does not satisfy any of the above conditions, classify player B's message as "Empty Talk".

# Classification Tips
- Capture what had been said rather than why it was said or what effect it had.
- Operate as a "coding machine".

# Classification Coding
Code your classification of the text as `1` or `0`:
 - Code a "promise or a statement of intent" as `1`.
 - Code an "empty talk" as `0`.

# Output Format:
0/1"""

prompt_weak = """# General Task: Classify a player's message in an economic experiment game to determine if it constitutes a promise.

# Context
- Players: Two players, A and B.
- Game Mechanics: 
  + First, player A chooses between the two decisions 'IN' and 'OUT'.
  + If player A chooses 'OUT', each player receives $5.
  + If player A chooses 'IN', player B chooses between the two decisions 'ROLL' or 'DON'T ROLL' (a die).
  + If player A chooses 'IN' and player B chooses 'DON'T ROLL', then player B receives $14 and A receives $0.
  + If player A chooses 'IN', and player B chooses 'ROLL', player B receives $10 and rolls a six-sided die to determine player A's payoff. If the die comes up 1 (1/6 likelihood), player A receives $0; if the die comes up 2-6 (5/6 likelihood), player A receives $12.
  + In addition, each player receives $5 show-up fee for participating in the experiment.
- Communication: Prior to Player A's decision to 'IN' or 'OUT', player B has the option to send a message to player A.
- Task: Classify whether the message sent by player B constitutes a promise or not.

# Classification Guidelines
- Classify a message as a "promise or a statement of intent" if at least one of the following conditions is **probably** satisfied:
  + Player B indicates in the message he would do something favorable to player A or refrain from doing something that hurts player A
  + The message gives player A reasons to believe or expect that player B would do something favorable to player A or refrain from doing something that hurts player A.
- If the message does not **probably** satisfy any of the above conditions, classify player B's message as "Empty Talk".

# Classification Tips
- Capture what had been said rather than why it was said or what effect it had.
- Operate as a "coding machine".

# Classification Coding
Code your classification of the text as `1` or `0`:
 - Code a "promise or a statement of intent" as `1`.
 - Code an "empty talk" as `0`.

# Output
0/1"""


prompt_strong = """# General Task: Classify a player's message in an economic experiment game to determine if it constitutes a promise.

# Context
- Players: Two players, A and B.
- Game Mechanics: 
  + First, player A chooses between the two decisions 'IN' and 'OUT'.
  + If player A chooses 'OUT', each player receives $5.
  + If player A chooses 'IN', player B chooses between the two decisions 'ROLL' or 'DON'T ROLL' (a die).
  + If player A chooses 'IN' and player B chooses 'DON'T ROLL', then player B receives $14 and A receives $0.
  + If player A chooses 'IN', and player B chooses 'ROLL', player B receives $10 and rolls a six-sided die to determine player A's payoff. If the die comes up 1 (1/6 likelihood), player A receives $0; if the die comes up 2-6 (5/6 likelihood), player A receives $12.
  + In addition, each player receives $5 show-up fee for participating in the experiment.
- Communication: Prior to Player A's decision to 'IN' or 'OUT', player B has the option to send a message to player A.
- Task: Classify whether the message sent by player B constitutes a promise or not.

# Classification Guidelines
- Classify a message as a "promise or a statement of intent" if at least one of the following conditions is **certainly** satisfied:
  + Player B indicates in the message he would do something favorable to player A or refrain from doing something that hurts player A
  + The message gives player A reasons to believe or expect that player B would do something favorable to player A or refrain from doing something that hurts player A.
- If the message does not **certainly** satisfy any of the above conditions, classify player B's message as "Empty Talk".

# Classification Tips
- Capture what had been said rather than why it was said or what effect it had.
- Operate as a "coding machine".

# Classification Coding
Code your classification of the text as `1` or `0`:
 - Code a "promise or a statement of intent" as `1`.
 - Code an "empty talk" as `0`.

# Output
0/1"""


prompt_og = """# General Task: Classify a player's message in an economic experiment game to determine if it constitutes a promise.

# Context
- Players: Two players, A and B.
- Game Mechanics: 
  + First, player A chooses between the two decisions 'IN' and 'OUT'.
  + If player A chooses 'OUT', each player receives $5.
  + If player A chooses 'IN', player B chooses between the two decisions 'ROLL' or 'DON'T ROLL' (a die).
  + If player A chooses 'IN' and player B chooses 'DON'T ROLL', then player B receives $14 and A receives $0.
  + If player A chooses 'IN', and player B chooses 'ROLL', player B receives $10 and rolls a six-sided die to determine player A's payoff. If the die comes up 1 (1/6 likelihood), player A receives $0; if the die comes up 2-6 (5/6 likelihood), player A receives $12.
  + In addition, each player receives $5 show-up fee for participating in the experiment.
- Communication: Prior to Player A's decision to 'IN' or 'OUT', player B has the option to send a message to player A.
- Task: Classify whether the message sent by player B constitutes a promise or not.

# Classification Guidelines:  
## Definition of a Promise:
- A player's message is classified as a 'promise' (`1`) if it:
  - Indicates a specific action by the player.
  - Gives others reason to believe or expect that the player will take a certain course of action.
## Non-Promise Categorization:
- Fails to meet the criteria of a promise.
### Coding Perspective:
- Capture what had been said rather than why it was said or what effect it had.
- Operate as a 'coding machine.'

# Message Coding Process:
## Promise Identification:
- Code as `1` for messages that constitute a promise.
## Other Messages:
- Code as `0` for messages that do not represent a promise. 

# Output
0/1"""


prompt_2_og = """# General Task: Classify a player's message in an economic experiment game to determine if it constitutes a promise.

# Context
- Players: Two players, A and B.
- Game Mechanics: 
  + First, player A chooses between the two decisions 'IN' and 'OUT'.
  + If player A chooses 'OUT', each player receives $5.
  + If player A chooses 'IN', player B chooses between the two decisions 'ROLL' or 'DON'T ROLL' (a die).
  + If player A chooses 'IN' and player B chooses 'DON'T ROLL', then player B receives $14 and A receives $0.
  + If player A chooses 'IN', and player B chooses 'ROLL', player B receives $10 and rolls a six-sided die to determine player A's payoff. If the die comes up 1 (1/6 likelihood), player A receives $0; if the die comes up 2-6 (5/6 likelihood), player A receives $12.
  + In addition, each player receives $5 show-up fee for participating in the experiment.
- Communication: Prior to Player A's decision to 'IN' or 'OUT', player B has the option to send a message to player A.
- Task: Classify whether the message sent by player B constitutes a promise or not.

# Role Persona: 
You are an 'Investment Game Analyst' specializing in player interactions. Your task is to scrutinize players' chat messages and classify them based on their commitment level.

# Classification Criteria:
- Code a player's message as '1' if it's a promise.
- Code as '0' if it's not a promise.

# Identifying a Promise:
- Player explicitly agrees to take an action suggested by another.
- Player explicitly states their intention to take a specific action.
- Player commits to an action, conditional on a specific event occurring.

# Identifying Non-Promise:
- Suggesting actions without commitment.
- Asking questions or discussing preferences without committing.
- Talking about hypothetical, ideal, or rational actions without explicit commitment.

# Output
0/1"""


prompt_superweak = """# General Task: Classify a player's message in an economic experiment game to determine if it constitutes a promise.

# Context
- Players: Two players, A and B.
- Game Mechanics: 
  + First, player A chooses between the two decisions 'IN' and 'OUT'.
  + If player A chooses 'OUT', each player receives $5.
  + If player A chooses 'IN', player B chooses between the two decisions 'ROLL' or 'DON'T ROLL' (a die).
  + If player A chooses 'IN' and player B chooses 'DON'T ROLL', then player B receives $14 and A receives $0.
  + If player A chooses 'IN', and player B chooses 'ROLL', player B receives $10 and rolls a six-sided die to determine player A's payoff. If the die comes up 1 (1/6 likelihood), player A receives $0; if the die comes up 2-6 (5/6 likelihood), player A receives $12.
  + In addition, each player receives $5 show-up fee for participating in the experiment.
- Communication: Prior to Player A's decision to 'IN' or 'OUT', player B has the option to send a message to player A.
- Task: Classify whether the message sent by player B constitutes a promise or not.

# Classification Guidelines
- Classify a message as a "promise or a statement of intent" if at least one of the following conditions is satisfied even if there is a slight possibility:
  + Player B indicates in the message he would do something favorable to player A or refrain from doing something that hurts player A
  + The message gives player A reasons to believe or expect that player B would do something favorable to player A or refrain from doing something that hurts player A.
- If the message does not satisfy even with a slight possibility any of the above conditions, classify player B's message as "Empty Talk".

# Classification Tips
- Capture what had been said rather than why it was said or what effect it had.
- Operate as a "coding machine".

# Classification Coding
Code your classification of the text as `1` or `0`:
 - Code a "promise or a statement of intent" as `1`.
 - Code an "empty talk" as `0`.

# Output
0/1"""

prompt_implicit = """# General Task: Classify a player's message in an economic experiment game to determine if it constitutes a promise.

# Context
- Players: Two players, A and B.
- Game Mechanics: 
  + First, player A chooses between the two decisions 'IN' and 'OUT'.
  + If player A chooses 'OUT', each player receives $5.
  + If player A chooses 'IN', player B chooses between the two decisions 'ROLL' or 'DON'T ROLL' (a die).
  + If player A chooses 'IN' and player B chooses 'DON'T ROLL', then player B receives $14 and A receives $0.
  + If player A chooses 'IN', and player B chooses 'ROLL', player B receives $10 and rolls a six-sided die to determine player A's payoff. If the die comes up 1 (1/6 likelihood), player A receives $0; if the die comes up 2-6 (5/6 likelihood), player A receives $12.
  + In addition, each player receives $5 show-up fee for participating in the experiment.
- Communication: Prior to Player A's decision to 'IN' or 'OUT', player B has the option to send a message to player A.
- Task: Classify whether the message sent by player B constitutes a promise or not.

# Classification Guidelines
- Classify a message as a "promise or a statement of intent" if at least one of the following conditions is satisfied:
  + Player B's message hints, at the very least, in an extremely vague or implicit manner indicates that he would do something favorable to player A or refrain from doing something that hurts player A
  + The message gives player A, at the very least, an extremely vague or implicit reason to believe or expect that player B would do something favorable to player A or refrain from doing something that hurts player A.
- If the message does not satisfy any of the above conditions, classify player B's message as "Empty Talk".

# Classification Tips
- Capture what had been said rather than why it was said or what effect it had.
- Operate as a "coding machine".

# Classification Coding
Code your classification of the text as `1` or `0`:
 - Code a "promise or a statement of intent" as `1`.
 - Code an "empty talk" as `0`.

# Output Format:
0/1"""

prompt_null = """# General Task: Classify a player's message in an economic experiment game to determine if it constitutes a promise.

# Context
- Players: Two players, A and B.
- Game Mechanics: 
  + First, player A chooses between the two decisions 'IN' and 'OUT'.
  + If player A chooses 'OUT', each player receives $5.
  + If player A chooses 'IN', player B chooses between the two decisions 'ROLL' or 'DON'T ROLL' (a die).
  + If player A chooses 'IN' and player B chooses 'DON'T ROLL', then player B receives $14 and A receives $0.
  + If player A chooses 'IN', and player B chooses 'ROLL', player B receives $10 and rolls a six-sided die to determine player A's payoff. If the die comes up 1 (1/6 likelihood), player A receives $0; if the die comes up 2-6 (5/6 likelihood), player A receives $12.
  + In addition, each player receives $5 show-up fee for participating in the experiment.
- Communication: Prior to Player A's decision to 'IN' or 'OUT', player B has the option to send a message to player A.
- Task: Classify whether the message sent by player B constitutes a promise or not.

# Classification Task:
- Code a player's message as `1` if it's a promise.
- Code a player's message as `0` if it's not a promise.

# Output Format:
 0/1"""

prompt_null_nocontext = """# Classification Task:
- Code a player's message as `1` if it's a promise.
- Code a player's message as `0` if it's not a promise.

# Output Format:
 0/1"""