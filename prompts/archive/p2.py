p1_sr = """
## New Instructions
1. Step-by-step reevaluate the reasoning of your classification.
2. Provide a final classification based on your reevaluation.

## Output Format:
 ### Reevaluate:
  ...
 ### Final Classification:
  P# : 0/1"""

# improved the markup structure and fixed some typos
p1_ex11 = """# General Task: Evaluate player messages in a group investment game to determine if they constitute a promise.

# Context:
- Players: Group of three.
- Investment: Maximum 200 pence each.
- Mechanics: Invested amount is doubled and split equally.
- Communication: Players can chat before investing.
- Duration: Multiple rounds.

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

# Examples:

## Example 1:
### Chat:
P1: all 200 then?  
P2: yes    
### Classification: 
P1: 0  
P2: 1 

## Example 2:
### Chat:
P1: Are we all just going to with max?
P2: agree    
### Classification: 
P1: 0 
P2: 1 

## Example 3:
### Chat:
P1: I think it's best if we invest 200  
### Classification: 
P1: 0 

## Example 4:
### Chat:
P1: let's do 200 
### Classification: 
P1: 0 

## Remark - Rather than spelling out numbers such as 200, people might refer to "max" or "all in".

## Example 5:
### Chat:
P1: I have been doing 200 in the last round 
### Classification: 
P1: 0 

## Example 6:
### Chat:
P1: 100 sounds good
### Classification: 
P1: 0 

## Remark - In a particular contexts, "let's do it" or "sounds good" may be a promise:

## Example 7:
### Chat:
P1: 200?
P2: let's do it    
### Classification: 
P1: 0 
P2: 1

## Example 8:
### Chat:
P1: 150. player 2 are you in agreement?
P2: hi sounds good    
### Classification: 
P1: 1 
P2: 1

## Remark - A message may include conditional promises (do something if someone else agrees): 

## Example 9:
### Chat:
P1: happy with 200 if we all agree
P2: cool let's do it   
P3: Yep. 
### Classification: 
P1: 1 (conditional promise)
P2: 1
P3: 1

## Remark - In **Example 10**, only players 2 and 3 make a promise, because "200 each?" does not make it clear that the first participant *will* do 200 if the others do that as well:

## Example 10:
### Chat:
P1: 200 each?
P2: agree  
P3: agree 
### Classification: 
P1: 0
P2: 1
P3: 1

## Remark: Initial promises about which player later change their mind do not count: 

## Example 11:
### Chat:
P1: 200
P2: agreed  (initial promise)
P3: I suggest 100
P2: I'm happy with either (P2 changes her mind)
### Classification: 
P1: 1 
P2: 0 (due to change of mind after an initial promise)
P3: 0


# Output Format:
 P# : 0/1

# Constraints:
- Please do not provide an explanation for your classification
- Please provide a final **single** classification for each player"""


# improved the markup structure and fixed some typos
p1_ex11_CoT = """# General Task: 
Evaluate player messages in a group investment game to determine if they constitute a promise.

# Context:
- Players: Group of three.
- Investment: Maximum 200 pence each.
- Mechanics: Invested amount is doubled and split equally.
- Communication: Players can chat before investing.
- Duration: Multiple rounds.

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

# Examples:
## Example 1:
### Chat:
P1: all 200 then? P2: yes    
### Classification: 
P1: 0 P2: 1 

## Example 2:
### Chat:
P1: Are we all just going to with max?
P2: agree    
### Classification: 
P1: 0 
P2: 1 

## Example 3:
### Chat:
P1: I think it's best if we invest 200  
### Classification: 
P1: 0 

## Example 4:
### Chat:
P1: let's do 200 
### Classification: 
P1: 0 

## Remark - Rather than spelling out numbers such as 200, people might refer to "max" or "all in".

## Example 5:
### Chat:
P1: I have been doing 200 in the last round 
### Classification: 
P1: 0 

## Example 6:
### Chat:
P1: 100 sounds good
### Classification: 
P1: 0 

## Remark - In a particular contexts, "let's do it" or "sounds good" may be a promise:

## Example 7:
### Chat:
P1: 200?
P2: let's do it    
### Classification: 
P1: 0 
P2: 1

## Example 8:
### Chat:
P1: 150. player 2 are you in agreement?
P2: hi sounds good    
### Classification: 
P1: 1 
P2: 1

## Remark - A message may include conditional promises (do something if someone else agrees): 

## Example 9:
### Chat:
P1: happy with 200 if we all agree
P2: cool let's do it   
P3: Yep. 
### Classification: 
P1: 1 (conditional promise)
P2: 1
P3: 1

## Remark - In **Example 10**, only players 2 and 3 make a promise, because "200 each?" does not make it clear that the first participant *will* do 200 if the others do that as well:

## Example 10:
### Chat:
P1: 200 each?
P2: agree  
P3: agree 
### Classification: 
P1: 0
P2: 1
P3: 1

## Remark: Initial promises about which player later change their mind do not count: 

## Example 11:
### Chat:
P1: 200
P2: agreed  (initial promise)
P3: I suggest 100
P2: I'm happy with either (P2 changes her mind)
### Classification: 
P1: 1 
P2: 0 (due to change of mind after an initial promise)
P3: 0

# Classification Process:
- Step 1: Provide a step-by-step reasoning for your classification
- Step 2: Provide your classification for each player's sum of messages

# Output Format:
 ### Reasoning:
  ...
 ### Classification:
  P# : 0/1

# Constraint:
- Please provide a final **single** classification for each player"""

