/**
 * Dataset-specific prompts
 * 
 * Key format: "dataset_task"
 * 
 * You need to fill in the actual prompt content for each.
 * The prompts should explain the classification task and the meaning of each category.
 */

const prompts = {

    // =========================================================================
    // P1: Promise Classification (single text)
    // Output: classification = "0" (not promise), "1" (promise), "NA"
    // =========================================================================
    p1_promise: `You are an expert at analyzing experimental economics data.

Your task is to classify whether the following message contains a promise.

Classification categories:
- "1" = The message contains a promise (explicit commitment to future action)
- "0" = The message does NOT contain a promise
- "NA" = Cannot determine / unclear

Analyze the message and provide your classification.

[ADD MORE DETAILED INSTRUCTIONS HERE]`,

    // =========================================================================
    // P2: Promise Classification (chat with up to 3 players)
    // Output: p1, p2, p3 each = "0", "1", or "NA"
    // =========================================================================
    p2_promise: `You are an expert at analyzing experimental economics data.

Your task is to classify whether each player in the following chat made a promise.

The chat involves up to 3 players labeled [Player 1], [Player 2], [Player 3].
For each player who speaks, determine if they made a promise.

Classification for each player:
- "1" = Player made a promise
- "0" = Player did NOT make a promise  
- "NA" = Player did not speak OR cannot determine

If a player does not appear in the chat, classify them as "NA".

[ADD MORE DETAILED INSTRUCTIONS HERE]`,

    // =========================================================================
    // L1: Strategic Voting Level (0-3)
    // Output: classification = "0", "1", "2", or "3"
    // =========================================================================
    l1_level: `You are an expert at analyzing strategic reasoning in voting behavior.

Your task is to classify the level of strategic thinking demonstrated in the following message.

Level definitions:
- "0" = Level 0 - No strategic reasoning, votes based on simple preference
- "1" = Level 1 - Basic strategic reasoning, considers what others might do
- "2" = Level 2 - Second-order reasoning, considers what others think others will do
- "3" = Level 3+ - Higher-order reasoning, multiple levels of strategic thinking

[ADD MORE DETAILED INSTRUCTIONS HERE]`,

    // =========================================================================
    // L2: Asymmetric Payoff Coordination - Level (0-5)
    // Output: classification = "0" through "5"
    // =========================================================================
    l2_level: `You are an expert at analyzing strategic reasoning in coordination games.

Your task is to classify the level of strategic thinking in the following message from an asymmetric payoff coordination game.

Level definitions:
- "0" = Level 0 - Random/no reasoning
- "1" = Level 1 - Basic reasoning about the game structure
- "2" = Level 2 - Considers opponent's perspective
- "3" = Level 3 - Second-order beliefs about opponent
- "4" = Level 4 - Third-order reasoning
- "5" = Level 5+ - Higher-order strategic thinking

[ADD MORE DETAILED INSTRUCTIONS HERE]`,

    // =========================================================================
    // L2: Asymmetric Payoff Coordination - Belief Categories
    // Output: classification = "0" through "8"
    // 
    // Map (adjust as needed):
    //   0 = payoff_high
    //   1 = payoff_low
    //   2 = no_payoff
    //   3 = label_x
    //   4 = label_y
    //   5 = label_# (hash)
    //   6 = label_¶ (paragraph)
    //   7 = label_$ (dollar)
    //   8 = no_label / none
    // =========================================================================
    l2_belief: `You are an expert at analyzing belief formation in coordination games.

Your task is to classify what belief category the player expresses in the following message.

The player may focus on payoffs or labels when forming their belief about what the opponent will choose.

Belief categories (respond with the number):
- "0" = Payoff high - Believes opponent will choose the option with highest payoff
- "1" = Payoff low - Believes opponent will choose the option with lowest payoff
- "2" = No payoff - No payoff-based reasoning
- "3" = Label X - Believes opponent will choose based on label X
- "4" = Label Y - Believes opponent will choose based on label Y
- "5" = Label # - Believes opponent will choose based on label #
- "6" = Label ¶ - Believes opponent will choose based on label ¶
- "7" = Label $ - Believes opponent will choose based on label $
- "8" = No label / None - No label-based reasoning or no clear belief

[ADD MORE DETAILED INSTRUCTIONS HERE]`,

    // =========================================================================
    // L3: Beauty Contest - Level
    // Output: classification = "0", "1", "2", "3", "4", "eq", "na"
    // =========================================================================
    l3_level: `You are an expert at analyzing strategic reasoning in beauty contest (guessing) games.

In this game, players choose a number between 0 and 100. The winner is the player whose number is closest to 2/3 of the average of all chosen numbers.

Your task is to classify the level of strategic thinking in the following message.

Level definitions:
- "0" = Level 0 - Random guess or no strategic reasoning (e.g., picks 50, birthday, lucky number)
- "1" = Level 1 - One step of reasoning (e.g., "others pick 50, so 2/3 of 50 = 33")
- "2" = Level 2 - Two steps (e.g., "others do level-1, so 2/3 of 33 = 22")
- "3" = Level 3 - Three steps of reasoning
- "4" = Level 4+ - Four or more steps
- "eq" = Equilibrium thinker - Recognizes the Nash equilibrium is 0
- "na" = Cannot determine level from the text

[ADD MORE DETAILED INSTRUCTIONS HERE]`,

    // =========================================================================
    // L3: Beauty Contest - Belief (mean of level-0 distribution)
    // Output: classification = "0", "16", "26", "36", "46", "56", "66", "76", "none"
    // Bins: [0,16), [16,26), [26,36), [36,46), [46,56), [56,66), [66,76), [76,100]
    // =========================================================================
    l3_belief: `You are an expert at analyzing belief formation in beauty contest (guessing) games.

Your task is to classify what the player believes the average Level-0 (naive) guess would be.

This represents the player's belief about what random/naive players would choose on average.

Belief bins (respond with the lower bound of the bin):
- "0" = Believes average is in [0, 16)
- "16" = Believes average is in [16, 26)
- "26" = Believes average is in [26, 36)
- "36" = Believes average is in [36, 46)
- "46" = Believes average is in [46, 56)
- "56" = Believes average is in [56, 66)
- "66" = Believes average is in [66, 76)
- "76" = Believes average is in [76, 100]
- "none" = No belief about level-0 average is stated

[ADD MORE DETAILED INSTRUCTIONS HERE]`,

};

module.exports = { prompts };
