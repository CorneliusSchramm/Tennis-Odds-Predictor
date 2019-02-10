
# SHUFFELING & MERGING MTACHES WITH CORRECT RANKINGS DATA

# Setup -------------------------------------------------------------------

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/data1991_2017.RData")
load("../Roeser, Jonas - 2_Data/matches.RData")
load("../Roeser, Jonas - 2_Data/rankings.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/data1991_2017.RData")
load("../2_Data/matches.RData")
load("../2_Data/rankings.RData")


# Reordering winners & losers to randomised Y -----------------------------

# Set seed to have the same random distribution every time we shuffle and add 
# the column with the randomly chosen zeros and ones to the end of "matches"
set.seed(1)
matches = matches %>%
  mutate(randomisedY = sample(c(0,1), replace=TRUE, size=nrow(matches)))

# We need the original matches dataframe for switching the columns in the for-loop
matches_shuffled = matches

# We now randomise our results, so that a "0" indicates player0 winning whilst a "1"
# indicates player1 winning
for (i in 1:nrow(matches)) {
  if (matches_shuffled$randomisedY[i] == 1) {
    matches_shuffled$winner_player_id[i] = matches$loser_player_id[i]
    matches_shuffled$loser_player_id[i] = matches$winner_player_id[i]
    
    matches_shuffled$winner_column[i] = matches$loser_column[i]
    matches_shuffled$loser_column[i] = matches$winner_column[i]
  }
}

# Now we don't need the original (sorted) "matches" dataframe anymore, so we overwrite it
matches = matches_shuffled
rm(matches_shuffled)

# We rename to player0 and player1
colnames(matches) = c("tourney_dates",
                      "match_id",
                      "player0",
                      "player1",
                      "player0_rank_id",
                      "player1_rank_id",
                      "Y")


# Merging matches and rankings ---------------------------------------

# We merge matches and rankings by rank ID and identifier in 
matches = merge(matches, rankings, by.x = "player0_rank_id", by.y = "identifier")
matches = merge(matches, rankings, by.x = "player1_rank_id", by.y = "identifier")


# Saving -------------------------------------------------------------

# Saving this shuffled matches with the correctly assigned rankings so
# we can keep the old matches data for later reference.
matches_w_rkns = matches
# save(matches_w_rkns, file = "../Roeser, Jonas - 2_Data/matches_w_rkns.RData")