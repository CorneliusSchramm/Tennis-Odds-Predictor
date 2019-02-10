
# CREATING WINS ON PLAYING CONDITIONS FEATURE

# Setup --------------------------------------------------------------

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/D1.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/D1.RData")


# Creating wins on playing conditions feature ---------------------------------------------------

D1 = D1[c(58,2,9,10,20,38,56)] %>%
  mutate(condition_wins_player0 = NA) %>%
  mutate(condition_wins_player1 = NA)

for (i in 1:nrow(D1)) {
  # Calculating condition wins for player0
  matches0 = which(as.character(D1$player0) == as.character(D1$player0[i]) &
                     D1$tourney_conditions == D1$tourney_conditions[i] &
                     D1$tourney_surface == D1$tourney_surface[i])
  matches1 = which(as.character(D1$player1) == as.character(D1$player0[i]) &
                     D1$tourney_conditions == D1$tourney_conditions[i] &
                     D1$tourney_surface == D1$tourney_surface[i])
  matches_smaller_i_indeces0 = which(matches0<i)
  matches_smaller_i_indeces1 = which(matches1<i)
  matches_smaller_i0 = matches0[matches_smaller_i_indeces0]
  matches_smaller_i1 = matches1[matches_smaller_i_indeces1]
  matches_smaller_iboth = sort(append(matches_smaller_i1, matches_smaller_i0))
  
  n_matches0 = length(matches_smaller_i0)
  n_matches1 = length(matches_smaller_i1)
  n_matches = length(matches_smaller_iboth)

    matchResults = D1$Y[matches_smaller_iboth[1:n_matches]]
    
    matchesInPlayer0Column = match(matches_smaller_i0[1:n_matches0],
                                   matches_smaller_iboth[1:n_matches])
    
    matchesInPlayer1Column = match(matches_smaller_i1[1:n_matches1],
                                   matches_smaller_iboth[1:n_matches])
    
    D1$condition_wins_player0[i] = (length(which(matchResults[matchesInPlayer0Column] == 0)) +
                                   length(which(matchResults[matchesInPlayer1Column] == 1))) / n_matches
  
  # Calculating condition wins for player1
    matches0 = which(as.character(D1$player0) == as.character(D1$player1[i]) &
                       D1$tourney_conditions == D1$tourney_conditions[i] &
                       D1$tourney_surface == D1$tourney_surface[i])
    matches1 = which(as.character(D1$player1) == as.character(D1$player1[i]) &
                       D1$tourney_conditions == D1$tourney_conditions[i] &
                       D1$tourney_surface == D1$tourney_surface[i])
    matches_smaller_i_indeces0 = which(matches0<i)
    matches_smaller_i_indeces1 = which(matches1<i)
    matches_smaller_i0 = matches0[matches_smaller_i_indeces0]
    matches_smaller_i1 = matches1[matches_smaller_i_indeces1]
    matches_smaller_iboth = sort(append(matches_smaller_i1, matches_smaller_i0))
    
    n_matches0 = length(matches_smaller_i0)
    n_matches1 = length(matches_smaller_i1)
    n_matches = length(matches_smaller_iboth)
    
    matchResults = D1$Y[matches_smaller_iboth[1:n_matches]]
    
    matchesInPlayer0Column = match(matches_smaller_i0[1:n_matches0],
                                   matches_smaller_iboth[1:n_matches])
    
    matchesInPlayer1Column = match(matches_smaller_i1[1:n_matches1],
                                   matches_smaller_iboth[1:n_matches])
    
    D1$condition_wins_player1[i] = (length(which(matchResults[matchesInPlayer0Column] == 0)) +
                                      length(which(matchResults[matchesInPlayer1Column] == 1))) / n_matches
}


# Saving -------------------------------------------------------------

# Saving "D3" as "D3.RData"
condition_wins = D1
# save(condition_wins, file = "../Roeser, Jonas - 2_Data/condition_wins.RData")