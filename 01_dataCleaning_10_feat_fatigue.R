
# CREATE FATIGUE FEATURE FOR D

# Setup --------------------------------------------------------------

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/D1.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/D1.RData")


# Creating Feature: Fatigue ------------------------------------------

D1 = D1[c(58,2,3,19,20,38)] %>%
  mutate(fatigue_player0 = NA) %>%
  mutate(fatigue_player1 = NA)

for (i in 1:nrow(D1)) {
  # Calculating Fatigue for player0
  matches0 = which(as.character(D1$player0) == as.character(D1$player0[i]) &
                     as.character(D1$tourney_year_id) == as.character(D1$tourney_year_id[i]))
  matches1 = which(as.character(D1$player1) == as.character(D1$player0[i]) &
                     as.character(D1$tourney_year_id) == as.character(D1$tourney_year_id[i]))
  matches_smaller_i_indeces0 = which(matches0<i)
  matches_smaller_i_indeces1 = which(matches1<i)
  matches_smaller_i0 = matches0[matches_smaller_i_indeces0]
  matches_smaller_i1 = matches1[matches_smaller_i_indeces1]
  matches_smaller_iboth = sort(append(matches_smaller_i1, matches_smaller_i0))
  
  n_matches0 = length(matches_smaller_i0)
  n_matches1 = length(matches_smaller_i1)
  n_matches = length(matches_smaller_iboth)
  
  if (n_matches >= 2) {
    
    matchDuration = D1$match_duration[matches_smaller_iboth[c((n_matches)-1):n_matches]]
    D1$fatigue_player0[i] = matchDuration[length(matchDuration)] + matchDuration[length(matchDuration)-1]/3
    
  } else if (n_matches == 1) {
    
    matchDuration = D1$match_duration[matches_smaller_iboth[c((n_matches)-1):n_matches]]
    D1$fatigue_player0[i] = matchDuration[length(matchDuration)]
    
  } else {
    
    D1$fatigue_player0[i] = 0
    
  }
  
  # Calculating form for player1
  matches0 = which(as.character(D1$player0) == as.character(D1$player1[i]) &
                     as.character(D1$tourney_year_id) == as.character(D1$tourney_year_id[i]))
  matches1 = which(as.character(D1$player1) == as.character(D1$player1[i]) &
                     as.character(D1$tourney_year_id) == as.character(D1$tourney_year_id[i]))
  matches_smaller_i_indeces0 = which(matches0<i)
  matches_smaller_i_indeces1 = which(matches1<i)
  matches_smaller_i0 = matches0[matches_smaller_i_indeces0]
  matches_smaller_i1 = matches1[matches_smaller_i_indeces1]
  matches_smaller_iboth = sort(append(matches_smaller_i1, matches_smaller_i0))
  
  n_matches0 = length(matches_smaller_i0)
  n_matches1 = length(matches_smaller_i1)
  n_matches = length(matches_smaller_iboth)
  
  if (n_matches >= 2) {
    
    matchDuration = D1$match_duration[matches_smaller_iboth[c((n_matches)-1):n_matches]]
    D1$fatigue_player1[i] = matchDuration[length(matchDuration)] + matchDuration[length(matchDuration)-1]/3
    
  } else if (n_matches == 1) {
    
    matchDuration = D1$match_duration[matches_smaller_iboth[c((n_matches)-1):n_matches]]
    D1$fatigue_player1[i] = matchDuration[length(matchDuration)]
    
  } else {
    
    D1$fatigue_player1[i] = 0
    
  }
}


# Saving -------------------------------------------------------------

# Saving "D1" as "fatigue.RData"
fatigue = D1
# save(fatigue, file = "../Roeser, Jonas - 2_Data/fatigue.RData")