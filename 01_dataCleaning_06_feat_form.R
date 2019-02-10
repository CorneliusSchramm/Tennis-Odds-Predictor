
# CREATING FORM FEATURE FOR D

# Setup --------------------------------------------------------------

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/D1.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/D1.RData")


# Creating form feature for last 10 games ---------------------------------------------------

D1 = D1[c(58,2,20,38,56)] %>%
  mutate(form_10_player0 = NA) %>%
  mutate(form_10_player1 = NA)

for (i in 1:nrow(D1)) {
  # Calculating form for player0
  matches0 = which(as.character(D1$player0) == as.character(D1$player0[i]))
  matches1 = which(as.character(D1$player1) == as.character(D1$player0[i]))
  matches_smaller_i_indeces0 = which(matches0<i)
  matches_smaller_i_indeces1 = which(matches1<i)
  matches_smaller_i0 = matches0[matches_smaller_i_indeces0]
  matches_smaller_i1 = matches1[matches_smaller_i_indeces1]
  matches_smaller_iboth = sort(append(matches_smaller_i1, matches_smaller_i0))
  
  n_matches0 = length(matches_smaller_i0)
  n_matches1 = length(matches_smaller_i1)
  n_matches = length(matches_smaller_iboth)
  if (n_matches >= 10) {
    matchResults = D1$Y[matches_smaller_iboth[c((n_matches)-9):n_matches]]
    
    matchesInPlayer0Column = match(matches_smaller_i0[c((n_matches0-9):n_matches0)[which(c((n_matches0-9):n_matches0)>0)]],
                                   matches_smaller_iboth[c((n_matches)-9):n_matches])
    matchesInPlayer0ColumnNoNA = matchesInPlayer0Column[which(!is.na(matchesInPlayer0Column))]
    
    matchesInPlayer1Column = match(matches_smaller_i1[c((n_matches1-9):n_matches1)[which(c((n_matches1-9):n_matches1)>0)]],
                                   matches_smaller_iboth[c((n_matches)-9):n_matches])
    matchesInPlayer1ColumnNoNA = matchesInPlayer1Column[which(!is.na(matchesInPlayer1Column))]
    
    D1$form_10_player0[i] = length(which(matchResults[matchesInPlayer0ColumnNoNA] == 0)) + length(which(matchResults[matchesInPlayer1ColumnNoNA] == 1))
  } else {
    D1$form_10_player0[i] = NA
  }
  
  # Calculating form for player1
  matches0 = which(as.character(D1$player0) == as.character(D1$player1[i]))
  matches1 = which(as.character(D1$player1) == as.character(D1$player1[i]))
  matches_smaller_i_indeces0 = which(matches0<i)
  matches_smaller_i_indeces1 = which(matches1<i)
  matches_smaller_i0 = matches0[matches_smaller_i_indeces0]
  matches_smaller_i1 = matches1[matches_smaller_i_indeces1]
  matches_smaller_iboth = sort(append(matches_smaller_i1, matches_smaller_i0))
  
  n_matches0 = length(matches_smaller_i0)
  n_matches1 = length(matches_smaller_i1)
  n_matches = length(matches_smaller_iboth)
  if (n_matches >= 10) {
    matchResults = D1$Y[matches_smaller_iboth[c((n_matches)-9):n_matches]]
    
    matchesInPlayer0Column = match(matches_smaller_i0[c((n_matches0-9):n_matches0)[which(c((n_matches0-9):n_matches0)>0)]],
                                   matches_smaller_iboth[c((n_matches)-9):n_matches])
    matchesInPlayer0ColumnNoNA = matchesInPlayer0Column[which(!is.na(matchesInPlayer0Column))]
    
    matchesInPlayer1Column = match(matches_smaller_i1[c((n_matches1-9):n_matches1)[which(c((n_matches1-9):n_matches1)>0)]],
                                   matches_smaller_iboth[c((n_matches)-9):n_matches])
    matchesInPlayer1ColumnNoNA = matchesInPlayer1Column[which(!is.na(matchesInPlayer1Column))]
    
    D1$form_10_player1[i] = length(which(matchResults[matchesInPlayer0ColumnNoNA] == 0)) + length(which(matchResults[matchesInPlayer1ColumnNoNA] == 1))
  } else {
    D1$form_10_player1[i] = NA
  }
}


# Creating form feature for last 5 games ---------------------------------------------------

D1 = D1 %>%
  mutate(form_5_player0 = NA) %>%
  mutate(form_5_player1 = NA)

for (i in 1:nrow(D1)) {
  # Calculating form for player0
  matches0 = which(as.character(D1$player0) == as.character(D1$player0[i]))
  matches1 = which(as.character(D1$player1) == as.character(D1$player0[i]))
  matches_smaller_i_indeces0 = which(matches0<i)
  matches_smaller_i_indeces1 = which(matches1<i)
  matches_smaller_i0 = matches0[matches_smaller_i_indeces0]
  matches_smaller_i1 = matches1[matches_smaller_i_indeces1]
  matches_smaller_iboth = sort(append(matches_smaller_i1, matches_smaller_i0))
  
  n_matches0 = length(matches_smaller_i0)
  n_matches1 = length(matches_smaller_i1)
  n_matches = length(matches_smaller_iboth)
  if (n_matches >= 5) {
    matchResults = D1$Y[matches_smaller_iboth[c((n_matches)-4):n_matches]]
    
    matchesInPlayer0Column = match(matches_smaller_i0[c((n_matches0-4):n_matches0)[which(c((n_matches0-4):n_matches0)>0)]],
                                   matches_smaller_iboth[c((n_matches)-4):n_matches])
    matchesInPlayer0ColumnNoNA = matchesInPlayer0Column[which(!is.na(matchesInPlayer0Column))]
    
    matchesInPlayer1Column = match(matches_smaller_i1[c((n_matches1-4):n_matches1)[which(c((n_matches1-4):n_matches1)>0)]],
                                   matches_smaller_iboth[c((n_matches)-4):n_matches])
    matchesInPlayer1ColumnNoNA = matchesInPlayer1Column[which(!is.na(matchesInPlayer1Column))]
    
    D1$form_5_player0[i] = length(which(matchResults[matchesInPlayer0ColumnNoNA] == 0)) + length(which(matchResults[matchesInPlayer1ColumnNoNA] == 1))
  } else {
    D1$form_5_player0[i] = NA
  }
  
  # Calculating form for player1
  matches0 = which(as.character(D1$player0) == as.character(D1$player1[i]))
  matches1 = which(as.character(D1$player1) == as.character(D1$player1[i]))
  matches_smaller_i_indeces0 = which(matches0<i)
  matches_smaller_i_indeces1 = which(matches1<i)
  matches_smaller_i0 = matches0[matches_smaller_i_indeces0]
  matches_smaller_i1 = matches1[matches_smaller_i_indeces1]
  matches_smaller_iboth = sort(append(matches_smaller_i1, matches_smaller_i0))
  
  n_matches0 = length(matches_smaller_i0)
  n_matches1 = length(matches_smaller_i1)
  n_matches = length(matches_smaller_iboth)
  if (n_matches >= 10) {
    matchResults = D1$Y[matches_smaller_iboth[c((n_matches)-4):n_matches]]
    
    matchesInPlayer0Column = match(matches_smaller_i0[c((n_matches0-4):n_matches0)[which(c((n_matches0-4):n_matches0)>0)]],
                                   matches_smaller_iboth[c((n_matches)-4):n_matches])
    matchesInPlayer0ColumnNoNA = matchesInPlayer0Column[which(!is.na(matchesInPlayer0Column))]
    
    matchesInPlayer1Column = match(matches_smaller_i1[c((n_matches1-4):n_matches1)[which(c((n_matches1-4):n_matches1)>0)]],
                                   matches_smaller_iboth[c((n_matches)-4):n_matches])
    matchesInPlayer1ColumnNoNA = matchesInPlayer1Column[which(!is.na(matchesInPlayer1Column))]
    
    D1$form_5_player1[i] = length(which(matchResults[matchesInPlayer0ColumnNoNA] == 0)) + length(which(matchResults[matchesInPlayer1ColumnNoNA] == 1))
  } else {
    D1$form_5_player1[i] = NA
  }
}


# Creating form feature for last game ---------------------------------------------------

D1 = D1 %>%
  mutate(form_1_player0 = NA) %>%
  mutate(form_1_player1 = NA)

for (i in 1:nrow(D1)) {
  # Calculating form for player0
  matches0 = which(as.character(D1$player0) == as.character(D1$player0[i]))
  matches1 = which(as.character(D1$player1) == as.character(D1$player0[i]))
  matches_smaller_i_indeces0 = which(matches0<i)
  matches_smaller_i_indeces1 = which(matches1<i)
  matches_smaller_i0 = matches0[matches_smaller_i_indeces0]
  matches_smaller_i1 = matches1[matches_smaller_i_indeces1]
  matches_smaller_iboth = sort(append(matches_smaller_i1, matches_smaller_i0))
  
  n_matches0 = length(matches_smaller_i0)
  n_matches1 = length(matches_smaller_i1)
  n_matches = length(matches_smaller_iboth)
  if (n_matches >= 5) {
    matchResults = D1$Y[matches_smaller_iboth[n_matches]]
    
    matchesInPlayer0Column = match(matches_smaller_i0[n_matches0[which(n_matches0>0)]],
                                   matches_smaller_iboth[n_matches])
    matchesInPlayer0ColumnNoNA = matchesInPlayer0Column[which(!is.na(matchesInPlayer0Column))]
    
    matchesInPlayer1Column = match(matches_smaller_i1[n_matches1[which(n_matches1>0)]],
                                   matches_smaller_iboth[n_matches])
    matchesInPlayer1ColumnNoNA = matchesInPlayer1Column[which(!is.na(matchesInPlayer1Column))]
    
    D1$form_1_player0[i] = length(which(matchResults[matchesInPlayer0ColumnNoNA] == 0)) + length(which(matchResults[matchesInPlayer1ColumnNoNA] == 1))
  } else {
    D1$form_1_player0[i] = NA
  }
  
  # Calculating form for player1
  matches0 = which(as.character(D1$player0) == as.character(D1$player1[i]))
  matches1 = which(as.character(D1$player1) == as.character(D1$player1[i]))
  matches_smaller_i_indeces0 = which(matches0<i)
  matches_smaller_i_indeces1 = which(matches1<i)
  matches_smaller_i0 = matches0[matches_smaller_i_indeces0]
  matches_smaller_i1 = matches1[matches_smaller_i_indeces1]
  matches_smaller_iboth = sort(append(matches_smaller_i1, matches_smaller_i0))
  
  n_matches0 = length(matches_smaller_i0)
  n_matches1 = length(matches_smaller_i1)
  n_matches = length(matches_smaller_iboth)
  if (n_matches >= 10) {
    matchResults = D1$Y[matches_smaller_iboth[c((n_matches)-4):n_matches]]
    
    matchesInPlayer0Column = match(matches_smaller_i0[n_matches0[which(n_matches0>0)]],
                                   matches_smaller_iboth[n_matches])
    matchesInPlayer0ColumnNoNA = matchesInPlayer0Column[which(!is.na(matchesInPlayer0Column))]
    
    matchesInPlayer1Column = match(matches_smaller_i1[n_matches1[which(n_matches1>0)]],
                                   matches_smaller_iboth[n_matches])
    matchesInPlayer1ColumnNoNA = matchesInPlayer1Column[which(!is.na(matchesInPlayer1Column))]
    
    D1$form_1_player1[i] = length(which(matchResults[matchesInPlayer0ColumnNoNA] == 0)) + length(which(matchResults[matchesInPlayer1ColumnNoNA] == 1))
  } else {
    D1$form_1_player1[i] = NA
  }
}


# Creating combined weighted form of last 10 games ---------------------------------------------------

D1 = D1 %>%
  mutate(form_weighted_player0 = D1$form_1_player0 + D1$form_5_player0 + D1$form_10_player0) %>%
  mutate(form_weighted_player1 = D1$form_1_player1 + D1$form_5_player1 + D1$form_10_player1)


# Saving -------------------------------------------------------------

# Saving "D1" as "form.RData"
form = D1
# save(form, file = "../Roeser, Jonas - 2_Data/form.RData")