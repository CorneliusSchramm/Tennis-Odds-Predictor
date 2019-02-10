# PLAYER PROPERTIES

# Setup --------------------------------------------------------------

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/D1.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/D1.RData")


# Ceating Feature: Age Difference ------------------------------------

# Create column with the age difference of player 0 and player 1
D1.prop = D1[c(2,29,47,35,53)] %>%
  mutate(age_diff = D1$player_age.x - D1$player_age.y) %>%
  
  
  # Ceating Feature: Height Difference ---------------------------------

# Create column with the height difference player 0 and player 1
  mutate(height_diff = D1$height_cm.x - D1$height_cm.y)

# Select relevant columns: Match_id
D1.prop = D1.prop[c(1,6,7)]


# Ceating Feature: Title difference ---------------------------------------------

D1 = D1[c(58,2,3,19,20,38,57,14)] %>%
  mutate(titles_player0 = 0) %>%
  mutate(titles_player1 = 0)

for (i in 1:nrow(D1)) {
  D1winners = D1[which(D1$reversed_round_order == 10),c(3,8)]
  
  titles = as.double(rownames(D1winners[which(as.character(D1winners$singles_winner_player_id) == as.character(D1$player0[i])),]))
  titles_smaller_i_indeces = which(titles<i)
  titles_smaller_i = titles[titles_smaller_i_indeces]
  
  D1$titles_player0[i] = length(titles_smaller_i)
  
  
  titles = as.double(rownames(D1winners[which(as.character(D1winners$singles_winner_player_id) == as.character(D1$player1[i])),]))
  titles_smaller_i_indeces = which(titles<i)
  titles_smaller_i = titles[titles_smaller_i_indeces]
  
  D1$titles_player1[i] = length(titles_smaller_i)
}

# Create column with the height difference player 0 and player 1
D1$title_diff = D1$titles_player0 - D1$titles_player1

# Saving -------------------------------------------------------------

# Saving "D1.prop" & "D1" as "playerProp.RData"
playerProp = cbind(D1.prop, D1[,11])
# save(playerProp, file = "../Roeser, Jonas - 2_Data/playerProp.RData")