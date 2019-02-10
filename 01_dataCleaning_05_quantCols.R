
# QUANTIFYING SELECTED COLUMNS OF D

# Setup --------------------------------------------------------------

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/data1991_2017.RData")
load("../Roeser, Jonas - 2_Data/D0.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/data1991_2017.RData")
load("../2_Data/D0.RData")


# Quantifying playing conditions --------------------------------------------------

# Forcing these colums to be characters so (sapply and) switch work (otherwise factor problems)

D0 <- D0 %>%
  mutate(tourney_conditions = sapply(as.character(D0$tourney_conditions), switch, 
                                     Outdoor = 1, 
                                     Indoor = 0) ) %>%           
  mutate(tourney_surface = sapply(as.character(D0$tourney_surface), switch, 
                                  Clay = 1,
                                  Grass = 2,
                                  Hard = 3,
                                  Carpet = 4))


# Quantifying move directions --------------------------------------------------

# To avoid getting NULLS later
D0$move_direction.x = as.double(D0$move_direction.x)
D0$move_direction.y = as.double(D0$move_direction.y)

# Forcing these colums to be characters so (sapply and) switch work (otherwise factor problems)
D0$move_direction.x = as.character(D0$move_direction.x)
D0$move_direction.y = as.character(D0$move_direction.y)

# Making NA to 0 in move directions
D0[["move_positions.x"]][is.na(D0[["move_positions.x"]])] <- 0
D0[["move_positions.y"]][is.na(D0[["move_positions.y"]])] <- 0

D0 <- D0 %>%
  mutate(move_direction.x = sapply(D0$move_direction.x, switch, 
                                   "3" = 1,
                                   "2" = (-1),
                                   "1" = 0)) %>%
  mutate(move_direction.y = sapply(D0$move_direction.y, switch, 
                                   "3" = 1, 
                                   "2" = (-1),
                                   "1" = 0)) %>%
  mutate(ranking_move_p0 = move_positions.x * move_direction.x ) %>%
  mutate(ranking_move_p1 = move_positions.y * move_direction.y )


# Quantifying round name --------------------------------------------------

D0$tourney_round_name = as.double(D0$tourney_round_name)


# Saving -------------------------------------------------------------

# Saving "D1" as D1.RData
D1 = D0
# save(D, file = "../Roeser, Jonas - 2_Data/D1.RData")