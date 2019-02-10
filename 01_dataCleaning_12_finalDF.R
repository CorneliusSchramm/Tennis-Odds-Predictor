
# ADDING FORM, H2H & CONDITION_WINS TO D; FINALIZING D

# Setup --------------------------------------------------------------

library(tidyverse)
library(dplyr)
rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/D1.RData")
load("../Roeser, Jonas - 2_Data/form.RData")
load("../Roeser, Jonas - 2_Data/condition_wins.RData")
load("../Roeser, Jonas - 2_Data/h2h.RData")
load("../Roeser, Jonas - 2_Data/fatigue.RData")
load("../Roeser, Jonas - 2_Data/homeGame.RData")
load("../Roeser, Jonas - 2_Data/playerProp.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/D1.RData")
load("../2_Data/form.RData")
load("../2_Data/condition_wins.RData")
load("../2_Data/h2h.RData")
load("../2_Data/fatigue.RData")
load("../2_Data/homeGame.RData")
load("../2_Data/playerProp.RData")

set.seed(1)




# Selecting & renaming columns of D1 --------------------------------------------------------------

# Creating DFref for reference (for presentation)
# DFref = D1[,c(2,4,6,7,22,23,27,59,40,41,45,60,56)]

D1 = D1[,c(27,59,45,60,56)]
DFref = D1

colnames(D1) = c("rank_number_p0",
                 "ranking_move_p0",
                 "rank_number_p1",
                 "ranking_move_p1",
                 "Y")


# Adding form, h2h, fatigue, homeGame & condition_wins ---------------

D1 = cbind(D1, form[,c(6:13)], h2h[,c(11,12)], condition_wins[,c(8,9)], fatigue[,c(7,8)], homeGame[,c(5,6)], playerProp[,c(2:4)])
DFref = cbind(DFref, form[,c(6:13)], h2h[,c(11,12)], condition_wins[,c(8,9)], fatigue[,c(7,8)], homeGame[,c(5,6)], playerProp[,c(2:4)])



# Creating rank difference from rank_player_0 and rank_player_1
D1 = D1 %>%
  mutate(diff_rank = D1$rank_number_p0 - D1$rank_number_p1) %>%
  mutate(diff_form_10 = D1$form_10_player0 - D1$form_10_player1) %>%
  mutate(diff_form_5 = D1$form_5_player0 - D1$form_5_player1) %>%
  mutate(diff_form_1 = D1$form_1_player0 - D1$form_1_player1) %>%
  mutate(diff_form_weighted = D1$form_weighted_player0 - D1$form_weighted_player1) %>%
  mutate(h2h = h2h_v3_player0) %>%
  mutate(diff_conditions_wins = D1$condition_wins_player0 - D1$condition_wins_player1) %>%
  mutate(fatigue_diff = D1$fatigue_player0 - D1$fatigue_player1) %>%
  mutate(home_game_diff = D1$home_game_p0 - D1$home_game_p1)


# Selecting Columns and ordering D1 ----------------------------------

# Ordering and selecting relevant features
D1 = D1[,c(22:25,29:33,5)]
colnames(D1)[3] = "title_diff"

# Prdering ad selectig DFref
DFref = DFref[,c(2,4,6,7,15,16,20,22,23,27:35,59,61,63,65,67,69,71,73,75,38,40,41,45,53,60,62,64,66,68,70,72,74,76,56)]

# separate Y from df
Y = D1$Y


# Standardizing D1 ---------------------------------------

# We dont want to standardize Y
std_params = rbind(apply(D1, 2, mean, na.rm = T),
                   apply(D1, 2, sd, na.rm = T))
# Saving "std_params" as "std_params.RData"
# save(std_params, file = "../Roeser, Jonas - 2_Data/std_params.RData")

D1 = scale(D1[1:(ncol(D1)-1)])
# --> maybe this needs to be done manually in order to be able to standardise the fornt-end input!

# Binding D1 to Y
D1 = cbind(D1,Y)


# Removing NAs ---------------------------------------

D1 = D1[complete.cases(D1),]


# Creating a DF and a shuffled version of DF --------------------
DF = D1
DF = sample_n(as.data.frame(D1),nrow(D1))


# Saving -------------------------------------------------------------

# Saving "DF" as "DF.RData"
# save(DF, file = "../Roeser, Jonas - 2_Data/DF.RData")
# save(DFref, file = "../Roeser, Jonas - 2_Data/DFref.RData")
