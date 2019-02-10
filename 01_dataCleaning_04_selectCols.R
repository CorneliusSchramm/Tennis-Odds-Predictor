
# MERGE MATCHES_W_RKNS WITH ALL REMAINING DATASETS, DROPPING COLUMNS, ORDERING ROWS

# Setup --------------------------------------------------------------

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/data1991_2017.RData")
load("../Roeser, Jonas - 2_Data/matches_W_rkns.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/data1991_2017.RData")
load("../2_Data/matches_W_rkns.RData")


# Data preparation ---------------------------------------------------

# Reording columns, omitting "player_rank_id" and "week_title"
matches_w_rkns = matches_w_rkns[c(3:5,10:15,6,18:23,7)]

# Adding a match sequence id, so we can resort our data when the merging messes up the order
matches_w_rkns$match_sq = 1:nrow(matches_w_rkns)


# Merging dataframes -------------------------------------------------

# Merging "matches_w_rkns" & "playerOverviews"
D = merge(matches_w_rkns, playerOverviews, by.x = "player0", by.y = "player_id")
D = merge(D, playerOverviews, by.x = "player1", by.y = "player_id")

# Merging "D" & "matchScores"
D = merge(D, matchScores[c(1,5:7,14:23)], by = "match_id")

# Merging "D" & "tournaments"
D = merge(D, tournaments, by = "tourney_year_id")

# Merging "D" & "matchStats" 
D = merge(D, matchStats, by = "match_id")


# Dropping columns --------------------------------------------------------

# Omitting all redundant colums (e.g website links) and resorting
# all match stats dropped except match duration ( match stats would be colnr(61,62,64:73, 102:148))
D = D[c(5,1,2,70,71,74,75,79,81:83,85,87,88,58:60,63,100,4,20:22,24:26,6:11,31,33,36:38,3,39:41,43:45,12:17,50,52,55:57,18)]


# Building sequencer ------------------------------------------------------

# Create match order from "tourney_year", "tourney_order", reversed "round_order"
# Format: "year-tourney_order-reversed_round_order"

# Maximum rounds in one tourney
D$round_order[which.max(D$round_order)]
# --> 10

# Reverse the round_order to create a time logical sequence column
D = D %>%
  mutate(reversed_round_order = sapply(as.character(D$round_order), switch,
                                       "1" = "10",
                                       "2" = "09",
                                       "3" = "08",
                                       "4" = "07",
                                       "5" = "06",
                                       "6" = "05",
                                       "7" = "04",
                                       "8" = "03",
                                       "9" = "02",
                                       "10" = "01"
                                       ))

# Creating the time logical sequence column order_sq
D = D %>%
  mutate(order_sq = paste(D$tourney_year, sprintf("%02d",D$tourney_order.x), as.character(D$reversed_round_order), sep ="-"))

# Ordering D according to order_sq and renaming the rows accordingly
D = D[order(D$order_sq),]
rownames(D) = 1:nrow(D)


# Saving -------------------------------------------------------------

# Saving "D0" as "D0.RData"
D0 = D
# save(D0, file = "../Roeser, Jonas - 2_Data/D0.RData")
