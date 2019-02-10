
# PREPARING THE MERGER OF MATCHES & RANKINGS DATA

# Setup --------------------------------------------------------------

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/data1991_2017.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/data1991_2017.RData")


# Checking for columns that allow us to merge the dataframes ---------

sapply(tournaments, class) # tourney_year_id "factor", tourney_dates "factor"
sapply(matchScores, class) # match_id "factor, tourney_year_id "factor"
sapply(matchStats, class) # match_id "factor"
sapply(rankings, class) # week_title "factor", player_id "factor"
sapply(playerOverviews, class) # player_id "factor", 
# We can merge every dataset except rankings without further preparation
# Furthermore: in our data the winner is always the first player of the two!


# Selecting relevant columns from matchScores -------------------------

matches = subset(matchScores, select = c(tourney_year_id,
                                    match_id,
                                    winner_player_id,
                                    loser_player_id))


# Merging matchScores & rankings ------------------------------------------

# In order to match matchScores & rankings we need to match matchScores & torunaments.
# This will give us a tourney_dates column, that can then be used to match matchScores
# rankings.
matches = subset(merge(matches, tournaments, by = "tourney_year_id"), select = c(tourney_dates,
                                                                                 match_id,
                                                                                 winner_player_id,
                                                                                 loser_player_id))

# In order to be able to match matchScores & rankings we match the new column in matches
# tourney_year_id, with the column week_title in rankings.
# However, we have to convert the dates into numeric format first. We do that by substituting
# "." for "-" so that we have them in format "xxxx-xx-xx". Then we convert them into dates,
# before finally converting them into numericals.
matches$tourney_dates = as.numeric(as.Date(chartr(".", "-", matches$tourney_dates)))
matches = matches[order(matches$tourney_dates),] # Ordering data according to date
rownames(matches) = 1:nrow(matches)

# We do the same thing for rankings. But we select the relevant columns week_title, player_id
# rank_number & ranking_points first.
rankings = subset(rankings, select = c(week_title,
                                       player_id,
                                       rank_number,
                                       ranking_points,
                                       player_age,
                                       tourneys_played,
                                       move_positions,
                                       move_direction))

rankings$week_title = as.numeric(as.Date(chartr(".", "-", rankings$week_title)))
rankings = rankings[order(rankings$week_title),] # Ordering data according to date
rownames(rankings) = 1:nrow(rankings)
rankings$identifier = 1:nrow(rankings)

# We create new columns of NAs that we can later fill up in the for loop
matches$winner_column = NA

matches$loser_column= NA

# WARNING: this for-loop takes quite some time to finish!
# for(i in 1:nrow(matches)) {
#   sth = R %>%
#     filter(week_title < matches[i,1])
#   
#   sth = sth %>%
#     filter(sth[,1] == max(sth[,1]))
#   
#   sth1 = sth %>%
#     filter(as.character(sth[,2]) == as.character(matches[i,3]))
# 
#   matches$winner_column[i] = sth1[1,9]
#   
#   sth2 = sth %>%
#     filter(as.character(sth[,2]) == as.character(matches[i,4]))
#   
#   matches$loser_column[i] = sth2[1,9]
# }


# Saving -------------------------------------------------------------

# Saving "rankings" and "matches" as "rankings.RData" and "matches.RData"
# save(matches, file = "../Roeser, Jonas - 2_Data/matches.RData")
# save(rankings, file = "../Roeser, Jonas - 2_Data/rankings.RData")