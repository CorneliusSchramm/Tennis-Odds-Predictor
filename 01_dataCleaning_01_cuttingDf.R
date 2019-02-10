
# CUTTING ALL DATASETS TO REQUIRED LENGTH

# Setup --------------------------------------------------------------

library(tidyverse)

rm(list = ls())

# Reading all of our data of the selected time period
matchScores1991_2016 = read.csv("../2_Data/Downloaded Potential Data/match_scores_1991-2016_unindexed_csv.csv")
matchScores2017 = read.csv("../2_Data/Downloaded Potential Data/match_scores_2017_unindexed_csv.csv")
matchStats1991_2016 = read.csv("../2_Data/Downloaded Potential Data/match_stats_1991-2016_unindexed_csv.csv")
matchStats2017 = read.csv("../2_Data/Downloaded Potential Data/match_stats_2017_unindexed_csv.csv")
playerOverviews = read.csv("../2_Data/Downloaded Potential Data/player_overviews_unindexed_csv.csv")
rankings1973_2017 = read.csv("../2_Data/Downloaded Potential Data/rankings_1973-2017_csv.csv")
tournaments1877_2017 = read.csv("../2_Data/Downloaded Potential Data/tournaments_1877-2017_unindexed_csv.csv")

# Because of OneDrive we need to load from two different paths
matchScores1991_2016 = read.csv("../Roeser, Jonas - 2_Data/Downloaded Potential Data/match_scores_1991-2016_unindexed_csv.csv")
matchScores2017 = read.csv("../Roeser, Jonas - 2_Data/Downloaded Potential Data/match_scores_2017_unindexed_csv.csv")
matchStats1991_2016 = read.csv("../Roeser, Jonas - 2_Data/Downloaded Potential Data/match_stats_1991-2016_unindexed_csv.csv")
matchStats2017 = read.csv("../Roeser, Jonas - 2_Data/Downloaded Potential Data/match_stats_2017_unindexed_csv.csv")
playerOverviews = read.csv("../Roeser, Jonas - 2_Data/Downloaded Potential Data/player_overviews_unindexed_csv.csv")
rankings1973_2017 = read.csv("../Roeser, Jonas - 2_Data/Downloaded Potential Data/rankings_1973-2017_csv.csv")
tournaments1877_2017 = read.csv("../Roeser, Jonas - 2_Data/Downloaded Potential Data/tournaments_1877-2017_unindexed_csv.csv")


# Cut rankings1973_2017 ----------------------------------------------

typeof(rankings1973_2017$week_year)
rankings <- rankings1973_2017 %>%
  filter(week_year >= 1991)
# Check earliest date
min(rankings$week_year)
# Check if complete
nrow(is.na(rankings$week_year))


# Cut tournaments1877_2017 -------------------------------------------

typeof(tournaments1877_2017$tourney_year)
tournaments <- tournaments1877_2017 %>%
  filter(tourney_year >= 1991)
# Check earliest date
min(tournaments$tourney_year)
# Check if complete
nrow(is.na(tournaments$tourney_year))


# Stats Check --------------------------------------------------------

# Checking if colnames of matchStats match
colnames(matchStats1991_2016) == colnames(matchStats2017)               # All true
# Checking if classes of matchStats columns match
sapply(matchStats1991_2016, class) == sapply(matchStats2017, class)     # All true
# Checking if types of matchStats columns match
sapply(matchStats1991_2016, typeof) == sapply(matchStats2017, typeof)   # All true

# Combining matchStats
matchStats = rbind(matchStats1991_2016, matchStats2017)


# Scores Check -------------------------------------------------------

# Checking if colnames of matchScores match
colnames(matchScores1991_2016) == colnames(matchScores2017)             # All true
# Checking if classes of matchScores columns match
sapply(matchScores1991_2016, class) == sapply(matchScores2017, class)   # All true
# Checking if types of matchScores columns match
sapply(matchScores1991_2016, typeof) == sapply(matchScores2017, typeof) # All true

# Combining matchScores
matchScores = rbind(matchScores1991_2016, matchScores2017)


# Removing other datasets --------------------------------------------
rm(matchStats1991_2016, 
   matchStats2017, 
   matchScores1991_2016, 
   matchScores2017,
   rankings1973_2017,
   tournaments1877_2017)


# Saving -------------------------------------------------------------

# Save all remaining datasets as "data1991_2017.RData"
# save.image(file="../Roeser, Jonas - 2_Data/data1991_2017.RData")