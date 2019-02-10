
# CREATING HEAD TO HEAD COMPARISON

# Setup --------------------------------------------------------------

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/D1.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/D1.RData")


# Creating h2h feature ---------------------------------------------------

# Creating three columns for h2h
D1 = D1[c(58,20,38,56,2)] %>%
  mutate(h2h_player0 = 0) %>%
  mutate(h2h_player1 = 0) %>%
  mutate(h2h = 0)

for (i in 1:nrow(D1)) {
  i = 29723
  # Gives us all rows of specific p0 vs p1 matches (and the other way around)
  m_p0vp1 = which(as.character(D1$player1) == as.character(D1$player1[i]) & as.character(D1$player0) == as.character(D1$player0[i]))
  m_p1vp0 = which(as.character(D1$player0) == as.character(D1$player1[i]) & as.character(D1$player1) == as.character(D1$player0[i]))

  smaller_i_i0 = which(m_p0vp1 < i)
  smaller_i_i1 = which(m_p1vp0 < i)
  smaller_i0 = m_p0vp1[smaller_i_i0]
  smaller_i1 = m_p1vp0[smaller_i_i1]
  
  smaller_iboth = sort(append(smaller_i0, smaller_i1))
  D1$h2h[i] = length(smaller_iboth)
  
  for (j in 1:length(smaller_i0)) {
    if (length(smaller_i0) == 0) {
      D1$h2h_player0[i] = 0
      D1$h2h_player1[i] = 0
    } else if (D1[smaller_i0[j],4] == 0) {
      D1$h2h_player0[i] = D1$h2h_player0[i] + 1
    } else {
      D1$h2h_player1[i] = D1$h2h_player1[i] + 1
    }
  }
  for (j in 1:length(smaller_i1)) {
    if (length(smaller_i1) == 0) {
      
    } else if (D1[smaller_i1[j],4] == 1) {
      D1$h2h_player0[i] = D1$h2h_player0[i] + 1
    } else {
      D1$h2h_player1[i] = D1$h2h_player1[i] + 1
    }
  }
}

# Try out different versions of head to head to minimate variance due preventing a math error...

#
D1$h2h_v2_player0 = D1$h2h_player0 + 1
D1$h2h_v2_player1 = D1$h2h_player1 + 1
D1$h2h_v2 = D1$h2h + 2

#
D1$h2h_v2_player0 = D1$h2h_v2_player0/D1$h2h_v2
D1$h2h_v2_player1 = D1$h2h_v2_player1/D1$h2h_v2

#
D1$h2h_v3_player0 = D1$h2h_v2_player0/D1$h2h_v2_player1
D1$h2h_v3_player1 = D1$h2h_v2_player1/D1$h2h_v2_player0

for (i in 1:nrow(D1)) {
  i = 29723
  if (D1[[i,7]] != 0) {
    D1$h2h_player0[i] = D1$h2h_player0[i]/D1$h2h[i]
    D1$h2h_player1[i] = D1$h2h_player1[i]/D1$h2h[i]
  }
}
# --> We tried out all three versions and had the best accuracy for version three
#     so that we will take version three as a feature in our final data frame


# Saving -------------------------------------------------------------

# Saving "D1" as "h2h.RData"
h2h = D1
# save(h2h, file = "../Roeser, Jonas - 2_Data/h2h.RData")