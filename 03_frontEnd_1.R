
# RUNNING THE FRONT END

# install.packages("shiny")

library(tidyverse)
library(shiny)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/beta_logistic.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/beta_logistic.RData")

# install.packages("shiny")
# options(shiny.port = 4142) # set some port (maybe not necessary) --> set port from console
# options(shiny.host = "130.82.243.44") # set IP by copying IPv4 Address from cmd --> ipconfig

runApp("App")