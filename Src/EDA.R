rm(list=ls())

library(ggplot2)

# Name(s): Andrew Weathers ...
# Date: 17 October 2018
# Purpose: Initial exploration into our pre-processed datasets

# Import pre-processed data
speed <- read.csv("../Data/Mod_Speed.csv")
crash <- read.csv("../Data/Mod_Crash.csv")
coords <- read.csv("../Data/Aggr_Crash_Speed.csv")
speed$X <- NULL
crash$X <- NULL
coords$X <- NULL

# Initial Data Visualization
p <- ggplot(coords, aes(x=LONGITUDE, y=LATITUDE, col=type)) + geom_point()
