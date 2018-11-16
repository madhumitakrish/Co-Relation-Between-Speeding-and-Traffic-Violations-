
library(caret)
rm(list=ls())

# Name(s): Andrew Weathers ...
# Date: 10 October 2018
# Purpose: Aggregate traffic violations dataset with crash dataset
# by common features

# Import datasets
speed <- read.csv("../Data/Speed_Camera_Violations.csv")
crash <- read.csv("../Data/Traffic_Crashes_-_Crashes.csv")

# Remove observations which are missing a location 
speed <- speed[!is.na(speed$LONGITUDE),]
speed <- speed[!is.na(speed$LATITUDE),]

crash <- crash[!is.na(crash$LONGITUDE),]
crash <- crash[!is.na(crash$LATITUDE),]

# Strip crash of uneeded features
crash <- crash[,c("CRASH_DATE", "LATITUDE", "LONGITUDE")]

# Remove observations with little variance
speedRem <- nearZeroVar(speed)
crashRem <- nearZeroVar(crash)

# Apply NA to all blank entries in our data
speed[speed == ""] <- NA
crash[crash == ""] <- NA

if(length(speedRem) > 0)
  speed <- speed[-speedRem]

if(length(crashRem) > 0)
  crash <- crash[-crashRem]

# Write both updated speed and crash datasets to file
write.csv(crash, file="../Data/Mod_Crash.csv")
write.csv(speed, file="../Data/Mod_Speed.csv")

# Combine datasets by location


