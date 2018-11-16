rm(list=ls())

library(ggplot2)

# Name(s): Andrew Weathers ...
# Date: 17 October 2018
# Purpose: Initial exploration into our pre-processed datasets

# Import pre-processed data
speed <- read.csv("../Data/Mod_Speed.csv")
crash <- read.csv("../Data/Mod_Crash.csv")

speed$X <- NULL
crash$X <- NULL

# Ensure Speed violation camera is perfectly correlated with Latitude and Longitude
allCamViolations <- data.frame(speed$CAMERA.ID, speed$LATITUDE, speed$LONGITUDE)
# --- MISSING: official results of this

# Remove camera from dataset to avoid overfitting
speed$CAMERA.ID <- NULL

# Round Longitude and Latitude to 2 decimal places
#speed$LATITUDE <- round(speed$LATITUDE, 2)
#speed$LONGITUDE <- round(speed$LONGITUDE, 2)

#speed[, lapply(.SD, round, 2), "LONGITUDE"]
#speed[, lapply(.SD, round, 2), "LATITUDE"]

#crash$LATITUDE <- round(crash$LATITUDE, 2)
#crash$LONGITUDE <- round(crash$LONGITUDE, 2)

#crash[, lapply(.SD, round, 2), "LONGITUDE"]
#crash[, lapply(.SD, round, 2), "LATITUDE"]

# Merge datasets by similar features

# Common date 
crash$date <- substring(crash$CRASH_DATE, 1, 10)
speed$date <- speed$VIOLATION.DATE

# Define type to differentiate
crash$type <- "crash"
speed$type <- "speed"

# Aggregate df's by unique identifiers
coords <- merge(crash, speed, by=c("date", "LONGITUDE", "LATITUDE"), all=TRUE)
coords <- coords[, c("date", "type.x", "type.y", "LONGITUDE", "LATITUDE")]
coords$type <- paste(coords$type.x, coords$type.y)

# Remove NA tags
coords$type <- gsub("NA", "", as.character(coords$type))
coords <- coords[,c("date", "type", "LONGITUDE", "LATITUDE")]

# Initial Data Visualization
p <- ggplot(coords, aes(x=LONGITUDE, y=LATITUDE, col=type)) + geom_point()

# Finding Each Camera
library(sqldf)
EveryCamera <- sqldf('SELECT DISTINCT * FROM allCamViolations')
EveryCamera$CameraID <- gsub("CHI", "", as.character(EveryCamera$speed.CAMERA.ID))

# Loop function to assign a nearest speeding camera
# to every crash based on Euclidean distance. 

#for(crash in 1:nrow(crash)){
  # Get next camera
  #for(camera in 1:nrow(EveryCamera)){
    # Compute Euclidean distance

    #if(this.distance < best.distance)
      # Update nearest camera
  #}
#}

