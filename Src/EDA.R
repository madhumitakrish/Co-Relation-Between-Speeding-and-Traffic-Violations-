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

this.distance <- 0
chosen.camera <- NULL
regions <- rep("blank", nrow(crash))
cameras <- EveryCamera[,"CameraID"]

crash.long <- crash[,"LONGITUDE"]
cam.long <- EveryCamera[,"speed.LONGITUDE"]
cam.lat <- EveryCamera[,"speed.LATITUDE"]
crash.lat <- crash[,"LATITUDE"]
for(x in 1:nrow(crash)){
  shortest.distance <- 10000
  #Find shortest distance for given crash
  for(y in 1:nrow(EveryCamera)){
    # Compute Euclidean distance
    this.distance <- (crash.long[x] - cam.long[y])^2 + (crash.lat[x] - cam.lat[y])^2
    if(this.distance < shortest.distance){
      shortest.distance = this.distance
      regions[x] <- cameras[y]
    }
  }
}

