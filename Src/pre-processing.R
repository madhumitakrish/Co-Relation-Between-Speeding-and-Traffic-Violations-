
rm(list=ls())

# Name(s): Andrew Weathers ...
# Date: 10 October 2018
# Purpose: Aggregate traffic violations dataset with crash dataset
# by common features

# Import datasets
speed <- read.csv("../Data/Speed_Camera_Violations.csv")
crash <- read.csv("../Data/Traffic_Crashes_-_Crashes.csv")

# Strip crash of uneeded features
crash <- crash[,c("CRASH_DATE", "STREET_NAME", "LATITUDE", "LONGITUDE")]
speed <- speed[,c("CAMERA.ID", "LATITUDE", "LONGITUDE")]

# Latitude and longitude is required 
speed <- speed[!is.na(speed$LONGITUDE),]
speed <- speed[!is.na(speed$LATITUDE),]
crash <- crash[!is.na(crash$LONGITUDE),]
crash <- crash[!is.na(crash$LATITUDE),]

# Give a feature to both crash and speed called type
crash$type <- "crash"
speed$type <- "speed"

# Aggregate df's by unique identifiers
coords <- merge(crash, speed, by=c("LONGITUDE", "LATITUDE"), all=TRUE)
coords <- coords[, c("type.x", "type.y", "LONGITUDE", "LATITUDE")]
coords$type <- paste(coords$type.x, coords$type.y)

# Remove NA tags
coords$type <- gsub("NA", "", as.character(coords$type))
coords <- coords[,c("type", "LONGITUDE", "LATITUDE")]

# Finding Each Camera
library(sqldf)
everyCamera <- sqldf('SELECT DISTINCT * FROM speed')
everyCamera$CAMERA.ID <- gsub("CHI", "", as.character(everyCamera$CAMERA.ID))
everyCamera$type <- NULL

# Loop function to assign a nearest speeding camera
# to every crash based on Euclidean distance. 
this.distance <- 0
chosen.camera <- NULL
regions <- rep("blank", nrow(crash))
cameras <- everyCamera[,"CAMERA.ID"]

crash.long <- crash[,"LONGITUDE"]
cam.long <- everyCamera[,"LONGITUDE"]
cam.lat <- everyCamera[,"LATITUDE"]
crash.lat <- crash[,"LATITUDE"]
for(x in 1:nrow(crash)){
  shortest.distance <- 10000
  #Find shortest distance for given crash
  for(y in 1:nrow(everyCamera)){
    # Compute Euclidean distance
    this.distance <- (crash.long[x] - cam.long[y])^2 + (crash.lat[x] - cam.lat[y])^2
    if(this.distance < shortest.distance){
      shortest.distance = this.distance
      regions[x] <- cameras[y]
    }
  }
}
crash$regions <- regions

# Write both updated speed and crash datasets to file
# Also write aggregrated crash/speed 'coords' data to file
write.csv(crash, file="../Data/Mod_Crash.csv")
write.csv(speed, file="../Data/Mod_Speed.csv")
write.csv(coords, file="../Data/Aggr_Crash_Speed.csv")
