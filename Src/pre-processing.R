
rm(list=ls())
library(plyr)

# Name(s): Andrew Weathers, Siddhant Aggarwal, Madhumita Krishnan
# Date: 10 October 2018
# Purpose: Aggregate traffic violations dataset with crash dataset
# by common features

# Import datasets

speed <- read.csv("../Data/New_Speed.csv")
crash <- read.csv("../Data/New_Crash.csv")

# ---------- Data Cleaning ----------
# Strip crash of uneeded features
crash <- crash[,c("POSTED_SPEED_LIMIT", "STREET_NAME", "LATITUDE", "LONGITUDE")]
speed <- speed[,c("CAMERA.ID", "LATITUDE", "LONGITUDE")]

speed_limits <- crash$POSTED_SPEED_LIMIT


# Latitude and longitude is required 
speed <- speed[!is.na(speed$LONGITUDE),]
speed <- speed[!is.na(speed$LATITUDE),]
crash <- crash[!is.na(crash$LONGITUDE),]
crash <- crash[!is.na(crash$LATITUDE),]

# Give a feature to both crash and speed called type
crash$Type <- "crash"
speed$Type <- "speed"

# Aggregate df's by unique identifiers
coords <- merge(crash, speed, by=c("LONGITUDE", "LATITUDE"), all=TRUE)
coords <- coords[, c("Type.x", "Type.y", "LONGITUDE", "LATITUDE")]
coords$Type <- paste(coords$Type.x, coords$Type.y)

# Remove NA tags
coords$Type <- gsub("NA", "", as.character(coords$Type))
coords <- coords[,c("Type", "LONGITUDE", "LATITUDE")]

# ---------- Finding Each Camera ----------
library(sqldf)
everyCamera <- sqldf('SELECT DISTINCT * FROM speed')
everyCamera$CAMERA.ID <- gsub("CHI", "", as.character(everyCamera$CAMERA.ID))
everyCamera$Type <- NULL

# Remove duplicates 
everyCamera <- everyCamera[!duplicated(everyCamera$CAMERA.ID),]
everyCamera <- everyCamera[order(everyCamera$CAMERA.ID),]

# Number of violations on each Camera
violationCount <- count(speed, vars=("CAMERA.ID"))
everyCamera$ViolationCount <- violationCount$freq

# Loop function to assign a nearest speeding camera
# to every crash based on Euclidean distance. 
this.distance <- 0
chosen.camera <- NULL
regions <- rep("blank", nrow(crash))
cameras <- everyCamera[,"CAMERA.ID"]
distances<- rep("blank", nrow(crash))

crash.long <- crash[,"LONGITUDE"]
cam.long <- everyCamera[,"LONGITUDE"]
cam.lat <- everyCamera[,"LATITUDE"]
crash.lat <- crash[,"LATITUDE"]

regions <- rep("blank", nrow(crash))
distances <- rep("blank", nrow(crash))
cameras <- everyCamera[,"CAMERA.ID"]
for(x in 1:nrow(crash)){
  
  #Find shortest distance for given crash
  shortest.distance <- 10000
  
  #Count all crashes near the camera
  for(y in 1:nrow(everyCamera)){
    # Compute Euclidean distance
    this.distance <- (111.320*cos(crash.lat[x] - cam.lat[y])*(crash.long[x] - cam.long[y]))^2 + ((crash.lat[x] - cam.lat[y])*110.574)^2
    if(this.distance < shortest.distance){
      shortest.distance <- this.distance
      regions[x] <- cameras[y]
      distances[x]<-floor(((sqrt(this.distance))*10))/10
    }
  }
}

crash$regions <- regions
crash$distances <- distances

#Number of Traffic Crashes in each region
CrashesCount<-count(crash,vars=("regions"))
everyCamera$CrashesCount<-CrashesCount$freq

# Write both updated speed and crash datasets to file
# Also write aggregrated crash/speed 'coords' data to file
write.csv(crash, file="../Data/Mod_Crash.csv")
write.csv(speed, file="../Data/Mod_Speed.csv")
write.csv(coords, file="../Data/Aggr_Coordinates.csv")
write.csv(everyCamera, file="../Data/CameraMetrics.csv")
