
library(ggplot2)

# Name(s): Andrew Weathers 
# Date: 17 October 2018
# Purpose: Initial exploration into our pre-processed datasets

speed$X <- NULL
crash$X <- NULL
coords$X <- NULL

# Initial Data Visualization
p <- ggplot(coords, aes(x=LONGITUDE, y=LATITUDE, col=Type)) + geom_point()

length(which(crash$distances <=1.6))
#plot(x=crash$STREET_NAME,y=crash$distances,ylim=c(0,.1))
plot(crash$distances,ylim=c(0,3.0))
plot(aggregate(data.frame(count = crash$distances), list(value = crash$distances), length))
