
library(ggplot2)

# Name(s): Andrew Weathers, Siddhant Aggarwal, Madhumita Krishnan
# Date: 17 October 2018
# Purpose: Initial exploration into our pre-processed datasets

speed$X <- NULL
crash$X <- NULL
coords$X <- NULL

# All Crashes and Speed Violations in Chicago (Completely Maps Chicago)
p<-ggplot(coords, aes(x=LONGITUDE, y=LATITUDE, col=Type)) + geom_point()
p <- p + scale_color_manual(values=c("gray", "red", "blue"))+theme_bw()

# Crash Count vs Speed Count Scatterplot
plot(everyCamera$ViolationCount,everyCamera$CrashesCount,main="Traffic Crashes versus Speed Violation",xlab="Speed Violation", ylab="Traffic Crash")

# Distance from Speed Camera vs Number of Crashes
length(which(crash$distances <=1.6))
plot(crash$distances,ylim=c(0,3.0))
plot(aggregate(data.frame(count = crash$distances), list(value = crash$distances), length))

