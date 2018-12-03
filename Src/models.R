
# Name: Andrew Weathers
#Regression Splines Model for Distance of Crash from Camera versus Number of Crashes
library(splines)
value.grid <- seq(from =0, to = 3, by=0.1)
aggregatedcrash<-aggregate(data.frame(count = crash$distances), list(value = crash$distances), length)
aggregatedcrash<- aggregatedcrash[aggregatedcrash$value<=3,]
N<-nrow(aggregatedcrash)
Index<-sample(N,N*0.7)
Train<-aggregatedcrash[Index,]
Test<-aggregatedcrash[-Index,]
knot.position <- c(1,2,3)
fit <- lm(count ~ bs(value, knots=knot.position), data=Train)



pred <- predict(fit,newdata= list(value=value.grid), se=TRUE)
#png(filename="../Images/SplineCrashCountvsDistance.png")
plot(Test$value,Test$count,xlab="Distance of Crash from Camera",ylab="Number of Crashes", cex=.5, col="black",main="Cubic Spline Model")
lines(value.grid, pred$fit, lwd=2, col="blue")
lines(value.grid, pred$fit + 2*pred$se, lty="dashed",lwd=1, col="blue")
lines(value.grid, pred$fit - 2*pred$se, lty="dashed",lwd=1, col="blue")
abline(v = knot.position, lty="dashed")
#dev.off()
summary(fit)
