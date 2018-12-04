
# Name: Andrew Weathers,Madhumita Krishnan,Siddhant Aggarwal
#Modelling for Crash Count versus Speed Count
#Polynomial Regression
plot(everyCamera$ViolationCount,everyCamera$CrashesCount,main="TrafficCrashes versus SpeedViolation",xlab="Speed Violation", ylab="Traffic Crash")

fit <- lm(everyCamera$CrashesCount ~ poly(everyCamera$ViolationCount, 4))
count.grid <- seq(from = min(everyCamera$ViolationCount), to = max(everyCamera$ViolationCount),length.out = 150)
newdata <-list(count.grid)
pred=predict(fit,newdata,se=TRUE)
se.bands <- cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)

plot(everyCamera$ViolationCount,everyCamera$CrashesCount,cex=.5,col="black",main="Degree-4Polynomial",xlab="Speed Violation",ylab="Traffic Crash")
lines(count.grid,pred$fit,lwd=2)
matlines(count.grid,se.bands,lwd=1,col="blue",lty="dashed")



# Regression Splines
library(splines)

knot.position <- c(250,750,1450)
fit <- lm(everyCamera$CrashesCount ~ bs(everyCamera$ViolationCount, knots=knot.position), data=everyCamera)
count.grid <- seq(from = min(everyCamera$ViolationCount), to = max(everyCamera$ViolationCount),length.out = 150)
pred <- predict(fit,newdata=list(count=count.grid) , se=TRUE)

plot(everyCamera$ViolationCount,everyCamera$CrashesCount,xlab="Speed Violation",ylab="Traffic Crash", cex=.5, col="black",main="Cubic Spline Model")
lines(count.grid, pred$fit, lwd=2, col="blue")
lines(count.grid, pred$fit + 2*pred$se, lty="dashed",lwd=1, col="blue")
lines(count.grid, pred$fit - 2*pred$se, lty="dashed",lwd=1, col="blue")
abline(v = knot.position, lty="dashed")

summary(fit)


#smoothing splines

fit <- smooth.spline(everyCamera$ViolationCount,everyCamera$CrashesCount, df=10)
fit2 <- smooth.spline(everyCamera$ViolationCount,everyCamera$CrashesCount, cv=TRUE)

plot(everyCamera$ViolationCount,everyCamera$CrashesCount, cex=.5, col="darkgray")
lines(fit,  col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("10 DF"," (LOOCV)"), col=c("red", "blue"),lty=1, lwd=2, cex=.8)






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
