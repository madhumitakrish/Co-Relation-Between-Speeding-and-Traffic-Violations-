
# Name: Andrew Weathers

# ---------- Null model for Crash-Speed Correlation ---------- 
index<-sample(1:nrow(everyCamera), nrow(everyCamera)*0.7)
train<-everyCamera[index,]
test<-everyCamera[-index,]
avgCrashCount <- mean(train$CrashesCount)

offset <- abs(test$CrashesCount - avgCrashCount)

# MSE for Null Model
nullMSE <- mean(offset^2)

# Mean Error
nullRawError <- mean(offset)

# ---------- LASSO for Crash-Speed Correlation ----------
library(glmnet)
set.seed(99)

x <- data.matrix(everyCamera[,-grep("CrashesCount", colnames(everyCamera))])
y <- as.numeric(data.matrix(everyCamera[,grep("CrashesCount", colnames(everyCamera))]))

train<-sample(1:nrow(x), nrow(x)*.7)
y.test <- y[-train]

grid<-10^seq(10,-2, length =100)
lasso.mod <- glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

cv.lasso <- cv.glmnet(x[train,],y[train],family="gaussian",alpha=1, standardize=FALSE)
bestlam <- cv.lasso$lambda.min
lasso.pred<-predict(lasso.mod,s=bestlam,newx=x[-train,])

lassoMSE <- mean((lasso.pred-y.test)^2) #MSE for LASSO
lassoRawError <- mean(abs(lasso.pred-y.test))

# ---------- Quantify MSE Difference Between Models ---------- 
# By how much did MSE improve (%)
# Negative number means it got worse
decrease <- nullMSE - lassoMSE
perMSEImprove <- (decrease / nullMSE) * 100
