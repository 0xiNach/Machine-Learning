
library(MASS)
data("cats")
attach(cats)
#1.1
plot(Hwt~Bwt)
#Has linear pattern
#1.2
catmodel <- lm(Hwt~Bwt)
catmodel
summary(catmodel)
#1.3
abline(lm(Hwt~Bwt), col="red")
#1.4
plot(catmodel)
#1.5
confint(catmodel,level = 0.95)
#1.6

#1.7&1.8
catmodel$fitted.values
newdata = data.frame(Bwt = c(2.8, 5, 10))
conf.band = predict(lmcats, newdata, interval = "confidence")
pred.band = predict(lmcats, newdata, interval = "prediction")
plot(Hwt~Bwt)
abline(lmcats, lty = 2) 
lines(newdata$speed, conf.band[, 2], col = "blue") 
lines(newdata$speed, conf.band[, 3], col = "blue") 
lines(newdata$speed, pred.band[, 2], col = "red")
lines(newdata$speed, pred.band[, 3], col = "red") 


#Question2
#2.1
boxcox(lmcats)
#2.2
#lambda=0
#2.3$2.4
catmodel2 <- lm(log(Hwt)~log(Bwt))
summary(catmodel2)
#2.5
plot(catmodel2)
#2.6
None
#2.7
#original model looks better on normal qq-plot 
#2.8
boxcox(catmodel2)
