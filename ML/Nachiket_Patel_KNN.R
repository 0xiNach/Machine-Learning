library(PASWR)
library(mice)
library(caret)
library(Hmisc)
library(kknn)

help("titanic3")
data("titanic3")


summary(titanic3)
aggr(titanic3)
md.pattern(titanic3)

#Three variables; fare, age, and body. 

#  Fare: 1 missing value out of 1309 obs.
#  Age: 263 missing values out of 1309 obs.
#  Body:  1188 missing values out of 1309 obs.


sum(!complete.cases(titanic3))
#1190
mean(!complete.cases(titanic3)) * 100
#90.90%

sum(is.na(titanic3))/(nrow(titanic3)*ncol(titanic3)) * 100
#7.923%

md.pattern(titanic3)

#age: Missing at Random
#fare: Missing Completely at Random
#body: Missing at Random

m = Hmisc::impute(titanic3$age, mean)
r = Hmisc::impute(titanic3$age, "random")

plot(density(m), col = "red")
lines(density(r), col = "blue")
lines(density(titanic3$age, na.rm = TRUE), col = "green")


#question2:

f = Hmisc::impute(titanic3$fare, "random")
summary(f)


ggplot(titanic3, aes(x=age, y=fare, color =titanic3$pclass))+geom_point()

titanic <- titanic3 %>% select_(.,'age', 'pclass', 'fare')
titanic

age_miss <- c(50, 10)
pclass_miss <- c('NA', 'NA')
fare_miss <- c(400, 100)

missing_df <- cbind(age_miss, pclass_miss, fare_miss)
colnames(missing_df) <- c('age', 'pclass', 'fare')

final_missing_titanic <- rbind(titanic, missing_df)


install.packages('RANN')
library(RANN) #
pre.1nn = preProcess(final_missing_titanic, method = 'knnImpute', k=1)
imputed.1nn = predict(pre.1nn, final_missing_titanic)


# 3.1:

titanic1 <- titanic3 %>% select(., pclass, survived, sex, age, sibsp, parch)

titanic1 <- cbind(titanic1, titanic$fare)
colnames(titanic1)[7] <- 'fare'


# 3.2:
titaniccomp <- titanic1[complete.cases(titanic1) ,]
# 1046 observations

titanicincomp <- titanic1[!complete.cases(titanic1 [, 4]) ,]
# 263 observations

