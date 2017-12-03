library(data.table)
library(tidyverse)

wine <- fread("~/[10] Wine Quality.csv")
wine$quality <- as.factor(ifelse(wine$quality > 5, "high", "low"))
n <- map_lgl(wine, is.numeric)
wine[n] <- map(wine[n], scale)
set.seed(0)
train <- sample(1:nrow(wine), 0.8*nrow(wine))

plot(wine, col = wine$quality)


set.seed(0)
c = 10^(seq(-5, -.5, length = 50))
set.seed(0)
svc.model = tune(svm,
                 quality ~ .,
                 data = wine[train,],
                 kernel = "linear",
                 ranges = list(c = 10^(seq(-5, -.5, length = 100))))

summary(svc.model)
plot(svc.model$performances$cost,
     svc.model$performances$error,
     type = "l")
best = svc.model$best.model

svs.model = svm(quality ~ ., data = wine,
                    kernel = "linear",
                    cost = best$cost)
summary(svs.model)
svs.model$index
allpred = predict(svs.model, wine)
table("Predicted Values" = allpred, "True Values" = wine$quality)
plot(svs.model, wine, free.sulfur.dioxide ~ total.sulfur.dioxide)

svm.model = tune(svm,
                 quality ~ .,
                 data = wine[train,],
                 kernel = "radial",
                 ranges = list(cost = seq(.75, 1.25, length = 5), 
                gamma = seq(.55, .95, length = 5)))

summary(svm.model)
plot3d(svm.model$performances$cost,
       svm.model$performances$gamma,
       svm.model$performances$error,
       type = "s",
       size = 1)
best.svm = svm.model$best.model

svm.model.all = svm(quality ~ .,
                    data = wine,
                    kernel = "radial",
                    cost = best.svm$cost,
                    gamma = best.svm$gamma)
summary(svm.model.all)

svm.model.all$index

ypred = predict(svm.model.all, wine)
table("Predicted Values" = ypred, "True Values" = wine[,"quality"])


plot(svm.model.all, wine, free.sulfur.dioxide ~
       total.sulfur.dioxide)
