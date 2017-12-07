corm <- cor(sele)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
library(mlbench)
library(ggplot2)
library(gridExtra)
library(tabplot)
library(lsr)
library(corrplot)
library(xgboost)
library(Matrix)


train_data <- train %>%
  inner_join(prop, by="parcelid") %>% select(-transactiondate,-parcelid)

res <- apply(train_data, 2, function(col)(sum(is.na(col))/length(col))*100)
train1 <- train_data[res < 80]

train1 %>% group_by(propertylandusetypeid) %>% summarise(n = n()) %>% tail(12) %>%  
  ggplot(aes(x = propertylandusetypeid,y = n)) + geom_col() +
  scale_x_continuous(breaks = seq(245,275))


train1 %>% ggplot(aes(y = taxamount,x = logerror)) + geom_point()

train1 %>% group_by(yearbuilt) %>% summarise(n = n()) %>% 
  ggplot(aes(x = yearbuilt,y = n)) + geom_line() +
  scale_x_continuous(breaks = seq(1950,2017,10))



colMtx <- matrix(names(train1)[1:length(train1)], nrow = 8)
for (i in 1:ncol(colMtx)) {
  tableplot(train1, 
            select_string = c(colMtx[,i], "logerror"), 
            sortCol = "logerror", decreasing = TRUE, 
            nBins = 30)
}


numeric_features <- names(train1)[sapply(train1, is.numeric)]
numeric_features <- numeric_features[-length(numeric_features)]


corr_numtran <- cor(train1 %>% 
                      select(one_of(numeric_features)), 
                    method = "pearson", 
                    use = "pairwise.complete.obs")

corrplot(corr_numtran, method = "color", order="hclust")


impute = kNN(train1,'lotsizesquarefeet',k=3)

#############
#cleaning
#############

pro <- properties[,feature_names]
prop$taxdelinquencyflag <- ifelse(prop$taxdelinquencyflag == 'Y','Yes','No')
prop$taxdelinquencyflag <- as.factor(prop$taxdelinquencyflag)

prop$fireplaceflag <- as.factor(prop$fireplaceflag)
prop$pooltypeid10 <- ifelse(is.na(prop$pooltypeid10),'No','Yes')
prop$pooltypeid2 <- ifelse(is.na(prop$pooltypeid2),'No','Yes')
prop$pooltypeid10 <- as.factor(prop$pooltypeid10)
prop$pooltypeid2 <- as.factor(prop$pooltypeid2)
prop$yardbuildingsqft26 <- ifelse(is.na(prop$yardbuildingsqft26),0,prop$yardbuildingsqft26)

prop$yardbuildingsqft17 <- ifelse(is.na(prop$yardbuildingsqft17),0,prop$yardbuildingsqft17)








impute <- function (a, a.impute){ 
  ifelse (is.na(a), round(a.impute), a)
}


m1 = lm(buildingqualitytypeid ~ taxamount + taxvaluedollarcnt + yearbuilt + bedroomcnt + bathroomcnt + landtotaltax + roomcnt + calculatedfinishedsquarefeet, data = pro)
p1 = predict(m1)
pro$buildingqualitytypeid <- impute(pro$buildingqualitytypeid,p1)


m2 = lm(garagecarcnt ~ taxamount + taxvaluedollarcnt + yearbuilt + roomcnt + calculatedfinishedsquarefeet + bedroomcnt + bathroomcnt + fips,data = pro)
p2 = predict(m2)
train1$garagecarcnt = impute(train1$garagecarcnt,p2)



m3 = lm(garagetotalsqft ~ taxvaluedollarcnt + yearbuilt + roomcnt + fullbathcnt + calculatedbathnbr + bedroomcnt + fips + regionidcity + garagecarcnt, data = properties, subset = garagetotalsqft > 0)
p3 = predict(m3)
train1$garagetotalsqft = impute(train1$garagetotalsqft,p3)



target <- zillow1$logerror 

dtrain <- zillow1 %>% select(-logerror,-parcelid)

feature_names <- names(dtrain)

dtrain <- xgb.DMatrix(data=data.matrix(dtrain),label=target)

dtest <- xgb.DMatrix(data=data.matrix(pro),missing = NA)




foldsCV <- createFolds(target, k=3, list=TRUE, returnTrain=FALSE)

param <- list(objective = "reg:linear"
              , eval_metric = "mae" 
              , subsample = 0.7
              , max_depth = 3
              , colsample_bytree = 0.7
              , eta = 0.1
              )

xgb_cv <- xgb.cv(data=dtrain,
                      params=param,
                      nrounds=200,
                      prediction=TRUE,
                      maximize=FALSE,
                      folds=foldsCV,
                      early_stopping_rounds = 50,
                      print_every_n = 5,
                      showsd = T,
                      stratified = T
                 )

print(xgb_cv$evaluation_log[which.min(xgb_cv$evaluation_log$test_mae_mean)])
min(xgb_cv$test.error.mean)
nrounds <- xgb_cv$best_iteration

xgb <- xgb.train(params = param
                 , data = dtrain
                 # , watchlist = list(valid = dtrain[80000:90275,])
                 , nrounds = 150
                 # , early_stopping_rounds = 50
                 , verbose = 1
                 , print_every_n = 5)

importance_matrix <- xgb.importance(feature_names,model=xgb)
xgb.plot.importance(importance_matrix[1:10,])

preds <- predict(xgb,dtest)


train_predict <- round(predict(xgb, dtrain))

err = mean((target-train_predict)^2)

submission <- data.table(parcelid=properties$parcelid, 
                      '201610'=preds, 
                      '201611'=preds, 
                      '201612'=preds, 
                      '201710'=preds,
                      '201711'=preds,
                      '201712'=preds
)




###############
#GBM
###############


gbmModel <- gbm(logerror ~ .-(year + assessmentyear), 
                distribution="gaussian", interaction.depth=3, n.cores=detectCores()/2, n.trees = 1000, 
                shrinkage = 0.001, data = zillow %>% mutate(transactiondate=as.factor(transactiondate)))

gbmModel <- gbm(logerror ~ ., 
                                distribution="gaussian", interaction.depth=6, n.cores=detectCores()/2, n.trees = 3000, cv.folds = 5, 
                                 shrinkage = 0.001, data = train_data)

gbmTrainPredictions <- predict.gbm(object=gbmModel1, newdata=prop, n.trees=2000, type="response")

message(mean((gbmTrainPredictions-train_data$logerror)^2))

summpretty.gbm.tree(gbmModel)

submission <- data.table(parcelid=properties$parcelid, 
                         '201610'=gbmTrainPredictions, 
                         '201611'=gbmTrainPredictions, 
                         '201612'=gbmTrainPredictions, 
                         '201710'=gbmTrainPredictions,
                         '201711'=gbmTrainPredictions,
                         '201712'=gbmTrainPredictions
)


submission <- prop %>% select(-parcelid) %>% 
  mutate("201610"=predict.gbm(object=gbmModel, newdata=prop, n.trees=3000, type="response"), transactiondate=as.factor("2016-11-01"),
         "201611"=predict.gbm(object=gbmModel, newdata=prop, n.trees=3000, type="response"), transactiondate=as.factor("2016-12-01"),
         "201612"=predict.gbm(object=gbmModel, newdata=prop, n.trees=3000, type="response"), transactiondate=as.factor("2017-10-01"),
         "201710"=predict.gbm(object=gbmModel, newdata=prop, n.trees=3000, type="response"), transactiondate=as.factor("2017-11-01"),
         "201711"=predict.gbm(object=gbmModel, newdata=prop, n.trees=3000, type="response"), transactiondate=as.factor("2017-12-01"),
         "201712"=predict.gbm(object=gbmModel, newdata=prop, n.trees=3000, type="response")) %>% bind_cols(properties$parcelid,.) %>% 
  select(parcelid, `201610`, `201611`, `201612`, `201710`, `201711`, `201712`)







###############
#XGBoost
###############

ptrain <- model.matrix(~.,data = dtrain) 
ptest <- model.matrix(~.+0,data = prop)

xtrain <- xgb.DMatrix(data = ptrain,label = target) 
xtest <- xgb.DMatrix(data = ptest, missing = NA)

target <- train_data$logerror 
dtrain <- train_data %>% select(-logerror)

feature_names <- names(dtrain)

logilist <- c(
  "architecturalstyletypeid",
  "hashottuborspa",
  "pooltypeid10",
  "pooltypeid2",
  "typeconstructiontypeid",
  "fireplaceflag"
)

# Convert logicals to factors
properties[, logilist] <- lapply(properties[, logilist], factor)


####################
#GRID SEARCH
####################


xgbGrid <- expand.grid(
  nrounds = c(200, 300, 400),
  max_depth = c(5, 6, 7),
  eta = c(0.001, 0.003, 0.01),
  gamma = c(0, 1, 2),
  colsample_bytree = c(1, 0.5, 0.25),
  min_child_weight = c(1, 2),
  subsample = c(0.5,0.6,0.7)
)

xgbTrControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 2,
  verboseIter = FALSE,
  returnData = FALSE,
  allowParallel = TRUE
)

xgbTrain <- train(
  x = xtrain,
  y = target,
  objective = "reg:linear",
  trControl = xgbTrControl,
  tuneGrid = xgbGrid,
  method = "xgbTree"
)




