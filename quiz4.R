# practical machine learning quiz 4

library(ElemStatLearn)
library(caret)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

rf <- train(y~., data = vowel.train, method="rf" )
pred_rf <- predict(rf, vowel.test)
confusionMatrix(pred_rf, vowel.test$y)
#accuracy 0.6147

gbm <-  train(y~., data = vowel.train, method="gbm")
pred_gbm <- predict(gbm, vowel.test)
confusionMatrix(pred_gbm, vowel.test$y)
# accuracy 0.5368

#####################################################
# question2 

library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p=3/4)[[1]]
training =adData[inTrain,]
testing = adData[-inTrain,]
    
set.seed(62433)
rf <-  train(diagnosis~., data = training, method = "rf")
pred_rf <- predict(rf, testing)
confusionMatrix(pred_rf, testing$diagnosis)$overall[1]

gbm <- train(diagnosis~., data = training, method = "gbm", verbose=FALSE)
pred_gbm <- predict(gbm, testing)
confusionMatrix(pred_gbm, testing$diagnosis)$overall[1]

lda <- train(diagnosis~., data = training, method="lda")
pred_lda <- predict(lda, testing)
confusionMatrix(pred_lda, testing$diagnosis )$overall[1]

# stacking the models 
stacked <- data.frame(pred_rf, pred_gbm, pred_lda, diagnosis = testing$diagnosis)
# take the resulting data frame and use it as data to train and predict
stackedFit <- train(diagnosis~., data = stacked, method = "rf")
pred_stacked <- predict(stackedFit, stacked)
confusionMatrix(pred_stacked, stacked$diagnosis)$overall[1]

###################################################
# q3

set.seed(3523)
library(AppliedPredictiveModeling)
data("concrete")
inTrain = createDataPartition(concrete$CompressiveStrength, p=3/4)[[1]]
training = concrete[inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
lasso_fit <- train(CompressiveStrength~., data = training, method = "lasso")

plot.enet(lasso_fit$finalModel, xvar = "penalty", use.color = TRUE)

#####################################################
# q4

library(lubridate)
data = read.csv("gaData.csv")
training <- data[year(data$date)<2012,]
testing <- data[year(data$date)>2011,]
tstrain = ts(training$visitsTumblr)

library(forecast)
fit <- bats(tstrain)
