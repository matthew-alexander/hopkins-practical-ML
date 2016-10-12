library(caret)
library(rpart)
library(randomForest)

setwd("~/GitHub/hopkins-practical-ML")
train <- read.csv("pml-training.csv",  na.strings=c("NA","#DIV/0!",""))
test <- read.csv("pml-testing.csv",  na.strings=c("NA","#DIV/0!",""))
names(train)
summary(train$classe)


library(caTools)
set.seed(2000)

split = sample.split(train$classe, SplitRatio = 0.6)
train = subset(train, split == TRUE)
validation = subset(train, split == FALSE)

# data cleaning

key_columns <- c(7:11, 37:49, 60:68, 84:86, 102, 113:124, 151:159, 160)
train_small <- train[,key_columns]
validation_small <- validation[,key_columns]
test_small <- test[,key_columns]

# tree

tree <- rpart(classe ~ ., data=train_small, method="class")
predictions_tree <- predict(tree, validation_small, type = "class")

length(validation_small$classe)
length(predictions_tree)
dim(validation_small)

table(validation_small$classe, predictions_tree)
confusionMatrix(validation_small$classe, predictions_tree)


# random forest ( this classified the test best on the quiz)
rf <- randomForest(classe ~ ., data=train_small, ntree = 200)
predictions_rf <- predict(rf, validation_small)

#we need a validation set to test on
confusionMatrix(predictions_tree, validation_small)


predictions_rf_test <- predict(rf, test_small)



