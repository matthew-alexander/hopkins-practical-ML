library(caret)
library(rpart)
library(randomForest)

setwd("~/GitHub/hopkins-practical-ML")
train <- read.csv("pml-training.csv",  na.strings=c("NA","#DIV/0!",""))
test <- read.csv("pml-testing.csv",  na.strings=c("NA","#DIV/0!",""))
names(data)
summary(data$classe)
# the classe variable is the one to predict on 

carlitos = subset(data, user_name == "carlitos")
table(carlitos$classe)
transform(carlitos, classe=factor(classe))

# data cleaning

key_columns <- c(7:11, 37:49, 60:68, 84:86, 102, 113:124, 151:159, 160)
train_small <- train[,key_columns]
test_small <- test[,key_columns]





# svd 
carlitos_matrix <- data.matrix(carlitos_small, rownames.force = NA)

svd1 <- svd(carlitos_matrix)





# tree

tree <- rpart(classe ~ ., data=train_small, method="class")
predictions_tree <- predict(tree, test_small, type = "class")

# random forest ( this classified the test best on the quiz)
rf <- randomForest(classe ~ ., data=train_small, ntree = 200)
predictions_rf <- predict(rf, test_small)

#we need a validation set to test on
confusionMatrix(predictions_tree, test_small$classe)

