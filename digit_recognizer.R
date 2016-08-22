rm(list = ls())
setwd("/Users/amish913/Dropbox/kaggle/Digit Recognizer")

## Read training and test data
train <- read.csv("train.csv", header = T, sep = ",")
test <- read.csv("test.csv", header = T, sep = ",")
train$label <- as.factor(train$label)
set.seed(1234)
#head(train)
#names(train)

## Look into the data
# Create a 28*28 matrix with pixel color values
m <- matrix(unlist(train[5,-1]), nrow = 28, byrow = T)
# Plot that matrix
image(m, col=grey.colors(255))
# premilinary results
prop.table(table(train$label))
plot(train$pixel120, train$pixel124, col = train$label)

## Preprocessing 
# remove constant features to reduce complexity
for (n in names(train)){
  if (length(unique(train[,n])) == 1){
    train[,n] <- NULL
    test[,n] <- NULL
  }
}

## predictive classification modeling using random forest (benchmark)
library(randomForest)
# Random forest model
rf.1 <- randomForest(train$label~., data=train, importance = TRUE, ntree = 500)
# Significant features
varImpPlot(rf.1, type = 2)
# Predictions using test data
pred.rf1 <- predict(rf.1, test)
plot(pred.rf1)
# Preparing submission file
submission.rf1 <- data.frame(Imageid = 1:nrow(test), Label = pred.rf1)
# write onto csv file
write.csv(submission.rf1, file='submission_1_rf_digit.csv', row.names=FALSE)


## prediction against tests (if existed)
# table(pred, test$label)

