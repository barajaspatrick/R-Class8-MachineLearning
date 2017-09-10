# library(ElemStatLearn)
# data(vowel.train)
# data(vowel.test)

# library(caret)
# library(e1071)
# library(randomForest)

# Set the variable y to be a factor variable in both the training and test set. Then set the seed to 33833. Fit (1)
# a random forest predictor relating the factor variable y to the remaining variables and (2)
# a boosted predictor using the "gbm" method. Fit these both with the train() command in the caret package.
# What are the accuracies for the two approaches on the test data set? What is the accuracy among the test 
# set samples where the two methods agree?

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
mod_rf <- train(y ~ ., data = vowel.train, method = "rf")  ## need caret package
mod_gbm <- train(y ~ ., data = vowel.train, method = "gbm")
pred_rf <- predict(mod_rf, vowel.test)
pred_gbm <- predict(mod_gbm, vowel.test)

# Extract accuracies for (1) random forests and (2) boosting
confusionMatrix(pred_rf, vowel.test$y)$overall[1]

confusionMatrix(pred_gbm, vowel.test$y)$overall[1]

predDF <- data.frame(pred_rf, pred_gbm, y = vowel.test$y)
# Accuracy among the test set samples where the two methods agree
sum(pred_rf[predDF$pred_rf == predDF$pred_gbm] == 
            predDF$y[predDF$pred_rf == predDF$pred_gbm]) / 
        sum(predDF$pred_rf == predDF$pred_gbm)


###############################################################################

# Question 2
library(caret)
library(gbm)

set.seed(3433)
library(AppliedPredictiveModeling)

data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[inTrain, ]
testing = adData[-inTrain, ]


# Set the seed to 62433 and predict diagnosis with all the other variables using a random forest (“rf”), boosted trees (“gbm”)
# and linear discriminant analysis (“lda”) model. Stack the predictions together using random forests (“rf”). What is the resulting
# accuracy on the test set? Is it better or worse than each of the individual predictions?

set.seed(62433)
mod_rf <- train(diagnosis ~ ., data = training, method = "rf")
mod_gbm <- train(diagnosis ~ ., data = training, method = "gbm")
mod_lda <- train(diagnosis ~ ., data = training, method = "lda")
pred_rf <- predict(mod_rf, testing)
pred_gbm <- predict(mod_gbm, testing)
pred_lda <- predict(mod_lda, testing)
predDF <- data.frame(pred_rf, pred_gbm, pred_lda, diagnosis = testing$diagnosis)
combModFit <- train(diagnosis ~ ., method = "rf", data = predDF)
combPred <- predict(combModFit, predDF)

# Accuracy using random forests
confusionMatrix(pred_rf, testing$diagnosis)$overall[1]

# Accuracy using boosting
confusionMatrix(pred_gbm, testing$diagnosis)$overall[1]

# Accuracy using linear discriminant analysis
confusionMatrix(pred_lda, testing$diagnosis)$overall[1]

# Stacked Accuracy
confusionMatrix(combPred, testing$diagnosis)$overall[1]











