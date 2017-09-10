library(AppliedPredictiveModeling)
data(segmentationOriginal)
suppressMessages(library(caret))

# 1. Subset the data to a training set and testing set based on the Case variable in the data set. 
inTrain <- createDataPartition(y = segmentationOriginal$Case, p = 0.6, 
                               list = FALSE) # 60% training
training <- segmentationOriginal[inTrain, ]
testing <- segmentationOriginal[-inTrain, ]
# 2. Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and default caret settings. (The outcome class is contained in a factor variable called Class with levels "PS" for poorly segmented and "WS" for well segmented.)
set.seed(125)
modFit <- train(Class ~ ., method = "rpart", data = training)

# 3. 
modFit$finalModel

suppressMessages(library(rattle))
library(rpart.plot)
fancyRpartPlot(modFit$finalModel)

