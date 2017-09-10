library(ISLR); library(ggplot2); library(caret);
data(Wage)
summary(Wage)

## now we need to create a testing and training set of data

inTrain <- createDataPartition(y = Wage$wage,
                               p = .7, list = FALSE)
training <- Wage[inTrain,] # find the intersection of data in Wage and training data
testing <- Wage[-inTrain,] 
dim(training); dim(testing)

## feature plot time

featurePlot(x = training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")

qq <- qplot(age, wage, data = training, colour = jobclass)
qq + geom_smooth(method = "lm", formula = y~x)
## this allows us to see of there is any relationship between age and wage

p1 <- qplot(cutWage, age, data = training, fill = cutWage,
            geom = c("boxplot", "jitter"))
p1

## note look up "grid.arrange"

## DENSITY FUNCTION
qplot(wage, colour = education, data = training, geom = "density")

### notes and further thoughts:
        


