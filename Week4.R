## libraries
library(caret)
library(AppliedPredictiveModeling)
library(lattice)
library(ggplot2)
library(Hmisc)

## Regularized regression
infoUrl = "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/prostate.info.txt"
download.file(infoUrl, destfile = "prostate.info.txt", method = "curl")
dataUrl = "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/prostate.data" 
prostate <- read.csv2(dataUrl, sep = "\t")
str(prostate)
prostate <- prostate[,-1]
str(prostate)
small <- prostate[1:5, ]
small
lm(lpsa ~ ., data = small)

## Combining predictors

library(ISLR)
data("Wage")
Wage
str(Wage)
Wage <- subset(Wage, select = -c(logwage))
inBuild <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)
validation <- Wage[-inBuild, ]
buildData <- Wage[inBuild, ]
inTrain <- createDataPartition(y = buildData$wage, p = 0.7, list = FALSE)
training <- buildData[inTrain, ]
testing <- buildData[-inTrain, ]
dim(training)
dim(testing)
dim(validation)
mod1 <- train(wage ~ ., method = "glm", data = training)
mod2 <- train(wage ~ ., method = "rf", data = training, trControl = trainControl(method = "cv"), number = 3)

pred1 <- predict(mod1, testing)
pred2 <- predict(mod2, testing)
qplot(pred1, pred2, colour = wage, data = testing)

predDF <- data.frame(pred1, pred2, wage = testing$wage)
combModFit <- train(wage ~ ., method = "gam", data = predDF)
combPred <- predict(combModFit, predDF)

# Testing erros
sqrt(sum((pred1 - testing$wage)^2))
sqrt(sum((pred2 - testing$wage)^2))
sqrt(sum((combPred - testing$wage)^2))

## Predict on validation data set
pred1V <- predict(mod1, validation)
pred2V <- predict(mod2, validation)
predVDF <- data.frame(pred1 = pred1V, pred2 = pred2V)
combPredV <- predict(combModFit, predVDF)

## Evaluate on validation
sqrt(sum((pred1V - validation$wage)^2))
sqrt(sum((pred2V - validation$wage)^2))
sqrt(sum((combPredV - validation$wage)^2))

## Google Data
library(quantmod)
from.dat <- as.Date("01/01/08", format = "%m%d%y")
to.dat <- as.Date("12/31/13", format = "%m%d%y")
getSymbols("GOOG", src = "google", from = from.dat, to = to.dat)
head(GOOG)
mGoog <- to.monthly(GOOG)
googOpen <- Op(mGoog)
ts1 <- ts(googOpen, frequency = 12)
plot(ts1, xlab = "Years + 1", ylab = "GOOG")

# Decompose a time series into parts
plot(decompose(ts1), xlab = "Years + 1")

# Training and test sets

ts1Train <- window(ts1, start = 1, end = 5)
tas1Test <- window(ts1, start = 5, end = c(7 - 0.01))
ts1Train

# Simply moving average
plot(ts1Train)
lines(ma(ts1Train, order = 3), col = "red")

# Exponential smoothing
ets1 <- ets(ts1Train, model = "MMM")
fcast <- forecast(ets1)
plot(fcast)
lines(ts1Test, col = "red")

## Get the accuracy
accuracy(fcast, ts1Test)


## Unsupervised prediction
data("iris")
inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = FALSE)
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]
dim(training); dim(testing)

#Cluster with kMAens}
kMeans1 <- kmeans(subset(training, select = -c(Species)), centers = 3)
training$clusters <- as.factor(kMeans1$cluster)
qplot(Petal.Width, Petal.Length, colour = clusters, data = training)

# compare to real labels
table(kMeans1$cluster, training$Species)

#Build predictors
modFit <- train(clusters ~ ., data = subset(training, select = -c(Species)), method = "rpart")
table(predict(modFit, training), training$Species)

# Apply on test
testClusterPred <- predict(modFit, testing)
table(testClusterPred, testing$Species)
