library(caret)
library(AppliedPredictiveModeling)
library(lattice)
library(ggplot2)

## Predictiong with Trees
data("iris")
names(iris)
table(iris$Species)

inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain, ]
dim(training); dim(testing)
qplot(Petal.Width, Sepal.Width, colour = Species, data = training)
modFit <- train(Species ~ ., method = "rpart", data = training)
print(modFit$finalModel)
plot(modFit$finalModel, uniform = TRUE, main= "Classification Tree")
text(modFit$finalModel, use.n = TRUE, all = T, cex = .8)

library(rattle)
fancyRpartPlot(modFit$finalModel)
predict(modFit, newdata = testing)

## Random forest
data(iris)
library(ggplot2)
inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = FALSE)
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]

library(caret)
modFit <- train(Species ~ ., data = training, method = "rf", prox = TRUE)
modFit
getTree(modFit$finalModel, k = 2)
irisP <- classCenter(training[, c(3, 4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP)
irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col = Species, data = training)
p + geom_point(aes(x = Petal.Width, y = Petal.Length, col = Species), size = 5, shape = 4, data = irisP)
pred <- predict(modFit, testing)
testing$predRigth <- pred == testing$Species
table(pred, testing$Species)
qplot(Petal.Width, Petal.Length, colour = predRigth, data = testing, main = "newdata Predictions")

## Boosting
library(ISLR)
#data(Wage)
Wage
Wage <- subset(Wage, select = -c(logwage))
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]
modFit <- train(wage ~ ., method = "gbm", data = training, verbose = FALSE)
print(modFit)


## Model Based prediction
data("iris")
names(iris)
table(iris$Species)
inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain, ]
dim(training); dim(testing)

modlda <- train(Species ~ ., data = training, method = "lda")
modnb <- train(Species ~ ., data = training, method = "nb")
plda <- predict(modlda, testing); pnb <- predict(modnb, testing)
table(plda, pnb)
equalPredictions = (plda == pnb)
qplot(Petal.Width, Sepal.Width, colour = equalPredictions, data = testing)
