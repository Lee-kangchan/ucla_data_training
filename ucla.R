library(dplyr)
library(rpart)
library(randomForest)
library(e1071)
library(class)
ucla = read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
ucla$admit = factor(ucla$admit)
str(ucla)
n = nrow(ucla)
i = 1:n
trainingData <- sample(i, n*0.6)
testData <- setdiff(i, trainingData)
trainingData <- ucla[trainingData, ]
testData <- ucla[testData, ]


treeModel <- rpart(admit~., data = trainingData, method = 'class')
forest50 <- randomForest(admit~., data = trainingData, ntree=50)
forest1000 <- randomForest(admit~., data = trainingData, ntree=1000)
svm_rb <- svm(admit~., data=trainingData)
svm_p <- svm(admit~., data=trainingData, kernel= 'polynomial')

treeP = predict(treeModel, testData, type='class')
forest50P <- predict(forest50, testData)
forest1000P <- predict(forest1000, testData)
svm_rbP <- predict(svm_rb, testData)
svm_pP <- predict(svm_p, testData)
knn <- knn(trainingData, testData, trainingData$admit) 

table(treeP, testData$admit)
table(forest50P, testData$admit)
table(forest1000P, testData$admit)
table(svm_rbP, testData$admit)
table(svm_pP, testData$admit)
table(knn, testData$admit)

