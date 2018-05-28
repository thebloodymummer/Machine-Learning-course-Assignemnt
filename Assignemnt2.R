library(caret)
library(rpart)
library(randomForest)
library(rattle)
library(ggplot2)

set.seed(333)

if(!file.exists("rawData")){dir.create("rawData")}
train <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
traindest <- "./rawData/train.csv"
testdest <- "./rawData/test.csv"
download.file(train, destfile = traindest)
download.file(test, destfile = testdest)
trainData <- read.csv("./rawData/train.csv", na.strings=c("NA","#DIV/0!", ""))
testData <- read.csv("./rawData/test.csv", na.strings=c("NA","#DIV/0!", ""))

trainData <- trainData[,8:length(colnames(trainData))]
testData <- testData[,8:length(colnames(testData))]

trainData <- trainData[, colSums(is.na(trainData)) == 0] 
testData <- testData[, colSums(is.na(testData)) == 0]

nzv <- nearZeroVar(trainData,saveMetrics=TRUE)
zero.var.ind <- sum(nzv$nzv)
if ((zero.var.ind>0)) { trainData <- trainData[,nzv$nzv==FALSE]}

inTrain <- createDataPartition(y=trainData$classe, p=0.6, list=FALSE)
trainData2 <- trainData[inTrain, ] 
validation <- trainData[-inTrain, ]
dim(trainData2); dim(validation)

ggplot(trainData2, 
       aes(x = classe)) + geom_bar() + labs(main = "Plot of levels of variable classes",
                                            x = "Classe", 
                                            y = "Frequency")

classModel <- rpart(classe ~ ., data=trainData2, method="class")
classPredication <- predict(classModel, validation, type = "class")
fancyRpartPlot(classModel$finalModel)
confusionMatrix(classPredication, validation$classe)