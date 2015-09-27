library(caret)
library(randomForest)


pmlData<- read.csv("pml-training.csv", header = TRUE)

##Clean data

str(pmlData)
pmlDataClean <- pmlData[,-c(1:7)]

pmlDataClean[pmlDataClean==""] <- NA
pmlDataClean <- pmlDataClean[ lapply( pmlDataClean, function(x) sum(is.na(x)) / length(x) ) < 0.1 ]




##Set Train and Validation sets

set.seed(1122)

inTrain <- createDataPartition(y=pmlDataClean$classe, p=0.75, list=FALSE)

train <-pmlDataClean[inTrain,]
test <-pmlDataClean[-inTrain,]

##Exploratory

featurePlot(x=train[,c("total_accel_belt","total_accel_arm","total_accel_dumbbell","total_accel_forearm")],
            y=train$classe,plot="pairs")

##Set Model


model  <- train(classe ~ ., data = train, method = "rf")

model

predVal<- predict(model, test)
confusionMatrix(predVal, test$classe)

##Run model against Test Sample
pmlDataTest<- read.csv("pml-testing.csv", header = TRUE)
pmlDataTestClean <- pmlDataTest[,-c(1:7)]
predTest <- predict(model, pmlDataTestClean)
predTest
table(predTest)



