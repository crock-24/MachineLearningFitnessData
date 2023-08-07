#Libraries used for project
library(caret)
library(dplyr)
set.seed(1111)

#Links provided for data in assignment description
linkTrain <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
linkTest <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'

#download files to local device from links provided
linkTrain1 <- download.file(url = linkTrain, destfile = "linkTrain.csv", method = "curl")
linkTest1 <- download.file(url = linkTest, destfile = "linkTest.csv", method = "curl")

#read in the downloaded data to R session
trainData <- read.csv('linkTrain.csv')
testData <- read.csv('linkTest.csv')

#turn training data character into numeric factors, have to turn rest of classe sets into factors too
trainData <- transform(trainData, classe = as.numeric(as.factor(trainData$classe)))

#exploratory plot to make sure we have sufficient data for every classe type
hist(trainData$classe)

#get rid of the near zero variablity factors
trainData <- trainData[-nearZeroVar(trainData)]
testData <- testData[-nearZeroVar(testData)]

#grabbing only the numeric columns so we can do pca, can only do pca on numeric matricies to allow inversion
trainData <- select_if(trainData, is.numeric)
testData <- select_if(trainData, is.numeric)

#omitting Na's so we can build a model from the data
trainData <- na.omit(trainData)
testData <- na.omit(testData)

#difference between max and min for range of motion here --
trainData$yawROMarm <- (trainData$max_yaw_arm - trainData$min_yaw_arm)
trainData$pitchROMbelt <- (trainData$max_picth_belt - trainData$min_pitch_belt)
trainData$pitchROMdumbell <- (trainData$max_picth_dumbbell - trainData$min_pitch_dumbbell)
trainData$pitchROMforearm <- (trainData$max_picth_forearm - trainData$min_pitch_forearm)

#have to do same manipulations on test and train set
testData$yawROMarm <- (testData$max_yaw_arm - testData$min_yaw_arm)
testData$pitchROMbelt <- (testData$max_picth_belt - testData$min_pitch_belt)
testData$pitchROMdumbell <- (testData$max_picth_dumbbell - testData$min_pitch_dumbbell)
testData$pitchROMforearm <- (testData$max_picth_forearm - testData$min_pitch_forearm)

#exploratory plot to see how max pitch interacts with the quality of movement
featurePlot(x = trainData[,c("yawROMarm","pitchROMbelt", "pitchROMdumbell", "pitchROMforearm")], y = trainData$classe, plot = "pairs")

#pca analysis to show which columns are highly correlated
M <- abs(cor(trainData[,-99]))
diag (M) <- 0 
which(M > 0.9, arr.ind=T)

#Fitting the ML model
modelFit <- train(classe ~ ., method="glm", preProcess="pca", data=trainData)

#create predictions
pred <- round(predict(modelFit, subset(trainData, select = -c(classe))))
pred <- round(predict(modelFit, testData))

#limit predictions to the factors that actually show up in dataset 
pred[pred>5] <- 5
pred[pred<1] <- 1

#plotting the predictions vs the actual to see if there is a linear relationship
plot(pred, trainData$classe)

#comparing against actual values
confusionMatrix(as.factor(as.numeric(as.factor(trainData$classe))), as.factor(pred))




