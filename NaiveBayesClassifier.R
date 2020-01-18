setwd("C://Users//PRATAP KUMAR//OneDrive//Desktop//New folder")
cancer <- read.csv("LungDataSet.csv")
summary(cancer)
test <- read.csv(file="C://Users//PRATAP KUMAR//OneDrive//Desktop//New folder//Test.csv", header=TRUE, sep=",")
testdata<-data.frame(test)
#checking the distribution of the target variable

table(cancer$Level)
#Patitioning the dataset into training and testing sets


library(caret) 
#pseudo-random number generator

set.seed(2)  
# This will help to divide the package into training and testing sets. 

inTrain1 <- createDataPartition(cancer$Level, p = 0.6, list = F)
datTrain1 <- cancer[inTrain1,]
datTest1 <- cancer[-inTrain1,]
#Check the rows and porportion of target variable for both training
nrow(datTrain1)
nrow(datTest1)
prop.table(table(datTrain1$Level))
prop.table(table(datTest1$Level))
#NaiveBayes in e1071
library(e1071)

#model building
# e1071model <- naiveBayes(CLASSLABEL ~ smoking + age +  cough, data=datTrain1)
e1071model <- naiveBayes(Level ~ ., data=datTrain1)
#prediction on test dataset
#Run the model again and predict classes by using the training set
e1071predictions <- predict(e1071model, datTest1)
#check prediction for the first top 5 rows in the testing data
head(e1071predictions, n=10)
head(datTest1,n=10)
#e1071predictions <- predict(e1071model, head(datTest1,n=1))

#print the confusion matrix

xtab <- table(e1071predictions, datTest1$Level)
library(caret)
#It is used to calculate the accuracy, precision, recall and F-Measure. 
library(rminer)
confusionMatrix(xtab)

#by the below lines, can predit the output of unknown samples with known samples
per<-predict(e1071model,testdata)
per




#Use metric to calculate the accuracy, precision, recall and F-Measure
#mmetric(datTest1$CLASSLABEL ,e1071predictions,c("ACC"))

