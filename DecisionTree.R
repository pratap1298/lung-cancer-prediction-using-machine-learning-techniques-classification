setwd("C://Users//PRATAP KUMAR//Documents//SWE2009 datamining project//data set")
cancer <- read.csv("lung21.csv")
summary(cancer)
test <- read.csv(file="C://Users//PRATAP KUMAR//Documents//SWE2009 datamining project//data set//test1.csv", header=TRUE, sep=",")
testdata<-data.frame(test)

library(rpart)
library(rpart.plot)
library(caret) 
set.seed(75)  
inTrain1 <- createDataPartition(cancer$Level, p = 0.6, list = F)
cancer_train <- cancer[inTrain1,]
cancer_test <- cancer[-inTrain1,]
dtm<-rpart(Level~Age+Gender+AirPollution+DustAllergy+OccuPationalHazards+GeneticRisk+chronicLungDisease+BalancedDiet+Obesity+Alcoholuse+Smoking+PassiveSmoker+ChestPain +CoughingofBlood+	Fatigue	+WeightLoss+ShortnessofBreath+Wheezing+SwallowingDifficulty+ClubbingofFingerNails	+FrequentCold	+DryCough+Snoring,cancer_train,method="class")

#plot(dtm)
#text(dtm)
#rpart.plot(dtm)
rpart.plot(dtm,type=4,extra=101)
p<-predict(dtm,cancer_test,type="class")

xtab <-table(cancer_test$Level,p)


library(caret)
#It is used to calculate the accuracy, precision, recall and F-Measure. 
library(rminer)
confusionMatrix(xtab)

per<-predict(dtm,testdata)
per


