#loading the data
library(gdata)

mydata1<-read.csv(file="C:/Users/Jerry/Desktop/Data Analystics and Mining/assignment/breast-cancer-wisconsin.csv"
                 ,header=TRUE, sep=",")
# number to factor of class
mydata1$Class <-  as.factor(mydata1$Class)
mydata<-mydata1[1:350,]
results<-array()
#First :Resubstitution
ptm <- proc.time()
library("rpart")
acc_array<-array()
print(acc_array)
for (i in 1:20) {
  sampleRate<-0.1
  sampleSize<-round(nrow(mydata)*sampleRate)
  testSampleIdx<-sample(nrow(mydata),size=sampleSize)
  #testset
  testSet<-mydata[testSampleIdx,]
  #trainingSet
  trainingSet <- mydata[-testSampleIdx,]

  mydata.dt<-rpart(Class~Sample.code.number+Clump.Thickness
                   +Uniformity.of.Cell.Size
                   +Uniformity.of.Cell.Shape+Marginal.Adhesion
                   +Single.Epithelial.Cell.Size+Bare.Nuclei
                   +Bland.Chromatin
                   +Normal.Nucleoli+Normal.Nucleoli
                   +Mitoses
                   ,data=trainingSet,method="class")
  Prediction<-predict(mydata.dt, newdata=trainingSet, type="class")
  cM <- table(trainingSet$Class, Prediction)
  acc <- sum(diag(cM))/sum(cM)
  acc_array[i]<- acc
}

results<-array(c(mean(acc_array),sd(acc_array)))
print(results)

runningtime<-proc.time()-ptm
print(runningtime)
#Second -hold out-10%
ptm <- proc.time()
library("rpart")
acc_array<-array()
print(acc_array)
for (i in 1:20) {
  sampleRate<-0.1
  sampleSize<-round(nrow(mydata)*sampleRate)
  testSampleIdx<-sample(nrow(mydata),size=sampleSize)
  #testset
  testSet<-mydata[testSampleIdx,]
  #trainingSet
  trainingSet <- mydata[-testSampleIdx,]
  mydata.dt<-rpart(Class~Sample.code.number+Clump.Thickness
                   +Uniformity.of.Cell.Size
                   +Uniformity.of.Cell.Shape+Marginal.Adhesion
                   +Single.Epithelial.Cell.Size+Bare.Nuclei
                   +Bland.Chromatin
                   +Normal.Nucleoli+Normal.Nucleoli
                   +Mitoses
                   ,data=trainingSet,method="class")
  Prediction<-predict(mydata.dt, newdata=testSet, type="class")
  cM <- table(testSet$Class, Prediction)
  acc <- sum(diag(cM))/sum(cM)
  acc_array[i]<- acc
}

Acc_mean<-mean(acc_array)
print(Acc_mean) 
Acc_sd<-sd(acc_array)
print (Acc_sd)
runningtime<-proc.time()-ptm
print(runningtime)
############################################### 10-fold xVal
ptm <- proc.time()
library("caret")
library("rpart")

acc_array<-array()
p<-10
for (i in 1:20) 
{
  correct_prediction<-0
  require(caret)
  folds<-createFolds(c(1:nrow(mydata)),p)
  for (j in 1:p)
  {
    testSet<-mydata[folds[[j]],]
    trainingSet<-mydata[-folds[[j]],]
    mydata.dt<-rpart(Class~Sample.code.number+Clump.Thickness
                     +Uniformity.of.Cell.Size
                     +Uniformity.of.Cell.Shape+Marginal.Adhesion
                     +Single.Epithelial.Cell.Size+Bare.Nuclei
                     +Bland.Chromatin
                     +Normal.Nucleoli+Normal.Nucleoli
                     +Mitoses
                     ,data=trainingSet,method="class")
    Prediction<-predict(mydata.dt, newdata=testSet, type="class")
    cM <- table(testSet$Class, Prediction)
    correct_prediction=correct_prediction+sum(diag(cM))
  }
  acc<-correct_prediction/nrow(mydata)
  acc_array[i]<- acc
}
Acc_mean<-mean(acc_array)
print(Acc_mean) 
Acc_sd<-sd(acc_array)
print (Acc_sd)
runningtime<-proc.time()-ptm
print(runningtime)
######################################################### LOOCV method
ptm <- proc.time()
library("caret")
library("rpart")

acc_array <- array()
for (i in 1:20)
{
  correct_prediction <- 0
  
  samplesize <- round(nrow(mydata) * 0.9)+ 1
  sampleindex <- sample(nrow(mydata), size = samplesize)
  sampleset <- mydata[sampleindex, ]
  for (j in 1:length(sampleindex))
  {
    trainingSet <- sampleset[-j, ]
    testSet <- sampleset[j, ]
    mydata.dt<-rpart(Class~Sample.code.number+Clump.Thickness
                     +Uniformity.of.Cell.Size
                     +Uniformity.of.Cell.Shape+Marginal.Adhesion
                     +Single.Epithelial.Cell.Size+Bare.Nuclei
                     +Bland.Chromatin
                     +Normal.Nucleoli+Normal.Nucleoli
                     +Mitoses
                     ,data=trainingSet,method="class")
    Prediction<-predict(mydata.dt, newdata=testSet, type="class")
    cM <- table(testSet$Class, Prediction)
    correct_prediction = correct_prediction + sum(diag(cM))
  }
  acc <- correct_prediction / length(sampleindex)
  acc_array[i] <- acc
}
Acc_mean <- mean(acc_array)
print(Acc_mean)
Acc_sd <- sd(acc_array)
print (Acc_sd)
runningtime<-proc.time()-ptm
print(runningtime)


