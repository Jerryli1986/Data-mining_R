#Resubstitution 
ptm <- proc.time()
library("rpart")
acc_array<-array()
print(acc_array)
for (i in 1:20) {
  sampleRate<-0.1
  sampleSize<-nrow(iris)*sampleRate
  testSampleIdx<-sample(nrow(iris),size=sampleSize)
  #testset
  testSet<-iris[testSampleIdx,]
  #trainingSet
  trainingSet <- iris[-testSampleIdx,]
  
  iris.dt<-rpart(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=trainingSet,method="class")
  # Resubstitution method: acc/error on the training set
  Prediction<-predict(iris.dt, newdata=trainingSet, type="class")
  cM <- table(trainingSet$Species, Prediction)
  acc <- sum(diag(cM))/sum(cM)
  acc_array[i]<- acc
}
M<-mean(acc_array)
print(M) 
S<-sd(acc_array)
print (S)

runningtime<-proc.time()-ptm
print(runningtime)


####################################### hold out
ptm <- proc.time()
library("rpart")
acc_array<-array()
print(acc_array)
for (i in 1:20) {
  sampleRate<-0.1
  sampleSize<-nrow(iris)*sampleRate
  paste("sample size is:",sampleSize)
  #set.seed(1234)
  testSampleIdx<-sample(nrow(iris),size=sampleSize)
  #testSampleIdx
  testSet<-iris[testSampleIdx,]
  #trainingSet
  trainingSet <- iris[-testSampleIdx,]
  
  paste("Test set size<",nrow(testSet),">; Training set size<", nrow(trainingSet),
        ">", sep="")
  
  iris.dt<-rpart(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=trainingSet,method="class")
  # Hold-out method: acc/error on the test set
  prediction <- predict(iris.dt, newdata=testSet, type="class")
  cM <- table(testSet$Species, prediction)
  print(cM)
  acc <- sum(diag(cM))/sum(cM)
  rErr <- 1.0 - acc
  print(paste("hold-out method: accuracy = ", round(acc*100,1), "% and error = ",
              round(rErr*100,1), "%", sep=""))
  acc_array[i]<- acc
}
M<-mean(acc_array)
#print(M) 
S<-sd(acc_array)
print (S)
runningtime<-proc.time()-ptm
print(runningtime)
############################################### 10-fold xVal
ptm <- proc.time()
library("caret")
library("rpart")

acc_array<-array()
for (i in 1:20) 
{
  #mySample <- iris[sample.int(nrow(iris)),]
  correct_prediction<-0
  require(caret)
  folds<-createFolds(c(1:150),10)
  #print(folds)
  for (j in 1:10)
  {
    testSet<-iris[folds[[j]],]
    trainingSet<-iris[-folds[[j]],]
    #paste("Test set size<",nrow(testSet),">; Training set size<", nrow(trainingSet),
    #      ">", sep="")
    iris.dt<-rpart(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=trainingSet,method="class")
    prediction <- predict(iris.dt, newdata=testSet, type="class")
    cM <- table(testSet$Species, prediction)
    correct_prediction=correct_prediction+sum(diag(cM))
    #print(cM)
    #print(sum(diag(cM)))
  }
  acc<-correct_prediction/nrow(iris)
  rErr <- 1.0 - acc
 # print(paste("hold-out method: accuracy = ", round(acc*100,1), "% and error = ",
 #             round(rErr*100,1), "%", sep=""))
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
  
  samplesize <- nrow(iris) * 0.9 + 1
  sampleindex <- sample(nrow(iris), size = samplesize)
  sampleset <- iris[sampleindex, ]
  for (j in 1:length(sampleindex))
  {
    trainingSet <- sampleset[-j, ]
    testSet <- sampleset[j, ]
   # paste(
   #   "Test set size<",
   #   nrow(testSet),
   #   ">; Training set size<",
   #   nrow(trainingSet),
    #  ">",
   #   sep = ""
   # )
    iris.dt <-
      rpart(
        Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
        data = trainingSet,
        method = "class"
      )
    prediction <- predict(iris.dt, newdata = testSet, type = "class")
    cM <- table(testSet$Species, prediction)
    correct_prediction = correct_prediction + sum(diag(cM))
  #  print(cM)
  #  print(sum(diag(cM)))
  }
  acc <- correct_prediction / length(sampleindex)
  rErr <- 1.0 - acc
#  print(paste(
 #   "hold-out method: accuracy = ",
 #   round(acc * 100, 1),
 #   "% and error = ",
  #  round(rErr * 100, 1),
 #   "%",
  #  sep = ""
 # ))
  acc_array[i] <- acc
}
Acc_mean <- mean(acc_array)
print(Acc_mean)
Acc_sd <- sd(acc_array)
print (Acc_sd)
runningtime<-proc.time()-ptm
print(runningtime)

