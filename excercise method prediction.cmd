---
title: "Excercise prediction"
output: html_document
---
This writeup will analyse the data from accelerometers on the belt, forearm, arm, and dumbell 

of 6 participants, to predict the manner(the "classe" variable in the training set) in which 

they did the exercise. 

The data available in https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv and 

more information is available from the website here: http://groupware.les.inf.puc-rio.br/har 

(see the section on the Weight Lifting Exercise Dataset). 

```{r, echo=FALSE}
library(lattice)
library(ggplot2)
library(caret)
setwd("E:\\coursera\\machine learning in R")
a<-read.csv("pml-training.csv")
n1<-nrow(a)
n2<-ncol(a)
tc<-rep(0,n2)
tr<-rep(0,n1)
for (i in 1:n2){

     tc[i]<-sum(as.numeric(is.na(a[,i])))
     if (tc[i]>500) ka=i
 }
for (i in 1:n1){
     if (is.na(a[i,ka])==F) tr[i]=1
 }


b<-a[,tc<500]
b<-b[tr==0,]
rm(a)
n2<-ncol(b)
tc<-rep(0,n2)
for (i in 1:n2){
     tc[i]<-sum(as.numeric(b[,i]==""))
 }
b<-b[,tc<500]

n2<-ncol(b)

n2<-ncol(b)
b[,60]<-as.numeric(b[,60])

```
All the columns with NA and Blank will be removwed. The available variables will be 
```{r,echo=F}
names(b)
cc<-b[,8:n2]
```
The first 7 variables just for the data indication and will not affect the result prediction 

so these 7 variables will be removed in the prediction algo.


Since different variable will have different range, need to perform pre-processing.
The step will calculate each variable mean and standard devication then all the data will be 

substracted by mean then divided by the standard devication.
```{r}
n1<-nrow(cc)
n2<-ncol(cc)

tmean<-rep(0,n2-1)
tsd<-rep(1,n2-1)
for (i in 1:(n2-1)){
  tmean[i]<-mean((cc[,i]))
  tsd[i]<-sd((cc[,i]))
  cc[,i]<-(cc[,i]-tmean[i])/tsd[i]
}
```
Then the training data will seperated to 3:1, 3 will be as training set and 1 will be as 

testing set.

```{r}
inTrain<-createDataPartition(y=cc$classe,p=.75,list=FALSE)
training<-cc[inTrain,]
testing<-cc[-inTrain,]
set.seed(3243)
modelFit<-train(classe~.,data=training, method='glm')
summary(modelFit)
```


The training set accuracy will be 
```{r}
trainresult<-predict(modelFit,newdata=training)
trainresult<-cbind(trainresult,training$classe,trainresult-training$classe)


1-sum(abs(trainresult[,3])<.5*(trainresult[,1]>1))/nrow(trainresult)
```

The testing set accuracy will be 

```{r}
predictions<-predict(modelFit,newdata=testing)
predresult<-cbind(predictions,testing$classe,predictions-testing$classe)

1-sum(abs(predresult[,3])<.5*(predresult[,1]>1))/nrow(predresult)
```

The in sample error (training set error) is very closed to the out of sample error(testing set 

error). However, the total accuracy is around 65%, it's not so hight. Besides, since use total 

52 variables to perform the predication, there may be high chance to have over fit and if 

change testing set, there may be high out of sample error.

To reduce the over fit high out of sample error, extra work need to be done to reduce the 

variables in the prediction but with the similar result.
