#Decision Tree
datadt<-read.table(".... .txt", header=TRUE)
head(datadt)

#clean dataset

n=nrow(datadt)
n
sampel<-sample(1:n,10, replace=FALSE)
Train<-datadt[-sampel,]
Test<-datadt[sampel,]
head(Train)

#clasification tree
library(rpart)
fitCT<-rpart(IP~Usia+Jarak+WaktuJalan+WaktuKampus, data = Train, methode="class")
summary(fitCT)

plot(fitCT, uniform=TRUE, main = "Classification Tree")
text(fitCT, use.n=TRUE, all=TRUE, cex=.8)
names(fitCT)

#make prediction using test dataset
TestingfitCT<-predict(fitCT,newdata=Test,type'class')
TestingfitCT