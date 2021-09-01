library(readxl)
library(MVN)
library(tidyverse)
library(car)
library(biotools)

##MANOVA## 
#Data
library(readxl)
Data_Kuis <- read_excel("D:/BRAWIJAYA/Kuliah SMT 6/3. Analisis Multivariat I B/Praktikum/Data Kuis.xlsx")
View(Data_Kuis)
str(Data_Kuis)

#Asumsi Antar Variabel Saling Bebaas
cor(Data_Kuis)

#Asumsi Normalitas
mvn(Data_Kuis)

#Homogenitas Matriks Ragam Peragam
boxM(data = Data_Kuis[,c(2,3,4)],grouping = Data_Kuis$Pekerjaan)

#Asumsi Ragam Residual Homogen
bartlett.test(Data_Kuis)

#Membentuk model
model<-manova(cbind(`Kimia`,`Matematika`,`Bahasa Indonesia`)~
                as.factor(Pekerjaan),data= Data_Kuis)
model
summary.aov(model)

# Menentukan kriteria test yang digunakan
summary(model, test = "Pillai")
summary(model, test = "Wilks")
summary(model, test = "Hotelling-Lawley")
summary(model, test = "Roy")
