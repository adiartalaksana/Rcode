library(readxl)
library(lmtest)
library(tseries)
library(car)
library(MASS)
DATA <- read_excel("D:/BRAWIJAYA/Kuliah SMT 5/Komputasi Statistika/Laporan Praktikum/Data Laprak 2.xlsx", 
                            col_types = c("numeric", "numeric", "numeric", 
                                          "numeric"))
View(DATA)
scatter.smooth(x=DATA$Matematika, y=DATA$intelegensi,main="Plot Intelegensi~Nilai Matematika")
scatter.smooth(x=DATA$Fisika, y=DATA$intelegensi,main="Plot Intelegensi~Nilai Fisika")
scatter.smooth(x=DATA$Biologi, y=DATA$intelegensi,main="Plot Intelegensi~Nilai Biologi")

par(mfrow=c(1,4))
boxplot(DATA$intelegensi, main="Intelegensi", sub=paste("Outlier rows:"), boxplot.stats(DATA$intelegensi)$out)
boxplot(DATA$Matematika, main="Nilai Matematika", sub=paste("Outlier rows:"), boxplot.stats(DATA$Matematika)$out)
boxplot(DATA$Fisika, main="Nilai Fisika", sub=paste("Outlier rows:"), boxplot.stats(DATA$Fisika)$out)
boxplot(DATA$Biologi, main="Nilai Biologi"), sub=paste("Outlier rows:"), boxplot.stats(DATA$Biologi)$out)

cor(DATA$intelegensi, DATA$Matematika)
cor(DATA$intelegensi, DATA$Fisika)
cor(DATA$intelegensi, DATA$Biologi)

linearMod<-lm(intelegensi~Matematika+Fisika+Biologi, data=DATA)
print(linearMod)

modelSummary<-summary(linearMod)
modelSummary 

AIC(linearMod)
BIC(linearMod)

yduga=fitted(linearMod)
yduga

sisaan=resid(linearMod)
sisaan

#uji normalitas
jarque.bera.test(sisaan)
sresid<-studres(linearMod)
shapiro.test(sresid)

#uji homoskedastisitas
bptest(linearMod)

#uji non-multikolinearitas
vif(linearMod)

#uji non-autokorelasi
durbinWatsonTest(linearMod)
