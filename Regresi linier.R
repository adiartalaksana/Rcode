library(readxl)
Data <- read_excel("D:/BRAWIJAYA/Kuliah SMT 5/Komputasi Statistika/tugas kelompok/Data Regresi.xlsx", 
                           col_types = c("numeric", "numeric"))
View(Data)

#cek scatter plot data
scatter.smooth(x=Data$`Pelayanan Konsumen`, y=Data$`Minat Beli`,main="Plot Minat Beli~Pelayanan Konsumen")

# dari plot dikettahui bahwa variabel prediktor dan respons berhubungan linier

#buat boxplot untuk cek pencilan
par(mfrow=c(1,2))
boxplot(Data$`Pelayanan Konsumen`, main="Pelauanan Konsumen", sub=paste("Outlier rows:"), boxplot.stats(Data$`Pelayanan Konsumen`)$out)
boxplot(Data$`Minat Beli`, main="Minat Beli", sub=paste("Outlier rows:"), boxplot.stats(Data$`Minat Beli`)$out)

#cek nilai korelasi
cor(Data$`Pelayanan Konsumen`,Data$`Minat Beli`)

#pembuatan model linier
linearMod<-lm(`Minat Beli`~`Pelayanan Konsumen`, data=Data)
print(linearMod)

#Linear Regression Diagnostics
modelSummary<-summary(linearMod)
modelSummary #untuk pemanggilan seluruh summary

modelCoeffs<-modelSummary$coefficients #koefisien modelnya saja (penduga model dan uji t)
modelCoeffs

#nilai AIC
AIC(linearMod)
#nilai BIC
BIC(linearMod)

#Mendapatkan prediksi dan sisaan
yduga=fitted(linearMod)
yduga

sisaan=resid(linearMod)
sisaan

#analisis kebaikan model dan uji asumsi
install.packages("lmtest") #bisa lewati kalo udah install
install.packages("lmtest") #bisa lewati kalo udah install

library(lmtest)
library(tseries)

#uji normalitas
jarque.bera.test(sisaan)

#uji homoskedastisitas
bptest(linearMod)
rowna
dpois(5,10)
qchisq(0.01,20,lower.tail=F)
square_and_add<-function(x,y = 1){
  x^2 + y
}
square_and_add(sqrt(6))
dnorm()
dnorm(x, mean = 0, sd = 1, log = FALSE) 
pnorm(2, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) 
qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) 
rnorm(n, mean = 0, sd = 1)
row.names()