library(readxl)
library(forecast)
library(nortest)
library(tseries)
library(ggplot2)
library(lmtest)
library(car)
library(MASS)
data<- read_excel("C:/Users/Adi Laksana/Downloads/SOAL_UAR.xlsx", 
                           sheet = "Sheet1", col_types = c("numeric", 
                                                           "numeric"))
Peramalan=data.frame(data)
Peramalan
Z=Peramalan$Data_4
#Plot data
data$Bulan <- as.Date(data$Bulan, format = "%Y-%m-%d")
data%>%
  ggplot(aes(x=Bulan, y=Data_4)) +
  geom_line(col="blue") +
  labs(title = "Peramalan Soal UAR")
#cek stasioneritas ragam - transformasi boxcox
BoxCox(Peramalan$Data_4,1)
qqnorm(Z)
qqline(Z,col="blue")
#cek stasioneritas rataan - plot acf pacf
plot(Z, type = "l", col="blue")
acf(Z)
pacf(Z)
#Uji Dickey Fuller
adf.test(Z)

#yang ini kebawah blm coba hehe
#MODEL TENTATIF
m1=Arima(Z,order=c(2,1,2),fixed=c(0,NA,0,NA))
m1
m2=Arima(Z,order=c(2,1,0),fixed=c(0,NA))
m2
m3=Arima(Z,order=c(0,1,2),fixed=c(0,NA))
m3
lmtest::coeftest(m1)
lmtest::coeftest(m2)
lmtest::coeftest(m3)
#Cek Autokorelasi Sisaan
checkresiduals(m2)
checkresiduals(m3)
Box.test(m2$residuals,type = "Ljung", lag=7)
Box.test(m2$residuals,type = "Ljung", lag=12)
Box.test(m2$residuals,type = "Ljung", lag=17)
Box.test(m2$residuals,type = "Ljung", lag=22)
Box.test(m3$residuals,type = "Ljung", lag=7)
Box.test(m3$residuals,type = "Ljung", lag=12)
Box.test(m3$residuals,type = "Ljung", lag=17)
Box.test(m3$residuals,type = "Ljung", lag=22)
#penentuan model terbaik
AIC(m2)
AIC(m3)
#Cek Normalitas Sisaan
lillie.test(m2$residuals)
lillie.test(m3$residuals)
summary(m1)
summary(m2)
summary(m3)
mo = auto.arima(Z, d=1, max.p = 3, max.q = 3, lambda =
                  "auto")
mo
par(mfrow = c(1,1))
forecast(mo)
plot(forecast(mo))


y=c(0.55,0.15,1.36,1.10,0.66,0.65,0.38,1.73,1.39,0.16,0.41,1.91)
y
library(TSA)
library(TSstudio)
ts.plot(y, Title= "Data Time Series" , Xtitle="Waktu", Ytitle="Data",Col="Blue",width = 3)
y_t1=lag(y,1)
y_t1
data=ts(y)
install.packages("ggfortify")
library(ggfortify)
library(forcats)
library(tseries)
autoplot(data, main = "Data Time Series", ylab = "Data",xlab = "Waktu", colour = "blue")
y_t1=lag(data,-1)
y_t2=lag(data,-2)
y_t3=lag(data,-3)
X=cbind(y_t1,y_t2,y_t3)
X
data
model=lm(y~y_t1+y_t2+y_t3)
summary(model)

model=arima(data,order=c(2,0,1)
model

IPK=c(3.74,4.00,3.52,3.33,3.13,2.95,3.73,2.86,3.51,2.99)
TB=c(164,163,164,165,174,162,156,161,158,156)
JK=c(1,1,1,1,1,0,0,0,0,0)
linearMod<-lm(IPK~JK+TB)
summary(linearMod)
model=linearMod$coefficients
model
m1=mean(TB)
sd1=sd(TB)
X1=rnorm(10,m1,sd1)
X1
X2=abs(trunc(rnorm(100,0,1)))
X2
X2=c(rep(0,5),rep(1,5))
X2
Y=predict(X1,X2)
databaru=data.frame(X1,X2)
Ybangkit=predict(linearMod,databaru)
Ybangkit
view(Ybangkit)
acf(model)
