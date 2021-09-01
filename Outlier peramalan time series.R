library(TSA)
library(forecast)
library(tidyverse)
library(tseries)
library('nortest')
library('lmtest')
library('forecast')
win.graph(width=4.875,height=2.5,pointsize=8)
data.frame(rivers)
plot(rivers,ylab='miles',xlab='Bulan&', type='o')
acf(rivers)
pacf(rivers)       
adf.test(rivers)
FitAR::BoxCox.ts(rivers)
FitAR::BoxCox.ts(1/log(rivers))
adf.test(1/log(rivers))
acf(1/log(rivers))
pacf(1/log(rivers))

#ARIMA([7],0,1)
arima(1/log(rivers), order = c(7, 0, 1), fixed=c(rep(0,6),NA,NA,NA), include.mean = T, method = 'ML')

#ARIMA([7],0,0)
arima(1/log(rivers), order = c(7, 0, 0), fixed=c(rep(0,6),NA, NA), include.mean = T, method = 'ML')

#ARIMA(1,0,1)
arima(1/log(rivers),order=c(1,0,1),method='ML',include.mean= T)             

#ARIMA(1,0,0)
arima(1/log(rivers),order=c(1,0,0),method='ML',include.mean= T)
           
#ARIMA(0,0,1)
arima(1/log(rivers),order=c(0,0,1),method='ML',include.mean= T)
             
#ARIMA([1,7],0,1)
arima(1/log(rivers), order = c(7, 0, 1), fixed=c(NA,rep(0,5),NA,NA,NA), include.mean = T, method = 'ML')

#ARIMA([1,7],0,0)
arima(1/log(rivers), order = c(7, 0, 0), fixed=c(NA,rep(0,5),NA,NA), include.mean = T, method = 'ML')



#MODEL#
#ARIMA([7],0,1)
model1=arima(x=(1/log(rivers)),order=c(7,0,1),fixed=c(rep(0,6),NA,NA,NA),
             include.mean=T,method='ML')
model1
#ARIMA([7],0,0)
model2=arima(x=(1/log(rivers)),order=c(7,0,0),fixed=c(rep(0,6),NA,NA),
             include.mean=T,method='ML')

#ARIMA(1,0,1)
model3=arima((1/log(rivers)),order=c(1,0,1),method='ML',include.mean=T)             

#ARIMA(1,0,0)
model4=arima((1/log(rivers)),order=c(1,0,0),method='ML',include.mean=T)

#ARIMA(0,0,1)
model5=arima((1/log(rivers)),order=c(0,0,1),method='ML',include.mean=T)

#ARIMA([1,7],0,1)
model6=arima(x=(1/log(rivers)),order=c(7,0,1),fixed=c(NA,rep(0,5),NA,NA,NA),
             include.mean=T,method='ML')

#ARIMA([1,7],0,0)
model7=arima(x=(1/log(rivers)),order=c(7,0,0),fixed=c(NA,rep(0,5),NA,NA),
             include.mean=T,method ='ML')

#Signifikansi Penduga Parameter
coeftest(model1) #G
coeftest(model2)
coeftest(model3) #G
coeftest(model4)
coeftest(model5)
coeftest(model6) #G
coeftest(model7)

#Pemeriksaan Autokorelasi Sisaan
checkresiduals(model1) 
checkresiduals(model2) #G
checkresiduals(model3) 
checkresiduals(model4) 
checkresiduals(model5)
checkresiduals(model6) 
checkresiduals(model7)

#Pemeriksaan Normalitas Sisaan
lillie.test(model1$residuals) 
lillie.test(model2$residuals) 
lillie.test(model3$residuals) 
lillie.test(model4$residuals)
lillie.test(model5$residuals)
lillie.test(model6$residuals) 
lillie.test(model7$residuals) #Terbaik

#Penentuan Model Terbaik
AIC(model1, model2, model3, model4, model5,model6, model7)

#Model Outlier
riv.model7=arima(rivers,order=c(7,0,0),fixed=c(NA,rep(0,5),NA,NA),
                 method='ML',include.mean=T)
riv.model7

#Deteksi AO
detectAO(riv.model7)

#Deteksi IO
detectIO(riv.model7)

riv.m2=arimax(1/log(rivers),order=c(1,0,1),xreg=data.frame(AO1
      =1*(AO=seq(rivers)==66),AO2=1*(AO=seq(rivers)==68)),io=c(101),
              method='ML')
riv.m2

#Model Outlier
riv.model7o=arimax(1/log(rivers),order=c(7,0,0),fixed=c(NA,rep(0,5),NA,NA),
                   xreg=data.frame(AO1=1*(AO=seq(rivers)==66)),AO2=1*
                     (AO=seq(rivers)==68),AO3=1*(AO=seq(rivers)
                                                 ==70),IO=c(101)
                   ,method='ML')
riv.model7o

#Model Outlier
riv.model7o=arimax(1/log(rivers),order=c(7,0,0),fixed=c(NA,rep(0,5),NA,NA,NA,NA,NA,NA),
                   xreg=data.frame(AO1=1*(AO=seq(rivers)==66),
                   AO2=1*(AO=seq(rivers)==68),
                   AO3=1*(AO=seq(rivers)==70)),io=c(101)
                   ,method='ML', transform.pars = F)
riv.model7o
