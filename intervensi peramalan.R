install.packages('arules')
library(TSA)
library(arules)
win.graph(width=4.875,height=2.5,pointsize=8)
data(airmiles)
airmiles
plot(log(airmiles),ylab='log(airmiles)',xlab='Tahun', type='o')

acf(diff(diff(window(log(airmiles),end=c(2001,8)),12)),lag.max=48,main='')
pacf(diff(diff(window(log(airmiles),end=c(2001,8)),12)),lag.max=48,main='')

air.m1=arimax(log(airmiles), order=c(0,1,1), seasonal=list(order=c(0,1,1),period=12), 
              xtransf=data.frame(I911=1*(seq(airmiles)==69),I911=1*(seq(airmiles)==69)), 
              transfer=list(c(0,0),c(1,0)), method='ML')
air.m1
arimax()
xtransf=data.frame(I911=1*(seq(airmiles)==69),I911=1*(seq(airmiles)==69))
xtransf
transfer=list(c(0,0),c(1,0))
transfer

tsdiag(air.m1,gof=15,omit.initial=F)

plot(log(airmiles),ylab='Log(airmiles)')
points(fitted(air.m1))

Nine11p=1*(seq(airmiles)==69)

plot(ts(Nine11p*(-0.1290)+
          filter(Nine11p,filter=.8901,method='recursive', side=1)*
          (-0.2419),frequency=12,start=1996),ylab='9/11 Effects',
     type='h'); abline(h=0)
