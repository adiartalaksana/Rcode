ttest=function(x,mu,h1,tk){
  thit=(mean(x)-mu)/(sd(x)/sqrt(length(x)))
  if (h1=="two.sided") {
    ttabel=qt(1-(1-tk)/2,length(x)-1,lower.tail = T)
    if ((thit<(-1)*ttabel) | (thit>ttabel)){ 
      kep=paste("Tolak H0,","Dengan tingkat kepercayaan",tk,"terbukti bahwa rata-rata data sama dengan",mu)
    }else{
      kep=paste("Terima H0,","Dengan tingkat kepercayaan",tk,"tidak terbukti bahwa rata-rata data sama dengan",mu)
      }
  }else if (h1=="less"){
    ttabel=qt(tk,length(x)-1,lower.tail = T)
    if (thit<(-1*ttabel)){
      kep=paste("Tolak H0,","Dengan tingkat kepercayaan",tk,"terbukti bahwa rata-rata data kurang dari",mu)
    }else{
      kep=paste("Terima H0,","Dengan tingkat kepercayaan",tk,"tidak terbukti bahwa rata-rata data kurang dari",mu)
    }
  }else{
      ttabel=qt(tk,length(x)-1,lower.tail = T)
      if (thit>ttabel){
        kep=paste("Tolak H0,","Dengan tingkat kepercayaan",tk,"terbukti bahwa rata-rata data lebih dari",mu)
      }else{
        kep=paste("Terima H0,","Dengan tingkat kepercayaan",tk,"tidak terbukti bahwa rata-rata data lebih dari",mu)
      }
  }
  kep
  }
x=rnorm(75, mean=36500,sd=2000)
tk=0.95
mu=36-00
ttest(x,mu,"less",tk) 
