ttest=function(x,mu,h1,tk){
  thit=(mean(x)-mu)/(sd(x)/sqrt(length(x)))
  pvalue=pt(thit,(length(x)-1),lower.tail = T)
  df=length(x)-1
  if (h1=="two.sided") {
    ttabel=qt(1-(1-tk)/2,length(x)-1,lower.tail = T)
    if ((thit<(-1)*ttabel) | (thit>ttabel)){ 
      kep=paste("data : x",",t =",thit,",df =",df,",p-value =",pvalue,
                ",keputusan = Tolak H0,","Dengan tingkat kepercayaan",
                tk,"tidak terbukti bahwa rata-rata data sama dengan",mu)
    }else{
      kep=paste("data : x",",t =",thit,",df =",df,",p-value =",pvalue,
                ",keputusan = Terima H0,","Dengan tingkat kepercayaan",
                tk,"terbukti bahwa rata-rata data sama dengan",mu)
    }
  }else if (h1=="less"){
    ttabel=qt(tk,length(x)-1,lower.tail = T)
    if (thit<(-1*ttabel)){
      kep=paste("data : x",",t =",thit,",df =",df,",p-value =",pvalue,
                ",keputusan = Tolak H0,","Dengan tingkat kepercayaan",
                tk,"terbukti bahwa rata-rata data kurang dari",mu)
    }else{
      kep=paste("data : x",",t =",thit,",df =",df,",p-value =",pvalue,
                ",keputusan = Terima H0,","Dengan tingkat kepercayaan",
                tk,"tidak terbukti bahwa rata-rata data kurang dari",mu)
    }
  }else{
    ttabel=qt(tk,length(x)-1,lower.tail = T)
    if (thit>ttabel){
      kep=paste("data : x",",t =",thit,",df =",df,",p-value =",pvalue,
                ",keputusan = Tolak H0,","Dengan tingkat kepercayaan",tk,
                "terbukti bahwa rata-rata data lebih dari",mu)
    }else{
      kep=paste("data : x",",t =",thit,",df =",df,",p-value =",pvalue,
                ",keputusan = Terima H0,","Dengan tingkat kepercayaan",
                tk,"tidak terbukti bahwa rata-rata data lebih dari",mu)
    }
  }
  kep
}
x=(c(26,28,28,29,30,22,24,26,25,28,
     29,29,30,28,28,26,26,24,30,28,
     29,25,31,32,31,26,28,23,29,32,
     26,29,32,25,31,22,26,25,25,26))
tk=0.95
mu=25
h1="two.sided"
ttest(x,mu,h1,tk) 