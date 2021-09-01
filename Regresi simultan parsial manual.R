library(readxl)
DATA <- read_excel("D:/BRAWIJAYA/Kuliah SMT 5/Komputasi Statistika/Laporan Praktikum/DATA1.xlsx", 
                   col_types = c("numeric", "numeric", "numeric", 
                                 "numeric", "numeric"))
View(DATA)
C=rep(1,length(DATA$age))
X<-cbind(C,DATA$age,DATA$bmi,DATA$children,DATA$smoker)
Y<-cbind(DATA$charges)
#Metode OLS Mencari Koefisien Regresi
Xt=t(X)
XtX=Xt%*%X
XtX
XtXinv=solve(XtX)
XtY=Xt%*%Y
b=XtXinv%*%XtY
b
Z=X%*%XtXinv%*%Xt
Z
#uji kesesuaian model (ANOVA)
dbr=length(DATA[1,])
dbt=length(DATA$age)
dbg=dbt-dbr
JKT=t(Y)%*%Y
JKR=t(Y)%*%X%*%solve(t(X)%*%X)%*%(t(X)%*%Y)
JKG=JKT-JKR
KTR=JKR/dbr
KTG=JKG/dbg
Fhit=KTR/KTG
Ftab= qf(0.95,dbr-1,dbg+1)
#Membentuk Tabel Anova
SumberKeragaman<-c("Regresi","Galat","Total")
JK<-c(JKR,JKG,JKT)
DB<-c(dbr,dbg,dbt)
KT<-c(KTR,KTG,"-")
Fhitung<-c(Fhit,"-","-")
Ftabel<-c(Ftab,"-","-")
if (Fhitung[1]>Ftabel[1]){
  kep<-"Terima H0"
  }else{
    kep<-"Tolak H0"
  }
Keputusan<-c(kep,"-","-")
ANOVA<-data.frame(SumberKeragaman,JK,DB,KT,Fhitung,Ftabel,Keputusan)
ANOVA
#signifikansi parameter parsial
c<-c()
SEb<-c()
thit<-c()
ttab<-qt(0.975,dbg)
ttabel<-c()
beta<-c("b0","b1","b2","b3","b4","b5","b6","b7","b8","b9","b10")
Parameter<-c()
Koefisien<-c()
Keputusan<-c()
dmns<-dim(XtXinv)
for(i in 1:dmns[1]){
  for(j in 1:dmns[2]){
    if(i==j){
      c[i]=XtXinv[i,j]
      SEb[i]=sqrt(KTG)*sqrt(c[i])
      thit[i]=b[i]/SEb[i]
      ttabel[i]<-ttab
      Parameter[i]<-beta[i]
      Koefisien[i]<-b[i]
      if (thit[i]<ttabel[i]){
        Keputusan[i]<-"Terima H0"
      }else{
        Keputusan[i]<-"Tolak H0"
      }
      }
  }
}
#membentuk tabel uji signifikansi parameter parsial
Parsial<-data.frame(Parameter,Koefisien,SEb,thit,ttabel,Keputusan)
Parsial

#pendugaan sigma kuadrat dengan matriks
#mehitung matriks H
H=X%*%solve(t(X)%*%X)%*%t(X)
head(H,1)
I=matrix(0, nrow(H), ncol(H))
diag(I)<-rep(1, nrow(H))
sigma_2=t(Y)%*%(I-H)%*%(Y/(n-2))
sigma_2
