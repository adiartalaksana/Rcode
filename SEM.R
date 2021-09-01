library(readxl)
Data_SEM <- read_excel("D:/BRAWIJAYA/Kuliah SMT 5/Statistika Sosial/Data SEM.xlsx", 
                       col_types = c("text", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric"))
View(Data_SEM)
install.packages("lavaan", dependencies=TRUE)
library(lavaan)
HS.model <- ' quality=~x1+x2+x3
              value=~x4+x5+x6
              best.score=~y1+y2+y3
              CS=~y4+y5+y6
              best.score~quality
              CS~quality+value+best.score'

#fitting SEM Model
fit <- cfa(HS.model, data=Data_SEM)
fit
lavInspect(fit, "cov.lv")
summary(fit, fit.measures=TRUE)
modindices(fit, sort. = TRUE)
inspect(fit, what = "std")

#Rsquare
inspect(fit, 'r2')
fitmeasures(fit)
fitmeasures(fit, c("gfi","agfi","nfi","cfi","rmsea","srmr"))

#representasi visual
install.packages("semPlot")
library(semPlot)
semPaths(fit,"std")
