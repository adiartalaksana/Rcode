library(readxl)
library(lavaan)
library(semPlot)
Data_SEM <- read_excel("D:/BRAWIJAYA/Kuliah SMT 5/Statistika Sosial/Data SEM.xlsx", 
                       col_types = c("text", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric"))
View(Data_SEM)

#SEM Model
HS.model <- ' quality=~x1+x2+x3
              value=~x4+x5+x6
              best.score=~y1+y2+y3
              CS=~y4+y5+y6
              best.score~quality
              CS~quality+value+best.score
              quality~~value'

#fitting SEM Model
fit <- cfa(HS.model, data=Data_SEM)
fit
summary(fit, fit.measures=TRUE)
modindices(fit, sort. = TRUE)
inspect(fit, what = "std")
estfun.lavaan(fit, scaling = FALSE, ignore.constraints = FALSE,
              remove.duplicated = TRUE, remove.empty.cases = TRUE)
inspect(fit, what="rsquare" )
#pemeriksaan godness of fit model
inspect(fit, 'r2')
fitmeasures(fit)
fitmeasures(fit, c("gfi","tli","agfi","cfi","rmsea"))

#representasi visual dari model
semPaths(fit,"std")
