library(mvtnorm)
library(readxl)
Data <- read_excel("D:/BRAWIJAYA/Kuliah SMT 6/3. Analisis Multivariat I B/Tugas/T2/Book1.xlsx", 
                    col_types = c("numeric", "numeric", "numeric", 
                                  "numeric"))
View(Data)
manova.data = manova(cbind(Data$X1, Data$X2, Data$X3)~Data$Tempat, data=Data)
summary(manova.data, test="Wilks")

library(readxl)
Data.hotelling <- read_excel("D:/BRAWIJAYA/Kuliah SMT 6/3. Analisis Multivariat I B/Tugas/T2/Book2.xlsx", 
                    col_types = c("numeric", "numeric", "numeric"))
View(Data.hotelling)
Y = cbind(Data.hotelling$X1,Data.hotelling$X2)
Y
library(ICSNP)
HotellingsT2(Y[1:11,],Y[12:22,])
