library(readxl)
GDKI <- read_excel("D:/BRAWIJAYA/Kuliah SMT 5/Komputasi Statistika/jumlahgurudkijakarta.xlsx", col_types = c("text", "numeric","text", "numeric", "numeric"))
View(GDKI)
mode(GDKI$kabkota)
GDKI$negeri
D <- cbind(GDKI$negeri, GDKI$negeri, GDKI$negeri)
D
E <- rbind(GDKI$negeri, GDKI$negeri, GDKI$negeri)
E
plot(GDKI$nokabkota, GDKI$negeri, main="Plot Guru Negeri DKI Jakarta",
     xlab="Kabupaten",ylab="Jumlah",type="p",col="red")
plot(GDKI$nokabkota, GDKI$swasta, main="Plot Guru Swasta DKI Jakarta",
     xlab="Kabupaten",ylab="Jumlah",type="p",col="blue")
hist(GDKI$negeri, main="Histogram Guru Negeri Setiap Kab/Kota",
     xlab="Banyak Guru Negeri",ylab="Jumlah Kabupaten/Kota",col="red")
hist(GDKI$swasta, main="Histogram Guru Swasta Setiap Kab/Kota",
     xlab="Banyak Guru Swasta",ylab="Jumlah Kabupaten/Kota",col="blue")
boxplot(GDKI$negeri, main="Boxplot Jumlah Guru Negeri Setiap Kab/Kota",
        col="red",xlab="Jumlah Guru Negeri")
boxplot(GDKI$swasta, main="Boxplot Jumlah Guru Swasta Setiap Kab/Kota",
        col="Blue",xlab="Jumlah Guru Swasta")
boxplot(GDKI$negeri~GDKI$kabkota, main="Boxplot Jumlah Guru Negeri Setiap Kab/Kota",
        xlab="kabupaten/kota",ylab="Jumlah Guru Negeri")
boxplot(GDKI$swasta~GDKI$kabkota, main="Boxplot Jumlah Guru Swasta Setiap Kab/Kota",
        xlab="kabupaten/kota",ylab="Jumlah Guru Swasta")
jakpusn = 0
jakpuss = 0
jakutn = 0
jakuts = 0
jakbarn = 0
jakbars = 0
jakseln = 0
jaksels = 0
jaktimn = 0
jaktims = 0
kepsern = 0
kepsers = 0
for (i in 1:44) {if(GDKI$nokabkota[i]==1){
                    jakpusn<-jakpusn+GDKI$negeri[i]
                  }else if(GDKI$nokabkota[i]==2){
                    jakutn<-jakutn+GDKI$negeri[i]
                  }else if(GDKI$nokabkota[i]==3){
                    jakbarn<-jakbarn+GDKI$negeri[i]
                  }else if(GDKI$nokabkota[i]==4){
                    jakseln<-jakseln+GDKI$negeri[i]
                  }else if(GDKI$nokabkota[i]==5){
                    jaktimn<-jaktimn+GDKI$negeri[i]
                  }else if(GDKI$nokabkota[i]==6){
                    kepsern<-kepsern+GDKI$negeri[i]
                  }}
for (i in 1:44) {if(GDKI$nokabkota[i]==1){
                    jakpuss<-jakpuss+GDKI$negeri[i]
                  }else if(GDKI$nokabkota[i]==2){
                    jakuts<-jakuts+GDKI$swasta[i]
                  }else if(GDKI$nokabkota[i]==3){
                    jakbars<-jakbars+GDKI$swasta[i]
                  }else if(GDKI$nokabkota[i]==4){
                    jaksels<-jaksels+GDKI$swasta[i]
                  }else if(GDKI$nokabkota[i]==5){
                    jaktims<-jaktims+GDKI$swasta[i]
                  }else if(GDKI$nokabkota[i]==6){
                    kepsers<-kepsers+GDKI$swasta[i]
                  }}
gn = c(jakpusn,jakutn,jakbarn,jakseln,jaktimn,kepsern)
gn
gs = c(jakpuss,jakuts,jakbars,jaksels,jaktims,kepsers)
gs
kabupaten = c("Jakarta Pusat", "Jakarta Utara", "Jakarta Barat", "Jakarta Selatan", "Jakarta Timur", "Kepulauan Seribu")
pie(gn, main="Jumlah Guru Negeri Tiap Kab/Kota",col=rainbow(length(gn)),label=kabupaten)
pie(gs, main="Jumlah Guru Swasta Tiap Kab/Kota",col=rainbow(length(gs)),label=kabupaten)
    