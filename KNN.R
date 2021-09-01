#KNN
library(radxl)
library(class)
data<-read_excel()
data
#melihat struktur dataset
str(data)
#pemilihan variabel yang akan digunakan(data cleaning)
data.subset<-loan[c('variabel1','variabel2','variabel3')]
str(data.subset)

#normalisasi data agar output tidak bias
normalisasi<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
normal.data.subset<-as.data.frame(lapply(data.subset[,2:5],normalisasi))

head(normal.data.subset)

set.seed(123)

data.d<-sample(1:nrow(normal.data.subset),size=nrow(normal.data.subset)*0.7, replace=FALSE)

train.data<-data.subset[data.d,] #70% data training
test.data<-data.subset[-data.d,] #30% data testing

#membuat dataframe yang terpisah berdasar fitur 'decision' yang merupakan variabel target
train.data_labels<-data.subset[data.d,1]
test.data_labels<-data.subset[-data.d,1]

#menentukkan banyaknya observasi
NROW(train.data_labels)

#bergantung pada akar sampel
knn.26<-knn(train=train.data,test=test.data,cl=train.data_labels, k=26)
knn.27<-knn(train=train.data,test=test.data,cl=train.data_labels, k=27)

#menghitung proporsi klasifikasi yang tepat
ACC.26<-100*sum(test.data_labels==knn.26)/NROW(test.data_labels)
ACC.27<-100*sum(test.data_labels==knn.27)/NROW(test.data_labels)
