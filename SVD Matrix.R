### SVD (langsung pakai fungsi) 
A <- matrix(c(3,1,1,-1,3,1),2,3,byrow <- TRUE) 
A
#Langsung memanggil fungsi SVD
A.svd <- svd(A) 
A.svd 
#Matriks diagonal 
Di <- A.svd$d 
Di 
D1 <- Di*diag(length(Di)) 
D1
Cek_A <- A.svd$u%*%D1%*%t(A.svd$v) 
Cek_A 
#matriks A dan Cek_A hasil SVD sama 

