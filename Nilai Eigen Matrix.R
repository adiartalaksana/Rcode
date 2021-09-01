### Masalah Eigen (matriks harus setangkup) 
C <- matrix(c(11,1,1,11),2,2)
C 
E_C <- eigen(C,symmetric <- TRUE) 
E_C 
Nilai_Eigen_1 <- E_C$values[1]
Nilai_Eigen_1 
Nilai_Eigen_2 <- E_C$values[2]
Nilai_Eigen_2 
Vektor_Eigen_1 <- E_C$vectors[,1] 
Vektor_Eigen_1
Vektor_Eigen_2 <- E_C$vectors[,2]
Vektor_Eigen_2
#Untuk mengecek kebenaran: Cx=(nilai eigen)x
Tes1_C <- C%*%Vektor_Eigen_1
Tes1_C
Tes1_Eigen <- Nilai_Eigen_1*Vektor_Eigen_1 
Tes1_Eigen 
#hasil keduanya sama, begitu juga dg nilai eigen kedua 
Tes2_C <- C%*%Vektor_Eigen_2
Tes2_C
Tes2_Eigen <- Nilai_Eigen_2*Vektor_Eigen_2
Tes2_Eigen 
#disimpulkan Vektor Eigen bersesuaian dg Nilai Eigen

