A <- matrix(c(2,-5,4,1,-2.5,1,1,-4,6),byrow=T,nrow=3,ncol=3)
b <- matrix(c(-3,5,10),nrow=3,ncol=1)
p <- nrow(A)
U.pls <- cbind(A,b)
for (i in 1:p){
  for (j in (i+1):(p+1)) U.pls[i,j] <- U.pls[i,j]/U.pls[i,i]
  U.pls[i,i] <- 1
  if (i < p) {
    for (k in (i+1):p) U.pls[k,] <- 
        U.pls[k,] - U.pls[k,i]/U.pls[i,i]*U.pls[i,]
  }
}
U.pls
x <- rep(0,p)
for (i in p:1){
  if (i < p){
    temp <- 0
    for (j in (i+1):p) temp <- temp + U.pls[i,j]*x[j]
    x[i] <- U.pls[i,p+1] - temp
  }
  else x[i] <- U.pls[i,p+1]
}
x

