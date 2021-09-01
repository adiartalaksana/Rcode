#Latihan 2
M <- c(3,5,4,2,8,9,7,2,4,6)
Rata_rata <- function(x){
  total = 0
  i=1
  repeat{
    total=total+x[i]
    i=i+1
    if (i==length(x)+1){
      break
    }
  }
  return(total/length(x))
}
Rata_rata(M)
