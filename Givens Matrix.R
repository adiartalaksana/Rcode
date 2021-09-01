#transformasi given
rep(1,4)
1:4
diag(4)
x=matrix(c(rep(1,4),2,4,6,5),4,2)
x
#fungsi untuk operasi givens
Givens=function(x,i,j) {
  a=x[i,i]; b=x[j,i]
  r=sqrt(a^2+b^2)
  uij=matrix(c(a/r,-b/r,b/r,a/r),2,2) #membuat matriks uij
  print(uij)   #menampilkan matrix uij
  x[c(i,j),]=uij%*%x[c(i,j),]
  u=diag(4)    #matriks diagonal berordo 4
  u[c(i,j),c(i,j)]=uij
  print(u)
  Givens=x 
}
x=Givens(x,1,2)
x
x=Givens(x,1,3)
x
x=Givens(x,1,4)
x
x=Givens(x,2,3)
x
x=Givens(x,2,4)
x
ux=t(x)%*%x
ux
invers=solve(ux)
invers
y=c(1,5,7,3)
y
uxy=t(x)%*%y
uxy
beta=invers%*%uxy
beta
