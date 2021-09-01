#Titik P(X<=x) 
qbinom(c(0.25), size=20, prob=0.5, lower.tail=TRUE, log = FALSE)
qbinom(c(0.25), size=20, prob=0.5, lower.tail=FALSE)
qbinom(c(0.75), size=20, prob=0.5, lower.tail=TRUE)
#Peluang di titik tertentu
dbinom(8, size=20, prob=0.5)
#Peluang kumulatif
pbinom(c(8), size=20, prob=0.5, lower.tail=TRUE)
pbinom(c(8), size=20, prob=0.5, lower.tail=FALSE)
pbinom(c(11), size=20, prob=0.5, lower.tail=FALSE)
#Peluang distribusi binomial
Tbel<-data.frame(Pr=dbinom(0:20, size=20, prob=0.5))
rownames(Tbel)<-0:20
Tbel
#Plot distribusi Binomial
x<-3:17
plot(x, dbinom(x, size=20, prob=0.5), xlab="Number of Successes",
     ylab="Probability Mass", main="Binomial Distribution: Trial = 20, Probability of success = 0.5",
     type="l")
points(x, dbinom(x, size=20, prob=0.5), pch=16)
abline(h=0, col="Blue")
#Plot distribusi Binomial kumulatif
x<-rep(x, rep(2, length(x)))
x[-1]
plot(x[-1], pbinom(x, size=20, prob=0.5)[-length(x)],
     xlab="Number of Success", ylab="Cumulative Probability",
     main="Binomial Distribution: Trial = 20, probability of Success = 0.5",
     type="l")
abline(h=0, col="red")

#Membangkitkan data distribusi binomial
BinomialSample <-as.data.frame(matrix(rbinom(15*5, size=20, prob=0.5), ncol=5))
rownames(BinomialSample)<- paste("sample", 1:15,sep="")
colnames(BinomialSample)<- paste("obs", 1:5,sep="")
BinomialSample$mean<-rowMeans(BinomialSample[,1:5])
BinomialSample
showData(BinomialSample,placement='-10+200', font=getRcmdr('logFont'),
          maxwidth=80, maxheight=30)
