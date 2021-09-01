a <- matrix(c(1,2,3,4,5,6,7,8,9,10,
              11,12,13,14,15,16,17,18,19,20),
            nrow = 5, ncol = 4,byrow = TRUE)
a

b <- matrix(c(1,2,3,4,5,6,7,8,9,10,
              11,12,13,14,15,16,17,18,19,20),
            nrow = 5, ncol = 4,byrow = FALSE)
b

c <- matrix(c(1,2,3,4,5,6,7,8,9,10,
              11,12,13,14,15,16,17,18,19,20),
            nrow = 4, ncol = 5,byrow = TRUE)
c

d <- matrix(c(1,2,3,4,5,6,7,8,9,10,
              11,12,13,14,15,16,17,18,19,20),5,4,T)
d

e <- matrix(c(1:20),5,4,T)
e

rownames(e) <- c('aaa','bbb','ccc','ddd','rrr')
colnames(e) <- c('lll','kkk','ppp','vvv')
e
