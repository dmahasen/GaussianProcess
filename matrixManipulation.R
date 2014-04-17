require(Matrix)
a = matrix(c(16,4,4,-4,4,10,4,2,4,4,6,-2,-4,2,-2,4),nrow=4)
a
L = chol(a)
t(L)
?solve
solve(a)


calcSigma <- function(X1,X2,l=1) {
  Sigma <- matrix(rep(0, length(X1)*length(X2)), nrow=length(X1))
  for (i in 1:nrow(Sigma)) {
    for (j in 1:ncol(Sigma)) {
      Sigma[i,j] <- exp(-0.5*(abs(X1[i]-X2[j])/l)^2)
    }
  }
  return(Sigma)
}

f <- data.frame(x=c(-4,-3,-1,0,2),
                y=c(-2,0,1,2,-1))

x.star <- seq(-5,5,len=5)


x <- f$x
k.xx <- calcSigma(x,x)
k.xxs <- calcSigma(x,x.star)
k.xsx <- calcSigma(x.star,x)
k.xsxs <- calcSigma(x.star,x.star)                    
                    
f.star.bar <- k.xsx%*%solve(k.xx)%*%f$y
f.star.bar <- k.xsx%*%chol

solve(k.xx) # gives inverse of k.xx
chol(k.xx)
                    