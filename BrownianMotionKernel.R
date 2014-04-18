#Brownian Motion
require(MASS)
require(reshape2)
require(ggplot2)

brownianMotionK <- function(X1,X2,variance=1)
{
  return(variance*min(X1,X2))
}

add <- function(X1,X2)
{
  return(min(X1,X2))
}


covFun <- function(X1,X2,kern)
{
  l1 <- length(X1)
  l2 <- length(X2)
  K <- matrix(nrow=l1,ncol=l2)
  for(i in 1:l1)
  {
    for(j in 1:l2)
    {
      K[i,j] <- kern(X1[i],X2[j])
    }
  }
  return(K)
  
}
X <- seq(0,1,by=0.1)

#BRCFun <- function(outer
                   
outer(X,X,add)           

K <- covFun(X,X,brownianMotionK)


values <- mvrnorm(30, rep(0, length=length(X)), K)

dat <- data.frame(x=X, t(values))
dat <- melt(dat, id="x")
head(dat)

figBM2a <- ggplot(dat,aes(x=x,y=value)) +
  geom_rect(xmin=-Inf, xmax=Inf, ymin=-2, ymax=2, fill="grey80") +
  geom_line(aes(group=variable)) +   theme_bw() +
  scale_y_continuous(lim=c(-3,3), name="output, f(x)") +
  xlab("input, x")
figBM2a

obs <- data.frame(x = c(-4, -3,-1,0,2),
                  y = c(-2,  0,  1,  2, -1))