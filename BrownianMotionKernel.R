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


data <- read.csv("data/gdp.csv")

obs.ts <- data.frame(x = data$YEAR[1:5], y = data$CANADA[1:5])

X <- seq(min(obs.ts$x),max(obs.ts$x),by=1)

#BRCFun <- function(outer
                   
#outer(X,X,add)           

K.SS <- covFun(X,X,brownianMotionK)


values <- mvrnorm(30, rep(0, length=length(X)), K.SS)

dat <- data.frame(x=X, t(values))
dat <- melt(dat, id="x")
head(dat)

figBM2a <- ggplot(dat,aes(x=x,y=value)) +
  geom_rect(xmin=-Inf, xmax=Inf, ymin=-2, ymax=2, fill="grey80") +
  geom_line(aes(group=variable)) +   theme_bw() +
  scale_y_continuous(lim=c(-100,100), name="output, f(x)") +
  xlab("input, x")
figBM2a

                     #(1,2,3,5,6),
                  #y = c(-2,  0,  1,  2, -1))

K <- covFun(obs.ts$x,obs.ts$x,brownianMotionK)
K.S <- covFun(obs.ts$
                x,X,brownianMotionK)


cov_xx_inv <- solve(K)
Ef <- t(K.S) %*% cov_xx_inv %*% obs.ts$y
Cf <- K.SS - t(K.S) %*% cov_xx_inv %*% K.S



values <- mvrnorm(10, Ef, Cf)


dat <- data.frame(x=X, t(values))
dat <- melt(dat, id="x")

figBM2b <- ggplot(dat,aes(x=x,y=value)) +
  geom_ribbon(data=NULL, 
              aes(x=X, y=Ef, ymin=(Ef-2*sqrt(diag(Cf))), ymax=(Ef+2*sqrt(diag(Cf)))),
              fill="grey80") +
  geom_line(aes(color=variable)) + #REPLICATES
  geom_line(data=NULL,aes(x=X,y=Ef), size=1) + #MEAN
  geom_point(data=obs.ts,aes(x=x,y=y)) +  #OBSERVED DATA
  scale_y_continuous(lim=c(3,4), name="output, f(x)") +
  xlab("input, x") + 
  theme_bw()
figBM2b

plot(obs.ts)
