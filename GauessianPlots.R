library(gptk)
library(ggplot2)
library(reshape)
gptk::demInterpolation()



l=9; x = seq(-1, 1, length=l)
trueKern = kernCreate(x, 'rbf')
K = kernCompute(trueKern, x)

yTrue = gaussSamp(Sigma=K, numSamps=10)

xy <- data.frame(x,t(yTrue))
#colnames(xy)<-c('x','y1','y2','y3')
  
xy_long <- melt(xy, id="x")  # convert to long format

ggplot(data=xy_long, aes(x=x, y=value, colour=variable)) +
  geom_line()


steps = 2^c(round(log2(l-1)):0); s=0
indTrain=list(); length(indTrain)=length(steps)
indTrain = lapply(indTrain, function(x) x=(seq(1,l,by=steps[(s<<-s+1)]))) ## <<-transcends local scope

i = 1
yTrain = as.matrix(yTrue[indTrain[[i]]])
xTrain = as.matrix(x[indTrain[[i]]])
kern = kernCreate(x, 'rbf')
kern$inverseWidth = 5 ## Change inverse variance (1/(lengthScale^2)))

xTest = as.matrix(seq(-2, 2, length=200))

Kx = kernCompute(kern, xTest, xTrain)
Ktrain = kernCompute(kern, xTrain, xTrain)

invKtrain = .jitCholInv(Ktrain, silent=TRUE)$invM
yPred = Kx%*%invKtrain%*%yTrain
yVar = kernDiagCompute(kern, xTest) - rowSums(Kx%*%invKtrain * Kx)

model = gpCreate(dim(xTrain)[2], dim(yTrain)[2], xTrain, yTrain, options)

dev.new(); plot.new()
gpPlot(model, xTest, yPred, yVar, ylim=c(-3,3), col='black')
pathfilename = paste(path,'/',filename,'_', figNo, '.eps', sep='')
if (png) {
  dev.copy2eps(file = pathfilename) ## Save plot as eps
  ## Convert to png. Needs the 'eps2png' facility.
  ## If not already installed: 'sudo apt-get install eps2png'
  system(paste('eps2png ', pathfilename, sep=''))
  


ggplot(aes(x),data=xy)+
  geom_line(aes(y=y1,colour='y1'))+
  geom_line(aes(y=y2,colour='y2'))

qplot(x,y1,data=xy,geom=c('point','smooth'))
qplot(x,y2,data=xy,geom=c('smooth','point'))

