

logBinContInter <- function(N,b0,b1,b2,b3){
  x1 = rnorm(N)           # some continuous variables 
  x2 = rbinom(N,1,.5)
  
  b0 <- log(b0)
  b1 <- log(b1)
  b2 <- log(b2)
  b3 <- log(b3)
  
  z = b0 + b1*x1 + b2*x2 + b3*x1*x2   # linear combination with a bias
  pr = 1/(1+exp(-z))         # pass through an inv-logit function
  y = rbinom(N,1,pr)      # bernoulli response variable
  
  #now feed it to glm:
  df = data.frame(y=y,x1=x1,x2=as.factor(x2))
  
  return(df)
}

set.seed(500)

test<-logBinContInter(300,1,1.5,1.1, 1.8)
m<-glm( y~x1*x2,data=test,family="binomial")
exp(m$coefficients)

iter <- 1000
samples <- seq(from=150,to=500,by=25)
powerMat <- matrix(nrow=length(samples),ncol=2)
for(t in 1:length(samples)){
  i <- samples[t]
  simMat <- matrix(nrow=iter,ncol=1)
  for(s in 1:iter){
    curDf<-logBinContInter(i,1,1.5,1.1, 1.5)
    m<-glm( y~x1*x2,data=curDf,family="binomial")
    simMat[s,] <- summary(m)$coefficients[16]
  }
  powerMat[t,] <- c(i,mean(simMat[,1]<.05) )
}
