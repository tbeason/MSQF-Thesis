
#### my model with kMC approach
kMC.me <- function(numPlus,numMinus,p0,alpha1,alpha2,beta,gamma,delta,nu,numIt,numTrials,dt,pf,Tf,r,ma=5)
{
  totNum <- numPlus+numMinus
  nplus <- numeric(numIt)
  nminus <- numeric(numIt)
  price <- numeric(numIt)
  nout <- rep(0,numIt)
  pout <- rep(0,numIt)
  nplus[1] <- numPlus
  nminus[1] <- numMinus
  price[1] <- p0
  
  Util <- function(npl,nmi,pri,t)
  {
    if(t*dt <= Tf*dt)
    {
      L <- exp(-r*(Tf-t)*dt)
    }
    else
    {
      L <- 1
    }
    if(t > ma)
    {
      movingAvg <- mean(price[(t-ma):(t-1)])
      pChange <- movingAvg - price[t-1]
    }
    else
    {
      pChange <- 0
    }
    alpha1*((npl-nmi)/totNum)+alpha2*pChange+beta*(1/nu)*L*(pf-pri)
  }
  
  fnpl <- function(npl,nmi,pri,t)
  {
    nu*exp(Util(npl,nmi,pri,t))*nmi/totNum - nu*exp(-Util(npl,nmi,pri,t))*npl/totNum
  }
  
  fnmi <- function(npl,nmi,pri,t)
  {
    nu*exp(-Util(npl,nmi,pri,t))*npl/totNum - nu*exp(Util(npl,nmi,pri,t))*nmi/totNum
  }
  
  fp <- function(npl,nmi)
  {
    gamma*delta*(npl-nmi)/totNum
  }
  
  for(k in 1:numTrials)
  {
    for(i in 2:numIt)
    {
      np <- nplus[i-1]
      nm <- nminus[i-1]
      p <- price[i-1]
      
      rand <- runif(nm,0,1)
      numChange <- length(rand[rand <= nu*exp(Util(np,nm,p,i))*dt])
      np <- np + numChange
      nm <- nm - numChange
      
      rand2 <- runif(np,0,1)
      numChange2 <- length(rand2[rand2 <= nu*exp(-Util(np,nm,p,i))*dt])
      np <- np - numChange2
      nm <- nm + numChange2
      
      
      price[i] <- p + dt*fp(np,nm)+(dt^2)*gamma*delta*0.5*(fnpl(np,nm,p,i)-fnmi(np,nm,p,i))
      nplus[i] <- np
      nminus[i] <- nm
    }
    nout <- nout + nplus
    pout <- pout + price
  }
  
  list(pout/numTrials,nout/numTrials)
}
####


#### Schmidt original model with kMC approach
kMC.orig <- function(numPlus,numMinus,p0,alpha1,beta,gamma,delta,nu,numIt,numTrials,dt,pf)
{
  totNum <- numPlus+numMinus
  nplus <- numeric(numIt)
  nminus <- numeric(numIt)
  price <- numeric(numIt)
  nout <- rep(0,numIt)
  pout <- rep(0,numIt)
  nplus[1] <- numPlus
  nminus[1] <- numMinus
  price[1] <- p0
  
  Util <- function(npl,nmi,pri,t)
  {
    alpha1*gamma*delta*((npl-nmi)/totNum)/pri+beta*(pf-pri)
  }
  
  fnpl <- function(npl,nmi,pri,t)
  {
    nu*exp(Util(npl,nmi,pri,t))*nmi/totNum - nu*exp(-Util(npl,nmi,pri,t))*npl/totNum
  }
  
  fnmi <- function(npl,nmi,pri,t)
  {
    nu*exp(-Util(npl,nmi,pri,t))*npl/totNum - nu*exp(Util(npl,nmi,pri,t))*nmi/totNum
  }
  
  fp <- function(npl,nmi)
  {
    gamma*delta*(npl-nmi)/totNum
  }
  
  for(k in 1:numTrials)
  {
    for(i in 2:numIt)
    {
      np <- nplus[i-1]
      nm <- nminus[i-1]
      p <- price[i-1]
      
      rand <- runif(nm,0,1)
      numChange <- length(rand[rand <= nu*exp(Util(np,nm,p,i))*dt])
      np <- np + numChange
      nm <- nm - numChange
      
      rand2 <- runif(np,0,1)
      numChange2 <- length(rand2[rand2 <= nu*exp(-Util(np,nm,p,i))*dt])
      np <- np - numChange2
      nm <- nm + numChange2
      
      
      price[i] <- p + dt*fp(np,nm)+(dt^2)*gamma*delta*0.5*(fnpl(np,nm,p,i)-fnmi(np,nm,p,i))
      nplus[i] <- np
      nminus[i] <- nm
    }
    nout <- nout + nplus
    pout <- pout + price
  }
  
  list(pout/numTrials,nout/numTrials)
}
