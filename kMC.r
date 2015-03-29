kMC <- function(numPlus,numMinus,p0,alpha1,alpha2,beta,gamma,delta,vega,numIt,numTrials,dt,pf,Tf,r)
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
    if(t > 2)
    {
      pChange <- price[t-1] - price[t-2]
    }
    else
    {
      pChange <- 0
    }
    alpha1*((npl-nmi)/totNum)+alpha2*pChange+beta*(1/vega)*L*(pf-pri)
  }
  
  fnpl <- function(npl,nmi,pri,t)
  {
    vega*exp(Util(npl,nmi,pri,t))*nmi/totNum - vega*exp(-Util(npl,nmi,pri,t))*npl/totNum
  }
  
  fnmi <- function(npl,nmi,pri,t)
  {
    vega*exp(-Util(npl,nmi,pri,t))*npl/totNum - vega*exp(Util(npl,nmi,pri,t))*nmi/totNum
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
      numChange <- length(rand[rand <= vega*exp(Util(np,nm,p,i))*dt])
      np <- np + numChange
      nm <- nm - numChange
      
      rand2 <- runif(np,0,1)
      numChange2 <- length(rand2[rand2 <= vega*exp(-Util(np,nm,p,i))*dt])
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
