kMC <- function(numPlus,numMinus,p0,alpha,beta,gamma,delta,numIt,numTrials,dt)
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
  
  Util <- function(npl,nmi,pri)
  {
    alpha*gamma*delta*((npl-nmi)/totNum)/pri+beta*(1-pri)
  }
  
  fnpl <- function(npl,nmi,pri)
  {
    exp(Util(npl,nmi,pri))*nmi/totNum - exp(-Util(npl,nmi,pri))*npl/totNum
  }
  
  fnmi <- function(npl,nmi,pri)
  {
    exp(-Util(npl,nmi,pri))*npl/totNum - exp(Util(npl,nmi,pri))*nmi/totNum
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
      numChange <- length(rand[rand <= exp(Util(np,nm,p))*dt])
      np <- np + numChange
      nm <- nm - numChange
      
      rand2 <- runif(np,0,1)
      numChange2 <- length(rand2[rand2 <= exp(-Util(np,nm,p))*dt])
      np <- np - numChange2
      nm <- nm + numChange2
      
      
      price[i] <- p + dt*fp(np,nm)+(dt^2)*gamma*delta*0.5*(fnpl(np,nm,p)-fnmi(np,nm,p))
      nplus[i] <- np
      nminus[i] <- nm
    }
    nout <- nout + nplus
    pout <- pout + price
  }
  
  list(pout/numTrials,nout/numTrials)
}
