kMC <- function(numPlus,numMinus,p0,alpha,beta,gamma,vega,numIt,numTrials,a)
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
  for(k in 1:numTrials)
  {
    for(i in 2:numIt)
    {
      np <- nplus[i-1]
      nm <- nminus[i-1]
      p <- price[i-1]
      x <- 0.5*(np-nm)/totNum
      util <- beta*(100-p)
      #prob <- nm*exp(a*x)/(nm*exp(a*x)+np*exp(-a*x))
      prob <- nm*exp(util)/(nm*exp(util)+np*exp(-util))
      rand <- runif(1)
      if(rand <= prob)
      {
        nplus[i] <- min(np+1,totNum)
        nminus[i] <- max(nm-1,0)
      }
      else if(rand > prob)
      {
        nminus[i] <- min(nm+1,totNum)
        nplus[i] <- max(np-1,0)
      }
      price[i] <- max(0,p+gamma*(nplus[i]-nminus[i])/totNum)
    }
    nout <- nout + nplus
    pout <- pout + price
  }
  
  list(pout/numTrials,nout/numTrials)
}