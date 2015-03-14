CPT1 <- function(numPlus,numMinus,p0,alpha,beta,gamma,delta,numIt,numTrials,dt,stdv,pf=10)
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
  
  Util <- function(pri)
  {
    wp <- function(x)
    {
      cpf <- pnorm(x,mean=(pf-pri)/pri,sd=stdv)
      if(x>=0)
      {
        pow <- 0.61
      }
      else
      {
        pow <- 0.69
      }
      
      (cpf^pow)/((cpf^pow+(1-cpf)^pow)^(1/pow))
    }
    
    valFun <- function(x)
    {
      if(x>=0)
      {
        result <- x^0.88
      }
      else
      {
        result <- -2.25*(-x)^0.88
      }
      result
    }
    
    ret <- rnorm(1000,mean=(pf-pri)/pri,sd=stdv)
    weightedProb <- vapply(ret,wp,numeric(1))
#     normalizedProb <- weightedProb/sum(weightedProb)
    values <- vapply(ret,valFun,numeric(1))
    beta*sum(weightedProb*values)
  }
  
  fnpl <- function(npl,nmi,pri)
  {
    exp(Util(pri))*nmi/totNum - exp(-Util(pri))*npl/totNum
  }
  
  fnmi <- function(npl,nmi,pri)
  {
    exp(-Util(pri))*npl/totNum - exp(Util(pri))*nmi/totNum
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
#       print(p)
      rand <- runif(nm,0,1)
      numChange <- length(rand[rand <= exp(Util(p))*dt])
      np <- np + numChange
      nm <- nm - numChange
      
      rand2 <- runif(np,0,1)
      numChange2 <- length(rand2[rand2 <= exp(-Util(p))*dt])
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





# ret <- rnorm(1000,0,0.4)
# weightedProb <- vapply(ret,wp,numeric(1))
# #normalizedProb <- weightedProb/sum(weightedProb)
# values <- vapply(ret,valFun,numeric(1))
# sum(weightedProb*values)
# plot(ret,values)
# cf <- pnorm(ret,sd=0.4)
# plot(cf,weightedProb)