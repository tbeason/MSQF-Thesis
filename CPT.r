CPTkMC <- function(numPlus,numMinus,p0,alpha,beta,gamma,delta,numIt,numTrials,dt,conf=0.5,pf=10,Tconf=1,Fconf=1)
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
    wp <- function(x,conf)
    {
      1
#       cpf <- conf
#       if(x>=0)
#       {
#         pow <- 0.61
#       }
#       else
#       {
#         pow <- 0.69
#       }
#       
#       (cpf^pow)/((cpf^pow+(1-cpf)^pow)^(1/pow))
    }
    
    valFun <- function(x)
    {
      x
#       if(x>=0)
#       {
#         result <- x^0.88
#       }
#       else
#       {
#         result <- -2.25*(-x)^0.88
#       }
#       result
    }
    
    retF <- (pf-pri)/pri
    wpF <- wp(retF,Fconf)
    valueF <- valFun(retF)
    
    retT <- gamma*delta*((npl-nmi)/totNum)/pri
    wpT <- wp(retT,Tconf)
    valueT <- valFun(retT)
    alpha*wpT*valueT + beta*wpF*valueF #return
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
#       print(p)
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


CPTRK2 <- function(alpha=1,beta=1,gamma=1,delta=1,d=0,rho=0,n0=0.4,p0=1.05,dt=0.01,N=25,Tconf=0.5,Fconf=0.5,pf){
  T <- N/dt+1
  n_plus <- numeric(T)
  price <- numeric(T)
  utility <- numeric(T)
  n_plus[1] <- n0
  price[1] <- p0
  Util <- function(n,pri)
  {
    wp <- function(x,conf)
    {
      cpf <- conf
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
    
    retF <- (pf-pri)/pri
    wpF <- wp(retF,Fconf)
    valueF <- valFun(retF)
    
    retT <- gamma*delta*(2*n-1)/pri
    wpT <- wp(retT,Tconf)
    valueT <- valFun(retT)
    alpha*wpT*valueT + beta*wpF*valueF #return
  }
  
  fn <- function(n,p)
  {
    exp(Util(n,p))*(1-n) - exp(-1*Util(n,p))*n
  }
  
  fp <- function(n)
  {
    gamma*(delta*(2*n-1)+d*rnorm(1))+rho*rnorm(1)
  }
  
  utility[1] <- Util(n0,p0)
  for(i in 2:T)
  {
    np <- n_plus[i-1]+ 0.5*dt*fn(n_plus[i-1],price[i-1])
    pp <- price[i-1] + 0.5*dt*fp(n_plus[i-1])
    n_plus[i] <- n_plus[i-1] + dt*fn(np,pp)
    price[i] <- price[i-1] + dt*fp(np)
    utility[i] <- Util(n_plus[i],price[i])
  }
  t <- seq(0,N,dt)
  as.matrix(cbind(t,n_plus,price,utility))
}


# ret <- rnorm(1000,0,0.4)
# weightedProb <- vapply(ret,wp,numeric(1))
# #normalizedProb <- weightedProb/sum(weightedProb)
# values <- vapply(ret,valFun,numeric(1))
# sum(weightedProb*values)
# plot(ret,values)
# cf <- pnorm(ret,sd=0.4)
# plot(cf,weightedProb)