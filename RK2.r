##### normal functionality as found in the Schmidt papers
RK2.me <- function(alpha=1,kappa=1,beta=1,gamma=1,nu=1,rho1=0,rho2=0,n0=0.4,p0=1.05,dt=0.01,numPeriods=25,pf=1,Tf,r,ma=5){
  Tnum <- numPeriods/dt+1
  n_plus <- numeric(Tnum)
  price <- numeric(Tnum)
  util <- numeric(Tnum)
  n_plus[1] <- n0
  price[1] <- p0
  rands <- t(replicate(Tnum,snorm()))
  U <- function(n,p,t,e1)
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
    
    alpha*((2*n-1)+rho1*e1) + kappa*(pChange/p) + beta*(1/nu)*L*((pf-p)/p)
  }
  
  fn <- function(n,p,t,e1)
  {
    nu*exp(U(n,p,t,e1))*(1-n) - nu*exp(-1*U(n,p,t,e1))*n
  }
  
  fp <- function(n,e2)
  {
    gamma*((2*n-1)+rho2*e2)
  }
  
  util[1] <- U(n0,p0,1,rands[1,1])
  for(i in 2:Tnum)
  {
    np <- n_plus[i-1]+ 0.5*dt*fn(n_plus[i-1],price[i-1],i-1,rands[i,1])
    pp <- price[i-1] + 0.5*dt*fp(n_plus[i-1],rands[i,2])
    n_plus[i] <- n_plus[i-1] + dt*fn(np,pp,i,rands[i,1])
    price[i] <- price[i-1] + dt*fp(np,rands[i,2])
    util[i] <- U(n_plus[i],price[i],i,rands[i,1])
  }
  t <- seq(0,numPeriods,dt)
  #as.matrix(cbind(t,n_plus,price,util))
  price
}
#####

##### normal functionality as found in the Schmidt papers
RK2.orig <- function(alpha=1,beta=1,gamma=1,delta=1,nu=1,d=0,rho=0,n0=0.4,p0=1.05,dt=0.01,N=25,pf=1){
  T <- N/dt+1
  n_plus <- numeric(T)
  price <- numeric(T)
  util <- numeric(T)
  n_plus[1] <- n0
  price[1] <- p0
  U <- function(n,p,t)
  {    
    alpha*fp(n)/p + beta*(pf-p)
  }
  
  fn <- function(n,p,t)
  {
    nu*exp(U(n,p,t))*(1-n) - nu*exp(-1*U(n,p,t))*n
  }
  
  fp <- function(n)
  {
    gamma*(delta*(2*n-1)+d*rnorm(1))+rho*rnorm(1)
  }
  
  util[1] <- U(n0,p0,1)
  for(i in 2:T)
  {
    np <- n_plus[i-1]+ 0.5*dt*fn(n_plus[i-1],price[i-1],i-1)
    pp <- price[i-1] + 0.5*dt*fp(n_plus[i-1])
    n_plus[i] <- n_plus[i-1] + dt*fn(np,pp,i)
    price[i] <- price[i-1] + dt*fp(np)
    util[i] <- U(n_plus[i],price[i],i)
  }
  t <- seq(0,N,dt)
  as.matrix(cbind(t,n_plus,price,util))
}
#####


#### Box-Muller algorithm for generating white noise random variables (standard normal)
snorm <- function(){
  u1 <- runif(1)
  u2 <- runif(1)
  z1 <- ((((-2)*log(u1))^(1/2))*cos(2*u2*pi))
  z2 <- ((((-2)*log(u1))^(1/2))*sin(2*u2*pi))
  z <- c(z1,z2)
  return(z)
}
