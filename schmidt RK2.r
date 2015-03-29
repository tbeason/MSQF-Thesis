##### normal functionality as found in the Schmidt papers
SchmidtRK2 <- function(alpha1=1,alpha2=1,beta=1,gamma=1,delta=1,vega=1,d=0,rho=0,n0=0.4,p0=1.05,dt=0.01,N=25,pf=1,Tf,r){
  T <- N/dt+1
  n_plus <- numeric(T)
  price <- numeric(T)
  util <- numeric(T)
  n_plus[1] <- n0
  price[1] <- p0
  U <- function(n,p,t)
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
    
    alpha1*(2*n-1) + alpha2*pChange + beta*(1/vega)*L*(pf-p)
  }
  
  fn <- function(n,p,t)
  {
    vega*exp(U(n,p,t))*(1-n) - vega*exp(-1*U(n,p,t))*n
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

