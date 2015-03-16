##### normal functionality as found in the Schmidt papers
SchmidtRK2 <- function(alpha=1,beta=1,gamma=1,delta=1,d=0.1,rho=0.05,n0=0.4,p0=1.05,dt=0.01,N=25,pf=1){
  T <- N/dt+1
  n_plus <- numeric(T)
  price <- numeric(T)
  util <- numeric(T)
  n_plus[1] <- n0
  price[1] <- p0
  U <- function(n,p)
  {
    alpha*gamma*delta*(2*n-1) +beta*(pf-p)
  }
  
  fn <- function(n,p)
  {
    exp(U(n,p))*(1-n) - exp(-1*U(n,p))*n
  }
  
  fp <- function(n)
  {
    gamma*(delta*(2*n-1)+d*rnorm(1))+rho*rnorm(1)
  }
  
  util[1] <- U(n0,p0)
  for(i in 2:T)
  {
    np <- n_plus[i-1]+ 0.5*dt*fn(n_plus[i-1],price[i-1])
    pp <- price[i-1] + 0.5*dt*fp(n_plus[i-1])
    n_plus[i] <- n_plus[i-1] + dt*fn(np,pp)
    price[i] <- price[i-1] + dt*fp(np)
    util[i] <- U(n_plus[i],price[i])
  }
  t <- seq(0,N,dt)
  as.matrix(cbind(t,n_plus,price,util))
}
#####

##### under endowment effects
SchmidtRK2.endow <- function(alpha=1,beta=1,gamma=1,delta=1,d=0.1,rho=0.05,n0=0.4,p0=1.05,dt=0.01,N=25,ee=0){
  T <- N/dt+1
  n_plus <- numeric(T)
  price <- numeric(T)
  util <- numeric(T)
  n_plus[1] <- n0
  price[1] <- p0
  U <- function(n,p)
  {
    U <- alpha*gamma*delta*(2*n-1)/p + beta*(1-p)+ee
  }
  
  fn <- function(n,p)
  {
    fn <- exp(U(n,p))*(1-n) - exp(-1*U(n,p))*n
  }
  
  fp <- function(n)
  {
    fp <- gamma*(delta*(2*n-1)+d*rnorm(1))+rho*rnorm(1)
  }
  
  util[1] <- U(n0,p0)
  for(i in 2:T)
  {
    np <- n_plus[i-1]+ 0.5*dt*fn(n_plus[i-1],price[i-1])
    pp <- price[i-1] + 0.5*dt*fp(n_plus[i-1])
    n_plus[i] <- n_plus[i-1] + dt*fn(np,pp)
    price[i] <- price[i-1] + dt*fp(np)
    util[i] <- U(n_plus[i],price[i])
  }
  t <- seq(0,N,dt)
  as.matrix(cbind(t,n_plus,price,util))
}
#####

##### under anchoring (expected value of )
SchmidtRK2.anchor1 <- function(alpha=1,beta=1,gamma=1,delta=1,d=0.1,rho=0.05,n0=0.4,p0=1.05,dt=0.01,N=25,w,anchors){
  T <- N/dt+1
  n_plus <- numeric(T)
  price <- numeric(T)
  util <- numeric(T)
  n_plus[1] <- n0
  price[1] <- p0
  U <- function(n,p)
  {
    temp <- mean(w*(anchors-p))
    U <- alpha*gamma*delta*(2*n-1)/p + beta*temp
  }
  
  fn <- function(n,p)
  {
    fn <- exp(U(n,p))*(1-n) - exp(-1*U(n,p))*n
  }
  
  fp <- function(n)
  {
    fp <- gamma*(delta*(2*n-1)+d*rnorm(1))+rho*rnorm(1)
  }
  
  util[1] <- U(n0,p0)
  for(i in 2:T)
  {
    np <- n_plus[i-1]+ 0.5*dt*fn(n_plus[i-1],price[i-1])
    pp <- price[i-1] + 0.5*dt*fp(n_plus[i-1])
    n_plus[i] <- n_plus[i-1] + dt*fn(np,pp)
    price[i] <- price[i-1] + dt*fp(np)
    util[i] <- U(n_plus[i],price[i])
  }
  t <- seq(0,N,dt)
  as.matrix(cbind(t,n_plus,price,util))
}
#####

##### under anchoring (random chance that they ignore fundmental value and use anchor instead)
SchmidtRK2.anchor2 <- function(alpha=1,beta=1,gamma=1,delta=1,d=0.1,rho=0.05,n0=0.4,p0=1.05,dt=0.01,N=25,poss=0.5){
  T <- N/dt+1
  n_plus <- numeric(T)
  price <- numeric(T)
  util <- numeric(T)
  n_plus[1] <- n0
  price[1] <- p0
  U <- function(n,p)
  {
    ran <- runif(1)
    if(ran >= poss && p < 1)
    {
      temp <- 0.75-p
    }
    else if( ran >= poss && p > 1)
    {
      temp <- 1.25 - p
    }
    else
    {
      temp <- 1-p
    }
    U <- alpha*gamma*delta*(2*n-1)/p + beta*temp
  }
  
  fn <- function(n,p)
  {
    fn <- exp(U(n,p))*(1-n) - exp(-1*U(n,p))*n
  }
  
  fp <- function(n)
  {
    fp <- gamma*(delta*(2*n-1)+d*rnorm(1))+rho*rnorm(1)
  }
  
  util[1] <- U(n0,p0)
  for(i in 2:T)
  {
    np <- n_plus[i-1]+ 0.5*dt*fn(n_plus[i-1],price[i-1])
    pp <- price[i-1] + 0.5*dt*fp(n_plus[i-1])
    n_plus[i] <- n_plus[i-1] + dt*fn(np,pp)
    price[i] <- price[i-1] + dt*fp(np)
    util[i] <- U(n_plus[i],price[i])
  }
  t <- seq(0,N,dt)
  as.matrix(cbind(t,n_plus,price,util))
}
#####

##### reworked functions
SchmidtRK2.glv <- function(alpha=1,beta=1,gamma=1,delta=1,d=0.1,rho=0.05,n0=0.4,p0=1.05,dt=0.01,N=25,poss=0.5){
  T <- N/dt+1
  n_plus <- numeric(T)
  price <- numeric(T)
  util <- numeric(T)
  n_plus[1] <- n0
  price[1] <- p0
  U <- function(n,p)
  {
    alpha*gamma*(2*n-1)/p + beta*(1-p)
  }
  
  fn <- function(n,p)
  {
    front <- exp(beta*(1-p))+alpha
    back <- exp(-beta*(1-p))+alpha
    dp <- gamma*sign(2*n-1)*abs(2*n-1)
    de <- front/(front+back)
    (de-(1-de))*n*(1-n)
    #(exp(U(n,p)) - exp(-1*U(n,p)))*n*(1-n)
  }
  
  fp <- function(n)
  {
    gamma*(2*n-1)
  }
  
  util[1] <- U(n0,p0)
  for(i in 2:T)
  {
    np <- n_plus[i-1]+ 0.5*dt*fn(n_plus[i-1],price[i-1])
    pp <- price[i-1] + 0.5*dt*fp(n_plus[i-1])
    n_plus[i] <- n_plus[i-1] + dt*fn(np,pp)
    price[i] <- price[i-1] + dt*fp(np)
    util[i] <- U(n_plus[i],price[i])
  }
  t <- seq(0,N,dt)
  as.matrix(cbind(t,n_plus,price,util))
}