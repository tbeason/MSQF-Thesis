GLV <- function(alpha=1,beta=1,gamma=1,mu=1,num,n0,p0,dt=0.01,N=25){
  T <- N/dt+1
  n_agents <- matrix(0,T,num)
  price <- matrix(0,T,num)
  util <- matrix(0,T,num)
  n_agents[1,] <- n0
  price[1,] <- p0
  interact <- matrix(mu,num,num)
  for(i in 1:num)
  {
    interact[i,i] <- 1
  }
  U <- function(n,p)
  {
    alpha*gamma*(2*n-1)/p + beta*(1-p)
  }
  
  fn <- function(n,p,indexN,indexT)
  {
    
    
  }
  
  fp <- function(n)
  {
    gamma*(2*n-1)
  }
  
  util[1,] <- U(n0,p0)
  for(i in 2:T)
  {
    rand <- ceiling(runif(1,0,num))
    n_agents[i,] <- n_agents[i-1,]
    np <- n_agents[i-1,rand]+ 0.5*dt*fn(n_agents[i-1,rand],price[i-1,rand],rand,i)
    pp <- price[i-1,rand] + 0.5*dt*fp(n_agents[i-1,rand])
    n_agents[i,rand] <- n_agents[i-1,rand] + dt*fn(np,pp,rand,i)
    price[i,rand] <- price[i-1,rand] + dt*fp(np)
  
    util[i,] <- U(n_agents[i,],price[i,])
  }
  t <- seq(0,N,dt)
  list(numAgents = n_agents, prices = price, utility = util)
}

ninit <- c(0.4,0.6)
pinit <- c(1.05,0.91)
G <- GLV(num=2,n0=ninit,p0=pinit,mu=2)