LuxGrid <- function(L=100,maxit=10000,n0=0.5,p0=105,pf=100,alpha=1,beta=1,gamma=1)
{
  nplus <- numeric(maxit+1)
  nminus <- numeric(maxit+1)
  price <- numeric(maxit+1)
  grid <- matrix(-1,L,L)
  Ninit <- ceiling(L*L*n0)
  rX <- ceiling(runif(Ninit,0,L))
  rY <- ceiling(runif(Ninit,0,L))
  for(i in 1:Ninit)
  {
    grid[rX[i],rY[i]] <- 1
  }
  startGrid <- grid
  nplus[1] <- length(which(grid==1))
  nminus[1] <- (L*L)-nplus[1]
  price[1] <- p0
  for(i in 1:maxit)
  {
    np <- nplus[i]
    nm <- nminus[i]
    p <- price[i]
    x <- 0.5*(np-nm)/(L*L)
    U <- beta*x
    prob <- prob <- nm*exp(U)/(nm*exp(U)+np*exp(-U))
    rand <- runif(1)
    rpos <- ceiling(runif(2,0,L))
    
    op <- grid[rpos[1],rpos[2]]
    if(rand <= prob)
    {
      grid[rpos[1],rpos[2]] = -1*op
    }
    
    nplus[i+1] <- length(which(grid==1))
    nminus[i+1] <- (L*L)-nplus[i+1]
    excDem <- nplus[i+1]-nminus[i+1]
    price[i+1] <- max(0,p+gamma*excDem/(L*L))
    
  }
  
  
  
  list(startGrid = startGrid, grid = grid, nplus = nplus, price = price)
}