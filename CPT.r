
wp <- function(x)
{
  cpf <- pnorm(x,sd=0.4)
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

ret <- rnorm(1000,0,0.4)
weightedProb <- vapply(ret,wp,numeric(1))
#normalizedProb <- weightedProb/sum(weightedProb)
values <- vapply(ret,valFun,numeric(1))
sum(weightedProb*values)
plot(ret,values)
cf <- pnorm(ret,sd=0.4)
plot(cf,weightedProb)