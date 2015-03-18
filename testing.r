Util <- function(pri)
{
  wp <- function(cpf)
  {
    pow <- 0.65
    
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
  payoff <- (9.7-pri)*exp(-lmb)
  outcomes <- rnorm(1000,mean=payoff,sd=stdv*dt)
  profits <- outcomes[outcomes >= 0]
  losses <- outcomes[outcomes < 0]
  probGain <- 1 - pnorm(profits,mean=payoff,sd=stdv*dt)
  probGain <- probGain/sum(probGain)
  probLosses <- pnorm(losses,mean=payoff,sd=stdv*dt)
  probLosses <- probLosses/sum(probLosses)
  weightedProbGains <- vapply(probGain,wp,numeric(1))
  weightedProbLosses <- vapply(probLosses,wp,numeric(1))
  distortedGains <- vapply(profits,valFun,numeric(1))
  distortedLosses <- vapply(losses,valFun,numeric(1))
  beta*(sum(weightedProbGains*distortedGains)+sum(weightedProbLosses*distortedLosses))
}
lmb <- 2
dt <- 0.01
pf <- 10
pri <- seq(9.5,10,0.05)
stdv <- 0.5
conf <- 1
beta <- 1
plot(pri,vapply(pri, Util, numeric(1)))