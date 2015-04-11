# this is the driver program for schmidt-style 

#source outside files to access functions
source('~/GitHub/MSQF-Thesis/RK2.r')
source('~/GitHub/MSQF-Thesis/kMC.r')

require("PerformanceAnalytics")

b <- 2
a1 <- 1
a2 <- 3
gamma <- 1
delta <- 1
nu <- 1
n0 <- 0.4
dt <- 0.001
N <- 100
pf <- 50
Tf <- 0.75*N/dt
r <- 0.05
ma <- 5


p0 <- 50.1
model <- RK2.me(alpha1=a1,alpha2=a2,beta=b,gamma=gamma,delta=delta,nu=nu,d=0,rho=0,n0=n0,p0=p0,dt=dt,N=N,pf=pf,Tf=Tf,r=r,ma=ma)
matplot(model[,1],model[,3],type="l",ylab="Price",xlab="Time",main="Price (RK2)")
matplot(model[,1],model[,2]-0.5,type="l",ylab="Excess Demand",xlab="", main="Demand (RK2)")
matplot(model[,1],model[,4],type="l",ylab="Utility",xlab="", main ="Utility (RK2)")
hist(diff(log(model[,3])), main="Dist of Log Returns (RK2)",xlab="Log Return")
kurtosis(diff(log(model[,3])),method="sample")

total <- 10000
kmc <- kMC.me(numPlus=total*n0,numMinus=total-total*n0,p0=p0,alpha1=a1,alpha2=a2,beta=b,gamma=gamma,nu=nu,delta=delta,numIt=N/dt+1,
           numTrials=8,dt=dt,pf=pf,Tf=Tf,r=r,ma=ma)
plot(kmc[[1]],type="l",ylab="Price",xlab="Time",main="Price (kMC)")
plot(diff(log(kmc[[1]])),type="l",xlab="Time", ylab="Log Return",main="Log Return (kMC)")
plot(kmc[[1]],(kmc[[2]]-5000)/10000,type="l",lty="dashed", main="Phase plot (kMC)",xlab="Price",ylab="Excess Demand")
hist(diff(log(kmc[[1]])), main="Dist of Log Returns (kMC)",xlab="Log Return")
kurtosis(diff(log(kmc[[1]])),method="sample")
# plot(kmc[[2]],type="l",main="Buyers")

matplot(model[,1],cbind(model[,3],kmc[[1]]),type="l",lty=1,main="Price Comparison",ylab="Price",xlab="Time")
