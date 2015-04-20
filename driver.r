# this is the driver program for schmidt-style 

#source outside files to access functions
source('~/GitHub/MSQF-Thesis/RK2.r')
source('~/GitHub/MSQF-Thesis/kMC.r')

require("PerformanceAnalytics")
require(plyr)
b <- 1
a1 <- 1
a2 <- 1
gamma <- 1
delta <- 1
nu <- 1
n0 <- 0.5
dt <- 0.05
N <- 25
pf <- 1
Tf <- 0.75*N/dt
r <- 0.0
ma <- 5
p0 <- 1.05


Time <- seq(0,N,dt)

params <- seq(2,20,1)
Price <- t(laply(params,.fun=RK2.me,alpha1=a1,alpha2=a2,beta=b,delta=delta,gamma=gamma,nu=nu,n0=n0,d=0,rho=0,p0=p0,dt=dt,N=N,pf=pf,Tf=Tf,r=r ))
persp3D(x=Time,y=params,z=Price,shade=0.4,ticktype="detailed",xlab="Time",zlab="Price",ylab="j",theta=40,phi=40)

matplot(Time,Price,type='l',lty=1,main="Varying beta")
legend("topright",legend=params,lty=1)



model <- RK2.me(alpha1=a1,alpha2=a2,beta=b,gamma=gamma,delta=delta,nu=nu,d=0,rho=0,n0=n0,p0=p0,dt=dt,N=N,pf=pf,Tf=Tf,r=r,ma=ma)
matplot(model[,1],model[,3],type="l",ylab="Price",xlab="Time",main="Price (RK2)")
matplot(model[,1],model[,2]-0.5,type="l",ylab="Excess Demand",xlab="", main="Demand (RK2)")
matplot(model[,1],model[,4],type="l",ylab="Utility",xlab="", main ="Utility (RK2)")
hist(diff(log(model[,3])), main="Dist of Log Returns (RK2)",xlab="Log Return")
kurtosis(diff(log(model[,3])),method="sample")


# model2 <- RK2.orig(alpha1=1,beta=1,gamma=1,delta=1,nu=1,d=0,rho=0,n0=0.5,p0=1.05,dt=0.01,N=25,pf=1)
# matplot(model2[,1],model2[,3],type="l",ylab="Price",xlab="Time",main="Price (RK2)")
# matplot(model[,1],model[,2]-0.5,type="l",ylab="Excess Demand",xlab="", main="Demand (RK2)")
# matplot(model[,1],model[,4],type="l",ylab="Utility",xlab="", main ="Utility (RK2)")
# hist(diff(log(model[,3])), main="Dist of Log Returns (RK2)",xlab="Log Return")
# kurtosis(diff(log(model[,3])),method="sample")
# 
# total <- 10000
# kmc <- kMC.me(numPlus=total*n0,numMinus=total-total*n0,p0=p0,alpha1=a1,alpha2=a2,beta=b,gamma=gamma,nu=nu,delta=delta,numIt=N/dt+1,
#            numTrials=1,dt=dt,pf=pf,Tf=Tf,r=r,ma=ma)
# plot(kmc[[1]],type="l",ylab="Price",xlab="Time",main="Price (kMC)")
# plot(diff(log(kmc[[1]])),type="l",xlab="Time", ylab="Log Return",main="Log Return (kMC)")
# plot(kmc[[1]],(kmc[[2]]-5000)/10000,type="l",lty="dashed", main="Phase plot (kMC)",xlab="Price",ylab="Excess Demand")
# hist(diff(log(kmc[[1]])), main="Dist of Log Returns (kMC)",xlab="Log Return")
# kurtosis(diff(log(kmc[[1]])),method="sample")
# # plot(kmc[[2]],type="l",main="Buyers")
# 
# matplot(model[,1],cbind(model[,3],kmc[[1]]),type="l",lty=1,main="Price Comparison",ylab="Price",xlab="Time")
# 
# spec <- ugarchspec()
# fit.rk2 <- ugarchfit(data=diff(log(model[,3])),spec=spec)
# fit.kmc <- ugarchfit(data=diff(log(kmc[[1]])),spec=spec)
# plot(sqrt(252)*fit.rk2@fit$sigma,type='l')
# plot(sqrt(252)*fit.kmc@fit$sigma,type='l')
# coef(fit.rk2)
# coef(fit.kmc)


