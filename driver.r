# this is the driver program for schmidt-style 

#source outside files to access functions
source('~/GitHub/MSQF-Thesis/schmidt RK2.r')
source('~/GitHub/MSQF-Thesis/kMC.r')


b <- 2
z <- 5
a1 <- 0.1
a2 <- z/b-a1
model <- SchmidtRK2(alpha1=a1,alpha2=a2,beta=b,gamma=1,delta=1,vega=1,d=0,rho=0,n0=0.4,p0=10.05,dt=0.01,N=150,pf=10,Tf=100/0.01,r=0.05)
matplot(model[,1],model[,3],type="l",ylab="Price",xlab="")
matplot(model[,1],model[,2]-0.5,type="l",ylab="Excess Demand",xlab="")
matplot(model[,1],model[,4],type="l",ylab="Utility",xlab="")
hist(diff(log(model[,3])), main="Dist of Log Returns")
kurtosis(diff(log(model[,3])),method="sample")


kmc <- kMC(4000,5000,p0=10.1,alpha1=a1,alpha2=a2,beta=b,gamma=1,vega=1,delta=1,numIt=15000,numTrials=1,dt=0.01,pf=10,Tf=100/0.01,r=0.05)
plot(kmc[[1]],type="l",main="Price")
plot(diff(log(kmc[[1]])),type="l", main="Log Return")
plot(kmc[[1]],(kmc[[2]]-5000)/10000,type="l",lty="dashed", main="Phase plot",xlab="price",ylab="excess demand")
hist(diff(log(kmc[[1]])), main="Dist of Log Returns")
kurtosis(diff(log(kmc[[1]])),method="sample")
# plot(kmc[[2]],type="l",main="Buyers")

