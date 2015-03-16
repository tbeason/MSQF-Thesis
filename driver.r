# this is the driver program for schmidt-style 

#source outside files to access functions
source('~/GitHub/MSQF-Thesis/schmidt RK2.r')
source('~/GitHub/MSQF-Thesis/kMC.r')
source('~/GitHub/MSQF-Thesis/LuxGrid.r')

model <- SchmidtRK2(alpha=0,beta=1,gamma=1,delta=1,d=0,rho=0,n0=0.4,p0=100.5,dt=0.01,N=25,pf=105)
modelc <- CPTRK2(alpha=0,beta=1,gamma=1,delta=1,d=0,rho=0,n0=0.4,p0=100.5,dt=0.01,N=25,pf=105,Fconf=0.1,Tconf=1)
matplot(model[,1],cbind(model[,3],modelc[,3]),type="l",ylab="Price",xlab="")
matplot(model[,1],cbind(model[,2]-0.5,modelc[,2]-0.5),type="l",ylab="Excess Demand",xlab="")
matplot(model[,1],cbind(model[,4],modelc[,4]),type="l",ylab="Utility",xlab="")


# kmc <- kMC(4000,6000,p0=1.05,alpha=1,beta=1,gamma=1,delta=1,numIt=1000,numTrials=1,dt=0.05)
# plot(kmc[[1]],type="l",main="Price")
# plot(kmc[[2]],type="l",main="Buyers")

cpt <- CPT1(4000,6000,p0=9.8,alpha=1,beta=1,gamma=1,delta=1,numIt=1000,numTrials=10,dt=0.05,conf=1,pf=10)
plot(cpt[[1]],type="l",main="Price")
plot(cpt[[2]],type="l",main="Buyers")
# require(reshape2)
# require(ggplot2)
# gr <- LuxGrid(L=100,maxit=100000,n0=0.7,p0=105,pf=100,alpha=1,beta=1,gamma=0.1)
# # x1 <- melt(gr$grid)
# # names(x1)=c("x","y","color")
# # x1$color=factor(x1$color==1)
# # levels(x1$color)=c("nminus","nplus")
# # qplot(x, y, fill=color, data=x1,geom='tile')
# par(mfrow=c(2,2))
# par(mar=c(3,4.5,1,1.5))
# plot(gr$price, type="l",ylab="Price")
# plot(gr$nplus, type="l",ylab="nplus")