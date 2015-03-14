# this is the driver program for schmidt-style 

#source outside files to access functions
source('~/GitHub/MSQF-Thesis/schmidt RK2.r')
source('~/GitHub/MSQF-Thesis/kMC.r')
source('~/GitHub/MSQF-Thesis/LuxGrid.r')

# model <- SchmidtRK2(alpha=1,beta=10,gamma=1,delta=1,d=0,rho=0,n0=0.5,p0=1.05,dt=0.01,N=25)
# par(mfrow=c(2,2))
# par(mar=c(3,4.5,1,1.5))
# plot(model[,1],model[,3],type="l",ylab="Price",xlab="")
# plot(model[,1],model[,2]-0.5,type="l",col="blue",ylab="Excess Demand",xlab="")
# #plot(model[,1],model[,4],type="l",col="red",ylab="Utility",xlab="")
# 
# modele <- SchmidtRK2.endow(alpha=1,beta=1,gamma=1,delta=1,d=0,rho=0,n0=0.4,p0=1.05,dt=0.01,N=25,ee=0.5)
# par(mfrow=c(2,2))
# par(mar=c(3,4.5,1,1.5))
# plot(modele[,1],modele[,3],type="l",ylab="Price",xlab="")
# plot(modele[,1],modele[,2],type="l",col="blue",ylab="Excess Demand",xlab="")
# plot(modele[,1],modele[,4],type="l",col="red",ylab="Utility (w/ endowment)",xlab="")


# anc <- c(1.1,1,0.9)
# w <- c(0.4,0.3,0.3)
# modela <- SchmidtRK2.anchor1(alpha=1,beta=1,gamma=1,delta=1,d=0,rho=0,n0=0.4,p0=1.05,dt=0.01,N=25,w=w,anchors=anc)
# par(mfrow=c(2,2))
# par(mar=c(3,4.5,1,1.5))
# plot(modela[,1],modela[,3],type="l",ylab="Price",xlab="")
# plot(modela[,1],modela[,2],type="l",col="blue",ylab="Excess Demand",xlab="")
# plot(modela[,1],modela[,4],type="l",col="red",ylab="Utility (w/ anchoring)",xlab="")

# modela2 <- SchmidtRK2.anchor2(alpha=1,beta=1,gamma=1,delta=1,d=0,rho=0,n0=0.4,p0=0.95,dt=0.01,N=25,poss=0.5)
# par(mfrow=c(2,2))
# par(mar=c(3,4.5,1,1.5))
# plot(modela2[,1],modela2[,3],type="l",ylab="Price",xlab="")
# plot(modela2[,1],modela2[,2],type="l",col="blue",ylab="Excess Demand",xlab="")
# plot(modela2[,1],modela2[,4],col="red",ylab="Utility (w/ anchoring)",xlab="")


# modelg <- SchmidtRK2.glv(alpha=1,beta=10,gamma=1,delta=1,d=0,rho=1,n0=0.8,p0=1.05,dt=0.01,N=25)
# par(mfrow=c(2,2))
# par(mar=c(3,4.5,1,1.5))
# plot(modelg[,1],modelg[,3],type="l",ylab="Price v2",xlab="")
# plot(modelg[,1],modelg[,2]-0.5,type="l",col="blue",ylab="Excess Demand",xlab="")
# plot(modelg[,1],modelg[,4],type="l",col="red",ylab="Utility",xlab="")

kmc <- kMC(4000,6000,p0=1.05,alpha=1,beta=1,gamma=1,delta=1,numIt=1000,numTrials=1,dt=0.05)
plot(kmc[[1]],type="l",main="Price")
plot(kmc[[2]],type="l",main="Buyers")



cpt <- CPT1(4000,6000,p0=1.05,alpha=1,beta=1,gamma=1,delta=1,numIt=1000,numTrials=1,dt=0.05,stdv=0.25)
plot(kmc[[1]],type="l",main="Price")
plot(kmc[[2]],type="l",main="Buyers")
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