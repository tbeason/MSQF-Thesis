# this is the driver program for schmidt-style 

#source outside files to access functions
source('~/GitHub/MSQF-Thesis/schmidt RK2.r')

model <- SchmidtRK2(alpha=1,beta=1,gamma=1,delta=1,d=0,rho=0,n0=0.4,p0=1.05,dt=0.01,N=25)
par(mfrow=c(2,2))
par(mar=c(3,4.5,1,1.5))
plot(model[,1],model[,3],type="l",ylab="Price",xlab="")
plot(model[,1],model[,2],type="l",col="blue",ylab="Excess Demand",xlab="")
plot(model[,1],model[,4],type="l",col="red",ylab="Utility",xlab="")
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

modela2 <- SchmidtRK2.anchor2(alpha=1,beta=1,gamma=1,delta=1,d=0,rho=0,n0=0.4,p0=0.95,dt=0.01,N=25,poss=0.5)
par(mfrow=c(2,2))
par(mar=c(3,4.5,1,1.5))
plot(modela2[,1],modela2[,3],type="l",ylab="Price",xlab="")
plot(modela2[,1],modela2[,2],type="l",col="blue",ylab="Excess Demand",xlab="")
plot(modela2[,1],modela2[,4],col="red",ylab="Utility (w/ anchoring)",xlab="")