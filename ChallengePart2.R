rm(list=ls())

library(deSolve)
library(ggplot2)

SIRSim=function(t,y,p){
  S=y[1]
  I=y[2]
  R=y[3]
  
  B=p[1]
  Y=p[2]
  
  dS=-B*I*S
  dI=B*I*S-Y*I
  dR=Y*I
  
  return(list(c(dS,dI,dR)))
}

param=matrix(c(0.0005,0.005,0.0001,0.00005,0.0001,0.0002,0.0001,0.05,0.5,0.1,0.1,0.05,0.05,0.06),nrow=7,ncol=2)
Initial=c(999,1,0)
times=1:500

Results=data.frame(matrix(ncol=4,nrow=7))
colnames(Results) = c('Maximum Daily Incidence','Maximum Daily Prevalance', 'Percent Affected', 'Basic Reproduction Number')

for (j in 1:nrow(param)){
  
  modelSim=ode(y=Initial,times=times,func=SIRSim,parms=param[j,])
  modelOutput=data.frame(time=modelSim[,1],S=modelSim[,2],I=modelSim[,3],R=modelSim[,4])
  Incidence=data.frame(matrix(ncol=1,nrow=499))
  Prevalance=data.frame(matrix(ncol=1,nrow=500))
  
  for (i in 1:nrow(modelOutput)){
    Incidence[i,1]=modelOutput[i+1,3]-modelOutput[i,3]
    Prevalance[i,1]=modelOutput[i,3]/(modelOutput[i,1]+modelOutput[i,2]+modelOutput[i,3])
  }
  
  Results[j,1]=max(Incidence,na.rm=T)
  Results[j,2]=max(Prevalance,na.rm=T)
  Results[j,3]=(modelOutput[nrow(modelOutput),3]+modelOutput[nrow(modelOutput),4])/(modelOutput[nrow(modelOutput),4]+modelOutput[nrow(modelOutput),2]+modelOutput[nrow(modelOutput),3])
  Results[j,4]=1000*param[1,1]/param[1,2]
}

#Comment
