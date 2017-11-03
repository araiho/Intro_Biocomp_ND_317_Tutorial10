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

param=c(0.0005,0.05)
Initial=c(999,1,0)
times=1:500

modelSim=ode(y=Initial,times=times,func=SIRSim,parms=param)

modelOutput=data.frame(time=modelSim[,1],S=modelSim[,2],I=modelSim[,3],R=modelSim[,4])

ggplot()+geom_line(data=modelOutput,aes(x=time,y=S),color="black")+geom_line(data=modelOutput,aes(x=time,y=I),color="red")+geom_line(data=modelOutput,aes(x=time,y=R),color="blue")+theme_classic()+xlab('Time (Days)')+ylab('Number of People')