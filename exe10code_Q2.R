library(deSolve)
library(ggplot2)

DiseaseModels<-function(t,y,p){
  S = y[1]
  I = y[2]
  R = y[3]
  B = p[1]
  G = p[2]
  dSdt=-B*I*S
  dIdt=B*I*S-G*I
  dRdt= G*I
  return(list(c(dSdt,dIdt,dRdt)))
}

DiseaseModels<-function(t,y,p){
  Susceptible = y[1]
  Infected = y[2]
  Resistant = y[3]
  B= p[1]
  G = p[2]
  dSdt=-B*Infected*Susceptible
  dIdt=B*Infected*Susceptible-G*Infected
  dRdt= G*Infected
  return(list(c(dSdt,dIdt,dRdt)))
}


params=c(0.0005,0.05)
pop0 = as.vector(c(999,1,0))
times=1:500

modelSim=ode(y=pop0,times=times,func=DiseaseModels,parms = params)

modelOutput=data.frame(time=modelSim[,1],N=modelSim[,2:4])

ggplot(data=modelOutput) + geom_line(aes(x=time,y=N.1),color="firebrick") + geom_line(aes(x=time,y=N.2),color="royalblue")+ geom_line(aes(x=time,y=N.3),color="mediumturquoise") + theme_classic()+ ylab("N")

params=c(0.00005,0.1)
pop0 = as.vector(c(999,1,0))
times=1:500

modelSim=ode(y=pop0,times=times,func=DiseaseModels,parms = params)

modelOutput=data.frame(time=modelSim[,1],N=modelSim[,2:4])

ggplot(data=modelOutput) + geom_line(aes(x=time,y=N.1),color="firebrick") + geom_line(aes(x=time,y=N.2),color="royalblue")+ geom_line(aes(x=time,y=N.3),color="green") + theme_classic()+ ylab("N")
 
