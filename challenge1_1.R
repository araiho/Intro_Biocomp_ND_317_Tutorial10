# load packages 
library(deSolve)
library(ggplot2)

#Define custom model function
ddSim<-function(t,y,p){
  
# "unpack" vectors containing state variables (y) and parameters (p)
  N=y
  r=p[1]
  K=p[2]
  
#calculate change in state variables with time, given parameter values
#and current value of state variables
  dNdt=r*(1-N/K)*N
  
  
#return list containing change in state variables with time
  return(list(dNdt))
}


#Define parameters, initial values for state variables, and time steps
params=c(-0.1,100)
N0=10
times=1:100


#Simulate the model using ode()
modelSim=ode(y=N0,times=times,func=ddSim,parms=params)


#convert to a dataframe for plotting purposes
modelOutput1=data.frame(time=modelSim[,1],N=modelSim[,2])

#Repeat the same with different values of r

ddSim<-function(t,y,p){
  N=y
  r=p[1]
  K=p[2]
  
  dNdt=r*(1-N/K)*N
  
  return(list(dNdt))
}

params=c(0.1,100)
N0=10
times=1:100

modelSim=ode(y=N0,times=times,func=ddSim,parms=params)
modelOutput2=data.frame(time=modelSim[,1],N=modelSim[,2])


ddSim<-function(t,y,p){
  
  N=y
  r=p[1]
  K=p[2]
  
  dNdt=r*(1-N/K)*N
  
  return(list(dNdt))
}

params=c(0.4,100)
N0=10
times=1:100

modelSim=ode(y=N0,times=times,func=ddSim,parms=params)
modelOutput3=data.frame(time=modelSim[,1],N=modelSim[,2])


ddSim<-function(t,y,p){
  
  N=y
  r=p[1]
  K=p[2]
  
  dNdt=r*(1-N/K)*N
  
  return(list(dNdt))
}

params=c(0.8,100)
N0=10
times=1:100

modelSim=ode(y=N0,times=times,func=ddSim,parms=params)
modelOutput4=data.frame(time=modelSim[,1],N=modelSim[,2])


ddSim<-function(t,y,p){
  
  N=y
  r=p[1]
  K=p[2]
  
  dNdt=r*(1-N/K)*N
  
  return(list(dNdt))
}

params=c(1,100)
N0=10
times=1:100

modelSim=ode(y=N0,times=times,func=ddSim,parms=params)
modelOutput5=data.frame(time=modelSim[,1],N=modelSim[,2])

##Plot the population size as a function of time with 5 populations
ggplot(data=modelOutput1,aes(x=time, y=N), colour="black")+
  geom_line(data=modelOutput2, aes(x=time, y=N), color="red")+
  geom_line(data=modelOutput3,aes(x=time, y=N), color= "blue")+
  geom_line(data=modelOutput4,aes(x=time, y=N), color="green")+
  geom_line(data=modelOutput5,aes(x=time, y=N), color="pink")+
  theme_classic()


