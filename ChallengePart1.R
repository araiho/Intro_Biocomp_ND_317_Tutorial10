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
modelOutput11=data.frame(time=modelSim[,1],N=modelSim[,2])

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
modelOutput12=data.frame(time=modelSim[,1],N=modelSim[,2])


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
modelOutput13=data.frame(time=modelSim[,1],N=modelSim[,2])


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
modelOutput14=data.frame(time=modelSim[,1],N=modelSim[,2])


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
modelOutput15=data.frame(time=modelSim[,1],N=modelSim[,2])


#Define custom model function
ddSim<-function(t,y,p){
  # "unpack" vectors containing state variables (y) and parameters (p)
  N=y
  r=p[1]
  K=p[2]
  # calculate change in state variables with time, given parameter values
  # and current value of state variables
  dNdt=r*(1-N/K)*N
  # return list containing change in state variables with time
  return(list(dNdt))
}

### Define parameters, initial values for state variables, and time steps
params=c(0.2,10)
N0=1
times=1:100

#Simulate the model using ode()
modelSim=ode(y=N0,times=times,func=ddSim,parms=params)

#convert to a dataframe for plotting purposes
modelOutput21=data.frame(time=modelSim[,1],N=modelSim[,2])

#Repeat the above simulation for different k values

ddSim<-function(t,y,p){
  N=y
  r=p[1]
  K=p[2]
  
  dNdt=r*(1-N/K)*N
  
  return(list(dNdt))
}
params=c(0.2,50)
N0=1
times=1:100
modelSim=ode(y=N0,times=times,func=ddSim,parms=params)
modelOutput22=data.frame(time=modelSim[,1],N=modelSim[,2])


ddSim<-function(t,y,p){
  N=y
  r=p[1]
  K=p[2]
  
  dNdt=r*(1-N/K)*N
  
  return(list(dNdt))
}
params=c(0.2,100)
N0=1
times=1:100
modelSim=ode(y=N0,times=times,func=ddSim,parms=params)
modelOutput23=data.frame(time=modelSim[,1],N=modelSim[,2])


#Define custom model function
ddSim<-function(t,y,p){
  # "unpack" vectors containing state variables (y) and parameters (p)
  N=y
  r=p[1]
  K=p[2]
  
  # calculate change in state variables with time, given parameter values
  # and current value of state variables
  dNdt=r*(1-N/K)*N
  
  # return list containing change in state variables with time
  return(list(dNdt))
}

### Define parameters, initial values for state variables, and time steps
params=c(0.1,50)
N0=1
times=1:100

### Simulate the model using ode()
modelSim=ode(y=N0,times=times,func=ddSim,parms=params)
# modelSim is a variable that contains a deSolve object
# this contains some attributes about the way the simulation was conducted, but the key content is a
# matrix with time as the first column and model state variables in subsequent columns
# convert to a dataframe for plotting purposes
modelOutput31=data.frame(time=modelSim[,1],N=modelSim[,2])

##Repeat the above simulation for different N values
ddSim<-function(t,y,p){
  N=y
  r=p[1]
  K=p[2]
  
  dNdt=r*(1-N/K)*N
  
  return(list(dNdt))
}
params=c(0.1,50)
N0=50
times=1:100
modelSim=ode(y=N0,times=times,func=ddSim,parms=params)
modelOutput32=data.frame(time=modelSim[,1],N=modelSim[,2])


ddSim<-function(t,y,p){
  N=y
  r=p[1]
  K=p[2]
  
  dNdt=r*(1-N/K)*N
  
  return(list(dNdt))
}
params=c(0.1,50)
N0=100
times=1:100
modelSim=ode(y=N0,times=times,func=ddSim,parms=params)
modelOutput33=data.frame(time=modelSim[,1],N=modelSim[,2])

##Plot the population size as a function of time with 5 populations
ggplot()+
  geom_line(data=modelOutput11,aes(x=time, y=N), colour="black")+
  geom_line(data=modelOutput12, aes(x=time, y=N), color="red")+
  geom_line(data=modelOutput13,aes(x=time, y=N), color= "blue")+
  geom_line(data=modelOutput14,aes(x=time, y=N), color="green")+
  geom_line(data=modelOutput15,aes(x=time, y=N), color="pink")+
  theme_classic()

##Plot the population size as a function of time with 3 populations
ggplot()+
  geom_line(data=modelOutput21,aes(x=time, y=N), colour="black")+
  geom_line(data=modelOutput22, aes(x=time, y=N), color="red")+
  geom_line(data=modelOutput23,aes(x=time, y=N), color= "blue")+
  theme_classic()

##Plot the population size as a function of time with 3 populations
ggplot()+
  geom_line(data=modelOutput31,aes(x=time, y=N), colour="black")+
  geom_line(data=modelOutput32, aes(x=time, y=N), color="red")+
  geom_line(data=modelOutput33,aes(x=time, y=N), color= "blue")+
  theme_classic()


