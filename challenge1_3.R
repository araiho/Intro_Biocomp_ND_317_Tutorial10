# load packages 
library(deSolve)
library(ggplot2)

#k=50
#r=0.1
#N0=1,50,100

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
modelOutput1=data.frame(time=modelSim[,1],N=modelSim[,2])

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
modelOutput2=data.frame(time=modelSim[,1],N=modelSim[,2])


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
modelOutput3=data.frame(time=modelSim[,1],N=modelSim[,2])


##Plot the population size as a function of time with 3 populations
ggplot(data=modelOutput1,aes(x=time, y=N), colour="black")+
  geom_line(data=modelOutput2, aes(x=time, y=N), color="red")+
  geom_line(data=modelOutput3,aes(x=time, y=N), color= "blue")+
  theme_classic()

