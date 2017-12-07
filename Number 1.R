#######QUESTION 1#############

##load the deSolve package and ggplot2 for plotting
library(deSolve)
library(ggplot2)

#####Plot of population size as a function of time with 5 populations with different r
#custom function that defines the model differential equations for density-dependent pop growth
ddSim=function(t,y,p){
  N1=y
  
  r1=p[1]
  K1=p[2]
  
  dNdt=r1*(1-N1/K1)*N1
  
  return(list(c(dNdt)))
}

##create a dataframe model output for various r values
rValues=data.frame(c(-0.1,0.1,0.4,0.8,1.0))

#create a for loop to simulate with the different parameters of interest and store information
#define parameters, initial values and time steps
for(i in 1:length(rValues)){
  params=c(i,100)
  N0=c(10)
  times=1:10
##simulate model using ode()
modelSim=ode(y=N0,times=times,func=ddSim,parms=params)
}
#convert to datafrome for plotting purposes
modelOutput=data.frame(time=modelSim[,1],N=modelSim[,2])

#plot
#ggplot(modelOutput) + geom_line(aes(x=time,y=N)) + geom_line(x=time, y=?)
ggplot(modelOutput,aes(x=time,y=N))+geom_line()+theme_classic()






####Plot of population size as a function of time with three populations with different k
#custom function that defines the model differential equations for density-dependent pop growth
ddSim=function(t,y,p){
  N=y
  
  r=p[1]
  K=p[2]
  
  dNdt=r*(1-N/K)*N
  
  return(list(dNdt))
}
##create a dataframe model output for various k values
kValues=data.frame(c(10,50,100))

#create a for loop to simulate with the different parameters of interest and store information
#define parameters, initial values and time steps
for(i in 1:length(kValues)){
  params=c(0.2, i)
  N0=1
  times=1:10
}

##simulate model using ode()
modelSim=ode(y=N0,times=times,func=ddSim,parms=params)

#convert to datafrome for plotting purposes
modelOutput=data.frame(time=modelSim[,1],N=modelSim[,2])

ggplot(modelOutput,aes(x=time,y=N))+geom_line()+theme_classic()





#####Plot of population size as a function of time with three populations with different N0
#custom function that defines the model differential equations for density-dependent pop growth
ddSim=function(t,y,p){
  N=y
  
  r=p[1]
  K=p[2]
  
  dNdt=r*(1-N/K)*N
  
  return(list(dNdt))
}
##create a dataframe model output for various N0 values
nValues=data.frame(c(1,50,100))

#create a for loop to simulate with the different parameters of interest and store information
#define parameters, initial values and time steps
for(i in 1:length(nValues)){
  params=c(0.1,50)
  N0=i
  times=1:10
}

##simulate model using ode()
modelSim=ode(y=N0,times=times,func=ddSim,parms=params)

#convert to datafrome for plotting purposes
modelOutput=data.frame(time=modelSim[,1],N=modelSim[,2])

ggplot(modelOutput,aes(x=time,y=N))+geom_line()+theme_classic()
