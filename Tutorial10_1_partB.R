#######QUESTION 1 part 2#############

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

#define parameters, initial values and time steps for first k value
params=c(0.2,10)
N0=1
times=1:100
##simulate model using ode()
k1modelSim=ode(y=N0,times=times,func=ddSim,parms=params)
#convert to datafrome for plotting purposes
k1modelOutput=data.frame(time=k1modelSim[,1],N=k1modelSim[,2])
#plot
k1=ggplot(k1modelOutput,aes(x=time,y=N))+geom_line()+theme_classic()

##second k value: 
params=c(0.2,50)
N0=1
times=1:100
##simulate model using ode()
k2modelSim=ode(y=N0,times=times,func=ddSim,parms=params)
#convert to datafrome for plotting purposes
k2modelOutput=data.frame(time=k2modelSim[,1],N=k2modelSim[,2])
#plot
k2=ggplot(k2modelOutput,aes(x=time,y=N))+geom_line()+theme_classic()

##third k value: 
params=c(0.2,100)
N0=1
times=1:100
##simulate model using ode()
k3modelSim=ode(y=N0,times=times,func=ddSim,parms=params)
#convert to datafrome for plotting purposes
k3modelOutput=data.frame(time=k3modelSim[,1],N=k3modelSim[,2])
#plot
k3=ggplot(k3modelOutput,aes(x=time,y=N))+geom_line()+theme_classic()


#to plot all on one graph,
#combine all modelOutputs into one dataframe
time=1:100
k1=c(k1modelOutput$N)
k2=c(k2modelOutput$N)
k3=c(k3modelOutput$N)

allk=data.frame(time,k1,k2,k3)

#plot the graph:
ggplot(data=allk) + geom_line(aes(x=time,y=k1),color="firebrick") + geom_line(aes(x=time,y=k2),color="royalblue")+ geom_line(aes(x=time,y=k3),color="mediumturquoise") + theme_classic()+ ylab("N")

