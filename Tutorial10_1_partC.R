#######QUESTION 1 part 3#############

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

#define parameters, initial values and time steps for first N0 value
params=c(0.1,50)
N0=1
times=1:100
##simulate model using ode()
n1modelSim=ode(y=N0,times=times,func=ddSim,parms=params)
#convert to datafrome for plotting purposes
n1modelOutput=data.frame(time=n1modelSim[,1],N=n1modelSim[,2])
#plot
n1=ggplot(n1modelOutput,aes(x=time,y=N))+geom_line()+theme_classic()

##second N0 value: 
params=c(0.1,50)
N0=50
times=1:100
##simulate model using ode()
n2modelSim=ode(y=N0,times=times,func=ddSim,parms=params)
#convert to datafrome for plotting purposes
n2modelOutput=data.frame(time=n2modelSim[,1],N=n2modelSim[,2])
#plot
n2=ggplot(n2modelOutput,aes(x=time,y=N))+geom_line()+theme_classic()

##third N0 value: 
params=c(0.1,50)
N0=100
times=1:100
##simulate model using ode()
n3modelSim=ode(y=N0,times=times,func=ddSim,parms=params)
#convert to datafrome for plotting purposes
n3modelOutput=data.frame(time=n3modelSim[,1],N=n3modelSim[,2])
#plot
n3=ggplot(n3modelOutput,aes(x=time,y=N))+geom_line()+theme_classic()


#to plot all on one graph,
#combine all modelOutputs into one dataframe
time=1:100
n1=c(n1modelOutput$N)
n2=c(n2modelOutput$N)
n3=c(n3modelOutput$N)

alln=data.frame(time,n1,n2,n3)

#plot the graph:
ggplot(data=alln) + geom_line(aes(x=time,y=n1),color="firebrick") + geom_line(aes(x=time,y=n2),color="royalblue")+ geom_line(aes(x=time,y=n3),color="mediumturquoise") + theme_classic()+ ylab("N")

