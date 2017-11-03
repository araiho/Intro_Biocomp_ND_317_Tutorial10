#######QUESTION 1 part 1#############

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

#define parameters, initial values and time steps for first r value
params=c(-0.1,100)
N0=10
times=1:100
##simulate model using ode()
r1modelSim=ode(y=N0,times=times,func=ddSim,parms=params)
#convert to datafrome for plotting purposes
r1modelOutput=data.frame(time=r1modelSim[,1],N=r1modelSim[,2])
#plot
r1=ggplot(r1modelOutput,aes(x=time,y=N))+geom_line()+theme_classic()

##second r value: 
params=c(0.1,100)
N0=10
times=1:100
##simulate model using ode()
r2modelSim=ode(y=N0,times=times,func=ddSim,parms=params)
#convert to datafrome for plotting purposes
r2modelOutput=data.frame(time=r2modelSim[,1],N=r2modelSim[,2])
#plot
r2=ggplot(r2modelOutput,aes(x=time,y=N))+geom_line()+theme_classic()

##third r value: 
params=c(0.4,100)
N0=10
times=1:100
##simulate model using ode()
r3modelSim=ode(y=N0,times=times,func=ddSim,parms=params)
#convert to datafrome for plotting purposes
r3modelOutput=data.frame(time=r3modelSim[,1],N=r3modelSim[,2])
#plot
r3=ggplot(r3modelOutput,aes(x=time,y=N))+geom_line()+theme_classic()

##fourth r value: 
params=c(0.8,100)
N0=10
times=1:100
##simulate model using ode()
r4modelSim=ode(y=N0,times=times,func=ddSim,parms=params)
#convert to datafrome for plotting purposes
r4modelOutput=data.frame(time=r4modelSim[,1],N=r4modelSim[,2])
#plot
r4=ggplot(r4modelOutput,aes(x=time,y=N))+geom_line()+theme_classic()

##fifth r value: 
params=c(1.0,100)
N0=10
times=1:100
##simulate model using ode()
r5modelSim=ode(y=N0,times=times,func=ddSim,parms=params)
#convert to datafrome for plotting purposes
r5modelOutput=data.frame(time=r5modelSim[,1],N=r5modelSim[,2])
#plot
r5=ggplot(r5modelOutput,aes(x=time,y=N))+geom_line()+theme_classic()

#to plot all on one graph,
#combine all modelOutputs into one dataframe
time=1:100
r1=c(r1modelOutput$N)
r2=c(r2modelOutput$N)
r3=c(r3modelOutput$N)
r4=c(r4modelOutput$N)
r5=c(r5modelOutput$N)
allr=data.frame(time,r1,r2,r3,r4,r5)

#plot the graph:
ggplot(data=allr) + geom_line(aes(x=time,y=r1),color="firebrick") + geom_line(aes(x=time,y=r2),color="royalblue")+ geom_line(aes(x=time,y=r3),color="mediumturquoise") +geom_line(aes(x=time,y=r4),color="forestgreen") +geom_line(aes(x=time,y=r5),color="darkorchid") + theme_classic()+ ylab("N")

