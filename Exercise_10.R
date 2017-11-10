######### Question 1 pseudo-code
# load packages
# define custom model function
# set a 'pool' of values for the parameters of interest
# create a dataframe to store model output
# using a for loop to simulate with the different values of parameters
  # of interest
# 3 different plots based on how we set r, K, and N_0

library(deSolve)
library(ggplot2)

######################## Part 1 - different r values

# Custom function
ddSim = function(t,y,p){
  N=y
  r=p[1]
  K=p[2]
  dNdt=r*(1-N/K)*N
  
  return(list(dNdt))
}

# Define parameters
r=c(-0.1,0.1,0.4,0.8,1.0)
N0=10
times=1:100

# Create output dataframe
modelOutput=data.frame(matrix(NA, ncol=6, nrow=length(times)))
colnames(modelOutput)=c("time", "P1", "P2","P3","P4","P5")
modelOutput[,1]=times

# For loop for the different r values
for(i in 1:length(r)){
  params=c(r[i],100)
  modelSim=ode(y=N0, times=times, func=ddSim, parms=params)
  modelOutput[,i+1]=modelSim[,2]
}

# Plot for 5 different r populations
ggplot() + geom_line(data=modelOutput,aes(x=time,y=P1)) +
  geom_line(data=modelOutput,aes(x=time,y=P2)) +
  geom_line(data=modelOutput,aes(x=time,y=P3)) +
  geom_line(data=modelOutput,aes(x=time,y=P4)) +
  geom_line(data=modelOutput,aes(x=time,y=P5)) +
  theme_classic() + xlab("Time") + ylab("Population Size") +
  ggtitle("The effect of different maximum growth rates on a population")


######################## Part 2 - different K values

# Custom function
ddSim_K = function(t,y,p){
  N=y
  K=p[1]
  r=p[2]
  dNdt=r*(1-N/K)*N
  
  return(list(dNdt))
}

# Define parameters
K=c(10,50,100)
N0=1
times=1:100

# Create output dataframe
modelOutput_K=data.frame(matrix(NA, ncol=4, nrow=length(times)))
colnames(modelOutput_K)=c("time", "P1", "P2","P3")
modelOutput_K[,1]=times

# For loop for the different K values
for(i in 1:length(K)){
  params=c(K[i],0.2)
  modelSim_K=ode(y=N0, times=times, func=ddSim_K, parms=params)
  modelOutput_K[,i+1]=modelSim_K[,2]
}

# Plot for 3 different K populations
ggplot() + geom_line(data=modelOutput_K,aes(x=time,y=P1)) +
  geom_line(data=modelOutput_K,aes(x=time,y=P2)) +
  geom_line(data=modelOutput_K,aes(x=time,y=P3)) +
  theme_classic() + xlab("Time") + ylab("Population Size") +
  ggtitle("The effect of different carrying capacities on a population")


######################## Part 3 - different N0 values

# Custom function
ddSim_N0 = function(t,y,p){
  N=y
  r=p[1]
  K=p[2]
  dNdt=r*(1-N/K)*N
  
  return(list(dNdt))
}

# Define parameters
params=c(0.1,50)
N0=c(1,50,100)
times=1:100

# Create output dataframe
modelOutput_N0=data.frame(matrix(NA, ncol=4, nrow=length(times)))
colnames(modelOutput_N0)=c("time", "P1", "P2","P3")
modelOutput_N0[,1]=times

# For loop for the 3 different N0 values
for(i in 1:length(N0)){
  N0=c(1,50,100)
  modelSim_N0=ode(y=N0[i], times=times, func=ddSim_N0, parms=params)
  modelOutput_N0[,i+1]=modelSim_N0[,2]
}

# Plot for 3 different N0 populations
ggplot() + geom_line(data=modelOutput_N0,aes(x=time,y=P1)) +
  geom_line(data=modelOutput_N0,aes(x=time,y=P2)) +
  geom_line(data=modelOutput_N0,aes(x=time,y=P3)) +
  theme_classic() + xlab("Time") + ylab("Population Size") +
  ggtitle("The effect of different initial population size on a population")


########## Question 2 pseudo-code
# start by drawing out a conceptual model
# running simulations
# data exploration - making plots of parameters of interest
  # scatter plots of difference response variables
    # different valeus of r - saturation curves, all starting and ending 
          # at the same place, but different slopes
    # different values of K - all starting at the same place, but ending 
          # in different places
    # different values of N_0 - all starting in different places, but 
          # ending at the same place
    # plot number of people (y-axis) vs time (x-axis)
          # Susceptible starts high and decreases over time
          # Infected increases, reaches max, then decreases
          # Recovered starts low and increases over time
          # max incidence is when S=I
          # max prevalence is when I is max
          # % affected is difference between R and I

## Question 2

library(deSolve)
library(ggplot2)
library(gridExtra)

sir <- function(t, y, p){
  S=y[1]
  I=y[2]
  R=y[3]
  B=p[1]
  l=p[2]
  
  dSdt = -B*I*S
  dIdt = (B*I*S)-(l*I)
  dRdt = l*I
  
  return(list(c(dSdt,dIdt,dRdt)))
}

params=c(0.0005,0.05)
init=c(999,1,0)
times=c(1:500)

modelsim = ode(y=init, times=times, func=sir, parms=params)
modeloutput1 = data.frame(time=modelsim[,1], S=(modelsim[,2]), I=(modelsim[,3]), R=(modelsim[,4]))
calcs = 
  a<-ggplot()+
  theme_classic()+
  geom_line(data=modeloutput,aes(x=modeloutput1$time,y=modeloutput1$S),color="red")+
  geom_line(data=modeloutput,aes(x=modeloutput1$time,y=modeloutput1$I),color="blue")+
  geom_line(data=modeloutput,aes(x=modeloutput1$time,y=modeloutput1$R),color="green")
a                         


params=c(0.005,0.5)
init=c(999,1,0)
times=c(1:500)

modelsim = ode(y=init, times=times, func=sir, parms=params)
modeloutput2 = data.frame(time=modelsim[,1], S=(modelsim[,2]), I=(modelsim[,3]), R=(modelsim[,4]))
b<-ggplot()+
  theme_classic()+
  geom_line(data=modeloutput,aes(x=modeloutput2$time,y=modeloutput2$S),color="red")+
  geom_line(data=modeloutput,aes(x=modeloutput2$time,y=modeloutput2$I),color="blue")+
  geom_line(data=modeloutput,aes(x=modeloutput2$time,y=modeloutput2$R),color="green")

b  

params=c(0.0001,0.1)
init=c(999,1,0)
times=c(1:500)

modelsim = ode(y=init, times=times, func=sir, parms=params)
modeloutput3 = data.frame(time=modelsim[,1], S=(modelsim[,2]), I=(modelsim[,3]), R=(modelsim[,4]))
c<-ggplot()+
  theme_classic()+
  geom_line(data=modeloutput,aes(x=modeloutput3$time,y=modeloutput3$S),color="red")+
  geom_line(data=modeloutput,aes(x=modeloutput3$time,y=modeloutput3$I),color="blue")+
  geom_line(data=modeloutput,aes(x=modeloutput3$time,y=modeloutput3$R),color="green")

c 

params=c(0.00005,0.1)
init=c(999,1,0)
times=c(1:500)

modelsim = ode(y=init, times=times, func=sir, parms=params)
modeloutput4 = data.frame(time=modelsim[,1], S=(modelsim[,2]), I=(modelsim[,3]), R=(modelsim[,4]))
d<-ggplot()+
  theme_classic()+
  geom_line(data=modeloutput,aes(x=modeloutput4$time,y=modeloutput4$S),color="red")+
  geom_line(data=modeloutput,aes(x=modeloutput4$time,y=modeloutput4$I),color="blue")+
  geom_line(data=modeloutput,aes(x=modeloutput4$time,y=modeloutput4$R),color="green")

d 

params=c(0.0001,0.05)
init=c(999,1,0)
times=c(1:500)

modelsim = ode(y=init, times=times, func=sir, parms=params)
modeloutput5 = data.frame(time=modelsim[,1], S=(modelsim[,2]), I=(modelsim[,3]), R=(modelsim[,4]))
e<-ggplot()+
  theme_classic()+
  geom_line(data=modeloutput,aes(x=modeloutput5$time,y=modeloutput5$S),color="red")+
  geom_line(data=modeloutput,aes(x=modeloutput5$time,y=modeloutput5$I),color="blue")+
  geom_line(data=modeloutput,aes(x=modeloutput5$time,y=modeloutput5$R),color="green")

e 

params=c(0.0002,0.05)
init=c(999,1,0)
times=c(1:500)

modelsim = ode(y=init, times=times, func=sir, parms=params)
modeloutput6 = data.frame(time=modelsim[,1], S=(modelsim[,2]), I=(modelsim[,3]), R=(modelsim[,4]))
f<-ggplot()+
  theme_classic()+
  geom_line(data=modeloutput,aes(x=modeloutput6$time,y=modeloutput6$S),color="red")+
  geom_line(data=modeloutput,aes(x=modeloutput6$time,y=modeloutput6$I),color="blue")+
  geom_line(data=modeloutput,aes(x=modeloutput6$time,y=modeloutput6$R),color="green")

f 

params=c(0.0001,0.06)
init=c(999,1,0)
times=c(1:500)

modelsim = ode(y=init, times=times, func=sir, parms=params)
modeloutput7 = data.frame(time=modelsim[,1], S=(modelsim[,2]), I=(modelsim[,3]), R=(modelsim[,4]))
g<-ggplot()+
  theme_classic()+
  geom_line(data=modeloutput,aes(x=modeloutput7$time,y=modeloutput7$S),color="red")+
  geom_line(data=modeloutput,aes(x=modeloutput7$time,y=modeloutput7$I),color="blue")+
  geom_line(data=modeloutput,aes(x=modeloutput7$time,y=modeloutput7$R),color="green")

g 


grid.arrange(a,b,c,d,e,f,g,nrow=4,ncol=2) 

# What patterns do we see in the results?
# In parameter sets 3 and 4 the infection never takes off because the population slow becomes resistant
# In parameter sets 1, 2, and the infection is really fast, with the population transitioning through the 
# SIR stages in the first 100 days
# In parameter sets 5 and 7 the infection is more drawn-out, with the number of suseptible within the
# population never reaching zero during 500 days


# Exploring basic reproduction number
beta<-c(0.0005,0.005,0.0001,0.00005,0.0001,0.0002,0.0001,0.5,0.9,0.1,0.1,0.00005)
gamma<-c(0.05,0.5,0.1,0.1,0.05,0.05,0.06,0.1,0.1,0.5,0.9,0.00005)
results<-matrix(0,nrow = length(beta),ncol = 3)
colnames(results)<-c("Beta","Gamma","BRN")

for (i in 1:length(beta)){
  params=c(beta[i],gamma[i])
  init=c(999,1,0)
  times=c(1:500)
  
  modelsim = ode(y=init, times=times, func=sir, parms=params)
  modeloutput = data.frame(time=modelsim[,1], S=(modelsim[,2]), I=(modelsim[,3]), R=(modelsim[,4]))
  results[i,1] = beta[i]
  results[i,2] = gamma[i]
  results[i,3] = (beta[i]*(modeloutput$S[i]+modeloutput$I[i]+modeloutput$R[i]))/gamma[i]
}








