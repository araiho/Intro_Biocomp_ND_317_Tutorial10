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




