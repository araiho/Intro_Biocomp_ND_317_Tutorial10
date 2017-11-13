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


# SIR model
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



# Exploring with the given parameters, plus some additional

beta<-c(0.0005,0.005,0.0001,0.00005,0.0001,0.0002,0.0001,0.5,0.0000005,0.0000001,10,1)
gamma<-c(0.05,0.5,0.1,0.1,0.05,0.05,0.06,0.5,0.0000005,0.1,1,10)
results<-matrix(0,nrow = length(beta),ncol = 6)
colnames(results)<-c("Beta","Gamma","Max Daily Incidence","Max Daily Prevalence","Percent Affected","Basic Reproduction #")

for (i in 1:length(beta)){
  params=c(beta[i],gamma[i])
  init=c(999,1,0)
  times=c(1:500)
  
  modelsim = ode(y=init, times=times, func=sir, parms=params)
  modeloutput = data.frame(time=modelsim[,1], S=(modelsim[,2]), I=(modelsim[,3]), R=(modelsim[,4]))
  for (j in 1:length(modeloutput)){
    modeloutput$incidence[j] = modeloutput$I[j+1]-modeloutput$I[j]
    modeloutput$prevalence[j] = modeloutput$I[j]/(modeloutput$S[j]+modeloutput$I[j]+modeloutput$R[j])
    modeloutput$percentaffect[j] = (modeloutput$I[j]+modeloutput$R[j])/(modeloutput$S[j]+modeloutput$I[j]+modeloutput$R[j])
  }
  results[i,1] = beta[i]
  results[i,2] = gamma[i]
  results[i,3] = max(modeloutput$incidence)
  results[i,4] = max(modeloutput$prevalence)
  results[i,5] = modeloutput$percentaffect[500]
  results[i,6] = (beta[i]*(modeloutput$S[1]+modeloutput$I[1]+modeloutput$R[1]))/gamma[i]
}
results












