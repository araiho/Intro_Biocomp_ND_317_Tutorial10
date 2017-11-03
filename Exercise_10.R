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

#Custom function
ddSim = function(t,y,p){
  N=y
  r=p[1:5]
  K=p[6]
  
  dNdt = matrix(NA,nrow=5,length(t))
  
  for(i in 1:length(r)){
    dNdt[i,]=r[i]*(1-N/K)*N
  }
  
  return(list(dNdt))
}

#Define parameters
params=c(-0.1,0.1,0.4,0.8,1.0,100)
N0=10
times=1:100

modelSim=ode(y=N0, times=times, func=ddSim, parms=params)

modelOutput=data.frame(time=modelSim[,1],N=modelSim[,2])

ggplot(modelOutput,aes(x=time,y=N))+geom_line()+theme_classic()





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




