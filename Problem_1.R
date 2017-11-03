### Problem 1 ###

#Load Packages
library(deSolve)
library(ggplot2)

#Custom function
popSim=function(t,y,p){
  N=y
  r=p[1]
  K=p[2]

dNdt=r*(1-N/K)*N

return(list(dNdt))
}

#Putting r values into a dataframe for subsequent looping
rList=c(-0.1,0.1,0.4,0.8,1.0)

#For loop generating plot
