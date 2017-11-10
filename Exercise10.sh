###EXERCISE10###
###QUESTION 1###

#SITUATION1:

#Set y
y <- c(N=10)

#Set a sequence of r values
rvec=c(-0.1, 0.1, 0.4, 0.8, 1.0)

#Set times
times <- seq(0,100,1)

#Define a custom function
ddpg <- function(time,y,parms){
  with(as.list(c(y,parms)),{
    dNdt = r*N*(1-N/k)
    return(list(dNdt))
  })
}
##create an empty variable, in which we will append our results for each step in the loop
modelOutput<-NULL

#loop over rvec
for (r in rvec) {
  #set parameters
  parms=c(r, k=100)
  #Generate the results of the ode
  Results <- ode(y=y,times = times,func=ddpg, parms=parms)
  ##append them to model output
  modelOutput<- rbind(modelOutput, data.frame(time=Results[,1],N=Results[,2], col=r))
}
##plot the output
ggplot(modelOutput,aes(x=time,y=N, group=col, color=factor(col)))+geom_line()+theme_classic()


#SITUATION2:

#Set a vector of k values
kvec=c(10,50,100)

#Set times
times <- seq(0,100,1)

#Define a custom function
ddpg <- function(time,y,parms){
  with(as.list(c(y,parms)),{
    dNdt = r*N*(1-N/k)
    return(list(dNdt))
  })
}
##create an empty variable, in which we will append our results for each step in the loop
modelOutput<-NULL

#loop over rvec
for (k in kvec) {
  #set parameters
  parms=c(r=0.2,k)
  #Generate the results of the ode
  Results <- ode(y=y,times = times,func=ddpg, parms=parms)
  ##append them to model output
  modelOutput<- rbind(modelOutput, data.frame(time=Results[,1],N=Results[,2], col=k))
}
##plot the output
ggplot(modelOutput,aes(x=time,y=N, group=col, color=factor(col)))+geom_line()+theme_classic()


#SITUATION3: 

#Set a vector of N values
Nvec=c(10,50,100)

#Set times
times <- seq(0,10,1)

#Define a custom function
ddpg <- function(time,y,parms){
  N=y
  dNdt = r*N*(1-N/k)
  return(list(dNdt))
  
}
##create an empty variable, in which we will append our results for each step in the loop
modelOutput<-NULL

#loop over rvec
for (i in Nvec) {
  #set parameters
  parms=c(r=0.1,k=50)
  #Generate the results of the ode
  Results <- ode(y=i,times = times,func=ddpg, parms=parms)
  ##append them to model output
  modelOutput<- rbind(modelOutput, data.frame(time=Results[,1],N=Results[,2], col=i))
}
##plot the output
ggplot(modelOutput,aes(x=time,y=N, group=col, color=factor(col)))+geom_line()+theme_classic()


###QUESTION 2###

# Load packages
library(deSolve)
library(ggplot2)

# Create custom SIR function
SIR <- function(t, y, p){
  S = y[1]
  I = y[2]
  R = y[3]
  
  B = p[1]
  g = p[2]
  
  dSdt = -B*I*S
  dIdt = B*I*S-g*I
  dRdt = g*I
  return(list(c(dSdt,dIdt,dRdt)))
}

# Set inital conditions
initial = c(999,1,0)
times = 1:500

# Create dataframe of simulation parameters
beta = c(.0005, .005, .0001, .00005, .0001, .0002, .0001)
gamma = c(0.05, 0.5, .1, .1, .05, .05, .06)
parameters = data.frame(beta, gamma)

# Create dataframe of simulation answers
Answernames = c("Simulation", "Incidence", "Prevalence", "Percent", "BRN")
Answer = data.frame(matrix(data = NA, nrow = 7, ncol = 5))
colnames(Answer) = Answernames
Answer$Simulation = c(1:7)


for(x in 1:7){
  # Create Simulation
  simulation = ode(y=initial,times=times,func=SIR,parms=parameters[x,])
  sim =data.frame(data = simulation, nrow = 500, ncol = 4)
  names = c("Time", "Susceptible", "Infected", "Recovered")
  colnames(sim) = names
  
  # Create Plot
  ggplot(sim, aes(sim$Time)) +
    geom_line(aes(y=sim$Susceptible, color="mediumpurple3")) +
    geom_line(aes(y=sim$Infected, color="mediumvioletred")) +
    geom_line(aes(y=sim$Recovered, color="mediumseagreen")) +
    theme_classic() +
    xlab("Time (days)") +
    ylab("Number of Individuals") +
    scale_color_manual(labels=c("Susceptible", "Infected", "Recovered"), 
    values = c("mediumpurple3", "mediumvioletred", "mediumseagreen")) + 
    ggtitle("Simulation Plot") +
    theme(plot.title = element_text(hjust = 0.5))

  # Calculate Daily Incidence 
  Today = sim[-nrow(sim), 3]
  Tomorrow = sim[-1,3]
  Incidence = Tomorrow-Today
  
  Answer$Incidence[x] = max(Incidence)
  
  # Calculate Daily Prevalence
  Answer$Prevalence[x] = max((sim[,3]/(sim[,2] + sim[,3] + sim[,4])))
  
  # Calculate Percent Affected
  Answer$Percent[x] = (sim[500,3] + sim[500,4])/(sim[500,2] + sim[500,3] + sim[500,4])
  
  # Calculate Basic Reproduction Number (BRN)
  Answer$BRN[x] = (parameters[x,1]*(999+1+0))/parameters[x,2]

}

