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

