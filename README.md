# Intro_Biocomp_ND_317_Tutorial10

#  Code used to generate different parameters of beta and gamma
modelSim=ode(y=initial,times=times,func=SIR,parms=c(beta, gamma))
Sim = data.frame(modelSim)
ggplot(Sim, aes(Sim$Time)) +
  geom_line(aes(y=Sim1$Susceptible, color="mediumpurple3")) +
  geom_line(aes(y=Sim1$Infected, color="mediumvioletred")) +
  geom_line(aes(y=Sim1$Recovered, color="mediumseagreen")) +
  theme_classic() +
  xlab("Time (days)") +
  ylab("Number of Individuals") +
  scale_color_manual(labels=c("Susceptible", "Infected", "Recovered"), 
                     values = c("mediumpurple3", "mediumvioletred", "mediumseagreen")) + 
  ggtitle("Simulation Plot") +
  theme(plot.title = element_text(hjust = 0.5))
  
  ## Trends seen
  # Increase in beta leads to faster increase to threshold capacity
  # Decrease in beta leads to slower rate of increase to threshold capacity
  # Beta is responsible for rate of change of infected and recovered
  # Examples seen in simulation comparisons of parameters 1 and 6
  
  # Increase in gamma leads to lower threshold capacity of infected and recovered
  # Decrease in gamma leads to lower threshold capacity of infected and recovered
  # Gamma is responsible for carrying capacity of they system
  # Examples seen in simulation comparisons of parameters 5 and 7
  
