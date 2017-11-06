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


# Create simulation
modelSim1=ode(y=initial,times=times,func=SIR,parms=parameters[1,])
Sim1 = data.frame(modelSim1)
modelSim2=ode(y=initial,times=times,func=SIR,parms=parameters[2,])
Sim2 = data.frame(modelSim2)
modelSim3=ode(y=initial,times=times,func=SIR,parms=parameters[3,])
Sim3 = data.frame(modelSim3)
modelSim4=ode(y=initial,times=times,func=SIR,parms=parameters[4,])
Sim4 = data.frame(modelSim4)
modelSim5=ode(y=initial,times=times,func=SIR,parms=parameters[5,])
Sim5 = data.frame(modelSim5)
modelSim6=ode(y=initial,times=times,func=SIR,parms=parameters[6,])
Sim6 = data.frame(modelSim6)
modelSim7=ode(y=initial,times=times,func=SIR,parms=parameters[7,])
Sim7 = data.frame(modelSim7)

# Change header titles
names = c("Time", "Susceptible", "Infected", "Recovered")
colnames(Sim1) = names
colnames(Sim2) = names
colnames(Sim3) = names
colnames(Sim4) = names
colnames(Sim5) = names
colnames(Sim6) = names
colnames(Sim7) = names

# Plot Simulations
ggplot(Sim1, aes(Sim1$Time)) +
  geom_line(aes(y=Sim1$Susceptible, color="mediumpurple3")) +
  geom_line(aes(y=Sim1$Infected, color="mediumvioletred")) +
  geom_line(aes(y=Sim1$Recovered, color="mediumseagreen")) +
  theme_classic() +
  xlab("Time (days)") +
  ylab("Number of Individuals") +
  scale_color_manual(labels=c("Susceptible", "Infected", "Recovered"), 
  values = c("mediumpurple3", "mediumvioletred", "mediumseagreen")) + 
  ggtitle("Simulation 1 Plot") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Sim1, aes(Sim2$Time)) +
  geom_line(aes(y=Sim2$Susceptible, color="mediumpurple3")) +
  geom_line(aes(y=Sim2$Infected, color="mediumvioletred")) +
  geom_line(aes(y=Sim2$Recovered, color="mediumseagreen")) +
  theme_classic() +
  xlab("Time (days)") +
  ylab("Number of Individuals") +
  scale_color_manual(labels=c("Susceptible", "Infected", "Recovered"), 
  values = c("mediumpurple3", "mediumvioletred", "mediumseagreen")) + 
  ggtitle("Simulation 2 Plot") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Sim1, aes(Sim3$Time)) +
  geom_line(aes(y=Sim3$Susceptible, color="mediumpurple3")) +
  geom_line(aes(y=Sim3$Infected, color="mediumvioletred")) +
  geom_line(aes(y=Sim3$Recovered, color="mediumseagreen")) +
  theme_classic() +
  xlab("Time (days)") +
  ylab("Number of Individuals") +
  scale_color_manual(labels=c("Susceptible", "Infected", "Recovered"), 
  values = c("mediumpurple3", "mediumvioletred", "mediumseagreen")) + 
  ggtitle("Simulation 3 Plot") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Sim1, aes(Sim4$Time)) +
  geom_line(aes(y=Sim4$Susceptible, color="mediumpurple3")) +
  geom_line(aes(y=Sim4$Infected, color="mediumvioletred")) +
  geom_line(aes(y=Sim4$Recovered, color="mediumseagreen")) +
  theme_classic() +
  xlab("Time (days)") +
  ylab("Number of Individuals") +
  scale_color_manual(labels=c("Susceptible", "Infected", "Recovered"), 
  values = c("mediumpurple3", "mediumvioletred", "mediumseagreen")) + 
  ggtitle("Simulation 4 Plot") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Sim1, aes(Sim5$Time)) +
  geom_line(aes(y=Sim5$Susceptible, color="mediumpurple3")) +
  geom_line(aes(y=Sim5$Infected, color="mediumvioletred")) +
  geom_line(aes(y=Sim5$Recovered, color="mediumseagreen")) +
  theme_classic() +
  xlab("Time (days)") +
  ylab("Number of Individuals") +
  scale_color_manual(labels=c("Susceptible", "Infected", "Recovered"), 
  values = c("mediumpurple3", "mediumvioletred", "mediumseagreen")) + 
  ggtitle("Simulation 5 Plot") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Sim1, aes(Sim6$Time)) +
  geom_line(aes(y=Sim6$Susceptible, color="mediumpurple3")) +
  geom_line(aes(y=Sim6$Infected, color="mediumvioletred")) +
  geom_line(aes(y=Sim6$Recovered, color="mediumseagreen")) +
  theme_classic() +
  xlab("Time (days)") +
  ylab("Number of Individuals") +
  scale_color_manual(labels=c("Susceptible", "Infected", "Recovered"), 
                     values = c("mediumpurple3", "mediumvioletred", 
"mediumseagreen")) + 
  ggtitle("Simulation 6 Plot") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Sim1, aes(Sim7$Time)) +
  geom_line(aes(y=Sim7$Susceptible, color="mediumpurple3")) +
  geom_line(aes(y=Sim7$Infected, color="mediumvioletred")) +
  geom_line(aes(y=Sim7$Recovered, color="mediumseagreen")) +
  theme_classic() +
  xlab("Time (days)") +
  ylab("Number of Individuals") +
  scale_color_manual(labels=c("Susceptible", "Infected", "Recovered"), 
  values = c("mediumpurple3", "mediumvioletred", "mediumseagreen")) + 
  ggtitle("Simulation 7 Plot") +
  theme(plot.title = element_text(hjust = 0.5))

# Creating datadrame for daily calculations
DCnames = c("Incidence", "Prevalence")

DC1 = data.frame(matrix(data = NA, nrow = 500, ncol = 2))
DC2 = data.frame(matrix(data = NA, nrow = 500, ncol = 2))
DC3 = data.frame(matrix(data = NA, nrow = 500, ncol = 2))
DC4 = data.frame(matrix(data = NA, nrow = 500, ncol = 2))
DC5 = data.frame(matrix(data = NA, nrow = 500, ncol = 2))
DC6 = data.frame(matrix(data = NA, nrow = 500, ncol = 2))
DC7 = data.frame(matrix(data = NA, nrow = 500, ncol = 2))

colnames(DC1) = DCnames
colnames(DC2) = DCnames
colnames(DC3) = DCnames
colnames(DC4) = DCnames
colnames(DC5) = DCnames
colnames(DC6) = DCnames
colnames(DC7) = DCnames

# Calculate maximum daily incidence
for (x in 1:nrow(Sim1)){
  incidence = Sim1$Infected[x+1]-Sim1$Infected[x]
  DC1$Incidence[x] = c(incidence)
}
for (x in 1:nrow(Sim2)){
  incidence = Sim2$Infected[x+1]-Sim2$Infected[x]
  DC2$Incidence[x] = c(incidence)
}
for (x in 1:nrow(Sim3)){
  incidence = Sim3$Infected[x+1]-Sim3$Infected[x]
  DC3$Incidence[x] = c(incidence)
}
for (x in 1:nrow(Sim4)){
  incidence = Sim4$Infected[x+1]-Sim4$Infected[x]
  DC4$Incidence[x] = c(incidence)
}
for (x in 1:nrow(Sim5)){
  incidence = Sim5$Infected[x+1]-Sim5$Infected[x]
  DC5$Incidence[x] = c(incidence)
}
for (x in 1:nrow(Sim6)){
  incidence = Sim6$Infected[x+1]-Sim6$Infected[x]
  DC6$Incidence[x] = c(incidence)
}
for (x in 1:nrow(Sim7)){
  incidence = Sim7$Infected[x+1]-Sim7$Infected[x]
  DC7$Incidence[x] = c(incidence)
}

# Maximum daily prevalence
for (x in 1:nrow(Sim1)){
  DC1$Prevalence[x] = Sim1$Infected[x]/(Sim1$Susceptible[x] + 
Sim1$Infected[x] + Sim1$Recovered[x])
}
for (x in 1:nrow(Sim2)){
  DC2$Prevalence[x] = Sim2$Infected[x]/(Sim2$Susceptible[x] + 
Sim2$Infected[x] + Sim2$Recovered[x])
}
for (x in 1:nrow(Sim3)){
  DC3$Prevalence[x] = Sim3$Infected[x]/(Sim3$Susceptible[x] + 
Sim3$Infected[x] + Sim3$Recovered[x])
}
for (x in 1:nrow(Sim4)){
  DC4$Prevalence[x] = Sim4$Infected[x]/(Sim4$Susceptible[x] + 
Sim4$Infected[x] + Sim4$Recovered[x])
}
for (x in 1:nrow(Sim5)){
  DC5$Prevalence[x] = Sim5$Infected[x]/(Sim5$Susceptible[x] + 
Sim5$Infected[x] + Sim5$Recovered[x])
}
for (x in 1:nrow(Sim6)){
  DC6$Prevalence[x] = Sim6$Infected[x]/(Sim6$Susceptible[x] + 
Sim6$Infected[x] + Sim6$Recovered[x])
}
for (x in 1:nrow(Sim7)){
  DC7$Prevalence[x] = Sim7$Infected[x]/(Sim7$Susceptible[x] + 
Sim7$Infected[x] + Sim7$Recovered[x])
}

# Maximums
MAXnames = c("Simulation", "Incidence", "Prevalence")
MAX = data.frame(matrix(data = NA, nrow = 7, ncol = 3))
colnames(MAX) = MAXnames
MAX$Simulation = c(1:7)

## Can't get max to work
MAX[1,2] = max(DC1$Incidence) 
MAX[2,2] = max(DC2$Incidence) 
MAX[3,2] = max(DC3$Incidence) 
MAX[4,2] = max(DC4$Incidence) 
MAX[5,2] = max(DC5$Incidence) 
MAX[6,2] = max(DC6$Incidence) 
MAX[7,2] = max(DC7$Incidence)


MAX[1,3] = max(DC1$Prevalence) 
MAX[2,3] = max(DC2$Prevalence) 
MAX[3,3] = max(DC3$Prevalence) 
MAX[4,3] = max(DC4$Prevalence) 
MAX[5,3] = max(DC5$Prevalence) 
MAX[6,3] = max(DC6$Prevalence) 
MAX[7,3] = max(DC7$Prevalence)

# Create dataframe for global calculations
GCnames = c("Simulation", "Percent", "BRN")
Global = data.frame(matrix(data = NA, nrow = 7, ncol = 3))
colnames(Global) = GCnames
Global$Simulation = c(1:7)

# Percent affected over the simulation
Global[1,2] = (Sim1[500,3] + Sim1[500,4])/(Sim1[500,2] + Sim1[500,3] + 
Sim1[500,4])
Global[2,2] = (Sim2[500,3] + Sim2[500,4])/(Sim2[500,2] + Sim2[500,3] + 
Sim2[500,4])
Global[3,2] = (Sim3[500,3] + Sim3[500,4])/(Sim3[500,2] + Sim3[500,3] + 
Sim3[500,4])
Global[4,2] = (Sim4[500,3] + Sim4[500,4])/(Sim4[500,2] + Sim4[500,3] + 
Sim4[500,4])
Global[5,2] = (Sim5[500,3] + Sim5[500,4])/(Sim5[500,2] + Sim5[500,3] + 
Sim5[500,4])
Global[6,2] = (Sim6[500,3] + Sim6[500,4])/(Sim6[500,2] + Sim6[500,3] + 
Sim6[500,4])
Global[7,2] = (Sim7[500,3] + Sim7[500,4])/(Sim7[500,2] + Sim7[500,3] + 
Sim7[500,4])

# Basic Reproduction number
Global[1,3] = (parameters[1,1]*(999+1+0))/parameters[1,2]
Global[2,3] = (parameters[2,1]*(999+1+0))/parameters[2,2]
Global[3,3] = (parameters[3,1]*(999+1+0))/parameters[3,2]
Global[4,3] = (parameters[4,1]*(999+1+0))/parameters[4,2]
Global[5,3] = (parameters[5,1]*(999+1+0))/parameters[5,2]
Global[6,3] = (parameters[6,1]*(999+1+0))/parameters[6,2]
Global[7,3] = (parameters[7,1]*(999+1+0))/parameters[7,2]


