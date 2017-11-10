### Problem 1 ###
#Run the creation of each graph seperately (some of the same objecty names are used form figure to figure
#and are thus overwritten). The legend ("Var2") for each figure is in the order given the exercise pdf, which is the same 
#order as the values in each of the "Lists" (rList, KList, NList). 


#Constant K, varying r

#Load Packages
library(deSolve)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(regexr)

#Custom function
popSim=function(t,y,p){
  N=y
  r=p[1]
  K=p[2]
  
  dNdt=r*(1-N/K)*N
  
  return(list(dNdt))
}

#Putting r values into a dataframe for subsequent looping
rVals=c(-0.1,0.1,0.4,0.8,1.0)
rList=array(data=rVals, dim=c(1,5))

#For loop generating plot
N0=10
K=100
times=1:100

modelPopSimOutput=matrix(nrow = length(times), ncol=(1+length(rList)))
modelPopSimOutput[,1]=c(1:100)

for (i in 1:length(rList[1,])){
  modelPopSim=ode(y=N0,times=times,func=popSim,parms=c(rList[i],K))
  modelPopSimOutput[,(1+i)]=modelPopSim[,2]
}

modelPopSimOutput_m=melt(modelPopSimOutput)

ggplot()+
  geom_line(data = modelPopSimOutput_m[modelPopSimOutput_m$Var2 != 1,], aes(x = Var1, y = value, group=Var2, color=factor(Var2)))+
  labs(title="Constant K with Varying r Values", x="time", y="N")

#Constant r, varying K

KVals=c(10,50,100)
KList=array(data=KVals, dim=c(1,3))

N0=1
r=0.2

modelPopSimOutput=matrix(nrow = length(times), ncol=(1+length(KList)))
modelPopSimOutput[,1]=c(1:100)

for (i in 1:length(KList[1,])){
  modelPopSim=ode(y=N0,times=times,func=popSim,parms=c(r,KList[i]))
  modelPopSimOutput[,(1+i)]=modelPopSim[,2]
}

modelPopSimOutput_m=melt(modelPopSimOutput)

ggplot()+
  geom_line(data = modelPopSimOutput_m[modelPopSimOutput_m$Var2 != 1,], aes(x = Var1, y = value, group=Var2, color=factor(Var2)))+
  labs(title="Constant r with Varying K Values", x="time", y="N")

#Varying N0

NVals=c(1,50,100)
NList=array(data=NVals, dim=c(1,3))

r=0.1
K=50

modelPopSimOutput=matrix(nrow = length(times), ncol=(1+length(NList)))
modelPopSimOutput[,1]=c(1:100)

for (i in 1:length(NList[1,])){
  modelPopSim=ode(y=NList[i],times=times,func=popSim,parms=c(r,K))
  modelPopSimOutput[,(1+i)]=modelPopSim[,2]
}

modelPopSimOutput_m=melt(modelPopSimOutput)

ggplot()+
  geom_line(data = modelPopSimOutput_m[modelPopSimOutput_m$Var2 != 1,], aes(x = Var1, y = value, group=Var2, color=factor(Var2)))+
  labs(title="Constant K and r with Varying N0 Values", x="time", y="N")





#### Problem 2 ####

###ODE Function
SIRmodel <- function (t,y,p){
  S = y[1]
  I = y[2] 
  R = y[3]
  
  beta = p[1]
  gamma = p[2]

  
  dS_dt = -beta*I*S
  dI_dt = beta*I*S - gamma*I
  dR_dt = gamma*I
  
  return(list(c(dS_dt, dI_dt, dR_dt)))
}

###Functions to compute desired quantities 
#Maximum Daily Incidence
MDI <- function(I){
  MDI <- I[2] - I[1] #Assume first incidence is maximum 
  for (i in 3:500){
    #Check if the next incidence is greater than current value for MDI
    if(I[i] - I[i-1] > MDI){ 
      MDI = I[i] - I[i-1]
    }
  }
  return(MDI)
}

#Maximum Daily Prevelance 
MDP <- function(S,I,R){
  MDP <- I[1]/(S[1]+I[1]+R[1])
  for (i in 2:500){
    #Check if the next incidence is greater than current value for MDI
    if(I[i]/(S[i] + I[i] + R[i]) > MDP){ 
      MDP <- I[i]/(S[i]+I[i]+R[i])
    }
  }
  return(MDP)
}

#Percent Affected
PA <- function(S,I,R){
  PA <- (I[500] + R[500])/(S[500] + I[500] + R[500])
  return(PA)
}

#Base Reproduction Number
BPN <- function(p,S,I,R){
  BPN <- (p[1]*(S[1] + I[1] + R[1]))/p[2]
  return(BPN)
}


#Initial Conditions and Timespan is the same for every run
N0 = c(999,1,0)
times = 1:500

###Solving ODE
#Case i corresponds to the ith row of the table

#Case 1
params1 = c(.0005,.05)
Case1 = ode(y=N0, times = times, func = SIRmodel, parms = params1)
modelOutput1 = data.frame(time=Case1[,1],S=Case1[,2],I=Case1[,3],R=Case1[,4])
attach(modelOutput1)
mdi <- MDI(I)
mdp <- MDP(S,I,R)
pa <- PA(S,I,R)
bpn <- BPN(params1,S,I,R)


RESULTS <- matrix(c(params1[1],params1[2],mdi,mdp,pa,bpn), ncol=6)
colnames(RESULTS) <- c("Beta","Gamma","Max Daily Incidence", "Max Daily Prevalence","Percent Affected","Basic Reproduction Number")
rownames(RESULTS) <- c("Case 1")
RESULTS <- as.table(RESULTS)

#Case 2 
params2 = c(.005,.5)
Case2 = ode(y=N0, times = times, func = SIRmodel, parms = params2)
modelOutput2 = data.frame(time=Case2[,1],S=Case2[,2],I=Case2[,3],R=Case2[,4])
attach(modelOutput2)
mdi <- MDI(I)
mdp <- MDP(S,I,R)
pa <- PA(S,I,R)
bpn <- BPN(params2,S,I,R)

RESULTS <- rbind(RESULTS, "Case 2" = c(params2[1],params2[2],mdi,mdp,pa,bpn))


#Case 3
params3 = c(.0001,.1)
Case3 = ode(y=N0, times = times, func = SIRmodel, parms = params3)
modelOutput3 = data.frame(time=Case3[,1],S=Case3[,2],I=Case3[,3],R=Case3[,4])
attach(modelOutput3)
mdi <- MDI(I)
mdp <- MDP(S,I,R)
pa <- PA(S,I,R)
bpn <- BPN(params3,S,I,R)

RESULTS <- rbind(RESULTS, "Case 3" = c(params3[1],params3[2],mdi,mdp,pa,bpn))

#Case 4
params4 = c(.00005,.1)
Case4 = ode(y=N0, times = times, func = SIRmodel, parms = params4)
modelOutput4 = data.frame(time=Case4[,1],S=Case4[,2],I=Case4[,3],R=Case4[,4])
attach(modelOutput4)
mdi <- MDI(I)
mdp <- MDP(S,I,R)
pa <- PA(S,I,R)
bpn <- BPN(params4,S,I,R)

RESULTS <- rbind(RESULTS, "Case 4" = c(params4[1],params4[2],mdi,mdp,pa,bpn))

#Case 5
params5 = c(.0001,.05)
Case5 = ode(y=N0, times = times, func = SIRmodel, parms = params5)
modelOutput5 = data.frame(time=Case5[,1],S=Case5[,2],I=Case5[,3],R=Case5[,4])
attach(modelOutput5)
mdi <- MDI(I)
mdp <- MDP(S,I,R)
pa <- PA(S,I,R)
bpn <- BPN(params5,S,I,R)

RESULTS <- rbind(RESULTS, "Case 5" = c(params5[1],params5[2],mdi,mdp,pa,bpn))

#Case 6
params6 = c(.0002,.05)
Case6 = ode(y=N0, times = times, func = SIRmodel, parms = params6)
modelOutput6 = data.frame(time=Case6[,1],S=Case6[,2],I=Case6[,3],R=Case6[,4])
attach(modelOutput6)
mdi <- MDI(I)
mdp <- MDP(S,I,R)
pa <- PA(S,I,R)
bpn <- BPN(params6,S,I,R)

RESULTS <- rbind(RESULTS, "Case 6" = c(params6[1],params6[1],mdi,mdp,pa,bpn))

#Case 7 
params7 = c(.0001,.06)
Case7 = ode(y=N0, times = times, func = SIRmodel, parms = params2)
modelOutput7 = data.frame(time=Case7[,1],S=Case7[,2],I=Case7[,3],R=Case7[,4])
attach(modelOutput7)
mdi <- MDI(I)
mdp <- MDP(S,I,R)
pa <- PA(S,I,R)
bpn <- BPN(params7,S,I,R)

RESULTS <- rbind(RESULTS, "Case 7" = c(params7[1],params7[2],mdi,mdp,pa,bpn))




###Plotting the results - add the number to "modelOutput" that you want to plot
#plot2 <- ggplot(modelOutput) + 
  #geom_line(aes(x = time, y = S), colour = "blue") + 
  #geom_line(aes(x = time, y = I), colour = "grey") + 
  #geom_line(aes(x = time, y = R), colour = "red")

