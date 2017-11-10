

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
