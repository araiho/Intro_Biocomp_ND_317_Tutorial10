#mypath=file.path("C:", "cygwin64", "home", "Shane", "IBC2017ND", "Tutorial","tut10","Intro_Biocomp_ND_317_Tutotial10", "constant_Kgraphs", paste("rVal", rList[i], ".jpg", sep="_"))
#jpeg(file=mypath)
#mytitle=paste("Constant K with r Value ")



### Problem 1 ###

#Load Packages
library(deSolve)
library(ggplot2)
library(cowplot)
library(gridExtra)

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

#Creating initial plot
modelPopSim=ode(y=N0,times=times,func=popSim,parms=c(rList[1,1],K))
modelPopSimOutput=data.frame(time=modelPopSim[,1], N=modelPopSim[,2])

a=ggplot(modelPopSimOutput,aes(x=time,y=N))+geom_line()+theme_classic()

for (i in rList[1,2:5]){
  modelPopSim=ode(y=N0,times=times,func=popSim,parms=c(i,K))
  modelPopSimOutput=data.frame(time=modelPopSim[,1], N=modelPopSim[,2])
  
  b=ggplot(modelPopSimOutput,aes(x=time,y=N))+geom_line()+theme_classic()+labs(title=paste("Constant K with r Value "), rList[i], sep="_")
  a=grid.arrange(a,b,ncol=5)
}
