ddSim <- function (t, y, p){
  N = y
  r = p[1]
  K=p[2]
  
  dNdt = r*(1-N/K)*N
  return(list(dNdt))
}

##plot1
N0 = 10
times = 1:100
params <- list()
modelOutput<-list()
params[[1]] <- c(-0.1, 100)
params[[2]] <- c(0.1, 100)
params[[3]] <- c(0.4, 100)
params[[4]] <- c(0.8, 100)
params[[5]] <- c(1, 100)

for (i in 1:5){
modelSim = ode(y = N0, times = times, func = ddSim, parms = params[i])
modelOutput1[[i]]=data.frame(time=modelSim[,1], N = modelSim[,2])
}

##plot2
params <- NULL
params[[1]] <- c(0.2, 10)
params[[2]] <- c(0.2, 50)
params[[3]] <- c(0.2, 100)
N0 = 1

for (j in 1:3){
  modelSim = ode(y = N0, times = times, func = ddSim, parms = params[i])
  modelOutput2[[i]]=data.frame(time=modelSim[,1], N = modelSim[,2])
}

##plot3
Nnauts <- c(1, 50, 100)
params <- NULL
params <- c(0.1, 50)
for (k in 1:3){
  modelSim = ode(y = Nnauts[i], times = times, func = ddSim, parms = params)
  modelOutput3[[i]]=data.frame(time=modelSim[,1], N = modelSim[,2])
}
