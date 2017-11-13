## Question 2

library(deSolve)


# SIR model
sir <- function(t, y, p){
  S=y[1]
  I=y[2]
  R=y[3]
  B=p[1]
  l=p[2]

  dSdt = -B*I*S
  dIdt = (B*I*S)-(l*I)
  dRdt = l*I
  
  return(list(c(dSdt,dIdt,dRdt)))
}



# Exploring with the given parameters, plus some additional

beta<-c(0.0005,0.005,0.0001,0.00005,0.0001,0.0002,0.0001,0.5,0.0000005,0.0000001,10,1)
gamma<-c(0.05,0.5,0.1,0.1,0.05,0.05,0.06,0.5,0.0000005,0.1,1,10)
results<-matrix(0,nrow = length(beta),ncol = 6)
colnames(results)<-c("Beta","Gamma","Max Daily Incidence","Max Daily Prevalence","Percent Affected","Basic Reproduction #")

for (i in 1:length(beta)){
  params=c(beta[i],gamma[i])
  init=c(999,1,0)
  times=c(1:500)
  
  modelsim = ode(y=init, times=times, func=sir, parms=params)
  modeloutput = data.frame(time=modelsim[,1], S=(modelsim[,2]), I=(modelsim[,3]), R=(modelsim[,4]))
  for (j in 1:length(modeloutput)){
    modeloutput$incidence[j] = modeloutput$I[j+1]-modeloutput$I[j]
    modeloutput$prevalence[j] = modeloutput$I[j]/(modeloutput$S[j]+modeloutput$I[j]+modeloutput$R[j])
    modeloutput$percentaffect[j] = (modeloutput$I[j]+modeloutput$R[j])/(modeloutput$S[j]+modeloutput$I[j]+modeloutput$R[j])
  }
  results[i,1] = beta[i]
  results[i,2] = gamma[i]
  results[i,3] = max(modeloutput$incidence)
  results[i,4] = max(modeloutput$prevalence)
  results[i,5] = modeloutput$percentaffect[500]
  results[i,6] = (beta[i]*(modeloutput$S[1]+modeloutput$I[1]+modeloutput$R[1]))/gamma[i]
}
results

