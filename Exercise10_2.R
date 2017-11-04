sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I
    dI <- beta * S * I - gamma * I
    dR <- gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}
#set up various scenarios
init <- c(S = 999, I = 1, 0.0)
params<-list()
params<-NULL
params[[1]] <- c(beta = 0.0005, gamma =  0.05)
params[[2]] <- c(beta = 0.005,gamma =  0.5)
params[[3]] <- c(beta = 0.0001,gamma =  0.1)
params[[4]] <- c(beta = 0.00005,gamma =  0.1)
params[[5]] <- c(beta = 0.0001,gamma =  0.05)
params[[6]] <- c(beta = 0.0002,gamma =  0.05)
params[[7]] <- c(beta = 0.0001,gamma =  0.06)

times <- seq(0, 500, by = 1)
out <-list()
out<-NULL

for (i in 1:7){
out[[i]] <- as.data.frame(ode(y = init, times = times, func = sir, parms = params[[i]]))
out[[i]]$time <- NULL
daily_incidences <- NULL
daily_prevalence <- NULL
for (j in 2:500){
  temp_DI <- out[[i]]$I[j]-out[[i]]$I[j-1]
  daily_incidences <- c(daily_incidences, temp_DI)
  temp_DP <- (out[[i]]$I[j]/(out[[i]]$I[j]+out[[i]]$S[j]+out[[i]]$V4[j]))
  daily_prevalence <- c(daily_prevalence, temp_DP)
}
#calculate disease epidemic quantities
MDI <- signif(max(daily_incidences, na.rm = FALSE), 2)
MDP <- signif(max(daily_prevalence, na.rm = FALSE),2)
PA <- signif((out[[i]]$I[500]+out[[i]]$V4[500])/(out[[i]]$S[500]+out[[i]]$I[500]+out[[i]]$V4[500]),4)
BRN <- signif((params[[i]][1]*(out[[i]]$S[1]+out[[i]]$I[1]+out[[i]]$V4[1]))/params[[i]][2],10)

#plots
matplot(times, out[[i]], type = "l", xlab = "Time", ylab = "Susceptibles and Recovereds", main = paste0("SIR Model with beta ",params[[i]][1], " and gamma ", params[[i]][2]), lwd = 1, lty = 1, bty = "l", col = 2:4)
legend(300, 600, c("Susceptibles", "Infecteds", "Recovereds", 
                   paste0("Max Daily Incidence: ",MDI), paste0("Max Daily prevalence: ",MDP), paste0("% affected: ",PA*100, "%"), paste0("Basic Reproduction Number: ", BRN)), 
                    pch = c(NA, NA, NA, 21, 22 ,23, 24), pt.bg = c(NA,NA,NA,"purple", "orange", "pink", "yellow"),
                    lty = c(1,1,1,NA,NA,NA,NA), col = c("red", "green", "blue", "purple", "orange", "pink", "yellow"), 
                    cex = 0.75)

}
#increasing gamma causes the basic reproduction number to decrease and the number of infecteds to decrease.
#increasing beta causes the basic reproduction number to increase and the rate of infection to increase