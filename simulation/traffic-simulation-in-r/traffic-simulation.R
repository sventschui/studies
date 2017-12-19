{
  hoursToDeltat <- function(h) {
    # 10th of a second
    h * 60 * 60 * 10
  }
  
  dt <- 0.1
  EA <- c(
    0.04, # Traffic of A1, 2/3 of EB[[1]]
    0.02, # Motorway entrace 60 Weiningen, 1/3 of EB[[1]]
    0.02  # Traffic of A53 Brütiseller Kreuz -> Sankt Gallen, 71k vehicles/day on A53 -> 0.04 vehicles/(s/10) in one direction -> 0.02 Vehicles/(s/10) take the Brütiseller Kreuz in direction Sankt Gallen
  )
  EB <- c(
    0.06, # Traffic Gubrist, 105k vehicles/day -> 0.06 Vehilces/(s/10) in one direction
    0.03, # Brütiseller Kreuz A1 direction of Gubrist to Zürcher Oberland, 71k Vehicles/Day on A53 -> 0.04 Vehicles/(s/10) in one direction -> 0.03 Fz/(s/10) from direction of Gubrist, others from direction of SG
    0.09 # 160k vehicles/day Brütiseller Kreuz Nord
  )
  pA <- 1-exp(-EA*dt)
  pB <- 1-exp(-EB*dt)
  pS <- c(0.6, 0.5, 0.5)
  pV <- c(2/3)
  
  m <- 20
  n <- hoursToDeltat(0.5) # hours simulation time
  allPaths <- c()
  
  # allPA = c()
  
  for (run in 1:m) {
    N <- list(0,0,0,0,0)

    for (t in 1:n) {
      N[[1]][t+1] <- N[[1]][t]
      N[[2]][t+1] <- N[[2]][t]
      N[[3]][t+1] <- N[[3]][t]
      N[[4]][t+1] <- N[[4]][t]
      N[[5]][t+1] <- N[[5]][t]
      
      # arrivals
      rand_pA_1 <- runif(1)
      rand_pA_2 <- runif(1)
      rand_pA_3 <- runif(1)
      # servings
      rand_pB_1 <- runif(1)
      rand_pB_2 <- runif(1)
      rand_pB_3 <- runif(1)
      # which Queue is served, if the server serves
      rand_pS_1 <- runif(1)
      rand_pS_3 <- runif(1)
      # which Queue is feeded, if the server serves
      rand_pV_1 <- runif(1)

      # S2 can serve AND did serve
      if (N[[3]][t] > 0 && rand_pB_2 < pB[2]) {
        N[[3]][t+1] <- N[[3]][t+1] - 1
      }
      
      # S3 can serve AND did serve
      if ((N[[5]][t] > 0 || N[[4]][t] > 0) && rand_pB_3 < pB[3]) {
        # can serve Q4 AND (did serve Q4 OR can not serve Q5)
        if (N[[4]][t] > 0 && (rand_pS_3 < pS[3] || N[[5]][t] == 0)) {
          N[[4]][t+1] <- N[[4]][t+1] - 1
        }
        
        # can serve Q5 AND (did serve Q5 OR can not serve Q4)
        if (N[[5]][t] > 0 && (rand_pS_3 >= pS[3] || N[[4]][t] == 0)) {
          N[[5]][t+1] <- N[[5]][t+1] - 1
        }
      }
      
      # S1 can serve AND did serve
      if ((N[[1]][t] > 0 || N[[2]][t] > 0) && rand_pB_1 < pB[1]) {
        # can serve Q1 AND (did serve Q1 OR can not serve Q2)
        if (N[[1]][t] > 0 && (rand_pS_1 < pS[1] || N[[2]][t] == 0)) {
          N[[1]][t+1] <- N[[1]][t+1] - 1
        }
        # can serve Q2 AND (did serve Q2 OR can not serve Q1)
        if (N[[2]][t] > 0 && (rand_pS_1 >= pS[1] || N[[1]][t] == 0)) {
          N[[2]][t+1] <- N[[2]][t+1] - 1
        }
        # served one goes to Q4
        if (rand_pV_1 < pV[1]) {
          N[[4]][t+1] <- N[[4]][t+1] + 1
        } else {
          # goes to Q3
          N[[3]][t+1] <- N[[3]][t+1] + 1
        }
      }
      
      # Q1 has arrival
      if (rand_pA_1 < pA[1]) {
        N[[1]][t+1] <- N[[1]][t+1] + 1
      }
      
      # Q2 has arrival
      if (rand_pA_2 < pA[2]) {
        N[[2]][t+1] <- N[[2]][t+1] + 1
      }
      
      # Q5 has arrival
      if (rand_pA_3 < pA[3]) {
        N[[5]][t+1] <- N[[5]][t+1] + 1
      }
    }
    
    allPaths <- c(allPaths, list(N))
  }
  
  {
    colors <- rainbow(m, v=0.8)
    for (queue in 1:5) {
      png(
        paste("~/cloudstation/cloud/hwz/Simulation/queue_", queue, "_paths.png", sep = ""),
        width = 2000,
        height = 1600,
        res = 300
      )
      # plot a chart of all paths
      y <- max(allPaths[[1]][[queue]])
      for (run in 2:m) {
        y <- max(y, max(allPaths[[run]][[queue]]))
      }

      ?png
        
      plot(
        main = "Simulationspfade",
        allPaths[[1]][[queue]], 
        type="l", 
        col=colors[1], 
        ylim = c(0,y),
        ylab = "Anzahl Fahrzeuge",
        xlab = "t [0.1s]"
      )
      for (run in 2:m) {
        lines(allPaths[[run]][[queue]], col=colors[run])
      }
      
      dev.off()
      
      png(
        paste("~/cloudstation/cloud/hwz/Simulation/queue_", queue, "_mean.png", sep = ""),
        width = 2000,
        height = 1600,
        res = 300
      )
      
      # plot mean and std derivation
      E <- c()
      Std <- c()
      
      for (t in 1:n) {
        itms <- c()
        for (run in 1:m) {
          itm <- allPaths[[run]][[queue]][t]
          itms <- c(itms, itm)
        }
        
        E = c(E, mean(itms))
        Std = c(Std, sqrt(var(itms)))
      }
      
      meanPlotY <- max(c(5, E, Std))
      
      plot(
        main = "Mittelwert und Standardabweichung",
        E, 
        type = "l", 
        col="green",
        ylim = c(0,meanPlotY),
        ylab = "Anzahl Fahrzeuge",
        xlab = "t [0.1s]"
      )
      legend(x = 2500, y=meanPlotY*0.9, legend=c(paste("E[N", queue, "(t)]", sep=""), paste("Std[N", queue, "(t)]", sep="")), col = c("green", "red"), lwd=2, cex=1, xjust=0.5, yjust=0.5)
      lines(Std, col="red")
      
      dev.off()
    }
  }
  
}



