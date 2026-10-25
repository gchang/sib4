makeGlobalC <- function(numv, numt,
    lonv, latv, pftareav, 
    betaGPP, betaRESP, gpp, resp,
    imageFile)
{

  totalGPPorig <- array(0.0, dim=c(numv,numt))
  totalRESPorig <- array(0.0, dim=c(numv,numt))
  totalNEEorig <- array(0.0, dim=c(numv,numt))
  totalGPP <- array(0.0, dim=c(numv,numt))
  totalRESP <- array(0.0, dim=c(numv,numt))
  totalNEE <- array(0.0, dim=c(numv,numt))

  for (time in 1:numt)
  {
      tmpGPP <- betaGPP * pftareav * gpp[,,time]
      tmpRESP <- betaRESP * pftareav * resp[,,time]

      oneTimeGPP <- rowSums(tmpGPP)
      oneTimeRESP <- rowSums(tmpRESP)
      oneTimeNEE <- oneTimeRESP - oneTimeGPP

      totalGPP[,time] <- oneTimeGPP
      totalRESP[,time] <- oneTimeRESP
      totalNEE[,time] <- oneTimeNEE

      tmpGPP <- pftareav * gpp[,,time]
      tmpRESP <- pftareav * resp[,,time]
      oneTimeGPP <- rowSums(tmpGPP)
      oneTimeRESP <- rowSums(tmpRESP)
      oneTimeNEE <- oneTimeRESP - oneTimeGPP
      totalGPPorig[,time] <- oneTimeGPP
      totalRESPorig[,time] <- oneTimeRESP
      totalNEEorig[,time] <- oneTimeNEE
  }
  
  # Calculate grid cell areas
  source("makeAreasVector.R")
  areav <- makeAreasVector(numv, lonv, latv)

  # Calculate annual C
  # ...conversion for umol C to g C
  cfac <- 12 / 1000000

  #...conversion for g C to Gt C
  gC.to.GtC <- 10^-15

  #...seconds in a day
  secPerDay <- 60 * 60 * 24

  #...seconds in each month
  days_per_month <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  secPerMonth <- days_per_month * secPerDay

  annualNEP <- array(0.0, dim=c(numv))
  checkNEEorig <- array(0.0, dim=c(numv,numt))
  checkGPPorig <- array(0.0, dim=c(numv,numt))
  checkRESPorig <- array(0.0, dim=c(numv,numt))
  checkNEE <- array(0.0, dim=c(numv,numt))
  checkGPP <- array(0.0, dim=c(numv,numt))
  checkRESP <- array(0.0, dim=c(numv,numt))

  for (time in 1:numt)
  {
     annualNEP <- annualNEP + totalNEE[,time] * days_per_month[time]

     checkNEE[,time] <- totalNEE[,time] * areav * cfac * secPerMonth[time]
     checkGPP[,time] <- totalGPP[,time] * areav * cfac * secPerMonth[time]
     checkRESP[,time] <- totalRESP[,time] * areav * cfac * secPerMonth[time]

     checkNEEorig[,time] <- totalNEEorig[,time] * areav * cfac * secPerMonth[time]
     checkGPPorig[,time] <- totalGPPorig[,time] * areav * cfac * secPerMonth[time]
     checkRESPorig[,time] <- totalRESPorig[,time] * areav * cfac * secPerMonth[time]
  }


  # Print messages
  finalCheckGPPorig <- sum( colSums(checkGPPorig))*gC.to.GtC
  finalCheckRESPorig <- sum( colSums(checkRESPorig))*gC.to.GtC
  message(sprintf("Original Global GPP/RESP (gtC/yr): %s %s",
           finalCheckGPPorig, finalCheckRESPorig))
  finalCheckNEEorig <- sum( colSums(checkNEEorig))*gC.to.GtC
  message(sprintf("Original Global NEE: %s",finalCheckNEEorig))

  finalCheckGPP <- sum( colSums(checkGPP))*gC.to.GtC
  finalCheckRESP <- sum( colSums(checkRESP))*gC.to.GtC
  message(sprintf("Global GPP/RESP (gtC/yr): %s %s",
           finalCheckGPP, finalCheckRESP))
  finalCheckNEE <- sum( colSums(checkNEE))*gC.to.GtC
  message(sprintf("Global NEE: %s",finalCheckNEE))

  diffNEE <- finalCheckNEEorig - finalCheckNEE
  message(sprintf("Annual Global Sink (gtC/yr): %s\n",diffNEE))


  # Plot annual map of sinks
  annualNEP <- annualNEP / -365.
  ptsRef <- which(annualNEP < 0.)
  ptsini <- length(ptsRef)
  for (np in 1:ptsini) {
      ns <- ptsRef[np]
      annualNEP[ns] <- 0.0
      #message(sprintf("Source of Carbon at Lon/Lat: %s",
      #        lonv[ns], latv[ns]))
      #message(sprintf("  betaGPP/betaRESP: %s  %s",
      #        betaGPP[ns],betaRESP[ns]))
  }

  source("plotVector.R")
  plotVector(lonv, latv, annualNEP, title="Annual Sink (uMol/m2/s)",
             filename=imageFile)

}