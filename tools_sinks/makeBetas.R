makeBetas <- function(nsib, nlevs, 
    lonarray, latarray, pftarray, 
    respEff, CfertEff, AmerNfertEff, EurNfertEff, borWarmEff) 
{
  
  # Create matrices to hold betas for GPP and RESP
  betaGPP <- array(data=0.0, dim=c(nsib, nlevs))
  betaRESP <- array(data=0.0, dim=c(nsib, nlevs))

  #################
  # CO2 fertilization enhances GPP everywhere except crops
  CO2fert <- which(pftarray < 17 & pftarray > 1)
  betaGPP[CO2fert] <- betaGPP[CO2fert] + CfertEff
  
  #################
  # Regrowing forests in Eastern USA have less respiration
  # Also N fertilization by atmospheric deposition (downwind of Ohio Valley)
  # Only in regrowing forests as defined above
  # Use respEff for reduction related to regrowing forests
  regrowth.lat <- 40
  regrowth.decorr <- 10

  # Use AmerNfertEff for increase due to N fertilization
  nitro.lon <- -79  # N-dep region centered at 79 W and 41 N
  nitro.lat <- 41
  decorr.lon <- 20  # decorrelation in lon
  decorr.lat <- 8   # decorrelation in lat
  for(ns in 1:nsib)
  {
      if ((lonarray[ns] > -100) & (lonarray[ns] < -55) &
          (latarray[ns] > 20))
      {
          pftsRG <- which(pftarray[ns,] > 1 & pftarray[ns,] < 13)
          pftFini <- length(pftsRG) 
      
          if (pftFini > 0)
          {
             for (pftID in pftsRG[1]:pftsRG[pftFini])
             {
                 # Respiration reduction for regrowing Eastern US forests
                 betaRESP[ns, pftID] <- betaRESP[ns, pftID] - respEff *
                    exp(-abs(latarray[ns]-regrowth.lat)/regrowth.decorr)

                 # Increase GPP due to N fertilization
                 betaGPP[ns, pftID] <- betaGPP[ns, pftID] + AmerNfertEff *
                    exp(-abs(lonarray[ns] - nitro.lon) / decorr.lon) *
                    exp(-abs(latarray[ns]-nitro.lat)/decorr.lat)
             }
          }
      }
      
   }
  
  #################
  # N fertilization by atmospheric deposition (downwind of Germany)
  # Use EurNfertEff for increase due to N fertilization
  nitro.lon <- 4    # N-dep region centered at 4 E and 49 N
  nitro.lat <- 49
  decorr.lon <- 30  # decorrelation in lon
  decorr.lat <- 24  # decorrelation in lat
  eurasianForests <- which(lonarray > -10 & lonarray < 65 & 
                           latarray > 30 & latarray < 65)
    
  for(ns in 1:nsib)
  {
      if ((lonarray[ns] > -10) & (lonarray[ns] < 65) &
          (latarray[ns] > 30) & (latarray[ns] < 65))
      {
         pftsEF <- which(pftarray[ns,] > 1 & pftarray[ns,] < 13)
         pftFini <- length(pftsEF)
      
         if (pftFini > 0)
         {
             for (pftID in pftsEF[1]:pftsEF[pftFini])
             {   
                # Increase GPP due to N fertilization
                betaGPP[ns, pftID] <- betaGPP[ns, pftID] + EurNfertEff *
                   exp(-abs(lonarray[ns] - nitro.lon) / decorr.lon) *
                   exp(-abs(latarray[ns]-nitro.lat)/decorr.lat)
             }
          }
       }
   }

  #################
  # Boreal warming: enhance GPP north of 60 N
  borealForests <- which(latarray > 40)
  boreal.lat <- 60    # boreal warming region centered at 60 N
  decorr.lat <- 15  # decorrelation in lat

  for(ns in 1:nsib)
  {
     pftsEF <- which(pftarray[ns,] > 1 & pftarray[ns,] < 17)
     pftFini <- length(pftsEF)
      
     if (pftFini > 0)
     {
       for (pftID in pftsEF[1]:pftsEF[pftFini])
       {   
        # Increase GPP due to warming
         betaGPP[ns, pftID] <- betaGPP[ns, pftID] + borWarmEff *
           exp(-abs(latarray[ns]-boreal.lat)/decorr.lat)
        }
      }
   }

  
  #################
  # Add 1 to all elements in beta matrices
  betaGPP <- betaGPP + 1.0
  betaRESP <- betaRESP + 1.0

  message(sprintf(""))
  message(sprintf("Min/Max betaGPP : %s %s",min(betaGPP),max(betaGPP)))
  message(sprintf("Min/Max betaRESP: %s %s",min(betaRESP),max(betaRESP)))

  return(list(betaGPP=betaGPP, betaRESP=betaRESP))  
  
}

