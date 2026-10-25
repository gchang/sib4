makeBetasVector <- function(filename, 
    respEff, CfertEff, AmerNfertEff, EurNfertEff, borWarmEff,
    outFile, outText, outSource, imageFile)
{
  # Create multipliers for GPP and RESP in SiB fluxes
  #  that make reasonable sinks for use in OSSEs
  
  library("pracma")
  library("ncdf4")
  
  # Read the SiB info from vector file
  ncfid <- nc_open(filename)
  
  # Retrieve dimensions
  ptsDim <- ncfid$dim[["landpoints"]]
  nsib <- ptsDim$len
  levelDim <- ncfid$dim[["nlu"]]
  nlevs <- levelDim$len
  pftDim <- ncfid$dim[["npft"]]
  npfts <- pftDim$len
  timeDim <- ncfid$dim[["time"]]
  ntime <- timeDim$len
  message(sprintf("Number Of Points: %s", nsib))
  message(sprintf("Number of LUs/PFTs: %s  %s",nlevs,npfts))
  message(sprintf("Number of Timesteps: %s",ntime))

  # Retrieve latitudes, longitudes, PFTs,
  # and fluxes associated with each land point
  latsib <- ncvar_get(ncfid,"latsib")
  lonsib <- ncvar_get(ncfid,"lonsib")
  pftnames <- ncvar_get(ncfid,"pft_names")
  pftrefs <- ncvar_get(ncfid,"pft_refnums")
  pftsib <- ncvar_get(ncfid,"lu_pref")
  pftareasib <- ncvar_get(ncfid,"lu_area")
  gpp <- ncvar_get(ncfid,"assim")
  #resp <- ncvar_get(ncfid,"resp_bal")
  resp <- ncvar_get(ncfid,"resp_tot")
  nc_close(ncfid)

  # Fill the betas
  source("makeBetas.R")
  betaData <- makeBetas(nsib, nlevs,
    lonsib, latsib, pftsib,
    respEff, CfertEff, AmerNfertEff, EurNfertEff, borWarmEff)
  betaGPP <- betaData$betaGPP
  betaRESP <- betaData$betaRESP
  
  # Calculate annual sink
  source("makeGlobalC.R")
  makeGlobalC(nsib, ntime, lonsib, latsib, pftareasib,
              betaGPP, betaRESP, gpp, resp, imageFile)

 # Write betas to file
 source("makeNCDFvector.R")
 message("")
 makeNCDFvector(outFile, outText, outSource,
       nsib, nlevs, npfts,
       lonsib, latsib, pftnames, pftrefs, 
       pftsib, pftareasib, 
       betaGPP, betaRESP)

}
