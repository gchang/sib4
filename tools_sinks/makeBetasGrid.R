makeBetasGrid <- function(filename, 
   respEff, CfertEff, AmerNfertEff, EurNfertEff, borWarmEff,
   outFile, outText, outSource, imageFile) 
{
  # Create multipliers for GPP and RESP in SiB fluxes 
  # that make a reasonable sink for use in OSSEs
  
  library("pracma")
  library("ncdf4")
  
  # Read the SiB info from gridded file
  ncfid <- nc_open(filename)
  
  # Retrieve latitudes and longitudes associated with each land point
  nlevs <- 1
  for(i in 1:ncfid$ndims) {
     dinfo <- ncfid$dim[[i]]
     #message(sprintf("  Dimension Name/Length: %s, %s",
     #          dinfo$name,dinfo$len))

     if (dinfo$name == "lon") {
          nlon <- dinfo$len  }
     if (dinfo$name == "lat") {
          nlat <- dinfo$len  }
     if (dinfo$name == "nlu") {
          nlevs <- dinfo$len }
     if (dinfo$name == "npft") {
          npfts <- dinfo$len  }
     if (dinfo$name == "time") {
          ntime <- dinfo$len }
  }
  nCells <- nlon * nlat
  message(sprintf("Number of Lon/Lat: %s %s",nlon,nlat))
  message(sprintf("Number of LUs/PFTs: %s %s",nlevs,npfts))
  message(sprintf("Number of Timesteps: %s",ntime))

  if (npfts == 15) {
      prefs <- c(1,2,4,5,8,11,12,13,14,15,17,18,20,22,24)
  } else { stop("Expecting 15 PFTs.") }

  # Retrieve latitudes, longitudes, PFTs,
  # and fluxes associated with each land point
  lon <- ncvar_get(ncfid, "lon")
  lat <- ncvar_get(ncfid, "lat")
  pftnames <- ncvar_get(ncfid, "pft_names")
 
  if (nlevs == 1) {
     pftareaorig <- ncvar_get(ncfid, "pft_area")
     pftarea <- array(data=0.0, dim=c(nlon,nlat,npfts))
     pftref <- array(data=0, dim=c(nlon,nlat,npfts))
     for (i in 1:nlon) {
         for (j in 1:nlat) {
             for (k in 1:npfts) {
                 if (pftareaorig[i,j,k] > 0.)  {
                     pftref[i,j,k] <- prefs[k] 
                     pftarea[i,j,k] <- 1.0 
                 }
              }
         }
     }
     nlevs <- npfts
  } else {
     pftareaorig <- ncvar_get(ncfid,"lu_area")
     pftarea <- pftareaorig
     pftref <- ncvar_get(ncfid,"lu_pref")
  }

  gpp <- ncvar_get(ncfid,"assim")
  #resp <- ncvar_get(ncfid,"resp_bal")
  resp <- ncvar_get(ncfid,"resp_tot")
  nc_close(ncfid)

  # Create vectors of variables
  geogrid <- meshgrid(lat, lon)
  vlats = as.vector(geogrid$X)
  vlons = as.vector(geogrid$Y)

  dim(pftareaorig) <- c(nCells, nlevs)
  dim(pftarea) <- c(nCells, nlevs)
  dim(pftref) <- c(nCells, nlevs)
  dim(gpp) <- c(nCells, nlevs, ntime)
  dim(resp) <- c(nCells, nlevs, ntime)

  # Fill the betas
  source("makeBetas.R")
  betaData <- makeBetas(nCells, nlevs,
        vlons, vlats, pftref, 
        respEff, CfertEff, AmerNfertEff, EurNfertEff, borWarmEff)
  betaGPP <- betaData$betaGPP
  betaRESP <- betaData$betaRESP
  
  # Calculate annual sink
  source("makeGlobalC.R")
  makeGlobalC(nCells, ntime, vlons, vlats, pftarea,
              betaGPP, betaRESP, gpp, resp, imageFile)

  # Write betas to file
  source("makeNCDFgrid.R")
  makeNCDFgrid(
       outFile, outText, outSource,
       nlon, nlat, nlevs, npfts, 
       lon, lat, pftnames, prefs,
       pftref, pftareaorig, 
       betaGPP, betaRESP)

}
