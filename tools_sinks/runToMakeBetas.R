runToMakeBetas<- function()
{
  
  # Make sure we have these packages!
  library("pracma")
  library("ncdf4")
  library("gtools")
  library("sp")
  library("raster")
  library("rgdal")
  
  # Read spec file
  fin <- file("runToMakeBetas.txt",open="r")
  trash <- readLines(fin,1)
  trash <- readLines(fin,1)
  CfertEff <- as.numeric(trash)
  message(sprintf("CfertEff: %s",CfertEff))
  trash <- readLines(fin,1)
  trash <- readLines(fin,1)
  respEff <- as.numeric(trash)
  message(sprintf("respEff: %s",respEff))
  trash <- readLines(fin,1)
  trash <- readLines(fin,1)
  AmerNfertEff <- as.numeric(trash)
  message(sprintf("AmerNfertEff: %s",AmerNfertEff))
  trash <- readLines(fin,1)
  trash <- readLines(fin,1)
  EurNfertEff <- as.numeric(trash)
  message(sprintf("EurNfertEff: %s",EurNfertEff))
  trash <- readLines(fin,1)
  trash <- readLines(fin,1)
  borWarmEff <- as.numeric(trash)
  message(sprintf("borWarmEff: %s\n",borWarmEff))

  trash <- readLines(fin,1)
  trash <- readLines(fin,1)
  fileName <- trash[1]
  trash <- readLines(fin,1)
  trash <- readLines(fin,1)
  outFile <- trash[1]
  trash <- readLines(fin,1)
  trash <- readLines(fin,1)
  outText <- trash[1]
  trash <- readLines(fin,1)
  trash <- readLines(fin,1)
  outSource <- trash[1]
  trash <- readLines(fin,1)
  trash <- readLines(fin,1)
  saveImage <- trash[1]
  if (saveImage == "TRUE") {
      trash <- readLines(fin,1)
      trash <- readLines(fin,1)
      imageFile <- trash[1]
  } else {imageFile <- ""}
  close(fin)

  # FIND BETAS
  message("Opening File: ")
  message(sprintf(" %s",fileName))
  ncid <- nc_open(fileName)
  dimnames = names(ncid[['dim']])
  nc_close(ncid)

  if (dimnames[1] == 'landpoints') {
     source("makeBetasVector.R")
     makeBetasVector(fileName,
              respEff, CfertEff, AmerNfertEff,
              EurNfertEff, borWarmEff,
              outFile, outText, outSource,
              imageFile)
  } else {
     source("makeBetasGrid.R")
     makeBetasGrid(fileName,
              respEff, CfertEff, AmerNfertEff,
              EurNfertEff, borWarmEff,
              outFile, outText, outSource,
              imageFile)
  }
  message(sprintf("\nFinished Creating Sinks.\n"))
}

