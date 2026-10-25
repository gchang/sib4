makeNCDFgrid <- function(
   fileName, fileText, fileSource,
   nlon, nlat, nlus, npfts, 
   lon, lat, pftnames, prefs, 
   pftref, pftarea, gppArray, respArray)
{


  # create and write the netCDF file -- ncdf4 version
  library(ncdf4)
  message(sprintf("Creating File: "))
  message(sprintf(" %s",fileName))

  # determine if writing stacked or unstacked PFTs
  if (nlus != npfts) {
     sflag <- TRUE 
  } else { sflag <- FALSE }

  # define dimensions
  nlondim <- ncdim_def("lon","",1:nlon,create_dimvar=FALSE)
  nlatdim <- ncdim_def("lat","",1:nlat,create_dimvar=FALSE)
  if (sflag) {
     nludim <- ncdim_def("nlu", "", 1:nlus,
                create_dimvar=FALSE)
  }
  npftdim <- ncdim_def("npft", "", 1:npfts,
             create_dimvar=FALSE)
  if (!sflag) {nludim <- npftdim}

  clendim <- ncdim_def("clen", "", 1:3,
             create_dimvar=FALSE)

  # define variables
  lon_def <- ncvar_def("lon","degrees_east",nlondim,
                       prec="float",longname="Longitude")
  lat_def <- ncvar_def("lat","degrees_north",nlatdim,
                       prec="float",longname="Latitude")
  pref_def <- ncvar_def("pft_refnums","unitless",list(npftdim),
                       prec="integer",longname="PFT Reference Numbers")
  pname_def <- ncvar_def("pft_names","unitless",list(clendim,npftdim),
                       prec="char",longname="PFT Name")
  if (sflag) {
     pft_def <- ncvar_def("lu_pref","unitless",list(nlondim,nlatdim,nludim),
                       prec="integer",longname="PFT Reference")
     parea_def <- ncvar_def("lu_area","unitless",list(nlondim,nlatdim,nludim),
                       prec="float",longname="PFT Fractional Area")
  } else {
     parea_def <- ncvar_def("pft_area","unitless",list(nlondim,nlatdim,nludim),
                       prec="float",longname="PFT Fractional Area")
  }

  gpp_def <- ncvar_def("betaGPP","unitless",list(nlondim,nlatdim,nludim),
                       prec="float",longname="GPP multiplier")
  resp_def <- ncvar_def("betaRESP","unitless",list(nlondim,nlatdim,nludim),
                       prec="float",longname="RESP multiplier")

  # create netCDF file
  if (sflag) {
     varlist <- list(lon_def, lat_def, pref_def, pname_def,
                     pft_def, parea_def, gpp_def, resp_def)
  } else {
     varlist <- list(lon_def, lat_def, pref_def, pname_def,
                     parea_def, gpp_def, resp_def)
  }
  ncout <- nc_create(fileName,varlist)


  # put variables
  ncvar_put(ncout,lon_def,lon)
  ncvar_put(ncout,lat_def,lat)
  ncvar_put(ncout,pref_def,prefs)
  ncvar_put(ncout,pname_def,pftnames)
  if (sflag) {
     dim(pftref) <- c(nlon,nlat,nlus)
     ncvar_put(ncout,pft_def,pftref)
  }

  dim(pftarea) <- c(nlon,nlat,nlus)
  ncvar_put(ncout,parea_def,pftarea)

  dim(gppArray) <- c(nlon,nlat,nlus)
  ncvar_put(ncout,gpp_def,gppArray)

  dim(respArray) <- c(nlon,nlat,nlus)
  ncvar_put(ncout,resp_def,respArray)


  # add global attributes
  ncatt_put(ncout,0,"description", fileText)
  ncatt_put(ncout,0,"source", fileSource)
  ncatt_put(ncout,0,"institution", "Colorado State University")
  history <- paste("Denning Research Group", date(), sep=", ")
  ncatt_put(ncout,0,"history",history)

  # close the file, writing data to disk
  nc_close(ncout)
}
