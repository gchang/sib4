makeNCDFvector <- function(
   fileName, fileText, fileSource,
   npts, nlus, npfts, lonsib, latsib,
   pftnames, pftrefs, 
   pftref, pftarea, gppArray, respArray)
{

  # create and write the netCDF file -- ncdf4 version
  library(ncdf4)
  message(sprintf("Creating File: "))
  message(sprintf(" %s",fileName))

  # define dimensions
  nsibdim <- ncdim_def("landpoints", "", 1:npts,
             create_dimvar=FALSE) 
  nludim <- ncdim_def("nlu", "", 1:nlus,
             create_dimvar=FALSE)
  npftdim <- ncdim_def("npft", "", 1:npfts,
             create_dimvar=FALSE)
  clendim <- ncdim_def("clen", "", 1:3,
             create_dimvar=FALSE)

  # define variables
  lon_def <- ncvar_def("lonsib","degrees_east",nsibdim,
                       prec="float",longname="Point Longitude")
  lat_def <- ncvar_def("latsib","degrees_north",nsibdim,
                       prec="float",longname="Point Latitude")
  pref_def <- ncvar_def("pft_refnums","unitless",list(npftdim),
                       prec="integer",longname="PFT Reference Numbers")
  pname_def <- ncvar_def("pft_names","unitless",list(clendim,npftdim),
                       prec="char",longname="PFT Name")
  pft_def <- ncvar_def("lu_pref","unitless",list(nsibdim,nludim),
                       prec="integer",longname="PFT Reference")
  parea_def <- ncvar_def("lu_area","unitless",list(nsibdim,nludim),
                       prec="float",longname="PFT Fractional Area")
  gpp_def <- ncvar_def("betaGPP","unitless",list(nsibdim,nludim),
                       prec="float",longname="GPP multiplier")
  resp_def <- ncvar_def("betaRESP","unitless",list(nsibdim,nludim),
                       prec="float",longname="RESP multiplier")

  # create netCDF file and put arrays
  ncout <- nc_create(fileName,list(lon_def, lat_def,
            pref_def, pname_def, 
            pft_def, parea_def, 
            gpp_def, resp_def))

  # put variables
  ncvar_put(ncout,lon_def,lonsib)
  ncvar_put(ncout,lat_def,latsib)
  ncvar_put(ncout,pref_def,pftrefs)
  ncvar_put(ncout,pname_def,pftnames)
  ncvar_put(ncout,pft_def,pftref)
  ncvar_put(ncout,parea_def,pftarea)
  ncvar_put(ncout,gpp_def,gppArray)
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
