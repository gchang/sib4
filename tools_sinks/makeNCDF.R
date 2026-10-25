makeNCDF <- function(fileName, varName, varLongName, varUnits, varArray, lon, lat, lev)
{
  # Required inputs - 
  #    filename:  Name of file to be created
  #    varName:   String containing name of the variable 
  #    varLongName: String containing descriptive name 
  #    varUnits:    String containing variable units 
  #    varArray:    Array containing values on grid
  #    lon :        Vector of longitudes - dimension
  #    lat :        Vector of latitudes - dimension
  #    time :       Vector of times - dimension
  #    data:      Data from the makeBetas function
  #                (e.g. out$totalGPP[,1] for a single time slice)
  #    title:     For the plot

  # create and write the netCDF file -- ncdf4 version
  library(ncdf4)

  # define dimensions
  londim <- ncdim_def("lon","degrees_east", as.double(lon)) 
  latdim <- ncdim_def("lat","degrees_north", as.double(lat)) 
  nludim <- ncdim_def("nlu","land unit", as.double(lev))

  # define variables
  fillvalue <- -99999
  dlname <- varLongName
  var_def <- ncvar_def(varName,varUnits,list(londim,latdim,nludim),fillvalue,dlname,prec="single")

  # create netCDF file and put arrays
  ncfname <- fileName
  ncout <- nc_create(ncfname,list(var_def),force_v4=T)

  # put variables
  ncvar_put(ncout,var_def,varArray)

  # put additional attributes into dimension and data variables
  ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
  ncatt_put(ncout,"lat","axis","Y")
  ncatt_put(ncout,"nlu","axis","Z")

  # add global attributes
  ncatt_put(ncout,0,"institution", "Colorado State University")
  history <- paste("Denning Research Group", date(), sep=", ")
  ncatt_put(ncout,0,"history",history)

  # close the file, writing data to disk
  nc_close(ncout)
}
