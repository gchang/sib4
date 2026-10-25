plotVector <- function (lons, lats, data, title='', filename='') 
{

  # Libraries
  library(ncdf4)
  library(sp)
  library(raster)
  library(rgdal)

  ################
  # Using sp, rgdal, and raster libraries create plot
  df = data.frame(lons, lats, data)
  coordinates(df)=~lons+lats
  proj4string(df)=CRS("+init=epsg:4326")
  transDF = spTransform(df,CRS(proj4string(df)))

  # This will generate a warning you can ignore
  gridded(transDF) <- TRUE
  
  # Convert to raster and plot
  r=raster(transDF)
  X11.options(width=8, height=5, xpos=-100)
  dev.new()
  plot(r, main=title, xlab="Longitude", ylab="Latitude",
      xlim=c(-180,180), ylim=c(-90,90), 
      xaxs="i", yaxs="i", yaxt="n") #, asp=NA)
  axis(2, at=seq(-90,90, by=30) )

  # Make name for graphic file - can use various extensions for different file types
  if (filename != '') {
       message(sprintf("Saving Image:"))
       message(sprintf("%s",filename))
       png(filename=filename, res=200, 
           width=8, height=5, units="in")
       plot(r, main=title, xlab="Longitude", 
            ylab="Latitude",
            xlim=c(-180,180), ylim=c(-90,90),
            xaxs="i", yaxs="i", yaxt="n")
       axis(2, at=seq(-90,90, by=30))
       dev.off()
  }
}
