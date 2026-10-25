makeAreasVector <- function(nsib, lonarray, latarray)
{

  # Set local variables
  pi <- acos(-1.0)
  radius <- 6371000.

  # Calculate areal variables for SiB4 vector points
  lontemp <- lonarray
  min1<-min(lontemp)
  minref <- which(lontemp <= min1)
  lontemp[minref] <- 999.
  min2 <- min(lontemp)
  lonres <- (min2-min1)
  deltalon <- lonres*0.5
  #message(sprintf("Longitude Resolution: %s",lonres))

  lattemp <- latarray
  min1 <- min(lattemp)
  minref <- which(lattemp <= min1)
  lattemp[minref] <- 999.
  min2 <- min(lattemp)
  latres <- (min2-min1)
  deltalat <- latres*0.5
  #message(sprintf("Latitutde Resolution: %s",latres))

  area <- array(0.0, dim=c(nsib))
  areatot <- 0.0
  for (ns in 1:nsib)
  {
     lone <- lonarray[ns]+deltalon
     lonw <- lonarray[ns]-deltalon
     loner <- lone*pi/180.
     lonwr <- lonw*pi/180.
     londiff <- loner - lonwr

     lats <- latarray[ns]-deltalat
     latn <- latarray[ns]+deltalat
     latsr <- lats*pi/180.
     latnr <- latn*pi/180.

     sinlatsr <- sin(latsr)
     sinlatnr <- sin(latnr)
     sinlatdiff <- sinlatnr - sinlatsr

     area[ns] <- radius * radius * londiff * sinlatdiff
     areatot <- areatot + area[ns]
  }

  #message(sprintf("Area Actual: 0.509904365E+15"))
  #message(sprintf("Area Calculated: %s",areatot))

 return(area)

}
