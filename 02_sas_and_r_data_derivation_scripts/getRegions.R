############################################################################################
# 
# Script to turn lat/long data into
# marine ecoregions using Spalding's Marine Ecoregions
# of the World (MEOW).
#
# To run, first acquire the appropriate GIS files from 
# http://maps.tnc.org/gis_data.html and unzip them into
# a directory (MEOW-TNC) - then go from there.
#
# getRegionalInfo returns Realms, Provinces, and Ecoregions
# for a single lat/long. I'm sure this can be improved, as this is
# my first attempt at doing anything GIS-y in R!
#
# - Jarrett Byrnes
#
# Last Updated 6/10/2014
############################################################################################

library(rgdal)
library(raster) 
# for shapefiles, first argument of the read/write/info functions is the
# directory location, and the second is the file name without suffix

# optionally report shapefile details
ogrInfo("../MEOW-TNC", "meow_ecos")


regions <- readOGR("../MEOW-TNC", "meow_ecos")

#let's see the map
#plot(regions, axes=TRUE, border="gray")

getRegionalInfo  <- function(lat1, long1){
#lat1 <- c(50.09444)
#long1 <- c(-127.5589)


  #first, extract the co-ordinates (x,y - i.e., Longitude, Latitude)
  coords <- cbind(long1, lat1)

  FB.sp <- SpatialPointsDataFrame(coords,data.frame(value = c(4)))

  proj4string(FB.sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

#  plot(regions)
#  plot(FB.sp, add=T)


  dsdat <- over(regions, FB.sp, add=T, fn = mean) 

  ret <- data.frame(ECOREGION = regions$ECOREGION[which(dsdat$value==4)],
                    PROVINCE = regions$PROVINCE[which(dsdat$value==4)],
    REALM = regions$REALM[which(dsdat$value==4)])

  if(nrow(ret)==0) ret <- data.frame(ECOREGION = NA,
                                      PROVINCE = NA,
                                      REALM = NA)
  return(ret)

}


#Not Run Demos
#getRegionalInfo(50.00806, -127.4342)

# they take a while, as the raw data file is large...must be a better way

#rawData <- read.csv("../clean_datasets/dataQA/CleanData.csv")
#if(is.na(rawData[1,1])) rawData <- rawData[-1,] #first row is NA in early versions of the dataset

#getRegionalInfo(rawData$Latitude[2], rawData$Longitude[2])

#library(plyr)
#allRegions <- lapply(1:nrow(rawData), function(i) getRegionalInfo(rawData$Latitude[i], rawData$Longitude[i]))
#allRegions <- ldply(allRegions)
