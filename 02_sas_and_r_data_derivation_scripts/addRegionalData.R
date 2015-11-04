library(plyr)
source("./getRegions.R")

# they take a while, as the raw data file is large...must be a better way

rawData <- read.csv("../03_derived_data/CleanData.csv")


#get the unique lat/long pairs - speeds things up 5x!
uniqueLatLong <- ddply(rawData, .(Latitude, Longitude), summarise, len = length(Latitude))
allRegions <- lapply(1:nrow(uniqueLatLong), function(i) getRegionalInfo(uniqueLatLong$Latitude[i], uniqueLatLong$Longitude[i]))
allRegions <- ldply(allRegions)
allRegions <- cbind(uniqueLatLong, allRegions)

regionalData <- join(rawData, allRegions)

#write the data with regions
write.csv(regionalData, "../03_derived_data/CleanDataWithRegions.csv")
