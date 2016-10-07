library(dplyr)
file_dir <- "../01_clean_raw_data/"
files <- list.files(file_dir, "*.csv")

#######
#Read in and process the mined file
#######
kelp_tr_data <- read.csv(paste0(file_dir, files[1]), stringsAsFactors=FALSE) %>%
  dplyr::select(-Taxon, -Direct.or.Indirect, -Stressor.or.Cause) %>%
  dplyr::rename(Taxon = Species, 
         Study = Study.ID) %>%
  dplyr::mutate(Entry = 1:nrow(.)) %>%
  dplyr:: mutate(Sample.ID = ifelse(is.na(Sample.ID), Site, Sample.ID),
         Sample.Month = ifelse(is.na(Sample.Month), 6, Sample.Month))


#Read in the rest and merge in
for(i in 2:length(files)){
  file_i <- read.csv(paste0(file_dir, files[i]), stringsAsFactors=FALSE)
  kelp_tr_data <- rbind(kelp_tr_data, file_i)
}

#filter out CYOS or >3m depth
kelp_tr_data <- kelp_tr_data%>%
  filter(Depth.m>=3)

#Add regions
library(sp)
library(meowR)

#Create a spatial Points Data Frame
pts <- with(kelp_tr_data, SpatialPoints(cbind(Longitude, Latitude),
                                   proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
pts <- SpatialPointsDataFrame(pts, kelp_tr_data)
#plot(pts)

#get the regions shapefile
data(regions)

#extract regional info
dataRegions <- over(pts, regions) %>%
  dplyr::select(ECOREGION, PROVINCE, REALM) %>%
  dplyr::rename(Ecoregion = ECOREGION,
         Province = PROVINCE,
         Realm = REALM)

#bind to outfile
regionalData <- cbind(kelp_tr_data, dataRegions)


write.csv(regionalData, file="~/Dropbox/Projects/TemperateReefBase/kelp_timeseries/krumhansl_kelp_timeseries_raw.csv", row.names=FALSE)

##Get and Clean the derived dataset
derived_set <- read.csv("../03_derived_data/CleanDataWithRegions.csv") %>%
  dplyr::select(-elapsed, -ROC, -X)
write.csv(derived_set, file="~/Dropbox/Projects/TemperateReefBase/kelp_timeseries/krumhansl_kelp_timeseries_aggregated.csv", row.names=FALSE)

system("cp ../03_derived_data/CleanDataWithRegions.csv ~/Dropbox/Projects/TemperateReefBase/kelp_timeseries/krumhansl_kelp_timeseries_aggregated.csv")
