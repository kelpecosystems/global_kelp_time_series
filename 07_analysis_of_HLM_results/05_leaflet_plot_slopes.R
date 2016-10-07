########################################################################
#### A script to plot sites with maps                                   ####
########################################################################
library(dplyr)
library(meowR); data(regions)
library(leaflet)

rawData <- read.csv("../03_derived_data/CleanData.csv", stringsAsFactors=FALSE)
site_slopes <- read.csv("../06_HLM_output/eco_sites.csv", stringsAsFactors=FALSE) %>%
  filter(Period == "1900-2015")

#First, need to make a key in rawData to join on via the SiteName of 
#site_slopes
#with(rawDataMod, paste(Study, Site, study_ID,trajectory_ID,sep="-"))
rawDataFilt <- rawData %>%
  mutate(SiteName =  paste(Study, Site, study_ID,trajectory_ID,sep="-")) %>%
  mutate(SiteName = paste(SiteName, Study, sep=":")) %>%
  select(Study, SiteName, Latitude, Longitude) %>%
  group_by(SiteName) %>%
  slice(1L) %>%
  ungroup()

#Great, now join
site_slopes_latlong <- left_join(site_slopes, rawDataFilt)
sum(is.na(site_slopes_latlong$Latitude))

#now plot!
#Then join to 
ssl <- site_slopes_latlong %>%
  filter(!is.na(Latitude)) %>%
  filter(!is.na(Longitude))

library(leaflet)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = ssl, 
             lng = ~Longitude, lat =  ~Latitude,
             opacity=1,
             color = ~colorQuantile("RdBu", ssl$mean, n = 20)(mean))
