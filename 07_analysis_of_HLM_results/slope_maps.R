library(rgeos)
#devtools::install_github("meowR", "jebyrnes")
library(meowR)
library(dplyr)

#read in the parameters
params <- read.csv("../06_HLM_output/site_slopes.csv")
params <- params %>% filter(parameter=="mean_slope")
params$group_name <- as.character(params$group_name)
params$group_name[which(params$group_name=="Gulf of Maine-Bay of Fundy")] <- "Gulf of Maine/Bay of Fundy"


slopeMap <- function(geoGroup="Ecoregion", Timespan="1900-2015",
                     addStar=F, ci=0.9){
    adf <- params %>% filter(grouping==geoGroup &
                             Period==Timespan) %>% 
      filter(parameter=="mean_slope")

    adf$starMe <- ""
    adf$starMe[which(adf$p < 1-ci)] <- "*" #yuck
    
    ret <- makeMEOWmap(adf ,
                fillColName="mean", 
                type=toupper(geoGroup),
                regionColName="group_name", 
                guide=guide_colorbar(title="Estimated\nSlope"),
                add.worldmap=T)+ 
          xlab("\nLongitude") + ylab("\nLatitude")
    
    if(addStar){
    centroids <- data.frame(getSpPPolygonsLabptSlots(regions))
    #centroids <- data.frame(gCentroid(regions))
    names(centroids) = c("x", "y")
    centroids$group_name <- regions[[toupper(geoGroup)]]
    centroids <- join(centroids, adf)
    ret <- ret +
      geom_text(data=centroids, mapping=aes(x=x, y=y, label=starMe), 
                size=15, vjust=0.7)
  }
  ret
}

jpeg("../Figures/HLM_slope_maps/ecoregion_slope_map.jpg", height=768, width=1024, type = c("quartz"))
print(slopeMap())
dev.off()

jpeg("../Figures/HLM_slope_maps/ecoregion_slope_map_1983-1992.jpg", height=768, width=1024, type = c("quartz"))
print(slopeMap(Timespan="1983-1992")+ggtitle("1983-1992"))
dev.off()

jpeg("../Figures/HLM_slope_maps/ecoregion_slope_map_1993-2002.jpg", height=768, width=1024, type = c("quartz"))
print(slopeMap(Timespan="1993-2002")+ggtitle("1993-2002"))
dev.off()

jpeg("../Figures/HLM_slope_maps/ecoregion_slope_map_2003-2012.jpg", height=768, width=1024, type = c("quartz"))
print(slopeMap(Timespan="2003-2012")+ggtitle("2003-2012"))
dev.off()



jpeg("../Figures/HLM_slope_maps/province_slope_map.jpg", height=768, width=1024, type = c("quartz"))
print(slopeMap("Province"))
dev.off()

jpeg("../Figures/HLM_slope_maps/province_slope_map_1983-1992.jpg", height=768, width=1024, type = c("quartz"))
print(slopeMap("Province", Timespan="1983-1992")+ggtitle("1983-1992"))
dev.off()

jpeg("../Figures/HLM_slope_maps/province_slope_map_1993-2002.jpg", height=768, width=1024, type = c("quartz"))
print(slopeMap("Province", Timespan="1993-2002")+ggtitle("1993-2002"))
dev.off()

jpeg("../Figures/HLM_slope_maps/province_slope_map_2003-2012.jpg", height=768, width=1024, type = c("quartz"))
print(slopeMap("Province", Timespan="2003-2012")+ggtitle("2003-2012"))
dev.off()


jpeg("../Figures/HLM_slope_maps/realm_slope_map.jpg", height=768, width=1024, type = c("quartz"))
print(slopeMap("Realm"))
dev.off()

jpeg("../Figures/HLM_slope_maps/realm_slope_map_1983-1992.jpg", height=768, width=1024, type = c("quartz"))
print(slopeMap("Realm", Timespan="1983-1992")+ggtitle("1983-1992"))
dev.off()

jpeg("../Figures/HLM_slope_maps/realm_slope_map_1993-2002.jpg", height=768, width=1024, type = c("quartz"))
print(slopeMap("Realm", Timespan="1993-2002")+ggtitle("1993-2002"))
dev.off()

jpeg("../Figures/HLM_slope_maps/realm_slope_map_2003-2012.jpg", height=768, width=1024, type = c("quartz"))
print(slopeMap("Realm", Timespan="2003-2012")+ggtitle("2003-2012"))
dev.off()


