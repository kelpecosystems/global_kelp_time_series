library(rgeos)
#devtools::install_github("meowR", "jebyrnes")
library(meowR)
library(dplyr)

#read in the parameters
#params <- read.csv("../06_HLM_output/site_slopes.csv")
params <- read.csv("../06_HLM_output/site_slopes_3points.csv")
params <- params %>% filter(parameter=="mean_slope")
params$group_name <- as.character(params$group_name)
params$group_name[which(params$group_name=="Gulf of Maine-Bay of Fundy")] <- "Gulf of Maine/Bay of Fundy"

getProb <- function(p, levs=c(0.05, 0.1)){
  levs <- sort(levs)
  levs <- c(-1, levs)
  probs <- cut(p, levs)
  probs <- gsub("\\(.*,", "", probs)
  probs <- gsub("\\]", "", probs)
  probs
}

params$bayesian_probability <- getProb(params$p)


slopeMap <- function(geoGroup="Ecoregion", Timespan="1900-2015",
                     addP = TRUE, pathCol="black", pathSize=2, ...){
  
    pathColNow <- pathCol
    adf <- params %>% filter(grouping==geoGroup &
                             Period==Timespan) %>% 
      filter(parameter=="mean_slope")

    if(addP) pathColNow<-NA
    
    ret <- makeMEOWmap(adf,
                fillColName="mean", 
                type=toupper(geoGroup),
                regionColName="group_name", 
                guide=guide_colorbar(title="Estimated\nSlope"),
                add.worldmap=T, pathCol=NA, ...)+ 
          xlab("\nLongitude") + ylab("\nLatitude")
    
    if(addP){
      retData <- makeMEOWmapData(adf, fillColName="mean",
                                 type=toupper(geoGroup),
                                 regionColName="group_name")
      
      ret <- ret+
        geom_path(data=retData, color="black", alpha=1, 
                  size=1.5, mapping=aes(lty=bayesian_probability, 
                                      x=long, y=lat, group=group)) +
        scale_linetype_discrete(guide=guide_legend(title="Bayesian Posterior\nProbability Cutoff"))
      
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


