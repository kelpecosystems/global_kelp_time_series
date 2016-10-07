library(rgeos)
#devtools::install_github("meowR", "jebyrnes")
library(meowR)
library(dplyr)
library(colorspace)

#read in the parameters
#params <- read.csv("../06_HLM_output/site_slopes.csv")
params <- read.csv("../06_HLM_output/site_slopes_3_points.csv")
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

params$bayesian_probability <- getProb(1-ifelse(params$p-0.5 < 0, 1-params$p, params$p))
params$bayesian_probability <- gsub("0.05", "95%", params$bayesian_probability)
params$bayesian_probability <- gsub("0.1", "90%", params$bayesian_probability)

slopeMap <- function(geoGroup="Ecoregion", Timespan="1900-2015",
                     addP = TRUE, pathCol="black", pathSize=2, limits=c(-0.3, 0.3), 
                     #fillPal=rev(diverge_hsv(3)), 
                     #fillPal=brewer.pal(11, "RdYlBu"),
                     fillPal=rev(diverge_hsv(20, power=0.4)),
                     add.worldmap=T,
                     ...){
  
    pathColNow <- pathCol
    adf <- params %>% filter(grouping==geoGroup &
                             Period==Timespan) %>% 
      filter(parameter=="mean_slope")

    if(addP) pathColNow<-NA
    
    ret <- makeMEOWmap(adf, fillPal=fillPal,
                fillColName="mean", 
                type=toupper(geoGroup),
                regionColName="group_name", 
                guide=guide_colorbar(title="Estimated\nSlope"),
                add.worldmap=add.worldmap, pathCol=NA, limits=limits, ...)+ 
          xlab("\nLongitude") + ylab("\nLatitude")
    
    if(addP){
      retData <- makeMEOWmapData(adf, fillColName="mean",
                                 type=toupper(geoGroup),
                                 regionColName="group_name")
      retData$alpha <- !is.na(retData$bayesian_probability)
      
      ret <- ret+
        geom_path(data=retData, 
                  size=1.2, mapping=aes(colour=bayesian_probability, 
                                      x=long, y=lat, group=group, alpha=alpha)) +
       scale_colour_manual(guide=guide_legend(title="Bayesian Probability\nCutoff"),
                           values=c("darkgrey", "black")) +
        scale_alpha_discrete(guide="none")
      
    }
    
  ret
}

jpeg("../Figures/HLM_slope_maps/ecoregion_slope_map.jpg", height=768, width=1024, type = c("quartz"))
print(slopeMap()+theme_bw(base_size=20))
dev.off()

pdf("../Figures/HLM_slope_maps/ecoregion_slope_map.pdf", width= 11,height= 14)
print(slopeMap()+theme_bw(base_size=20))
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


jpeg("../Figures/HLM_slope_maps/ecoregion_slope_map_oz.jpg", height=768, width=1024, type = c("quartz"))
print(slopeMap() + xlim(c(100,200)) + ylim(c(-60,0)))
dev.off()


##### Zoom in for ecoregion plots
jpeg("../Figures/HLM_slope_maps/ecoregion_slope_map_atlantic.jpg", height=768, width=1024, type = c("quartz"))
print(slopeMap() + xlim(c(-100,30)) + ylim(c(30,90)))
dev.off()

jpeg("../Figures/HLM_slope_maps/ecoregion_slope_map_ne_atlantic.jpg", height=768, width=1024, type = c("quartz"))
print(slopeMap() + xlim(c(-20,30)) + ylim(c(30,70)))
dev.off()

jpeg("../Figures/HLM_slope_maps/ecoregion_slope_map_nw_atlantic.jpg", height=768, width=1024, type = c("quartz"))
print(slopeMap() + xlim(c(-80,-50)) + ylim(c(35,60)))
dev.off()


jpeg("../Figures/HLM_slope_maps/ecoregion_slope_map_ne_pacific.jpg", height=768, width=1024, type = c("quartz"))
print(slopeMap() + xlim(c(-180,-110)) + ylim(c(15,80)))
dev.off()


jpeg("../Figures/HLM_slope_maps/ecoregion_slope_map_s_amer.jpg", height=768, width=1024, type = c("quartz"))
print(slopeMap() + xlim(c(-100,-30)) + ylim(c(-65,15)))
dev.off()