library(meowR)
library(dplyr)
library(colorspace)
library(ggplot2)

#read in the parameters
params <- read.csv("../06_HLM_output/site_slopes.csv")
params <- params %>% filter(parameter=="mean_slope")
params$group_name <- as.character(params$group_name)
params$group_name[which(params$group_name=="Gulf of Maine-Bay of Fundy")] <- "Gulf of Maine/Bay of Fundy"

#probability of change signed by weight of evidence for direction of change
params$pSign <- sign(params$p-0.5) * ifelse(params$p-0.5 < 0, 1-params$p, params$p)
params$pSign2 <- cut(params$pSign, breaks=c(seq(-1, -0.5, .1), -0.4, 0.4,seq(0.5, 1, .1)))

#Create a palatte for easy differentiation of probabilities
fillPal <- brewer.pal(9,"RdYlBu")
fillPal <- fillPal[9:1]
fillPal <- diverge_hsv(9)
fillPal[5] <- "darkgray"
fillPalGood <- fillPal

makePSignMap <- function(geoGroup="Ecoregion", Timespan="1900-2015", fillPal=fillPalGood, ...){
  makeMEOWmap(params %>% filter(grouping==geoGroup) %>% filter(Period==Timespan) %>% filter(parameter=="mean_slope"),
              fillColName="pbox", "group_name",
              type=toupper(geoGroup),
              fillPal=fillPal, 
              guide=guide_legend(title="Probability of\nKelp Having Changed\nwith Sign"),
              add.worldmap=T, limits=levels(params$pbox), ...) + 
    xlab("\nLongitude") + ylab("\nLatitude")
}


#?use cut
params$pbox <- cut(params$pSign, breaks=c(-1.1, -0.95, -0.9, -0.8, -0.7,  0.7, 0.8, 0.9, 0.95, 1.1))
params$pbox <- gsub("\\(", "\\[", params$pbox)
params$pbox <- gsub("\\.1", "", params$pbox)
params$pbox <- factor(params$pbox)
#params$pbox <- factor(params$pbox, levels=levels(params$pbox)[c(5:1, 6:9)])
params$pbox <- factor(params$pbox, levels=levels(params$pbox)[c(9:6, 1:5)])




#maps of p values
jpeg("../Figures/HLM_probability_maps/ecoregion_probability_map.jpg", height=768, width=1024, type = c("quartz"))
makePSignMap() 
dev.off()


jpeg("../Figures/HLM_probability_maps/province_probability_map.jpg", height=768, width=1024, type = c("quartz"))
makePSignMap("Province")
dev.off()


jpeg("../Figures/HLM_probability_maps/realm_probability_map.jpg", height=768, width=1024, type = c("quartz"))

makePSignMap("Realm")
dev.off()


#maps of p values for 1983-1992
jpeg("../Figures/HLM_probability_maps/ecoregion_probability_map_1983-1992.jpg", height=768, width=1024, type = c("quartz"))
makePSignMap(Timespan="1983-1992")
dev.off()


jpeg("../Figures/HLM_probability_maps/province_probability_map_1983-1992.jpg", height=768, width=1024, type = c("quartz"))

makePSignMap("Province", Timespan="1983-1992")

dev.off()


jpeg("../Figures/HLM_probability_maps/realm_probability_map.jpg_1983-1992", height=768, width=1024, type = c("quartz"))

makePSignMap("Realm", Timespan="1983-1992")

dev.off()



#maps of p values 1993-2002
jpeg("../Figures/HLM_probability_maps/ecoregion_probability_map_1993-2002.jpg", height=768, width=1024, type = c("quartz"))

makePSignMap("Ecoregion", Timespan="1993-2002")

dev.off()


jpeg("../Figures/HLM_probability_maps/province_probability_map_1993-2002.jpg", height=768, width=1024, type = c("quartz"))

makePSignMap("Province", Timespan="1993-2002")

dev.off()


jpeg("../Figures/HLM_probability_maps/realm_probability_map.jpg_1993-2002", height=768, width=1024, type = c("quartz"))

makePSignMap("Realm", Timespan="1993-2002")

dev.off()


#maps of p values 2003-2012
jpeg("../Figures/HLM_probability_maps/ecoregion_probability_map_2003-2012.jpg", height=768, width=1024, type = c("quartz"))

makePSignMap("Ecoregion", Timespan="2003-2012")

dev.off()


jpeg("../Figures/HLM_probability_maps/province_probability_map_2003-2012.jpg", height=768, width=1024, type = c("quartz"))

makePSignMap("Province", Timespan="2003-2012")

dev.off()


jpeg("../Figures/HLM_probability_maps/realm_probability_map_2003-2012.jpg", height=768, width=1024, type = c("quartz"))

makePSignMap("Realm", Timespan="2003-2012")

dev.off()


kelpy.df <- subset(regions.df, regions.df$hasKelp==1)

jpeg("../Figures/HLM_probability_maps/chile_probability_map.jpg", height=768, width=1024, type = c("quartz"))
makePSignMap(fillPal=fillPal,
             prevggplot=ggplot()+ 
               geom_polygon(data=kelpy.df, 
                            aes(x=long, y=lat, group=ECOREGION), 
                            fill="white", color="grey")) + xlim(-90,-25) + ylim(-70,15)
dev.off()


jpeg("../Figures/HLM_probability_maps/new_england_probability_map.jpg", height=768, width=1024, type = c("quartz"))
makePSignMap() + xlim(-90,-50) + ylim(20,70)
dev.off()



