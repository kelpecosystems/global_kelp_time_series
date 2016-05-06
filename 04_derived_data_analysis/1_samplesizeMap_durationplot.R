####################################################
# Make maps of distribution of sample sites by Ecoregion
# and Province as well as plot the distribution of sample durations
#
# Jarrett Byrnes
#
# Changelog
# 11/3/15 - Re-ordered ecoregions for cleaner plot
# 3/24/15 - Kira re-named ecoregions to be consistent with other plots, and re-ordered ecoregion_duration figure according to mean duration of studies
####################################################
library(meowR)
library(dplyr)
library(ggplot2)
source("./0_temporal_kelp_prep.R")

kelpy.df <- subset(regions.df, regions.df$hasKelp==1)
#rawDataMod2 <- rawDataMod2[,-ncol(rawDataMod2)]
#Get just unique study/site info
#rawDataMod_3points_3years
#rawDataMod2
rd <- rawDataMod2 %>% 
  group_by(StudySite, Study, Latitude, Longitude, REALM, PROVINCE, ECOREGION) %>%
  summarise(n = length(Longitude), timespan = range(year)[2] - range(year)[1],
            `First Year` = min(year), `Last Year` = max(year))

#editing ecoregion names to shorten and be consistent with co-author comments
levels(rd$ECOREGION) <- c(levels(rd$ECOREGION), "Beaufort Sea")
rd$ECOREGION[rd$ECOREGION=="Beaufort Sea - continental coast and shelf"]<-"Beaufort Sea"
levels(rd$ECOREGION) <- c(levels(rd$ECOREGION), "Gulf of Maine")
rd$ECOREGION[rd$ECOREGION == "Gulf of Maine/Bay of Fundy"]<-"Gulf of Maine"
levels(rd$ECOREGION) <- c(levels(rd$ECOREGION), "Oregon, Washington, Vancouver Is.")
rd$ECOREGION[rd$ECOREGION=="Oregon, Washington, Vancouver Coast and Shelf"]<-"Oregon, Washington, Vancouver Is."
levels(rd$ECOREGION) <- c(levels(rd$ECOREGION), "Northern and Central California")
rd$ECOREGION[rd$ECOREGION=="Northern California"]<-"Northern and Central California"
levels(rd$ECOREGION) <- c(levels(rd$ECOREGION), "Gulf of St. Lawrence")
rd$ECOREGION[rd$ECOREGION=="Gulf of St. Lawrence - Eastern Scotian Shelf"]<-"Gulf of St. Lawrence"
levels(rd$ECOREGION) <- c(levels(rd$ECOREGION), "NE New Zealand")
rd$ECOREGION[rd$ECOREGION=="Northeastern New Zealand"]<-"NE New Zealand"


provinces <- rd %>% group_by(PROVINCE) %>%
  summarise(Sites = length(PROVINCE))


ecoregions <- rd %>% group_by(ECOREGION) %>%
  summarise(Sites = n(), `Average_Duration` = mean(timespan),
            `First_Year` = min(`First Year`), `Last_Year` = max(`Last Year`), `Standard_Deviation`=sd(timespan))%>%
  mutate(se=Standard_Deviation/sqrt(Sites), LCI=Average_Duration+qnorm(0.025)*se, UCI=Average_Duration+qnorm(0.975)*se)


realms <- rd %>% group_by(REALM) %>%
  summarise(Sites = length(REALM))

jpeg("../Figures/data_properties/ecoregion_samplesize_map.jpg", height=768, width=1024, type = c("quartz"))
makeMEOWmap(ecoregions, type="ECOREGION", fillColName="Sites", 
            fillPal=rainbow(11, start=.7, end=.1), 
            trans="log10", 
            prevggplot=ggplot()+ 
              geom_polygon(data=kelpy.df, aes(x=long, y=lat, group=ECOREGION), fill="lightgrey"),
            add.worldmap=T) + xlab("Longitude") + ylab("Latitude") 
dev.off()

jpeg("../Figures/data_properties/province_samplesize_map.jpg", height=768, width=1024, type = c("quartz"))

makeMEOWmap(provinces, type="PROVINCE", fillColName="Sites", 
            fillPal=rainbow(11, start=.7, end=.1), 
            trans="log10", 
            prevggplot=ggplot()+ 
              geom_polygon(data=kelpy.df, aes(x=long, y=lat, group=ECOREGION), fill="lightgrey"),
            add.worldmap=T) + xlab("Longitude") + ylab("Latitude") 
dev.off()


jpeg("../Figures/data_properties/realm_samplesize_map.jpg", height=768, width=1024, type = c("quartz"))

makeMEOWmap(realms, type="REALM", fillColName="Sites", 
            fillPal=rainbow(11, start=.7, end=.1), 
            prevggplot=ggplot()+ 
              geom_polygon(data=kelpy.df, aes(x=long, y=lat, group=ECOREGION), fill="lightgrey"),
            add.worldmap=T) + xlab("Longitude") + ylab("Latitude")  
dev.off()

#############
# Incorporate Study Duration into Maps & other Plots
#############

jpeg("../Figures/data_properties/ecoregion_average_duration.jpg", height=768, width=1024, type = c("quartz"))
makeMEOWmap(ecoregions, type="ECOREGION", fillColName="Average Duration", 
            fillPal=rainbow(11, start=.7, end=.1), 
            prevggplot=ggplot()+ 
              geom_polygon(data=kelpy.df, aes(x=long, y=lat, group=ECOREGION), fill="lightgrey"),
            add.worldmap=T) + xlab("Longitude") + ylab("Latitude") 
dev.off()



#re-ordering ECOREGION by average duration
ecoregions<-as.data.frame(ecoregions)
ecoregions_r<-ecoregions
ecoregions_r$ECOREGION<-factor(ecoregions$ECOREGION, levels=ecoregions[order(ecoregions$Average_Duration), "ECOREGION"])
ecoregions_o<-ecoregions_r[order(ecoregions_r$ECOREGION),]

library(grid)
pdf("../Figures/data_properties/ecoregion_duration.pdf", height=15, width=20)
ggplot(data=ecoregions_o) + 
  geom_point(aes(x=Average_Duration, y=ECOREGION), size=7)+
  geom_errorbarh(aes(y=ECOREGION,x=Average_Duration, xmin=Average_Duration-Standard_Deviation, xmax=Average_Duration+Standard_Deviation), height=0, size=1)+
  geom_point(aes(x=timespan, y=ECOREGION), data=rd, size=3, position="jitter", alpha=0.15)+
  labs(x="Duration (yr)", y="Ecoregion")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), 
        legend.position="none", 
        axis.title.x=element_text(size=28, face="bold"),
        axis.title.y=element_text(size=28, face="bold", vjust=2),
        axis.text.y=element_text(size=24),
        axis.text.x=element_text(size=24),
        strip.text = element_text(size=28, face="bold"),
        panel.margin = unit(20, "mm"),
        plot.margin = unit(c(10,10,10,10), "mm"),
        strip.background=element_blank())
dev.off()


jpeg("../Figures/data_properties/province_duration.jpg", height=768, width=1024, type = c("quartz"))
ggplot(rd, aes(x=PROVINCE, y=timespan)) +
  geom_jitter(color="lightgrey") +
  theme_bw(base_size=24) +
  coord_flip() +
  stat_summary(fun.data = "mean_cl_boot", color="red", size=1.5) +
  ylab("Duration (years)") + xlab("")
dev.off()