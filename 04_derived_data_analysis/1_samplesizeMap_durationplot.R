####################################################
# Make maps of distribution of sample sites by Ecoregion
# and Province as well as plot the distribution of sample durations
#
# Jarrett Byrnes
#
# Changelog
# 11/3/15 - Re-ordered ecoregions for cleaner plot
####################################################
library(meowR)
library(dplyr)
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

provinces <- rd %>% group_by(PROVINCE) %>%
  summarise(Sites = length(PROVINCE))


ecoregions <- rd %>% group_by(ECOREGION) %>%
  summarise(Sites = length(ECOREGION), `Average Duration` = mean(timespan),
            `First Year` = min(`First Year`), `Last Year` = max(`Last Year`))


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

rd$ECOREGION <- factor(rd$ECOREGION, levels=sort(levels(rd$ECOREGION), decreasing=T))
rd$PROVINCE <- factor(rd$PROVINCE, levels=sort(levels(rd$PROVINCE), decreasing=T))

pdf("../Figures/data_properties/ecoregion_duration.pdf", height=15, width=20)
ggplot(rd, aes(x=ECOREGION, y=timespan)) +
  geom_jitter(color="lightgrey") +
  theme_bw() +
  theme(panel.grid.minor = element_line(colour = NA),
        panel.grid.major = element_line(colour = NA),
        axis.title.x=element_text(size=24),
        axis.title.y=element_text(size=24),
        axis.text=element_text(size=24))+
  coord_flip() +
  stat_summary(fun.data = "mean_cl_boot", color="black", size=2) +
  ylab("Duration (years)") + xlab("")
dev.off()

jpeg("../Figures/data_properties/province_duration.jpg", height=768, width=1024, type = c("quartz"))
ggplot(rd, aes(x=PROVINCE, y=timespan)) +
  geom_jitter(color="lightgrey") +
  theme_bw(base_size=24) +
  coord_flip() +
  stat_summary(fun.data = "mean_cl_boot", color="red", size=1.5) +
  ylab("Duration (years)") + xlab("")
dev.off()