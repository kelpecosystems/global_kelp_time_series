####################################################
# Make figure showing the spatiotemporal distribution of data
#
# Jarrett Byrnes
#
# Changelog
####################################################
library(dplyr)
library(ggplot2)
library(cowplot)
library(meowR)
source("./0_temporal_kelp_prep.R")

kelpy.df <- subset(regions.df, regions.df$hasKelp==1)
#rawDataMod2 <- rawDataMod2[,-ncol(rawDataMod2)]
#Get just unique study/site info
#rawDataMod_3points_3years
#rawDataMod2
rd <- rawDataMod2 %>% 
  group_by(StudySite, Study, Latitude, Longitude, REALM, PROVINCE, ECOREGION) %>%
  dplyr::summarise(n = length(Longitude), timespan = range(year)[2] - range(year)[1],
            `First Year` = min(year), `Last Year` = max(year)) %>%
  arrange(`First Year`) %>% ungroup()

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
levels(rd$ECOREGION) <- c(levels(rd$ECOREGION), "North American Pacific Fjordland")
rd$ECOREGION[rd$ECOREGION=="North American Pacific Fijordland"]<-"North American Pacific Fjordland"


#Get ecoregions with sample sizes
ecoSamp <- rd %>% group_by(ECOREGION) %>%
  dplyr::summarize(nEco = length(ECOREGION)) %>%
  ungroup()

rd <- left_join(rd, ecoSamp)

#add numbers that match map in Fig 1.
numbered <- data.frame(ECOREGION = c("Beaufort Sea",
                                     "Aleutian Islands",
                                     "Gulf of Alaska",
                                     "North American Pacific Fjordland",
                                     "Oregon, Washington, Vancouver Is.",
                                     "Puget Trough/Georgia Basin",
                                     "Northern and Central California",
                                     "Southern California Bight",
                                     "Virginian",
                                     "Gulf of Maine",
                                     "Scotian Shelf",
                                     "Gulf of St. Lawrence",
                                     "Northern Grand Banks - Southern Labrador",
                                     "East Greenland Shelf",
                                     "Southern Norway",
                                     "North Sea",
                                     "Celtic Seas",
                                     "South European Atlantic Shelf",
                                     "Northeastern Honshu",
                                     "Sea of Japan/East Sea",
                                     "Humboldtian",
                                     "Central Chile",
                                     "Araucanian",
                                     "Chiloense",
                                     "Namaqua",
                                     "Agulhas Bank",
                                     "Houtman",
                                     "Leeuwin",
                                     "South Australian Gulfs",
                                     "Western Bassian",
                                     "Bassian",
                                     "Cape Howe",
                                     "Manning-Hawkesbury",
                                     "NE New Zealand"),  stringsAsFactors = FALSE) #Was Northeastern


numbered$LAB = paste(1:nrow(numbered), numbered$ECOREGION, sep=". ")

rd <- left_join(rd, numbered)


rd$xlab = factor(with(rd, paste0(LAB, " (", nEco,")")))
rd$xlab <- factor(rd$xlab, levels=gtools::mixedsort(levels(rd$xlab), decreasing=TRUE))


#THE PLOT
timePlot <- ggplot(data=rd, aes(x=xlab, 
                    ymin=`First Year`, ymax=`Last Year`)) +
  coord_flip() +
  geom_linerange(data = rd, position=position_dodge(width=0.7), alpha=1, 
                 color="black", 
                 mapping=aes(group=sort(paste(`First Year`, StudySite), decreasing=TRUE)), 
                 size=0.3) +
  ylab("\nYear") +
  xlab("") +
  theme_light(base_size=24) +
  geom_vline(xintercept=1:33+0.5, lty=1, color="grey85", size=0.25) +
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.minor.x = element_blank(),
        axis.title.y=element_text(angle=-90),
        axis.text.x=element_text(color="black", size=24),
        axis.text.y=element_text(color="black", size=24),
        panel.border=element_rect(color="grey50"),
        axis.ticks = element_line(colour = "grey50")) +
  scale_y_continuous(breaks=seq(1940, 2020, 10), limits=c(1945, 2015))

jpeg("../Figures/data_properties/site_timespans.jpg", height=1024, width=1224, type = c("quartz"))
ggdraw(switch_axis_position(timePlot, axis = 'y'))
dev.off()


pdf("../Figures/data_properties/site_timespans.pdf", height=14, width=17)
ggdraw(switch_axis_position(timePlot, axis = 'y'))
dev.off()

ggsave(plot=ggdraw(switch_axis_position(timePlot, axis = 'y')),
       filename="../Figures/data_properties/site_timespans.eps", 
       height=14, width=17, units="in")
