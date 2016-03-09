########################################################################
#### A script to plot temporal change data from NCEAS kelp database ####
#### Created by D. Okamoto and edited by K. Krumhansl               ####
#### Last edited November 3 2015                                    ####
########################################################################


library(ggplot2)

setwd("~/Dropbox/nceas_kelp_climate_2013/temporal_change/github_repo/06_HLM_output")

data<-read.csv("site_slopes_3points.csv")

####shortening and correcting some ecoregion names####
levels(data$group_name) <- c(levels(data$group_name), "Beaufort Sea")
data$group_name[data$group_name=="Beaufort Sea - continental coast and shelf"]<-"Beaufort Sea"
levels(data$group_name) <- c(levels(data$group_name), "Gulf of Maine")
data$group_name[data$group_name == "Gulf of Maine/Bay of Fundy"]<-"Gulf of Maine"
levels(data$group_name) <- c(levels(data$group_name), "Oregon, Washington, Vancouver Island")
data$group_name[data$group_name=="Oregon, Washington, Vancouver Coast and Shelf"]<-"Oregon, Washington, Vancouver Island"
levels(data$group_name) <- c(levels(data$group_name), "Northern and Central California")
data$group_name[data$group_name=="Northern California"]<-"Northern and Central California"

sites<-data[data$parameter=="site_slope",]
eco_sites<-sites[sites$grouping=="Ecoregion",]
prov_sites<-sites[sites$grouping=="Province",]
realm_sites<-sites[sites$grouping=="Realm",]
world_sites<-sites[sites$grouping=="World",]
means<-data[data$parameter=="mean_slope",]
eco_mu<-means[means$grouping=="Ecoregion",]
prov_mu<-means[means$grouping=="Province",]
realm_mu<-means[means$grouping=="Realm",]
world_mu<-means[means$grouping=="World",]




#############################
####ecoregion facet plots####
#############################

#pull out site info and order the data
slopes.order <- eco_sites[order(eco_sites$group_name,eco_sites$mean),]
slopes.order.group <- eco_mu[order(eco_mu$mean),]

head(slopes.order)
head(slopes.order.group)

slopes.order.group$ymin= -Inf
slopes.order.group$ymax= Inf
slopes.order$StudyName2 <- factor(slopes.order$SiteName)
slopes.order$StudyName2 <- factor(slopes.order$SiteName,
                                  levels= sample(levels(slopes.order$StudyName2),
                                                 size= nlevels(slopes.order$StudyName2),replace= F)) 
####FULL DATA SET####

### plot the results ###
library(ggplot2)
library(gridExtra)

sitedata <- slopes.order[slopes.order$Period=="1900-2015",]
ecoregiondata <- slopes.order.group[slopes.order.group$Period=="1900-2015",]
ecoregiondata$DF0 <- ifelse(sign(ecoregiondata$upper_0.9_q)==sign(ecoregiondata$lower_0.9_q),1,0)
ecoregiondata$sign<-ecoregiondata$DF0*ecoregiondata$lower_0.9_q
  
sitedata$Site2 <- 1:nrow(sitedata)

realm_info<-realm_sites[c("SiteName", "group_name")]
colnames(realm_info)[2]<-"Realm"
sitedata<-merge(sitedata, realm_info, by="SiteName")

realm_eco_info<-sitedata[c("group_name", "Realm")]
ecoregiondata<-merge(ecoregiondata, realm_eco_info, by="group_name")

mylab<-expression(paste(plain('Change relative to max'), plain(' yr'^{-1})))

for (i in 1:nlevels(factor(sitedata$Realm))){
   sitedata2 <- subset(sitedata,Realm==levels(factor(sitedata$Realm))[i])
   ecoregiondata2 <- subset(ecoregiondata,Realm==levels(factor(sitedata$Realm))[i])

plot1<-ggplot(data=sitedata2)+
  geom_rect(aes(xmax= upper_0.9_q,xmin= lower_0.9_q,ymin= ymin,ymax= ymax,fill=factor(DF0)),data= ecoregiondata2)+
  geom_vline(aes(xintercept= mean),colour= "grey20",data= ecoregiondata2, size=0.25)+
  geom_errorbarh(aes(y=Site2,x=mean,xmin=lower_0.9_q,xmax=upper_0.9_q),colour= "grey52", height=0,size=0.25)+
  geom_point(aes(y=Site2,x=mean),size= 1, colour="grey52")+
  facet_wrap(~group_name,scales= "free")+
  geom_vline(xintercept= 0,size= 1,linetype= "dotted")+
  theme_bw()+
  scale_fill_manual(values=c("grey90","red"))+
  theme(panel.grid.minor = element_line(colour = NA),
        panel.grid.major = element_line(colour = NA),
        axis.text.y = element_text(size=0),
        axis.ticks.y = element_line(size=0),
        legend.position="none",
        plot.title = element_text(size=10),
        axis.title.x = element_text(size=8),
        axis.text.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        strip.text = element_text(size=8),
        strip.text.x = element_text(size=8))+
  labs(title=sitedata2$Realm)+
  labs(x=mylab, y="Ecoregion", size=24)

assign(paste("realm_", i, sep = ""), plot1)
}

pdf(height=2.4, width=2.3, "Arctic.pdf")
print(realm_1)
dev.off()

pdf(height=6, width=6, "Temperate Australiasia.pdf")
print(realm_2)
dev.off()

pdf(height=4.2, width=6, "Temperate North Atlantic.pdf")
print(realm_3)
dev.off()

pdf(height=4.24, width=6, "Temperate Northern Pacific.pdf")
print(realm_4)
dev.off()

pdf(height=2.4, width=4.25, "Temperate South America.pdf")
print(realm_5)
dev.off()

pdf(height=2.5, width=4.25, "Temperate South Africa.pdf")
print(realm_6)
dev.off()



#########################
####ecoregion means######
#########################

##FULL data set AND by Period##
eco_mu_ALL <- eco_mu
eco_sites_ALL <- eco_sites

eco_mu_ALL$group_name<-factor(eco_mu_ALL$group_name, levels=rev(levels(eco_mu_ALL$group_name)))
eco_sites_ALL$group_name<-factor(eco_sites_ALL$group_name, levels=rev(levels(eco_sites_ALL$group_name)))

eco_mu_FULL<-eco_mu_ALL[eco_mu_ALL$Period=="1900-2015",]
eco_mu_FULL_r<-eco_mu_FULL
eco_mu_FULL_r$group_name<-factor(eco_mu_FULL$group_name, levels=eco_mu_FULL[order(eco_mu_FULL$mean), "group_name"])

eco_mu_FULL_r$DF0 <- ifelse(sign(eco_mu_FULL_r$upper_0.9_q)==sign(eco_mu_FULL_r$lower_0.9_q),1,0)
eco_mu_FULL_r$DF0<- as.factor(eco_mu_FULL_r$DF0)

eco_mu_REST<-eco_mu_ALL[!eco_mu_ALL$Period=="1900-2015",]
eco_sites_REST<-eco_sites_ALL[!eco_sites_ALL$Period=="1900-2015",]

eco_mu_REST$DF0 <- ifelse(sign(eco_mu_REST$upper_0.9_q)==sign(eco_mu_REST$lower_0.9_q),1,0)
eco_mu_REST$DF0<- as.factor(eco_mu_REST$DF0)

eco_mu_ALL<-rbind(eco_mu_FULL_r, eco_mu_REST)


levels(eco_mu_ALL$Period) <- c(levels(eco_mu_ALL$Period), "1949-2015")
eco_mu_ALL$Period[eco_mu_ALL$Period == "1900-2015"]<-"1949-2015"

levels(eco_sites_ALL$Period) <- c(levels(eco_sites_ALL$Period), "1949-2015")
eco_sites_ALL$Period[eco_sites_ALL$Period == "1900-2015"]<-"1949-2015"

eco_mu_ALL$Period<-factor(eco_mu_ALL$Period, levels=rev(levels(eco_mu_ALL$Period)))
eco_sites_ALL$Period<-factor(eco_sites_ALL$Period, levels=rev(levels(eco_sites_ALL$Period)))

mylab<-expression(paste(bold('Change relative to max'), bold(' yr'^{-1})))

pdf(width= 20,height= 15,"slopes_ecoregion_ALL_95.pdf")
ggplot(data=eco_mu_ALL) + 
  geom_point(aes(x=mean, y=group_name, colour=DF0), size=7)+
  scale_colour_manual(values=c("black","red"))+
  geom_errorbarh(aes(y=group_name,x=mean,xmin=lower_0.9_q,xmax=upper_0.9_q), height=0,size= .5, facet=TRUE)+
  geom_vline(xintercept= 0,size= 1,linetype= "dotted")+
  geom_point(aes(x=mean, y=group_name), data=eco_sites_ALL, size=2, position="jitter", alpha=0.15)+
  facet_grid(~Period)+
  labs(x=mylab, y="Ecoregion")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), 
        legend.position="none", 
        axis.title.x=element_text(size=28, face="bold"),
        axis.title.y=element_text(size=28, face="bold"),
        axis.text=element_text(size=24),
        strip.text = element_text(size=28, face="bold"),
        strip.background=element_blank())
dev.off()


#########################
####world plot###########
#########################

#pull out site info and order the data
slopes.order <- world_sites[order(world_sites$group_name,world_sites$mean),]
slopes.order.group <- world_mu[order(world_mu$mean),]

head(slopes.order)
head(slopes.order.group)

slopes.order.group$ymin= -Inf
slopes.order.group$ymax= Inf
slopes.order$StudyName2 <- factor(slopes.order$SiteName)
slopes.order$StudyName2 <- factor(slopes.order$SiteName,
                                  levels= sample(levels(slopes.order$StudyName2),
                                                 size= nlevels(slopes.order$StudyName2),replace= F)) 
####FULL DATA SET####

### plot the results ###
library(ggplot2)

sitedata <- slopes.order[slopes.order$Period=="1900-2015",]
worlddata <- slopes.order.group[slopes.order.group$Period=="1900-2015",]
worlddata$DF0 <- ifelse(sign(worlddata$upper_0.9_q)==sign(worlddata$lower_0.9_q),1,0)
sitedata$Site2 <- 1:nrow(sitedata)


pdf(width= 15,height= 15,"slopes_facets_world_FULL_95.pdf")
ggplot(data=sitedata)+
  geom_rect(aes(xmax= upper_0.9_q,xmin= lower_0.9_q,ymin= ymin,ymax= ymax,fill=factor(DF0)),data= worlddata)+
  geom_vline(aes(xintercept= mean),colour= "grey20",data= worlddata)+
  geom_errorbarh(aes(y=Site2,x=mean,xmin=lower_0.9_q,xmax=upper_0.9_q), colour= "grey20", height=0,size= .5, alpha=0.4)+
  geom_point(aes(y=Site2,x=mean),colour="grey20", size= 3)+
  geom_vline(xintercept= 0,size= 1,linetype= "dotted")+
  theme_bw()+
  scale_fill_manual(values=c("red"))+
  theme(panel.grid.minor = element_line(colour = NA),
        panel.grid.major = element_line(colour = NA),
        axis.text.y= element_text(size=0),
        axis.text.x= element_text(size=28),
        axis.ticks.y= element_line(size=0, colour="white"),
        legend.position="none",
        plot.title = element_text(size=0),
        axis.title.x = element_text(size=30),
        axis.title.y = element_text(size=30))+
  ylab("Site")+xlab(mylab)
dev.off()




 