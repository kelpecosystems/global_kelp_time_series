###Simple code to plot number of sampling points by year###
library(dplyr)

rw<-read.csv("../03_derived_data/CleanDataWithRegions.csv")
rw$uniq<-paste(rw$year, rw$Site, sep="_")
rw$StudySite <- paste(rw$Study, rw$Site, rw$trajectory_ID,sep="-")
year<-rw[,c("year", "uniq")]
sw<-unique(year)
y<-table(sw$year)
yc<-data.frame(cbind(y))
yc$year <- row.names(yc)
  
pdf(width= 7,height= 4,"../Figures/n_by_year.pdf")
par(mar=c(5.1,5,4.1,2.1))
plot(y~year, data=yc)
barplot(y, xlab="Year", ylab="Number of Sites", ylim=c(0,800), cex.lab=1.5)
dev.off()



sites_year<-sw %>% 
  group_by(year) %>% 
  summarise(count=length(id))

library(plyr)

s_n<-rw[,c("StudySite", "year")]
s_n<-unique(s_n)
site.freq<-table(s_n$year)
si_f<-cbind(site.freq)

pdf(width= 7,height= 4,"../Figures/sites_by_year.pdf")
barplot(site.freq, xlab="Year", ylab="Number of Sites", cex.lab=1.5)
dev.off()

library(tidyr)
dur <- rw %>% 
  group_by(trajectory_ID) %>%
  dplyr::summarise(Duration = max(year) - min(year)) %>%
  ungroup() %>%
  group_by(Duration) %>%
  dplyr::summarise(num_sites = length(Duration)) %>%
  ungroup() %>%
  filter(Duration>2)  %>%
  complete(Duration, fill=list(num_sites=0))


durTab <- dur$num_sites
names(durTab) <- dur$Duration

pdf(width= 7,height= 4,"../Figures/duration_distribution.pdf")
par(mar=c(5.1,5,4.1,2.1))
barplot(durTab, xlab="Study Duration", 
        ylab="Number of Sites", cex.lab=1.5)
pdf()
