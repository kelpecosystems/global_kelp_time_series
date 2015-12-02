###Simple code to plot number of sampling points by year###
library(dplyr)

setwd("~/Dropbox/nceas_kelp_climate_2013/temporal_change/github_repo/03_derived_data")
rw<-read.csv("CleanDataWithRegions.csv")
rw$uniq<-paste(rw$year, rw$Site, sep="_")
year<-rw[,c("year", "uniq")]
sw<-unique(year)
y<-table(sw$year)
yc<-cbind(y)

pdf(width= 7,height= 4,"n_by_year.pdf")
par(mar=c(5.1,5,4.1,2.1))
plot(y~row.names, data=yc)
barplot(y, xlab="Year", ylab="Number of Sites", ylim=c(0,800), cex.lab=1.5)
dev.off()



sites_year<-sw %>% 
  group_by(year) %>% 
  summarise(count=sum(id))

library(plyr)

s_n<-rw[,c("StudySite", "year")]
s_n<-unique(s_n)
site.freq<-table(s_n$year)
si_f<-cbind(site.freq)

pdf(width= 7,height= 4,"sites_by_year.pdf")
barplot(site.freq, xlab="Year", ylab="Number of Sites", cex.lab=1.5)
dev.off()