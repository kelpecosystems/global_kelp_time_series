library(plyr)
setwd("~/Dropbox/nceas_kelp_climate_2013/temporal_change/R/analysis_scripts")
source("./temporal_kelp_prep.R")

format_data <- function(x) {
    DATA <- x
    DATA$ProvinceSite <- DATA$PROVINCE:DATA$Site
    DATA$EcoregionSite <- DATA$ECOREGION:DATA$Site
    
    ### add julian date
    DATA$julian <- julian(DATA$sasDate)
    
    ### summarize Study and site info to gather info about lengths
    data_summary <- ddply(DATA,.(Study),
                          summarize,nsites =length(unique(StudySite)))
    
    data_summary2 <- ddply(DATA,.(Study,StudySite,Site),
                           summarize,nyears =length(unique(year)))
    
    data_summary3 <- ddply(DATA,.(PROVINCE),
                           summarize,nsites =length(unique(ProvinceSite)))
    
    data_summary4 <- ddply(DATA,.(ECOREGION),
                           summarize,nsites =length(unique(EcoregionSite)))
    
    data_summary5 <- ddply(DATA,.(Study,StudySite,Site),
                           summarize,length =(max(julian)-min(julian))/365.25)
    
    ggplot(data= data_summary5)+geom_histogram(aes(length),breaks=seq(0,35, by= 0.25),fill= "blue",colour="black")+theme_bw()+scale_x_continuous(breaks= 0:35)+xlab("time series length")+ylab("number of sites")
    
    ggplot(data= data_summary2)+geom_histogram(aes(nyears),breaks=0:35,fill= "blue",colour="black")+theme_bw()+scale_x_continuous(breaks= 0:35+0.5,labels= 0:35)+xlab("calendar years included")+ylab("number of sites")
    
    
    # ### more than minyears years, more than minsites sites ###
    # data_summaryb <- subset(data_summary,nsites>=min_sites_study)
    # data_summary2b <- subset(data_summary2,nyears>=minyears)
    # data_summary3b <- subset(data_summary3,nsites>=min_sites_province)
    # data_summary4b <- subset(data_summary4,nsites>=min_sites_ecoregion)
    
    # ### add only data that match the above criteria
    # DATA_SUBSET <- subset(DATA,Study%in%data_summaryb$Study&
    #                         StudySite%in%data_summary2b$StudySite&
    #                         PROVINCE%in%data_summary3b$PROVINCE&
    #                         ECOREGION%in%data_summary4b$ECOREGION)
    DATA_SUBSET <- DATA
    ### center julian days for regression
    datsummary <- ddply(DATA,.(StudySite),summarize,meanjul= mean(julian,na.rm= T))
    DATA_SUBSET <-join(DATA_SUBSET,datsummary)
    
    ### get site, study and province number 
    DATA_SUBSET$SiteN <-  as.numeric(factor(DATA_SUBSET$StudySite))
    DATA_SUBSET$StudyN <-  as.numeric(factor(DATA_SUBSET$Study))
    DATA_SUBSET$ProvinceN <-  as.numeric(factor(DATA_SUBSET$PROVINCE))
    DATA_SUBSET$EcoregionN <-  as.numeric(factor(DATA_SUBSET$ECOREGION))
    hierarchical_mod_data <- subset(DATA_SUBSET, !is.na(stdByPROVINCE)&!is.na(stdByECOREGION))
    hierarchical_mod_data
}

DATA3_YEARS <- format_data(rawDataMod_3points_3years)
DATA3_POINTS <- format_data(rawDataMod2)
setwd("./Hierarchical_LME")
write.csv(DATA3_YEARS,"formatted_data_3years.csv",row.names= F)
write.csv(DATA3_POINTS,"formatted_data_3points.csv",row.names= F)


