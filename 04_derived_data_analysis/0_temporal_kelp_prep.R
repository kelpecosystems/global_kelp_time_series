#####################
# Script to prep the kelp timeseries data
# for other analyses yielding 
# rawData - the rawData loaded and cleaned
# rawDataMod - the rawData loaded and cleaned with no singletons
# rawDataMod2 - the rawData loaded and cleaned with no singletons or doubletons
# rawDataMod_3points_3years - the rawData loaded and cleaned with
#                             n years>=3 and 3 data points >=3
#
# Last Changed 11/1/2015
# 11/1/2015 - fixed too few years bug in shortData2
# 9/12/2015 - added shortData2 to filter by nyears and n data points
#####################

library(plyr)
library(ggplot2)

getValues <- function (alist) {
  avec <- vector()
  sapply(alist, function(x) avec <<- c(avec, x)) -> blank
  avec
}




rawData <- read.csv("../03_derived_data/CleanDataWithRegions.csv")

rawData$sasDate <- as.Date(as.character(rawData$sasDate), format="%m/%d/%Y")
#get - out of ecoregions
rawData$ECOREGION <- as.character(rawData$ECOREGION )
#rawData$ECOREGION <- gsub("/", "-", rawData$ECOREGION )
rawData$ECOREGION <- factor(rawData$ECOREGION)

###########
#deal with sites that only have one or sample point and exclude them
###########
singletons <- ddply(rawData, .(study_ID, Study, Site, focalUnit), summarize, len = length(focalKelp))
singletons <- singletons[which(singletons$len==1),]

singletonIDX <- apply(singletons, 1, function(arow) which(rawData$Study==arow[2] & rawData$Site==arow[3] & rawData$focalUnit==arow[4]))
if(length(singletonIDX)>0) rawDataMod <- rawData[-singletonIDX,]

###########
#deal with sites that only have no kelp
###########
nokelp <- ddply(rawDataMod, .(study_ID, Study, Site), summarize, kelp = sum(focalKelp, na.rm=T))
nokelp <- nokelp[which(nokelp$kelp==0),]

nokelpIDX <- getValues(apply(nokelp, 1, function(arow) which(rawDataMod$Study==arow[2] & rawDataMod$Site==arow[3]))) #used getvalues b/c list return
if(length(nokelpIDX)>0) rawDataMod <- rawDataMod[-nokelpIDX,]

#############
# Rebalance factor levels
#############
rawDataMod$Study <- factor(rawDataMod$Study)
rawDataMod$Site <- factor(rawDataMod$Site)

rawDataMod$StudySite <- with(rawDataMod, paste(Study, Site, study_ID,trajectory_ID,sep="-"))

#############
# Add a more meaningful date label
#############
rawDataMod$Date <- as.numeric(gsub("-", "", rawDataMod$sasDate))

rawDataMod$DaysSince1900 <- as.numeric(julian(as.POSIXlt(rawDataMod$sasDate, format = "%y-%m-%d"), 
       origin = as.POSIXct("1900-01-01", tz = "UTC")))

rawDataMod$YearsSince1900 <- rawDataMod$DaysSince1900/365.25
rawDataMod$Decade <- floor(rawDataMod$year/10)*10

###########
# Create some standardized columns
#
# I bet this is easier with dplyr, but no time to learn
###########
stdMax <- function(x, samp=3) {
  m <- mean(sort(x, decreasing=T, na.last=T)[1:samp])
  x/m
}

ln_stdMax <- function(x, samp=3) {
  m <- mean(sort(x, decreasing=T, na.last=T)[1:samp])
  log(x/m+1/m)
}

stdZ <- function(x)  (x)/sd(x, na.rm=T)

rawDataMod <- ddply(rawDataMod, .(ECOREGION, focalUnit), function(adf){
  adf$stdByECOREGION <- stdMax(adf$focalKelp)
  adf$stdByECOREGIONZ <- stdZ(adf$focalKelp)
  adf$ln_stdByECOREGION <- ln_stdMax(adf$focalKelp)
  adf
}
)

rawDataMod <- ddply(rawDataMod, .(REALM, focalUnit), function(adf){
  adf$stdByREALM <-  stdMax(adf$focalKelp)
  adf$stdByREALMZ <- stdZ(adf$focalKelp)
  adf$ln_stdByREALM <- ln_stdMax(adf$focalKelp)
  adf
}
)


rawDataMod <- ddply(rawDataMod, .(PROVINCE, focalUnit), function(adf){
  adf$stdByPROVINCE <-  stdMax(adf$focalKelp)
  adf$stdByPROVINCEZ <- stdZ(adf$focalKelp)
  adf$ln_stdByPROVINCE <- ln_stdMax(adf$focalKelp)
  adf
}
)

#########
# Add a relative Latitude by Realm
#########

rawDataMod <- ddply(rawDataMod, .(REALM), function(adf){
  adf$realLatByRealm <-  abs(adf$Latitude)-min(abs(adf$Latitude))
  adf
}
)

###########
#make subdatasets of studies that have n or greater points
###########
shortData <- function(studyPoints=2, d = rawDataMod){
  
  smalls <- ddply(d, .(study_ID, Study, Site, trajectory_ID, focalUnit), summarize, len = length(focalKelp))
  smalls <- smalls[which(smalls$len<=studyPoints),]
  
  smallsIDX <- apply(smalls, 1, function(arow) which(d$Study==arow[2] & d$Site==arow[3] & d$focalUnit==arow[5]))
  smallsIDX <- getValues(smallsIDX)
  if(length(smallsIDX)>0) return( d[-smallsIDX,])
  
  return(NA)
}



shortData2 <- function(studyPoints=3, studyYears=3, d = rawDataMod){
  require(dplyr)
  
  #group the data
  d %>% group_by(study_ID, Study, Site, trajectory_ID, focalUnit) %>%
    
    # what is the study length and number of data points  
    mutate(len=n(), num_years = max(year) - min(year)) %>%
    ungroup() %>%
    #boot studies without studyPoints or more data points
    filter(len >= studyPoints) %>%
    #boot studies with fewer than studyYears years
    
    filter(num_years>=studyYears-1)
  
  
  
}
#add filter by year?

###########
#dataset of sites that only have > two sample points (i.e., 3 or more points)
###########
rawDataMod2 <- shortData()
rawDataMod_3points_3years <- shortData2()

length(unique(rawDataMod2$trajectory_ID))
length(unique(rawDataMod_3points_3years$trajectory_ID))


###########
#dataset of sites that only >= five sample points 
###########
rawDataMod5 <- shortData(5)
rawDataMod_5points_3years <- shortData2(5, 3)

