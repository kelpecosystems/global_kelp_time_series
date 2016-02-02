######################################################
###  Script to sample model posteriors             ###
###  for the hierarchical linear model             ###
###  Author:  D.K. Okamoto                         ###
######################################################

library(rstan);library(plyr);library(gdata);library(ggplot2)
library(parallel);library(grid);library(coda);library(rdrop2)

cat("Stan version:", stan_version(), "\n")
rstan_options(auto_write = TRUE)

### read in dropbox token to be able to write to dropbox
token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)

### clear old data ###
rm(list=ls())
setwd("../")
source("05_HLM_analysis_code/01_data_formatting.R")
setwd("../05_HLM_analysis_code/")

#kelpdata <- read.csv("formatted_data_3years.csv")
kelpdata <- read.csv("formatted_data_3points.csv")

### min number of years and observations in each analysis ###
min_years =2
min_obs =3
min_num_sites = 3

### list of parameters to save ###
params_for_stan <- c("beta","beta_mu",
           "sd_e","y_loc","y_loc_mu",
           "L_vcov_beta","L_omega_beta",
            "resid","log_lik")

### MCMC details
MCMC_details <- list(n.iter = 1500,
                     n.burnin = 500,
                     set.seed = 234,
                     n.chains = 2,
                     n.thin = 1)

### fetch the model ###
source("stan_model.R")

### fetch the necessary summary functions ###
source("stan_functions.R")

### array of sampling year constraints (each column is a sampling period)
year_bounds <- array(c(1900,2015,1983,1992,1993,2002,2003,2012),dim= c(2,4))

  model_data <- with(kelpdata,data.frame(list(
                                              ProvinceName = as.character(PROVINCE),
                                              EcoregionName = as.character(ECOREGION),
                                              RealmName = as.character(REALM),
                                              WorldName = as.character("Whole World"),
                                              StudyName=as.character(Study),
                                              SiteName= as.character(StudySite),
                                              Unit= focalUnit,
                                              y=stdByECOREGION+0.001,
                                              Year= YearsSince1900+1900),
                                         stringsAsFactors=FALSE))
  
  ### order data
  model_data <- model_data[order(model_data$Site),]
  grouping_list <- c("Ecoregion","Province","Realm","World")
  
for (i in 1:ncol(year_bounds)){
  ### make sure each group has the minimum number of sites
  data_subset <- years_subset(model_data,min_obs,x_min=year_bounds[1,i], x_max=year_bounds[2,i])

  ### make sure each group has the minimum number of sites
  data_subset <- min_sites_subset(data_subset,"EcoregionName",min_sites=min_num_sites)
  
  ### wrapper using correct data, parameters and mcmc details to pass to mclapply for parallel sampling
  fit.fun <- function(x) {
    HLM_stan_fit(group=x, data= data_subset,params= params_for_stan, MCMC_details= MCMC_details)
  }  
  
  model_list <- mclapply(grouping_list,fit.fun,mc.cores= 4)
  
  ### combine the summary data for each model 
  all_summaries <- rbind.fill(lapply(model_list,function(x) x$summary))
  
  ### provide year boundaries for the summaries
  all_summaries$Period <- paste(year_bounds[,i],collapse= "-")
 
  ### combine the output
  if(i==1){
    combined_summaries <- all_summaries
  }
  else {
    combined_summaries <- rbind.fill(combined_summaries,all_summaries)
  }
 
  ### save individual model output
   save(model_list, all_summaries,file= paste0("../06_HLM_output/",
                               paste(year_bounds[,i],collapse= "-"),
                               "_3_points",
                               "_",Sys.Date(),".RData"))
}

save(combined_summaries,file= paste0("../06_HLM_output/combined_model_summaries",
                                            paste(year_bounds[,i],collapse= "-"),
                                  "_3_points",
                                  "_",Sys.Date(),".RData"))

write.csv(combined_summaries,"../06_HLM_output/site_slopes_3_points.csv",row.names= F)


### upload huge file to dropbox ###
for (i in 1:ncol(year_bounds)){
drop_upload(file= paste0("../",
                     paste(year_bounds[,i],collapse= "-"),
                     "_3_points",
                     "_",Sys.Date(),".RData"),
            dest = "nceas_kelp_data/temporal_change_big/HLME_Output",
            dtoken= token)
  
drop_upload(file= paste0("../",
                           paste(year_bounds[,i],collapse= "-"),
                           "_",Sys.Date(),".RData"),
            dest = "nceas_kelp_data/temporal_change_big/HLME_Output",
            dtoken= token)  
}