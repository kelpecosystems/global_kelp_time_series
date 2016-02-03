######################################################
###  Script to sample model posteriors             ###
###  for the hierarchical linear model             ###
###  Author:  D.K. Okamoto                         ###
######################################################

library(rstan);library(plyr);library(gdata);library(ggplot2)
library(parallel);library(grid);library(coda);library(scales)

cat("Stan version:", stan_version(), "\n")
rstan_options(auto_write = TRUE)

### read in dropbox token to be able to write to dropbox
token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)

### clear old data ###
rm(list=ls())
setwd("~/Dropbox/nceas_kelp_climate_2013/temporal_change/github_repo/")
source("05_HLM_analysis_code/01_data_formatting.R")
setwd("~/Dropbox/nceas_kelp_climate_2013/temporal_change/github_repo/05_HLM_analysis_code/")

#kelpdata <- read.csv("formatted_data_3years.csv")

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
  kelpdata <- read.csv("formatted_data_3points.csv")
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
                               "_",".RData"))
}

save(combined_summaries,file= paste0("../06_HLM_output/combined_model_summaries",
                                            paste(year_bounds[,i],collapse= "-"),
                                  "_3_points",
                                  "_",".RData"))

write.csv(combined_summaries,"../06_HLM_output/site_slopes_3_points.csv",row.names= F)



load("~/Dropbox/nceas_kelp_data/temporal_change_big/HLME_Output/1900-2015_3_points_2016-02-01.RData")
load("~/Dropbox/nceas_kelp_climate_2013/temporal_change/github_repo/06_HLM_output/combined_model_summaries_3_points.RData")
        
options <- theme(strip.text.x = element_text(size =12),
                 panel.grid.major.y = element_line(colour = NA),
                 panel.grid.major.x = element_line(colour = "grey80"),
                 panel.grid.minor.x = element_line(colour = "NA"),
                 panel.grid.minor.y = element_line(colour = "NA"),
                 plot.background=element_rect(fill=NA),
                 strip.background = element_rect(fill= NA,colour= NA,size= 0),
                 legend.key = element_blank(),
                 legend.background = element_rect(colour = "NA",fill = NA, size = .25),
                 axis.ticks.length = unit(0.25,"lines"),
                 panel.background=element_rect(fill= NA),
                 legend.direction="vertical",
                 legend.position= "left",
                 panel.border= element_rect(fill= NA),
                 legend.key.width = unit(2,"lines"),
                 legend.key.height = unit(1,"lines"),
                 legend.key.size = unit(1,"lines"),
                 plot.margin = unit(rep(0.2, 4), "inches"))

for (j in 1:4){
  model_results <- model_list[[j]]$data_pred
  model_results$pred <-  exp(colMeans(model_list[[j]]$chains$y_loc))
  model_results$pred2 <-  exp(colMeans(model_list[[j]]$chains$y_loc_mu))
  model_results$pred_CIU <-  exp(apply(model_list[[j]]$chains$y_loc_mu,2,quantile,probs= 0.975))
  model_results$pred_CIL <-  exp(apply(model_list[[j]]$chains$y_loc_mu,2,quantile,probs= 0.025))
  pdf(width= 11, height= 8.5,file= paste("../Figures/full_dataset_predictions_",  grouping_list[j],".pdf",sep= ""))
  for (i in 1:length(unique(model_results$group_name))){
    data <- subset(model_results,group_name==unique(model_results$group_name)[i])
    max <- max(data$y)*1.15
    params <- subset(combined_summaries,group_name==unique(model_results$group_name)[i]&parameter%in%c("mean_slope","mean_intercept")&Period=="1900-2015"&grouping==grouping_list[j])[,c("mean","parameter","p")]
    p_val <- ifelse(params$p[params$parameter=="mean_slope"]>0.95|params$p[params$parameter=="mean_slope"]<0.05,1,3)
    plot1 <- ggplot(aes(x=Year),data=data)+
      geom_ribbon(aes(y=pred2,ymax= pred_CIU,ymin= pred_CIL),fill= alpha("grey70",0.5))+
      geom_point(aes(y=y,colour= SiteName,shape= StudyName),alpha= 0.5,size= 3)+
      geom_line(aes(y=pred,colour= SiteName))+
      geom_line(aes(y=y,colour= SiteName),linetype= 3,size=0.1)+
      geom_line(aes(y=pred2),linetype= p_val)+
      ylab("standardized kelp + 0.001")+
      options+
      xlab("Year")+
      scale_shape_manual(values= c(16:17,3,7,8,13)[(1:length(unique(data$Study)))])+
      guides(colour= FALSE,shape= guide_legend(title= "Study",title.hjust= 0.5))+
      coord_cartesian(ylim= c(0.001,max))+
      ggtitle(paste(unique(model_results$group_name)[i],  grouping_list[j],sep= ";"))
    print(plot1)
  }
  dev.off()
  
  pdf(width= 11, height= 8.5,file= paste("../Figures/full_dataset_predictions_log",  grouping_list[j],".pdf",sep= ""))
  for (i in 1:length(unique(model_results$group_name))){
    data <- subset(model_results,group_name==unique(model_results$group_name)[i])
    max <- max(data$y)*1.15
    min <- ifelse(min(data$y)==0.001,0.0009,min(data$y)*0.87)
    params <- subset(combined_summaries,group_name==unique(model_results$group_name)[i]&parameter%in%c("mean_slope","mean_intercept")&Period=="1900-2015"&grouping==grouping_list[j])[,c("mean","parameter","p")]
    p_val <- ifelse(params$p[params$parameter=="mean_slope"]>0.95|params$p[params$parameter=="mean_slope"]<0.05,1,3)
    plot1 <- ggplot(aes(x=Year),data=data)+
      geom_ribbon(aes(y=pred2,ymax= pred_CIU,ymin= pred_CIL),fill= alpha("grey70",0.5))+
      geom_point(aes(y=y,colour= SiteName,shape= StudyName),alpha= 0.5,size= 3)+
      geom_line(aes(y=pred,colour= SiteName))+
      geom_line(aes(y=y,colour= SiteName),linetype= 3,size=0.1)+
      geom_line(aes(y=pred2),linetype= p_val)+
      ylab("standardized kelp + 0.001")+
      scale_y_continuous(trans= "log10",labels = trans_format("log10", math_format(10^.x)),expand= c(0,0))+
      annotation_logticks(base=10,sides= "l")+
      options+
      scale_shape_manual(values= c(16:17,3,7,8,13)[(1:length(unique(data$Study)))])+
      xlab("Year")+
      guides(colour= FALSE,shape= guide_legend(title= "Study",title.hjust= 0.5))+
      coord_cartesian(ylim= c(min,max))+
      ggtitle(paste(unique(model_results$group_name)[i],  grouping_list[j],sep= ";"))
    print(plot1)
  }
  dev.off()
}
