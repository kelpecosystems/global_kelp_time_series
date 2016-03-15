######################################################
###  Script to sample model posteriors             ###
###  for the hierarchical linear model             ###
###  Author:  D.K. Okamoto                         ###
######################################################

library(rstan);library(plyr);library(gdata);library(ggplot2)
library(parallel);library(grid);library(coda);library(rdrop2)
library(reshape2);library(quantreg);library(dplyr)

cat("Stan version:", stan_version(), "\n")
rstan_options(auto_write = TRUE)

### clear old data ###
rm(list=ls())

### be sure to set working directory to the github repo
setwd()

source("05_HLM_analysis_code/01_data_formatting.R")
setwd("../05_HLM_analysis_code/")

### min number of years and observations in each analysis ###
min_years =2
min_obs =3
min_num_sites = 3

### list of parameters to save ###
params_for_stan <- c("beta","beta_mu",
                     "sd_e","A",
                     "y_loc","y_loc_mu",
                     "L_vcov_beta","L_omega_beta",
                     "resid","log_lik")

### MCMC details
MCMC_details <- list(n.iter = 3000,
                     n.burnin = 1500,
                     set.seed = 234,
                     n.chains = 3,
                     n.thin = 1)

### fetch the model ###
source("stan_model.R")

### fetch the necessary summary functions ###
source("stan_functions.R")

### fetch plotting functions ###
source("plot_funs.R")

### compile model
opt_model<-stan_model(model_code=HLM_model)


for (j in 1:2){ 
  if(j==1){
    kelpdata <- read.csv("formatted_data_3points.csv")
  }
  else{
    kelpdata <- read.csv("formatted_data_3years.csv")
  }

  ### array of sampling year constraints (each column is a sampling period)
  year_bounds <- array(c(1900,2015,1983,1992,1993,2002,2003,2012),dim= c(2,4))
  
    model_data <- with(kelpdata,data.frame(list(
                                                ProvinceName = as.character(PROVINCE),
                                                EcoregionName = as.character(ECOREGION),
                                                RealmName = as.character(REALM),
                                                WorldName = as.character("Whole World"),
                                                StudyName=as.character(Study),
                                                SiteName= as.character(StudySite),
                                                SiteMethod= focalUnit,
                                                y=stdByECOREGION+0.01,
                                                Year= YearsSince1900+1900),
                                           stringsAsFactors=FALSE))
    
    ### order data
    model_data <- model_data[order(model_data$SiteName),]
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
  
    if(j==1){
      save(model_list, all_summaries,file= paste0("../06_HLM_output/",
        paste(year_bounds[,i],collapse= "-"),
          "_3_points",
          "_",Sys.Date(),".RData"))
    }
    else {
      save(model_list, all_summaries,file= paste0("../06_HLM_output/",
        paste(year_bounds[,i],collapse= "-"),
        "_3_years",
        "_",Sys.Date(),".RData"))
    }
  }
  if(j==1){
    save(combined_summaries,file= paste0("../06_HLM_output/combined_model_summaries",
                                         "_3_points_",Sys.Date(),".RData"))
    write.csv(combined_summaries,"../06_HLM_output/site_slopes_3_points.csv",row.names= F)
  }
  else {
    save(combined_summaries,file= paste0("../06_HLM_output/combined_model_summaries",
                                         "_3_years_",Sys.Date(),".RData"))
    write.csv(combined_summaries,"../06_HLM_output/site_slopes_3_years.csv",row.names= F)
  }   
}


### set plotting options
options <- theme(strip.text.x = element_text(size =6),
                 panel.grid.major.y = element_line(colour = NA),
                 panel.grid.major.x = element_line(colour = "grey80"),
                 panel.grid.minor.x = element_line(colour = "NA"),
                 panel.grid.minor.y = element_line(colour = "NA"),
                 plot.background=element_rect(fill=NA),
                 strip.background = element_rect(fill= NA,colour= NA,size= 0),
                 legend.text = element_text(size=14),
                 legend.background = element_rect(colour = NA,fill = NA, size = .25),
                 axis.ticks.length = unit(0.25,"lines"),
                 panel.background=element_rect(fill= NA),
                 legend.direction="vertical",
                 legend.position= "bottom",
                 panel.border= element_rect(fill= NA,colour= "black"),
                 legend.key.width = unit(1,"lines"),
                 legend.key= element_rect(fill= NA),
                 legend.key.height = unit(1,"lines"),
                 axis.text=element_text(size=16),
                 axis.title.y=element_text(size=16),
                 axis.title.x=element_blank(),
                 legend.key.size = unit(1,"lines"),
                 plot.margin = unit(rep(0.3, 4), "inches"))

### generate plots for each of the j groupings and i groups within each grouping 
### plotting code is in a separate function file
load("../06_HLM_output/1900-2015_3_points_2016-03-11.RData")
load("../06_HLM_output/combined_model_summaries_3_points_2016-03-11.RData")
source("plot_funs.R")

for (j in 1:4){
  ### get model results
  model_results <- model_list[[j]]$data_pred
  model_results$x <- model_results$Year- mean(model_results$Year)
  
  ### generate site level mean predictions ###
  model_results$pred <-  exp(colMeans(model_list[[j]]$chains$y_loc))
  
  ### get list of group IDs ###
  group_ID <- subset(model_list[[j]]$summary,parameter=="mean_slope")$group_name
  
  ### generate PDFs (while setting heights of plots equal using grobs ###
  ### be patient!! the quantile regressions to generate smooth credible sets takes a bit.
  pdf(width= 8, height= 6,file= paste("../Figures/full_dataset_predictions_",  grouping_list[j],".pdf",sep= ""))
  if(j==1){
    a <- plot_fun_1(23,quant_method="other")
    gA <- ggplotGrob(a$p1)
    gB <- ggplotGrob(a$p2)
    for (i in 1:nlevels(factor(model_results$group_name))){
      b <- plot_fun_1(i,quant_method="other")
      gAa <- ggplotGrob(b$p1)
      gBb <- ggplotGrob(b$p2)
      gCc <- ggplotGrob(b$p3)
      gAa$heights <- gA$heights
      gBb$heights <- gB$heights
      grid.draw(gAa)
      grid.newpage()
      grid.draw(gBb) 
      grid.newpage()
      grid.draw(gCc) 
      grid.newpage()
    }
  }
  if (j==2){
    a <- plot_fun_1(4,quant_method="other")
    gA <- ggplotGrob(a$p1)
    gB <- ggplotGrob(a$p2)
    for (i in 1:nlevels(factor(model_results$group_name))){
      b <- plot_fun_1(i,quant_method="other")
      gAa <- ggplotGrob(b$p1)
      gBb <- ggplotGrob(b$p2)
      gCc <- ggplotGrob(b$p3)
      gAa$heights <- gA$heights
      gBb$heights <- gB$heights
      grid.draw(gAa)
      grid.newpage()
      grid.draw(gBb) 
      grid.newpage()
      grid.draw(gCc) 
      grid.newpage()
    }
  }
  if (j%in%c(3,4)){
    for (i in 1:nlevels(factor(model_results$group_name))){
      a <- plot_fun_2(i,quant_method="other")
      
      print(a$p1)
      print(a$p2)
      print(a$p3)
    }
  }
  dev.off()
}