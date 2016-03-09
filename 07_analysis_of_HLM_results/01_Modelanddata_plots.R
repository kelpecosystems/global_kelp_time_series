setwd("~/Dropbox/nceas_kelp_climate_2013/temporal_change/github_repo/06_HLM_output")

load("combined_model_summaries_3_points.RData")
load("~/Dropbox/nceas_kelp_data/temporal_change_big/HLME_Output/model_summaries1900-2015_3_points.RData")
library(rstan);

library(plyr);library(gdata);library(ggplot2)
library(parallel);library(grid);library(coda)
grouping_list <- c("Ecoregion","Province","Realm","World")

for (j in 1:4){
    model_results <- model_list[[j]]$data_pred
    options <- theme(strip.text.x = element_text(size =12),
                     panel.grid.major.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     panel.margin= unit(0.5,"lines"),
                     plot.background=element_rect(fill=NA),
                     strip.background = element_rect(fill= NA,colour= NA,size= 0),
                     legend.key = element_blank(),
                     legend.background = element_rect(colour = "NA",fill = NA, size = .25),
                     legend.position = c(.2,0.9),
                     axis.ticks.length = unit(-0.25,"lines"),
                     panel.background=element_rect(fill= NA),
                     legend.direction="horizontal",
                     legend.title = element_text(size = 0),
                     panel.border= element_rect(fill= NA),
                     legend.key.width = unit(2,"lines"),
                     plot.margin = unit(rep(1, 4), "inches"))
    
  model_results$pred <-  exp(colMeans(model_list[[j]]$chains$y_loc))
  model_results$pred2 <-  exp(colMeans(model_list[[j]]$chains$y_loc_mu))
  model_results$pred_CIU <-  exp(apply(model_list[[j]]$chains$y_loc_mu,2,quantile,probs= 0.975))
  model_results$pred_CIL <-  exp(apply(model_list[[j]]$chains$y_loc_mu,2,quantile,probs= 0.025))
  
    pdf(width= 6, height= 6,file= paste("../Figures/full_dataset_predictions_",  grouping_list[j],".pdf",sep= ""))
      for (i in 1:length(unique(model_results$group_name))){
        data <- subset(model_results,group_name==unique(model_results$group_name)[i])
        max <- data$y*1.2
        params <- subset(combined_summaries,group_name==unique(model_results$group_name)[i]&parameter%in%c("mean_slope","mean_intercept")&Period=="1900-2015"&grouping==grouping_list[j])[,c("mean","parameter","p")]
      
        p_val <- ifelse(params$p[params$parameter=="mean_slope"]>0.95|params$p[params$parameter=="mean_slope"]<0.05,1,3)
        plot1 <- ggplot(aes(x=Year),data=data)+
          geom_ribbon(aes(y=pred2,ymax= pred_CIU,ymin= pred_CIL),fill= "grey90")+
          geom_point(aes(y=y,colour= SiteName,group= SiteName),alpha= 0.5,size= 2)+
          geom_line(aes(y=pred,colour= SiteName,group= SiteName))+
          geom_line(aes(y=y,colour= SiteName,group= SiteName),linetype= 3,size=0.1)+theme_bw()+
          theme(legend.position="none")+
          geom_line(aes(y=pred2),linetype= p_val)+
          ylab("Standardized Kelp Abundance")+
          xlab("Year")+
          coord_cartesian(ylim= c(0.001,max))+
          ggtitle(paste(unique(model_results$group_name)[i])) 
        print(plot1)
      }
    dev.off()
}

pdf(width= 6, height= 6,file= paste("../Figures/full_dataset_predictions_log",  grouping_list[j],".pdf",sep= ""))
for (i in 1:length(unique(model_results$group_name))){
  data <- subset(model_results,group_name==unique(model_results$group_name)[i])
  max <- data$y*1.2
  params <- subset(combined_summaries,group_name==unique(model_results$group_name)[i]&parameter%in%c("mean_slope","mean_intercept")&Period=="1900-2015"&grouping==grouping_list[j])[,c("mean","parameter","p")]
  
  p_val <- ifelse(params$p[params$parameter=="mean_slope"]>0.95|params$p[params$parameter=="mean_slope"]<0.05,1,3)
  plot1 <- ggplot(aes(x=Year),data=data)+
    geom_ribbon(aes(y=pred2,ymax= pred_CIU,ymin= pred_CIL),fill= "grey90")+
    geom_point(aes(y=y,colour= SiteName,group= SiteName),alpha= 0.5,size= 2)+
    geom_line(aes(y=pred,colour= SiteName,group= SiteName))+
    geom_line(aes(y=y,colour= SiteName,group= SiteName),linetype= 3,size=0.1)+theme_bw()+
    theme(legend.position="none")+
    geom_line(aes(y=pred2),linetype= p_val)+
    ylab("Standardized Kelp Abundance")+
    scale_y_continuous(trans= "log10",labels = trans_format("log10", math_format(10^.x)),expand= c(0,0))+
    annotation_logticks(base=10,sides= "l")+
    xlab("Year")+
    coord_cartesian(ylim= c(0.0009,max*1.05))+
    ggtitle(paste(unique(model_results$group_name)[i],  grouping_list[j],sep= ";")) 
  print(plot1)
}
dev.off()
}

write.csv(combined_summaries,"../06_HLM_output/site_slopes.csv",row.names= F)

