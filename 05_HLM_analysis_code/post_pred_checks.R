######################################################
###  Script for posterior predictive checks        ###
###  for the hierarchical linear model             ###
###  Author:  D.K. Okamoto                         ###
######################################################

library(rstan);
library(plyr);library(gdata);library(ggplot2)
library(parallel);library(grid);library(coda);library(scales);library(reshape2)
library(quantreg)

### plot predictions ###
load("~/Dropbox/nceas_kelp_data/temporal_change_big/HLME_Output/1900-2015_3_points_2016-02-01.RData")
load("~/Dropbox/nceas_kelp_climate_2013/temporal_change/github_repo/06_HLM_output/combined_model_summaries_3_points.RData")

  study <- model_list[[1]]$data_pred$StudyName
  data <- model_list[[1]]$data_pred
  data_res <- cbind(data,resid)
  data_res<- melt(data_res,id.vars= names(data))
  names(data_res) <- c(names(data),"iteration","resid")
 
  samples <- sample(1:length(unique(res2$iteration)),300)
  res3 <-subset(data_res,iteration%in%samples)
  res3$Study <- res3$StudyName
  res_mean <- ddply(res3,.(Study,iteration),summarize,mean= mean(resid))
  head(res_mean)
  
  
  test1 <- ddply(res3,.(Study,iteration),summarize,
                 stat = ks.test(resid,"pnorm", 0,1)$statistic[[1]],
                 stat.null = ks.test(rnorm(length(resid),0,1),"pnorm", 0,1)$statistic[[1]])
  test2 <- ddply(test1,.(Study),summarize,
                 P = as.character(paste("P =",sprintf("%.2f",round(mean(stat<stat.null),digits= 2)))),P_num = mean(stat<stat.null))
  
  qq_data <- ddply(res3,.(Study,iteration),summarize,
                 quant = sort(resid),
                 exp_quant = qnorm(ppoints(resid)))

  
  
  
  measure_unit <- ddply(model_list[[x]]$data_pred,.(StudyName),summarize,Unit= paste(unique(Unit),collapse= ";"),N= length(y))
  
  names(measure_unit)[1] <- "Study"
  test2 <- join(test2,measure_unit)
  
  options <- theme(strip.text.x = element_text(size =0, vjust=-2.5,hjust=0.9),
                   axis.text.y = element_text( colour= "black",size =10,hjust=0.5,vjust=1.5, angle=90),
                   axis.text.x = element_text(colour= "black",size =10,),
                   axis.title.y = element_text( colour= "black",size =10,vjust=1),
                   axis.title.x = element_text(colour= "black",size =10),
                   panel.grid.minor = element_line(colour = NA),
                   panel.grid.major = element_line(colour = NA),
                   panel.margin= unit(.5,"lines"),
                   strip.background = element_rect(fill= NA,colour= NA,size= 0),
                   panel.grid = element_line(colour = NA),
                   panel.background=element_rect(fill= NA),
                   plot.background=element_rect(fill= NA),
                   panel.border= element_rect(fill= NA),
                   plot.margin = unit(rep(0.1, 4), "inches"))
  
  pdf(width= 8, height= 8,file= "../Figures/post_pred_checks.pdf")
    for(i in 1:length(unique(study))){
      max <-max(subset(test1,Study==unique(study)[i])[,c("stat","stat.null")])*1.1
      labels <- subset(test2,Study==unique(study)[i])
      plot1 <- ggplot(aes(stat,stat.null),data =subset(test1,Study==unique(study)[i]))+
        geom_point(alpha= 0.2,size= 1)+geom_abline(intercept=0,slope=1)+
        coord_fixed(ratio=1)+coord_equal(xlim= c(0,max),ylim= c(0,max))+
        options+
        ylab("predicted Kolmogorov-Smirnov statistic")+
        xlab("observed Kolmogorov-Smirnov statistic")+
        annotate("text",  x=-Inf, y = Inf, label = labels$P, vjust=2, hjust=-0.2)+
        annotate("text",  x=-Inf, y = Inf, label = paste("Measurement = ",labels$Unit), vjust=7, hjust=-0.1)+
        annotate("text",  x=-Inf, y = Inf, label = paste("N=",labels$N), vjust=4.5, hjust=-0.25)
      
      test <-  subset(qq_data,Study==unique(Study)[i])
      maxq <-max(subset(test,Study==unique(study)[i])[,c("quant","exp_quant")])*1.01
      minq <-min(subset(test,Study==unique(study)[i])[,c("quant","exp_quant")])*1.01
      maxq-minq
      
      plot2 <- ggplot(aes(quant,exp_quant),data=test)+
        geom_line(aes(group= iteration),alpha= 0.5,lwd= 0.1,col= "grey")+
        geom_abline(intercept=0,slope=1)+
        ylab("predicted normal quantiles")+
        xlab("posterior normal quantiles")+
        coord_cartesian(xlim= c(minq,maxq),ylim= c(-2,2))+
        options+coord_fixed((maxq-minq)/4)
    
      interquartile <- function(x){
        out <- quantile(x, probs = c(0.025, 0.5, 0.975))
        names(out) <- c("ymin", "y", "ymax")
        out
      }
    
      data_sub <-  subset(res3,Study==unique(Study)[i])
      nyears <- length(unique(as.integer(data_sub$Year)))
      data_sub <- ddply(data_sub,.(y_hat,Study,Year, y_hat_L95,y_hat_U95),summarize,med = median(resid),QU= quantile(resid,0.975),QL= quantile(resid,0.025))
      plot4 <- ggplot(aes(Year,med),data= data_sub)+
        geom_linerange(aes(ymin= QL,ymax= QU),alpha= 0.25,lwd=0.2)+
        geom_point(alpha= 0.25,size=1)+
        ylab("residual")+options+
        geom_hline(yintercept=0)
     
      
      plot3 <- ggplot(aes(log(y_hat),med),data= data_sub)+
       geom_errorbarh(aes(xmin=  log(y_hat_L95), 
                          xmax=log(y_hat_U95)),
                      alpha=0.25,lwd=0.2)+
        geom_linerange(aes(ymin= QL,ymax= QU),
                       alpha= 0.25,lwd=0.2)+
        geom_point(alpha= 0.25,size=1)+
        ylab("residual")+options+
        geom_hline(yintercept=0)

      gA <- ggplotGrob(plot1)
      gB <- ggplotGrob(plot2)
      gC <- ggplotGrob(plot3)
      gD <- ggplotGrob(plot4)
      gB$heights<- gA$heights
      gC$widths<- gA$widths
      gD$widths<- gA$widths
      grid.arrange(gA,gB,gC,gD,ncol=2,top=textGrob(unique(study)[i]))
    }
  dev.off()
  
 