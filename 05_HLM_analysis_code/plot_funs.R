plot_fun_1 <- function(ident) {
  ### gather group level data
  data <- subset(model_results,group_name==group_ID[ident])
  
  ### generate centered data
  data <- data %>% group_by(SiteName) %>%
    mutate(Year_group_cent= Year-mean(Year),
           log_y_cent= exp(log(y)-mean(log(y))),
           log_pred_cent= exp(log(pred)-mean(log(pred))))
  
  ### generate 100 points for aggregate predictions
  newdata <- data.frame(list(x= seq(from= min(data$x),to= max(data$x),length.out= 40),
                             Year_group_cent= seq(from= min(data$Year_group_cent),to= max(data$Year_group_cent),length.out= 40)))
  
  ### generate full range of ecoregion level predictions ###
  pred_fun <- function(x) model_list[[j]]$chains$beta_mu[,ident,1]+model_list[[j]]$chains$beta_mu[,ident,2]*x
  pred_fun2 <- function(x) model_list[[j]]$chains$beta_mu[,ident,2]*x
  pred <- data.frame(mapply(pred_fun,newdata$x))
  pred2 <- data.frame(mapply(pred_fun2,newdata$Year_group_cent))
  
  names(pred) <- newdata$x
  test <- melt(pred)
  names(test) <- c("x","pred")
  test$x <- as.numeric(as.character(test$x))
  test$Year <- as.numeric(test$x)+mean(model_results$Year)
  newdata$pred <- exp(colMeans(pred))
  ## generate smoothed prediction quantiles
  fit_1 <-  rq(pred~poly(x,5),data= test, tau= c(0.025,0.975))
  pred3 <-  exp(predict(fit_1,newdata= newdata))
  newdata$CIL<- pred3[,1]
  newdata$CIU <- pred3[,2]
  
  names(pred2) <- newdata$Year_group_cent
  test <- melt(pred2)
  names(test) <- c("Year_group_cent","pred")
  test$Year_group_cent <- as.numeric(as.character(test$Year_group_cent))
  test$Year_group_cent <- as.numeric(test$Year_group_cent)
  newdata$pred2 <- exp(colMeans(pred2))
  ## generate centered prediction quantiles
  quants <-  apply(pred2,2,quantile,probs = c(0.025,0.975))
  newdata$CIL_cent<- exp(quants[1,])
  newdata$CIU_cent <- exp(quants[2,])
  
  
  ### ensure year is adjusted from the centered value (x)
  newdata$Year <- as.numeric(newdata$x)+mean(model_results$Year)
  
  ### ensure year is adjusted from the centered value (x)
  newdata$Year <- as.numeric(newdata$x)+mean(model_results$Year)
  
  ### extract summary of aggregate parameters
  params <- subset(combined_summaries,parameter%in%c("mean_slope","mean_intercept")&Period=="1900-2015"&grouping==grouping_list[j]&group_name==group_ID[ident])
  
  ### plotting max and min y values
  maxp <- max(data$y)*1.15
  minp <- ifelse(min(data$y)==0.01,0.009,min(data$y)*0.87)
  maxp_c <- max(data$log_y_cent)*1.15
  minp_c <- ifelse(min(data$log_y_cent)==0.01,0.009,min(data$log_y_cent)*0.87)
  ### generate plot title and slope annotation
  title <-paste(group_ID[ident])
  slope <- paste0("mean slope: ",sprintf("%.3f",signif(params$mean[1],5)),
                  "\u00B1",sprintf("%.3f",signif(params$se[1],5)))
  
  ### line assignment from p-values
  p_val <- ifelse(params$p[params$parameter=="mean_slope"]>0.95|params$p[params$parameter=="mean_slope"]<0.05,1,3)
  
  ### generate plots (original and log-scale)
  plot1 <- ggplot(aes(x=Year),data=data)+
    geom_ribbon(aes(x=Year,y= pred,ymax= CIU,ymin= CIL),
             data= newdata,fill="grey50",alpha=0.5)+
    geom_point(aes(y=y,colour= SiteName,shape= StudyName),alpha= 0.5,size= 3)+
    geom_line(aes(y=pred,colour= SiteName,linetype= signif),alpha= 0.5,size= .8)+
    geom_line(aes(y=y,colour= SiteName),linetype= 3,size=0.1)+
    geom_line(aes(y=pred,x= Year),data= newdata,
              linetype= p_val,colour= "black",size= .8)+
    ylab("Standardized Kelp Abundance+0.01")+
    options+
    xlab("Year")+
    scale_linetype_manual(values= c(2,1))+
    scale_y_continuous(breaks= c(0,0.5,1.0,1.5),labels= c(0,0.5,1.0,1.5))+
    scale_shape_manual(values= c(16:17,3,4,7,8,9,12,13)[(1:length(unique(data$Study)))])+
    guides(colour= FALSE,linetype= FALSE,shape= guide_legend(title="Study" ,title.hjust= 0.5,ncol= 2))+
    coord_cartesian(ylim= c(-0.01,maxp),expand= c(0,0))+
    annotate("text",  x=Inf, y = Inf, label = slope, vjust=1.5, hjust=1.05)+
    ggtitle(title)
  
  plot2 <- plot1+
    scale_y_continuous(trans= "log10",breaks= c(0,0.001,0.01,0.1,0.5,1.0),labels= c(0,0.001,0.01,0.1,0.5,1.0),expand= c(0,0))+
    annotation_logticks(base=10,sides= "l")+
    coord_cartesian(ylim= c(minp,maxp))
  
  plot3 <- ggplot(aes(x=Year_group_cent),data=data)+
    geom_ribbon(aes(x=Year_group_cent,ymax= CIU_cent,ymin= CIL_cent),
                data= newdata,fill="grey50",alpha=0.5)+
    geom_point(aes(y=log_y_cent,colour= SiteName,shape= StudyName),alpha= 0.5,size= 0.75,shape= 21)+
    geom_line(aes(y=log_pred_cent,colour= SiteName,linetype=signif),alpha= 0.5,size= .8)+
    geom_line(aes(y=log_y_cent,colour= SiteName),linetype= 3,size=0.1)+
    geom_line(aes(y=pred2,x= Year_group_cent),data= newdata,
              linetype= p_val,colour= "black",size=1.5)+
    options+
    ylab("Standardized Kelp Abundance+0.01 (centered)")+
    xlab(expression(paste("Year - ",mu[Year])))+
    scale_linetype_manual(values= c(2,1))+
    options+theme(axis.title.x=element_text(size=16))+
    scale_shape_manual(values= c(16:17,3,4,7,8,9,12,13)[(1:length(unique(data$Study)))])+
    guides(colour= FALSE,linetype= FALSE,shape= guide_legend(title="Study" ,title.hjust= 0.5,ncol= 2))+
    scale_y_continuous(trans= "log10",breaks= c(0,0.001,0.01,0.1,1.0,10),labels= c(0,0.001,0.01,0.1,1.0,10),expand= c(0,0))+
    annotation_logticks(base=10,sides= "l")+
    ggtitle(title)
    
  return(list(p1=plot1,p2 =plot2,p3=plot3)) 
}

plot_fun_2 <- function(ident) {
  ### gather group level data
  data <- subset(model_results,group_name==group_ID[ident])
  
  ### generate centered data
  data <- data %>% group_by(SiteName) %>%
    mutate(Year_group_cent= Year-mean(Year),
           log_y_cent= exp(log(y)-mean(log(y))),
           log_pred_cent= exp(log(pred)-mean(log(pred))))
  
  ### generate 100 points for aggregate predictions
  newdata <- data.frame(list(x= seq(from= min(data$x),to= max(data$x),length.out= 40),
                             Year_group_cent= seq(from= min(data$Year_group_cent),to= max(data$Year_group_cent),length.out= 40)))
  
  ### generate full range of ecoregion level predictions ###
  pred_fun <- function(x) model_list[[j]]$chains$beta_mu[,ident,1]+model_list[[j]]$chains$beta_mu[,ident,2]*x
  pred_fun2 <- function(x) model_list[[j]]$chains$beta_mu[,ident,2]*x
  pred <- data.frame(mapply(pred_fun,newdata$x))
  pred2 <- data.frame(mapply(pred_fun2,newdata$Year_group_cent))
  
  names(pred) <- newdata$x
  test <- melt(pred)
  names(test) <- c("x","pred")
  test$x <- as.numeric(as.character(test$x))
  test$Year <- as.numeric(test$x)+mean(model_results$Year)
  newdata$pred <- exp(colMeans(pred))
  ## generate smoothed prediction quantiles
  fit_1 <-  rq(pred~poly(x,5),data= test, tau= c(0.025,0.975))
  pred3 <-  exp(predict(fit_1,newdata= newdata))
  newdata$CIL<- pred3[,1]
  newdata$CIU <- pred3[,2]
  
  
  names(pred2) <- newdata$Year_group_cent
  test <- melt(pred2)
  names(test) <- c("Year_group_cent","pred")
  test$Year_group_cent <- as.numeric(as.character(test$Year_group_cent))
  test$Year_group_cent <- as.numeric(test$Year_group_cent)
  newdata$pred2 <- exp(colMeans(pred2))
  
  ## generate centered prediction quantiles
  quants <-  apply(pred2,2,quantile,probs = c(0.025,0.975))
  newdata$CIL_cent<- exp(quants[1,])
  newdata$CIU_cent <- exp(quants[2,])
  
  ### ensure year is adjusted from the centered value (x)
  newdata$Year <- as.numeric(newdata$x)+mean(model_results$Year)
  
  ### ensure year is adjusted from the centered value (x)
  newdata$Year <- as.numeric(newdata$x)+mean(model_results$Year)
  
  ### extract summary of aggregate parameters
  params <- subset(combined_summaries,parameter%in%c("mean_slope","mean_intercept")&Period=="1900-2015"&grouping==grouping_list[j]&group_name==group_ID[ident])
  
  ### plotting max and min y values
  maxp <- max(data$y)*1.15
  minp <- ifelse(min(data$y)==0.01,0.009,min(data$y)*0.87)
  maxp_c <- max(data$log_y_cent)*1.15
  minp_c <- ifelse(min(data$log_y_cent)==0.01,0.009,min(data$log_y_cent)*0.87)
  
  ### generate plot title and slope annotation
  title <-paste(group_ID[ident])
  slope <- paste0("mean slope: ",sprintf("%.3f",signif(params$mean[1],5)),
                  "\u00B1",sprintf("%.3f",signif(params$se[1],5)))
  
  ### line assignment from p-values
  p_val <- ifelse(params$p[params$parameter=="mean_slope"]>0.95|params$p[params$parameter=="mean_slope"]<0.05,1,3)
  
  ### generate plots (original and log-scale)
  plot1 <- ggplot(aes(x=Year),data=data)+
    geom_ribbon(aes(x=Year,y= pred,ymax= CIU,ymin= CIL),
             data= newdata,fill="grey50",alpha=0.5)+
    geom_point(aes(y=y,fill= SiteName),alpha= 0.5,size= 0.75,shape= 21)+
    geom_line(aes(y=pred,colour= SiteName,linetype= signif),alpha= 0.5,size= .8)+
    geom_line(aes(y=y,colour= SiteName),linetype= 3,size=0.1)+
    geom_line(aes(y=pred,x= Year),data= newdata, 
              linetype= p_val,colour= "black",size= .8)+
    scale_y_continuous(breaks= c(0,0.5,1.0,1.5),labels= c(0,0.5,1.0,1.5))+
    ylab("Standardized Kelp Abundance+0.01")+
    options+
    xlab("Year")+
    scale_linetype_manual(values= c(2,1))+
    guides(colour= FALSE,fill= FALSE,linetype= FALSE)+
    coord_cartesian(ylim= c(-0.001,maxp),expand= c(0,0))+
    annotate("text",  x=Inf, y = Inf, label = slope, vjust=1.5, hjust=1.05)+
    ggtitle(title)
  
  plot2 <- plot1+
  scale_y_continuous(trans= "log10",breaks= c(0,0.001,0.01,0.1,0.5,1.0),labels= c(0,0.001,0.01,0.1,0.5,1.0),expand= c(0,0))+
    annotation_logticks(base=10,sides= "l")+
    coord_cartesian(ylim= c(minp,maxp))
  
  plot3 <-  ggplot(aes(x=Year_group_cent),data=data)+
    geom_ribbon(aes(x=Year_group_cent,ymax= CIU_cent,ymin= CIL_cent),
                data= newdata,fill="grey50",alpha=0.5)+
    geom_point(aes(y=log_y_cent,fill= SiteName),alpha= 0.5,size= 0.75,shape= 21)+
    geom_line(aes(y=log_y_cent,colour= SiteName),linetype= 3,size=0.1)+
    geom_line(aes(y=log_pred_cent,colour= SiteName,linetype= signif),alpha= 0.5,size= .8)+
    geom_line(aes(y=pred2,x= Year_group_cent),data= newdata,
              linetype= p_val,colour= "black",size= .8)+
    scale_y_continuous(trans= "log10")+
    scale_linetype_manual(values= c(2,1))+
    annotation_logticks(base=10,sides= "l")+
    ylab("Standardized Kelp Abundance+0.01 (centered)")+
    options+theme(axis.title.x=element_text(size=16))+
    xlab(expression(paste("Year - ",mu[Year])))+
    guides(colour= FALSE,fill= FALSE,linetype= FALSE)+
    ggtitle(title)
  
  return(list(p1=plot1,p2 =plot2,p3=plot3)) 
}
