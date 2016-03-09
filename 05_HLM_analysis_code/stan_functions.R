### functions to get median, mean and other quantiles from the posterior ###

HPD.mean.fun <- function(chains,probs,source_name,names,chain_ID) {
  
  HPD <- data.frame(do.call(cbind,lapply(probs,
                                         function(x) HPDinterval(mcmc(chains),prob= x))))
  #cbind(mean=mean(x),se = sd(x),HPD)
  names(HPD) <- paste(rep(c("lower","upper"),
                          length(probs)),rep(probs,each=2),sep= "_")
  
  
  if(is.null(dim(chains))){
    quants <- data.frame(t(quantile(chains, probs = c((1-probs)/2,rev(1-(1-probs)/2)))))
    names(quants) <- paste(rep(c("lower","upper"), each= length(probs)),c(probs, rev(probs)),"q",sep= "_")
    HPD <- cbind(mean=mean(chains),se = sd(chains),p= mean(chains>0),HPD,quants)
    HPD$Source <- 1:nrow(HPD)
    names(HPD)[ncol(HPD)] <- source_name   
    a <- mcmc.list(lapply(chain_ID,function(x) mcmc(chains[x])))
    HPD <- join(x=HPD,y=names,type= "left")
    HPD[,c("Rhat","Rhat_upper_CI")] <- gelman.diag(a)$psrf   
  }
  
  else{
    quants <- data.frame(t(apply(chains, 2, quantile, probs = c((1-probs)/2,rev(1-(1-probs)/2)))))
    names(quants) <- paste(rep(c("lower","upper"), each= length(probs)),c(probs, rev(probs)),"q",sep= "_")
    HPD <- cbind(mean=apply(chains,2,mean),se = apply(chains,2,sd),p= apply(chains,2,function(x)mean(x>0)),HPD,quants)
    HPD$Source <- 1:nrow(HPD)
    names(HPD)[ncol(HPD)] <- source_name 
    HPD <- join(x=HPD,y=names,type= "left")
    g <- matrix(NA, nrow=dim(chains)[2], ncol=2)
    for (v in 1:dim(chains)[2]) {
      a <- mcmc.list(lapply(chain_ID,function(x) mcmc(chains[x,v])))
      g[v,] <- gelman.diag(a)$psrf
    }
    HPD[,c("Rhat","Rhat_upper_CI")] <- g   
  }
  return(HPD)
}

#### functions to calculate WAIC 
colVars <- function (a){
  diff <- a - matrix (colMeans(a), nrow(a), ncol(a), byrow=TRUE)
  vars <- colMeans (diff^2)*nrow(a)/(nrow(a)-1)
  return (vars)
}

waic <- function(log_lik){
  dim(log_lik) <- if (length(dim(log_lik))==1) c(length(log_lik),1) else
    c(dim(log_lik)[1], prod(dim(log_lik)[2:length(dim(log_lik))]))
  S <- nrow(log_lik)
  n <- ncol(log_lik)
  lpd <- log(colMeans(exp(log_lik)))
  p_waic <- colVars(log_lik)
  elpd_waic <- lpd - p_waic
  waic <- -2*elpd_waic
  loo_weights_raw <- 1/exp(log_lik-max(log_lik))
  loo_weights_normalized <- loo_weights_raw/
    matrix(colMeans(loo_weights_raw),nrow=S,ncol=n,byrow=TRUE)
  loo_weights_regularized <- pmin (loo_weights_normalized, sqrt(S))
  elpd_loo <- log(colMeans(exp(log_lik)*loo_weights_regularized)/
                    colMeans(loo_weights_regularized))
  p_loo <- lpd - elpd_loo
  pointwise <- cbind(waic,lpd,p_waic,elpd_waic,p_loo,elpd_loo)
  total <- colSums(pointwise)
  se <- sqrt(n*colVars(pointwise))
  return(list(waic=total["waic"], elpd_waic=total["elpd_waic"],
              p_waic=total["p_waic"], elpd_loo=total["elpd_loo"], p_loo=total["p_loo"],
              pointwise=pointwise, total=total, se=se))
}

waic_diff <- function(stanfit1, stanfit2){
  log_lik1 <- extract (stanfit1, "log_lik")$log_lik
  log_lik2 <- extract (stanfit2, "log_lik")$log_lik
  log_lik <- log_lik1-log_lik2
  dim(log_lik) <- if (length(dim(log_lik))==1) c(length(log_lik),1) else
    c(dim(log_lik)[1], prod(dim(log_lik)[2:length(dim(log_lik))]))
  S <- nrow(log_lik)
  n <- ncol(log_lik)
  lpd <- log(colMeans(exp(log_lik)))
  p_waic <- colVars(log_lik)
  elpd_waic <- lpd - p_waic
  waic <- -2*elpd_waic
  loo_weights_raw <- 1/exp(log_lik-max(log_lik))
  loo_weights_normalized <- loo_weights_raw/
    matrix(colMeans(loo_weights_raw),nrow=S,ncol=n,byrow=TRUE)
  loo_weights_regularized <- pmin (loo_weights_normalized, sqrt(S))
  elpd_loo <- log(colMeans(exp(log_lik)*loo_weights_regularized)/
                    colMeans(loo_weights_regularized))
  p_loo <- lpd - elpd_loo
  pointwise <- cbind(waic,lpd,p_waic,elpd_waic,p_loo,elpd_loo)
  total <- colSums(pointwise)
  se <- sqrt(n*colVars(pointwise))
  return(list(waic=total["waic"], elpd_waic=total["elpd_waic"],
              p_waic=total["p_waic"], elpd_loo=total["elpd_loo"], p_loo=total["p_loo"],
              pointwise=pointwise, total=total, se=se))
}

### df of group and site names for reference
years_subset <- function(x,min_obs,x_min=1900,x_max=2015){
  conditional_sites <-  subset(ddply(subset(x,Year>=x_min&Year<x_max),.(StudyName,SiteName),
                                     summarize,n_obs =length(y),
                                     n_years= length(unique(Year))),
                               n_years>=min_years&n_obs>=min_obs)
  
  output <- subset(x,SiteName%in%conditional_sites$SiteName) 
  return(output)
}

### df of group and site names for reference
min_sites_subset <- function(x,group_name,min_sites){
  conditional_sites <- subset(ddply(x,c(group_name),
                                    summarize,n_sites = length(unique(SiteName))),
                              n_sites >=min_sites)
  output <- x[x[,group_name]%in%conditional_sites[,group_name],] 
  return(output)
}


#### function to fit the model and produce output ###
HLM_stan_fit <- function(data=model_data,
                         group,min_num_sites=3, 
                         params = pars,MCMC_details,quantiles= c(0.99,0.95,0.9),
                         model=HLM_model){
  ### rename groupings for convenience in Stan
  data$SiteName <- factor(factor(data$SiteName):factor(data$StudyName))
  data$group_name <- data[,paste(group,"Name",sep= "")]
  data$group_num <- as.numeric(factor(data$group_name))
  data$study_num <- as.numeric(factor(data$StudyName))
  data$site_num <- as.numeric(data$SiteName)
  
  ### get site and grouping name info to combine with model output
  GroupSite <- ddply(data,c("site_num","study_num","group_num","group_name","StudyName","SiteName"),
                     summarise,group_num_site =unique(group_num))
  
  unique_study <- ddply(data,c("group_num"),summarise,N_studies = length(unique(study_num)))
  unique_site <- ddply(data,c("study_num"),summarise,N_sites = length(unique(site_num)))
  data <- join(data,unique_study)
  data <- join(data,unique_site)
  
  ### fetch the data to be used in the model 
  data_for_stan <- with(data,
                        list(NG= length(unique(group_num)),
                             NSI= length(unique(SiteName)),
                             NST= nrow(unique(data[,c("SiteMethod","study_num")])),
                             NM= length(unique(SiteMethod)),
                             Group=group_num,
                             Site= site_num,
                             Study= study_num,
                             x=Year-mean(Year),
                             GroupSite = unique(data[,c("group_num","site_num")])[,1],
                             y=y,
                             N=nrow(data),
                             SiteMethod= unique(data[,c("SiteMethod","study_num")])[,1])
  )
  
  fit <- with(MCMC_details,stan(model_code= model,data= data_for_stan,
                                pars= params,
                                iter= n.iter,thin= n.thin,warmup= n.burnin,
                                chains=n.chains,
                                init= "random",seed= 123,cores=1,
                                control= list(adapt_delta = 0.9,max_treedepth= 15)))
  
  chainmat <-  with(MCMC_details,matrix(1:((n.iter-n.burnin)/n.thin*n.chains),ncol= n.chains))
  
  chain_ID <-split(chainmat, rep(1:ncol(chainmat), each = nrow(chainmat)))
  
  fit.params <- extract(fit)
  
  ### get the HPD for each parameter ###
  mu_slope <- HPD.mean.fun(fit.params$beta_mu[,,2],probs= quantiles, chain_ID=chain_ID,
                           source_name= "group_num_site",
                           names= unique(GroupSite[,c("group_num_site","group_name")]))
  
  ### name the average slope ###
  mu_slope$parameter <- "mean_slope"
  
  mu_intercept <- HPD.mean.fun(fit.params$beta_mu[,,1],probs= quantiles, chain_ID=chain_ID,
                               source_name= "group_num_site",
                               names= unique(GroupSite[,c("group_num_site","group_name")]))
  
  mu_intercept$parameter <- "mean_intercept"
  
  mean_summary <- rbind(mu_slope,mu_intercept)
  
  groupings <- unique(data[,c("SiteName","group_name","group_num","site_num","study_num")])
  
  beta_mu_rep <-fit.params$beta_mu[,groupings$group_num,]
  
  betas <- fit.params$beta+beta_mu_rep
  
  site_slope <- HPD.mean.fun(betas[,,2],probs= quantiles, chain_ID=chain_ID,
                             source_name= "site_num",
                             names= groupings)
  
  site_slope$parameter <- "site_slope"
  site_intercept <- HPD.mean.fun(betas[,,1],probs= quantiles, chain_ID=chain_ID,
                                 source_name= "site_num",
                                 names= groupings)
  
  site_slope$parameter <- "site_slope"
  site_intercept$parameter <- "site_intercept"
  
  data$y_hat <- colMeans(exp(fit.params$y_loc))
  data[,c("y_hat_L95","y_hat_U95")] <- data.frame(t(apply(exp(fit.params$y_loc),2,
                                                          function(x) HPDinterval(as.mcmc(x)))))
  
  data$y_hat_mu <- colMeans(exp(fit.params$y_loc_mu))
  data[,c("y_hat_mu_L95","y_hat_mu_U95")] <- data.frame(t(apply(exp(fit.params$y_loc_mu),2,
                                                                function(x) HPDinterval(as.mcmc(x)))))
  
  site_summary <- rbind(site_slope,site_intercept)
  output_summary <- rbind.fill(site_summary,mean_summary)
  output_summary <- subset(output_summary, select=-c(site_num,group_num_site))
  names(output_summary)[names(output_summary)=="group_name"] 
  output_summary$grouping <- group
  waic_output <- waic(fit.params$log_lik)
  return(list(summary= output_summary,chains = fit.params,waic = waic_output,data_pred= data))
}  

