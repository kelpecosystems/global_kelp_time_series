HLM_model <- 
  'data { 
    int NST; // number of studies 
    int NSI; // number of sites
    int NG; // number of Groups
    int N; // number of observations
    vector<lower=0>[N] y; // observations (on the original scale)
    int Site[N]; // vector of length N IDing sites
    int Group[N]; // vector of length N IDing Groups 
    int Study[N]; // vector of length N IDing Studies
    vector[N] x;
    }
    
  parameters {
      vector[2] beta[NSI]; // site level slopes
      vector[2] beta_mu[NG]; //Group level slopes
      vector<lower= 0.000001>[2] sigma_beta; // ranef variance
      cholesky_factor_corr[2] L_omega_beta; // ranef cholesky factors
      vector<lower=0.000001, upper= 10>[NST] sd_e; // error variance
    }
    
  transformed parameters { 
      matrix[2,2] L_vcov_beta; // ranef variance-covariance matrix
      matrix[2,2] D; // diagonal matrix of ranef SD
      vector[N] y_loc; // expected value given parameters
      vector[2] zeros;

      zeros <- rep_vector(0,2);
      D <- diag_matrix(sigma_beta);
      L_vcov_beta <- D * L_omega_beta;
          
      for (i in 1:N){
        y_loc[i] <- beta_mu[Group[i],1]+beta[Site[i],1]+(beta[Site[i],2]+beta_mu[Group[i],2])*x[i]; // expectation equation
      }  
    }
    
    model {       
      sigma_beta ~ cauchy(0,2.5); // vague priors for the ranef SD
      L_omega_beta ~ lkj_corr_cholesky(2.0); // vague priors for the ranef corr matrix
      beta ~ multi_normal_cholesky(zeros,L_vcov_beta); // site ranef
      for (i in 1:N){
        y[i]~lognormal(y_loc[i],sd_e[Study[i]]); // likelihood
      }
    }

  generated quantities {
    vector[N] y_loc_mu; // expected value given mean parameters
    vector[N] resid; // residuals 
    vector[N] log_lik; // individual log-likelihood
    
    for (i in 1:N){
     log_lik[i] <- lognormal_log(y[i],y_loc[i],sd_e[Study[i]]); // likelihood
     resid[i] <- (log(y[i])-y_loc[i])/sd_e[Study[i]]; // standardized residual
     y_loc_mu[i] <- beta_mu[Group[i],1]+(beta_mu[Group[i],2])*x[i]; // mean expectation equation
    }
  }
'

HLM_model_2 <-
  'data { 
    int NST; // number of studies 
    int NSI; // number of sites
    int NG; // number of Groups
    int N; // number of observations
    vector<lower=0>[N] y; // observations (on the original scale)
    int Site[N]; // vector of length N IDing sites
    int Group[N]; // vector of length N IDing Groups 
    int Study[N]; // vector of length N IDing Studies
    vector[N] x;
    vector[N] ST_ID;
  }
  
  parameters {
    vector[2] beta[NSI]; // site level slopes
    vector[2] beta_s[NST]; // study level slopes (sites within study >1)
    vector[2] beta_mu[NG]; // Group level slopes
    vector<lower= 0.000001>[2] sigma_beta; // ranef variance
    cholesky_factor_corr[2] L_omega_beta; // ranef site cholesky factors
    vector<lower= 0.000001>[2] sigma_beta_s; // ranef variance
    cholesky_factor_corr[2] L_omega_beta_s; // ranef study cholesky factors
    vector<lower=0.000001, upper= 10>[NSI] A; // half-cauchy scale parameter
    vector<lower=0.000001, upper= 10>[NSI] sd_e; // error variance
  }
  
  transformed parameters { 
    matrix[2,2] L_vcov_beta; // ranef variance-covariance matrix of sites
    matrix[2,2] D; // diagonal matrix of ranef SD of sites
    matrix[2,2] L_vcov_beta_s; // ranef variance-covariance matrix of studies
    matrix[2,2] D_s; // diagonal matrix of ranef SD of studies
    vector[N] y_loc; // expected value given parameters
    vector[2] zeros;
    
    zeros <- rep_vector(0,2);
    D <- diag_matrix(sigma_beta);
    L_vcov_beta <- D * L_omega_beta;
    D_s <- diag_matrix(sigma_beta_s);
    L_vcov_beta_s <- D * L_omega_beta_s;
    
    for (i in 1:N){
      y_loc[i] <- beta_mu[Group[i],1]+beta_s[Study[i],1]*ST_ID[i]+beta[Site[i],1]+(beta[Site[i],2]+beta_s[Study[i],2]*ST_ID[i]+beta_mu[Group[i],2])*x[i]; // expectation equation
    }  
  }
  
  model {       
    sigma_beta ~ cauchy(0,2.5); // vague priors for the ranef SD
    L_omega_beta ~ lkj_corr_cholesky(2.0); // vague priors for the ranef corr matrix
    beta ~ multi_normal_cholesky(zeros,L_vcov_beta); // site ranef

    sigma_beta_s ~ cauchy(0,2.5); // vague priors for the ranef SD
    L_omega_beta_s ~ lkj_corr_cholesky(2.0); // vague priors for the ranef corr matrix
    beta_s ~ multi_normal_cholesky(zeros,L_vcov_beta_s); // study ranef
    
    sd_e ~ cauchy(0,A); // hierarchical error variance parameters

    for (i in 1:N){
      y[i]~lognormal(y_loc[i],sd_e[Site[i]]); // likelihood
    }
  }
  
  generated quantities {
    vector[N] y_loc_mu; // expected value given mean parameters
    vector[2] study_beta[NST]; // mean study level slope
    vector[2] site_beta[NSI]; // mean site level slope
    vector[N] resid; // residuals 
    vector[N] log_lik; // individual log-likelihood
    
    for (i in 1:N){
      log_lik[i] <- lognormal_log(y[i],y_loc[i],sd_e[Site[i]]); // likelihood
      resid[i] <- (log(y[i])-y_loc[i])/sd_e[Site[i]]; // standardized residual
      y_loc_mu[i] <- beta_mu[Group[i],1]+(beta_mu[Group[i],2])*x[i]; // mean expectation equation
    }
  }
'