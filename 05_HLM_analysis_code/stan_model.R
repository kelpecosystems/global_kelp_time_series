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