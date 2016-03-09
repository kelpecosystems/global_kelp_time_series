HLM_model <-
  'data { 
      int NST; // number of studies x methods
      int NSI; // number of sites
      int N; // number of observations
      int NG; // number of Groups
      int NM; // number of methods
      vector<lower=0>[N] y; // observations (on the original scale)
      int Site[N]; // vector of length N IDing sites
      int Study[N]; // vector of length N IDing Studies
      int GroupSite[NSI]; // vector of length N IDing Studies
      int Group[N]; // vector of length N IDing Groups 
      int SiteMethod[NST]; // vector of length NSI IDing site sampling methods
      vector[N] x;
    }
    
    parameters {
      vector[2] beta[NSI]; // site level parameters
      vector[2] beta_mu[NG]; // Group level parameters
      vector<lower= 0.000001,upper= 5>[2] sigma_beta; // ranef variance
      cholesky_factor_corr[2] L_omega_beta; // ranef site cholesky factors
      vector<lower=0.000001, upper= 5>[NST] sd_e; // error variance
      vector<lower=0.000001, upper= 3>[NM] A; // half-cauchy scale parameter
    }
    
    transformed parameters { 
      matrix[2,2] L_vcov_beta; // ranef variance-covariance matrix of sites
      vector[N] y_loc; // expected value given parameters
      vector[2] zeros;
      zeros <- rep_vector(0,2);
      L_vcov_beta <- diag_pre_multiply(sigma_beta, L_omega_beta);
      
    for (i in 1:N){
      y_loc[i] <- beta[Site[i],1]+beta_mu[Group[i],1]+(beta_mu[Group[i],2]+beta[Site[i],2])*x[i]; // expectation equation
      }  
    }
    
    model {  
      sigma_beta ~ cauchy(0,2.5); // vague priors for the ranef SD intercept
      L_omega_beta ~ lkj_corr_cholesky(2); // vague priors for the ranef corr matrix
      beta ~ multi_normal_cholesky(zeros,L_vcov_beta); // site ranef
      for(i in 1:NST){
        sd_e[i] ~ cauchy(0,A[SiteMethod[i]]); // hierarchical error variance parameters
      }
      for (i in 1:N){
        y[i]~lognormal(y_loc[i],sd_e[Study[i]]); // likelihood
      }
    }
    
    generated quantities {
      vector[N] y_loc_mu; // expected value given mean parameters
      vector[2] site_beta[NSI]; // mean site level slope
      vector[N] resid; // residuals 
      vector[N] log_lik; // individual log-likelihood
      
      for (i in 1:N){
        log_lik[i] <- lognormal_log(y[i],y_loc[i],sd_e[Study[i]]); // likelihood
        resid[i] <- (log(y[i])-y_loc[i])/sd_e[Study[i]]; // standardized residual
        y_loc_mu[i] <- beta_mu[Group[i],1]+beta_mu[Group[i],2]*x[i]; // mean expectation equation
      }
}'