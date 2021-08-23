// Basic Model with fixed Resistance per group // base model
//
data {
  int<lower=0> N; // nb of samples
  real<lower=0> R[N];    // resistance data
  int<lower=0> G[N]; // group index
  int<lower=0> N_G;   // number of groups
}


parameters {
  real mu[N_G] ; // mean per group
  real<lower=0> sigma;  // std dev for all groups
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  // priors
  sigma ~ normal(0, 0.1);
  mu ~ normal(0.5, 0.2);
  // likelihood
  for (i in 1:N)
      R ~ normal(mu[G[i]], sigma);


} // END


