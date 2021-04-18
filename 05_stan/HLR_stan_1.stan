//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
    int N; // number of obs (trials)
    int M; // number of groups (subjects)
    int K; // number of predictors
    
    int y[N]; // outcome
    row_vector[K] x[N]; // predictors
    int g[N];    // map obs to groups (trials to subjects)
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
    real alpha; //the constant 
    real a[M]; // the subject-specific random effects
    vector[K] beta; // the coefficients of the k predictors
    real<lower=0,upper=10> sigma;  // the standard deviation of the random effects
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  alpha ~ normal(0,100); 
  a ~ normal(0,sigma); 
  beta ~ normal(0,100); 
  for(n in 1:N) {
    y[n] ~ bernoulli(inv_logit( alpha + a[g[n]] + x[n]*beta));
  }

}