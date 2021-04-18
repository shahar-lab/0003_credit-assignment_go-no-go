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
  int<lower=0> N; // number of observation 
  int<lower=1> K; // number of predectiors
  matrix[N,K] x; //data matrix
  int<lower=0,upper=1> y[N] ; // response vector (should i add a response victor?)   //   vector[N] y;      // outcome vector


}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real alpha; //intercept
  vector[K] beta; // regression coefficent 
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  //priors 
  
  beta ~ normal (0,10);
  alpha ~ normal (0,10);

  //likelihood / distrbution of y 
  y ~ bernoulli_logit_glm(x,alpha,beta); //response variable (which one to choose?) 
}

