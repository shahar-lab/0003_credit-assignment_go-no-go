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
int<lower=0> N;                // number of observations
int<lower=0,upper=1> stay1[N];  // setting the dependent variable (stay1) as binary
vector[N] prv_reward;             // independent variable 1
vector[N] prv_key1;              // independent variable 2
vector[N] prv_key2;              // independent variable 3
vector[N] adhd;                 // independent variable 4
}

transformed data {
vector[N] rk1_int;              // create new variable (4), interaction betweeen the previous outcome and previous trails first response (key1)  (no dots in the variable name)
vector[N] rk2_int;              // create new variable (5), interaction betweeen the previous outcome and previous trails second response (key2) 
vector[N] radhd_int;            // create new variable (6), interaction betweeen the previous outcome and adhd subclinical score 
vector[N] triple_int1;          // create new variable (7), quadrouple interaction betweeen the previous outcome, previous trails first response (key1) and adhd subclinical score 
vector[N] triple_int2;          // create new variable (8), quadrouple interaction betweeen the previous outcome, previous trails second response (key2) and adhd subclinical score  
vector[N] quad_int;             // create new variable (9), quadrouple interaction betweeen the previous outcome, previous trails first response (key1),previous trails second response (key2) and adhd subclinical score 
rk1_int = prv_reward .* prv_key1;          // formula for the variable, do not forget the . before multiplication
rk2_int = prv_reward .* prv_key2;
radhd_int = prv_reward .* adhd;          
triple_int1 = prv_reward .* prv_key1 .* adhd;
triple_int2 = prv_reward .* prv_key2 .* adhd;
quad_int = prv_reward .* prv_key1 .* prv_key2 .* adhd;

}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
real alpha;                    // intercept
real b_prv_reward;                // beta for prv_reward, etc
real b_prv_key1; 
real b_prv_key2;
real b_adhd; 
real b_rk1_int; 
real b_rk2_int;
real b_radhd_int;
real b_triple_int1; 
real b_triple_int2; 
real b_quad_int; 

}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
alpha ~ normal(0,100);         // you can set priors for all betas
b_prv_reward ~ normal(0,100);     // if you prefer not to, uniform priors will be used
b_prv_key1 ~ normal(0,100);
b_prv_key2 ~ normal(0,100);
b_adhd ~ normal(0,100);
b_rk1_int ~ normal(0,100);
b_rk2_int ~ normal(0,100);
b_radhd_int ~ normal(0,100);
b_triple_int1 ~ normal(0,100);
b_triple_int2 ~ normal(0,100);
b_quad_int ~ normal(0,100);

stay1 ~ bernoulli_logit(alpha + b_prv_reward * prv_reward + b_prv_key1 * prv_key1 + b_prv_key2 * prv_key2 + b_adhd * adhd + b_rk1_int * rk1_int + b_rk2_int * rk2_int + b_radhd_int * radhd_int + b_triple_int1 * triple_int1 + b_triple_int2 * triple_int2 + b_quad_int * quad_int ); // model
}

