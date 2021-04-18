data {
    int N; //number of data points
    real X[N]; //data values 
}

parameters {
    real mu; //mean
    real sigma; //standard deviation 
  
}

model {
    X ~ normal(mu,sigma);
}