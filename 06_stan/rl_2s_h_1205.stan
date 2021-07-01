data {
  int<lower = 1> Nsubj; //number of subjects
  int<lower = 1> Ntrials; //number of trials
  int<lower = 2> Narms; //number of alternatives 
  int<lower = 0> N2states; //number of second stated at the second stage 
  int<lower = 1, upper = 2> a1[Nsubj,Ntrials]; //index of which arm was pulled at the first stage
  int<lower = 1, upper = 4> a2[Nsubj,Ntrials]; //index of which arm was pulled at the second stage
  int<lower = 2, upper = 3> s2[Nsubj,Ntrials]; //index of the second stage state 
  int<lower = 0, upper = 1> reward[Nsubj,Ntrials]; //outcome of bandit arm pull
}

parameters {
  real alpha1_subjects[Nsubj]; //learning rate - 1st stage
  real alpha2_subjects[Nsubj]; //learning rate - 2nd stage
  real beta_subjects[Nsubj]; //softmax parameter - inverse temperature
  real lambda_subjects[Nsubj]; // eligibility trace

  //hyper parameters 
  real mu_alpha1;
  real <lower = 0> tau_alpha1;
  real mu_alpha2;
  real <lower = 0> tau_alpha2;
  real mu_beta;
  real <lower = 0> tau_beta;
  real mu_lambda;
  real <lower = 0> tau_lambda;
}

transformed parameters {
      real alpha1[Nsubj];
      real alpha2[Nsubj];
      real beta[Nsubj];
      real lambda[Nsubj];
      matrix [Nsubj,Ntrials] log_lik;
      vector <lower=0, upper=2> [Narms] Q1s; 
      vector <lower=0, upper=2> [Narms*N2states] Q2s; 


  for (subj in 1:Nsubj){
        real PE1;
        real PE2;

        alpha1[subj] = inv_logit(alpha1_subjects[subj]);
        alpha2[subj] = inv_logit(alpha2_subjects[subj]);
        beta[subj] = exp(beta_subjects[subj]);
        lambda[subj] = inv_logit(lambda_subjects[subj]);

        for (a in 1:Narms) Q1s[a] = 0;
        for (a in 1:(Narms*N2states)) Q2s[a] = 0;

        for (trial in 1:Ntrials){
            print("Q1s: ", Q1s);
            //print("Q2s: ", Q2s);
            //print("Q1s*beta[subj]: ", Q1s*beta[subj]);
            //print("Q1s[a2[subj,trial]] ", Q1s[a2[subj,trial]]);
            //print("Q1s[a2[subj,trial]] ", Q1s[a2[subj,trial]]);
            log_lik[subj,trial]=log_softmax(Q1s*beta[subj])[a1[subj,trial]];
            log_lik[subj,trial]+=log_softmax(Q2s*beta[subj])[a2[subj,trial]];
            PE1= Q2s[a2[subj,trial]] - Q1s[a1[subj,trial]];
            PE2= reward[subj,trial] - Q2s[a2[subj,trial]];
            Q1s[a1[subj,trial]] += alpha1[subj] * PE1+alpha1[subj] * lambda[subj] * PE2;
            Q2s[a2[subj,trial]] += alpha2[subj] * PE2;

        } 
  }
}
model {
  
  // population level priors (hyper-parameters)
  mu_alpha1 ~ normal(0, 5);
  tau_alpha1 ~ normal(0, 5);
  mu_alpha2 ~ normal(0, 5);
  tau_alpha2 ~ normal(0, 5);
  mu_beta ~ normal(0, 5);
  tau_beta ~ normal(0, 5);
  mu_lambda ~ normal(0, 5);
  tau_lambda ~ normal(0, 5);

  // indvidual level priors (subject parameters)
  beta_subjects ~ normal(mu_beta, tau_beta);
  alpha1_subjects ~ normal(mu_alpha1, tau_alpha1);
  alpha2_subjects ~ normal(mu_alpha2, tau_alpha2);
  lambda_subjects ~ normal(mu_lambda, tau_lambda);


  target += sum(log_lik);

}

