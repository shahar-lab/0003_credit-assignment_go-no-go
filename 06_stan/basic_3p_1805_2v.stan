data {
  int<lower = 1> Nsubj; //number of subjects
  int<lower = 1> Ntrials; //number of trials
  int<lower = 2> Narms; //number of alternatives 
  int<lower = 0> N2states; //number of second stated at the second stage 
  int<lower = 1, upper = 2> a1[Nsubj,Ntrials]; //index of which arm was pulled at the first stage
  int<lower = 1, upper = 2> a2[Nsubj,Ntrials]; //index of which arm was pulled at the second stage
  int<lower = 0, upper = 2> s2[Nsubj,Ntrials]; //index of the second stage state 
  int<lower = 0, upper = 1> reward[Nsubj,Ntrials]; //outcome of bandit arm pull
}

parameters {
  real alpha1_subjects[Nsubj]; //learning rate - 1st stage
  real lambda_subjects[Nsubj]; //learning rate - 2nd stage
  real beta_subjects[Nsubj]; //softmax parameter - inverse temperature

  //hyper parameters 
  real mu_alpha1;
  real <lower = 0> tau_alpha1;
  real mu_lambda;
  real <lower = 0> tau_lambda;
  real mu_beta;
  real <lower = 0> tau_beta;
}

transformed parameters {
      real alpha1[Nsubj];
      real lambda[Nsubj];
      real beta[Nsubj];
      matrix [Nsubj,Ntrials] log_lik;
      real <lower=0, upper=1> Qval [2,N2states,Narms];
      vector <lower=0, upper=1> [2] Qnet; 
      Qval = rep_array(0,2,N2states,Narms);
      Qnet = rep_vector(0,Narms);


  for (subj in 1:Nsubj){
        real PE1;
        real PE2;

        alpha1[subj] = inv_logit(alpha1_subjects[subj]);
        lambda[subj] = inv_logit(lambda_subjects[subj]);
        beta[subj] = exp(beta_subjects[subj]);
                
        for (trial in 1:Ntrials){
            //print("trial: ", trial);
            //print("Q1s: ", Q1s);
            //print("Q2s: ", Q2s);
            Qnet[1] = Qval[1,1,1];
            Qnet[2] = Qval[1,1,2];
            log_lik[subj,trial]=log_softmax(Qnet*beta[subj])[a1[subj,trial]];
            Qnet[1] = Qval[2,s2[subj,trial],1];
            Qnet[2] = Qval[2,s2[subj,trial],2];
            log_lik[subj,trial]+=log_softmax(Qnet*beta[subj])[a2[subj,trial]];
            PE1= Qval[2,s2[subj,trial],a2[subj,trial]] - Qval[1,1,a1[subj,trial]];
            PE2= reward[subj,trial] - Qval[2,s2[subj,trial],a2[subj,trial]];
            Qval[1,1,a1[subj,trial]] += alpha1[subj] * PE1+alpha1[subj] * lambda[subj] * PE2;
            Qval[2,s2[subj,trial],a2[subj,trial]] += alpha1[subj] * PE2;

        } 
  }
}
model {
  
  // population level priors (hyper-parameters)
  mu_alpha1 ~ normal(0, 5);
  tau_alpha1 ~ normal(0, 5);
  mu_lambda ~ normal(0, 5);
  tau_lambda ~ normal(0, 5);
  mu_beta ~ normal(0, 5);
  tau_beta ~ normal(0, 5);

  // indvidual level priors (subject parameters)
  beta_subjects ~ normal(mu_beta, tau_beta);
  alpha1_subjects ~ normal(mu_alpha1, tau_alpha1);
  lambda_subjects ~ normal(mu_lambda, tau_lambda);


  target += sum(log_lik);

}

