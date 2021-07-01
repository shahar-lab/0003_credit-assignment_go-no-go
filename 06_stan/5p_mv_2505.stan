data {
  int<lower = 1> Nsubj; //number of subjects
  int<lower = 1> Ntrials; //number of trials
  int<lower = 2> Narms; //number of alternatives 
  int<lower = 0> N2states; //number of second stated at the second stage 
  int<lower = 0, upper = 2> a1[Nsubj,Ntrials]; //index of which arm was pulled at the first stage
  int<lower = 0, upper = 2> a2[Nsubj,Ntrials]; //index of which arm was pulled at the second stage
  int<lower = 0, upper = 2> s2[Nsubj,Ntrials]; //index of the second stage state 
  int<lower = 0, upper = 1> reward[Nsubj,Ntrials]; //outcome of bandit arm pull
  int<lower = 0, upper = 2> map1[Nsubj,Ntrials]; //mapping of GO at the first stage 
  int<lower = 0, upper = 2> map2[Nsubj,Ntrials]; //mapping of GO at the second stage
  int<lower = 0, upper = 1> perCh1[Nsubj,Ntrials]; //perseveration for first stimuli at the first stage 
  int<lower = 0, upper = 1> perCh2[Nsubj,Ntrials]; //perseveration for second stimuli at the first stage
  int final_trl[1,Nsubj]; //number of trials (with no NA) 
}

parameters {
  real alpha1_subjects[Nsubj]; //learning rate - 1st stage
  real beta_subjects[Nsubj]; //softmax parameter - inverse temperature
  real lambda_subjects[Nsubj]; //eligibilty trace 
  real pers_subjects[Nsubj]; //perseveration 
  real gob_subjects[Nsubj]; //go bias 

  //hyper parameters 
  real mu_alpha1;
  real <lower = 0> tau_alpha1;
  real mu_beta;
  real <lower = 0> tau_beta;
  real mu_lambda;
  real <lower = 0> tau_lambda;
  real mu_pers;
  real <lower = 0> tau_pers;
  real mu_gob;
  real <lower = 0> tau_gob;

}

transformed parameters {
      real alpha1[Nsubj];
      real beta[Nsubj];
      real lambda[Nsubj];
      real pers[Nsubj];
      real go_b[Nsubj];
      matrix [Nsubj,Ntrials] log_lik;
      real <lower=0, upper=1> Qval [2,N2states,Narms];
      vector [2] Qnet; 
      vector [2] go_bias; 

      Qval = rep_array(0,2,N2states,Narms); // fill the Qvals with 0
      Qnet = rep_vector(0,Narms); // fill the Qnet with 0
      go_bias = rep_vector(0,Narms);
      log_lik = rep_matrix(0,Nsubj,Ntrials);


  for (subj in 1:Nsubj){
        real PE1;
        real PE2;

        alpha1[subj] = inv_logit(alpha1_subjects[subj]);
        beta[subj] = exp(beta_subjects[subj]);
        lambda[subj] = inv_logit(lambda_subjects[subj]);
        pers[subj] = pers_subjects[subj];
        go_b[subj] = gob_subjects[subj];
        go_bias[1] = go_b[subj];

                
        for (trial in 1:final_trl[1,subj]){
            //print("Q1s: ", Q1s);
            //print("Q2s: ", Q2s);
            Qnet[1] = Qval[1,1,1] + pers[subj]*perCh1[subj,trial] + go_bias[map1[subj,trial]] ;
            Qnet[2] = Qval[1,1,2] + pers[subj]*perCh2[subj,trial] + go_bias[3-map1[subj,trial]] ;
            //print("pers: ", pers[subj]);
            //print("perCh1: ", perCh1[subj,trial]);
            //print("perCh2: ", perCh2[subj,trial]);
            log_lik[subj,trial]=log_softmax(Qnet*beta[subj])[a1[subj,trial]];
            Qnet[1] = Qval[2,s2[subj,trial],1] + go_bias[map2[subj,trial]];
            Qnet[2] = Qval[2,s2[subj,trial],2] + go_bias[3-map2[subj,trial]];
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
  mu_beta ~ normal(0, 5);
  tau_beta ~ normal(0, 5);
  mu_lambda ~ normal(0, 5);
  tau_lambda ~ normal(0, 5);
  mu_pers ~ normal(0, 5);
  tau_pers ~ normal(0, 5);
  mu_gob ~ normal(0, 5);
  tau_gob ~ normal(0, 5);

  // indvidual level priors (subject parameters)
  alpha1_subjects ~ normal(mu_alpha1, tau_alpha1);
  beta_subjects ~ normal(mu_beta, tau_beta);
  lambda_subjects ~ normal(mu_lambda, tau_lambda);
  pers_subjects ~ normal(mu_pers, tau_pers);
  gob_subjects ~ normal(mu_gob, tau_gob);


  target += sum(log_lik);

}

