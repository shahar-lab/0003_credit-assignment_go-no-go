data {
  int<lower = 1> Nsubj; //number of subjects
  int<lower = 1> Ntrials; //number of trials
  int<lower = 2> Narms; //number of alternatives 
  int<lower = 1> Nparam; //number of parameters
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
  vector[Nparam] auxiliary_parameters[Nsubj]; 
  //hyper parameters 
  vector[Nparam] mu;
  vector<lower=0>[Nparam] tau; //vector of random effects variance
  cholesky_factor_corr[Nparam] L_Omega;
}

transformed parameters {
      real alpha1[Nsubj];
      real beta1[Nsubj];
      real lambda[Nsubj];
      real pers[Nsubj];
      real gob[Nsubj]; // go bias
      real gamma1[Nsubj];

      matrix [Nsubj,Ntrials] log_lik;
      real<lower=0, upper=1> Qval [2,N2states,Narms];
      vector[2] Qnet; 
      vector[2] go_bias;
      vector[2] gamma_1;


      matrix[Nparam,Nparam] sigma_matrix;
      sigma_matrix = diag_pre_multiply(tau, (L_Omega*L_Omega'));
      sigma_matrix = diag_post_multiply(sigma_matrix, tau);
      
      Qval = rep_array(0,2,N2states,Narms); // fill the Qvals with 0
      Qnet = rep_vector(0,Narms); // fill the Qnet with 0
      go_bias = rep_vector(0,Narms);
      gamma_1 = rep_vector(0,Narms);
      log_lik = rep_matrix(0,Nsubj,Ntrials);

for (subj in 1:Nsubj){
        real PE1;
        real PE2;
        int resp1; // 1st stage response was GO (1) or No-go (2)
        int resp2; // 2nd stage response was GO (1) or No-go (2)

        alpha1[subj] = inv_logit(auxiliary_parameters[subj][1]);
        beta1[subj] = exp(auxiliary_parameters[subj][2]);
        lambda[subj] = inv_logit(auxiliary_parameters[subj][3]);
        pers[subj] = auxiliary_parameters[subj][4];
        gob[subj] = auxiliary_parameters[subj][5];
        gamma1[subj] = auxiliary_parameters[subj][6];
        go_bias[1] = gob[subj];
        gamma_1[1] =1;
        gamma_1[2] =gamma1[subj];


        for (trial in 1:final_trl[1,subj]){
            Qnet[1] = Qval[1,1,1] + pers[subj]*perCh1[subj,trial] + go_bias[map1[subj,trial]] ;
            Qnet[2] = Qval[1,1,2] + pers[subj]*perCh2[subj,trial] + go_bias[3-map1[subj,trial]] ;
            log_lik[subj,trial]=log_softmax(Qnet*beta1[subj])[a1[subj,trial]];
            
            Qnet[1] = Qval[2,s2[subj,trial],1] + go_bias[map2[subj,trial]];
            Qnet[2] = Qval[2,s2[subj,trial],2] + go_bias[3-map2[subj,trial]];
            log_lik[subj,trial]+=log_softmax(Qnet*beta1[subj])[a2[subj,trial]];
            
            PE1= Qval[2,s2[subj,trial],a2[subj,trial]] - Qval[1,1,a1[subj,trial]];
            PE2= reward[subj,trial] - Qval[2,s2[subj,trial],a2[subj,trial]];
            
            resp1 = 2-(map1[subj,trial] == a1[subj,trial])*1;
            resp2 = 2-(map2[subj,trial] == a2[subj,trial])*1;
            
            Qval[1,1,a1[subj,trial]] += alpha1[subj] * gamma_1[resp1] * PE1+alpha1[subj] * gamma_1[resp2] *lambda[subj] * PE2;
            Qval[2,s2[subj,trial],a2[subj,trial]] += alpha1[subj] * gamma_1[resp2] * PE2;
        } 
  }
}

model {
  
  // population level priors (hyper-parameters)
  mu ~ normal(0, 5);
  tau ~ cauchy(0, 1);
  L_Omega ~ lkj_corr_cholesky(2); //the correlations in the off-diagonals are near zero


  // indvidual level priors (subject parameters)
  auxiliary_parameters ~ multi_normal(mu, sigma_matrix);

  target += sum(log_lik);

}
