# Aim: Parameter recovery Hierarchical fit Stan for Null model 
# 5 Parameters : alpha1, beta1, lambda, perseveration, go bias

rm(list=ls())
library('rstan') # observe startup messages
library("truncnorm")
library(parallel)
library(gtools) #inv.logit function 
library(MASS)

# generate population and subject level parameters -----------------------------------------------------------

setwd("~/shared/0003_credit-assignment_go-no-go/06_stan/Hierarchical")
source('sim_null.R')
rndwlk<-read.csv('rndwlk_4frc_1000trials.csv',header=F)

#generate parameters and data for N agents. 
Nsubj =50       #number of agents
Nalt  =2         #number of alternatives
Ntrl  =300       #number of trials
Nparam=5         #number of parameters
N2states = 2     # number of 2nd stage states 

#population parameters
alpha1_mu     =0.5
beta1_mu      =5
lambda_mu    = 0.5
pers_mu      = 0.1 #לא יכול להיות 0
gob_mu      = 0.1 

#population aux parameters
alpha1_aux_mu    = logit(alpha1_mu)
beta1_aux_mu     = log(beta1_mu)
lambda_aux_mu    = logit(lambda_mu)
pers_aux_mu    = pers_mu # האם כאן לשים -0.5? 
gob_aux_mu    = gob_mu # האם כאן לשים -0.5? 

alpha1_aux_var = 0.5
beta1_aux_var  = 0.05
lambda_aux_var = 0.5
pers_aux_var = 0.05
gob_aux_var = 0.05

corr_param = 0
cov_param  =sqrt(alpha1_aux_var)*sqrt(beta1_aux_var)*sqrt(lambda_aux_var)*sqrt(pers_aux_var)*sqrt(gob_aux_var)*corr_param

#Create a mean vector and a variance-covariance matrix (i.e., sigma_matrix)
mu_vector    =c(alpha1_aux_mu,beta1_aux_mu,lambda_aux_mu,pers_aux_mu,gob_aux_mu)
tau          =c(alpha1_aux_var,beta1_aux_var,lambda_aux_var,pers_aux_var,gob_aux_var) #var vector
sigma_matrix = diag(tau)
sigma_matrix[!diag(nrow=Nparam)]=cov_param

#demonstrate conversion from cov matrix (sigma_matirx), to cor matrix (Omega) to cholesky factor (L_omega)
Omega=cov2cor(sigma_matrix)    #cov to cor
L_Omega=t(chol(Omega))         #cor to cholesky
round(t(L_Omega) %*% L_Omega,1)#cholesky back to cor
#משהו פה יוצא מוזר 

# sample aux parameters from the population with mu_vecto and cov matrix (sigma_matrix)
auxiliary_parameters = mvrnorm(n = Nsubj, mu = mu_vector, Sigma = sigma_matrix)

#convert auxiliary parameters to true parameters 
true.parms <-auxiliary_parameters
colnames(true.parms)<-c("alpha1","beta1","lambda","pers","go_bias")
true.parms[,1]<-inv.logit(true.parms[,1])
true.parms[,2]<-exp(true.parms[,2])
true.parms[,3]<-inv.logit(true.parms[,3])
true.parms[,4]<-true.parms[,4] # האם כאן לשים -0.5
true.parms[,5]<-true.parms[,5] # האם כאן לשים -0.5

hist(true.parms[,1])
hist(true.parms[,2])
hist(true.parms[,3])
hist(true.parms[,4])
hist(true.parms[,5])


#check that we got data with statistics as expected
print(paste('true alpha1 population parm is', alpha1_mu,'true sample mean is',mean(true.parms[,1])))
print(paste('true beta1 population parm is', beta1_mu,'true sample mean is',mean(true.parms[,2])))
print(paste('true lambda population parm is', lambda_mu,'true sample mean is',mean(true.parms[,3])))
print(paste('true pers population parm is', pers_mu,'true sample mean is',mean(true.parms[,4])))
print(paste('true go bias population parm is', gob_mu,'true sample mean is',mean(true.parms[,5])))

print(paste('true alpha1 population parm is', alpha1_aux_mu,'true sample mean is',mean(auxiliary_parameters[,1])))
print(paste('true beta1 population parm is', beta1_aux_mu,'true sample mean is',mean(auxiliary_parameters[,2])))
print(paste('true lambda population parm is', lambda_aux_mu,'true sample mean is',mean(auxiliary_parameters[,3])))
print(paste('true pers population parm is', pers_aux_mu,'true sample mean is',mean(auxiliary_parameters[,4])))
print(paste('true go bias population parm is', gob_aux_mu,'true sample mean is',mean(auxiliary_parameters[,5])))

print(paste('true alpha1 aux var parm is', alpha1_aux_var,'true sample mean is',var(auxiliary_parameters[,1])))
print(paste('true beta1 aux var parm is', beta1_aux_var,'true sample mean is',var(auxiliary_parameters[,2])))
print(paste('true lambda aux var parm is', lambda_aux_var,'true sample mean is',var(auxiliary_parameters[,3])))
print(paste('true pers aux var parm is', pers_aux_var,'true sample mean is',var(auxiliary_parameters[,4])))
print(paste('true go bias aux var parm is', gob_aux_var,'true sample mean is',var(auxiliary_parameters[,5])))

print(paste('true corr between aux parms is',corr_param,'true sample mean is',cor(auxiliary_parameters[,2],auxiliary_parameters[,5])))

# run a simulation study -----------------------------------------------------------
# simulating N agents in the 2 step task 

df<- lapply(1:Nsubj,function(s)           {
  
  df_subj=cbind(subj=rep(s,Ntrl),
                trial=(1:Ntrl),
                sim.null(Ntrl=Ntrl,x=true.parms[s,],rndwlk=rndwlk))
})

df<-do.call(rbind,df)

# model fit with stan --------------------------------------------
#prepare action and reward matrices (subject x trial)
a1=t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'ch1']}))
a2=t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'ch2']}))
s2=t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'state']}))
reward=t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'rw']}))
map1=t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'map1']}))
map2=t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'map2']}))
perCh1=t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'perCh1']}))
perCh2=t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'perCh2']}))

final_trl =list()
final_trl = t(sapply(1:Nsubj,function(subj) {final_trl[subj]=Ntrl-(sum(is.na(a1[subj,]))*1)})) #number of trials without missing values  

#לתקן את זה ככה שזה יחזיר בליסט ולךבדוק לגבי השורות וזה 
source('arrange_MD_forStan.R')
a1=arrange_MD_forStan(mydata=a1,newval=0)
a2=arrange_MD_forStan(mydata=a2,newval=0)  
s2=arrange_MD_forStan(mydata=s2,newval=0)  
reward=arrange_MD_forStan(mydata=reward,newval=0)  
map1=arrange_MD_forStan(mydata=map1,newval=0)  
map2=arrange_MD_forStan(mydata=map2,newval=0)  
perCh1=arrange_MD_forStan(mydata=perCh1,newval=0)  
perCh2=arrange_MD_forStan(mydata=perCh2,newval=0)  

#prepare data
model_data <- list(Nsubj = Nsubj,
                   Ntrials = Ntrl,
                   Narms = Nalt,
                   Nparam = Nparam,
                   N2states = N2states,
                   a1 = a1,
                   a2 = a2,
                   s2 = s2,
                   reward = reward,
                   map1 = map1,
                   map2 = map2,
                   perCh1=perCh1,
                   perCh2=perCh2,
                   final_trl= final_trl)

#fit Null model 
model_data[['Nparam']]=5
fit_null <- stan(file ="Hierarchical_null.stan" ,
                 data = model_data,
                 warmup = 1000,
                 iter = 2000,#20000
                 chains=4,
                 cores =4) 

print(fit_null)


# compare recovered parameters to true parameters  --------------------------------------------

#population level (hyperparameter)
rl_fit = fit_null
alpha1_aux_mu_recovered = summary(rl_fit , pars=c("mu[1]"))$summary[,1]
beta1_aux_mu_recovered = summary(rl_fit , pars=c("mu[2]"))$summary[,1]
lambda_aux_mu_recovered = summary(rl_fit , pars=c("mu[3]"))$summary[,1]
pers_aux_mu_recovered = summary(rl_fit , pars=c("mu[4]"))$summary[,1]
gob_aux_mu_recovered = summary(rl_fit , pars=c("mu[5]"))$summary[,1]

alpha1_aux_sigma_recovered = summary(rl_fit , pars=c("tau[1]"))$summary[,1]
beta1_aux_sigma_recovered = summary(rl_fit , pars=c("tau[2]"))$summary[,1]
lambda_aux_sigma_recovered = summary(rl_fit , pars=c("tau[3]"))$summary[,1]
pers_aux_sigma_recovered = summary(rl_fit , pars=c("tau[4]"))$summary[,1]
gob_aux_sigma_recovered = summary(rl_fit , pars=c("tau[5]"))$summary[,1]

cov_param_recovered = summary(rl_fit , pars=c("sigma_matrix[2,1]"))$summary[,1]

#compare recovered to true population parameters
print(paste('true alpha1 population parm is', alpha1_mu,'true sample mean is',mean(true.parms[,1]),'and recovered is',inv.logit(alpha1_aux_mu_recovered)))
print(paste('true beta1 population parm is', beta1_mu,'true sample mean is',mean(true.parms[,2]),'and recovered is',exp(beta1_aux_mu_recovered)))
print(paste('true lambda population parm is', lambda_mu,'true sample mean is',mean(true.parms[,3]),'and recovered is',inv.logit(lambda_aux_mu_recovered)))
print(paste('true pers population parm is', pers_mu,'true sample mean is',mean(true.parms[,4]),'and recovered is',pers_aux_mu_recovered))
print(paste('true gob population parm is', gob_mu,'true sample mean is',mean(true.parms[,5]),'and recovered is',gob_aux_mu_recovered))

print(paste('true alpha1 aux population parm is', alpha1_aux_mu,'true sample mean is',mean(auxiliary_parameters[,1]),'and recovered is',alpha1_aux_mu_recovered))
print(paste('true beta1 aux population parm is', beta1_aux_mu,'true sample mean is',mean(auxiliary_parameters[,2]),'and recovered is',beta1_aux_mu_recovered))
print(paste('true lambda aux population parm is', lambda_aux_mu,'true sample mean is',mean(auxiliary_parameters[,3]),'and recovered is',lambda_aux_mu_recovered))
print(paste('true pers aux population parm is', pers_aux_mu,'true sample mean is',mean(auxiliary_parameters[,4]),'and recovered is',pers_aux_mu_recovered))
print(paste('true gob aux population parm is', gob_aux_mu,'true sample mean is',mean(auxiliary_parameters[,5]),'and recovered is',gob_aux_mu_recovered))

print(paste('true alpha1 aux var parm is', alpha1_aux_var,'true sample mean is',var(auxiliary_parameters[,1]),'and recovered is',alpha1_aux_sigma_recovered))
print(paste('true beta1 aux var parm is', beta1_aux_var,'true sample mean is',var(auxiliary_parameters[,2]),'and recovered is',beta1_aux_sigma_recovered))
print(paste('true lambda aux var parm is', lambda_aux_var,'true sample mean is',var(auxiliary_parameters[,3]),'and recovered is',lambda_aux_sigma_recovered))
print(paste('true pers aux var parm is', pers_aux_var,'true sample mean is',var(auxiliary_parameters[,4]),'and recovered is',pers_aux_sigma_recovered))
print(paste('true gob aux var parm is', gob_aux_var,'true sample mean is',var(auxiliary_parameters[,5]),'and recovered is',gob_aux_sigma_recovered))

print(paste('true corr between aux parms is', corr_alpha_beta,'true sample mean is',cor(auxiliary_parameters[,1],auxiliary_parameters[,2]),
            'and recovered is',cov_alpha_beta_recovered/(sqrt(alpha_aux_sigma_recovered)*sqrt(beta_aux_sigma_recovered))))

#individual level parameters (subjects parameters)
alpha1_individual_recovered=summary(rl_fit , pars=c("alpha1"))$summary[,1] 
beta1_individual_recovered=summary(rl_fit , pars=c("beta1"))$summary[,1]
lambda_individual_recovered=summary(rl_fit , pars=c("lambda"))$summary[,1]
pers_individual_recovered=summary(rl_fit , pars=c("pers"))$summary[,1]
gob_individual_recovered=summary(rl_fit , pars=c("gob"))$summary[,1]

plot(true.parms[,1],(alpha1_individual_recovered))
plot(true.parms[,2],(beta1_individual_recovered))
plot(true.parms[,3],(lambda_individual_recovered))
plot(true.parms[,4],(pers_individual_recovered))
plot(true.parms[,5],(gob_individual_recovered))

round(cor(cbind(true.parms[,1],true.parms[,2],true.parms[,3],true.parms[,4],true.parms[,5]),cbind(alpha1_individual_recovered,beta1_individual_recovered,lambda_individual_recovered,pers_individual_recovered,gob_individual_recovered)),2)
round(cor(cbind(alpha1_individual_recovered,beta1_individual_recovered,lambda_individual_recovered,pers_individual_recovered,gob_individual_recovered),cbind(alpha1_individual_recovered,beta1_individual_recovered,lambda_individual_recovered,pers_individual_recovered,gob_individual_recovered)),2)
round(cor(cbind(true.parms[,1],true.parms[,2],true.parms[,3],true.parms[,4],true.parms[,5]),cbind(true.parms[,1],true.parms[,2],true.parms[,3],true.parms[,4],true.parms[,5])),2)

#additional
summary(rl_fit, pars=c("L_Omega"))$summary
L_Omega_recovered = matrix(nrow=5,ncol=5)
L_Omega_recovered[1,c(1:5)]<-summary(rl_fit, pars=c("L_Omega"))$summary[c(1:5),1]
L_Omega_recovered[2,c(1:5)]<-summary(rl_fit, pars=c("L_Omega"))$summary[c(6:10),1]
L_Omega_recovered[3,c(1:5)]<-summary(rl_fit, pars=c("L_Omega"))$summary[c(11:15),1]
L_Omega_recovered[4,c(1:5)]<-summary(rl_fit, pars=c("L_Omega"))$summary[c(16:20),1]
L_Omega_recovered[5,c(1:5)]<-summary(rl_fit, pars=c("L_Omega"))$summary[c(21:25),1]
round(L_Omega_recovered,2)
L_Omega
# לא בטוחה איך לבדוק את זה... 
round(L_Omega_recovered*t(L_Omega_recovered),2)

summary(rl_fit, pars=c("sigma_matrix"))$summary[,1]
sigma_matrix_recovered = matrix(nrow=5,ncol=5)
sigma_matrix_recovered[1,c(1:5)]<-summary(rl_fit, pars=c("sigma_matrix"))$summary[c(1:5),1]
sigma_matrix_recovered[2,c(1:5)]<-summary(rl_fit, pars=c("sigma_matrix"))$summary[c(6:10),1]
sigma_matrix_recovered[3,c(1:5)]<-summary(rl_fit, pars=c("sigma_matrix"))$summary[c(11:15),1]
sigma_matrix_recovered[4,c(1:5)]<-summary(rl_fit, pars=c("sigma_matrix"))$summary[c(16:20),1]
sigma_matrix_recovered[5,c(1:5)]<-summary(rl_fit, pars=c("sigma_matrix"))$summary[c(21:25),1]
round(sigma_matrix_recovered,2)
sigma_matrix


# Visualization -----------------------------------------------------------

traceplot(fit_null, pars = c("mu"), inc_warmup = FALSE)
plot(fit_null, plotfun = "hist", pars = c("mu", "tau"))
