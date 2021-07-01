# Parameter recovery Hierarchical fit Stan
#basic 2 step task
# 4 parameters: alpha1, alpha2, beta, lambda

rm(list=ls())

library("rstan") # observe startup messages
library("truncnorm")
library(parallel)
library(gtools) #inv.logit function 

source('sim_basic.R')
rndwlk<-read.csv('rndwlk_4frc_1000trials.csv',header=F) #each row is a bandit
detectCores()

# simulate data -----------------------------------------------------------
#generate parameters and data for N agents. 
Nsubj   <-5          #number of agents
Nalt<-2               #number of alternatives
Ntrl<-100           #number of trials
Nexp <- 1           #number of experiments
N2states <- 2      #number of second stated at the second stage


alpha1_mu = 0.5 
alpha1_sigma = 0.2
alpha2_mu = 0.75 
alpha2_sigma = 0.05
beta_mu = 3
beta_sigma = 2
lambda_mu = 5
lambda_sigma = 1

true.parms<-data.frame(alpha1=rtruncnorm(Nsubj, mean =alpha1_mu, sd = alpha1_sigma, a = 0,b = 1),
                       alpha2=rtruncnorm(Nsubj, mean =alpha2_mu, sd = alpha2_sigma, a = 0,b = 1),
                       beta =rtruncnorm(Nsubj, mean =beta_mu, sd = beta_sigma, a = 0.01,b = 10),
                       lambda=rtruncnorm(Nsubj, mean =lambda_mu, sd = lambda_sigma, a = 0,b = Inf))
hist(true.parms$alpha1)
hist(true.parms$alpha2)
hist(true.parms$beta)
hist(true.parms$lambda)

df_2<-  lapply(1:length(Nalt), function(alt) {
  lapply(1:length(Ntrl), function(trl) {
    lapply(1:Nexp, function(experiment){
      mclapply(1:Nsubj,function(s)               {
        sim_basic_2st(Ntrl[trl],Nalt[alt],Nexp[experiment],x=true.parms[s,],rndwlk=rndwlk[,,],subj=s,experiment=Nexp[experiment],Ntrls=Ntrl[trl])},
        mc.cores=2)})})})

df_basic<-
  mclapply(1:Nsubj,function(s) {
    sngdf   <-lapply(1:Nexp, function(experiment) {sim_basic_2st(x=true.parms[s,],rndwlk=rndwlk[,,],subj=s,experiment,Ntrls=Ntrl)})
    sngdf   <-do.call(rbind,sngdf)}
    ,mc.cores =2
  )

df=df_basic

#prepare action and reward matrices (subject x trial)
a1=t(sapply(1:Nsubj,function(subj) {df[[subj]][,'ch1']}))
a2=t(sapply(1:Nsubj,function(subj) {df[[subj]][,'ch2']}))
s2=t(sapply(1:Nsubj,function(subj) {df[[subj]][,'state']}))
reward=t(sapply(1:Nsubj,function(subj) {df[[subj]][,'rw']}))

#fit stan model
model_data <- list(Nsubj = Nsubj,
                   Ntrials = Ntrl[trl],
                   Narms = Nalt[alt],
                   N2states = N2states,
                   a1 = a1,
                   a2 = a2,
                   s2 = s2,
                   reward = reward)

#options(mc.cores = parallel::detectCores())
#rstan_options(auto_write=TRUE)

rl_fit<- stan(file = "rl_2s_h_1205.stan", data=model_data, iter=2000,chains=4,cores =4) #iter - number of MCMC samples 
#m = summary(rl_fit , pars=c("alpha","beta"))
#c(m$summary[1,1],m$summary[2,1])




# parameter recovery with stan --------------------------------------------
str(df) #checking the data frame 

sim_param<-
  lapply(1:length(Nalt), function(alt) {
    lapply(1:length(Ntrl), function(trl) {
      print(paste('alt=',alt,'trl=',trl))
      
      #prepare action and reward matrices (subject x trial)
      a1=t(sapply(1:Nsubj,function(subj) {df[[alt]][[trl]][[subj]]['ch1']}))
      reward=t(sapply(1:Nsubj,function(subj) {df[[alt]][[trl]][[subj]][,'reward']}))
      
      #fit stan model
      model_data <- list(Nsubj = Nsubj,
                         Ntrials = Ntrl[trl],
                         Narms = Nalt[alt],
                         a1 = a1,
                         reward = reward)
      
      #options(mc.cores = parallel::detectCores())
      #rstan_options(auto_write=TRUE)
      
      rl_fit<- stan(file = "rl_Hierarchical_1.stan", data=model_data, iter=2000,chains=4,cores =4) #iter - number of MCMC samples 
      #m = summary(rl_fit , pars=c("alpha","beta"))
      #c(m$summary[1,1],m$summary[2,1])
      
      
    })
  })

#population level (hyperparameter)
alpha_population_recovered=summary(rl_fit , pars=c("mu_alpha"))$summary[,1]
beta_population_recovered=summary(rl_fit , pars=c("mu_beta"))$summary[,1]
alpha_tau_population_recovered=summary(rl_fit , pars=c("tau_alpha"))$summary[,1]
beta_tau_population_recovered=summary(rl_fit , pars=c("tau_beta"))$summary[,1]

plot(alpha_mu,inv.logit(alpha_population_recovered))
plot(alpha_sigma,inv.logit(alpha_tau_population_recovered))
plot(beta_mu,exp(beta_population_recovered))
plot(beta_sigma,exp(beta_tau_population_recovered))

#individual level parameters (subjects parameters)
alpha_recovered=summary(rl_fit , pars=c("alpha_subjects"))$summary[,1] #אולי זה צריך להיות alpha[1]
beta_recovered=summary(rl_fit , pars=c("beta_subjects"))$summary[,1]
plot(true.parms[,1],inv.logit(alpha_recovered))
plot(true.parms[,2],exp(beta_recovered))

##2nd version
options(mc.cores = parallel::detectCores())
rstan_options(auto_write=TRUE)

my_model<- stan_model(file = "rl_basic.stan") 
sample <- sampling(object = my_model, data = model_data)

fit <-optimizing(object = my_model, data = model_data)
#c(fit$par[1],fit$par[2])



my_model<- stan_model(file = "rl_basic.stan") 
sample <- sampling(object = my_model, data = model_data)

plot(sample, plotfun = "hist", pars= "alpha")
plot(sample, plotfun = "hist", pars= "beta")

library("shinystan")
launch_shinystan(sample)

#calculate cor between true and recovered params
df.tbl   <-lapply(1:length(Nalt), function(alt) {
  lapply(1:length(Ntrl), function(trl) {
    data.frame(Nalt=Nalt[alt],
               Ntrl=Ntrl[trl],
               cor.alpha=cor(true.parms$alpha,inv.logit((do.call(rbind,alpha[[alt]][[trl]])))),
               cor.beta=cor(true.parms$beta,exp((do.call(rbind,beta[[alt]][[trl]])))))
  })})

df.tbl<-do.call(rbind,lapply(1:length(Nalt), function(alt) {do.call(rbind,df.tbl[[alt]])}))

#print table to file
df.tbl %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))        







# Arch --------------------------------------------------------------------
require(dplyr)
library(tidyr)
library("tidyverse")
library(knitr) #for printing the correlation table to a file
library(kableExtra) #for printing the correlation table to a file
library(triangle) #triangular distribution
library(msm)#for generating the parameters 


my_model <- stan_model(file = "rl_basic.stan")
fit <- optimizing(object = my_model, data = model_data)

#get alpha and beta estimates
fit$par[1]
fit$par[2]


#if Rhat is not close to 1.01 it means the model ran efficiently 
#comparing to glm
summary(glm(stay1_bandit~reward_oneback, family = "binomial", data = df)) #should induce similar estimates values
#plot 
traceplot(log_fit,c("alpha","beta"),ncol=1,nrow=6,inc_warmup=F)



