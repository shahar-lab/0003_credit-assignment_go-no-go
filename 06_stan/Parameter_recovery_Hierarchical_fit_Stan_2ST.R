# Parameter recovery Hierarchical fit Stan
#basic 2 step task
# 4 parameters: alpha1, alpha2, beta, lambda

rm(list=ls())

library("rstan") # observe startup messages
library("truncnorm")
library(parallel)
library(gtools) #inv.logit function 

#source('sim_basic.R')
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
alpha1_sigma = 0.25
#alpha2_mu = 0.5 
#alpha2_sigma = 0.25
beta_mu = 5 
beta_sigma = 1.5 
lambda_mu = 0.5
lambda_sigma = 0.25

true.parms<-data.frame(alpha1=rtruncnorm(Nsubj, mean =alpha1_mu, sd = alpha1_sigma, a = 0,b = 1),
                       #alpha2=rtruncnorm(Nsubj, mean =alpha2_mu, sd = alpha2_sigma, a = 0,b = 1),
                       beta =rtruncnorm(Nsubj, mean =beta_mu, sd = beta_sigma, a = 0.01,b = 10),
                       lambda=rtruncnorm(Nsubj, mean =lambda_mu, sd = lambda_sigma, a = 0,b = 1)
                       )
hist(true.parms$alpha1)
hist(true.parms$alpha2)
hist(true.parms$beta)
hist(true.parms$lambda)

df_2<-  lapply(1:length(Nalt), function(alt) {
  lapply(1:length(Ntrl), function(trl) {
    lapply(1:Nexp, function(experiment){
    mclapply(1:Nsubj,function(s)               {
      sim_basic_2st(Ntrl[trl],Nalt[alt],Nexp[experiment],x=true.parms[s,],rndwlk=rndwlk[,,],subj=s,experiment=Nexp[experiment],Ntrls=Ntrl[trl])},
      mc.cores=4)})})})

source('sim_basic_2p.R')
df_basic<-
  mclapply(1:Nsubj,function(s) {
    sngdf   <-lapply(1:Nexp, function(experiment) {sim_basic_2st(x=true.parms[s,],rndwlk=rndwlk[,,],subj=s,experiment,Ntrls=Ntrl)})
    sngdf   <-do.call(rbind,sngdf)}
    ,mc.cores =2
    )

df=df_basic

source('sim_basic_3p.R')
df_basic<-
  mclapply(1:Nsubj,function(s) {
    sngdf   <-lapply(1:Nexp, function(experiment) {sim_basic_2st(x=true.parms[s,],rndwlk=rndwlk[,,],subj=s,experiment,Ntrls=Ntrl)})
    sngdf   <-do.call(rbind,sngdf)}
    ,mc.cores =4
  )

df=df_basic

source('sim_basic_3p_2v.R')
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
trl=alt=1
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

#2 parameters
rl_fit<- stan(file = "basic_2p_1305.stan", data=model_data, iter=2000,chains=4,cores =4) #iter - number of MCMC samples 
rl_fit@.MISC[["summary"]][["msd"]]
#plot individual alpha
plot(true.parms$alpha1,rl_fit@.MISC[["summary"]][["msd"]][105:154])
#plot individual bets
plot(true.parms$beta,rl_fit@.MISC[["summary"]][["msd"]][155:204])

#rl_fit<- stan(file = "rl_2s_h_1205.stan", data=model_data, iter=2000,chains=4,cores =4) #iter - number of MCMC samples 
#m = summary(rl_fit , pars=c("alpha","beta"))
#c(m$summary[1,1],m$summary[2,1])

#3 parameters
rl_fit<- stan(file = "basic_3p_1805.stan", data=model_data, iter=2000,chains=4,cores =4) #iter - number of MCMC samples 
print(rl_fit)
#plot individual alpha1
plot(true.parms$alpha1,rl_fit@.MISC$summary[[1]][157:206,][,1])
#plot individual alpha2
plot(true.parms$alpha2,rl_fit@.MISC$summary[[1]][207:256,][,1])
#plot individual bets
plot(true.parms$beta,rl_fit@.MISC$summary[[1]][257:306,][,1])

traceplot(rl_fit , pars = c("beta","sigma_e"),14 inc_warmup = FALSE)

#3 parameters - lambda
rl_fit<- stan(file = "basic_3p_1805_2v.stan", data=model_data, iter=2000,chains=4,cores =4) #iter - number of MCMC samples 
print(rl_fit)

inv.logit(rl_fit@.MISC$summary[[1]]['mu_alpha1',1])
exp(rl_fit@.MISC$summary[[1]]['mu_beta',1])
inv.logit(rl_fit@.MISC$summary[[1]]['mu_lambda',1])


#plot individual alpha1
plot(true.parms$alpha1,rl_fit@.MISC$summary[[1]][382:506,][,1])
#plot individual bets
plot(true.parms$beta,rl_fit@.MISC$summary[[1]][632:756,][,1])
#plot individual lambda
plot(true.parms$lambda,rl_fit@.MISC$summary[[1]][507:631,][,1])



# 5 parameters -----------------------------------------------------------
#parameters: alpha1, beta, lambda,perseveration, go bias

#simulate data 
#generate parameters and data for N agents. 
Nsubj   <-50           #number of agents
Nalt<-2               #number of alternatives
Ntrl<-300           #number of trials
Nexp <- 1           #number of experiments
N2states <- 2      #number of second stated at the second stage


alpha1_mu = 0.5  
alpha1_sigma = 0.25
beta_mu = 5 
beta_sigma = 1.5 
lambda_mu = 0.5
lambda_sigma = 0.25
pers_mu = 0
pers_sigma = 0.25
gob_mu = 0
gob_sigma = 0.25

true.parms<-data.frame(alpha1=rtruncnorm(Nsubj, mean =alpha1_mu, sd = alpha1_sigma, a = 0,b = 1),
                       beta =rtruncnorm(Nsubj, mean =beta_mu, sd = beta_sigma, a = 0.01,b = 10),
                       lambda=rtruncnorm(Nsubj, mean =lambda_mu, sd = lambda_sigma, a = 0,b = 1),
                       pers=rnorm(Nsubj, mean =pers_mu, sd = pers_sigma),
                       go_b=rnorm(Nsubj, mean =gob_mu, sd = gob_sigma)
)

hist(true.parms$alpha1)
hist(true.parms$beta)
hist(true.parms$lambda)
hist(true.parms$pers)
hist(true.parms$go_b)

source('sim_basic_5p_2005.R')
df_basic<-
  mclapply(1:Nsubj,function(s) {
    sngdf   <-lapply(1:Nexp, function(experiment) {sim_basic_2st(x=true.parms[s,],rndwlk=rndwlk[,,],subj=s,experiment,Ntrls=Ntrl)})
    sngdf   <-do.call(rbind,sngdf)}
    ,mc.cores =8
  )

df=df_basic



#prepare action and reward matrices (subject x trial)
a1=t(sapply(1:Nsubj,function(subj) {df[[subj]][,'ch1']}))
a2=t(sapply(1:Nsubj,function(subj) {df[[subj]][,'ch2']}))
s2=t(sapply(1:Nsubj,function(subj) {df[[subj]][,'state']}))
reward=t(sapply(1:Nsubj,function(subj) {df[[subj]][,'rw']}))
map1=t(sapply(1:Nsubj,function(subj) {df[[subj]][,'map1']}))
map2=t(sapply(1:Nsubj,function(subj) {df[[subj]][,'map2']}))
perCh1=t(sapply(1:Nsubj,function(subj) {df[[subj]][,'perCh1']}))
perCh2=t(sapply(1:Nsubj,function(subj) {df[[subj]][,'perCh2']}))


#fit stan model
trl=alt=1
model_data <- list(Nsubj = Nsubj,
                   Ntrials = Ntrl[trl],
                   Narms = Nalt[alt],
                   N2states = N2states,
                   a1 = a1,
                   a2 = a2,
                   s2 = s2,
                   reward = reward,
                   map1 = map1,
                   map2 = map2,
                   perCh1=perCh1,
                   perCh2=perCh2)


rl_fit<- stan(file = "5p_2005.stan", data=model_data, iter=2000,chains=4,cores =4) #iter - number of MCMC samples 
print(rl_fit)

inv.logit(rl_fit@.MISC$summary[[1]]['mu_alpha1',1])
exp(rl_fit@.MISC$summary[[1]]['mu_beta',1])
inv.logit(rl_fit@.MISC$summary[[1]]['mu_lambda',1])
rl_fit@.MISC$summary[[1]]['mu_pers',1]
rl_fit@.MISC$summary[[1]]['mu_gob',1]


#plot individual alpha1
plot(true.parms$alpha1,rl_fit@.MISC$summary[[1]][261:310,][,1])
#plot individual bets
plot(true.parms$beta,rl_fit@.MISC$summary[[1]][311:(311+Nsubj-1),][,1])
#plot individual lambda
plot(true.parms$lambda,rl_fit@.MISC$summary[[1]][361:(361+Nsubj-1),][,1])
#plot individual perserveration
plot(true.parms$pers,rl_fit@.MISC$summary[[1]][411:(411+Nsubj-1),][,1])
#plot individual go bias
plot(true.parms$go_b,rl_fit@.MISC$summary[[1]][461:(461+Nsubj-1),][,1])









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



