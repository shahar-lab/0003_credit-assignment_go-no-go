# Parameter recovery Hierarchical fit Stan
#basic 2 step task
#parameters: alpha1, beta, lambda,perseveration, go bias

rm(list=ls())

library("rstan") # observe startup messages
library("truncnorm")
library(parallel)
library(gtools) #inv.logit function 

rndwlk<-read.csv('rndwlk_4frc_1000trials.csv',header=F) #each row is a bandit
detectCores()

#simulate data 
#generate parameters and data for N agents. 
Nsubj   <-50           #number of agents
Nalt<-2               #number of alternatives
Ntrl<-300           #number of trials
Nexp <- 1           #number of experiments
N2states <- 2      #number of second stated at the second stage
pmax.missing_value<-0.1       #max number of simulated missing value 


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
    #insert missing data
    index  =sample(1:Ntrl,as.integer(runif(1,min=0,max=pmax.missing_value*Ntrl)))
    sngdf[[1]][index,-c(1,2,3)] = NA
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


final_trl =list()
final_trl = t(sapply(1:Nsubj,function(subj) {final_trl[subj]=Ntrl-(sum(is.na(a1[subj,]))*1)})) #number of trials without missing values  

source('arrange_MD_forStan.R')
a1=arrange_MD_forStan(mydata=a1,newval=0)
a2=arrange_MD_forStan(mydata=a2,newval=0)  
s2=arrange_MD_forStan(mydata=s2,newval=0)  
reward=arrange_MD_forStan(mydata=reward,newval=0)  
map1=arrange_MD_forStan(mydata=map1,newval=0)  
map2=arrange_MD_forStan(mydata=map2,newval=0)  
perCh1=arrange_MD_forStan(mydata=perCh1,newval=0)  
perCh2=arrange_MD_forStan(mydata=perCh2,newval=0)  


#fit stan model
model_data <- list(Nsubj = Nsubj,
                   Ntrials = Ntrl,
                   Narms = Nalt,
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


rl_fit<- stan(file = "5p_mv_2505.stan", data=model_data, iter=2000,chains=4,cores =8) #iter - number of MCMC samples 
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
