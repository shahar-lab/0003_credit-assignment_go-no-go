rm(list=ls())

require(dplyr)
load('03_data/02_aggregated_data/complete_task.Rdata')
df<-df_task_clean
str(df) #checking the data frame 


# logistic regression 1 -----------------------------------------------------

df_log = list(N=dim(df)[1],
                 K=4,
                 x= df%>%select(reward_oneback,key1_oneback,key2_oneback,adhd_group_score),
                 y= df$stay1_bandit)

#runing the stan code
require(rstan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write=TRUE)

#define the stan logistic regression model 
log_fit<- stan(file = "my_stan1.stan", data=df_log, iter=1000,chains=2) #iter - number of MCMC sampels 
 summary(log_fit , pars=c("alpha","beta"))
#if Rhat is not close to 1.01 it means the model ran efficiently 
#comparing to glm
summary(glm(stay1_bandit~reward_oneback*key1_oneback*key2_oneback, family = "binomial", data = df)) #should induce similar estimates values
#plot 
traceplot(log_fit,c("alpha","beta"),ncol=1,nrow=6,inc_warmup=F)

#questions: 
#1. how many chains and iteration? 
#2. does it consider interaction effect? 
#3. ADHD compound or group score 
#4.  stay2_state - logi or num (now is logi) 


# logistic regression 2 ---------------------------------------------------
source(file = "log_stan_2.stan")
# Create a list with the chosen variables
data.list_log2 <- list(N = dim(df)[1], stay1 = df$stay1_bandit, prv_reward = df$reward_oneback, prv_key1 = df$key1_oneback,
                  prv_key2 = df$key2_oneback, adhd = df$adhd_group_score)
str(data.list_log2)

# Estimate the model
fit <- stan(file = "log_stan_2.stan", data = data.list_log2, iter = 1000, chains = 4)
print(fit, digits = 3)




# Hierarchical logistic regression ----------------------------------------

df_h <- list(N=dim(df)[1],
             M=56,
             K=3, # K=?
             x= df%>%select(reward_oneback,key1_oneback,key2_oneback),
             y= df$stay1_bandit,
             g=df[,6]) # g=?  

#define the stan Hierarchical logistic regression model 
H_log_fit<- stan(file = "HLR_stan_1.stan", data=df_h, iter=1000,chains=2) #iter - number of MCMC sampels 
summary(H_log_fit , pars=c("alpha","beta"))
#if Rhat is not close to 1.01 it means the model ran efficiently 
#plot 
traceplot(H_log_fit,c("alpha","beta[1]","beta[2]","beta[3]","beta[4]","sigma"),
          ncol=1,nrow=6,inc_warmup=F)
#it is possible to change the name of variables brom beta1 to beta_prv_reward and so on 

#define the stan Hierarchical logistic regression model 
BHR_fit<- stan(file = "HLR_stan_1.stan", data=data_list, iter=1000,chains=2) #iter - number of MCMC sampels 
summary(H_log_fit , pars=c("alpha","beta"))
#if Rhat is not close to 1.01 it means the model ran efficiently

