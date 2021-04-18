library(R.matlab)
library(parallel)


rm(list=ls())
detectCores()
source('01_functions/01_gong_functions.R')

comper = data.frame()
#### Simulate data ----

#rndwlk=readMat('myfiles/nspn_rndwlk_frcXstateXtrlXcounter.mat')[[1]]
rndwlk=read.csv('rndwlk_v1.csv')
Nsubj=50*2 #(50*2)
Nexp =10 #10 
Ntrls=1000

##### origin ------------------
#simulate parameters
source('01_functions/01_gong_rand_params.R')
x<-lapply(1:Nsubj,function(s) {rand_params()})
x<-do.call(rbind,x)

#simulate data
source('02_models/01_gong_simme.R')

df<-
  mclapply(1:Nsubj,function(s) {
    sngdf   <-lapply(1:Nexp, function(experiment) {gong_simme(x[s,],rndwlk=rndwlk[,,],subj=s,experiment,Ntrls)})
    sngdf   <-do.call(rbind,sngdf)}
    ,mc.cores =2)
x1=x

###### null model---------------------------------------------------------
#simulate parameters
source('01_functions/01_gong_rand_params_null.R')
x<-lapply(1:Nsubj,function(s) {rand_params_null()})
x<-do.call(rbind,x)

#simulate data
source('02_models/01_gong_simme_null.R')

df_null<-
  mclapply(1:Nsubj,function(s) {
    sngdf   <-lapply(1:Nexp, function(experiment) {gong_simme_null(x[s,],rndwlk=rndwlk[,,],subj=s,experiment,Ntrls)})
    sngdf   <-do.call(rbind,sngdf)}
    ,mc.cores =2)
x_null=x
####### model _1 ---------------------------------------------------------
#simulate parameters
source('01_functions/01_gong_rand_params_m1.R')
x<-lapply(1:Nsubj,function(s) {rand_params_m1()})
x<-do.call(rbind,x)

#simulate data
source('02_models/01_gong_simme_m1.R')

df_model_1<-
  mclapply(1:Nsubj,function(s) {
    sngdf   <-lapply(1:Nexp, function(experiment) {gong_simme_m1(x[s,],rndwlk=rndwlk[,,],subj=s,experiment,Ntrls)})
    sngdf   <-do.call(rbind,sngdf)}
    ,mc.cores =2)

x2=x
###parameter recovery ----
### original ------------
source('02_models/01_gong_fitme.R')
fitparms<-data.frame()
Nparms<-dim(x)[2]
start_time <- Sys.time()

#optim
y<-
  lapply(1:Nsubj,function(s)
  {
    fit<- optim(par = runif(Nparms,-3,3),
                fn = gong_fitme,
                df = df[[s]],
                Ntrls = dim(df[[s]])[1],
                lower = rep(-3,Nparms),
                upper = rep(3,Nparms),
                method = "L-BFGS-B")$par})
#mc.cores =2)
mytime <- Sys.time() - start_time

#transform and copy best fitted params
y<-do.call(rbind,y)
#y<-transform_params(y,c('logit','exp','logit','logit','logit','logit','logit','logit','logit','logit'))
#y[,5:8]<-y[,5:7]-.5

#model parameters
y1<-data.frame(
  sim.alpha1        =mylogit(y[,1]),
  #  sim.alpha2        =mylogit(y[,1]),
  sim.gamma1        =mylogit(y[,2]),
  sim.gamma2        =mylogit(y[,3]),
  sim.beta1         =exp(y[,4]),
  sim.lambda        =mylogit(y[,5]),
  sim.persv         =mylogit(y[,6])-.5,
  sim.go_bias       =mylogit(y[,7])-.5
)

cor(x,y1)
diag(cor(x,y1))
mean_original = mean(diag(cor(x,y1)))


##### null model --------------------------
source('02_models/01_gong_fitme_null.R')
fitparms<-data.frame()
Nparms<-dim(x)[2]
start_time <- Sys.time()
df=df_null
#optim
y<-
  lapply(1:Nsubj,function(s)
  {
    fit<- optim(par = runif(Nparms,-3,3),
                fn = gong_fitme_null,
                df = df[[s]],
                Ntrls = dim(df[[s]])[1],
                lower = rep(-3,Nparms),
                upper = rep(3,Nparms),
                method = "L-BFGS-B")$par})
#mc.cores =2)
mytime <- Sys.time() - start_time

#transform and copy best fitted params
y<-do.call(rbind,y)
#y<-transform_params(y,c('logit','exp','logit','logit','logit','logit','logit','logit','logit','logit'))
#y[,5:8]<-y[,5:7]-.5

#model parameters
y2<-data.frame(
  sim.alpha1        =mylogit(y[,1]),
  sim.alpha2        =mylogit(y[,2]),
  sim.beta1         =exp(y[,3]),
  sim.lambda        =mylogit(y[,4]),
  sim.persv         =mylogit(y[,5])-.5,
  sim.go_bias       =mylogit(y[,6])-.5
)
#### !!!!! to check!! x_null
cor(x,y2)
diag(cor(x,y2))
mean_null= mean(diag(cor(x,y2)))
 
### model 1 ----------
source('02_models/01_gong_fitme_m1.R')
fitparms<-data.frame()
Nparms<-dim(x)[2]
start_time <- Sys.time()
df=df_model_1
#optim
y<-
  lapply(1:Nsubj,function(s)
  {
    fit<- optim(par = runif(Nparms,-3,3),
                fn = gong_fitme_m1,
                df = df[[s]],
                Ntrls = dim(df[[s]])[1],
                lower = rep(-3,Nparms),
                upper = rep(3,Nparms),
                method = "L-BFGS-B")$par})
#mc.cores =2)
mytime <- Sys.time() - start_time

#transform and copy best fitted params
y<-do.call(rbind,y)
#y<-transform_params(y,c('logit','exp','logit','logit','logit','logit','logit','logit','logit','logit'))
#y[,5:8]<-y[,5:7]-.5

#model parameters
y3<-data.frame(
  sim.alpha1        =mylogit(y[,1]),
  sim.alpha2        =mylogit(y[,2]),
  sim.gamma1        =mylogit(y[,3]),
  sim.beta1         =exp(y[,4]),
  sim.lambda        =mylogit(y[,5]),
  sim.persv         =mylogit(y[,6])-.5,
  sim.go_bias       =mylogit(y[,7])-.5
)

cor(x,y2)
diag(cor(x,y2))
mean_model1= mean(diag(cor(x,y2)))


colnames(comper)<-("null,model_1,original_model")
row.names<-.data.frame(comper, c("1,2,3"))
