rm(list=ls())
load('03_data/02_aggregated_data/wurs.Rdata')
load('03_data/02_aggregated_data/asrs.Rdata')
load('03_data/02_aggregated_data/bis.Rdata')
load('03_data/02_aggregated_data/bisbas.Rdata')
load('03_data/02_aggregated_data/task_clean.Rdata')
load('03_data/02_aggregated_data/open_questions.Rdata')
library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(viridis)

# subclinical adhd score --------------------------------------------------

self.report<-data.frame(subj=asrs$subj,asrs=asrs$asrs6,asrs2=asrs$asrs.partb,wurs=wurs$wurs,bis=bis$bis)
cor(self.report)
self.report$compound<-rowMeans(self.report[,-1]) 
plot(self.report$compound)
hist(self.report$compound)

# bis bas questionnaire --------------------------------------------------
plot(bisbas$BIS_1)
hist(bisbas$BIS_1)
plot(bisbas$BAS_Drive)
hist(bisbas$BAS_Drive)
plot(bisbas$BAS_Fun_Seeking)
hist(bisbas$BAS_Fun_Seeking)
plot(bisbas$BAS_Reward_Responsiveness)
hist(bisbas$BAS_Reward_Responsiveness)

# model-agnostic effects --------------------------------------------------

#reward effect
df_task_reward_effect_bandit<-df_task_clean%>%group_by(subj,reward_oneback)%>%summarise(pStay_badnit1=mean(stay1_bandit),pStay_bandit2=mean(stay2_bandit))
df_task_reward_effect_bandit_group<-df_task_clean%>%group_by(reward_oneback)%>%summarise(pStay_badnit1=mean(stay1_bandit),pStay_bandit2=mean(stay2_bandit))

df_task_reward_resp_bandit<-df_task_clean%>%group_by(reward_oneback,key1_oneback,key2_oneback)%>%summarise(pStay_badnit1=mean(stay1_bandit),pStay_bandit2=mean(stay2_bandit))
plot(df_task_reward_resp_bandit$pStay_badnit1)
plot(df_task_reward_resp_bandit$pStay_bandit2)

df_task_reward_effect_resp<-df_task_clean%>%group_by(reward_oneback,key1_oneback,key2_oneback)%>%summarise(pStay_key1=mean(stay1_key),pStay_key2=mean(stay2_key))
plot(df_task_reward_effect_resp$pStay_key2)

#stay probability for second bandit as function of previous outcome and previous second stage response 
#and the probability to repeat second stage response as a function of previous outcome and previous second stage response 
df_task_reward_resp_bandit_2<-df_task_clean%>%group_by(reward_oneback,key2_oneback)%>%summarise(pStay_bandit2=mean(stay2_bandit),pStay_key2=mean(stay2_key))
plot(df_task_reward_resp_bandit_2$pStay_bandit2)
plot(df_task_reward_resp_bandit_2$pStay_key2)

df_task_reward_resp_bandit_1<-df_task_clean%>%group_by(reward_oneback,key1_oneback)%>%summarise(pStay_bandit1=mean(stay1_bandit),pStay_key1=mean(stay1_key))
plot(df_task_reward_resp_bandit_1$pStay_bandit1)
plot(df_task_reward_resp_bandit_1$pStay_key1)



names(df_task_clean)
#1st stage repetition
#the reward effect for sequences of : Ng-Ng, Ng-Go, Ng-Go, Go-Go
df_task_clean%>%
  filter()%>%
  mutate(reward_oneback=factor(reward_oneback,levels=c(0,1),labels=c('unrewarded','rewarded')))%>%
  group_by(reward_oneback,key1_oneback,key2_oneback)%>%
  summarise(pStay=mean(stay1_bandit))%>%
  pivot_wider(names_from = c('reward_oneback'),values_from=c('pStay'))%>%
  mutate(reward_effect=rewarded-unrewarded)

#2nd stage repetition
#the reward effect for sequences of : Ng-Ng, Ng-Go, Ng-Go, Go-Go
#only for trails that the previous second stage was the same. (meaning, same first stage choice as the previous trail)
df_task_clean%>%
  filter(stay2_state)%>%
  mutate(reward_oneback=factor(reward_oneback,levels=c(0,1),labels=c('unrewarded','rewarded')))%>%
  group_by(reward_oneback,key1_oneback,key2_oneback)%>%
  summarise(pStay=mean(stay2_bandit))%>%
  pivot_wider(names_from = c('reward_oneback'),values_from=c('pStay'))%>%
  mutate(reward_effect=rewarded-unrewarded)

df_task_clean%>%
  filter(stay2_state)%>%
  mutate(reward_oneback=factor(reward_oneback,levels=c(0,1),labels=c('unrewarded','rewarded')))%>%
  group_by(subj,reward_oneback,key1_oneback,key2_oneback)%>%
  summarise(pStay=mean(stay2_bandit))%>%
  pivot_wider(names_from = c('reward_oneback'),values_from=c('pStay'))%>%
  mutate(reward_effect=rewarded-unrewarded)%>%
  ggplot(aes(x=factor(key1_oneback), y=reward_effect, color=factor(key2_oneback))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(size=0.4, position=position_jitter(0.3)) +
  ggtitle("2nd stage repetition") +
  xlab("key1_oneback")+ ylab("reward_effect")


#hierarchical logistic regression model ???
m <- glmer(stay2_bandit ~ 1+reward_oneback+reward_oneback:key1_oneback+reward_oneback:key1_oneback+
             (1+reward_oneback+reward_oneback:key1_oneback+reward_oneback:key1_oneback+ | subj), 
           data = df_task_clean, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 0)
summay(m)



#stay probability after rewarded or unrewarded trails as a factor of the responses at the previos first and second stages.  
#only for trails that the previos second stage was the same. (meaning, same first stage choice as the previos trail)
df_seq <- df_task_clean%>%
  filter(stay2_state)%>%
  group_by(reward_oneback,key1_oneback,key2_oneback)%>%
  summarise(pStay=mean(stay2_bandit))
plot(df_seq$pStay)

#separately for each subject 
df_task_clean%>%
  filter(stay2_state)%>%
  group_by(subj,reward_oneback,key2_oneback)%>%
  summarise(pStay=mean(stay2_bandit))%>%
  ggplot(aes(x=factor(key2_oneback), y=pStay,color=factor(reward_oneback))) + 
  geom_boxplot()

####

#2nd stage repetition
#the reward effect for sequences of : Ng-Ng, Ng-Go, Ng-Go, Go-Go
df_task_clean%>%
  filter(stay2_state)%>%
  mutate(reward_oneback=factor(reward_oneback,levels=c(0,1),labels=c('unrewarded','rewarded')))%>%
  group_by(reward_oneback,key1_oneback,key2_oneback)%>%
  summarise(pStay=mean(stay2_bandit))%>%
  pivot_wider(names_from = c('reward_oneback'),values_from=c('pStay'))%>%
  mutate(reward_effect=rewarded-unrewarded)

#hierarchical logistic regression model 
#interaction between reward effect and response in the first and second stages at the previos trail
m <- glmer(stay2_bandit ~ 1+reward_oneback*key1_oneback*key2_oneback +
             (1+reward_oneback*key1_oneback*key2_oneback | subj), 
           data = df_task_clean, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 0)

summary(m)


x= coef(m)$subj[,'reward_oneback']
y= self.report$compound
fit <- lm(y ~ x)   ## polynomial of degree 3
plot(x, y,xlab = 'reward_oneback',ylab = 'self reports score')  ## scatter plot (colour: black)
x0 <- seq(min(x), max(x), length = 100)  ## prediction grid
y0 <- predict.lm(fit, newdata = list(x = x0))  ## predicted values
lines(x0, y0, col = 2)  ## add regression curve (colour: red)

x= coef(m)$subj[,'reward_oneback:key1_oneback']
y= self.report$compound
fit <- lm(y ~ x)   ## polynomial of degree 3
plot(x, y,xlab = 'reward_oneback:key1_oneback',ylab = 'self reports score')  ## scatter plot (colour: black)
x0 <- seq(min(x), max(x), length = 100)  ## prediction grid
y0 <- predict.lm(fit, newdata = list(x = x0))  ## predicted values
lines(x0, y0, col = 3)  ## add regression curve (colour: green)

x= coef(m)$subj[,'reward_oneback:key2_oneback']
y= self.report$compound
fit <- lm(y ~ x)   ## polynomial of degree 3
plot(x, y,xlab = 'reward_oneback:key2_oneback',ylab = 'self reports score')  ## scatter plot (colour: black)
x0 <- seq(min(x), max(x), length = 100)  ## prediction grid
y0 <- predict.lm(fit, newdata = list(x = x0))  ## predicted values
lines(x0, y0, col = 5)  ## add regression curve (colour: blue)


x= coef(m)$subj[,'reward_oneback:key1_oneback:key2_oneback']
y= self.report$compound
fit <- lm(y ~ x)   ## polynomial of degree 3
plot(x, y,xlab = 'reward_oneback:key1_oneback:key2_oneback',ylab = 'self reports score')  ## scatter plot (colour: black)
x0 <- seq(min(x), max(x), length = 100)  ## prediction grid
y0 <- predict.lm(fit, newdata = list(x = x0))  ## predicted values
lines(x0, y0, col = 6)  ## add regression curve (colour: purple)


####
df_1st_choice <- df_task_clean%>%
  group_by(reward_oneback,key1_oneback,key2_oneback)%>%
  summarise(pStay=mean(stay1_bandit))%>%
  ggplot(aes(x=factor(key2_oneback), y=pStay,color=factor(reward_oneback))) + 
  geom_boxplot()

plot(df_1st_choice$pStay)


#check corr with subclinical adhd
effects<-
  df_task_clean%>%
  filter(stay2_state)%>%
  group_by(subj,reward_oneback,key2_oneback)%>%
  summarise(pStay=mean(stay2_bandit))%>%
  pivot_wider(names_from = c('reward_oneback','key2_oneback'),values_from='pStay')

plot(self.report$compound,unlist(c(effects[,4]-effects[,2])))
plot(self.report$compound,unlist(c(effects[,5]-effects[,3])))

#check corr with bisbas score




# effects of reward on key-stay -------------------------------------------

#KEY2
#how likely it is to repeat the second stage response as a function of : the orevios first and second stage action and whether the trail was rewarded or not 
h<-df_task_clean%>%
  filter(stay2_state==F)%>%
  mutate(reward_oneback=factor(reward_oneback,levels=c(0,1),labels=c('unrewarded','rewarded')))%>%
  group_by(reward_oneback,key1_oneback,key2_oneback,key1)%>%
  summarise(pStay=mean(stay2_key))%>%
  pivot_wider(names_from = c('reward_oneback'),values_from=c('pStay'))%>%
  mutate(reward_effect=rewarded-unrewarded)
plot(h$reward_effect)

#hierarchical logistic regression model (NHT)
library(lme4)
m <- glmer(stay2_key ~ reward_oneback*key2_oneback*key1_oneback+(reward_oneback| subj), data = df_task_clean, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
summary(m)

#bayesian hierarchical logistic regression 
library(brms)
#m<-brm(stay2_key ~ reward_oneback*key2_oneback*key1_oneback + (reward_oneback*key2_oneback*key1_oneback|subj), data = df, family = bernoulli(link = "logit"),cores=20)
load('02_models/brms_stay2_key.Rdata')
summary(m)
plot(m, pars = c("reward_oneback")) 
cor.test(self.report$compound,ranef(m)$subj[,,'reward_oneback'][,1])



# pStay bandit as a function of mapping -----------------------------------

names(df_task_clean)
df_task_clean%>%
  filter(stay2_state==T)%>%
  mutate(reward_oneback=factor(reward_oneback,levels=c(0,1),labels=c('unrewarded','rewarded')))%>%
  group_by(stay2_mapping,reward_oneback,key1_oneback,key2_oneback)%>%
  summarise(pStay=mean(stay2_bandit))%>%
  pivot_wider(names_from = c('reward_oneback'),values_from=c('pStay'))%>%
  mutate(reward_effect=rewarded-unrewarded)

library(lme4)
m <- glmer(stay2_bandit ~ reward_oneback*stay2_mapping+(reward_oneback| subj), data = df_task_clean[df_task_clean$stay2_state==T,], family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
summary(m)

##########
names(df_task_clean)
df_task_clean%>%
  filter(stay2_state==F)%>%
  filter(stay1_key==1)%>%
  mutate(reward_oneback=factor(reward_oneback,levels=c(0,1),labels=c('unrewarded','rewarded')))%>%
  group_by(reward_oneback,key1_oneback,key2_oneback)%>%
  summarise(pStay=mean(stay2_key))%>%
  pivot_wider(names_from = c('reward_oneback'),values_from=c('pStay'))%>%
  mutate(reward_effect=rewarded-unrewarded)

library(lme4)
m <- glmer(stay2_bandit ~ reward_oneback*stay2_mapping+(reward_oneback| subj), data = df_task_clean[df_task_clean$stay2_state==T,], family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
summary(m)
