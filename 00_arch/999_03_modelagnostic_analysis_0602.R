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
library(ggpubr)

# subclinical adhd score --------------------------------------------------

self.report<-data.frame(subj=asrs$subj,asrs=asrs$asrs6,asrs2=asrs$asrs.partb,wurs=wurs$wurs,bis=bis$bis)
cor(self.report)
self.report$compound<-rowMeans(self.report[,-1]) 
plot(self.report$compound)
hist(self.report$compound)

self.report$compound<-scale(self.report$compound)
for (ind in unique(df_task_clean$subj)) {
  df_task_clean[df_task_clean$subj == ind, "adhd_compound"] = self.report$compound[self.report$subj == ind]
}

for (ind in unique(df_task_clean$subj)) {
  df_task_clean[df_task_clean$subj == ind, "adhd_group_score"] = (self.report$compound[self.report$subj == ind]>median(self.report$compound))*1
}


# bis bas questionnaire --------------------------------------------------
plot(bisbas$BIS_1)
hist(bisbas$BIS_1)
plot(bisbas$BAS_Drive)
hist(bisbas$BAS_Drive)
plot(bisbas$BAS_Fun_Seeking)
hist(bisbas$BAS_Fun_Seeking)
plot(bisbas$BAS_Reward_Responsiveness)
hist(bisbas$BAS_Reward_Responsiveness)


# first-stage choice rep as a function of first stage action -------------

df_task_clean%>%
  filter(stay1_mapping==1 & key2_oneback==1 )%>%
  mutate(reward_oneback=factor(reward_oneback,levels=c(0,1),labels=c('unrewarded','rewarded')))%>%
  group_by(reward_oneback,key1_oneback)%>%
  summarise(pStay=mean(stay1_bandit))%>%
  pivot_wider(names_from = c('reward_oneback'),values_from=c('pStay'))%>%
  mutate(reward_effect=rewarded-unrewarded)

m <- glmer(stay1_bandit ~ reward_oneback*key1_oneback*adhd_compound + (1 | subj), 
           data = df_task_clean%>% filter(stay1_mapping==1 & key2_oneback==1), 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)

summary(m)

library(effects)
plot(effect('reward_oneback:key1_oneback',m,xlevels=2))






#reward effect -------------------------------------------
df_task_reward_effect_bandit<-df_task_clean%>%group_by(subj,reward_oneback)%>%summarise(pStay_badnit1=mean(stay1_bandit),pStay_bandit2=mean(stay2_bandit))
df_task_reward_effect_bandit_group<-df_task_clean%>%group_by(reward_oneback)%>%summarise(pStay_badnit1=mean(stay1_bandit),pStay_bandit2=mean(stay2_bandit))

df_task_reward_effect_key_1<-df_task_clean%>%group_by(reward_oneback,key1_oneback)%>%summarise(pStay_key1=mean(stay1_key))
df_task_reward_effect_key_2<-df_task_clean%>%group_by(reward_oneback,key2_oneback)%>%summarise(pStay_key2=mean(stay2_key))

df_task_reward_effect_bandit_group<-df_task_clean%>%group_by(reward_oneback)%>%summarise(pStay_badnit1=mean(stay1_bandit),pStay_bandit2=mean(stay2_bandit))


# effects of reward on 1st stage bandit-stay -------------------------------------------

names(df_task_clean)
df_task_clean%>%
  filter()%>%
  mutate(reward_oneback=factor(reward_oneback,levels=c(0,1),labels=c('unrewarded','rewarded')))%>%
  group_by(reward_oneback,key1_oneback,key2_oneback)%>%
  summarise(pStay=mean(stay1_bandit))%>%
  pivot_wider(names_from = c('reward_oneback'),values_from=c('pStay'))%>%
  mutate(reward_effect=rewarded-unrewarded)

m <- glmer(stay1_bandit ~ 1+reward_oneback*key1_oneback*key2_oneback +
             (1+reward_oneback*key1_oneback*key2_oneback | subj), 
           data = df_task_clean, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 0)

summary(m)


df_task_clean%>%
  filter()%>%
  mutate(reward_oneback=factor(reward_oneback,levels=c(0,1),labels=c('unrewarded','rewarded')))%>%
  group_by(reward_oneback,key2_oneback)%>%
  summarise(pStay=mean(stay1_bandit))%>%
  pivot_wider(names_from = c('reward_oneback'),values_from=c('pStay'))%>%
  mutate(reward_effect=rewarded-unrewarded)

n <- glmer(stay2_bandit ~ 1+reward_oneback*key2_oneback +
             (1+reward_oneback*key2_oneback | subj), 
           data = df_task_clean, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 0)

summary(n)

# effects of reward on 2nd stage bandit-stay -------------------------------------------
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
           nAGQ = 1)

summary(m)

df_task_clean%>%
  filter(stay2_state)%>%
  mutate(reward_oneback=factor(reward_oneback,levels=c(0,1),labels=c('unrewarded','rewarded')))%>%
  group_by(reward_oneback,key2_oneback,key1)%>%
  summarise(pStay=mean(stay2_bandit))%>%
  pivot_wider(names_from = c('reward_oneback'),values_from=c('pStay'))%>%
  mutate(reward_effect=rewarded-unrewarded)

n <- glmer(stay2_bandit ~ 1+reward_oneback*key2_oneback*key1 +
             (1+reward_oneback*key2_oneback*key1 | subj), 
           data = df_task_clean, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 0)

summary(n)
# effects of reward on key-stay -------------------------------------------

#KEY1
#how likely it is to repeat the first stage response as a function of : the orevios first and second stage action and whether the trail was rewarded or not 
df_key1<-df_task_clean%>%
  filter()%>%
  mutate(reward_oneback=factor(reward_oneback,levels=c(0,1),labels=c('unrewarded','rewarded')))%>%
  group_by(subj,reward_oneback,key1_oneback,key2_oneback)%>%
  summarise(pStay=mean(stay1_key))%>%
  pivot_wider(names_from = c('reward_oneback'),values_from=c('pStay'))%>%
  mutate(reward_effect=rewarded-unrewarded)%>%
  filter(-0.26<reward_effect & reward_effect<0.4)
#sort(df_key1$reward_effect)

m <- glmer(stay1_key ~ reward_oneback*key1_oneback*key2_oneback+(reward_oneback*key1_oneback*key2_oneback| subj), data = df_task_clean, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
summary(m)


#plot
theme_set(theme_pubclean())
p<-ggplot(aes(y = reward_effect, x = factor(key1_oneback), color = factor(key2_oneback)), data = df_key1) 
p+geom_boxplot(width = 0.5, size = 0.4) +
  geom_dotplot(
    aes(fill = factor(key2_oneback)), trim = FALSE,
    binaxis='y', stackdir='center'
  )+
  scale_fill_manual(values = c("#00AFBB", "#bb0c00"))
# Change the position : interval between dot plot of the same group
p + geom_boxplot(
  aes(color = factor(key2_oneback)), width = 0.5, size = 0.8,
  position = position_dodge(0.55)
) +
  geom_dotplot(
    aes(fill = factor(key2_oneback), color = factor(key2_oneback)), trim = FALSE,
    binaxis='y', stackdir='center', dotsize = 0.25,
    position = position_dodge(0.55)
  )+
  scale_fill_manual(values = c("#00AFBB", "#bb0c00"))+
  scale_color_manual(values = c("#00AFBB", "#bb0c00"))+
  ggtitle("First response repetition") +
  xlab("Previous keys sequence")+ ylab("Reward effect")

#KEY2
#how likely it is to repeat the second stage response as a function of : the orevios first and second stage action and whether the trail was rewarded or not 
df_key2<-df_task_clean%>%
  filter()%>%
  mutate(reward_oneback=factor(reward_oneback,levels=c(0,1),labels=c('unrewarded','rewarded')))%>%
  group_by(subj,reward_oneback,key1_oneback,key2_oneback)%>%
  summarise(pStay=mean(stay2_key))%>%
  pivot_wider(names_from = c('reward_oneback'),values_from=c('pStay'))%>%
  mutate(reward_effect=rewarded-unrewarded)

#hierarchical logistic regression model (NHT)
library(lme4)
l <- glmer(stay2_key ~ reward_oneback*key1_oneback*key2_oneback+(reward_oneback*key1_oneback*key2_oneback| subj), data = df_task_clean, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
summary(l)

#bayesian hierarchical logistic regression 
library(brms)
#m<-brm(stay2_key ~ reward_oneback*key2_oneback*key1_oneback + (reward_oneback*key2_oneback*key1_oneback|subj), data = df, family = bernoulli(link = "logit"),cores=20)
load('02_models/brms_stay2_key.Rdata')
summary(m)
plot(m, pars = c("reward_oneback")) 
cor.test(self.report$compound,ranef(m)$subj[,,'reward_oneback'][,1])

#plot 
theme_set(theme_pubclean())
p<-ggplot(aes(y = reward_effect, x = factor(key1_oneback), color = factor(key2_oneback)), data = df_key2) 
p+geom_boxplot(width = 0.5, size = 0.4) +
  geom_dotplot(
    aes(fill = factor(key2_oneback)), trim = FALSE,
    binaxis='y', stackdir='center'
  )+
  scale_fill_manual(values = c("#00AFBB", "#bb0c00"))
# Change the position : interval between dot plot of the same group
p + geom_boxplot(
  aes(color = factor(key2_oneback)), width = 0.5, size = 0.8,
  position = position_dodge(0.55)
) +
  geom_dotplot(
    aes(fill = factor(key2_oneback), color = factor(key2_oneback)), trim = FALSE,
    binaxis='y', stackdir='center', dotsize = 0.25,
    position = position_dodge(0.55)
  )+
  scale_fill_manual(values = c("#00AFBB", "#bb0c00"))+
  scale_color_manual(values = c("#00AFBB", "#bb0c00"))+
  ggtitle("First response repetition") +
  xlab("Previous keys sequence")+ ylab("Reward effect")
##############

#plot
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

#correlation of coefficients and self reports
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

#repeat the same bandit sequence 
df_task_clean%>%
  filter()%>%
  mutate(reward_oneback=factor(reward_oneback,levels=c(0,1),labels=c('unrewarded','rewarded')))%>%
  group_by(reward_oneback,key1_oneback,key2_oneback)%>%
  summarise(pStay=mean(stay2_bandit))%>%
  pivot_wider(names_from = c('reward_oneback'),values_from=c('pStay'))%>%
  mutate(reward_effect=rewarded-unrewarded)


####

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


