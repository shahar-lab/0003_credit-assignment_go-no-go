rm(list=ls())
load('03_data/02_aggregated_data/wurs.Rdata')
load('03_data/02_aggregated_data/asrs.Rdata')
load('03_data/02_aggregated_data/bis.Rdata')
load('03_data/02_aggregated_data/bisbas.Rdata')
load('03_data/02_aggregated_data/task_clean.Rdata')
load('03_data/02_aggregated_data/open_questions.Rdata')
load('03_data/02_aggregated_data/basic_info.Rdata')

library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(ggpubr)
#library(brms)
library(psych)
library(nFactors)
library(corrplot)

# subclinical adhd score --------------------------------------------------
df= data.frame(asrs.partA=asrs[,2],
               asrs.partB=asrs[,3],
               bis.attention=bis[,2],
               bis.motor=bis[,3],
               bis.nonplan=bis[,4],
               wurs=wurs[,2])

colnames(df)
df <-as.data.frame(scale(df))
summary(df)
l<-round(cor(df),1)
corrplot(l, method="circle",tl.cex=0.4)

#PCA
scree(df,factors=TRUE,pc=TRUE,main="Scree plot",hline=NULL,add=FALSE) 
parallel<-fa.parallel(df, fm="ml", fa="pc", main = "Parallel Analysis Scree Plots",
                      n.iter=20,error.bars=T,ylabel=NULL,show.legend=TRUE,sim=TRUE)
m<-principal(df,nfactors = 1,rotate = 'promax',scores=TRUE)
print(m$loadings,cutoff = 0.5)

adhd <-data.frame(factor1=m$scores[,1])

self.report<-data.frame(subj=asrs$subj,adhd)
#self.report$compound<-rowMeans(self.report[,-1]) 
plot(self.report$factor1)
hist(self.report$factor1, 
     main='ADHD subclinical total score',
     xlab="ADHD subclinical total score")

#self.report$factor1<-scale(self.report$factor1)
for (ind in unique(df_task_clean$subj)) {
  df_task_clean[df_task_clean$subj == ind, "adhd_compound"] = self.report$factor1[self.report$subj == ind]
}

for (ind in unique(df_task_clean$subj)) {
  df_task_clean[df_task_clean$subj == ind, "adhd_group_score"] = (self.report$factor1[self.report$subj == ind]>median(self.report$factor1))*1
}

save(df_task_clean,file='03_data/02_aggregated_data/complete_task.Rdata')

# first-stage choice rep as a function of first stage action -------------
plotdf<-
df_task_clean%>%
  filter(stay1_mapping==1 & key2_oneback==1)%>%
  mutate(reward_oneback=factor(reward_oneback,levels=c(0,1),labels=c('unrewarded','rewarded')))%>%
  group_by(subj,reward_oneback)%>%
  summarise(pStay=mean(stay1_bandit))%>%
  pivot_wider(names_from = c('reward_oneback'),values_from=c('pStay'))%>%
  mutate(reward_effect=rewarded-unrewarded)



#plot
df<- data_1x1( array_1 = plotdf$unrewarded,
               array_2 = plotdf$rewarded,
               jit_distance = .09,
               jit_seed = 321)

library(raincloudplots)
raincloud_1 <- raincloud_1x1_repmes(data = df,colors = (c('dodgerblue', 'darkorange')), fills = (c('dodgerblue', 'darkorange')),line_color = 'gray',
                                    line_alpha = .3,size = 1,  alpha = .6,  align_clouds = FALSE) +  
  scale_x_continuous(breaks=c(1,2), labels=c("unrewarded", "rewarded"), limits=c(0, 3)) +
  xlab("previous-outcome") +  ylab("P(stay_fisherman)") + theme_classic()

raincloud_1

ggsave('03_data/03_figuers/fig2_go.png', raincloud_1,width = 6, height = 4)


theme_set(theme_pubclean())
p<-ggplot(aes(y = pStay, x = reward_oneback, color = factor(key1_oneback)), data = plotdf) 
p+geom_boxplot(width = 0.5, size = 0.4) +
  geom_dotplot(
    aes(fill = factor(key1_oneback)), trim = FALSE,
    binaxis='y', stackdir='center'
  )+
  scale_fill_manual(values = c("#00AFBB", "#bb0c00"))
# Change the position : interval between dot plot of the same group
p + geom_boxplot(
  aes(color = factor(key1_oneback)), width = 0.5, size = 0.8,
  position = position_dodge(0.55)
) +
  geom_dotplot(
    aes(fill = factor(key1_oneback), color = factor(key1_oneback)), trim = FALSE,
    binaxis='y', stackdir='center', dotsize = 0.25,
    position = position_dodge(0.55)
  )+
  scale_fill_manual(values = c("#00AFBB", "#bb0c00"))+
  scale_color_manual(values = c("#00AFBB", "#bb0c00"))+
  xlab("Previous outcome")+ ylab("Stay probability 1st bandit")


m2 <- glmer(stay1_bandit ~ reward_oneback*key1_oneback + (1 | subj), 
           data = df_task_clean%>% filter(stay1_mapping==1 & key2_oneback==1), 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)

summary(m2)
#reward_oneback:key1_oneback  -0.4823     0.1638  -2.945 0.003231 ** 


#regression
m <- glmer(stay1_bandit ~ reward_oneback*key1_oneback*adhd_compound + (1 | subj), 
           data = df_task_clean%>% filter(stay1_mapping==1 & key2_oneback==1), 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)

summary(m)

library(effects)
plot(effect('reward_oneback:key1_oneback',m,xlevels=2))

#brms
m                  <- brm(formula=stay1_bandit ~ reward_oneback*key1_oneback*key2_oneback + (1+reward_oneback | subj),  
                          data=df_task_clean, 
                          family = bernoulli(link = "logit"),
                          cores=2)

save(m,file='02_models/m.Rdata')

summary(m)
marginal_effects(m)
stanplot(m, pars = "^b", type = "areas")
plot(m)

#basic info correlation ----------------------------------------------
df_basic_info$df_questions.gender<-scale(df_basic_info$df_questions.gender)
df_basic_info$df_questions.age<-scale(df_basic_info$df_questions.age)

for (ind in unique(df_task_clean$subj)) {
  df_task_clean[df_task_clean$subj == ind, "gender"] = df_basic_info$df_questions.gender[df_basic_info$subj == ind]
  df_task_clean[df_task_clean$subj == ind, "age"] = df_basic_info$df_questions.age[df_basic_info$subj == ind]
  }


df_total<-df_task_clean%>%
  filter(stay1_mapping==1 & key2_oneback==1)%>%
  mutate(reward_oneback=factor(reward_oneback,levels=c(0,1),labels=c('unrewarded','rewarded')))%>%
  group_by(subj,reward_oneback)%>%
  summarise(pStay=mean(stay1_bandit))%>%
  pivot_wider(names_from = c('reward_oneback'),values_from=c('pStay'))%>%
  mutate(reward_effect=rewarded-unrewarded)

df_total$gender<-df_basic_info$df_questions.gender
df_total$age<-df_basic_info$df_questions.age 

#regression
m <- glmer(reward_effect ~ gender + (1 | subj), 
           data = df_total, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)

summary(m)



#האם הטייס האוטומטי מתרחש? השוואה בין האם המקש השני הקודם היה GO או NG 
#regression
m <- glmer(stay1_bandit ~ reward_oneback*key1_oneback*adhd_compound + (1 | subj), 
           data = df_task_clean%>% filter(key2_oneback==0), 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)

summary(m)

#reward effect -------------------------------------------
df_task_reward_effect_bandit<-df_task_clean%>%group_by(subj,reward_oneback)%>%summarise(pStay_badnit1=mean(stay1_bandit),pStay_bandit2=mean(stay2_bandit))
df_task_reward_effect_bandit_group<-df_task_clean%>%group_by(reward_oneback)%>%summarise(pStay_badnit1=mean(stay1_bandit),pStay_bandit2=mean(stay2_bandit))

df_task_reward_effect_key<-df_task_clean%>%group_by(reward_oneback,key1_oneback,key2_oneback)%>%summarise(pStay_key1=mean(stay1_key),pStay_key2=mean(stay2_key))
df_task_reward_effect_key<-df_task_clean%>%group_by(reward_oneback)%>%summarise(pStay_key1=mean(stay1_key),pStay_key2=mean(stay2_key))
df_task_reward_effect_key<-df_task_clean%>%group_by(reward_oneback,key1_oneback)%>%summarise(pStay_key1=mean(stay1_key),pStay_key2=mean(stay2_key))



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


# final1204 ---------------------------------------------------------------
#1st stage stay probability 
m2_2_uf <- glmer(stay1_bandit ~ reward_oneback*key1_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean, 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 0)

summary(m2_2_uf)
#reward_oneback:key2_oneback                             0.24864    0.11388   2.183  0.02900 *  
#reward_oneback:adhd_compound                            0.15930    0.08178   1.948  0.05142 .  
#reward_oneback:key2_oneback:adhd_compound               0.25113    0.11604   2.164  0.03045 *  
#reward_oneback:key1_oneback:key2_oneback:adhd_compound -0.25996    0.15736  -1.652  0.09854 .  


###optional 
m2_2_uf <- glmer(stay1_bandit ~ reward_oneback*key1_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean%>% filter(stay1_mapping==1), 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 0)

summary(m2_2_uf)
#reward_oneback:key1_oneback                            -0.38357    0.16692  -2.298  0.02157 *  
#reward_oneback:key2_oneback                             0.30894    0.16945   1.823  0.06828 .  
#reward_oneback:adhd_compound                            0.21518    0.12243   1.758  0.07881 .  


#2nd stage stay probability 
m2_2_uf <- glmer(stay2_bandit ~ reward_oneback*key1_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean, 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 0)

summary(m2_2_uf)
#key2_oneback:adhd_compound                             -0.20870    0.09313  -2.241  0.02503 *  
#reward_oneback:key1_oneback:adhd_compound               0.36103    0.11885   3.038  0.00238 ** 
#reward_oneback:key2_oneback:adhd_compound               0.36380    0.11958   3.042  0.00235 ** 
#reward_oneback:key1_oneback:key2_oneback:adhd_compound -0.60246    0.16703  -3.607  0.00031 ***


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
             (1 | subj), 
           data = df_task_clean, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 0)

summary(m)

m2_2_uf <- glmer(stay2_bandit ~ reward_oneback*key1_oneback*key2_oneback + (1 | subj), 
                 data = df_task_clean, 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 0)

summary(m2_2_uf)

m2_2_uf <- glmer(stay2_bandit ~ reward_oneback*key1_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean, 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 0)

summary(m2_2_uf)
#reward_oneback:key1_oneback:adhd_compound               0.39701    0.11910   3.333 0.000858 ***
#reward_oneback:key2_oneback:adhd_compound               0.42898    0.12031   3.566 0.000363 ***
# key1_oneback:key2_oneback:adhd_compound                 0.31114    0.13563   2.294 0.021794 *  
#  reward_oneback:key1_oneback:key2_oneback:adhd_compound -0.74869    0.16876  -4.436 9.15e-06 ***



m2_2_uf <- glmer(stay2_bandit ~ reward_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean, 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 0)

summary(m2_2_uf)

m2_2_uf <- glmer(stay2_bandit ~ reward_oneback*key1_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean%>% filter(stay1_mapping==1), 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)

summary(m2_2_uf)
#reward_oneback:key2_oneback                             0.335236   0.166292   2.016 0.043805 *  
#  reward_oneback:key1_oneback:key2_oneback               -0.464983   0.229528  -2.026 0.042783 *  
# reward_oneback:key1_oneback:adhd_compound               0.462479   0.168769   2.740 0.006138 ** 
#  reward_oneback:key2_oneback:adhd_compound               0.542789   0.173053   3.137 0.001710 ** 
#  reward_oneback:key1_oneback:key2_oneback:adhd_compound -0.900961   0.239989  -3.754 0.000174 ***

m2_2_uf <- glmer(stay2_bandit ~ reward_oneback*key1_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean%>% filter(stay1_mapping==1 & key1==1), 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)

summary(m2_2_uf)
#

m2_2_uf <- glmer(stay2_bandit ~ reward_oneback*key1_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean%>% filter(stay1_mapping==1 & key1==0), 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)

summary(m2_2_uf)
#reward_oneback:key2_oneback:adhd_compound               0.44087    0.20380   2.163   0.0305 *  

m2_2_uf <- glmer(stay2_bandit ~ reward_oneback*key1_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean%>% filter(key1==1), 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)

summary(m2_2_uf)
#reward_oneback:key2_oneback:adhd_compound               0.41069    0.17609   2.332   0.0197 *  
#reward_oneback:key1_oneback:key2_oneback:adhd_compound -0.56880    0.23781  -2.392   0.0168 *  

m2_2_uf <- glmer(stay2_bandit ~ reward_oneback*key1_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean%>% filter(key1==0), 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)

summary(m2_2_uf)
# מלא מובהקים 
#reward_oneback:key2_oneback                             0.31323    0.16241   1.929  0.05377 .  

m2_2_uf <- glmer(stay2_bandit ~ reward_oneback*key1_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean%>% filter(stay2_mapping==1 & key1==0), 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)

summary(m2_2_uf)
#

m2_2_uf <- glmer(stay2_bandit ~ reward_oneback*key1_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean%>% filter(stay2_mapping==1 & key1==1), 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)

summary(m2_2_uf)


###-------------------------
m2_2_uf <- glmer(stay2_bandit ~ 1+reward_oneback*key2_oneback +
             (1+reward_oneback*key2_oneback | subj), 
           data = df_task_clean, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)

summary(m2_2_uf)

m2_2_uf <- glmer(stay2_bandit ~ reward_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean, 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)

summary(m2_2_uf)

m2_2_f <- glmer(stay2_bandit ~ reward_oneback*key2_oneback*adhd_compound + (1 | subj), 
           data = df_task_clean%>% filter(stay1_mapping==1 & key1_oneback==1), 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)

summary(m2_2_f)


m2_2_f1 <- glmer(stay2_bandit ~ reward_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean%>% filter(stay1_mapping==1 & key1_oneback==0), 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)

summary(m2_2_f1)
#reward_oneback:key2_oneback                0.361673   0.167921   2.154  0.03125 *  
#key2_oneback:adhd_compound                -0.340685   0.139461  -2.443  0.01457 *  
#reward_oneback:key2_oneback:adhd_compound  0.565858   0.174073   3.251  0.00115 ** 

m2_2_f1 <- glmer(stay2_bandit ~ reward_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean%>% filter(stay2_mapping==1 & key1_oneback==1), 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)

summary(m2_2_f1)
#reward_oneback:key2_oneback:adhd_compound -0.43470    0.21914  -1.984 0.047294 *  


m2_2_f1 <- glmer(stay2_bandit ~ reward_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean%>% filter(stay2_mapping==1), 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)

summary(m2_2_f1)

m2_2_f1 <- glmer(stay2_bandit ~ reward_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean%>% filter(key1_oneback==0), 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)

summary(m2_2_f1)
#reward_oneback:key2_oneback:adhd_compound  0.43169    0.12125   3.560  0.00037 ***

m2_2_f1 <- glmer(stay2_bandit ~ reward_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean%>% filter(key1_oneback==1), 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)

summary(m2_2_f1)
#reward_oneback:adhd_compound               0.25056    0.08663   2.892  0.00382 ** 
#reward_oneback:key2_oneback:adhd_compound -0.32358    0.11799  -2.742  0.00610 ** 

m2_2_f1 <- glmer(stay2_bandit ~ reward_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean%>% filter(stay2_mapping==1 & key1_oneback==0), 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)

summary(m2_2_f1)
#reward_oneback:adhd_compound              -0.34356    0.15205  -2.260   0.0238 *  

m2_2_f1 <- glmer(stay2_bandit ~ reward_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean%>% filter(key1==0), 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)

summary(m2_2_f1)
#reward_oneback:key2_oneback                0.29728    0.11688   2.543    0.011 *  

m2_2_f1 <- glmer(stay2_bandit ~ reward_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean%>% filter(key1==1), 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)

summary(m2_2_f1)
#

m2_2_f1 <- glmer(stay2_bandit ~ reward_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean%>% filter(stay2_mapping==1 & key1==0), 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)

summary(m2_2_f1)
#reward_oneback:key2_oneback                0.52124    0.22144   2.354   0.0186 *  

m2_2_f1 <- glmer(stay2_bandit ~ reward_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean%>% filter(stay2_mapping==1 & key1==1), 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)

summary(m2_2_f1)
#

m2_2_f1 <- glmer(stay2_bandit ~ reward_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean%>% filter(stay1_mapping==1 & key1==1), 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)

summary(m2_2_f1)
#reward_oneback:adhd_compound               0.25764    0.12555   2.052   0.0402 *  

m2_2_f1 <- glmer(stay2_bandit ~ reward_oneback*key2_oneback*adhd_compound + (1 | subj), 
                 data = df_task_clean%>% filter(stay1_mapping==1 & key1==0), 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)

summary(m2_2_f1)
#reward_oneback:key2_oneback                0.31655    0.16582   1.909  0.05627 .  


###---------- stay2 bandit - key 1 
m2_2_f1 <- glmer(stay2_bandit ~ reward_oneback*key1*adhd_compound + (1 | subj), 
                 data = df_task_clean%>% filter(stay1_mapping==1 & key1==0), 
                 family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)

summary(m2_2_f1)




######From here on not relevant for the Poster######


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


