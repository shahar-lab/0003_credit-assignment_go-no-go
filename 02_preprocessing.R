rm(list=ls())

# Two-Step Task -----------------------------------------------------------
library(dplyr)
library(magrittr)
load('03_data/02_aggregated_data/task_raw.Rdata')


df_task%<>%
  mutate(mapping1=as.numeric(stim_3),rt1=rt,key1=(key!=c(-1))*1,ch1=(stim_selected_1%in%c(0,3))*1,
         rt2=rt_2nd,key2=(key_2nd!=c(-1))*1,sec_stg_banditA=stim_order_1_2nd,sec_stg_banditB=stim_order_1_2nd,
         sec_stg_state=stage_2nd,ch2=stim_selected_2nd,reward=unlist(outcome))%>%
  select(subj,trial_num,block,rt1,key1,ch1,mapping1,
         rt2,key2,sec_stg_banditA,sec_stg_banditB,sec_stg_state,ch2,reward)

df_task%<>%filter(block == 'test')%>%filter(trial_num != '0')%>%filter(trial_num != '84')%>%filter(trial_num != '168')

#add columns
df_task%<>%
  mutate(reward_oneback=lag(reward),
         key1_oneback =lag(key1),
         key2_oneback =lag(key2),
         stay1_bandit =(ch1==lag(ch1))*1,stay2_bandit=(ch2==lag(ch2))*1,
         stay1_key    =(key1==lag(key1))*1,stay2_key =(key2==lag(key2))*1,
         stay1_mapping=((mapping1==lag(mapping1))*1),stay2_mapping=((sec_stg_banditA==lag(sec_stg_banditA))*1),
         stay2_state =(sec_stg_state==lag(sec_stg_state)*1))%>%
  na.omit()

#visual check rt distrbution
hist(df_task$rt1[df_task$rt1>0],col='skyblue',border=F,main="Histogram for first-stage Reaction Time",xlab="Reaction Time", ylab="Frequency")
hist(df_task$rt2[df_task$rt2>0],xlim=c(0,1),add=T,col='#ff1a1a60',border=F)
legend("topright", c("first-stage", "second-stage"), col=c("skyblue", "#ff1a1a60"), lwd=10)
mean(df_task$rt1[df_task$rt1>0])
sd(df_task$rt1[df_task$rt1>0])
mean(df_task$rt2[df_task$rt2>0])
sd(df_task$rt2[df_task$rt2>0])

#percent of go
df_task_go<-df_task%>%group_by(subj)%>%summarise(pGo1=mean(key1),pGo2=mean(key2))
colMeans(df_task_go)

#counting the number of go trails 
#we might have a problem with a few subject thst has a low go rate  
c<-data.frame(table(df$subj[df$key1==1]),table(df$subj[df$key2==1]),table(df$subj[df$key1==1 & df$key2==1]))


sd(df_task_go$pGo1)
sd(df_task_go$pGo2)
hist(df_task_go$pGo1,xlim=c(0,1),col='skyblue',border=F,main="Histogram for 'go' responses",xlab="perecent of 'go' steps ", ylab="Frequency")
hist(df_task_go$pGo2,xlim=c(0,1),add=T,col='#ff1a1a60',border=F)
legend("topright", c("first-stage", "second-stage"), col=c("skyblue", "#ff1a1a60"), lwd=10)

sum(df_task$key1)/sum(df_task$key2) #go response was used 0.04 times more in the first stage than the 2nd one. 


#checking is rt of Go steps < 200ms 
df_task_rt1<-df_task%>%group_by(subj)%>%filter(key1==1)%>%summarise(rt1_good=mean((rt1>200)*1))
df_task_rt2<-df_task%>%group_by(subj)%>%filter(key2==1)%>%summarise(rt2_good=mean((rt2>200)*1))
df_task_rt_good = data.frame(
  subj = df_task_rt1$subj,
  good = (df_task_rt1$rt1_good>0.9 | df_task_rt2$rt2_good>0.9)*1
)
sum(df_task_rt_good$good)/length(df_task_rt_good$subj)==1

df_task_clean <- df_task%>%filter(key1==0 | key1==1 & rt1>200)%>%filter(key2==0 |key2==1 & rt2>200)

length(df_task_clean$subj)/length(df_task$subj)#~1% of the overall trails has been neglected due to ireasonable RT 
  
average_rt1<-df_task_clean%>%group_by(subj)%>%filter(key1==1)
mean(average_rt1$rt1)
sd(average_rt1$rt1)
average_rt2<-df_task_clean%>%group_by(subj)%>%filter(key2==1)
mean(average_rt2$rt2)
sd(average_rt2$rt2)

save(df_task_clean,file='03_data/02_aggregated_data/task_clean.Rdata')

#####BIS-------------------------------------------------------------------
load('03_data/02_aggregated_data/bis_raw.Rdata')

bis$bis<-apply(bis[,1:30],1,sum)
bis%<>%
  mutate(attention  =rowMeans(cbind(item_6, item_5, item_9, item_11, item_20, item_24, item_26, item_28)),
         motor      =rowMeans(cbind(item_2, item_3, item_4, item_16, item_17, item_19, item_21, item_22, item_23, item_25, item_30)),
         nonplanning=rowMeans(cbind(item_1, item_7, item_8, item_10, item_12, item_13, item_14, item_15, item_18, item_27, item_29)))%>%
  select(subj,attention,motor,nonplanning,bis)
head(bis)
hist(bis$bis)

save(bis,file='03_data/02_aggregated_data/bis.Rdata')

#####WURS-------------------------------------------------------------------
load('03_data/02_aggregated_data/wurs_raw.Rdata')
wurs$wurs<-apply(wurs[,1:25],1,sum)
wurs%<>%select(subj,wurs)
save(wurs,file='03_data/02_aggregated_data/wurs.Rdata')


#####ASRS-------------------------------------------------------------------
load('03_data/02_aggregated_data/asrs_raw.Rdata')
asrs$asrs6 <- apply(asrs[,paste("item_",1:6,sep="")],1,sum)
asrs$asrs.partb <- apply(asrs[,paste("item_",7:20,sep="")],1,sum)
asrs%<>% select(subj,asrs6,asrs.partb)
save(asrs,file='03_data/02_aggregated_data/asrs.Rdata')


#####BISBAS-------------------------------------------------------------------
load('03_data/02_aggregated_data/bisbas_raw.Rdata')

#bisbas$bisbas<-apply(bisbas[,1:24],1,sum)
#hist(bisbas$bisbas)

bisbas%<>%
  mutate(BIS_1  =rowMeans(cbind(item_2, item_8, item_13, item_16, item_19, item_22, item_24)),
         BAS_Drive      =rowMeans(cbind(item_3, item_9, item_12, item_21)),
         BAS_Fun_Seeking=rowMeans(cbind(item_5, item_10, item_15, item_20)),
         BAS_Reward_Responsiveness=rowMeans(cbind(item_4, item_7, item_14, item_18, item_23)))%>%
  select(subj,BIS_1,BAS_Drive,BAS_Fun_Seeking, BAS_Reward_Responsiveness)
head(bisbas)

hist(bisbas$BIS_1)
hist(bisbas$BAS_Drive)
hist(bisbas$BAS_Fun_Seeking)
hist(bisbas$BAS_Reward_Responsiveness)

save(bisbas,file='03_data/02_aggregated_data/bisbas.Rdata')

# starter -----------------------------------------------------------
load('03_data/02_aggregated_data/starter_raw.Rdata')
df_starter%<>%
  mutate(subj = df_starter[,1],rt= df_starter[,3],num_tries= df_starter[,2])%>%
  select(subj,rt,num_tries)

head(df_starter) 
hist(df_starter$rt)
mean(df_starter$rt)/60

hist(df_starter$rt[df_starter$rt<5000])
mean(df_starter$rt[df_starter$rt<5000])/60

hist(as.integer(df_starter$num_tries))
mean(as.integer(df_starter$num_tries))

hist(as.integer(df_starter$num_tries[df_starter$num_tries<8]))
mean(as.integer(df_starter$num_tries[df_starter$num_tries<8]))

#average reading+quiz time =11.58869 minutes , average number of tries = 1.727273
#without outlier :  average reading+quiz time = 10.06273 minutes , average number of tries = 1.839286


save(df_starter,file='03_data/02_aggregated_data/starter.Rdata')

# post task open questions -----------------------------------------------------------
load('03_data/02_aggregated_data/questions_raw.Rdata')
hist(df_questions$rt[(df_questions$rt)<795213]/60)
mean(df_questions$rt[(df_questions$rt)<795213]/60)/60

df_questions$age <-as.integer(gsub("[{\"Q\":\"]", '', df_questions$age))
df_questions$age[df_questions$age=="0"]<-19
hist(df_questions$age)
min(na.omit(df_questions$age))
max(na.omit(df_questions$age))
sd(na.omit(df_questions$age))
mean(na.omit(df_questions$age))
#age ranging from 18 to 52, mean age = 25.80357, SD= 8.945125


gender <-gsub("[{\"Q1\":\"]", '', df_questions$gender[df_questions$age!="{\"Q1\":\""])
df_questions$gender <- (gender=="male"|gender=="Male")*1
#35 male 21 female 
sum(as.integer(na.omit(df_questions$gender)))

df_basic_info = data.frame(
  df_questions$subj,
  df_questions$prolific_id,
  df_questions$age,
  df_questions$gender
)

save(df_basic_info,file='03_data/02_aggregated_data/basic_info.Rdata')


df_questions$comprehension
#28 participants understood it very well 
#participant 20,28 and 49 had difficulties 

df_questions$misunderstanding
#most participants had no misunderstandings 
#sub 52 -  preferred pressing a key then waiting 
#sub 4,6,28,35,44,46,56 had difficulty around the probability of preducing a pearl

df_questions$difficulty1
#11 participants experienced the task difficulty level as easy  
#37 participants experienced the task difficulty level as medium  
#5 participants experienced the task difficulty level as difficult  

df_questions$difficulty2
#sub 16 didn't enjoy the waiting.. 
#sub 7,11,12,17,23,29,53 it was just luck
#sub 2,3,13,26,47 it was long or repetitiveness
#sub 27,50 bored 
#sub 51 - tried to remember the ships position 
#  participants experienced difficulty by the changing reward probability and understanding the pattern 

df_questions$strategy
# sub 7,22,26,47 used random strategy / no strategy 
#10 always choose the purple fisherman, 18,24 stick to the purple wave  
#sub 37 is a computer science student :)
# sub 50 switched his choices (regardless of the outcome)
#sub 51 did press>no press followed by a no press>press and so on (regardless of the outcome)
#most of the subjects used a strategy of finding a beneficial oyster and then sticking to it until it loses then changing a selection 


save(df_questions,file='03_data/02_aggregated_data/open_questions.Rdata')
