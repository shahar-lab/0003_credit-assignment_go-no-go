rm(list=ls())

library(dplyr)
library(magrittr)
library(data.table)

load(file='raw_data/complete_task.Rdata')
df<-df_task_clean

#get the total score of each subject 
total_reward<-data.frame(table(df$subj[df$reward==1]))

#SD of RT 
SD <- df %>%group_by(subj)%>%summarise(SD=(mean(sd(rt1),sd(rt2))/1000))

# ADHD score 
ADHD_score<-df%>%group_by(subj)%>%summarise(adhd_group_score=mean(adhd_group_score))

#Age 
load(file='raw_data/basic_info.Rdata')



df_new=data.frame(
  SUBJ <-unique(df$subj),
  Age <-df_basic_info$df_questions.age,
  Gender <-df_basic_info$df_questions.gender,
  Total_Reward<-total_reward[,2],
  SD<-SD[,2],
  ADHD_score<-ADHD_score[,2]
  
)
colnames(df_new)<-c("Subject_ID","Age","Gender","Total_Reward","SD","ADHD_Score")
save(df_new,file='project_DF_2.csv')
write.csv(df_new,"/home/shared/0003_credit-assignment_go-no-go/09_data_science_project/Project_DF_2.csv", row.names = FALSE)


#requieres examining
mean(df_new$Total_Reward[df_new$ADHD_Score==0])

# Arch --------------------------------------------------------------------

df <- df%>%filter(key1==1 & key2==1)
df1 <- df%>%filter(key1==1)
df2 <- df%>%filter(key2==1)

df<- df %>%group_by(subj)%>%mutate(row_number_bygroup = 1:n())

c<-data.frame(table(df$subj[df$key1==1]),table(df$subj[df$key2==1]),table(df$subj[df$key1==1 & df$key2==1]))
min(c[,2])
max(c[,2])

