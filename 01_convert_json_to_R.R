rm(list=ls())
library("rjson")
library(data.table)
library(dplyr)
library(tidyr)
library(parallel)
library(tidyverse)

source('01_functions/convert_json_gonogo.R')
source('01_functions/convert_json_gonogo_questions.R')
source('01_functions/convert_json_gonogo_starter.R')
source('01_functions/convert_json_self_reports.R')

mainfolder<-paste(getwd(),'/03_data/01_raw_data/final',sep="")
subfolder <-dir(mainfolder)
df_starter<-df_questions<-df_task<-asrs<-wurs<-bis<-bisbas<-data.frame()

print(length(subfolder))

for (i in 1:length(subfolder)){
  print(paste('folder #',i,': ',subfolder[i],sep=""))
  files<-dir(curnfolder<-paste(mainfolder,'/',subfolder[i],sep="", row.names=NULL))
  asrs    <-con_sr_json('asrs',asrs,curnfolder,files,i)
  wurs    <-con_sr_json('wurs',wurs,curnfolder,files,i)
  bis     <-con_sr_json('bis-',bis,curnfolder,files,i)
  bisbas  <-con_sr_json('bis_bas',bisbas,curnfolder,files,i)
  
  df_starter <- con_gonogo_starter_json('starter',df_starter,curnfolder,files,i)
  df_questions <- con_gonogo_questions_json('test',df_questions,curnfolder,files,i)
  df_task      <-con_gonogo_json('test',df_task,curnfolder,files,i)
}  

wurs[,1:25]<-sapply(wurs[,1:25],function(v) {as.numeric(v)})
asrs[,1:20]<-sapply(asrs[,1:20],function(v) {as.numeric(v)})
bis[,1:30] <-sapply(bis[,1:30],function(v) {as.numeric(v)})
bisbas[,1:24]<-sapply(bisbas[,1:24],function(v) {as.numeric(v)})

save(wurs,file='03_data/02_aggregated_data/wurs_raw.Rdata')
save(asrs,file='03_data/02_aggregated_data/asrs_raw.Rdata')
save(bis,file='03_data/02_aggregated_data/bis_raw.Rdata')
save(bisbas,file='03_data/02_aggregated_data/bisbas_raw.Rdata')
save(df_task,file='03_data/02_aggregated_data/task_raw.Rdata')
save(df_questions,file='03_data/02_aggregated_data/questions_raw.Rdata')
save(df_starter,file='03_data/02_aggregated_data/starter_raw.Rdata')


