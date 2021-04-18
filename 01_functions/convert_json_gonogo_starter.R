
########################################################################
################ convert json to Rdata  ################################
####################################################################
#this converts the answers to the post-task open questions from jason to data.frame

#note that here prolific id is taken from the starter file

con_gonogo_starter_json<-function(task_name,datafile,curnfolder,files,subnum) {
  
  if (sum(grepl(task_name,files))==1) {
    df_starter<-
      data.frame(
        prolific_id <- fromJSON(file=paste(curnfolder,'/',files[grepl('starter',files)][1],sep=""))$`data[prolific-id]`,
        num_Tries <- fromJSON(file=paste(curnfolder,'/',files[grepl('starter',files)][1],sep=""))$`data[numTries]`,
        Time <- as.integer(fromJSON(file=paste(curnfolder,'/',files[grepl('starter',files)][1],sep=""))$`data[endTimeStamp]`)-as.integer(fromJSON(file=paste(curnfolder,'/',files[grepl('starter',files)][1],sep=""))$`data[startTimeStamp]`)
      )
    
    return(rbind(datafile,df_starter))
  }
  else if (sum(grepl(task_name,files))>1){
    df_starter<-
      data.frame(
        prolific_id <- fromJSON(file=paste(curnfolder,'/',files[grepl('starter',files)][1],sep=""))$`data[prolific-id]`,
        num_Tries <- fromJSON(file=paste(curnfolder,'/',files[grepl('starter',files)][1],sep=""))$`data[numTries]`,
        Time <- as.integer(fromJSON(file=paste(curnfolder,'/',files[grepl('starter',files)][1],sep=""))$`data[endTimeStamp]`)-as.integer(fromJSON(file=paste(curnfolder,'/',files[grepl('starter',files)][1],sep=""))$`data[startTimeStamp]`)
      )
    
    return(rbind(datafile,df_starter))
  }
  else{
    return(datafile)
  }
}


