
########################################################################
################ convert json to Rdata  ################################
####################################################################
#this converts the answers to the post-task open questions from jason to data.frame

#note that here prolific id is taken from the starter file

con_gonogo_questions_json<-function(task_name,datafile,curnfolder,files,subnum) {
  
  if (sum(grepl(task_name,files))>0) {
    #x - will hold the raw converted json 
    x <- fromJSON(fromJSON(file=paste(curnfolder,'/',files[grepl(task_name,files)],sep=""))$data)
    
    #prolific id
    prolific_id <- fromJSON(file=paste(curnfolder,'/',files[grepl('starter',files)][1],sep=""))$`data[prolific-id]`

    i <-sapply(1:length(x), function(i) {(x[[i]]$trial_id=='post task questions')}) 
    df_questions<-
      data.frame(
        subj       =rep(subnum,sum(i)),
        prolific_id=rep(prolific_id,sum(i)),
        Q1       =unlist(lapply(x[i],function(xx) {xx[names(xx)=='responses']})),
        rt         =unlist(lapply(x[i],function(xx) {xx[names(xx)=='rt']}))
      )
    
    temp<-str_split(df_questions$Q1,'\",\"')
    df_questions$age<- temp[[1]][1]
    df_questions$gender<- temp[[1]][2]
    df_questions$difficulty1 <- temp[[1]][3]
    df_questions$difficulty2<- temp[[1]][4]
    df_questions$comprehension<- temp[[1]][5]
    df_questions$misunderstanding<- temp[[1]][6]
    df_questions$strategy<- temp[[1]][7]
      
    return(rbind(datafile,df_questions))
  }
  else{
    return(datafile)
  }
}


