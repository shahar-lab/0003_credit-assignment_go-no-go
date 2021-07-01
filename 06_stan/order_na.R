#this functions recives a dataframe with missing values (NANs) and a  
#returns a klien matrix (where the nans are at the end of each row)
order_na <-function(df,x){
  Nnan_trails = list()
  for (subj in seq(1:nrow(df))){
    Nnan_trails[subj]=(sum(is.na(df[subj,]))*1)
  }
  df_clean=as.data.frame(matrix(0, ncol = ncol(df), nrow = nrow(df)))
  for (subj in seq(1:nrow(df))){
    #print(subj)
    df_clean[subj,c((Ntrails-Nnan_trails[subj][[1]]):Ntrails)] <- x
    df_clean[subj,c(1:(Ntrails-Nnan_trails[subj][[1]]))] = df[subj,][(is.na(df[subj,]))==FALSE]
  }
  return(df_clean)
}

