rm(list=ls())
rndwlk<-read.csv('rndwlk_v1.csv')
df<-rndwlk[seq(1:252),]

  temp_plot = ggplot(df, aes(seq(1:252))) +                    # basic graphical object
    geom_line(aes(y=df$V1), colour="red") +  # first bandit
    geom_line(aes(y=df$V2), colour="green") + # second bandit
    geom_line(aes(y=df$V3), colour="blue") + # third bandit
    geom_line(aes(y=df$V4), colour="orange")+  # forth bandit
    xlab("Trials")+ ylab("Reward probability")
  
  
  file_name = sprintf('rndwlk_vers.%d.csv', 1)
  write.csv(df,file = file_name, row.names = FALSE)
  files[1]<-file_name
  ggsave(temp_plot, file=paste0("plot_", 1,".png"), width = 14, height = 10, units = "cm")#use this to see the plots. they will appear at the files section


