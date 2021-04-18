Daw_fitme_tst<-function(x,df,Ntrls) {
  #model parameters
  alpha1   =mylogit(x[1])
  alpha2   =mylogit(x[2])
  beta1    =exp(x[3])
  beta2    =exp(x[4])
  lambda   =mylogit(x[5])
  w1       =mylogit(x[6])
  persv    =mylogit(x[7])-.5

  #get data
  trl  =df$trl
  ch1  =df$ch1
  ch2  =df$ch2
  state=df$state
  rw   =df$rw
  trn  =df$trn
#neglected 23-26
  
  Like1=Like2=vector()
  
  for (t   in 1:Ntrls) {
    if (trl[t]==1){
      #pre allocate vars
      Qmf             =matrix(0,3,2)     #state x fractal
      Qmb=Qnet=Qkey   =c(0,0)
      perCh1          =c(0,0) 
      stmat           =rbind(c(2,3),c(3,2))  #state transition map
      
      # make a first stage choice
      Qmb[1] =.7*max(Qmf[2,1:2])+.3*max(Qmf[3,1:2])
      Qmb[2] =.3*max(Qmf[2,1:2])+.7*max(Qmf[3,1:2])
      
      Qnet[1]=(1-w1)*Qmf[1,1] + (w1)*Qmb[1]+persv*perCh1[1]
      Qnet[2]=(1-w1)*Qmf[1,2] + (w1)*Qmb[2]+persv*perCh1[2]
      
      #softmax first choice
      Like1[t]  =(exp(beta1*Qnet)/sum(exp(beta1*Qnet)))[ch1[t]]
      
      #neglected 54-58
      
      # second stage choice
      Qnet[1]=Qmf[state[t],1] #no peresevration parameters
      Qnet[2]=Qmf[state[t],2]
      
      #update fractal model-free Qvalues
      PE1=(Qmf[state[t],ch2[t]]-Qmf[1,ch1[t]]);
      PE2=(rw[t]-Qmf[state[t],ch2[t]]);
      
      Qmf[1,ch1[t]]       =Qmf[1,ch1[t]]    +alpha1*PE1+alpha1*lambda1*PE2
      Qmf[state[t],ch2[t]]=Qmf[state[t],ch2[t]]+alpha1*PE2
      
      #update perservation parameter
      perCh1         =c((df$ch1==1)*1,(df$ch1==2)*1) #object perseveration "switch":ch1 is the first state object selected at the previous trial.
      
    }
    LL=sum(log(Like1))+sum(log(Like2))
    return(-LL)
  }
}
  