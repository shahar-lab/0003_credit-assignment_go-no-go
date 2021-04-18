#null model parameter recovery

gong_fitme_null<-function(x,df,Ntrls) {
  
  #  6 free parameters (α1, α2, β1, λ, pers, go_bias)
  alpha1         =mylogit(x[1])
  alpha2         =mylogit(x[2])
  beta1          =exp(x[3])
  lambda         =mylogit(x[4])
  persv          =mylogit(x[5])-.5
  go_b          =mylogit(x[6])-.5
  
  #get data
  trl  =df$trl
  ch1  =df$ch1
  ch2  =df$ch2
  state=df$state
  rw   =df$rw
  map1 = df$map1
  map2 = df$map2
  
  Like1=Like2=vector()
  
  for (t   in 1:Ntrls) {
    if (trl[t]==1){
      #pre allocate vars
      Qval           =matrix(0,3,2) #state (3) x fractal at each state (2)
      Qnet           =c(0,0)
      perCh          =c(0,0)       #repeating the same choice (state X fractal)
      stmat          =c(2,3) #state transition map
      go_bias        =c(go_b,0) #bias toward a Go action in each state 
    }    
    
    # make a first stage choice
    Qnet[1]= Qval[1,1] +persv*perCh[1]+go_bias[map1[t]]
    Qnet[2]= Qval[1,2] +persv*perCh[2]+go_bias[3-map1[t]]
    
    #softmax first choice
    Like1[t]  =(exp(beta1*Qnet)/sum(exp(beta1*Qnet)))[ch1[t]]
    
    # second stage choice
    Qnet[1]= Qval[state[t],1] +go_bias[map2[t]]
    Qnet[2]= Qval[state[t],2] +go_bias[3-map2[t]]
  
    Like2[t]  =(exp(beta1*Qnet)/sum(exp(beta1*Qnet)))[ch2[t]]
    
    #update objects model-free Q-values
    PE1=(Qval[state[t],ch2[t]]-Qval[1,ch1[t]])
    PE2=(rw[t]-Qval[state[t],ch2[t]])
    
    Qval[1,ch1[t]]    =Qval[1,ch1[t]]+alpha1*PE1+alpha1*lambda*PE2
    Qval[state[t],ch2[t]]=Qval[state[t],ch2[t]]+alpha2*PE2
    
    #update choice perservation parameter
    perCh[ch1[t]]    =1
    perCh[3-ch1[t]]  =0
    
  }
  LL=sum(log(Like1))+sum(log(Like2))
  return(-LL)
}