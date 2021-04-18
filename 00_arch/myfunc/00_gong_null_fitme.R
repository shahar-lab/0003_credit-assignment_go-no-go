gong_null_fitme<-function(x,df,Ntrls) {

  #  6 free parameters (α1, α2, β1, λ, pers, go_bias)
  alpha1         =mylogit(x[1])
  #alpha2         =mylogit(x[2])
  beta1          =exp(x[3])
  lambda         =mylogit(x[4])
  persv          =mylogit(x[5])-.5
  go_b          =mylogit(x[6])
  
  #get data
  trl  =df$trl
  ch1  =df$ch1
  ch2  =df$ch2
  state=df$state
  rw   =df$rw
#  trn  =df$trn
  map1 = df$map1
  map2 = df$map2
  

  Like1=Like2=vector()
  
  for (t   in 1:Ntrls) {
    if (trl[t]==1){
      #pre allocate vars
      Qval           =matrix(0,3,2) #state (3) x fractal at each state (2)
      Qnet           =c(0,0)
      perCh          =matrix(0,3,2)    #repeating the same choice (state X fractal)
      perAct1        =matrix(0,2,2)    #repeating the same action at stage1 (mapping options X fractal)
      perAct2        =matrix(0,2,2)    #repeating the same action at stage1 (mapping options X fractal)
      stmat          =c(2,3) #state transition map
      go_bias        =rbind(c(go_b,0),c(0,go_b)) #bias toward a Go action in each state 
    }  
    
      # make a first stage choice
      Qnet[1]= Qval[1,1] +persv*perCh[1,1]+persv*perAct1[map1[t],1]+go_bias[map1[t],1]
      Qnet[2]= Qval[1,2] +persv*perCh[1,2]+persv*perAct1[map1[t],2]+go_bias[map1[t],2]
      
      #softmax first choice
      Like1[t]  =(exp(beta1*Qnet)/sum(exp(beta1*Qnet)))[ch1[t]]
     
      # second stage choice
      Qnet[1]= Qval[state[t],1] +persv*perAct2[map2[t],1] +go_bias[map2[t],1]
      Qnet[2]= Qval[state[t],2] +persv*perAct2[map2[t],2] +go_bias[map2[t],2]
      
      Like2[t]  =(exp(beta1*Qnet)/sum(exp(beta1*Qnet)))[ch2[t]]
      
      #update objects model-free Qvalues
      PE1=(Qval[state[t],ch2[t]]-Qval[1,ch1[t]])
      PE2=(rw[t]-Qval[state[t],ch2[t]])
      
      Qval[1,ch1[t]]    =Qval[1,ch1[t]]+alpha1*PE1+alpha1*lambda*PE2
      Qval[state[t],ch2[t]]=Qval[state[t],ch2[t]]+alpha1*PE2
      # Qmf[1,ch1[t]]       =Qmf[1,ch1[t]]+alpha1[map[t],ch1[t]]*PE1+alpha1[map[t],ch1[t]]*lambda*PE2

      #update choice perservation parameter
      perCh[ch1[t]]    =1
      perCh[3-ch1[t]]  =0
      
      #update action perservation parameter
      # 1st stage
      go1 = (map1[t] == ch1[t])*1 
      ng1 = 1-go1
      perAct1    =rbind(c(go1,ng1),c(ng1,go1))
      # 2nd stage
      go2 = (map2[t] == ch2[t])*1 
      ng2 = 1-go2
      perAct2    =rbind(c(go2,ng2),c(ng2,go2))
  }
    LL=sum(log(Like1))+sum(log(Like2))
    return(-LL)
  }