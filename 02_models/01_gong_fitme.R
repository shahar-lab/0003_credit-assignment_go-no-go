gong_fitme<-function(x,df,Ntrls) {

  #  6 free parameters (α1, α2, β1, λ, pers, go_bias)
  alpha1         =mylogit(x[1])
  alpha2         =mylogit(x[1])
  gamma1         =mylogit(x[2])
  gamma2         =mylogit(x[3])
  beta1          =exp(x[4])
  lambda         =mylogit(x[5])
  persv          =mylogit(x[6])-.5
  go_b          =mylogit(x[7])-.5
  
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
      perCh          =c(0,0)       #repeating the same choice (state X fractal)
      perResp         =c(0,0)    #repeating the same action at stage1 (mapping options X fractal)
      stmat          =c(2,3) #state transition map
      go_bias        =c(go_b,0) #bias toward a Go action in each state 
      # gammas         =rbind(c(1,gamma2),c(gamma1,gamma1*gamma2)) #discounting factors for the learning rate (alpha). first row is for go at the first stage, first column is for go at the second stage :[(go1go2,go1ng2),(ng1go2,ng1ng2)] 
      gamma2         =c(1,gamma2)
      gamma1         =c(1,gamma1)
    }    
    
      # make a first stage choice
      Qnet[1]= Qval[1,1] +persv*perCh[1]+persv*perResp[map1[t]]+go_bias[map1[t]]
      Qnet[2]= Qval[1,2] +persv*perCh[2]+persv*perResp[3-map1[t]]+go_bias[3-map1[t]]
      
      #softmax first choice
      Like1[t]  =(exp(beta1*Qnet)/sum(exp(beta1*Qnet)))[ch1[t]]
      
      #update action perservation parameter
      perResp = c((map1[t]==ch1[t])*1,(map1[t]!=ch1[t])*1)
     
      # second stage choice
      Qnet[1]= Qval[state[t],1] +persv*perResp[map2[t]] +go_bias[map2[t]]
      Qnet[2]= Qval[state[t],2] +persv*perResp[3-map2[t]] +go_bias[3-map2[t]]
      
      Like2[t]  =(exp(beta1*Qnet)/sum(exp(beta1*Qnet)))[ch2[t]]
     
       #update action perservation parameter
      perResp = c((map2[t]==ch2[t])*1,(map2[t]!=ch2[t])*1)
      
      #update objects model-free Qvalues
      PE1=(Qval[state[t],ch2[t]]-Qval[1,ch1[t]])
      PE2=(rw[t]-Qval[state[t],ch2[t]])
      
      #assigning the alphas for the choices (a go or no-go affiliated)  
      resp1 = 2-(map1[t] == ch1[t])*1 #Go = 1, Nogo = 2 
      resp2 = 2-(map2[t] == ch2[t])*1 
     
      Qval[1,ch1[t]]    =Qval[1,ch1[t]]+alpha1*gamma1[resp1]*gamma2[resp2]*PE1+alpha1*gamma1[resp1]*gamma2[resp2]*lambda*PE2
      Qval[state[t],ch2[t]]=Qval[state[t],ch2[t]]+alpha2*gamma2[resp2]*PE2
      
      #update choice perservation parameter
      perCh[ch1[t]]    =1
      perCh[3-ch1[t]]  =0
      
  }
    LL=sum(log(Like1))+sum(log(Like2))
    return(-LL)
  }