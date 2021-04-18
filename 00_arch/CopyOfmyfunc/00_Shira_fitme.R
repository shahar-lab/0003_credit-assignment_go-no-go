Shira_fitme<-function(x,df,Ntrls) {

    # 10 free parameters (α_go, α_ng, α2, β1, β2, λ, w, p_obj, p_act, go_bias)
  alpha_go       =mylogit(x[1])
  alpha_ng       =mylogit(x[2])
  #alpha_ng       = 0.00000001
  alpha2         =mylogit(x[3])
  beta1          =exp(x[4])
  beta2          =exp(x[5]) #might be neglected
  lambda         =mylogit(x[6])
  w              =mylogit(x[7])
  persv_obj      =mylogit(x[8])-.5
  persv_act      =mylogit(x[9])-.5
  go_bias        =mylogit(x[10])-.5
  
  #get data
  trl  =df$trl
  ch1  =df$ch1
  ch2  =df$ch2
  state=df$state
  rw   =df$rw
  trn  =df$trn
  map = df$map
  go  = df$go
#neglected 23-26
  
  Like1=Like2=vector()
  
  for (t   in 1:Ntrls) {
    if (trl[t]==1){
      #pre allocate vars
      Qmf            =matrix(0,3,2) #state (3) x fractal at each state (2)
      Qmb=Qnet       =c(0,0)
      perCh1         =c(0,0)    #repeating the same first stage choice
      stmat          =rbind(c(2,3),c(3,2)) #state transition map
      alpha1         =rbind(c(alpha_go,alpha_ng),c(alpha_ng,alpha_go))
      perAct         =rbind(c(0,0),c(0,0))    #repeating the same action go | nogo
      go_bias1       =rbind(c(go_bias,0),c(0,go_bias)) #bias toward a Go action
    }
    
      
      # make a first stage choice
      Qmb[1] =.7*max(Qmf[2,1:2])+.3*max(Qmf[3,1:2])
      Qmb[2] =.3*max(Qmf[2,1:2])+.7*max(Qmf[3,1:2])
      
      Qnet[1]=(1-w)*Qmf[1,1] + (w)*Qmb[1]
      Qnet[2]=(1-w)*Qmf[1,2] + (w)*Qmb[2]
      
      Qnet[1]= Qnet[1] +persv_obj*perCh1[1]+persv_act*perAct[map[t],1]+go_bias1[map[t],1]
      Qnet[2]= Qnet[2] +persv_obj*perCh1[2]+persv_act*perAct[map[t],2]+go_bias1[map[t],2]
      
      #softmax first choice
      Like1[t]  =(exp(beta1*Qnet)/sum(exp(beta1*Qnet)))[ch1[t]]
      
      #neglected 54-58
      
      # second stage choice
      Qnet[1]=Qmf[state[t],1] #no peresevration parameters
      Qnet[2]=Qmf[state[t],2]
      
      Like2[t]  =(exp(beta2*Qnet)/sum(exp(beta2*Qnet)))[ch2[t]]
      
      #update objects model-free Qvalues
      PE1=(Qmf[state[t],ch2[t]]-Qmf[1,ch1[t]])
      PE2=(rw[t]-Qmf[state[t],ch2[t]])
      
      Qmf[1,ch1[t]]       =Qmf[1,ch1[t]]    +alpha1[map[t],ch1[t]]*PE1+alpha1[map[t],ch1[t]]*lambda*PE2
      Qmf[state[t],ch2[t]]=Qmf[state[t],ch2[t]]+alpha2*PE2
      
      #update perservation parameter
      perCh1[ch1[t]]    =1
      perCh1[3-ch1[t]]  =0
      
      #update action perservation parameter
      perAct    =rbind(c(go[t],1-go[t]),c(1-go[t],go[t]))
  }
    LL=sum(log(Like1))+sum(log(Like2))
    return(-LL)
  }