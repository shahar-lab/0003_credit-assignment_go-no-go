# this function simulate an agent in the two-stage go/nogo task Ntrls times

gong_simme<-function(x,rndwlk,subj,experiment,Ntrls) {
  
  # set up the models' parameters - 6 free parameters (α1, α2, β1, λ, pers, go_bias)
  alpha1         =x[1]
  alpha2         =x[1]
  gamma1         =x[2]
  gamma2         =x[3]
  beta1          =x[4]
  lambda         =x[5]
  persv          =x[6]
  go_b           =x[7]
  
  #pre allocate data frame
  df=data.frame()
  
  for (t in 1:Ntrls) {
    if (t==1){
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
    
    # Mapping first stage actions (Go/Nogo) to choices 
    map1       = sample(1:2,size=1) # assigning Go to a1 or a2 at state1 stage1 : map =1 -> a1=Go
    
    # make a first stage choice
    Qnet[1]= Qval[1,1] +persv*perCh[1]+persv*perResp[map1]+go_bias[map1]
    Qnet[2]= Qval[1,2] +persv*perCh[2]+persv*perResp[3-map1]+go_bias[3-map1]
    
    #softmax first choice
    p1  =exp(beta1*Qnet)/sum(exp(beta1*Qnet)) #Qnet[1]? #p1 = probability for action1 at state1 
    ch1 =sample(1:2,size=1,prob=c(p1[1],p1[2])) #where is p1[2] defined? shouldn't it be 1-p1? #the choice that actually occurred

    # make transition
    state=stmat[ch1]

    perResp = c((map1==ch1)*1,(map1!=ch1)*1)

    # Mapping second stage actions (Go/Nogo) to choices 
    map2       = sample(1:2,size=1) # assigning Go to a1 or a2 for the chosen state at stage2 : map =1 -> a1=Go
    
    # second stage choice
    Qnet[1]= Qval[state,1] +persv*perResp[map2] +go_bias[map2]
    Qnet[2]= Qval[state,2] +persv*perResp[3-map2] +go_bias[3-map2]
    
    p2  =exp(beta1*Qnet)/sum(exp(beta1*Qnet))
    ch2 =sample(1:2,size=1,prob=c(p2[1],p2[2]))
    
    perResp = c((map2==ch2)*1,(map2!=ch2)*1)
    
    #generate outcome
    rw.prob=rndwlk[t,ch2+(state-2)*2]
    rw     =sample(c(0,1),size=1,prob=c(1-rw.prob,rw.prob)) # reward =1  
    
    #update choices model-free Qvalues
    PE1=(Qval[state,ch2]-Qval[1,ch1])
    PE2=(rw-Qval[state,ch2])
    
    #assigning the alphas for the choices (a go or no-go affiliated)  
    resp1 = 2-(map1 == ch1)*1 #Go = 1, Nogo = 2 
    resp2 = 2-(map2 == ch2)*1 
    
    Qval[1,ch1]    =Qval[1,ch1]+alpha1*gamma1[resp1]*gamma2[resp2]*PE1+alpha1*gamma1[resp1]*gamma2[resp2]*lambda*PE2
    Qval[state,ch2]=Qval[state,ch2]+alpha2*gamma2[resp2]*PE2
    
    #update choice perservation parameter
    perCh [ch1]     =1
    perCh [3-ch1]   =0
    
    #save data
    df=rbind(df,
             data.frame(
               subj =subj,
               experiment  =experiment,
               trl  =t,
               ch1  =ch1,
               ch2  =ch2,
               state=state,
               rw   =rw,
               map1 = map1,
               map2 = map2,
               resp1 = resp1,
               resp2 = resp2,
               gamma1 = gamma1[2],
               gamma2 = gamma2[2]
               
             ))
  }
  return(df)
}
