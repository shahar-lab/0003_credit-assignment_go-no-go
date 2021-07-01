# 1st Model  
# this function simulate an agent in the two-stage go/nogo task Ntrls times
# this agents learning is influenced by the motor component of the action
# the agent has 6 parameters : alpha1, beta1, lambda, perseveration, go bias, gamma1 
# additionally, each stage the objects are randomly assigned (i.e, mapping)

sim.model1<-function(x,rndwlk,Ntrl) {
  
  # set up the models' parameters - 6 free parameters (α1, β1, λ, pers, go_bias)
  alpha1         =x[[1]]
  beta1          =x[[2]]
  lambda         =x[[3]]
  persv          =x[[4]]
  go_b           =x[[5]]
  gamma1         =x[[6]]
  
  
  #pre allocate data frame
  df=data.frame()
  
  for (t in 1:Ntrl) {
    if (t==1){
      #pre allocate vars
      zeros        <- rep(0, 2*2*2)
      Qval           <- array(zeros, c(2, 2, 2)) #stage (2) X state (2) x fractal at each state (2)   
      Qnet           =c(0,0)
      stmat          =c(1,2) #state transition map
      perCh          =c(0,0)       #repeating the same choice (state X fractal)
      go_bias        =c(go_b,0) #bias toward a Go action in each state 
      gamma1         =c(1,gamma1)
    }  
    
    #current trial decisions -----------------------------------
    map1       = sample(1:2,size=1) # assigning Go to a1 or a2 at state1 stage1 : map =1 -> a1=Go
    
    # make a first stage choice
    Qnet[1]= Qval[1,1,1] + persv*perCh[1] + go_bias[map1]
    Qnet[2]= Qval[1,1,2] + persv*perCh[2] + go_bias[3-map1]
    
    #softmax first choice
    p1  =exp(beta1*Qnet)/sum(exp(beta1*Qnet))  
    ch1 =sample(1:2,size=1,prob=c(p1[1],p1[2])) 
    
    # make transition
    state=stmat[ch1]
    
    # Mapping second stage actions (Go/Nogo) to choices 
    map2       = sample(1:2,size=1) # assigning Go to a1 or a2 for the chosen state at stage2 : map =1 -> a1=Go
    
    # second stage choice
    Qnet[1]= Qval[2,state,1] + go_bias[map2]
    Qnet[2]= Qval[2,state,2] + go_bias[3-map2]
    
    p2  =exp(beta1*Qnet)/sum(exp(beta1*Qnet))
    ch2 =sample(1:2,size=1,prob=c(p2[1],p2[2]))
    
    #generate outcome
    rw.prob=rndwlk[ch2+(state-1)*2,t]
    rw     =sample(c(0,1),size=1,prob=c(1-rw.prob,rw.prob)) # reward =1  
    
    #assigning the alphas for the choices (a go or no-go affiliated)  
    resp1 = 2-(map1 == ch1)*1 #Go = 1, Nogo = 2 
    resp2 = 2-(map2 == ch2)*1 
    
    #save data---------------------------------------------
    df=rbind(df,
             data.frame(
               ch1  =ch1,
               ch2  =ch2,
               state=state,
               rw   =rw,
               map1 = map1,
               map2 = map2,
               perCh1=perCh[1],
               perCh2=perCh[2],
               resp1 = resp1,
               resp2 =resp2
                ))
    
    #update for next trial--------------------------------
    #update choices model-free Qvalues
    PE1=(Qval[2,state,ch2]-Qval[1,1,ch1])
    PE2=(rw-Qval[2,state,ch2])
    
    Qval[1,1,ch1]    =Qval[1,1,ch1]+alpha1*gamma1[resp1]*PE1+alpha1*gamma1[resp2]*lambda*PE2 #האם שני הresp הם 1?
    Qval[2,state,ch2]=Qval[2,state,ch2]+alpha1*gamma1[resp2]*PE2 #האם כאן יש גאמא בכלל?
    
    #update choice perservation parameter
    perCh [ch1]     =1
    perCh [3-ch1]   =0
    
  }
  return(df)
}

