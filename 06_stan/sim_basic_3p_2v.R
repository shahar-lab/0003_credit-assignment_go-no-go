#basic model -  1805 
# this function simulate an agent in the two-stage task Ntrls times
# the agent has 2 parameters : alpha1, beta

sim_basic_2st<-function(x,rndwlk,subj,experiment,Ntrls) {
  
  # set up the models' parameters - 6 free parameters (α1, α2, β1, λ, pers, go_bias)
  alpha1         =x[[1]]
  beta1          =x[[2]]
  lambda         =x[[3]]
  
  
  #pre allocate data frame
  df=data.frame()
  
  for (t in 1:Ntrls) {
    if (t==1){
      #pre allocate vars
      zeros        <- rep(0, 2*2*2)
      Qval           <- array(zeros, c(2, 2, 2)) #stage (2) X state (2) x fractal at each state (2)   
      Qnet           =c(0,0)
      stmat          =c(1,2) #state transition map
    }  
    
    # make a first stage choice
    Qnet[1]= Qval[1,1,1] 
    Qnet[2]= Qval[1,1,2] 
    
    #softmax first choice
    p1  =exp(beta1*Qnet)/sum(exp(beta1*Qnet)) #Qnet[1]? #p1 = probability for action1 at state1 
    ch1 =sample(1:2,size=1,prob=c(p1[1],p1[2])) #where is p1[2] defined? shouldn't it be 1-p1? #the choice that actually occurred
    
    # make transition
    state=stmat[ch1]
    
    # second stage choice
    Qnet[1]= Qval[2,state,1]
    Qnet[2]= Qval[2,state,2]
    
    p2  =exp(beta1*Qnet)/sum(exp(beta1*Qnet))
    ch2 =sample(1:2,size=1,prob=c(p2[1],p2[2]))
    
    #generate outcome
    rw.prob=rndwlk[ch2+(state-1)*2,t]
    rw     =sample(c(0,1),size=1,prob=c(1-rw.prob,rw.prob)) # reward =1  
    
    #update choices model-free Qvalues
    PE1=(Qval[2,state,ch2]-Qval[1,1,ch1])
    PE2=(rw-Qval[2,state,ch2])
    
    
    Qval[1,1,ch1]    =Qval[1,1,ch1]+alpha1*PE1+alpha1*lambda*PE2
    Qval[2,state,ch2]=Qval[2,state,ch2]+alpha1*PE2
    
    
    #save data
    df=rbind(df,
             data.frame(
               subj =subj,
               experiment  =experiment,
               trl  =t,
               ch1  =ch1,
               ch2  =ch2,
               state=state,
               rw   =rw
             ))
  }
  return(df)
}
