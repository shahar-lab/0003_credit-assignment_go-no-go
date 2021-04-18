#lines i have questions about: 


Daw_simme<-function(x,rndwlk,subj,experiment,Ntrls) {
  # this function simulate an agent in the two-step go/nogo task Ntrls times
  
  # set up the models' parameters - 7 free parameters (β1, β2, α1, α2, λ, p, w)
  alpha_go       =x[1]
  alpha_ng       =x[2]
  alpha2         =x[3]
  beta1          =x[4]
  beta2          =x[5]
  lambda         =x[6]
  w              =x[7]
  persv_obj      =x[8]
  persv_act      =x[9]
  go_bias        =x[10]
    
  #pre allocate data frame
  df=data.frame()
  
  for (t in 1:Ntrls) {
    if (t==1){
      #pre allocate vars
      Qmf            =matrix(0,3,2)     #state (3) x fractal at each state (2)
      Qmb=Qnet       =c(0,0)
      perCh1         =c(0,0) 
      stmat          =rbind(c(2,3),c(3,2))}  #state transition map
    
#neglected r30-31
    
    # make a first stage choice
    Qmb[1] =.7*max(Qmf[2,1:2])+.3*max(Qmf[3,1:2])
    Qmb[2] =.3*max(Qmf[2,1:2])+.7*max(Qmf[3,1:2])
    
    Qnet[1]=(1-w)*Qmf[1,1] + (w)*Qmb[1]+persv*perCh1[1] #added the perservation to here 
    Qnet[2]=(1-w)*Qmf[1,2] + (w)*Qmb[2]+persv*perCh1[2]
    
    #softmax first choice
    p1  =exp(beta1*Qnet)/sum(exp(beta1*Qnet)) #Qnet[1]? #p1 = probability for action1 at state1 
    ch1 =sample(1:2,size=1,prob=c(p1[1],p1[2])) #where is p1[2] defined? shouldn't it be 1-p1? #the choice that actually occurred
    #i neglected the ket1 r46
    
    #neglected rows 48-52
    # make transition
    trn  =sample(0:1,size=1,prob=c(.7,.3)) #0-common 1-rare
    state=stmat[ch1,trn+1]
    #neglected row 57 
    
    # second stage choice
    #neglected rows 60-61, 65
    p2  =exp(beta2*Qnet)/sum(exp(beta2*Qnet))
    ch2 =sample(1:2,size=1,prob=c(p2[1],p2[2]))
    
    #generate outcome
    rw.prob=rndwlk[ch2,(state-1),t]
    rw     =sample(0:1,size=1,prob=c(1-rw.prob,rw.prob))
    
    #update fractal model-free Qvalues
    PE1=(Qmf[state,ch2]-Qmf[1,ch1])
    PE2=(rw-Qmf[state,ch2])
    
    Qmf[1,ch1]    =Qmf[1,ch1]+alpha1*PE1+alpha1*lambda*PE2
    Qmf[state,ch2]=Qmf[state,ch2]+alpha1*PE2
    
    #neglected rows 82-87
    #update perservation parameter
    perCh1         =c((ch1==1)*1,(ch1==2)*1) #object perseveration "switch":a1 is the first state object selected at the previos trial. the perseveration value will be added to the Q-value choosen at the previos trial.  will be used at the first state softmax
    
    
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
               trn  =trn,
#neglected r101-104
             ))
  }
  return(df)
}
