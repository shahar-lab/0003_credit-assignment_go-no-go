#lines i have questions about: 
#6,25,26,27,28
#do we need to assign fractals to keys? (30-31)

simme_tst<-function(x,rndwlk,subj,experiment,Ntrls) {
  # this function simulate an agent in the two step task Ntrls times
  
  # set up the models' parameters - 7 free parameters (β1, β2, α1, α2, λ, p, w)
  alpha1   =x[1]
  alpha2   =x[9]
  beta1    =x[2]
  beta2    =x[2] #twice X[2]?
  lambda1  =x[3]
  w       =x[4]
  persv    =x[5]
  
  #pre allocate data frame
  df=data.frame()
  
  for (t in 1:Ntrls) {
    if (t==1){
      #pre allocate vars
      Qmf            =matrix(0,3,2)     #state (3) x fractal at each state (2)
      Qmb=Qnet       =c(0,0)
      perCh1         =c(0,0) #what is this? 
      stmat          =rbind(c(2,3),c(3,2))}  #state transition map
    
    #randomly assign fractal to keys
    sr1=sample(1:2,size=1)
    
    # make a first stage choice
    Qmb[1] =.7*max(Qmf[2,1:2])+.3*max(Qmf[3,1:2])
    Qmb[2] =.3*max(Qmf[2,1:2])+.7*max(Qmf[3,1:2])
    
    Qnet[1]=(1-w)*Qmf[1,1] + (w1)*Qmb[1]
    Qnet[2]=(1-w)*Qmf[1,2] + (w1)*Qmb[2]
    
    Qnet[1]=Qnet[1]+persv*perCh1[1]+persvKey*perKey1[sr1]  +perKeygen[sr1]  + w2*Qkey[sr1]
    Qnet[2]=Qnet[2]+persv*perCh1[2]+persvKey*perKey1[3-sr1]+perKeygen[3-sr1]+ w2*Qkey[3-sr1]
  }
}
}