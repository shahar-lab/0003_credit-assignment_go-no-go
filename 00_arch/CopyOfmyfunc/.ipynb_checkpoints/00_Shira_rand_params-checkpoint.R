#this code generates 10 parameters to simulate an agent at the two-step go/nogo task

rand_params<-function(){
  x=
    c(
      alpha_go       =runif(1),# we simulated it with seq(0,1,.2)
      alpha_ng       =runif(1),#to 0 
      alpha2         =runif(1),
      beta1          =runif(1,min=0.1,max=10),
      beta2          =runif(1,min=0.1,max=10), #might be neglected
      lambda         =runif(1),
      w              =runif(1), # we simulated it with seq(0,1,.2)
      persv_obj      =runif(1,min=-0.5,max=0.5),
      persv_act      =runif(1,min=-0.5,max=0.5),
      go_bias        =runif(1,min=-1,max=1))

  return(x)}

transform_params<-function(x,cfg){
  x[cfg=='logit']<-mylogit(x[cfg=='logit'])
  x[cfg=='exp']  <-mylogit(x[cfg=='exp'])
  return(x)}
