#This file contains the code for the lambda_assignment function.
#This function takes in the current state and the parameters of the
#simulation, and returns a vector of lambda values, which each lambda
#value corresponding to the rate at which a given event happens. 
#The order of these event rates in the vector is consistent throughout the 
#rest of the simulation.

lambda_assignment <- function(state,params){
  
  i_age <- apply(state[["i"]],1,sum)
  e_age <- apply(state[["e"]],1,sum)
  s_age <- apply(state[["s"]],1,sum)
  N_age <- i_age+e_age+s_age
  N <- sum(N_age)
  lambda <- numeric(25)
  #individual born
  #b's are by AGE
  lambda[1]<-sum(params[["b"]]*N_age)*(1-N/params[["carrying"]])
  
  #individual infected
  #age 1
  lambda[2]<- sum((1/N)*(params[["beta"]][1,]*i_age*s_age[1]))
  #age 2
  lambda[3]<- sum((1/N)*(params[["beta"]][2,]*i_age*s_age[2]))
  #age 3
  lambda[4]<- sum((1/N)*(params[["beta"]][3,]*i_age*s_age[3]))
  #age 4
  lambda[5]<- sum((1/N)*(params[["beta"]][4,]*i_age*s_age[4]))
  #age 5
  lambda[6]<- sum((1/N)*(params[["beta"]][5,]*i_age*s_age[5]))
  
  #individual dies
  #s1
  lambda[7]<-params[["mu"]][1]*s_age[1]
  #s2
  lambda[8]<-params[["mu"]][2]*s_age[2]
  #s3
  lambda[9]<-params[["mu"]][3]*s_age[3]
  #s4
  lambda[10]<-params[["mu"]][4]*s_age[4]
  #s5
  lambda[11]<-params[["mu"]][5]*s_age[5]
  
  #e1
  lambda[12]<-params[["mu"]][1]*e_age[1]
  #e2
  lambda[13]<-params[["mu"]][2]*e_age[2]
  #e3
  lambda[14]<-params[["mu"]][3]*e_age[3]
  #e4
  lambda[15]<-params[["mu"]][4]*e_age[4]
  #e5
  lambda[16]<-params[["mu"]][5]*e_age[5]
  
  #i1
  lambda[17]<-(params[["mu"]]+params[["alpha"]]+params[["rho"]])[1]*i_age[1]
  #i2
  lambda[18]<-(params[["mu"]]+params[["alpha"]]+params[["rho"]])[2]*i_age[2]
  #i3
  lambda[19]<-(params[["mu"]]+params[["alpha"]]+params[["rho"]])[3]*i_age[3]
  #i4
  lambda[20]<-(params[["mu"]]+params[["alpha"]]+params[["rho"]])[4]*i_age[4]
  #i5
  lambda[21]<-(params[["mu"]]+params[["alpha"]]+params[["rho"]])[5]*i_age[5]
  
  #someone in s ages
  lambda[22]<-params[["m"]]*sum(s_age[1:4])
  #someone in e ages
  lambda[23]<-params[["m"]]*sum(e_age[1:4])
  #someone in i ages
  lambda[24]<-params[["m"]]*sum(i_age[1:4])
  
  #someone in e advances through a delay stage
  e_by_stage <- apply(state[["e"]],3,function(X){
    sum(X[1:(4)])
  })
  lambda[25]<-sum(e_age)*params[["n"]]*params[["k"]]
  return(lambda)
}


