#This file contains code for the state_update function, which
#given an event number (corresponding to a given possible population change)
#will apply the corresponding change to the population
state_update <- function(state,event_num,params){
  i_age <- apply(state[["i"]],1,sum)
  e_age <- apply(state[["e"]],1,sum)
  s_age <- apply(state[["s"]],1,sum)
  m<-params[["m"]]
  n<-params[["n"]]
  switch(event_num,
         #individual born
         {state[["s"]][1,1]<-state[["s"]][1,1]+1},
         
         #2-6
         #s1 infected
         {state[["s"]]<-s_removal(1,state[["s"]],m)
         state[["i"]][1,1]<-state[["i"]][1,1]+1},
         #s2 infected
         {state[["s"]]<-s_removal(2,state[["s"]],m)
         state[["i"]][2,1]<-state[["i"]][2,1]+1},
         #s3 infected
         {state[["s"]]<-s_removal(3,state[["s"]],m)
         state[["i"]][3,1]<-state[["i"]][3,1]+1},
         #s4 infected
         {state[["s"]]<-s_removal(4,state[["s"]],m)
         state[["i"]][4,1]<-state[["i"]][4,1]+1},
         #s5 infected
         {state[["s"]]<-s_removal(5,state[["s"]],m)
         state[["i"]][5,1]<-state[["i"]][5,1]+1},
         
         #7-11
         #s1 dies
         state[["s"]]<-s_removal(1,state[["s"]],m),
         #s2 dies
         state[["s"]]<-s_removal(2,state[["s"]],m),
         #s3 dies
         state[["s"]]<-s_removal(3,state[["s"]],m),
         #s4 dies
         state[["s"]]<-s_removal(4,state[["s"]],m),
         #s5 dies
         state[["s"]]<-s_removal(5,state[["s"]],m),
         
         #12-16
         #e1 dies
         state[["e"]]<-e_removal(1,state[["e"]],m,n),
         #e2 dies
         state[["e"]]<-e_removal(2,state[["e"]],m,n),
         #e3 dies
         state[["e"]]<-e_removal(3,state[["e"]],m,n),
         #e4 dies
         state[["e"]]<-e_removal(4,state[["e"]],m,n),
         #e5 dies
         state[["e"]]<-e_removal(5,state[["e"]],m,n),
         
         #17-21
         #i1 dies
         state[["i"]]<-s_removal(1,state[["i"]],m),
         #i2 dies
         state[["i"]]<-s_removal(2,state[["i"]],m),
         #i3 dies
         state[["i"]]<-s_removal(3,state[["i"]],m),
         #i4 dies
         state[["i"]]<-s_removal(4,state[["i"]],m),
         #i5 dies
         state[["i"]]<-s_removal(5,state[["i"]],m),
         
         #22
         #someone in s ages
         state[["s"]]<-s_age(state[["s"]],m),
         #23
         #someone in e ages
         state[["e"]]<-e_age(state[["e"]],m,n),
         #24
         #someone in i ages
         state[["i"]]<-s_age(state[["i"]],m),
         
         #25
         #someone in e moves through a delay stage
         {new_states <- e_advance(state[["e"]],state[["i"]],m,n)
         state[["e"]]<-new_states[[1]]
         state[["i"]]<-new_states[[2]]}
         )
  return(state)
}

#also works for i
s_removal <- function(age,s,m){
  s_stages <- s[age,]
  to_infect <- sample(1:m,1,prob=s_stages)
  s[age,to_infect] <- s[age,to_infect]-1
  return(s)
}
e_removal <- function(age,e,m,n){
  e_stages <- e[age,,]
  
  age_stage_sums <- apply(e_stages,2,sum)
  to_infect_age_stage <- sample(1:m,1,prob=age_stage_sum)
  
  to_infect_delay_stage <- sampl(1:n,1,prob=e[age,to_infect_age_stage,])
  e[age,to_infect_age_stage,to_infect_delay_stage]<-e[age,to_infect_age_stage,to_infect_delay_stage]-1
  return(e)
}

#also works for i
s_age <- function(s,m){
  s_ages <- apply(s[1:4,],1,sum)
  age_to_age <- sample(1:4,1,prob=s_ages)
  stage_to_age <- sample(1:m,1,prob=s[age_to_age,])
  
  s[age_to_age,stage_to_age]<-s[age_to_age,stage_to_age]-1
  
  if(stage_to_age==m){
    s[age_to_age+1,1]<-s[age_to_age+1,1]+1
  } else {
    s[age_to_age,stage_to_age+1]<-s[age_to_age,stage_to_age+1]+1
  }
  return(s)
}

e_age <- function(e,m,n){
  e_ages <- apply(e[1:4,,],1,sum)
  age_to_age <- sample(1:4,1,prob=e_ages)
  
  e_stages <- apply(e[age_to_age,,],1,sum)
  stage_to_age <- sample(1:m,1,prob=e_stages)
  
  e_delay_stages <- sum(e[age_to_age,stage_to_age,])
  delay_stage_to_age <- sample(1:n,1,prob=e_delay_stages)
  
  e[age_to_age,stage_to_age,delay_stage_to_age]<-e[age_to_age,stage_to_age,delay_stage_to_age]-1
  
  if(stage_to_age==m){
    e[age_to_age+1,1,delay_stage_to_age]<-e[age_to_age+1,1,delay_stage_to_age]+1
  } else {
    e[age_to_age,stage_to_age+1,delay_stage_to_age]<-e[age_to_age,stage_to_age+1,delay_stage_to_age]+1
  }
  return(e)
}

e_advance <- function(e,i,m,n){
  e_delay_stages <- apply(e,3,sum)
  delay_stage <- sample(1:(n-1),1,prob=e_delay_stages)
  
  ages <- apply(e[,,delay_stage],1,sum)
  age <- sample(1:5,1,prob=ages)
  
  age_delay_stage <- sample(1:m,1,prob=e[age,,delay_stage])
  
  e[age,age_delay_stage,delay_stage] <- e[age,age_delay_stage,delay_stage]-1
  if(delay_stage==n){
    i[age,age_delay_stage,1] <- i[age,age_delay_stage,1]+1
  } else {
    e[age,age_delay_stage,delay_stage+1]<-e[age,age_delay_stage,delay_stage+1]+1
  }
  return(list(e,i))
}











