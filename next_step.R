#This file contains code for the next_step function, which handles
#each step of the simulation. It takes in the state and the parameters
#of the simulation at a given time, stochastically applies a possible event,
#and then returns the new population state along with the new time.
next_step <- function(state,params,t){
  
  lambda <- lambda_assignment(state,params)
  lambda_overall <- sum(lambda)
  u <- runif(1)
  delta_t <- -1/lambda_overall*log(u)
  
  lambda_scaled <- cumsum(lambda/lambda_overall)
  z <- runif(1)
  event_num <- which(z<lambda_scaled)[1]
  if(event_num!=22){
    #print(event_num)
  }
  
  state <- state_update(state,event_num,params)
  return(list(state,t+delta_t))
}