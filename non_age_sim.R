#This file contains all the code necessary for running the 
#non-age-based simulations.
library(tidyverse)

#This function takes in the state and the parameters of the population,
#and assigns the rate for all of 8 possible events that can happen 
#in the simulation
lambda_assignment <- function(state,params){
  lambda <- numeric(8)
  lambda[1] <- params["b"]*sum(state)*(params["carrying"]-sum(state))/params["carrying"] #born
  lambda[2] <- params["mu"]*state[1] #_die_s
  lambda[3] <- params["beta"]*state[1]*state[3]/sum(state) #exposed
  lambda[4] <- params["k"]*state[2] #infected
  lambda[5] <- params["mu"]*state[2] #die_e
  lambda[6] <- params["mu"]*state[3] #die_i
  lambda[7] <- params["alpha"]*state[3] #die_infected
  lambda[8] <- params["p"]*state[3] #die_removed
  return(lambda)
}

#This function takes in the state of the population, along with a 
#given event number (corresponding to a possible event in the population
#as shown in lambda_assignment), and applies the corresponding event
#to the state and returns the new population state
state_update <- function(state,event_num){
  S <- state[1]
  E <- state[2]
  I <- state[3]
  switch(event_num,
         c(S+1,E,I),
         c(S-1,E,I),
         c(S-1,E+1,I),
         c(S,E-1,I+1),
         c(S,E-1,I),
         c(S,E,I-1),
         c(S,E,I-1),
         c(S,E,I-1))
}

#This is a helper function that takes in the state and the parameters and time,
#stochastically applies the appropriate event, and then returns the new state
#and the new time
next_step <- function(state,params,t){

  lambda <- lambda_assignment(state,params)
  lambda_overall <- sum(lambda)
  u <- runif(1)
  delta_t <- -1/lambda_overall*log(u)
  
  lambda_scaled <- cumsum(lambda/lambda_overall)
  z <- runif(1)
  event_num <- which(z<lambda_scaled)[1]
  
  state <- state_update(state,event_num)
  return(list(state,t+delta_t))
}

#The initial parameters for the model
params <- c(
  "b"=log(1.25)+log(2)/4,
  "mu"=log(2)/4,
  "beta"=2.644,
  "k"=log(2)/(3/12),
  "alpha"=(log(2)/(6/12))-log(2)/4,
  "p"=0,
  "carrying"=210
)

#The number of replicates to run the simulation
replicates <- 1000

data <- tibble(
  p=numeric(),
  replicate=numeric(),
  extinction_t=numeric(),
  extinct=logical(),
  min_s=numeric()
)

#The main simulation loop for the non-age-based simulation. This loop
#loops over a set variety of p values (removal rates), and for
#a given number of replicates runs unique simulations and then
#stores the required end statistics in the "data" data frame.
for(p in (0:20)*(1/20)){
  for(replicate in 1:replicates){
    state <- c(210-2,0,2)*0.5
    t <- 0
    params["p"] <- p
    
    min_s=1000
    #Run the simulation until no individuals are left, the simulation 
    #runs for more than 40 years, or all the infectious/exposed individuals
    #are eliminated
    while(sum(state)>0 & t <= 40 & (state[2]>0 | state[3]>0)){
      next_vals <- next_step(state,params,t)
      state <- next_vals[[1]]
      t <- next_vals[[2]]
      min_s_trial <- sum(state[1])
      if(min_s_trial<min_s){
        min_s <- min_s_trial
      }
    }
    to_add <- tibble(
      p=p,
      replicate=replicate,
      extinction_t=t,
      extinct=(sum(state)<1),
      disease_gone=(state[2]==0&state[3]==0),
      min_s=min_s
    )
    data <- rbind(data,to_add)
    print(c(p,replicate))
  }
}

#write the end results to a csv file
write_csv(data,"simple_results.csv")

