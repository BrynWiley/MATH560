#This file contains the main code for running the age-based simulation,
#and coordinates the helper function found in the other R files.

library(tidyverse)
source("assignment.R")
source("rules.R")
source("next_step.R")


params <- list()
#Parameters for the model
params[["b"]] <-c(0,0.13,1.3,1.65,1.21)*3.5
params[["mu"]] <- -log(c(0.449,0.453,0.512,0.538,0.256))
params[["alpha"]] <- log(2)/(6/12)-log(2)/4
params[["m"]] <- 4
params[["n"]] <- 10
params[["carrying"]] <- 210
params[["rho"]] <-0.9
params[["k"]] <- log(2)/(3/12)


#Number of replicates
replicates <- 100

#The data frame that will hold of our results
data <- tibble(
  p=numeric(),
  betac=numeric(),
  replicate=numeric(),
  extinction_t=numeric(),
  extinct=logical(),
  min_s=numeric(),
  final_s = numeric(),
  final_i = numeric()
)

#The main loop of this code. This loops through a number of 
#p values (removal rates) and effective contact rates (betac)
#for a given number of replicates, running a unique simulation each time.
#and storing the relevant results each time.
for(p in c((0:20)*(1/20),0.97,0.98,0.99)){
  params[["rho"]] <- p
  for(replicate in 1:replicates){
    for(betac in c(30,35,25)){
      #establish the initial settings of the simulation.
      #params is a list of the parameters of the simulation
      #state is a list of the different states of the system.
      params[["beta"]] <- matrix(c(0,0,0,0,0,
                                   0,0.602*betac,0.602*betac,0.602*betac,0.602*betac,
                                   0,0.602*betac,betac,betac,betac,
                                   0,0.602*betac,betac,betac,betac,
                                   0,0.602*betac,betac,betac,betac),ncol=5,byrow=TRUE)
      state <- list()
      state[["s"]] <- matrix(0,nrow=5,ncol=params[["m"]])
      state[["s"]][,1]<-round(c(0.63,0.23,0.085,0.035,0.0194)*params[["carrying"]]*0.5)
      
      state[["e"]] <- array(0,c(5,params[["m"]],params[["n"]]))
      
      state[["i"]] <- matrix(0,nrow=5,ncol=params[["m"]])
      state[["i"]][,1]<-1
      
      #The time of the simulation
      t <- 0
      min_s=1000
      
      #The simulation stops when either the time is over 40 years,
      #less than 5 individuals remain, 
      #or no infected individuals remain
      while(sum(unlist(state))>5 & t <= 40 & (sum(state[["i"]])>0 | sum(state[["e"]])>0)){
        next_vals <- next_step(state,params,t)
        state <- next_vals[[1]]
        t <- next_vals[[2]]
        min_s_trial <- sum(state[["s"]])
        if(min_s_trial<min_s){
          min_s <- min_s_trial
        }
      }
      
      #after the simulation finishes, store the results
      to_add <- tibble(
        p=p,
        replicate=replicate,
        betac=betac,
        extinction_t=t,
        extinct=(sum(unlist(state))<=5),
        disease_gone=(sum(state[["i"]])+sum(state[["e"]])<0),
        min_s=min_s,
        final_s=sum(state[["s"]]),
        final_i=sum(state[["i"]]+sum(state[["e"]]))
      )
      data <- rbind(data,to_add)
      print(c(p,replicate))
    }
  }
}
#write the results to a csv file.
write_csv(data,"age_results.csv")

