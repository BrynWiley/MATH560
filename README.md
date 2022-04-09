# MATH560

This repository contains the R code for the stochastic simulations run in "A Stochastic Simulation of Tasmanian Devil Face Tumor
Epidemiology". 

The file "non_age_sim.R" contains all the code necessary to run the non-age-based simulation in the report. Running this script will take a significant amount of time, as it loops through a thousand replicates for multiple parameter combinations applied to unique simulations.

The results of this simulation used in the report can be found in "simple_results.csv".

The file "age_sim.R" is used to run the age-based simulation in the report. Again, running this script will take a significant amount of time, as it loops through many replicates and parameter combinations. This loop depends on functions from "assignment.R", which assigns the rates to different possible simulation events at a given time period, "rules.R", which given a chosen event applies that event to the population state, and "next_step.R", which handles the stochastic choice and application of an event to a population at a given time period.

The results of this age-based simulation used in the report can be found in "age_results.csv".
