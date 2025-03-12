# Model diagnostics
# Alec Chiono, alec.chiono@colorado.edu
source("source/stan_diagnostics.R")
librarian::shelf("rmcelreath/rethinking")

persistence_fit <- readRDS("output/persistence_fit.RDS")
stan_diagnostics(persistence_fit) #make sure criteria for 5 common diagnostics are met
trankplot(persistence_fit$stanfit) #visually inspect all rankplots 

viability_fit <- readRDS("output/viability_fit.RDS")
stan_diagnostics(viability_fit) #make sure criteria for 5 common diagnostics are met
trankplot(viability_fit$stanfit) #visually inspect all rankplots 

time_fit <- readRDS("output/time_fit.RDS")
stan_diagnostics(time_fit) #make sure criteria for 5 common diagnostics are met
trankplot(time_fit$stanfit) #visually inspect all rankplots 
