# Fit model
# Alec Chiono, alec.chiono@colorado.edu
source("scripts/01_wrangle.R")
librarian::shelf(rstanarm)
options(mc.cores=ifelse(parallel::detectCores()>4, 4, 2))

# Seed persistence model -------------------------------------------------------
if (file.exists("output/persistence_fit.RDS")) { #if model fit file already exists
  persistence_fit <- readRDS("output/persistence_fit.RDS") #read in model fit
} else { #if model fit file does not exist
  set.seed(359058) #set seed for reproducibility
  persistence_fit <- stan_glmer( #fit model
    cbind(num_persisting_seeds, num_germinated_seeds) ~ #"successes" and "failures"
      ecotype*cold_stratification*water_potential + #fixed effects
      (cold_stratification|site/seed_family) + #random intercepts for population structure  with random slopes for cold stratification
      (1|plate_pair/plate), #random intercepts for plate location
    data=germ_df,
    family=binomial
  )
  if (!file.exists("output")) dir.create(file.path("output")) #create output directory if necessary
  saveRDS(persistence_fit, file="output/persistence_fit.RDS") #save model fit
}

# Seed viability model ---------------------------------------------------------
if (file.exists("output/viability_fit.RDS")) { #if model fit file already exists
  viability_fit <- readRDS("output/viability_fit.RDS") #read in model fit
} else { #if model fit file does not exist
  set.seed(665259) #set seed for reproducibility
  viability_fit <- stan_glmer( #fit model
    cbind(num_viable_seeds, num_inviable_seeds) ~ #"successes" and "failures"
      ecotype*cold_stratification*water_potential + #fixed effects
      (cold_stratification|site/seed_family) + #random intercepts for population structure with random slopes for cold stratification
      (1|plate_pair/plate), #random intercepts for plate location
    data=germ_df,
    family=binomial,
    adapt_delta=.98, #increase adapt_delta reduce divergent transitions
    iter=2500 #increase iterations to ensure convergence
  )
  if (!file.exists("output")) dir.create(file.path("output")) #create output directory if necessary
  saveRDS(viability_fit, "output/viability_fit.RDS") #save model fit
}


# Timing of germination model --------------------------------------------------
if (file.exists("output/time_fit.RDS")) { #if model fit file already exists
  time_fit <- readRDS("output/time_fit.RDS") #read in model fit
} else { #if model fit file does not exist
  set.seed(250989830) #set seed for reproducibility
  time_fit <- stan_glmer( #fit model
    cbind(1, Day-1) ~ #"successes" and "failures"; model a geometric variable as a binomial variable with 1 success and then the number of failed attempts before there was a success; in this case, there were Day-1 days that the seed "failed" to germinate and then 1 day where it "succeeded" in germinating.
      cold_stratification*ecotype*water_potential + #fixed effects
      (cold_stratification|site/seed_family) + #random intercepts for population structure  with random slopes for cold stratification
      (1|plate_pair/plate/well), #random intercepts for plate location
    data=time_df,
    family=binomial,
    adapt_delta=.98 #increase adapt_delta reduce divergent transitions
  )
  if (!file.exists("output")) dir.create(file.path("output")) #create output directory if necessary
  saveRDS(time_fit, "output/time_fit.RDS") #save model fit 
}
