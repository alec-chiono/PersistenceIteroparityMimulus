# Model predictions
# Alec Chiono, alec.chiono@colorado.edu
source("scripts/01_wrangle.R")
librarian::shelf(tidybayes)

# Predict for persistence model ------------------------------------------------
persistence_fit <- readRDS("output/persistence_fit.RDS")

## Marginal predictions (i.e. ignoring random effects)
persistence_marg_pred <- germ_df %>% 
  modelr::data_grid(ecotype, cold_stratification, water_potential) %>% #create data.frame with all combinations of fixed effects
  epred_draws(persistence_fit, ., re_formula=~0) %>% ungroup() %>% #make predictions
  rename(p=.epred) # #rename prediction variable

## Conditional predictions (e.g including random effects)
persistence_cond_pred_list <- list(NA) # create empty list
persistence_cond_pred_from <- germ_df %>% # create data.frame to predict from
  modelr::data_grid(ecotype, seed_family, plate, cold_stratification, water_potential) %>% #create data.frame with all combinations of fixed and random effects
  filter(paste(ecotype, seed_family) %in% with(germ_df, paste(ecotype, seed_family))) %>% #remove combinations of ecotype and seed family that don't exist
  mutate(
    site=as.factor(unname(sapply(as.character(seed_family), function(x) strsplit(x,"_")[[1]][1]))), #create site column from information in seed_family
    plate_pair=as.factor(unname(sapply(as.character(plate), function(x) strsplit(x,"_")[[1]][1]))) #create plate_pair column from information in plate
  )

for (i in 1:length(levels(germ_df$site))) { #predict for each site separately 
  persistence_cond_pred_list[[i]] <- persistence_cond_pred_from %>% #because otherwise memory
    filter(site==levels(germ_df$site)[i]) %>% #limit is reached
    epred_draws(persistence_fit, .) %>% ungroup() %>% 
    rename(p=.epred) %>% 
    group_by(ecotype, site, seed_family, cold_stratification, water_potential, .draw) %>% 
    summarize(p=mean(p), .groups="drop") #average over plate-pairs/plates to remove effect of location
}

persistence_cond_pred <- do.call(rbind.data.frame, persistence_cond_pred_list) %>%  #collapse list into one data.frame
  mutate(site=factor(site, levels=levels(germ_df$site))) #reorder levels of site to match germ_df
rm(persistence_cond_pred_list, persistence_cond_pred_from) #remove intermediate objects

# Predict for seed survival model ----------------------------------------------
viability_fit <- readRDS("output/viability_fit.RDS")

## Marginal predictions (i.e. ignoring random effects)
viability_pred <- germ_df %>% 
  modelr::data_grid(ecotype, cold_stratification, water_potential) %>% #create data.frame with all combinations of fixed effects
  epred_draws(viability_fit, ., re_formula=~0) %>% ungroup() %>% #make predictions
  rename(p=.epred) #rename prediction variable

# Predict for timing of germination model --------------------------------------
time_fit <- readRDS("output/time_fit.RDS")

## Marginal predictions (i.e. ignoring random effects)
time_pred <- time_df %>% 
  modelr::data_grid(ecotype, cold_stratification, water_potential) %>% #create data.frame with all combinations of fixed effects
  epred_draws(time_fit, ., re_formula=~0) %>% ungroup() %>% #make predictions
  mutate(.epred=1/.epred)  %>% #convert proportion to expected day of germination 
  rename(Day=.epred) #rename prediction variable
