# Wrangle data to prepare for analysis
# Alec Chiono, alec.chiono@colorado.edu
librarian::shelf(tidyverse)

germ_df <- read.csv("data/germ_df.csv") %>% 
  mutate(
    across(plate_pair:water_potential, as.factor), #turn categorical variables into factors
    num_persisting_seeds=num_tz_category1 + num_tz_category2, #number of viable seeds that didn't germinate
    num_viable_seeds=num_persisting_seeds + num_germinated_seeds, #total number of viable
    num_inviable_seeds=num_tz_category3 + num_tz_category4, #number of inviable seeds
    cold_stratification=fct_rev(cold_stratification), #for plotting later,
    site=factor(site, levels=c("S1", "S2", "S3", "I1", "I2", "I3")),
    ecotype=fct_rev(ecotype) #for plotting later
  )

time_df <- read.csv("data/time_df.csv") %>% 
  mutate(
    across(plate_pair:water_potential, as.factor), #turn categorical variables into factors
    Date=as.Date(Date), #convert to Date format
    cold_stratification=fct_rev(cold_stratification), #for plotting later
    site=factor(site, levels=c("S1", "S2", "S3", "I1", "I2", "I3")),
    ecotype=fct_rev(ecotype) #for plotting later
  )


