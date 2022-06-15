library(tidyverse)
library(openintro)


# Simple random sampling
# stratified random sampling
# Cluster random sampling
# ^-- financial data
# Multistage random sampling
# ^-- financial data

# load county data
data(county)
glimpse(county)

# unique values
unique(county$state)
length(unique(county$state))

# unique values in county data
sapply(county, function(x) length(unique(x)))

# remove "District of Columbia" as technically its not a state
county_noDC <- county %>% 
  filter(state != "District of Columbia") %>% 
  droplevels()

sapply(county_noDC, function(x) length(unique(x)))

# simple random sample of 150 counties
county_srs <- county_noDC %>% 
  sample_n(size = 150)

glimpse(county_srs)

# State distribution of srs counties
county_srs %>% 
  group_by(state) %>% 
  count()

## Stratified Sampling
county_str <- county_noDC %>% 
  group_by(state) %>% 
  sample_n(size = 3)

# State distribution of str counties
county_str %>% 
  group_by(state) %>% 
  count()

# Principles of Experimental Design ---------------------------------------

# Control
#       ^-- Compare treatment of interest to a control group
# Randomize
#         ^-- Randomly assign subjects to treatments.
# Replicate

