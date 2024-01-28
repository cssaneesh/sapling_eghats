# Packages----
rm(list = ls())
library(tidyverse)
library(viridis)
library(readr)
library(brms)
library(readxl)
library(dagitty)
library(patchwork)
library(bayesplot)

#read data----
rgr <- read.csv(file = 'rgr.csv')
names(rgr)


# individual disturbance to sapling (browsing, trampling, water stress)

# Calculate the percentage of occurrences for each combination of variables in the dataset 'rgr'

sap_status <- rgr %>% 
  group_by(
    village,        # group by village (ie. APA1, APA2 or APC1, APC 2)
    site,           # group by site
    Species,        # group by species
    sap_health,     # group by sapling health status
    disturbance,    # group by disturbance type
    Treatment       # group by treatment type
  ) %>% 
  summarise(
    Percentage = round(n() / nrow(rgr) * 100, 2),  # calculate the percentage of occurrences for each group
    .groups = 'drop'
  ) %>% 
  # to create binary variables indicating the presence of specific disturbances
  mutate(
    Browsing = if_else(disturbance == 'browsed', 1, 0),        # 1 if browsed, 0 otherwise
    Trampling = if_else(disturbance == 'trampled', 1, 0),      # 1 if trampled, 0 otherwise
    Wat.stress = if_else(disturbance == 'partly_dried', 1, 0), # 1 if partly dried (water stress), 0 otherwise
    Disturbance = if_else(disturbance == 'none', 1, 0)          # 1 if no disturbance, 0 otherwise
  ) %>% mutate(Treatment = fct_relevel(Treatment, c("Control", "CPFA", "CAFA")))
# :) reorder the levels of the 'Treatment' factor variable to Control, CPFA, CAFA, good for models and graph

# Grouping and Filtering to Identify Common Species with more than five individuals
com.species <- sap_status %>% 
  group_by(Treatment, Species) %>% 
  count() %>% filter(n >= 5) %>%  # more than five individuals
  count(Species, name = 'number') %>%  # gives the number of treatments species observed
  group_by(Species) %>% 
  summarise(sum = sum(number)) %>% filter(sum >= 3) %>%  # found in all three treatments
  select(Species) %>% # here, you can see saplings sp. with more than five individual
  left_join(sap_status, by = "Species") %>%  # then the freq sp is joined to sapstatus
  mutate(Treatment = fct_relevel(Treatment, c("Control", "CPFA", "CAFA")))

# View(com.species)
com.species %>% distinct(Species)

# number of sites for the analysis
com.species %>% distinct(site, Treatment) %>% group_by(Treatment) %>% summarise(sites=n())


# healthy common species-----
# rgr_com.species, make a df of healthy common species
# without observed disturbances, facilitating further analysis (fig 5=)focused on this subset.
# in all species 992, Common species without stress & disturbance = 176 (992-816= 176)
rgr_com.species <- rgr %>% filter(
  Species %in% c(
    'Acacia chundra',
    'Cassia fistula',
    'Chloroxylon swietenia',
    'Dalbergia paniculata',
    'Dolichandrone atrovirens',
    'Wrightia tinctoria'
  )
) %>%
  filter(disturbance == 'none') %>%  # Further filter for instances where there is no disturbance
  mutate(Treatment = fct_relevel(Treatment, c("Control", "CPFA", "CAFA")))

# number of sites with healthy common species
rgr_com.species %>% group_by(Treatment) %>% distinct(site) %>% summarise(sites=n())

