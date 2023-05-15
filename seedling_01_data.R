# Packages----
rm(list = ls())
library(tidyverse)
library(readr)
library(brms)
library(tidybayes) # to make the qq plot of residuals of the model
library(patchwork)
library(iNEXT.3D)
library(viridis)
library(gt)
library(plotly)

# seedling----
seedling_raw <- read.csv("seedling.dat.csv") 
# adult presence as binary (yea/no)
# adult trees from the phonology study and visit to sites

# add lui to seedling adult tree data
lui <- read.csv("Plot_profile_01.csv")
site.lui <- lui %>% select(site, LUI)

seedling.dat <- seedling_raw %>% 
  left_join(site.lui, by = 'site') %>% 
  filter(sci.name!= 'Senna siamea') %>%  # introduced ornamental tree
  # filter(sci.name!= 'Pongamia pinnata') %>% # planted tree
  mutate(treatment= factor(treatment)) %>% 
  mutate(treatment= fct_relevel(treatment, c('Control', 'CPFA', 'CAFA'))) %>% 
  mutate(adu.stat= factor(adu.stat)) %>% 
  arrange(treatment) %>% 
  rename(Treatment = treatment)

seedling.dat.pre.ab <- seedling.dat %>% 
  filter(sci.name!= 'Senna siamea') %>%  # introduced ornamental tree
  filter(seedling>0) %>% 
  mutate(adult.pre= ifelse(adult!= 0, 'Yes', 'No')) %>% 
  mutate(seedling.pre= ifelse(seedling!= 0, 'Yes', 'No')) %>% 
  mutate(adult.pre= as.factor(adult.pre)) %>% 
  mutate(seedling.pre= as.factor(seedling.pre))


  
# no of sites----
seedling.dat %>% 
  filter(seedling > 0) %>% 
  group_by(Treatment, village) %>%
  distinct(Treatment) %>% group_by(Treatment) %>% 
  count(Treatment, name= 'sites')


# Q1:-----
# How does the abundance change for different Treatments in the presence of adult trees and LUI?

# overall density of seedling
seedling.dat %>% 
  filter(seedling >0) %>% 
  mutate(sci.name= fct_reorder(sci.name, seedling, .fun= 'sum')) %>% 
  ggplot(aes(x= seedling, y= sci.name, col= Treatment))+
  geom_point()

# density of seedling for each species vs Treatments
seedling.dat %>% 
filter(seedling >0) %>% 
  mutate(sci.name= fct_reorder(sci.name, seedling, .fun= 'sum')) %>% 
  ggplot(aes(x= seedling, y= sci.name, col= Treatment))+
  geom_point()+
  facet_wrap(~Treatment)

# find out number of species in each Treatment
seedling.dat %>%
  filter(seedling > 0) %>%
  group_by(Treatment, sci.name) %>%
  distinct(sci.name) %>%
  count(Treatment, name = 'n') %>%
  ungroup() %>%
  group_by(Treatment) %>%
  summarise(seedling.sp = sum(n)) %>%
  left_join(
    seedling.dat %>%
      filter(seedling > 0) %>%
      group_by(Treatment, village) %>%
      distinct(Treatment) %>% group_by(Treatment) %>%
      count(Treatment, name = 'sites')
  )

# density of seedling for each species across Treatments in the presence of LUI
seedling.dat %>%
  filter(seedling > 0) %>%
  mutate(sci.name = fct_reorder(sci.name, seedling, .fun = 'sum')) %>%
  ggplot(aes(y = seedling, x = LUI, col = sci.name)) +
  geom_point() +
  ylim(0, 100) +
  facet_wrap( ~ Treatment)

names(seedling.dat)  
# In the presence of LUI, how does abundance of seedling look like?
seedling.dat %>%
  filter(seedling > 0) %>%
  ggplot(aes(x = LUI, y = seedling, col = Treatment)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) # for all Treatments

# It seems like as LUI increases, number of seedling reduce even in the presence of adult trees.

# In the presence of LUI, how does abundance seedling look like
# in Treatments
seedling.dat %>%
  filter(seedling > 0) %>%
  ggplot(aes(x = LUI, y = seedling, col = Treatment)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  facet_wrap( ~ sci.name)

# abundance of seedling and adults together
seedling.dat %>%
  mutate(sci.name = fct_reorder(sci.name, seedling, .fun = 'sum')) %>%
  ggplot() +
  geom_bar(
    aes(y = sci.name, x = seedling),
    fill = 'blue',
    stat = 'identity',
    position = 'dodge'
  ) +
  geom_bar(
    aes(y = sci.name, x = adult),
    fill = 'red',
    stat = 'identity',
    position = 'dodge'
  ) +
  facet_wrap( ~ Treatment) 

# Number of adults in each Treatments
seedling.dat %>% 
  group_by(Treatment, sci.name) %>% 
  distinct(sci.name) %>% 
  group_by(Treatment) %>% 
  count(Treatment, name= 'adult (S)')

seedling.dat %>%
  ggplot() +
  geom_bar(aes(x = adult), stat = 'count')+
  facet_wrap(~Treatment)

seedling.dat %>%
  ggplot() +
  geom_boxplot(aes(y=adult, group= Treatment))




seedling.dat %>% filter(Treatment=='Control') %>% 
    summarise(tot.adult= sum(adult))

seedling.dat %>% filter(Treatment=='CPFA') %>% 
   summarise(tot.adult= sum(adult))

seedling.dat %>% filter(Treatment=='CAFA') %>% 
    summarise(tot.adult= sum(adult))


# Count species richness of adult across Treatments
seedling.dat %>% 
  filter(adult>0) %>% 
  group_by(Treatment, sci.name) %>% 
  distinct(sci.name) %>% 
  count(Treatment, name = 'no.adults') %>% 
  group_by (Treatment) %>% 
  summarise(adul.richness= sum(no.adults))


