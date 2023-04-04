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
seedling_raw <- read_csv("seedling.dat.csv") 
# adult presence as binary (yea/no)
# adult trees from the phonology study and visit to sites

# add lui to seedling adult tree data
lui <- read.csv("Plot_profile_01.csv")
site.lui <- lui %>% select(site, LUI)

seedling.dat <- seedling_raw %>% 
  left_join(site.lui, by = 'site') %>% 
  filter(sci.name!= 'Senna siamea') %>%  # introduced ornamental tree
  mutate(treatment= factor(treatment)) %>% 
  mutate(treatment= fct_relevel(treatment, c('Control', 'CPFA', 'CAFA'))) %>% 
  mutate(adu.stat= factor(adu.stat)) %>% 
  arrange(treatment)

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
  group_by(treatment, village) %>%
  distinct(treatment) %>% group_by(treatment) %>% 
  count(treatment, name= 'sites')


# Q1:-----
# How does the abundance change for different treatments in the presence of adult trees and LUI?

# overall density of seedling
seedling.dat %>% 
  filter(seedling >0) %>% 
  mutate(sci.name= fct_reorder(sci.name, seedling, .fun= 'sum')) %>% 
  ggplot(aes(x= seedling, y= sci.name, col= treatment))+
  geom_point()

# density of seedling for each species vs treatments
seedling.dat %>% 
filter(seedling >0) %>% 
  mutate(sci.name= fct_reorder(sci.name, seedling, .fun= 'sum')) %>% 
  ggplot(aes(x= seedling, y= sci.name, col= treatment))+
  geom_point()+
  facet_wrap(~treatment)

# find out number of species in each treatment
seedling.dat %>%
  filter(seedling > 0) %>%
  group_by(treatment, sci.name) %>%
  distinct(sci.name) %>%
  count(treatment, name = 'n') %>%
  ungroup() %>%
  group_by(treatment) %>%
  summarise(seedling.sp = sum(n)) %>%
  left_join(
    seedling.dat %>%
      filter(seedling > 0) %>%
      group_by(treatment, village) %>%
      distinct(treatment) %>% group_by(treatment) %>%
      count(treatment, name = 'sites')
  )

# density of seedling for each species across treatments in the presence of LUI
seedling.dat %>%
  filter(seedling > 0) %>%
  mutate(sci.name = fct_reorder(sci.name, seedling, .fun = 'sum')) %>%
  ggplot(aes(y = seedling, x = LUI, col = sci.name)) +
  geom_point() +
  ylim(0, 100) +
  facet_wrap( ~ treatment)

names(seedling.dat)  
# In the presence of LUI, how does abundance of seedling look like?
seedling.dat %>%
  filter(seedling > 0) %>%
  ggplot(aes(x = LUI, y = seedling, col = treatment)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) # for all treatments

# It seems like as LUI increases, number of seedling reduce even in the presence of adult trees.

# In the presence of LUI, how does abundance seedling look like
# in treatments
seedling.dat %>%
  filter(seedling > 0) %>%
  ggplot(aes(x = LUI, y = seedling, col = treatment)) +
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
  facet_wrap( ~ treatment) 

# Number of adults in each treatments
seedling.dat %>% 
  group_by(treatment, sci.name) %>% 
  distinct(sci.name) %>% 
  group_by(treatment) %>% 
  count(treatment, name= 'adult (S)')

seedling.dat %>%
  ggplot() +
  geom_bar(aes(x = adult), stat = 'count')+
  facet_wrap(~treatment)

seedling.dat %>%
  ggplot() +
  geom_boxplot(aes(y=adult, group= treatment))




seedling.dat %>% filter(treatment=='Control') %>% 
    summarise(tot.adult= sum(adult))

seedling.dat %>% filter(treatment=='CPFA') %>% 
   summarise(tot.adult= sum(adult))

seedling.dat %>% filter(treatment=='CAFA') %>% 
    summarise(tot.adult= sum(adult))


# Count species richness of adult across treatments
seedling.dat %>% 
  filter(adult>0) %>% 
  group_by(treatment, sci.name) %>% 
  distinct(sci.name) %>% 
  count(treatment, name = 'no.adults') %>% 
  group_by (treatment) %>% 
  summarise(adul.richness= sum(no.adults))


