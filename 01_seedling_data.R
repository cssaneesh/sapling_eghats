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

# seedling----
seedling_dat <- read_csv("seedling.dat.csv") 
# adult presence as binary (yea/no)
# adult trees from the phonology study and visit to sites

# add lui to seedling adult tree data
lui <- read.csv("Plot_profile_01.csv")
site.lui <- lui %>% select(site, LUI)

seedling.dat <- seedling_dat %>% 
  left_join(site.lui, by = 'site') %>% 
  mutate(treatment= factor(treatment)) %>% 
  mutate(treatment= fct_relevel(treatment, c('Control', 'CPFA', 'CAFA'))) %>% 
  mutate(adu.stat= factor(adu.stat)) %>% 
  arrange(treatment)
  
# no of sites----
seedling.dat %>% 
  filter(seedling >0) %>% 
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

# density of seedling for each species across treatments in the presence of LUI
seedling.dat %>% 
  filter(seedling >0) %>% 
  mutate(sci.name= fct_reorder(sci.name, seedling, .fun= 'sum')) %>%
  ggplot(aes(y= seedling, x= LUI, col=sci.name))+
  geom_point()+
  ylim(0,100)+
facet_wrap(~treatment)

names(seedling.dat)  
# In the presence of LUI, how does abundance of seedling look like?
seedling.dat %>% 
  filter(seedling >0) %>% 
  ggplot(aes(x=LUI, y= seedling, col= treatment))+
  geom_point()+
  geom_smooth(method = 'lm', se = F) # for all treatments

# It seems like as LUI increases, number of seedling reduce even in the presence of adult trees.

# In the presence of LUI, how does abundance seedling look like
# in treatments
seedling.dat %>% 
  filter(seedling >0) %>% 
  ggplot(aes(x=LUI, y= seedling, col= treatment))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)+
  facet_wrap(~sci.name)

# abundance of seedling and adults together
seedling.dat %>%
  mutate(sci.name= fct_reorder(sci.name, seedling, .fun= 'sum')) %>% 
  ggplot() +
  geom_bar(aes(y = sci.name, x = seedling), fill= 'blue',
           stat = 'identity',
           position = 'dodge') +
  geom_bar(aes(y = sci.name, x = adult), fill= 'red',
           stat = 'identity',
           position = 'dodge') +
  facet_wrap(~ treatment) 

# Number of adults in each treatments
seedling.dat %>%
  ggplot() +
  geom_bar(aes(x = adu.stat), stat = 'count')+
  facet_wrap(~treatment)



seedling.dat %>% filter(treatment=='Control') %>% 
  count(adu.stat)

seedling.dat %>% filter(treatment=='CPFA') %>% 
  count(adu.stat)

seedling.dat %>% filter(treatment=='CAFA') %>% 
  count(adu.stat)



# Count species richness of adult across treatments
seedling.dat %>% 
  filter(adult>0) %>% 
  group_by(treatment, sci.name) %>% 
  distinct(sci.name) %>%  
  count(treatment, name = 'no.sites') %>% 
  group_by (treatment) %>% 
  summarise(tot.sp= sum(no.sites))

# species richness of adults # cafa=26, cpfa= 25 and control= 21
