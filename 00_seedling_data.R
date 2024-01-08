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
library(gridExtra)

# seedling----
seedling_raw <- read.csv("seedling.dat.csv") 
# adult presence as binary (yea/no)
# adult trees from the phonology study and visit to sites

# add lui to seedling adult tree data
site.lui <- read.csv("Plot_profile_01.csv")
names(site.lui)

site.lui <- site.lui %>% select(site, 
                                LUI, 
                                Goat, # relative number of goats
                                Trenches # relative area (m) of trenches 
)

# lui <- read.csv("Plot_profile_01.csv")
# site.lui <- lui %>% select(site, LUI)

seedling.dat <- seedling_raw %>% 
  left_join(site.lui, by = 'site') %>% 
  # filter(sci.name!= 'Senna siamea') %>%  # introduced ornamental tree
  # filter(sci.name!= 'Pongamia pinnata') %>% # planted tree
  filter(seedling>0) %>% # (APA15_CPFA & APA19_CPFA)
  mutate(treatment= factor(treatment)) %>% 
  mutate(treatment= fct_relevel(treatment, c('Control', 'CPFA', 'CAFA'))) %>% 
  mutate(adu.stat= factor(adu.stat)) %>% 
  arrange(treatment) %>% 
  rename(Treatment = treatment)

seedling.dat.pre.ab <- seedling.dat %>% 
  mutate(adult.pre= ifelse(adult!= 0, 'Yes', 'No')) %>% 
  mutate(seedling.pre= ifelse(seedling!= 0, 'Yes', 'No')) %>% 
  mutate(adult.pre= as.factor(adult.pre)) %>% 
  mutate(seedling.pre= as.factor(seedling.pre))


  
# no of sites----
seedling.dat %>% 
  group_by(Treatment, village) %>%
  distinct(Treatment) %>% group_by(Treatment) %>% 
  count(Treatment, name= 'sites')


# Q1:-----
# How does the abundance change for different Treatments in the presence of adult trees and LUI?

# overall density of seedling
seedling.dat %>% 
  mutate(sci.name= fct_reorder(sci.name, seedling, .fun= 'sum')) %>% 
  ggplot(aes(x= seedling, y= sci.name, col= Treatment))+
  geom_point()

# density of seedling for each species vs Treatments
seedling.dat %>% 
  mutate(sci.name= fct_reorder(sci.name, seedling, .fun= 'sum')) %>% 
  ggplot(aes(x= seedling, y= sci.name, col= Treatment))+
  geom_point()+
  facet_wrap(~Treatment)

# Mean number of seedling treatment wise in 100 m^2
seedling.dat %>% 
  group_by(Treatment) %>% 
  summarise(mean= round(mean(seedling)/80*100))

# find out number of species in each Treatment
seedling.dat %>%
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
  mutate(sci.name = fct_reorder(sci.name, seedling, .fun = 'sum')) %>%
  ggplot(aes(y = seedling, x = LUI, col = sci.name)) +
  geom_point() +
  ylim(0, 100) +
  facet_wrap( ~ Treatment)

names(seedling.dat)  

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

# number of adult trees in each treatment
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


seedling.dat %>%
  group_by(Treatment) %>%
  summarise(distinct_species = n_distinct(sci.name)) %>%
  arrange(-distinct_species)

seedling.dat %>%
  group_by(Treatment, sci.name) %>%
  summarise(occurrence = n()) %>%
  arrange(Treatment, -occurrence) %>% #View()
  slice(1) %>%   # Takes the first row of each group (highest occurrence due to previous arrangement)
  ungroup()

# Total number of seedlings in each treatment, specieswise.
seedling.dat %>%
  group_by(Treatment, sci.name) %>%
  summarise(total_seedlings = sum(seedling, na.rm = TRUE)) %>%
  arrange(Treatment, -total_seedlings) %>%
  ungroup() # %>% View()

# Functions----
# Create a function, which extracts legends from ggplot
extract_legend <- function(my_ggplot) {
  step1 <- ggplot_gtable(ggplot_build(my_ggplot))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

# my col function
mycol <- custom_theme <- function() {
  library(ggplot2)
  
  theme_set(
    theme_bw(base_size = 18) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour = "black", fill = "white"),
        legend.position = "bottom"
      )
  )
  
  scale_color_viridis <- function(discrete = TRUE, option = "D") {
    ggplot2::scale_color_viridis(discrete = discrete, option = option)
  }
  
  scale_fill_viridis <- function(discrete = TRUE, option = "D") {
    ggplot2::scale_fill_viridis(discrete = discrete, option = option)
  }
}
