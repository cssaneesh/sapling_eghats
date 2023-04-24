# Packages----
rm(list = ls())
library(tidyverse)
library(readr)
library(brms)
library(readxl)

#read data
visit.01 <-
  read_excel("sapling_may_2020.xlsx")

visit.01 <- visit.01 %>% 
  select(-site) %>% 
  rename(
    sci.name = name,
    village = code,
    tag.no = tag,
    recruit.type= type_recruit
  ) %>%
  mutate(village = toupper(village)) %>% 
  mutate(
    treatment = recode(
      treatment,
      'bgrnf' = "CAFA",
      'ab' = 'Control',
      'bgpnf' = "CPFA"
    )
  ) %>% 
  unite('site', village:treatment, remove = F) %>% 
  select(
    site,
    village,
    sci.name,
    tag.no,
    treatment,
    recruit.type,
    height,
    rcd,
    disturbance
  ) %>%
  mutate(
    treatment = as.factor(treatment),
    village = as.factor(village),
    recruit.type = as.factor(recruit.type)
  ) %>%
  mutate(treatment= fct_relevel(treatment, 'Control', 'CPFA', 'CAFA')) %>% 
  arrange(treatment)


visit.02 <- read_excel("sapling_oct_2020.xlsx")  

visit.02 <- visit.02 %>% 
  rename(
    sci.name = name,
    village = code,
    tag.no = tag
  ) %>%
  mutate(
    treatment = recode(
      treatment,
      'bgrnf' = "CAFA",
      'ab' = 'Control',
      'bgpnf' = "CPFA"
    )
  ) %>% 
  unite('site', village:treatment, remove = F) %>% 
  select(
    site,
    village,
    sci.name,
    tag.no,
    treatment,
    height,
    rcd,
    disturbance
  ) %>%
  mutate(
    treatment = as.factor(treatment),
    village = as.factor(village),
    disturbance = as.factor(disturbance)
  ) %>%
  mutate(treatment= fct_relevel(treatment, 'Control', 'CPFA', 'CAFA')) %>% 
  arrange(treatment)

names(visit.01)
names(visit.02)

# LUI----
# add lui index to visits
site.lui <- read.csv("Plot_profile_01.csv")

site.lui <- site.lui %>% select(site, LUI)

visit_01.lui <- left_join(visit.01, site.lui, "site")
visit_02.lui <- left_join(visit.02, site.lui, "site")




# adult trees----
adult.dat <- read_csv("adult.dat.csv") 

names(adult.dat)
adult.dat <- adult.dat %>% 
  select(-seedling, -adu.stat) %>% 
  filter(sci.name!= 'Senna siamea') %>%  # introduced ornamental tree
  mutate(treatment= factor(treatment)) %>% 
  mutate(treatment= fct_relevel(treatment, c('Control', 'CPFA', 'CAFA'))) %>%
  arrange(treatment) %>% 
  group_by(site, treatment, sci.name, village) %>%
  summarise(abundance= sum(adult),.groups = 'drop') %>% # abundance of adult trees
  group_by(site, treatment, village) %>%
  summarise (Sp.adu = n_distinct(sci.name),
             # number of unique species/richness
             Nu.adu = sum(abundance),
             # total number of adult trees
             .groups = "drop")


# no of sites----
visit_01.lui %>% group_by(treatment, village) %>%
  distinct(treatment) %>% group_by(treatment) %>% count(treatment, name= 'sites')

visit_02.lui %>% group_by(treatment, village) %>%
    distinct(treatment) %>% group_by(treatment) %>% count(treatment, name = 'sites')

# no of species in each treatment
visit_01.lui %>% group_by(sci.name, treatment) %>%
  distinct(sci.name) %>% group_by(treatment) %>% count(treatment, name='species')

visit_02.lui %>% group_by(sci.name, treatment) %>%
    distinct(sci.name) %>% group_by(treatment) %>% count(treatment, name='species')

