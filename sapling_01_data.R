# Packages----
rm(list = ls())
library(tidyverse)
library(viridis)
library(readr)
library(brms)
library(readxl)
library(dagitty)

#read data
visit.01 <-
  read_excel("sapling_may_2020.xlsx")

visit.01 <- visit.01 %>% 
  select(-site) %>% 
  rename(
    sci.name = name,
    village = code,
    tag.no = tag,
    recruit.type= type_recruit,
    Treatment= treatment
  ) %>%
  mutate(village = toupper(village)) %>% 
  mutate(
    Treatment = recode(
      Treatment,
      'bgrnf' = "CAFA",
      'ab' = 'Control',
      'bgpnf' = "CPFA"
    )
  ) %>% 
  unite('site', village:Treatment, remove = F) %>% 
  select(
    site,
    village,
    sci.name,
    tag.no,
    Treatment,
    recruit.type,
    height,
    rcd,
    disturbance
  ) %>%
  mutate(
    Treatment = as.factor(Treatment),
    village = as.factor(village),
    recruit.type = as.factor(recruit.type)
  ) %>%
  mutate(Treatment= fct_relevel(Treatment, 'Control', 'CPFA', 'CAFA')) %>% 
  arrange(Treatment)


visit.02 <- read_excel("sapling_oct_2020.xlsx", 
                       sheet = "Sheet1")  

visit.02 <- visit.02 %>% 
  rename(
    sci.name = name,
    village = code,
    tag.no = tag,
    Treatment= treatment
  ) %>%
  mutate(
    Treatment = recode(
      Treatment,
      'bgrnf' = "CAFA",
      'ab' = 'Control',
      'bgpnf' = "CPFA"
    )
  ) %>% 
  unite('site', village:Treatment, remove = F) %>% 
  select(
    site,
    village,
    sci.name,
    tag.no,
    Treatment,
    height,
    rcd,
    disturbance,
    resprout
  ) %>%
  mutate(
    Treatment = as.factor(Treatment),
    village = as.factor(village),
    disturbance = as.factor(disturbance),
    resprout= as.factor(resprout)
  ) %>%
  mutate(Treatment= fct_relevel(Treatment, 'Control', 'CPFA', 'CAFA')) %>% 
  arrange(Treatment)

names(visit.01)
names(visit.02)

# LUI----
# add lui index to visits
site.lui <- read.csv("Plot_profile_01.csv")
names(site.lui)

site.lui <- site.lui %>% select(site, 
                                LUI, 
                                Goat, # relative number of goats
                                Trenches # relative area (m) of trenches 
                                )

visit_01.lui <- left_join(visit.01, site.lui, "site")
visit_02.lui <- left_join(visit.02, site.lui, "site")




# adult trees----
adult.dat <- read.csv("adult.dat.csv") 

names(adult.dat)
adult.dat <- adult.dat %>% 
  select(-seedling, -adu.stat) %>% 
  rename(Treatment= treatment) %>% 
  filter(sci.name!= 'Senna siamea') %>%  # introduced ornamental tree
  mutate(Treatment= factor(Treatment)) %>% 
  mutate(Treatment= fct_relevel(Treatment, c('Control', 'CPFA', 'CAFA'))) %>%
  arrange(Treatment) %>% 
  group_by(site, Treatment, sci.name, village) %>%
  summarise(abundance= sum(adult),.groups = 'drop') %>% # abundance of adult trees
  group_by(site, Treatment, village) %>%
  summarise (Sp.adu = n_distinct(sci.name),
             # number of unique species/richness
             Nu.adu = sum(abundance),
             # total number of adult trees
             .groups = "drop")


# no of sites----
visit_01.lui %>% group_by(Treatment, village) %>%
  distinct(Treatment) %>% group_by(Treatment) %>% count(Treatment, name= 'sites')

visit_02.lui %>% group_by(Treatment, village) %>%
    distinct(Treatment) %>% group_by(Treatment) %>% count(Treatment, name = 'sites')

# no of species in each Treatment
visit_01.lui %>% group_by(sci.name, Treatment) %>%
  distinct(sci.name) %>% group_by(Treatment) %>% count(Treatment, name='species')

visit_02.lui %>% group_by(sci.name, Treatment) %>%
    distinct(sci.name) %>% group_by(Treatment) %>% count(Treatment, name='species')

