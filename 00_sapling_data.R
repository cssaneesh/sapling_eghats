# Packages----
rm(list = ls())
library(tidyverse)
library(viridis)
library(readr)
library(brms)
library(readxl)
library(dagitty)
library(patchwork)

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

# number of saplings
visit.01 %>% group_by(Treatment) %>% 
  summarise(numb_saplings= round(n()/2000*100))


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

# for rgr----
# first visit= time0 in May and second visit= time1 in October 
time0 <- visit_01.lui %>%
  select(Treatment,
         village,
         site,
         sci.name,
         tag.no,
         recruit.type,
         height, # height in cm, 
         rcd, # root collar diameter in cm,
         Goat,
         Trenches,
         # LUI
  ) %>%
  group_by(Treatment, site, sci.name) %>%
  rename(h0 = height) %>%
  rename(rcd0 = rcd)

time1 <- visit_02.lui %>%
  select(Treatment,
         village,
         site,
         sci.name,
         tag.no,
         height,
         rcd,
         disturbance,
         resprout
  ) %>%
  mutate(disturbance = replace_na(disturbance, 'none')) %>%
  mutate(disturbance = as.factor(disturbance)) %>%
  group_by(Treatment, village, site, sci.name) %>%
  rename(h1 = height) %>%
  rename(rcd1 = rcd)

names(time0)
names(time1)

t1t0 <- time0 %>% left_join(time1, multiple = 'all') %>%
  rename(sap.status= disturbance) %>% 
  # filter(disturbance != 'dead') %>%
  # filter(disturbance != 'partly_dried') %>% 
  # filter(recruit.type != 'unknown') %>%
  filter(rcd1 > 0) %>% # omit plant with 0 growth (dead)
  filter(h1 > 0) %>%
  mutate(sap.status = fct_relevel(
    sap.status,
    c('none',
      'browsed',
      'dead',
      'partly_dried', 
      'trampled')
  )) %>%
  arrange(sap.status) %>%
  mutate(recruit.type = fct_relevel(recruit.type, c(
    'seedling recruitment',
    'clonal offspring'
  ))) %>%
  arrange(recruit.type)

head(t1t0, 4)
tail(t1t0, 4)

# data for RCD---- 
rgr <- t1t0 %>% select(Treatment, 
                       village, 
                       site, 
                       sci.name, 
                       tag.no,
                       recruit.type, 
                       Goat,
                       Trenches,
                       sap.status,
                       resprout,
                       h0, 
                       h1,
                       rcd0, 
                       rcd1
) %>% 
  mutate(rgrH = log(h1) - log(h0)) %>%  # height in cm
  mutate(rgr_rcd = log(rcd1) - log(rcd0)) %>%  # rcd (RCD= Root collar diameter in cm)
  select(Treatment, 
         village, 
         site,
         sci.name, 
         # tag.no,
         # recruit.type, 
         Goat,
         Trenches,
         sap.status,
         resprout,
         # h0, 
         # h1,
         # rcd0, 
         # rcd1,
         rgrH,
         rgr_rcd
  ) %>% 
  mutate(sap_health= case_when(
    sap.status== 'none' ~ 1,
    sap.status== 'trampled' ~ 2,
    sap.status== 'browsed' ~ 3,
    sap.status== 'partly_dried' ~ 4
  ))

# examine how browsing and trampling is influenced by the number of livestock
sap_status <- rgr %>% 
  group_by(village, 
           site, 
           sci.name,
           sap_health,
           sap.status,
           Treatment, 
           Goat, 
           Trenches
  ) %>% 
  summarise(Percentage= round(n()/ nrow(rgr) * 100, 2),.groups = 'drop'
  ) %>% 
  mutate(Browsing= if_else(sap.status== 'browsed', 1, 0)) %>% 
  mutate(Trampling= if_else(sap.status== 'trampled', 1, 0)) %>% 
  mutate(Wat.stress= if_else(sap.status== 'partly_dried', 1, 0)) %>% 
  mutate(None= if_else(sap.status== 'none', 1, 0))

(freq.sp.list <- sap_status %>% 
    group_by(Treatment, sci.name) %>% 
    count() %>% filter (n >= 5) %>% # more than five individual
    count(sci.name, name = 'number') %>% # gives the number of treatments species observed
    group_by(sci.name) %>% 
    summarise(sum= sum(number)) %>% filter (sum >= 3) %>%  # found in all three treatments
    select(sci.name))

com.species  <- left_join(freq.sp.list, sap_status, multiple= 'all')

# View(com.species)

com.species %>% distinct(site, Treatment) %>% group_by(Treatment) %>% summarise(sites=n())
