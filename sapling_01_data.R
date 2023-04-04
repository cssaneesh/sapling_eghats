# Packages----
rm(list = ls())
library(tidyverse)
library(readr)
library(brms)

# Visit01----
visit.01 <- read.csv(
  'Sapling_visit_01.csv',
  header = T,
  fill = T,
  sep = ",",
  na.strings = c("", " ", "NA", "NA ", "na", "NULL")
)

visit.01 <- visit.01 %>% rename(
  sci.name = sapling_name,
  village = site_code,
  tag.no = sapling_tag_no,
  height = sapling_height..cm.,
  rcd = sapling_rcd..cm.,
  recruit.type = type_recruit,
  coodrdinates = sapling_coordinate,
  altitude = sapling_altitude,
  disturbance = sapling_disturbance
) %>%
  select(
    village,
    sci.name,
    tag.no,
    treatment,
    recruit.type,
    height,
    rcd,
    coodrdinates,
    altitude,
    disturbance
  ) %>%
  mutate(
    treatment = recode(
      treatment,
      'bodha_grass_removed_no_fire' = "CAFA",
      'all_burnt' = 'Control',
      'bodha_grass_present_no_fire' = "CPFA"
    )
  ) %>%
  mutate(
    treatment = as.factor(treatment),
    village = as.factor(village),
    recruit.type = as.factor(recruit.type)
  ) %>%
  mutate(treatment= fct_relevel(treatment, 'Control', 'CPFA', 'CAFA')) %>% 
  arrange(treatment) %>% 
  unite('site', village, treatment, remove = FALSE) %>%
  separate(coodrdinates, c('lat', 'long'), sep = '([,])')

# location <- visit.01 %>% select(sci.name, lat, long, altitude, treatment)
# write.csv(location, 'location.csv')

# Visit02----
visit.02 <- read_csv("Sapling_visit_02.csv", 
                     col_types = cols(tag.no = col_number()))
  
  
  read.csv(
  'Sapling_visit_02.csv',
  header = T,
  fill = T,
  sep = ",",
  na.strings = c("", " ", "NA", "NA ", "na", "NULL")
)


visit_02 <- read_csv("visit.02.csv", col_types = cols(tag.no = col_number()))

names(visit_02)
visit.02 <- visit_02 %>% 
#   rename(
#   site.code = `data-revisit-plants_info-site_code_tagged`,
#   dead.alive = `data-revisit-plants_info-deadalive`,
#   height = `data-revisit-plants_info-height`,
#   rcd = `data-revisit-plants_info-rcd`,
#   flame.height = `data-revisit-plants_info-flame_h`,
#   type.resprout = `data-revisit-plants_info-spot`,
#   disturbance = `data-revisit-plants_info-disturbance`
# ) %>%
  select(site,
         village,
         treatment,
         tag.no,
         sci.name,
         dead.alive,
         height,
         rcd,
         disturbance,
         flame.height,
         type.resprout) %>%
  mutate(type.resprout = as.factor(type.resprout)) %>%
  mutate(tag.no=as.numeric(tag.no)) %>% 
  # separate(site.code, c('village', 'treatment'), sep = '([-])') %>%
  # mutate(
  #   treatment = recode(
  #     treatment,
  #     'bodha_grass_removed_no_fire' = "CAFA",
  #     'all_burnt' = 'Control',
  #     'bodha_grass_present_no_fire' = "CPFA"
  #   )
  # ) %>% unite('site', village, treatment, remove = FALSE) %>%
  mutate(
    dead.alive = as.factor(dead.alive),
    treatment = as.factor(treatment),
    village = as.factor(village)
  ) %>%
  mutate(treatment= fct_relevel(treatment, 'Control', 'CPFA', 'CAFA')) %>% 
  arrange(treatment) %>% distinct() # ODK duplicates entries during network failures

# LUI----
# add lui index to visits
site.lui <- read.csv("Plot_profile_01.csv")

site.lui <- site.lui %>% select(site, LUI)

visit_01.lui <- left_join(visit.01, site.lui, "site")
visit_02.lui <- left_join(visit.02, site.lui, "site")

head(visit_01.lui)
tail(visit_01.lui)
str(visit_02.lui)

# adult trees----
adult_raw <- read_csv("adult.dat.csv") 

names(adult_raw)
adult.dat <- adult_raw %>% 
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

# most common species
visit_01.lui %>% group_by(sci.name, treatment) %>%
    count(sci.name, name = 'No.') %>% arrange(treatment, No.)

visit_02.lui %>% group_by(sci.name, treatment) %>%
  count(sci.name, name = 'No.') %>% arrange(treatment, No.)


# saplings in each treatment and 
visit_01.lui %>% filter(treatment == 'Control') %>%  group_by(sci.name, site) %>%
  count(name = 'saplings') #%>% View()

visit_01.lui %>% filter(treatment == 'CAFA') %>%  group_by(sci.name, site) %>%
  count(name = 'saplings') #%>% View()

visit_01.lui %>% filter(treatment == 'CPFA') %>%  group_by(sci.name, site) %>%
  count(name = 'saplings') #%>% View()

visit_02.lui %>% filter(treatment == 'Control') %>%  group_by(sci.name, site) %>%
  count(name = 'saplings') #%>% View()

visit_02.lui %>% filter(treatment == 'CAFA') %>%  group_by(sci.name, site) %>%
  count(name = 'saplings') #%>% View()

visit_02.lui %>% filter(treatment == 'CPFA') %>%  group_by(sci.name, site) %>%
  count(name = 'saplings') #%>% View()

# recruit type and treatment
visit_01.lui %>% mutate(recruit.type= as.factor(recruit.type)) %>% 
  filter(recruit.type!= "don't_know") %>% 
  group_by(site, treatment, sci.name) %>% 
  count(recruit.type, name = 'number') %>% 
  ggplot(aes(y= sci.name, x= number, fill= recruit.type))+
  geom_bar(stat = 'identity', position = 'dodge')+
  xlim(0,125)+
  facet_wrap(~treatment)

# we see more clonal offspring in control because they were affected by fire before the experiment.

# species abundance of saplings in treatments visit01
visit_01.lui %>% mutate(recruit.type= as.factor(recruit.type)) %>% 
  group_by(treatment) %>% 
  count(sci.name) %>% 
  count(treatment)

visit_01.lui %>% mutate(recruit.type= as.factor(recruit.type)) %>% 
  group_by(treatment) %>% 
  count(sci.name) %>% 
  count(treatment, name= 'species') %>%
  ggplot()+
  geom_bar(aes(x=treatment, y= species), stat = 'identity')# lower diversity in control in visit01

# species abundance of saplings in treatments visit02
visit_02.lui %>% 
  group_by(treatment) %>% 
  count(sci.name) %>% 
  count(treatment)

visit_02.lui %>% 
  group_by(treatment) %>% 
  count(sci.name) %>% 
  count(treatment, name= 'species') %>%
  ggplot()+
  geom_bar(aes(x=treatment, y= species), stat = 'identity')


# species richness in the first and second visits
visit_01.lui %>% mutate(recruit.type= as.factor(recruit.type)) %>% # visit01
  group_by(treatment) %>% 
  count(sci.name) %>% 
  count(treatment, name= 'speciesVisit01') %>%
  ggplot()+
  geom_bar(aes(x=treatment, y= speciesVisit01), stat = 'identity')+
  visit_02.lui %>% # visit02
  group_by(treatment) %>% 
  count(sci.name) %>% 
  count(treatment, name= 'speciesVisit02') %>%
  ggplot()+
  geom_bar(aes(x=treatment, y= speciesVisit02), stat = 'identity')


# recruit type in visit01
visit_01.lui %>% mutate(recruit.type= as.factor(recruit.type)) %>% # visit01
  group_by(treatment, recruit.type) %>% 
  count(sci.name) %>% 
  ggplot()+
  geom_bar(aes(y=recruit.type, x= n, fill= recruit.type), stat = 'identity')+
  facet_wrap(~treatment)+ # shows the impact of saplings as they were seedlings
guides(fill='none') # remove the legend of color

visit_01.lui %>% mutate(recruit.type= as.factor(recruit.type)) %>% # visit01
  group_by(treatment, recruit.type) %>% 
  count(sci.name, name = 'Number of seedling') %>% 
  ggplot()+
  geom_bar(aes(y=sci.name, x= `Number of seedling`, fill= recruit.type), stat = 'identity')+
  facet_wrap(~treatment)

# types of resprouts visit02
visit_02.lui %>% mutate(type.resprout= as.factor(type.resprout)) %>%
  filter(dead.alive== 'alive') %>% drop_na(type.resprout) %>%
  group_by(site, treatment, sci.name, type.resprout) %>% 
  count(type.resprout, name = 'number') %>% 
  ggplot(aes(y= sci.name, x= number, fill= type.resprout))+
  geom_bar(stat = 'identity')+
  xlim(0,125)+
  facet_wrap(~treatment) 
# axillary= leaf base, epicormic= bark of the stem and collar= from the collar

