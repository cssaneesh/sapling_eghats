# Packages----
rm(list = ls())
library(tidyverse)
library(readr)

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
  unite('site', village, treatment, remove = FALSE) %>%
  separate(coodrdinates, c('lat', 'long'), sep = '([,])')

# location <- visit.01 %>% select(sci.name, lat, long, altitude, treatment)
# write.csv(location, 'location.csv')

# Visit02----
visit.02 <- read.csv(
  'Sapling_visit_02.csv',
  header = T,
  fill = T,
  sep = ",",
  na.strings = c("", " ", "NA", "NA ", "na", "NULL")
)

visit.02 <- visit.02 %>% rename(
  site.code = data.revisit.plants_info.site_code_tagged,
  tag.no = data.revisit.plants_info.tag_sapling,
  dead.alive = data.revisit.plants_info.deadalive,
  height = data.revisit.plants_info.height,
  rcd = data.revisit.plants_info.rcd,
  flame.height = data.revisit.plants_info.flame_h,
  type.resprout = data.revisit.plants_info.spot,
  disturbance = data.revisit.plants_info.disturbance
) %>%
  select(site.code,
         tag.no,
         dead.alive,
         height,
         rcd,
         disturbance,
         flame.height,
         type.resprout) %>%
  mutate(type.resprout = as.factor(type.resprout)) %>%
  separate(site.code, c('village', 'treatment'), sep = '([-])') %>%
  separate(tag.no, c('tag.no', 'sci.name'), sep = '([-])') %>%
  mutate(
    treatment = recode(
      treatment,
      'bodha_grass_removed_no_fire' = "CAFA",
      'all_burnt' = 'Control',
      'bodha_grass_present_no_fire' = "CPFA"
    )
  ) %>% unite('site', village, treatment, remove = FALSE) %>%
  mutate(
    dead.alive = as.factor(dead.alive),
    treatment = as.factor(treatment),
    village = as.factor(village)
  ) %>%  distinct() # ODK duplicates entries during network failures

# no of sites----
visit.01 %>% group_by(treatment, village) %>%
  distinct(treatment) %>% group_by(treatment) %>% count(treatment, name= 'sites')

# no of species in each treatment
visit.01 %>% group_by(sci.name, treatment) %>%
  distinct(sci.name) %>% group_by(treatment) %>% count(treatment, name='species')

# LUI----
lui <- read.csv("Plot_profile_01.csv")

site.lui <- lui %>% select(site, LUI)

sapling.lui <- left_join(visit.01, site.lui, "site")


# no of sites in visit 02
(visit.02 %>% group_by(treatment, village) %>%
    distinct(treatment) %>% group_by(treatment) %>% count(treatment))

# no of species in each treatment in visit 02
(visit.02 %>% group_by(sci.name, treatment) %>%
    distinct(sci.name) %>% group_by(treatment) %>% count(treatment))

# most common species visit 02
(visit.01 %>% group_by(sci.name, treatment) %>%
    count(sci.name, name = 'No.') %>% arrange(treatment, No.))


# saplings in each treatment and 
visit.01 %>% filter(treatment == 'Control') %>%  group_by(sci.name, site) %>%
  count(name = 'saplings') #%>% View()

visit.01 %>% filter(treatment == 'CAFA') %>%  group_by(sci.name, site) %>%
  count(name = 'saplings') #%>% View()

visit.01 %>% filter(treatment == 'CPFA') %>%  group_by(sci.name, site) %>%
  count(name = 'saplings') #%>% View()


ggplot(seedling.adul, aes(x= stage, y= tran.tot, fill= treatment))+
  geom_boxplot()+
  facet_wrap(~treatment)+
  ylim(0, 50)


ggplot(seedling.adul, aes(x= stage, y= tran.tot, fill= treatment))+
  geom_bar(stat = 'identity', position = 'dodge')+
  facet_wrap(~treatment)


ggplot(seedling.adul, aes(y= sci.name, x= tran.tot, fill= stage))+
  geom_bar(stat = 'identity', position = 'dodge')+
  facet_wrap(~treatment)

seedling.adul %>% ggplot(aes(x=stage, y= tran.tot, col= sci.name))+
  geom_jitter(width = 0.2)+
  ylim(0, 100)+
  facet_wrap(~treatment)

# seedling.adul <- seedling.adul %>% 
#   group_by(site) %>% 
#   mutate(row= row_number()) %>% 
#   tidyr:: pivot_wider(names_from = stage, values_from = tran.tot) %>% 
#   select(-row) %>% replace(is.na(.), 0)

# all treatment have similar 

seedling.adul%>% 
  filter(site== 'APA12_Control')# %>% View()

names(seedling.adul)

seedling.adul %>% 
  mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
  ggplot(aes(y=sci.name, x= tran.tot, fill= stage))+
  geom_bar( stat= 'identity')+
  facet_wrap(~treatment)

# plot adult trees and saplings
ggplot(data = saplingTrastot.lui, aes(y= tran.tot, x= LUI, fill= treatment))+
  geom_bar(stat='identity')+
  facet_wrap(~treatment)

# to order saplings in the presence of adult trees from high to low
saplingVsadult <- Tree.Sap %>% 
  mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
  ggplot(aes(y= sci.name, x= tran.tot, fill=stage))+
  geom_bar(stat='identity')+
  xlim(0, 300)+
  facet_wrap(~treatment)


# to see performance of one species across treatments
(saplingTrastot.lui %>% 
  filter(sci.name=='Acacia chundra') %>% #View()
  mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
  ggplot(aes(x= sci.name, y= tran.tot, fill= treatment))+
  geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Acacia leucophloea') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill= treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Albizia amara') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Annona squamosa') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Azadirachata indica') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Buchnania axillaris') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Butea monosperma') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Carissa carandas') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Cassia fistula') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Chloroxylon swietenia') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Dalbergia paniculata') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Diospyros montana') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Dolichandrone atrovirens') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Emblica officinalis') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Erythroxylum monogynum') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Ficus arnottiana') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Ficus tomentosa') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Gardenia gummifera') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Gyrocarpus americanus') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment)) # absent in control and cpfa

(saplingTrastot.lui %>% 
    filter(sci.name=='Hardwickia binata') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment)) # absent in control

(saplingTrastot.lui %>% 
    filter(sci.name=='Holoptelia intergrifolia') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment)) # absent in control and cpfa

(saplingTrastot.lui %>% 
    filter(sci.name=='Ixora parviflora') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Lannea coromandelica') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Morinda pubescens') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Morinda tinctoria') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Pongamia pinnata') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Senna siamea') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Tectona grandis') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment)) # planted saplings

(saplingTrastot.lui %>% 
    filter(sci.name=='Wrightia tinctoria') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))

(saplingTrastot.lui %>% 
    filter(sci.name=='Ziziphus mauritiana') %>% #View()
    mutate(sci.name=fct_reorder(sci.name, tran.tot, .fun= 'sum')) %>% 
    ggplot(aes(x= sci.name, y= tran.tot, fill=treatment))+
    geom_bar(stat='identity')+
    facet_wrap(~treatment))



# Saplings and LUI
(saplingVsLUI <- Tree.Sap %>%
  filter(stage == 'sapling') %>%
  mutate(sci.name = fct_reorder(sci.name, LUI, .fun = 'sum')) %>%
  ggplot() +
  geom_point(aes(
    y = sci.name,
    x = tran.tot,
    size = LUI,
    col = sci.name
  ), alpha = 0.5) +
  facet_wrap( ~ treatment))

# recruit type and treatment

visit.01 %>% mutate(recruit.type= as.factor(recruit.type)) %>% 
  filter(recruit.type!= "don't_know") %>% 
  group_by(site, treatment, sci.name) %>% 
  count(recruit.type, name = 'number') %>% 
  ggplot(aes(y= sci.name, x= number, fill= recruit.type))+
  geom_bar(stat = 'identity', position = 'dodge')+
  xlim(0,125)+
  facet_wrap(~treatment)

# we see more clonal offspring in control because they were affected by fire before the experiment.

ggsave('visit_01_RC_LUI.jpg',
       width = 10,
       height = 6,
       dpi = 300)

visit.02 %>% mutate(type.resprout= as.factor(type.resprout)) %>%
  filter(dead.alive== 'alive') %>% drop_na(type.resprout) %>%
  group_by(site, treatment, sci.name, type.resprout) %>% 
  count(type.resprout, name = 'number') %>% 
  ggplot(aes(y= sci.name, x= number, fill= type.resprout))+
  geom_bar(stat = 'identity')+
  xlim(0,125)+
  facet_wrap(~treatment)

ggsave('visit_02_RC_LUI.jpg',
       width = 10,
       height = 6,
       dpi = 300)

# seedling vs. adult trees bar plot
(seedlingVsAdult <- seedling.adul %>%
    mutate(sci.name=fct_reorder(sci.name, adult, .fun= 'sum')) %>% 
    ggplot()+
    geom_bar(aes(y= adult, fill=treatment))+
    geom_bar(aes(y= seedling, fill=treatment))+
    facet_wrap(~treatment))


# seedling vs. LUI

# Saplings and LUI
(seedlingVsLUI <- seedling.adul %>%
  filter(stage == 'sapling') %>%
  mutate(sci.name = fct_reorder(sci.name, LUI, .fun = 'sum')) %>%
  ggplot() +
  geom_point(aes(
    y = sci.name,
    x = tran.tot,
    size = LUI,
    col = sci.name
  ), alpha = 0.3) +
  facet_wrap( ~ treatment))

# plots----

seedlingVsAdult # number of seedling and adults
# this might be because of the difference in sample size (CAFA=22, Control= 11, CPFA= 11)

# seedling %>% group_by(treatment, site) %>%
#   distinct(site) %>% count(treatment) %>% group_by(treatment, n) %>% summarise(n_site=sum(n))

seedlingVsLUI # number of seedlings and LUI

saplingVsadult # number of saplings and adults, 
# this might be because of the difference in sample size (CAFA=28, Control= 13, CPFA= 14)
# our treatments were primarily expected to affect growth of the saplings and regeneration if there is a damage.

saplingVsLUI # number of saplings and LUI
# LUI was collected in 2020 but the saplings recruited through clonal re-sprout might be older than 1 year

