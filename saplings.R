# Packages----
rm(list = ls())
library(tidyverse)
# Data----
# Visit01----
visit.01 <- read.csv('Sapling_visit_01.csv',
                     header = T,
                    fill = T,
                    sep = ",",
                    na.strings = c("", " ", "NA", "NA ", "na", "NULL"))

visit.01 <- visit.01 %>% rename(
  sci.name = sapling_name,
  village = site_code,
  tag.no = sapling_tag_no,
  height = sapling_height..cm.,
  rcd = sapling_rcd..cm.,
  recruit.type = type_recruit,
  coodrdinates=sapling_coordinate,
  altitude= sapling_altitude,
  disturbance=sapling_disturbance
) %>%
  select(village, sci.name, tag.no, treatment, 
         recruit.type, height, rcd, coodrdinates, altitude, disturbance) %>%
  mutate(
    treatment = recode(
      treatment,
      'bodha_grass_removed_no_fire' = "CAFA",
      'all_burnt' = 'Control',
      'bodha_grass_present_no_fire' = "CPFA"
    )
  ) %>%
  mutate(treatment = as.factor(treatment),
         recruit.type = as.factor(recruit.type)) %>%
  unite('site', village, treatment, remove = FALSE) %>% 
  separate(coodrdinates, c('lat','long'), sep= '([,])')

# location <- visit.01 %>% select(sci.name, lat, long, altitude, treatment)
# write.csv(location, 'location.csv')

# no of sites
(sites.01 <- visit.01 %>% group_by(treatment, village) %>% 
    distinct(treatment) %>% group_by(treatment) %>% count(treatment))

# no of species in each treatment
(sp.01 <- visit.01 %>% group_by(sci.name, treatment) %>% 
distinct(sci.name) %>% group_by(treatment) %>% count(treatment))

# most common species
(com.sp.01 <- visit.01 %>% group_by(sci.name, treatment) %>% 
  count(sci.name) %>% arrange(desc(n)) # %>% View()
)

# Visit02----
visit.02 <- read.csv('Sapling_visit_02.csv',
                     header = T,
                     fill = T,
                     sep = ",",
                     na.strings = c("", " ", "NA", "NA ", "na", "NULL"))

visit.02 <- visit.02 %>% rename(
  site.code=data.revisit.plants_info.site_code_tagged,
  tag.no = data.revisit.plants_info.tag_sapling,
  dead.alive=data.revisit.plants_info.deadalive,
  height = data.revisit.plants_info.height,
  rcd = data.revisit.plants_info.rcd,
  flame.height= data.revisit.plants_info.flame_h,
  type.resprout=data.revisit.plants_info.spot,
  disturbance=data.revisit.plants_info.disturbance
) %>%
  select(site.code, tag.no, dead.alive, height,  rcd, disturbance, flame.height, type.resprout) %>%
  mutate(type.resprout = as.factor(type.resprout)) %>% 
  separate(site.code, c('village', 'treatment'), sep = '([-])') %>% 
  separate(tag.no, c('tag.no', 'sci.name'),sep = '([-])') %>% 
  mutate(
    treatment = recode(
      treatment,
      'bodha_grass_removed_no_fire' = "CAFA",
      'all_burnt' = 'Control',
      'bodha_grass_present_no_fire' = "CPFA"
    )
  ) %>% unite('site', village, treatment, remove = FALSE) %>% 
  mutate(dead.alive= as.factor(dead.alive),
         treatment= as.factor(treatment),
         ) %>%  distinct() # ODK duplicates entries during network failures


# no of sites
(sites.02 <- visit.02 %>% group_by(treatment, village) %>% 
  distinct(treatment) %>% group_by(treatment) %>% count(treatment))

# no of species in each treatment
(sp.02 <- visit.02 %>% group_by(sci.name, treatment) %>% 
  distinct(sci.name) %>% group_by(treatment) %>% count(treatment))

# most common species
(com.sp.02 <- visit.01 %>% group_by(sci.name, treatment) %>% 
  count(sci.name) %>% arrange(desc(n)) # %>% View()
)
# no of sites
sites.01
sites.02
  
# no of species in each treatment
sp.01
sp.02
    
# most common species
com.sp.01
com.sp.02


