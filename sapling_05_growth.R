

source('sapling_01_data.R')
# growth----

time0 <- visit_01.lui %>% 
  select(treatment, site, sci.name, tag.no, height, rcd, LUI) %>% 
  group_by(treatment, site, sci.name) %>% 
  rename(h0= height) %>% 
  rename(rcd0= rcd)

time1 <- visit_02.lui %>% 
  select(treatment, 
         village, 
         site, 
         sci.name,
         tag.no,
         height, 
         rcd,
         dead.alive, 
         disturbance, 
         type.resprout) %>% 
  mutate(dead.alive= as.factor(dead.alive)) %>% 
  filter(dead.alive != 'dead') %>%  # removed dead plants
  mutate(disturbance= replace_na(disturbance, 'none')) %>% 
  mutate(disturbance= as.factor(disturbance)) %>% 
  group_by(treatment, village, site, sci.name)%>% 
  rename(h1= height) %>% 
  rename(rcd1= rcd)
  
t1t0 <- time1 %>% left_join(time0, by = c('tag.no', 'sci.name', 'treatment', 'site'))

changet0t1 <- t1t0 %>% 
  mutate(rgr= (rcd1-rcd0)/rcd0)

changet0t1 <- t1t0 %>% 
  mutate(rgr= log(rcd1)-log(rcd0))
  
ggplot(changet0t1, aes(x= treatment, y= rgr))+
  geom_boxplot()+
  ylim(0,50)+
  facet_wrap(~disturbance)

ggplot(changet0t1, aes(x= treatment, y= rgr))+
  geom_boxplot()+
  ylim(0,50)+
  facet_wrap(~type.resprout)

ggplot(changet0t1, aes(x= treatment, y= rgr, col=sci.name))+
  geom_boxplot()






