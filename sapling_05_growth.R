

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
  facet_wrap(~disturbance)

ggplot(changet0t1, aes(x= treatment, y= rgr))+
  geom_boxplot()+
  ylim(0,15)+
  facet_wrap(~disturbance)

ggplot(changet0t1, aes(x= treatment, y= rgr))+
  geom_boxplot()+
  ylim(0,15)+
  facet_wrap(~type.resprout)


ggplot(changet0t1, aes(y= rgr))+
  geom_boxplot(aes(col=sci.name))+
  facet_wrap(~treatment)

ggplot(changet0t1, aes(y= rgr))+
  geom_boxplot(aes(fill=type.resprout))+
  facet_wrap(~treatment)

ggplot(changet0t1, aes(y= rgr))+
  geom_boxplot(aes(fill=disturbance))+
  facet_wrap(~treatment)

names(changet0t1)

hist(changet0t1$rgr)

rgr.mod <- brm(rgr ~ treatment + LUI,
               data =  changet0t1)


rgr.mod


conditional_effects(rgr.mod)

# with species as random effects
rgr.mod.sp <- brm(rgr ~ treatment + LUI + (1|sci.name),
               data =  changet0t1)

rgr.mod.sp

conditional_effects(rgr.mod.sp)

rgr.mod.dist <- brm(rgr ~ treatment + disturbance,
                  data =  changet0t1)

summary(rgr.mod.dist)

conditional_effects(rgr.mod.dist)





