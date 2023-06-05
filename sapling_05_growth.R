source('sapling_01_data.R')

names(visit_01.lui)
names(visit_02.lui)

# DAG
sapling_dag <- dagitty('dag{
    Treatment-> RGR;
    Trench -> RGR;
    Treatment -> Livestock;
    Livestock-> Browsing;
    Livestock -> Trampling;
    Trampling -> RGR;
    Browsing -> RGR
    
}')

coordinates(sapling_dag) <-
  list( x=c(Treatment=1,  # column 1
            Livestock= 1, # column 2
            Trench= 1.5, # column 2
            RGR=2, # column 2
            Browsing= 2,
            Trampling= 3
  ),
  y=c(Treatment=0, # middle row/0 
      RGR=0, # middle row/0 
      Livestock= -1, # above middle row -1
      Trench= 1, # below the middle row/1
      Browsing= -1.5,
      Trampling= -1
  ))

plot(sapling_dag)

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

names(rgr)
head(rgr, 3)

# percentage of saplings browsed and trampled
rgr %>% 
  group_by(
    # village,
           # site, 
           # sci.name, 
           sap_health,
           sap.status,
           Treatment, 
           # Goat, 
           # Trenches
  ) %>% 
  summarise(Percentage= round(n()/ nrow(rgr) * 100, 2),.groups = 'drop'
  ) %>% view()
# same on a bar plot
rgr %>% 
  group_by(
    # village,
    # site, 
    # sci.name, 
    sap_health,
    sap.status,
    Treatment, 
    # Goat, 
    # Trenches
  ) %>% 
  summarise(Percentage= round(n()/ nrow(rgr) * 100, 2),.groups = 'drop'
  ) %>% ggplot(aes(x= Treatment, y= Percentage, fill= sap.status))+
  geom_bar(stat = 'identity', position = 'dodge')



# examine how browsing and trampling is influenced by the number of livestock
sap_status <- rgr %>% 
  group_by(village, 
           site, 
           # sci.name, 
           sap_health,
           sap.status,
           Treatment, 
           Goat, 
           Trenches
  ) %>% 
  summarise(Percentage= round(n()/ nrow(rgr) * 100, 2),.groups = 'drop'
  ) %>% 
  mutate(goatdat= if_else(sap.status== 'browsed', 1, 0)) %>% 
  mutate(trampdat= if_else(sap.status== 'trampled', 1, 0))

head(sap_status, 3)

sap_status %>% ggplot(aes(x= goatdat, fill= Treatment))+
  geom_bar(position = 'dodge')

sap_status %>% ggplot(aes(x= trampdat, fill= Treatment))+
  geom_bar(position = 'dodge')

sap_status %>% filter(sap.status== 'browsed') %>% ggplot(aes(x= Treatment, y= Percentage))+
  geom_boxplot()

sap_status %>% filter(sap.status== 'trampled') %>% ggplot(aes(x= Treatment, y= Percentage))+
  geom_boxplot()

sap_status %>% filter(sap.status== 'none') %>% ggplot(aes(x= Treatment, y= Percentage))+
  geom_boxplot()

sap_status %>% filter(sap.status== 'partly_dried') %>% ggplot(aes(x= Treatment, y= Percentage))+
  geom_boxplot()

# rgr of all species vs. treatment
rgr %>%
  ggplot(aes(x = Treatment, y = rgr_rcd)) +
  geom_boxplot()

# rgr different species vs. treatment
ggplot(rgr, aes(y = sci.name, x = rgr_rcd)) +
  geom_boxplot() +
  facet_wrap( ~ Treatment)

# rgr_rcd of all species vs. treatment and types of re-sprouts observed after sap.status
rgr %>%
  ggplot(aes(y = rgr_rcd)) +
  geom_boxplot(aes(fill = sap.status)) + # trampling and browsing
  facet_wrap( ~ Treatment) +
  geom_hline(aes(yintercept = mean(rgr_rcd)), col = 'red', lty = 2)

# rgr %>%
#   ggplot() +
#   geom_density(aes(x = rgr_rcd, col = Treatment)) + facet_wrap( ~ sci.name)
# # seven common species

hist(rgr$rgr_rcd)

# which predictor to take as a random effect?

boxplot(rgr_rcd ~ site, data = rgr)
boxplot(rgr_rcd ~ village, data = rgr)

# all of them have more than three levels!
names(rgr)
head(rgr, 3)

# rgrH of all species vs. treatment
rgr %>%
  ggplot(aes(x = Treatment, y = rgrH)) +
  geom_boxplot()

# rgrH different species vs. treatment
ggplot(rgr, aes(y = sci.name, x = rgrH)) +
  geom_boxplot() +
  facet_wrap( ~ Treatment)

# rgrH of all species vs. treatment and types of resprouts observed after disturbances
rgr %>%
  ggplot(aes(y = rgrH)) +
  geom_boxplot(aes(fill = sap.status)) + # trampling and browsing
  facet_wrap( ~ Treatment) +
  geom_hline(aes(yintercept = mean(rgrH)), col = 'red', lty = 2)

# rgr %>%
#   ggplot() +
#   geom_density(aes(x = rgrH, col = Treatment)) + facet_wrap( ~ sci.name)
# # seven common species

glimpse(rgr)

# Models----
# rgr_rcd----

# rcd ~ treatment
# mod.rgr.treat <-
#   brm(
#     rgr_rcd ~ Treatment + (1 | site),
#     data =  rgr,
#     family = gaussian(),
#     warmup = 1000,
#     iter = 4000,
#     chains = 4,
#     control = list(adapt_delta=0.95)
#   )
# save(mod.rgr.treat, file='mod.rgr.treat.Rdata')

load('mod.rgr.treat.Rdata')
pp_check(mod.rgr.treat)

# Model convergence
mcmc_plot(mod.rgr.treat,
          type = 'trace')

mcmc_plot(mod.rgr.treat,
          type = "acf_bar")

mcmc_plot(mod.rgr.treat,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')

summary(mod.rgr.treat)
conditional_effects(mod.rgr.treat)

# rcd ~ treatment * goat

# mod.rgr.treat.goat <-
#   brm(
#     rgr_rcd ~ Treatment * Goat + (1 | site),
#     data =  rgr,
#     family = gaussian(),
#     warmup = 1000,
#     iter = 4000,
#     chains = 4,
#     control = list(adapt_delta=0.95)
#   )
# save(mod.rgr.treat.goat, file='mod.rgr.treat.goat.Rdata')

load('mod.rgr.treat.goat.Rdata')
pp_check(mod.rgr.treat.goat)

# Model convergence
mcmc_plot(mod.rgr.treat.goat,
          type = 'trace')

mcmc_plot(mod.rgr.treat.goat,
          type = "acf_bar")

mcmc_plot(mod.rgr.treat.goat,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')

summary(mod.rgr.treat.goat)
conditional_effects(mod.rgr.treat.goat)
conditional_effects(mod.rgr.treat.goat, effects = 'Treatment:Goat')
conditional_effects(mod.rgr.treat.goat, effects = 'Goat:Treatment')


# rcd ~ treatment * trench
# mod.rgr.treat.trench <-
#   brm(
#     rgr_rcd ~ Treatment * Trenches + (1 | site),
#     data =  rgr,
#     family = gaussian(),
#     warmup = 1000,
#     iter = 4000,
#     chains = 4,
#     control = list(adapt_delta=0.95)
#   )
# save(mod.rgr.treat.trench, file='mod.rgr.treat.trench.Rdata')

load('mod.rgr.treat.trench.Rdata')
pp_check(mod.rgr.treat.trench)

# Model convergence
mcmc_plot(mod.rgr.treat.trench,
          type = 'trace')

mcmc_plot(mod.rgr.treat.trench,
          type = "acf_bar")

mcmc_plot(mod.rgr.treat.trench,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')

summary(mod.rgr.treat.trench)
conditional_effects(mod.rgr.treat.trench)
conditional_effects(mod.rgr.treat.trench, effects = 'Trenches:Treatment')
conditional_effects(mod.rgr.treat.trench, effects = 'Treatment:Trenches')

# rgrH----
# rgrh ~ treatment
# mod.rgrH.treat <-
#   brm(
#     rgrH ~ Treatment + (1 | site),
#     data =  rgr,
#     family = gaussian(),
#     warmup = 1000,
#     iter = 4000,
#     chains = 4,
#     control = list(adapt_delta=0.95)
#   )
# save(mod.rgrH.treat, file='mod.rgrH.treat.Rdata')

load('mod.rgrH.treat.Rdata')
pp_check(mod.rgrH.treat)

# Model convergence
mcmc_plot(mod.rgrH.treat,
          type = 'trace')

mcmc_plot(mod.rgrH.treat,
          type = "acf_bar")

mcmc_plot(mod.rgrH.treat,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')

summary(mod.rgrH.treat)
conditional_effects(mod.rgrH.treat)


# rgrh ~ treatment * goat
# mod.rgrH.treat.goat <-
#   brm(
#     rgrH ~ Treatment * Goat + (1 | village),
#     data =  rgr,
#     family = gaussian(),
#     warmup = 1000,
#     iter = 4000,
#     chains = 4,
#     control = list(adapt_delta=0.95)
#   )
# save(mod.rgrH.treat.goat, file='mod.rgrH.treat.goat.Rdata')

load('mod.rgrH.treat.goat.Rdata')
pp_check(mod.rgrH.treat.goat)

# Model convergence
mcmc_plot(mod.rgrH.treat.goat,
          type = 'trace')

mcmc_plot(mod.rgrH.treat.goat,
          type = "acf_bar")

mcmc_plot(mod.rgrH.treat.goat,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')

summary(mod.rgrH.treat.goat)
conditional_effects(mod.rgrH.treat.goat)


# rgrh ~ treatment * trench
# mod.rgrH.treat.trench <-
#   brm(
#     rgrH ~ Treatment * Trenches + (1 | site),
#     data =  rgr,
#     family = gaussian(),
#     warmup = 1000,
#     iter = 4000,
#     chains = 4,
#     control = list(adapt_delta=0.95)
#   )
# save(mod.rgrH.treat.trench, file='mod.rgrH.treat.trench.Rdata')

load('mod.rgrH.treat.trench.Rdata')
pp_check(mod.rgrH.treat.trench)

# Model convergence
mcmc_plot(mod.rgrH.treat.trench,
          type = 'trace')

mcmc_plot(mod.rgrH.treat.trench,
          type = "acf_bar")

mcmc_plot(mod.rgrH.treat.trench,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')

summary(mod.rgrH.treat.trench)
conditional_effects(mod.rgrH.treat.trench)

# goat casuing browsing and trampling----

names(sap_status)
head(sap_status, 3)

# treatment and number of goats in a village as predictors of browsing
# sap_browse <- brm(goatdat ~ Treatment * Goat + (1|village), data= sap_status,
#                   family = bernoulli(link = "logit"),
#                   chains = 4,
#                   warmup = 1000,
#                   iter = 2000,
#                   thin = 1
#                   )
# save(sap_browse, file= 'sap_browse.Rdata')

load('sap_browse.Rdata')
pp_check(sap_browse)

# Model convergence
mcmc_plot(sap_browse,
          type = 'trace')

mcmc_plot(sap_browse,
          type = "acf_bar")

mcmc_plot(sap_browse,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')


summary(sap_browse)
conditional_effects(sap_browse)

# conditional_effects(sap_browse, effects = 'Treatment:Goat')

# trampling
# treatment and number of goats in a village as predictors of trampling
# sap_tramp <- brm(trampdat ~ Treatment * Goat + (1|village), data= sap_status,
#                  family = bernoulli(link = "logit"),
#                  chains = 4,
#                  warmup = 1000,
#                  iter = 2000,
#                  thin = 1
#                  )
# save(sap_tramp, file= 'sap_tramp.Rdata')

load('sap_tramp.Rdata')
pp_check(sap_tramp)

# Model convergence
mcmc_plot(sap_tramp,
          type = 'trace')

mcmc_plot(sap_tramp,
          type = "acf_bar")

mcmc_plot(sap_tramp,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')


summary(sap_tramp)
conditional_effects(sap_tramp)

conditional_effects(sap_tramp, effects = 'Treatment:Goat')

