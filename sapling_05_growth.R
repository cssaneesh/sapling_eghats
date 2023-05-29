source('sapling_01_data.R')
names(visit_01.lui)

sapling_dag <- dagitty('dag{
    Treatment-> RGR <- Livestock;
    Trench -> RGR;
    Treatment -> Livestock
}')

coordinates(sapling_dag) <-
  list( x=c(Treatment=1,  # column 1
            Livestock= 1.5, # column 2
            Trench= 2, # column 2
            RGR=2 # column 2
  ),
  y=c(Treatment=0, # middle row/0 
      RGR=0, # middle row/0 
      Livestock= -1, # above middle row -1
      Trench= -1 # below the middle row/1
  ))

plot(sapling_dag)

# first visit time 0 and second visit time 1
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
         disturbance
         ) %>%
  mutate(disturbance = replace_na(disturbance, 'none')) %>%
  mutate(disturbance = as.factor(disturbance)) %>%
  group_by(Treatment, village, site, sci.name) %>%
  rename(h1 = height) %>%
  rename(rcd1 = rcd)

names(time0)
names(time1)

t1t0 <- time0 %>% left_join(time1, multiple = 'all') %>%
  filter(disturbance != 'dead') %>%
  filter(disturbance != 'partly_dried') %>% # this is not disturbance.
  filter(recruit.type != 'unknown') %>%
  filter(rcd1 > 0) %>% # omit plant with 0 growth (dead)
  filter(h1 > 0) %>%
  mutate(disturbance = fct_relevel(
    disturbance,
    c('none',
      'browsed',
      'dead',
      'partly_dried', # this is not disturbance.
      'trampled')
  )) %>%
  arrange(disturbance) %>%
  mutate(recruit.type = fct_relevel(recruit.type, c(
    'seedling recruitment',
    'clonal offspring'
  ))) %>%
  arrange(recruit.type)

head(t1t0, 4)
tail(t1t0, 4)

# data for RCD----
# RCD= Root collar diameter

rgr <- t1t0 %>% select(Treatment, 
                       village, 
                       site, 
                       sci.name, 
                       tag.no,
                       recruit.type, 
                       Goat,
                       Trenches,
                       disturbance,
                       h0, 
                       h1,
                       rcd0, 
                       rcd1
                       ) %>% 
  mutate(rgrH = log(h1) - log(h0)) %>%  # height
  mutate(rgr_rcd = log(rcd1) - log(rcd0)) # rcd


rgr %>% select(rgr_rcd) %>%
  arrange (desc(rgr_rcd)) %>% tail(5) # note the -ve values in rgr_rcd

# rgr %>% View()

# to shift rgr_rcd values up by a constant amount (the absolute minimum value of 'rgr_rcd'
# plus one), so that the smallest value becomes 1.
# This should make the response variable non-negative and suitable for use
# with the negative binomial distribution.

rgr$rgr_rcd <- rgr$rgr_rcd + abs(min(rgr$rgr_rcd)) + 1

rgr %>% select(rgr_rcd) %>%
  arrange (desc(rgr_rcd)) %>% tail(5)

rgr %>% select(rgr_rcd) %>%
  arrange (desc(rgr_rcd)) %>% head(5)

# rgr of all species vs. treatment
rgr %>%
  ggplot(aes(x = Treatment, y = rgr_rcd)) +
  geom_boxplot()

# rgr different species vs. treatment
ggplot(rgr, aes(y = sci.name, x = rgr_rcd)) +
  geom_boxplot() +
  facet_wrap( ~ Treatment)

# rgr_rcd of all species vs. treatment and types of re-sprouts observed after disturbances
rgr %>%
  ggplot(aes(y = rgr_rcd)) +
  geom_boxplot(aes(fill = disturbance)) + # trampling and browsing
  facet_wrap( ~ Treatment) +
  geom_hline(aes(yintercept = mean(rgr_rcd)), col = 'red', lty = 2)

# facet by type of disturbance 
rgr %>%
  ggplot(aes(y = rgr_rcd, x= Treatment)) +
  geom_boxplot(aes(fill = disturbance)) + # trampling and browsing
  facet_wrap( ~ recruit.type) +
  geom_hline(aes(yintercept = mean(rgr_rcd)), col = 'red', lty = 2)

names(rgr)

rgr %>%
  ggplot() +
  geom_density(aes(x = rgr_rcd, col = Treatment)) + facet_wrap( ~ sci.name)
# seven common species

hist(rgr$rgr_rcd)

# which predictor to take as a random effect?

boxplot(rgr_rcd ~ site, data = rgr)
boxplot(rgr_rcd ~ village, data = rgr)

# all of them have more than three levels!
names(rgr)

head(rgr, 3)

# data for Height----
rgr$rgrH <-
  rgr$rgrH + abs(min(rgr$rgrH)) + 1 # to shift rgrH values up by
# a constant amount (the absolute minimum value of 'rgrH'
# plus one), so that the smallest value becomes 1.

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
  geom_boxplot(aes(fill = disturbance)) + # trampling and browsing
  facet_wrap( ~ Treatment) +
  geom_hline(aes(yintercept = mean(rgrH)), col = 'red', lty = 2)

# rgrH facet by disturbance
rgr %>%
  ggplot(aes(y = rgrH, x= Treatment)) +
  geom_boxplot(aes(fill = disturbance)) + # trampling and browsing
  facet_wrap( ~ recruit.type) +
  geom_hline(aes(yintercept = mean(rgrH)), col = 'red', lty = 2)

rgr %>%
  ggplot() +
  geom_density(aes(x = rgrH, col = Treatment)) + facet_wrap( ~ sci.name)
# seven common species

glimpse(rgr)

# rgr.mod.rcd <-
#   brm(
#     rgr_rcd ~ Treatment + disturbance + (1 | site),
#     data =  rgr,
#     family = gaussian(),
#     warmup = 1000,
#     iter = 4000,
#     chains = 4,
#     control = list(adapt_delta=0.95)
#   )
# save(rgr.mod.rcd, file='rgr.mod.rcd.Rdata')

load('rgr.mod.rcd.Rdata')
pp_check(rgr.mod.rcd)

# Model convergence
mcmc_plot(rgr.mod.rcd,
          type = 'trace')

mcmc_plot(rgr.mod.rcd,
          type = "acf_bar")

mcmc_plot(rgr.mod.rcd,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')

summary(rgr.mod.rcd)
conditional_effects(rgr.mod.rcd)
conditional_effects(rgr.mod.rcd, effects = 'Treatment:disturbance')

rgr.mod.rcd_i <- # interaction
  brm(
    rgr_rcd ~ Treatment * disturbance + (1 | site),
    data =  rgr,
    family = gaussian(),
    warmup = 1000,
    iter = 4000,
    chains = 4,
    control = list(adapt_delta=0.95)
  )
save(rgr.mod.rcd_i, file= 'rgr.mod.rcd_i.Rdata')

load('rgr.mod.rcd_i.Rdata')

pp_check(rgr.mod.rcd_i)
summary(rgr.mod.rcd_i)
conditional_effects(rgr.mod.rcd_i)

hist(rgr$rgrH)

# which predictor to take as a random effect?
boxplot(rgrH ~ village, data = rgr)
boxplot(rgrH ~ site, data = rgr)

# all of them have more than three levels!

# rgr.mod.H <-
#   brm(
#     rgrH ~ Treatment + disturbance + (1 | site),
#     data =  rgr,
#     family = gaussian(),
#     warmup = 1000,
#     iter = 4000,
#     chains = 4
#   )
# save(rgr.mod.H, file='rgr.mod.H.Rdata')

load('rgr.mod.H.Rdata')
pp_check(rgr.mod.H)

# Model convergence
mcmc_plot(rgr.mod.H,
          type = 'trace')

mcmc_plot(rgr.mod.H,
          type = "acf_bar")

mcmc_plot(rgr.mod.H,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')

summary(rgr.mod.H)
conditional_effects(rgr.mod.H)
conditional_effects(rgr.mod.H, effects = 'Treatment:disturbance')



rgr.mod.H.1 <-
  brm(
    rgrH ~ Treatment * disturbance + (1 | site),
    data =  rgr,
    family = gaussian(),
    warmup = 1000,
    iter = 4000,
    chains = 4
  )
save(rgr.mod.H.1, file= 'rgr.mod.H.1.Rdata')

load('rgr.mod.H.1.Rdata')
pp_check(rgr.mod.H.1)
summary(rgr.mod.H.1)
conditional_effects(rgr.mod.H.1)


# rcd of common species across 3 treatments----
# com_Species <- rgr %>%
#   filter(
#     sci.name %in% c(
#       'Acacia chundra',
#       'Cassia fistula',
#       'Chloroxylon swietenia',
#       'Dalbergia paniculata',
#       'Dolichandrone atrovirens',
#       'Tectona grandis',
#       'Wrightia tinctoria'
#     )
#   )
# 
# 
# com_Species %>% names()
# 
# com_Species_mod <-
#   brm(
#     rgr_rcd ~ Treatment + disturbance + recruit.type + (1 | sci.name),
#     data =  com_Species,
#     family = gaussian(),
#     warmup = 1000,
#     iter = 4000,
#     chains = 4,
#     control = list(adapt_delta= 0.99)
#   )
# # save(com_Species_mod, file='com_Species_mod.Rdata')
# 
# # load('com_Species_mod.Rdata')
# # pp_check(com_Species_mod)
# 
# # Model convergence
# mcmc_plot(com_Species_mod,
#           type = 'trace')
# 
# mcmc_plot(com_Species_mod,
#           type = "acf_bar")
# 
# mcmc_plot(com_Species_mod,
#           type = "areas",
#           prob = 0.95) + # see if predictors CI contain zero.
#   geom_vline(xintercept = 0, col = 'grey')
# 
# summary(com_Species_mod)
# conditional_effects(com_Species_mod)

# height of common species 
# com_Species_H_mod <-
#   brm(
#     rgrH ~ treatment + disturbance + recruit.type + (1 | sci.name),
#     data =  com_Species,
#     family = gaussian(),
#     warmup = 1000,
#     iter = 4000,
#     chains = 4,
#     control = list(adapt_delta= 0.99)
#   )
# save(com_Species_H_mod, file='com_Species_H_mod.Rdata')

# load('com_Species_H_mod.Rdata')
# pp_check(com_Species_H_mod)
# 
# # Model convergence
# mcmc_plot(com_Species_H_mod,
#           type = 'trace')
# 
# mcmc_plot(com_Species_H_mod,
#           type = "acf_bar")
# 
# mcmc_plot(com_Species_H_mod,
#           type = "areas",
#           prob = 0.95) + # see if predictors CI contain zero.
#   geom_vline(xintercept = 0, col = 'grey')
# 
# summary(com_Species_H_mod)
# conditional_effects(com_Species_H_mod)
