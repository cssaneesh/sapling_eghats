# To test codes!!

source('sapling_01_data.R')
# first visit time 0 and second visit time 1
time0 <- visit_01.lui %>%
  select(treatment,
         village,
         site,
         sci.name,
         tag.no,
         recruit.type,
         height,
         rcd,
         LUI) %>%
  group_by(treatment, site, sci.name) %>%
  rename(h0 = height) %>%
  rename(rcd0 = rcd)

time1 <- visit_02.lui %>%
  select(treatment,
         village,
         site,
         sci.name,
         tag.no,
         height,
         rcd,
         disturbance) %>%
  mutate(disturbance = replace_na(disturbance, 'none')) %>%
  mutate(disturbance = as.factor(disturbance)) %>%
  group_by(treatment, village, site, sci.name) %>%
  rename(h1 = height) %>%
  rename(rcd1 = rcd)

names(time0)
names(time1)

t1t0 <- time0 %>% left_join(time1, multiple = 'all') %>%
  filter(disturbance != 'dead') %>%
  filter(recruit.type != 'unknown') %>%
  filter(rcd1 > 0) %>% # omit plant with 0 growth (dead)
  filter(h1 > 0) %>%
  mutate(disturbance = fct_relevel(
    disturbance,
    c('none',
      'browsed',
      'dead',
      'partly_dried',
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

rgr <- t1t0 %>%
  mutate(rgr_rcd = log(rcd1) - log(rcd0)) %>% # rcd
  mutate(rgrH = log(h1) - log(h0)) # height

rgr %>% select(rgr_rcd) %>%
  arrange (desc(rgr_rcd)) %>% tail(5) # note the -ve values in rgr_rcd

# to shift rgr_rcd values up by a constant amount (the absolute minimum value of 'rgr_rcd'
# plus one), so that the smallest value becomes 1.
# This should make the response variable non-negative and suitable for use
# with the negative binomial distribution.

rgr$rgr_rcd <- rgr$rgr_rcd + abs(min(rgr$rgr_rcd)) + 1

rgr %>% select(rgr_rcd) %>%
  arrange (desc(rgr_rcd)) %>% tail(5)

# rgr of all species vs. treatment
rgr %>%
  ggplot(aes(x = treatment, y = rgr_rcd)) +
  geom_boxplot()

# rgr different species vs. treatment
ggplot(rgr, aes(y = sci.name, x = rgr_rcd)) +
  geom_boxplot() +
  facet_wrap( ~ treatment)

# rgr of all species vs. treatment and types of resprouts observed after disturbances
ggplot(rgr, aes(x = treatment, y = rgr_rcd, fill = disturbance)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = mean(rgr_rcd)), col = 'red', lty = 2)

rgr %>%
  ggplot(aes(y = rgr_rcd)) +
  geom_boxplot(aes(fill = disturbance)) +
  facet_wrap( ~ treatment) +
  geom_hline(aes(yintercept = mean(rgr_rcd)), col = 'red', lty = 2)

names(rgr)

rgr %>%
  ggplot() +
  geom_density(aes(x = rgr_rcd, col = treatment)) + facet_wrap( ~ sci.name)
# seven common species

hist(rgr$rgr_rcd)

# which predictor to take as a random effect?
boxplot(rgr_rcd ~ sci.name, data = rgr)

boxplot(rgr_rcd ~ disturbance, data = rgr)

boxplot(rgr_rcd ~ recruit.type, data = rgr)

boxplot(rgr_rcd ~ site, data = rgr)

boxplot(rgr_rcd ~ village, data = rgr)


# all of them have more than three levels!

names(rgr)

head(rgr, 3)

glimpse(rgr)

# rgr.mod.rcd <-
#   brm(
#     rgr_rcd ~ treatment + disturbance + recruit.type + (1 | sci.name),
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


# rcd of common species across 3 treatments----

com_Species <- rgr %>%
  filter(
    sci.name %in% c(
      'Acacia chundra',
      'Cassia fistula',
      'Chloroxylon swietenia',
      'Dalbergia paniculata',
      'Dolichandrone atrovirens',
      'Tectona grandis',
      'Wrightia tinctoria'
    )
  )


com_Species %>% names()

# com_Species_mod <-
#   brm(
#     rgr_rcd ~ treatment + disturbance + recruit.type + (1 | sci.name),
#     data =  com_Species,
#     family = gaussian(),
#     warmup = 1000,
#     iter = 4000,
#     chains = 4,
#     control = list(adapt_delta= 0.99)
#   )
# save(com_Species_mod, file='com_Species_mod.Rdata')

load('com_Species_mod.Rdata')
pp_check(com_Species_mod)

# Model convergence
mcmc_plot(com_Species_mod,
          type = 'trace')

mcmc_plot(com_Species_mod,
          type = "acf_bar")

mcmc_plot(com_Species_mod,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')

summary(com_Species_mod)
conditional_effects(com_Species_mod)



# data for Height----
rgr$rgrH <-
  rgr$rgrH + abs(min(rgr$rgrH)) + 1 # to shift rgrH values up by
# a constant amount (the absolute minimum value of 'rgrH'
# plus one), so that the smallest value becomes 1.


# rgrH of all species vs. treatment
ggplot(rgr, aes(x = treatment, y = rgrH)) +
  geom_boxplot()

# rgr different species vs. treatment
ggplot(rgr, aes(y = sci.name, x = rgrH)) +
  geom_boxplot() +
  facet_wrap( ~ treatment)

# rgr of all species vs. treatment when disturbances are different
ggplot(rgr, aes(x = treatment, y = rgrH, fill = treatment)) +
  geom_boxplot() +
  facet_wrap( ~ disturbance)

# rgr of all species vs. treatment and types of resprouts observed after disturbances
ggplot(rgr, aes(x = treatment, y = rgrH, fill = disturbance)) +
  geom_boxplot() +
  facet_wrap( ~ recruit.type)

# rgr of all species vs. treatment compared to types of resprouts
ggplot(rgr, aes(y = rgrH)) +
  geom_boxplot(aes(fill = recruit.type)) +
  facet_wrap( ~ treatment)

ggplot(rgr, aes(y = rgrH)) +
  geom_boxplot(aes(fill = disturbance)) +
  facet_wrap( ~ treatment)

names(rgr)

rgr %>%
  ggplot() +
  geom_density(aes(x = rgrH, col = treatment)) + facet_wrap( ~ sci.name)

hist(rgr$rgrH)

# which predictor to take as a random effect?
boxplot(rgrH ~ sci.name, data = rgr)

boxplot(rgrH ~ village, data = rgr)

boxplot(rgrH ~ disturbance, data = rgr)

boxplot(rgrH ~ recruit.type, data = rgr) # two levels

boxplot(rgrH ~ site, data = rgr)

# all of them have more than three levels!


# rgr.mod.H <-
#   brm(
#     rgrH ~ treatment + disturbance + recruit.type + (1 | sci.name),
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

load('com_Species_H_mod.Rdata')
pp_check(com_Species_H_mod)

# Model convergence
mcmc_plot(com_Species_H_mod,
          type = 'trace')

mcmc_plot(com_Species_H_mod,
          type = "acf_bar")

mcmc_plot(com_Species_H_mod,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')

summary(com_Species_H_mod)
conditional_effects(com_Species_H_mod)
