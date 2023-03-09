source('sapling_01_data.R')

# alpha diversity----
alpha_sum.sap <- visit_01.lui %>%
  group_by(site, treatment, LUI, sci.name) %>%
  count(sci.name, name = 'abundance') %>%
  ungroup() %>%
  # calculate metrics for each site
  group_by(site, treatment, LUI) %>%
  summarise (
    coverage = iNEXT.3D::DataInfo3D(abundance)$SC,
    S = n_distinct(sci.name),
    # number of unique species/richness
    N = sum(abundance),
    # total number of saplings
    S_PIE = mobr::calc_PIE(abundance, ENS = T),
    # Simpson's evenness index
    ENSPIE = vegan::diversity(abundance, index = 'invsimpson'),
    .groups = "drop")%>%
  # ungroup() %>%
  mutate(treatment = fct_relevel(treatment, c("Control", "CPFA", "CAFA")))%>% 
  # add the minimum number of individuals for calculating IBR
  mutate(minN = min(N))

head(alpha_sum.sap)
tail(alpha_sum.sap)

# check are these data balanced? No, 28 sites in CAFA, 14 sites in CPFA and 13 sites in control.
# this is important for gamma- and beta-diversity comparisons
alpha_sum.sap %>% group_by(treatment) %>% 
  summarise(N_sites=n_distinct(site))

# individual based rarefaction before fitting models----

alpha_data.sap <- visit_01.lui %>% # sap= saplings
  # first collate the transects at each unique location
  group_by(site, treatment, sci.name, LUI) %>%
  count(sci.name, name = 'abundance') %>%
  ungroup() %>%
  inner_join(alpha_sum.sap %>% ungroup() %>% 
             dplyr::select(site, LUI, minN),
           by = c('site', 'LUI')) %>% 
  # next for calculating Sn
  group_by(site, LUI) %>% 
  nest(data=c(sci.name, abundance, minN)) %>% 
  ungroup() %>% 
  mutate(Sn = purrr::map(data, ~mobr::rarefaction(.x$abundance, method = 'IBR', effort = unique(.x$minN)))) %>% 
  unnest(Sn)
 
# put Sn into the alpha_summary df
alpha_sum.sap <- inner_join(alpha_sum.sap %>% ungroup(), 
                            alpha_data.sap %>% dplyr::select(site, treatment, Sn, LUI),
                            by = c('site', 'treatment', 'LUI'))

head(alpha_sum.sap)
tail(alpha_sum.sap)

# Models-----
# number of individuals vs. treatment and LUI

# N.alpha.sap <-
#   brm(
#     N ~ treatment + LUI + (1 | site),
#     family = poisson(),
#     data = alpha_sum.sap,
#     cores = 4,
#     chains = 4,
#     control = list(adapt_delta = 0.9)
#   )
# 
# save(N.alpha.sap, file= 'N.alpha.sap.Rdata')

load('N.alpha.sap.Rdata')
summary(N.alpha.sap)
pp_check(N.alpha.sap)

plot(N.alpha.sap)

# check model residual
alpha_sum.sap %>% 
  add_residual_draws(N.alpha.sap) %>% 
  median_qi(.residual) %>% 
  ggplot(aes(sample= .residual))+
  geom_qq()+
  geom_qq_line()

# sapling number-----

conditional_effects(
  N.alpha.sap,
  effects = 'treatment',
  re_formula = NA,
  method = 'fitted'
)  # conditional effects

cf_treatments <- conditional_effects(
  N.alpha.sap,
  effects = 'treatment',
  re_formula = NA,
  method = 'fitted'
)  # conditional effects

cf_treatments.df <- as.data.frame(cf_treatments$treatment)

cf_treatments.df

conditional_effects(
  N.alpha.sap,
  effects = 'LUI',
  re_formula = NA,
  method = 'fitted'
)  # conditional effects of LUI is positive, because trenches support resprouting

# number of species vs treatment and LUI
# poisson error for species richness

# S.alpha.rich.sap <-
#   brm(
#     S ~ treatment + LUI + (1 | site),
#     family = poisson(),
#     data = alpha_sum.sap,
#     cores = 4,
#     chains = 4,
#     control = list(adapt_delta = 0.9)
#   )

# save(S.alpha.rich.sap, file= 'S.alpha.rich.sap.Rdata')
load('S.alpha.rich.sap.Rdata')

summary(S.alpha.rich.sap)

pp_check(S.alpha.rich.sap)

plot(S.alpha.rich.sap)

# check model residuals

alpha_sum.sap %>%
  add_residual_draws(S.alpha.rich.sap) %>%
  median_qi() %>%
  ggplot(aes(sample = .residual)) +
  geom_qq() +
  geom_qq_line()

# Sapling richness----
  conditional_effects(
    S.alpha.rich.sap,
    effects = 'treatment',
    re_formula = NA,
    method = 'fitted'
  )  # conditional effects


conditional_effects(
  S.alpha.rich.sap,
  effects = 'LUI',
  re_formula = NA,
  method = 'fitted'
)  # conditional effects. not all can species perform well in the presence of LUI

# Expected number of species for n individuals vs treatment and LUI
# lognormal for Sn

# Sn_alpha.sap <- brms::brm(Sn ~ treatment + LUI + (1 | site),
#                             family = lognormal(),
#                             data = alpha_sum.sap,
#                             cores = 4,
#                             chains = 4,
#                             control = list(adapt_delta = 0.9)
#                               )

# save(Sn_alpha.sap, file='Sn_alpha.sap.Rdata')

load('Sn_alpha.sap.Rdata')

summary(Sn_alpha.sap)
pp_check(Sn_alpha.sap)

# check model residual
alpha_sum.sap %>% 
  add_residual_draws(Sn_alpha.sap) %>% 
  median_qi(.residual) %>% 
  ggplot(aes(sample= .residual))+
  geom_qq()+
  geom_qq_line()

# sn alpha----

# Sn is the number of species that are unique
  conditional_effects(
    Sn_alpha.sap,
    effects = 'treatment',
    re_formula = NA,
    method = 'fitted'
  )  # conditional effects

conditional_effects(
  Sn_alpha.sap,
  effects = 'LUI',
  re_formula = NA,
  method = 'fitted'
)  # conditional effects

# S_SPIE
# lognormal error for S_SPIE

# S_PIE_alpha.sap <- brms::brm(S_PIE ~ treatment + LUI + (1 | site),
#                             family = lognormal(),
#                             data = alpha_sum.sap,
#                             cores = 4,
#                             chains = 4,
#                             control = list(adapt_delta = 0.9)
# )
# 
# save(S_PIE_alpha.sap, file = 'S_PIE_alpha.sap.Rdata')

load('S_PIE_alpha.sap.Rdata')

summary(S_PIE_alpha.sap)
pp_check(S_PIE_alpha.sap)


# check model residual
alpha_sum.sap %>% 
  add_residual_draws(S_PIE_alpha.sap) %>% 
  median_qi(.residual) %>% 
  ggplot(aes(sample= .residual))+
  geom_qq()+
  geom_qq_line()

# S_PIE-----
  conditional_effects(
    S_PIE_alpha.sap,
    effects = 'treatment',
    re_formula = NA,
    method = 'fitted'
  )  # conditional effects

conditional_effects(
  S_PIE_alpha.sap,
  effects = 'LUI',
  re_formula = NA,
  method = 'fitted'
)  # conditional effects


# ENSPIE
# ENSPIE_alpha.sap <- brms::brm(ENSPIE ~ treatment + LUI + (1 | site),
#                          family = lognormal(),
#                          data = alpha_sum.sap,
#                          cores = 4,
#                          chains = 4,
#                          control = list(adapt_delta = 0.9)
# )
# 
# save(ENSPIE_alpha.sap, file = 'ENSPIE_alpha.sap.Rdata')

load('ENSPIE_alpha.sap.Rdata')

summary(ENSPIE_alpha.sap)
pp_check(ENSPIE_alpha.sap)

# check model residual
alpha_sum.sap %>% 
  add_residual_draws(ENSPIE_alpha.sap) %>% 
  median_qi(.residual) %>% 
  ggplot(aes(sample= .residual))+
  geom_qq()+
  geom_qq_line()

# conditional effects
conditional_effects(
  ENSPIE_alpha.sap,
    effects = 'treatment',
    re_formula = NA,
    method = 'fitted'
  )  # conditional effects

conditional_effects(
  ENSPIE_alpha.sap,
  effects = 'LUI',
  re_formula = NA,
  method = 'fitted'
)  # conditional effects


