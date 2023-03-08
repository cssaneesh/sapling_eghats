source('01_seedling_data.R')

# alpha diversity seedling----
alpha_sum_sd <- seedling.dat %>% # alpha_summary_sd, sd= seedling
  group_by(site, treatment, LUI, sci.name) %>%
  summarise(abundance= sum(seedling)) %>%
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
    .groups = "drop") %>%
  # ungroup() %>%
  mutate(treatment = fct_relevel(treatment, c("Control", "CPFA", "CAFA")))%>% 
  # add the minimum number of individuals for calculating IBR
  mutate(minN = min(N))

# check are these data balanced? No, 22 sites in CAFA, 11 sites in CPFA and 11 sites in control.
# this is important for gamma- and beta-diversity comparisons
alpha_sum_sd %>% group_by(treatment) %>% 
  summarise(N_sites=n_distinct(site))

# individual based rarefaction before fitting models----

alpha_data_sd <- seedling.dat %>% # sd= seedling
    # first collate the transects at each unique location
  group_by(site, treatment, sci.name, LUI ) %>%
  summarise(abundance= sum(seedling)) %>%
  ungroup() %>%
  inner_join(alpha_sum_sd %>% ungroup() %>% 
               dplyr::select(site, LUI, minN),
             by = c('site', 'LUI')) %>% 
  # next for calculating Sn
  group_by(site, LUI) %>% 
  nest(data=c(sci.name, abundance, minN)) %>% 
  ungroup() %>% 
  mutate(Sn = purrr::map(data, ~mobr::rarefaction(.x$abundance, method = 'IBR', effort = unique(.x$minN)))) %>% 
  unnest(Sn)

# put Sn into the alpha_summary df
alpha_sum_sd <- inner_join(alpha_sum_sd %>% ungroup(), 
                            alpha_data_sd %>% dplyr::select(site, treatment, Sn, LUI),
                            by = c('site', 'treatment', 'LUI'))

# Models-----

names(alpha_sum_sd)
# number of individuals vs. treatment and LUI

# N.alpha_sd <-
#   brm(
#     N ~ treatment + LUI + (1 | site),
#     family = poisson(),
#     data = alpha_sum_sd,
#     cores = 4,
#     chains = 4,
#     control = list(adapt_delta = 0.9)
#   )

# save(N.alpha_sd, file= 'N.alpha_sd.Rdata')

load('N.alpha_sd.Rdata')
summary(N.alpha_sd)
pp_check(N.alpha_sd)

plot(N.alpha_sd)

# check model residual
alpha_sum_sd %>% 
  add_residual_draws(N.alpha_sd) %>% 
  median_qi(.residual) %>% 
  ggplot(aes(sample= .residual))+
  geom_qq()+
  geom_qq_line()


conditional_effects(
    N.alpha_sd,
    effects = 'treatment', # conditional effects of treatment
    re_formula = NA,
    method = 'fitted'
  )  


  conditional_effects(
    N.alpha_sd,
    effects = 'LUI', # conditional effects of LUI
    re_formula = NA,
    method = 'fitted'
  )

# number of species vs treatment and LUI
# poisson error for species richness

  # S.alpha.rich_sd <-
  # brm(
  #   S ~ treatment + LUI + (1 | site),
  #   family = poisson(),
  #   data = alpha_sum_sd,
  #   cores = 4,
  #   chains = 4,
  #   control = list(adapt_delta = 0.9)
  # )

# save(S.alpha.rich_sd, file= 'S.alpha.rich_sd.Rdata')

load('S.alpha.rich_sd.Rdata')

summary(S.alpha.rich_sd)

pp_check(S.alpha.rich_sd)

plot(S.alpha.rich_sd)

# check model residuals

alpha_sum_sd %>%
  add_residual_draws(S.alpha.rich_sd) %>%
  median_qi() %>%
  ggplot(aes(sample = .residual)) +
  geom_qq() +
  geom_qq_line()

# seedling richness----
  conditional_effects(
    S.alpha.rich_sd,
    effects = 'treatment', # conditional effects of treatment on richness
    re_formula = NA,
    method = 'fitted'
  )  

conditional_effects(
  S.alpha.rich_sd,
  effects = 'LUI', # conditional effects of LUI on richness
  re_formula = NA,
  method = 'fitted'
) 

# Expected number of species for n individuals vs treatment and LUI
# lognormal for Sn

Sn_alpha_sd <- brms::brm(Sn ~ treatment + LUI + (1 | site),
                      family = lognormal(),
                      data = alpha_sum_sd,
                      cores = 4,
                      chains = 4,
                      control = list(adapt_delta = 0.9)
)

#save(Sn_alpha_sd, file='Sn_alpha_sd.Rdata')
 
#load('Sn_alpha_sd.Rdata')

summary(Sn_alpha_sd)
pp_check(Sn_alpha_sd)

# check model residual
alpha_summary_sd %>%
  add_residual_draws(Sn_alpha_sd) %>%
  median_qi(.residual) %>%
  ggplot(aes(sample= .residual))+
  geom_qq()+
  geom_qq_line()

ghats_Sn_alpha_sd <-
  conditional_effects(
    Sn_alpha_sd,
    effects = 'treatment',
    re_formula = NA,
    method = 'fitted'
  )  # conditional effects

ghats_Sn_alpha_sd

# lognormal error for S_SPIE

S_PIE_alpha_sd <- brms::brm(
  S_PIE ~ treatment + LUI + (1 | site),
                         family = lognormal(),
                         data = alpha_summary_sd,
                         cores = 4,
                         chains = 4,
                         control = list(adapt_delta = 0.9)
)
 
# save(S_PIE_alpha_sd, file = 'S_PIE_alpha_sd.Rdata')
 
load('S_PIE_alpha_sd.Rdata')
 
summary(S_PIE_alpha_sd)
pp_check(S_PIE_alpha_sd)

# check model residual
alpha_summary_sd %>% 
  add_residual_draws(S_PIE_alpha_sd) %>%
  median_qi(.residual) %>%
  ggplot(aes(sample= .residual))+
  geom_qq()+
  geom_qq_line()
 
ghats_S_PIE_alpha_sd <-
  conditional_effects(
    S_PIE_alpha_sd,
    effects = 'treatment',
    re_formula = NA,
    method = 'fitted'
  )  # conditional effects
 
ghats_S_PIE_alpha_sd


ENSPIE_alpha_sd <- brms::brm(ENSPIE ~ treatment + LUI + (1 | site),
                          family = 'lognormal',
                          data = alpha_sum_sd,
                          iter = 10000,
                          warmup = 1000,
                          cores = 4,
                          chains = 4,
                          #control = list(adapt_delta = 0.9)
)

save(ENSPIE_alpha_sd, file = 'ENSPIE_alpha_sd.Rdata')

load('ENSPIE_alpha_sd.Rdata')

summary(ENSPIE_alpha_sd)
pp_check(ENSPIE_alpha_sd)

# check model residual
alpha_sum_sd %>% 
  add_residual_draws(ENSPIE_alpha_sd) %>% 
  median_qi(.residual) %>% 
  ggplot(aes(sample= .residual))+
  geom_qq()+
  geom_qq_line()

# seedling eveness-----

  conditional_effects(
    ENSPIE_alpha_sd,
    effects = 'treatment',
    re_formula = NA,
    method = 'fitted'
  )  # conditional effects

