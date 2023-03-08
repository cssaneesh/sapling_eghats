source('01_saplings_data.R')

# alpha diversity----
alpha_summary <- sites.lui %>%
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

# check are these data balanced? No, 28 sites in CAFA, 14 sites in CPFA and 13 sites in control.
# this is important for gamma- and beta-diversity comparisons
alpha_summary %>% group_by(treatment) %>% 
  summarise(N_sites=n_distinct(site))

# individual based rarefaction before fitting models----

alpha_data <- sites.lui %>% 
  # first collate the transects at each unique location
  group_by(site, treatment, sci.name, LUI) %>%
  count(sci.name, name = 'abundance') %>%
  ungroup() %>%
  inner_join(alpha_summary %>% ungroup() %>% 
             dplyr::select(site, LUI, minN),
           by = c('site', 'LUI')) %>% 
  # next for calculating Sn
  group_by(site, LUI) %>% 
  nest(data=c(sci.name, abundance, minN)) %>% 
  ungroup() %>% 
  mutate(Sn = purrr::map(data, ~mobr::rarefaction(.x$abundance, method = 'IBR', effort = unique(.x$minN)))) %>% 
  unnest(Sn)
 
# put Sn into the alpha_summary df
alpha_summary <- inner_join(alpha_summary %>% ungroup(), 
                            alpha_data %>% dplyr::select(site, treatment, Sn, LUI),
                            by = c('site', 'treatment', 'LUI'))

# Models-----
# number of individuals vs. treatment and LUI
N.alpha <-
  brm(
    N ~ treatment + LUI + (1 | site),
    family = poisson(),
    data = alpha_summary,
    cores = 4,
    chains = 4,
    control = list(adapt_delta = 0.9)
  )
save(N.alpha, file= 'N.alpha.Rdata')

load('N.alpha.Rdata')
summary(N.alpha)
pp_check(N.alpha)

plot(N.alpha)

# check model residual
alpha_summary %>% 
  add_residual_draws(N.alpha) %>% 
  median_qi(.residual) %>% 
  ggplot(aes(sample= .residual))+
  geom_qq()+
  geom_qq_line()

# sapling number-----
ghats_alpha_N <-
  conditional_effects(
    N.alpha,
    effects = 'treatment',
    re_formula = NA,
    method = 'fitted'
  )  # conditional effects

ghats_alpha_N

# number of species vs treatment and LUI
# poisson error for species richness
S.alpha.rich <-
  brm(
    S ~ treatment + LUI + (1 | site),
    family = poisson(),
    data = alpha_summary,
    cores = 4,
    chains = 4,
    control = list(adapt_delta = 0.9)
  )

# save(S.alpha.rich, file= 'S.alpha.rich.Rdata')
load('S.alpha.rich.Rdata')

summary(S.alpha.rich)

pp_check(S.alpha.rich)

plot(S.alpha.rich)

# check model residuals

alpha_summary %>%
  add_residual_draws(S.alpha.rich) %>%
  median_qi() %>%
  ggplot(aes(sample = .residual)) +
  geom_qq() +
  geom_qq_line()

# Sapling richness----
ghats_alpha_rich <-
  conditional_effects(
    S.alpha.rich,
    effects = 'treatment',
    re_formula = NA,
    method = 'fitted'
  )  # conditional effects

ghats_alpha_rich


# Expected number of species for n individuals vs treatment and LUI
# lognormal for Sn
Sn_alpha <- brms::brm(Sn ~ treatment + LUI + (1 | site),
                            family = lognormal(),
                            data = alpha_summary,
                            cores = 4,
                            chains = 4,
                            control = list(adapt_delta = 0.9)
                              )

# save(Sn_alpha, file='Sn_alpha.Rdata')

load('Sn_alpha.Rdata')

summary(Sn_alpha)
pp_check(Sn_alpha)

# check model residual
alpha_summary %>% 
  add_residual_draws(Sn_alpha) %>% 
  median_qi(.residual) %>% 
  ggplot(aes(sample= .residual))+
  geom_qq()+
  geom_qq_line()

# sn alpha----

# Sn is the number of species that are unique
ghats_Sn_alpha <-
  conditional_effects(
    Sn_alpha,
    effects = 'treatment',
    re_formula = NA,
    method = 'fitted'
  )  # conditional effects

ghats_Sn_alpha

# lognormal error for S_SPIE

S_PIE_alpha <- brms::brm(S_PIE ~ treatment + LUI + (1 | site),
                            family = lognormal(),
                            data = alpha_summary,
                            cores = 4,
                            chains = 4,
                            control = list(adapt_delta = 0.9)
)

# save(S_PIE_alpha, file = 'S_PIE_alpha.Rdata')

load('S_PIE_alpha.Rdata')

summary(S_PIE_alpha)
pp_check(S_PIE_alpha)


# check model residual
alpha_summary %>% 
  add_residual_draws(S_PIE_alpha) %>% 
  median_qi(.residual) %>% 
  ggplot(aes(sample= .residual))+
  geom_qq()+
  geom_qq_line()

# S_PIE-----
ghats_S_PIE_alpha <-
  conditional_effects(
    S_PIE_alpha,
    effects = 'treatment',
    re_formula = NA,
    method = 'fitted'
  )  # conditional effects

ghats_S_PIE_alpha


ENSPIE_alpha <- brms::brm(ENSPIE ~ treatment + LUI + (1 | site),
                         family = lognormal(),
                         data = alpha_summary,
                         cores = 4,
                         chains = 4,
                         control = list(adapt_delta = 0.9)
)

# save(ENSPIE_alpha, file = 'ENSPIE_alpha.Rdata')

load('ENSPIE_alpha.Rdata')

summary(ENSPIE_alpha)
pp_check(ENSPIE_alpha)

# check model residual
alpha_summary %>% 
  add_residual_draws(ENSPIE_alpha) %>% 
  median_qi(.residual) %>% 
  ggplot(aes(sample= .residual))+
  geom_qq()+
  geom_qq_line()

# ENSPIE----

ghats_ENSPIE_alpha <-
  conditional_effects(
    ENSPIE_alpha,
    effects = 'treatment',
    re_formula = NA,
    method = 'fitted'
  )  # conditional effects

ghats_ENSPIE_alpha


# 22/2/2023
# 04_alpha_analysis.R


