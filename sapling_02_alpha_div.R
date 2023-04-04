source('sapling_01_data.R')

# alpha diversity----
alpha_sum.sap <- visit_01.lui %>%
  group_by(site, village, treatment, LUI, sci.name) %>%
  count(sci.name, name = 'abundance') %>%
  ungroup() %>%
  # calculate metrics for each site
  group_by(site, village, treatment, LUI) %>%
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
  group_by(site, village, treatment, sci.name, LUI) %>%
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

alpha_sum.sap <- alpha_sum.sap %>% 
  left_join(adult.dat, multiple = 'all') # count and abundance of adult trees 

head(alpha_sum.sap, 3)
tail(alpha_sum.sap, 3)

# Models-----
# number of saplings vs. treatment, number of adult trees and LUI

boxplot(N ~ site, data = alpha_sum.sap)
boxplot(N ~ village, data = alpha_sum.sap) # for random effect

hist(alpha_sum.sap$N)

# N.alpha.sap <-
#   brm(
#     N ~ treatment + LUI + Nu.adu + (1 | village),
#     family = poisson(),
#     data = alpha_sum.sap,
#     cores = 4,
#     chains = 4,
#     control = list(adapt_delta = 0.9)
#   )
# save(N.alpha.sap, file= 'N.alpha.sap.Rdata')

load('N.alpha.sap.Rdata')

pp_check(N.alpha.sap)
plot(N.alpha.sap)
plot.residuals <- cbind(alpha_sum.sap, residuals(N.alpha.sap))
plot.residuals <- as.data.frame(plot.residuals)
# plot residuals, treatment
plot.residuals %>% 
  ggplot(aes(x= treatment, y= Estimate))+
  geom_boxplot()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')

# plot residuals lui
plot.residuals %>% 
  ggplot(aes(x= LUI, y= Estimate))+
  geom_point()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')

# plot residuals total adult
plot.residuals %>% 
  ggplot(aes(x= Nu.adu, y= Estimate))+
  geom_point()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')


summary(N.alpha.sap)

conditional_effects(N.alpha.sap)

# ce= conditional effects of treatment
N.alpha.sap_ce_t <- conditional_effects(N.alpha.sap, effects = 'treatment')
N.alpha.sap_ce_lui <- conditional_effects(N.alpha.sap, effects = 'LUI')
N.alpha.sap_ce_Nu.adul <- conditional_effects(N.alpha.sap, effects = 'Nu.adu')

# N ~ treatment
ggplot() +
  geom_point(
    data = alpha_sum.sap,
    # raw data
    aes(x = treatment, # predicting variable
        y = N, # response variable
    ),
    color = "grey",
    size = 1.2,
    alpha = 0.9,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_point(
    data = N.alpha.sap_ce_t$treatment,
    # conditional effect
    aes(x = treatment, # ce of the predicting variable
        y = estimate__,
        col = treatment),
    size = 3
  ) +
  geom_errorbar(
    data = N.alpha.sap_ce_t$treatment,
    aes(
      x = treatment,
      ymin = lower__,
      ymax = upper__,
      col = treatment
    ),
    linewidth = 1.3,
    width = 0.1
  ) 

# N ~ LUI
ggplot() +
  geom_point(
    data = alpha_sum.sap,
    # raw data
    aes(x = LUI, # predicting variable
        y = N, # response variable
        col= treatment),
    size = 1.2,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  )+
  geom_smooth(
    data = N.alpha.sap_ce_lui$LUI,
    # conditional effect
    aes(x = LUI, # ce of the predicting variable
        y = estimate__),
    linewidth = 1, method = 'lm')

# N ~ number of adult trees
ggplot() +
  geom_point(
    data = alpha_sum.sap,
    # raw data
    aes(x = Nu.adu, # predicting variable
        y = N, # response variable
        col= treatment),
    size = 1.2,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  )+
  geom_smooth(
    data = N.alpha.sap_ce_Nu.adul$Nu.adu,
    # conditional effect
    aes(x = Nu.adu, # ce of the predicting variable
        y = estimate__),
    linewidth = 1, method = 'lm' )

# number of species vs treatment and LUI
# poisson error for species richness

S.alpha.rich.sap <-
  brm(
    S ~ treatment + LUI + Sp.adu + (1 | village),
    family = poisson(),
    data = alpha_sum.sap,
    cores = 4,
    chains = 4,
    control = list(adapt_delta = 0.9)
  )
save(S.alpha.rich.sap, file= 'S.alpha.rich.sap.Rdata')

load('S.alpha.rich.sap.Rdata')

pp_check(S.alpha.rich.sap)
plot(S.alpha.rich.sap)
plot.residuals <- cbind(alpha_sum.sap, residuals(S.alpha.rich.sap))
plot.residuals <- as.data.frame(plot.residuals)
# plot residuals, treatment
plot.residuals %>% 
  ggplot(aes(x= treatment, y= Estimate))+
  geom_boxplot()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')

# plot residuals lui
plot.residuals %>% 
  ggplot(aes(x= LUI, y= Estimate))+
  geom_point()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')

# plot residuals total adult
plot.residuals %>% 
  ggplot(aes(x= Nu.adu, y= Estimate))+
  geom_point()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')


summary(S.alpha.rich.sap)

conditional_effects(S.alpha.rich.sap)

# ce= conditional effects of treatment
S.alpha.rich.sap_ce_t <- conditional_effects(S.alpha.rich.sap, effects = 'treatment')
S.alpha.rich.sap_ce_lui <- conditional_effects(S.alpha.rich.sap, effects = 'LUI')
S.alpha.rich.sap_ce_Sp.adul <- conditional_effects(S.alpha.rich.sap, effects = 'Sp.adu')

# S ~ treatment
ggplot() +
  geom_point(
    data = alpha_sum.sap,
    # raw data
    aes(x = treatment, # predicting variable
        y = S, # response variable
    ),
    color = "grey",
    size = 1.2,
    alpha = 0.9,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_point(
    data = S.alpha.rich.sap_ce_t$treatment,
    # conditional effect
    aes(x = treatment, # ce of the predicting variable
        y = estimate__,
        col = treatment),
    size = 3
  ) +
  geom_errorbar(
    data = S.alpha.rich.sap_ce_t$treatment,
    aes(
      x = treatment,
      ymin = lower__,
      ymax = upper__,
      col = treatment
    ),
    linewidth = 1.3,
    width = 0.1
  ) 

# S ~ LUI
ggplot() +
  geom_point(
    data = alpha_sum.sap,
    # raw data
    aes(x = LUI, # predicting variable
        y = S, # response variable
        col= treatment),
    size = 1.2,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  )+
  geom_smooth(
    data = S.alpha.rich.sap_ce_lui$LUI,
    # conditional effect
    aes(x = LUI, # ce of the predicting variable
        y = estimate__),
    linewidth = 1, method = 'lm')

# S ~ number of adult tree species
ggplot() +
  geom_point(
    data = alpha_sum.sap,
    # raw data
    aes(x = Sp.adu, # predicting variable
        y = S, # response variable
        col= treatment),
    size = 1.2,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  )+
  geom_smooth(
    data = S.alpha.rich.sap_ce_Sp.adul$Sp.adu,
    # conditional effect
    aes(x = Sp.adu, # ce of the predicting variable
        y = estimate__),
    linewidth = 1, method = 'lm' )


# Rarefied richness vs treatment, LUI, number of adult species and village as random effect
# lognormal for Sn

# Sn_alpha.sap <- brm(Sn ~ treatment + LUI + Sp.adu + (1 | village) ,
#                             family = lognormal(),
#                             data = alpha_sum.sap,
#                             cores = 4,
#                             chains = 4,
#                             control = list(adapt_delta = 0.9)
#                               )
# save(Sn_alpha.sap, file='Sn_alpha.sap.Rdata')

load('Sn_alpha.sap.Rdata')

pp_check(Sn_alpha.sap)
plot(Sn_alpha.sap)
plot.residuals <- cbind(alpha_sum.sap, residuals(Sn_alpha.sap))
plot.residuals <- as.data.frame(plot.residuals)
# plot residuals, treatment
plot.residuals %>% 
  ggplot(aes(x= treatment, y= Estimate))+
  geom_boxplot()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')

# plot residuals lui
plot.residuals %>% 
  ggplot(aes(x= LUI, y= Estimate))+
  geom_point()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')

# plot residuals total adult
plot.residuals %>% 
  ggplot(aes(x= Nu.adu, y= Estimate))+
  geom_point()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')


summary(Sn_alpha.sap)

conditional_effects(Sn_alpha.sap)

# ce= conditional effects of treatment
Sn_alpha.sap_ce_t <- conditional_effects(Sn_alpha.sap, effects = 'treatment')
Sn_alpha.sap_ce_lui <- conditional_effects(Sn_alpha.sap, effects = 'LUI')
Sn_alpha.sap_ce_Sp.adul <- conditional_effects(Sn_alpha.sap, effects = 'Sp.adu')

# Sn ~ treatment
ggplot() +
  geom_point(
    data = alpha_sum.sap,
    # raw data
    aes(x = treatment, # predicting variable
        y = Sn, # response variable
    ),
    color = "grey",
    size = 1.2,
    alpha = 0.9,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_point(
    data = Sn_alpha.sap_ce_t$treatment,
    # conditional effect
    aes(x = treatment, # ce of the predicting variable
        y = estimate__,
        col = treatment),
    size = 3
  ) +
  geom_errorbar(
    data = Sn_alpha.sap_ce_t$treatment,
    aes(
      x = treatment,
      ymin = lower__,
      ymax = upper__,
      col = treatment
    ),
    linewidth = 1.3,
    width = 0.1
  ) 

# Sn ~ LUI
ggplot() +
  geom_point(
    data = alpha_sum.sap,
    # raw data
    aes(x = LUI, # predicting variable
        y = Sn, # response variable
        col= treatment),
    size = 1.2,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  )+
  geom_smooth(
    data = Sn_alpha.sap_ce_lui$LUI,
    # conditional effect
    aes(x = LUI, # ce of the predicting variable
        y = estimate__),
    linewidth = 1, method = 'lm')

# Sn ~ number of adult tree species
ggplot() +
  geom_point(
    data = alpha_sum.sap,
    # raw data
    aes(x = Sp.adu, # predicting variable
        y = Sn, # response variable
        col= treatment),
    size = 1.2,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  )+
  geom_smooth(
    data = Sn_alpha.sap_ce_Sp.adul$Sp.adu,
    # conditional effect
    aes(x = Sp.adu, # ce of the predicting variable
        y = estimate__),
    linewidth = 1, method = 'lm' )

# ENSPIE
# ENSPIE_alpha.sap <- brm(ENSPIE ~ treatment + LUI + Nu.adu + Sp.adu + (1 | village) ,
#                          family = lognormal(),
#                          data = alpha_sum.sap,
#                          cores = 4,
#                          chains = 4,
#                          control = list(adapt_delta = 0.9)
# )
# save(ENSPIE_alpha.sap, file = 'ENSPIE_alpha.sap.Rdata')

load('ENSPIE_alpha.sap.Rdata')

pp_check(ENSPIE_alpha.sap)
plot(ENSPIE_alpha.sap)
plot.residuals <- cbind(alpha_sum.sap, residuals(ENSPIE_alpha.sap))
plot.residuals <- as.data.frame(plot.residuals)
# plot residuals, treatment
plot.residuals %>% 
  ggplot(aes(x= treatment, y= Estimate))+
  geom_boxplot()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')

# plot residuals lui
plot.residuals %>% 
  ggplot(aes(x= LUI, y= Estimate))+
  geom_point()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')

# plot residuals total adult
plot.residuals %>% 
  ggplot(aes(x= Nu.adu, y= Estimate))+
  geom_point()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')

# plot residuals sp adult
plot.residuals %>% 
  ggplot(aes(x= Sp.adu, y= Estimate))+
  geom_point()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')

summary(ENSPIE_alpha.sap)

conditional_effects(ENSPIE_alpha.sap)

# ce= conditional effects of treatment
ENSPIE_alpha.sap_ce_t <- conditional_effects(ENSPIE_alpha.sap, effects = 'treatment')
ENSPIE_alpha.sap_ce_lui <- conditional_effects(ENSPIE_alpha.sap, effects = 'LUI')
ENSPIE_alpha.sap_ce_Sp.adul <- conditional_effects(ENSPIE_alpha.sap, effects = 'Sp.adu')
ENSPIE_alpha.sap_ce_Nu.adul <- conditional_effects(ENSPIE_alpha.sap, effects = 'Nu.adu')

# ENSPIE ~ treatment
ggplot() +
  geom_point(
    data = alpha_sum.sap,
    # raw data
    aes(x = treatment, # predicting variable
        y = ENSPIE, # response variable
    ),
    color = "grey",
    size = 1.2,
    alpha = 0.9,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_point(
    data = ENSPIE_alpha.sap_ce_t$treatment,
    # conditional effect
    aes(x = treatment, # ce of the predicting variable
        y = estimate__,
        col = treatment),
    size = 3
  ) +
  geom_errorbar(
    data = ENSPIE_alpha.sap_ce_t$treatment,
    aes(
      x = treatment,
      ymin = lower__,
      ymax = upper__,
      col = treatment
    ),
    linewidth = 1.3,
    width = 0.1
  ) 

# ENSPIE ~ LUI
ggplot() +
  geom_point(
    data = alpha_sum.sap,
    # raw data
    aes(x = LUI, # predicting variable
        y = ENSPIE, # response variable
        col= treatment),
    size = 1.2,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  )+
  geom_smooth(
    data = ENSPIE_alpha.sap_ce_lui$LUI,
    # conditional effect
    aes(x = LUI, # ce of the predicting variable
        y = estimate__),
    linewidth = 1, method = 'lm')

# ENSPIE ~ number of adult tree species
ggplot() +
  geom_point(
    data = alpha_sum.sap,
    # raw data
    aes(x = Sp.adu, # predicting variable
        y = ENSPIE, # response variable
        col= treatment),
    size = 1.2,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  )+
  geom_smooth(
    data = Sn_alpha.sap_ce_Sp.adul$Sp.adu,
    # conditional effect
    aes(x = Sp.adu, # ce of the predicting variable
        y = estimate__),
    linewidth = 1, method = 'lm' )

# ENSPIE ~ number of adult tree
ggplot() +
  geom_point(
    data = alpha_sum.sap,
    # raw data
    aes(x = Nu.adu, # predicting variable
        y = ENSPIE, # response variable
        col= treatment),
    size = 1.2,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  )+
  geom_smooth(
    data = ENSPIE_alpha.sap_ce_Nu.adul$Nu.adu,
    # conditional effect
    aes(x = Nu.adu, # ce of the predicting variable
        y = estimate__),
    linewidth = 1, method = 'lm' )



