source('00_seedling_data.R')

# alpha diversity seedling----
species.level_sd <- seedling.dat %>% # alpha_summary_sd, sd= seedling
  filter(seedling>0) %>% 
  group_by(site, Treatment, LUI, sci.name, adult) %>%
  summarise(abundance= sum(seedling),.groups = 'drop') 

species.level_ad <- seedling.dat %>% # alpha_summary_sd, ad= adult trees
  group_by(site, Treatment, LUI, sci.name) %>%
  summarise(abundance= sum(adult),.groups = 'drop') 

# data preparation for calculating alpha
alpha_sum_sd <- seedling.dat %>%
  filter(LUI < 1.20) %>% # filtered out high LUI
  group_by(site, Treatment, sci.name, village) %>%
  # summarise(abundance= sum(adult),.groups = 'drop') %>% # abundance of adult trees
  group_by(site, Treatment, village) %>%
  summarise (Sp.adu = n_distinct(sci.name),
             # number of unique species/richness
             # Nu.adu = sum(abundance),
             # total number of adult trees
             .groups = "drop") %>% # add the total number of adults in each site
  left_join(seedling.dat %>% select(!adult), multiple = "all") %>% 
  group_by(site, Treatment, sci.name, LUI, 
           # Sp.adu, 
           # Nu.adu, 
           village) %>%
  summarise(abundance= sum(seedling), .groups = 'drop') %>% # abundance of seedling
  filter(abundance>0) %>% 
  # calculate metrics for each site
  group_by(site, Treatment, village, LUI, 
           # Sp.adu, 
           # Nu.adu
           ) %>%
  summarise (
    coverage = iNEXT.3D::DataInfo3D(abundance)$SC,
    S = n_distinct(sci.name),
    # number of unique species/richness
    N = sum(abundance),
    # total number of seedlings
    S_PIE = mobr::calc_PIE(abundance, ENS = T),
    # Simpson's evenness index
    ENSPIE = vegan::diversity(abundance, index = 'invsimpson'),
    .groups = "drop") %>%
  # ungroup() %>%
  mutate(Treatment = fct_relevel(Treatment, c("Control", "CPFA", "CAFA")))%>% 
  # add the minimum number of individuals for calculating IBR
  mutate(minN = min(N))

# check are these data balanced? No, 22 sites in CAFA, 11 sites in CPFA and 11 sites in control.
# with all LUIs, but with low and medium LUI it is only
alpha_sum_sd %>% group_by(Treatment) %>% 
  summarise(N_sites=n_distinct(site))

# individual based rarefaction before fitting models----

alpha_data_sd <- seedling.dat %>% # sd= seedling
  select(-adu.stat) %>% 
  filter(seedling>0) %>%  # get rid of all sites with adults but 0 seedlings
  group_by(site, Treatment, sci.name) %>%
  summarise(abundance= sum(seedling), .groups = 'drop') %>%
  inner_join(alpha_sum_sd %>% ungroup() %>% 
               dplyr::select(site, LUI, 
                             # Sp.adu, Nu.adu, 
                             village,  minN),
             by = c('site')) %>% 
  # next for calculating Sn
  group_by(site, LUI) %>% 
  nest(data=c(sci.name, abundance, minN)) %>% 
  ungroup() %>% 
  mutate(Sn = purrr::map(data, ~mobr::rarefaction(.x$abundance, method = 'IBR', effort = unique(.x$minN)))) %>% 
  unnest(Sn)

# put Sn into the alpha_summary df
alpha_sum_sd <- inner_join(alpha_sum_sd %>% ungroup(), 
                            alpha_data_sd %>% 
                             dplyr::select(site, Treatment, Sn, village),
                            by = c('site', 'Treatment', 'village')
                           )
# Models-----

names(alpha_sum_sd)
# number of individuals vs. Treatment, LUI and number of adult trees 

boxplot(N ~ site, data = alpha_sum_sd) # not using site as a random effect
boxplot(N ~ village, data = alpha_sum_sd) # village as a random effect
# N ~ Treatment * LUI----
# N.alpha_sd <-
#   brm(
#     N ~ Treatment * LUI + (1|village),
#     family = poisson(),
#     data = alpha_sum_sd, 
#     chains = 4,
#     warmup = 1000,
#     iter = 4000)
# save(N.alpha_sd, file= 'N.alpha_sd.Rdata')

load('N.alpha_sd.Rdata')

mcmc_plot(N.alpha_sd,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'red')

mcmc_plot(N.alpha_sd, type= 'trace')
mcmc_plot(N.alpha_sd, type= 'acf')

pp_check(N.alpha_sd)

plot.residuals <- cbind(alpha_sum_sd, residuals(N.alpha_sd))
plot.residuals <- as.data.frame(plot.residuals)

# plot residuals, Treatment
plot.residuals %>% 
  ggplot(aes(x= Treatment, y= Estimate))+
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

summary(N.alpha_sd)

conditional_effects(N.alpha_sd)

# S ~ Treatment * LUI----
# number of species vs Treatment LUI
# poisson error for species richness
boxplot(S ~ village, data = alpha_sum_sd) # village as a random effect

# S.alpha.rich_sd <-
#   brm(
#     S ~ Treatment * LUI + (1 | village),
#     family = poisson(),
#     data = alpha_sum_sd,
#     chains = 4,
#     warmup = 1000,
#     iter = 4000
#   )
# save(S.alpha.rich_sd, file= 'S.alpha.rich_sd.Rdata')

load('S.alpha.rich_sd.Rdata')

mcmc_plot(S.alpha.rich_sd,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'red')

mcmc_plot(S.alpha.rich_sd, type= 'trace')

mcmc_plot(S.alpha.rich_sd, type= 'acf')

pp_check(S.alpha.rich_sd)

plot.residuals <- cbind(alpha_sum_sd, residuals(S.alpha.rich_sd) )
plot.residuals <- as.data.frame(plot.residuals)
# plot residuals Treatment
plot.residuals %>% 
  ggplot(aes(x= Treatment, y= Estimate))+
  geom_boxplot()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')
# plot residuals LUI
plot.residuals %>% 
  ggplot(aes(x= LUI, y= Estimate))+
  geom_point()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')

# plot residuals species adults
plot.residuals %>% 
  ggplot(aes(x= Sp.adu, y= Estimate))+
  geom_point()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')

summary(S.alpha.rich_sd)

conditional_effects(S.alpha.rich_sd)

# Sn ~ Treatment----
# Rarefied richness vs Treatment, adult trees and LUI
# lognormal for Sn
boxplot(Sn ~ village, data = alpha_sum_sd)

alpha_sum_sd %>% 
  ggplot(aes(Sn))+
  geom_density()+
  geom_vline(aes(xintercept = mean(Sn)))+
  xlim(0.5,3)

alpha_sum_sd %>% 
  ggplot(aes(y=Sn, x= Treatment))+
  geom_boxplot()+
  stat_summary(geom = 'point',
               fun= mean,
               col= 'red')
  
# Sn_alpha_sd <- brm(
#   Sn ~ Treatment * LUI + (1 | village),
#   family = lognormal() ,
#   data = alpha_sum_sd,
#   cores = 4,
#   chains = 4, 
#   warmup = 1000, 
#   iter = 4000,
#   # control = list(adapt_delta = 0.9)
# )
# save(Sn_alpha_sd, file='Sn_alpha_sd.Rdata')

load('Sn_alpha_sd.Rdata')

mcmc_plot(Sn_alpha_sd,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'red')

mcmc_plot(Sn_alpha_sd, type= 'trace')
mcmc_plot(Sn_alpha_sd, type= 'acf')

pp_check(Sn_alpha_sd)

plot.residuals <- cbind(alpha_sum_sd, residuals(Sn_alpha_sd) )
plot.residuals <- as.data.frame(plot.residuals)
# plot residuals Treatment
plot.residuals %>% 
  ggplot(aes(x= Treatment, y= Estimate))+
  geom_boxplot()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')
# plot residuals LUI
plot.residuals %>% 
  ggplot(aes(x= LUI, y= Estimate))+
  geom_point()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')

# plot residuals species adults
plot.residuals %>% 
  ggplot(aes(x= Sp.adu, y= Estimate))+
  geom_point()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')

summary(Sn_alpha_sd)

conditional_effects(Sn_alpha_sd)

# ENSPIE ~ Treatment * LUI----
# lognormal for ENSPIE
hist(alpha_sum_sd$ENSPIE)

select(alpha_sum_sd, site, ENSPIE) %>% 
  unique %>% 
  count( site ) %>% 
  subset( n > 1 )

select(alpha_sum_sd, village, ENSPIE) %>% 
  unique %>% 
  count( village ) %>% 
  subset( n > 1 )


hist(alpha_sum_sd$ENSPIE) # use lognormal for ENSPIE

alpha_sum_sd %>% 
  ggplot(aes(x= ENSPIE))+
  geom_density()+
  geom_vline(aes(xintercept = mean(ENSPIE))) # use lognormal for ENSPIE

boxplot(ENSPIE ~ site, data = alpha_sum_sd) # for random effect
boxplot(ENSPIE ~ village, data = alpha_sum_sd) # for random effect

# ENSPIE_alpha_sd <- brm(
#   ENSPIE ~ Treatment * LUI + (1 | village),
#   # family= Gamma(),
#   family = lognormal(),
#   data = alpha_sum_sd,
#   cores = 4,
#   chains = 4,
#   warmup = 1000,
#   iter = 4000,
#   # control = list(adapt_delta = 0.9)
# )
# save(ENSPIE_alpha_sd, file = 'ENSPIE_alpha_sd.Rdata')

load('ENSPIE_alpha_sd.Rdata')

mcmc_plot(object = ENSPIE_alpha_sd, 
          type = 'areas', 
          prob= 0.95)+
  geom_vline(xintercept = 0, col= 'red')

mcmc_plot(ENSPIE_alpha_sd, type= 'trace')
mcmc_plot(ENSPIE_alpha_sd, type= 'acf')

pp_check(ENSPIE_alpha_sd, ndraws = 30)

# check model residual
plot.residuals <- cbind(alpha_sum_sd, residuals(ENSPIE_alpha_sd))
plot.residuals <- as.data.frame(plot.residuals)

# plot residuals Treatment
plot.residuals %>% 
  ggplot(aes(x= Treatment, y= Estimate))+
  geom_boxplot()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')
# plot residuals LUI
plot.residuals %>% 
  ggplot(aes(x= LUI, y= Estimate))+
  geom_point()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')

# plot residuals species adults
plot.residuals %>% 
  ggplot(aes(x= Sp.adu, y= Estimate))+
  geom_point()+
  geom_hline(yintercept = 0, lty= 2, col= 'red')


summary(ENSPIE_alpha_sd)

conditional_effects(ENSPIE_alpha_sd)  # conditional effects

# plot----
# ce= conditional effects of Treatment
N.alpha_sd_ce_lui <- conditional_effects(N.alpha_sd, effects = 'LUI:Treatment')

# N ~ Treatment * LUI
N <- ggplot() +
  geom_point(
    data = alpha_sum_sd,
    # raw data
    aes(x = LUI, # predicting variable
        y = N, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_line(
    data = N.alpha_sd_ce_lui$LUI,
    # conditional effect
    aes(
      x = LUI,
      # ce of the predicting variable
      y = estimate__,
      group = effect2__,
      color = effect2__
    ),
    linewidth = 1
  ) +
  geom_ribbon(
    data = N.alpha_sd_ce_lui$LUI,
    # conditional effect
    aes(
      x = LUI,
      # ce of the predicting variable
      ymin= lower__,
      ymax= upper__,
      y = estimate__,
      group = effect2__,
      fill = effect2__
    ), alpha= 0.3
  ) +
  labs(y= 'N', x= 'Land use intensity index',
       subtitle = ' ')+
  coord_cartesian(ylim = c(0, 610), xlim = c(0.21, 1))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position=" ")+
  labs(subtitle= 'a)')+
  guides(fill= 'none') # remove a section of the legend, here fill= effect__

N

S.alpha.rich_sd_lui <- conditional_effects(S.alpha.rich_sd, effects = 'LUI:Treatment')

# S ~ Treatment * LUI
S <- ggplot() +
  geom_point(
    data = alpha_sum_sd,
    # raw data
    aes(x = LUI, # predicting variable
        y = S, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_line(
    data = S.alpha.rich_sd_lui$LUI,
    # conditional effect
    aes(
      x = LUI,
      # ce of the predicting variable
      y = estimate__,
      group = effect2__,
      color = effect2__
    ),
    linewidth = 1
  ) +
  geom_ribbon(
    data = S.alpha.rich_sd_lui$LUI,
    # conditional effect
    aes(
      x = LUI,
      # ce of the predicting variable
      ymin= lower__,
      ymax= upper__,
      y = estimate__,
      group = effect2__,
      fill = effect2__
    ), alpha= 0.3
  )+
  labs(y= 'S', x= 'Land use intensity index',
       subtitle = ' ')+
  coord_cartesian(ylim = c(0, 6), xlim = c(0.5, 1.09))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(subtitle= 'b)')+
  guides(fill= 'none') # remove a section of the legend, here fill= effect__

S

Sn_alpha_sd_t_LUI <- conditional_effects(Sn_alpha_sd, effects = 'LUI:Treatment')

# Sn ~ Treatment * LUI
Sn <- ggplot() +
  geom_point(
    data = alpha_sum_sd,
    # raw data
    aes(x = LUI, # predicting variable
        y = Sn, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_line(
    data = Sn_alpha_sd_t_LUI$LUI,
    # conditional effect
    aes(
      x = LUI,
      # ce of the predicting variable
      y = estimate__,
      group = effect2__,
      color = effect2__
    ),
    linewidth = 1
  ) +
  geom_ribbon(
    data = Sn_alpha_sd_t_LUI$LUI,
    # conditional effect
    aes(
      x = LUI,
      # ce of the predicting variable
      ymin= lower__,
      ymax= upper__,
      y = estimate__,
      group = effect2__,
      fill = effect2__
    ), alpha= 0.3
  )+
  labs(y= 'Sn', x= 'Land use intensity index',
       subtitle = ' ')+
  coord_cartesian(ylim = c(0.5,3), xlim = c(0.5, 1.01))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position=" ")+
  labs(subtitle= 'c)')+
  guides(fill= 'none')+ # remove a section of the legend, here fill= effect__
  ylab(expression(S[n]))

Sn

ENSPIE_alpha_sd_t_LUI <- conditional_effects(ENSPIE_alpha_sd, effects = 'LUI:Treatment')

# ENSPIE ~ Treatment * LUI
ENSPIE <- ggplot() +
  geom_point(
    data = alpha_sum_sd,
    # raw data
    aes(x = LUI, # predicting variable
        y = ENSPIE, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_line(
    data = ENSPIE_alpha_sd_t_LUI$LUI,
    # conditional effect
    aes(
      x = LUI,
      # ce of the predicting variable
      y = estimate__,
      group = effect2__,
      color = effect2__
    ),
    linewidth = 1
  ) +
  geom_ribbon(
    data = ENSPIE_alpha_sd_t_LUI$LUI,
    # conditional effect
    aes(
      x = LUI,
      # ce of the predicting variable
      ymin= lower__,
      ymax= upper__,
      y = estimate__,
      group = effect2__,
      fill = effect2__
    ), alpha= 0.3
  )+
  labs(y= 'ENSPIE', x= 'Land use intensity index',
       subtitle = '')+
  coord_cartesian(ylim = c(1, 4), c(0.5, 1.01))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), 
                                  panel.grid.minor = element_blank(), 
                                  strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(subtitle= 'd)')+
  guides(fill= 'none')+ # remove a section of the legend, here fill= effect__
  ylab(expression(ENS[PIE]))

ENSPIE


ENSPIE_legend <- ENSPIE + theme(legend.position="bottom")

legendfig1 <- extract_legend(ENSPIE_legend)

figure2 <- (N|S)/(Sn|ENSPIE)/(legendfig1) + plot_layout(heights = c(10,10,2))

save(figure2, file='figure2.Rdata')

load('figure2.Rdata')
figure2

# end----