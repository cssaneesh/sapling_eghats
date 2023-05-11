
source('seedling_01_data.R')

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
  group_by(site, Treatment, sci.name, village) %>%
  summarise(abundance= sum(adult),.groups = 'drop') %>% # abundance of adult trees
  group_by(site, Treatment, village) %>%
  summarise (Sp.adu = n_distinct(sci.name),
             # number of unique species/richness
             Nu.adu = sum(abundance),
             # total number of adult trees
             .groups = "drop") %>% # add the total number of adults in each site
  left_join(seedling.dat %>% select(!adult), multiple = "all") %>% 
  group_by(site, Treatment, sci.name, LUI, Sp.adu, Nu.adu, village) %>%
  summarise(abundance= sum(seedling), .groups = 'drop') %>% # abundance of seedling
  filter(abundance>0) %>% 
  # calculate metrics for each site
  group_by(site, Treatment, village, LUI, Sp.adu, Nu.adu) %>%
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
# this is important for gamma- and beta-diversity comparisons
alpha_sum_sd %>% group_by(Treatment) %>% 
  summarise(N_sites=n_distinct(site))

# individual based rarefaction before fitting models----

alpha_data_sd <- seedling.dat %>% # sd= seedling
  select(-adu.stat) %>% 
  filter(seedling>0) %>%  # get rid of all sites with adults but 0 seedlings
  group_by(site, Treatment, sci.name) %>%
  summarise(abundance= sum(seedling), .groups = 'drop') %>%
  inner_join(alpha_sum_sd %>% ungroup() %>% 
               dplyr::select(site, LUI, Sp.adu, Nu.adu, village,  minN),
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
# N ~ Treatment----
names(alpha_sum_sd)
# number of individuals vs. Treatment, LUI and number of adult trees 

boxplot(N ~ site, data = alpha_sum_sd) # not using site as a random effect
boxplot(N ~ village, data = alpha_sum_sd) # village as a random effect

# N.alpha_sd <-
#   brm(
#     N ~ Treatment * LUI + Nu.adu + (1|village),
#     family = poisson(),
#     data = alpha_sum_sd,
#     cores = 4,
#     chains = 4,
#     control = list(adapt_delta = 0.9)
#   )
# save(N.alpha_sd, file= 'N.alpha_sd.Rdata')

load('N.alpha_sd.Rdata')

pp_check(N.alpha_sd)
plot(N.alpha_sd)
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

# ce= conditional effects of Treatment
N.alpha_sd_ce_t <- conditional_effects(N.alpha_sd, effects = 'Treatment')
N.alpha_sd_ce_lui <- conditional_effects(N.alpha_sd, effects = 'LUI:Treatment')
N.alpha_sd_ce_Nu.adul <- conditional_effects(N.alpha_sd, effects = 'Nu.adu:Treatment') 

head(N.alpha_sd_ce_Nu.adul)

# N ~ Treatment
ggplot() +
  geom_point(
    data = alpha_sum_sd,
    # raw data
    aes(x = Treatment, # predicting variable
        y = N, # response variable
        ),
    color = "grey",
    size = 1.2,
    alpha = 0.9,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_point(
    data = N.alpha_sd_ce_t$Treatment,
    # conditional effect
    aes(x = Treatment, # ce of the predicting variable
        y = estimate__,
        col = Treatment),
    size = 3
  ) +
  geom_errorbar(
    data = N.alpha_sd_ce_t$Treatment,
    aes(
      x = Treatment,
      ymin = lower__,
      ymax = upper__,
      col = Treatment
    ),
    linewidth = 1.3,
    width = 0.1
  ) +
  labs(y= 'Number of seedlings', x= ' ',
       subtitle = 'N ~ Treatment * LUI + Nu.adu + (1|village)')+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") + ylim (0,150)

# N ~ Treatment * LUI
ggplot() +
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
    ), alpha= 0.5
  )+
labs(y= 'Number of seedlings', x= 'Land use intensity index',
     subtitle = 'N ~ Treatment * LUI + Nu.adu + (1|village)')+
  coord_cartesian(ylim = c(0,325))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom")+
  guides(fill= 'none') # remove a section of the legend, here fill= effect__

# N ~ number of adult trees
ggplot() +
  geom_point(
    data = alpha_sum_sd,
    # raw data
    aes(x = Nu.adu, # predicting variable
        y = N, # response variable
        col= Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  )+
  geom_line(
    data = N.alpha_sd_ce_Nu.adul$Nu.adu,
    # conditional effect
    aes(x = Nu.adu, # ce of the predicting variable
        y = estimate__,  group= effect2__, color =effect2__),
    linewidth = 1)+
  geom_ribbon(
    data = N.alpha_sd_ce_Nu.adul$Nu.adu,
    # conditional effect
    aes(x = Nu.adu, # ce of the predicting variable
        ymin= lower__,
        ymax= upper__,
        y = estimate__,  group= effect2__, fill =effect2__), alpha= 0.5
    )+
  labs(y= 'Number of seedlings', x= 'Number of adult trees') +
  coord_cartesian(ylim = c(0,200))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom")+ guides(fill= 'none')

# S ~ Treatment----
# number of species vs Treatment LUI and number of adult trees and species
# poisson error for species richness
boxplot(S ~ village, data = alpha_sum_sd) # village as a random effect

# For S, I removed Nu.adu and added Sp.adu

# S.alpha.rich_sd <-
#   brm(
#     S ~ Treatment * LUI + Sp.adu + (1 | village),
#     family = poisson(),
#     data = alpha_sum_sd,
#     cores = 4,
#     chains = 4,
#     control = list(adapt_delta = 0.9)
#   )
# save(S.alpha.rich_sd, file= 'S.alpha.rich_sd.Rdata')

load('S.alpha.rich_sd.Rdata')

pp_check(S.alpha.rich_sd)
plot(S.alpha.rich_sd)

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

S.alpha.rich_sd_t <- conditional_effects(S.alpha.rich_sd, effects = 'Treatment')
S.alpha.rich_sd_lui <- conditional_effects(S.alpha.rich_sd, effects = 'LUI:Treatment')
S.alpha.rich_sd_Sp.adu <- conditional_effects(S.alpha.rich_sd, effects = 'Sp.adu:Treatment') 


# plot S ~ Treatment
ggplot() +
  geom_point(
    data = alpha_sum_sd,
    # raw data
    aes(x = Treatment, # predicting variable
        y = S, # response variable
    ),
    color = "grey",
    size = 1.2,
    alpha = 0.9,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_point(
    data = S.alpha.rich_sd_t$Treatment,
    # conditional effect
    aes(x = Treatment, # ce of the predicting variable
        y = estimate__,
        col = Treatment),
    size = 3
  ) +
  geom_errorbar(
    data = S.alpha.rich_sd_t$Treatment,
    aes(
      x = Treatment,
      ymin = lower__,
      ymax = upper__,
      col = Treatment
    ),
    linewidth = 1.3,
    width = 0.1
  ) +
  labs(y= 'Number of seedling species', x= ' ',
       subtitle = 'S ~ Treatment * LUI + Sp.adu + (1|village)')+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") + ylim (0,7)

# S ~ Treatment * LUI
ggplot() +
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
    ), alpha= 0.5
  )+
  labs(y= 'Number of seedling species', x= 'Land use intensity index',
       subtitle = 'S ~ Treatment * LUI + Sp.adu + (1|village)')+
  coord_cartesian(ylim = c(0,12))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom")+
  guides(fill= 'none') # remove a section of the legend, here fill= effect__



# S ~ Sp.adu
# ggplot() +
#   geom_point(
#     data = alpha_sum_sd,
#     # raw data
#     aes(x = Sp.adu, # predicting variable
#         y = S, # response variable
#         col= Treatment),
#     size = 1.2,
#     alpha = 0.5,
#     position = position_jitter(width = 0.05, height = 0.45)
#   )+
#   geom_smooth(
#     data = S.alpha.rich_sd_Sp.adu$Sp.adu,
#     # conditional effect
#     aes(x = Sp.adu, # ce of the predicting variable
#         y = estimate__),
#     linewidth = 1, method = 'lm')+
#   labs(y= 'Number of seedling species', x= 'Number of adult tree species')+
#   scale_color_viridis(discrete = T, option="D")  +
#   scale_fill_viridis(discrete = T, option="D")  +
#   theme_bw(base_size=18 )+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
#                                  legend.position="bottom")

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
  
# For Sn, I removed Nu.adu and added Sp.adu

# Sn_alpha_sd <- brm(
#   Sn ~ Treatment * LUI + Sp.adu + (1 | village),
#   family = lognormal() ,
#   data = alpha_sum_sd,
#   cores = 4,
#   chains = 4,
#   control = list(adapt_delta = 0.9)
# )
# save(Sn_alpha_sd, file='Sn_alpha_sd.Rdata')

load('Sn_alpha_sd.Rdata')

pp_check(Sn_alpha_sd)
plot(Sn_alpha_sd)

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

Sn_alpha_sd_t <- conditional_effects(Sn_alpha_sd, effects = 'Treatment')
Sn_alpha_sd_t_LUI <- conditional_effects(Sn_alpha_sd, effects = 'LUI:Treatment')
Sn_alpha_sd_t_Sp.adu <- conditional_effects(Sn_alpha_sd, effects = 'Sp.adu:Treatment')


# Sn ~ Treatment
ggplot() +
  geom_point(
    data = alpha_sum_sd,
    # raw data
    aes(x = Treatment, # predicting variable
        y = Sn, # response variable, no of species observed at each site
    ),
    col= 'grey',
    size = 1.2,
    alpha = 0.9,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_point(
    data = Sn_alpha_sd_t$Treatment,
    # conditional effect
    aes(x = Treatment, # ce of the predicting variable
        y = estimate__,
        col = Treatment),
    size = 3
  ) +
  geom_errorbar(
    data = Sn_alpha_sd_t$Treatment,
    aes(
      x = Treatment,
      ymin = lower__,
      ymax = upper__,
      col = Treatment
    ),
    linewidth = 1.3,
    width = 0.1
  ) +
  labs(y= 'Sn', x= '',
       subtitle = 'Sn ~ Treatment * LUI + Sp.adu + (1 | village)')+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom")

# Sn ~ Treatment * LUI
ggplot() +
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
    ), alpha= 0.5
  )+
  labs(y= 'Sn', x= 'Land use intensity index',
       subtitle = 'Sn ~ Treatment * LUI + Sp.adu + (1|village)')+
  coord_cartesian(ylim = c(0,5))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom")+
  guides(fill= 'none') # remove a section of the legend, here fill= effect__

# Sn ~ Sp.adu
# ggplot() +
#   geom_point(
#     data = alpha_sum_sd,
#     # raw data
#     aes(x = Sp.adu, # predicting variable
#         y = Sn, # response variable
#         col= Treatment),
#     size = 1.2,
#     alpha = 0.5,
#     position = position_jitter(width = 0.05, height = 0.45)
#   )+
#   geom_smooth(
#     data = Sn_alpha_sd_t_Sp.adu$Sp.adu,
#     # conditional effect
#     aes(x = Sp.adu, # ce of the predicting variable
#         y = estimate__),
#     linewidth = 1, method = 'lm')+
#   labs(x= 'Number of adult tree species' )+
#   scale_color_viridis(discrete = T, option="D")  + 
#   scale_fill_viridis(discrete = T, option="D")  + 
#   theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
#                                   legend.position="bottom")


# ENSPIE ~ Treatment----
# evenness vs Treatment, adult trees and LUI
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
#   ENSPIE ~ Treatment * LUI +  Sp.adu + (1 | village),
#   # family= Gamma(),
#   family = lognormal(),
#   data = alpha_sum_sd,
#   cores = 4,
#   chains = 4,
#   control = list(adapt_delta = 0.9)
# )
# save(ENSPIE_alpha_sd, file = 'ENSPIE_alpha_sd.Rdata')

load('ENSPIE_alpha_sd.Rdata')

pp_check(ENSPIE_alpha_sd, ndraws = 30)
plot(ENSPIE_alpha_sd)

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
# seedling evenness-----
conditional_effects(ENSPIE_alpha_sd)  # conditional effects

ENSPIE_alpha_sd_t <- conditional_effects(ENSPIE_alpha_sd, effects = 'Treatment')
ENSPIE_alpha_sd_t_LUI <- conditional_effects(ENSPIE_alpha_sd, effects = 'LUI:Treatment')
ENSPIE_alpha_sd_t_Sp.adu <- conditional_effects(ENSPIE_alpha_sd, effects = 'Sp.adu:Treatment')

# ENSPIE ~ Treatment
ggplot() +
  geom_point(
    data = alpha_sum_sd,
    # raw data
    aes(x = Treatment, # predicting variable
        y = ENSPIE, # response variable, no of species observed at each site
    ),
    col= 'grey',
    size = 1.2,
    alpha = 0.9,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_point(
    data = ENSPIE_alpha_sd_t$Treatment,
    # conditional effect
    aes(x = Treatment, # ce of the predicting variable
        y = estimate__,
        col = Treatment),
    size = 3
  ) +
  geom_errorbar(
    data = ENSPIE_alpha_sd_t$Treatment,
    aes(
      x = Treatment,
      ymin = lower__,
      ymax = upper__,
      col = Treatment
    ),
    linewidth = 1.3,
    width = 0.1) +
  labs(y= 'ENSPIE', x= ' ',
       subtitle = 'ENSPIE ~ Treatment * LUI + Sp.adu + (1 | village)')+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") + ylim (0,4)


# ENSPIE ~ Treatment * LUI
ggplot() +
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
    ), alpha= 0.5
  )+
  labs(y= 'ENSPIE', x= 'Land use intensity index',
       subtitle = 'ENSPIE ~ Treatment * LUI + Sp.adu + (1 | village)')+
  coord_cartesian(ylim = c(0,8))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom")+
  guides(fill= 'none') # remove a section of the legend, here fill= effect__

# ENSPIE ~ Sp.adu
ggplot() +
  geom_point(
    data = alpha_sum_sd,
    # raw data
    aes(x = Sp.adu, # predicting variable
        y = ENSPIE, # response variable
        col= Treatment),
    size = 1.2,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  )+
  geom_smooth(
    data = ENSPIE_alpha_sd_t_Sp.adu$Sp.adu,
    # conditional effect
    aes(x = Sp.adu, # ce of the predicting variable
        y = estimate__),
    linewidth = 1, method = 'lm')+scale_color_viridis(discrete = T, option = 'D')+
  scale_fill_viridis(discrete = T, option= 'D')+
  theme_bw(base_size = 18)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        strip.background = element_rect(color = 'black', fill = 'white'),
        legend.position= 'bottom')

# plots----


