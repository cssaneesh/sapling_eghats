source('00_seedling_data.R')

# Question: 
# How do seedling abundance and diversity vary across treatments and 
# levels of land use intensity (LUI), both within and across sites?

# data preparation for calculating N, S, Sn and ENSPIE (alpha scale)
alpha_sum_sd <- seedling.dat %>%
  filter(LUI < 1.20) %>%  # filtered out high LUI
  group_by(site, Treatment, sci.name, village) %>%
   group_by(site, Treatment, village) %>%
  left_join(seedling.dat, multiple = "all") %>% 
  group_by(site, Treatment, sci.name, LUI, village) %>%
  summarise(abundance= sum(seedling), .groups = 'drop') %>% # abundance of seedling
  filter(abundance>0) %>% 
  # calculate metrics for each site
  group_by(site, Treatment, village, LUI) %>%
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
  summarise(N_sites = n_distinct(site))
# Control= 8, CPFA =10, CAFA = 17


# individual based rarefaction before fitting models----
# IBR is ideal for varying sampling efforts
alpha_data_sd <- seedling.dat %>% # sd= seedling
  filter(seedling>0) %>%  # get rid of all sites with adults but 0 seedlings e.g.APA15_CPFA & APA19_CPFA
  group_by(site, Treatment, sci.name) %>%
  summarise(abundance= sum(seedling), .groups = 'drop') %>%
  inner_join(alpha_sum_sd %>% ungroup() %>% 
               dplyr::select(site, LUI, village,  minN),
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
# 1. Number of individuals in response to Treatment and LUI

boxplot(N ~ site, data = alpha_sum_sd) # not using site as a random effect
boxplot(N ~ village, data = alpha_sum_sd) # village as a random effect

# N.alpha <-
#   brm(
#     N ~ Treatment * LUI + (1|village),
#     family = poisson(),
#     data = alpha_sum_sd,
#     chains = 4,
#     warmup = 1000,
#     iter = 4000)
# 
# save(N.alpha, file= 'N.alpha.Rdata')

load('N.alpha.Rdata')

mcmc_plot(N.alpha,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'red')

mcmc_plot(N.alpha, type= 'trace')
mcmc_plot(N.alpha, type= 'acf')

# pp_check
color_scheme_set("darkgray")
pp_check(N.alpha, ndraws = 30)+ # predicted vs. observed values
  xlab( "Number of seedlings") + ylab("Density")+
  theme_classic()+ 
  theme(legend.position = 'none') # predicted vs. observed values


summary(N.alpha)

conditional_effects(N.alpha, effects = "LUI:Treatment")

# expected decrease of seedlings
N_bserved <- max(alpha_sum_sd$N)
# Calculate e^(-2.27), estimate of TreatmentCAFA:LUI
decrease <- exp(-2.27)
# Calculate the percentage decrease
(percentage_decrease <- 1 - decrease)

# N_exptected= N_bserved*decrease
(N_exptected <- N_bserved*decrease)

# 2. Species Richness (S) Considering Treatment and Land Use Intensity (LUI)

boxplot(S ~ village, data = alpha_sum_sd) # village as a random effect

# S.alpha <-
#   brm(
#     S ~ Treatment * LUI + (1 | village),
#     family = poisson(), # poisson error for species richness
#     data = alpha_sum_sd,
#     chains = 4,
#     warmup = 1000,
#     iter = 4000
#   )
# save(S.alpha, file= 'S.alpha.Rdata')

load('S.alpha.Rdata')

mcmc_plot(S.alpha,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'red')

mcmc_plot(S.alpha, type= 'trace')

mcmc_plot(S.alpha, type= 'acf')

# pp_check
color_scheme_set("darkgray")
pp_check(S.alpha, ndraws = 30)+ # predicted vs. observed values
  xlab( "S") + ylab("Density")+
  theme_classic()+ 
  theme(legend.position = 'none') # predicted vs. observed values


summary(S.alpha)

conditional_effects(S.alpha, effects = 'LUI:Treatment')

# 3. Rarefied richness (Sn) Considering Treatment and Land Use Intensity (LUI)

boxplot(Sn ~ village, data = alpha_sum_sd)

alpha_sum_sd %>% 
  ggplot(aes(Sn))+
  geom_density()+
  geom_vline(aes(xintercept = mean(Sn)))+
  xlim(0.5,3) # right-skewed, lognormal for Sn

summary(alpha_sum_sd$Sn)

alpha_sum_sd %>% 
  ggplot(aes(y=Sn, x= Treatment))+
  geom_boxplot()+
  stat_summary(geom = 'point',
               fun= mean,
               col= 'red')
  
# Sn_alpha <- brm(
#   Sn ~ Treatment * LUI + (1 | village),
#   family = lognormal() ,
#   data = alpha_sum_sd,
#   cores = 4,
#   chains = 4,
#   warmup = 1000,
#   iter = 4000,
#   # control = list(adapt_delta = 0.9)
# )
# save(Sn_alpha, file='Sn_alpha.Rdata')

load('Sn_alpha.Rdata')

mcmc_plot(Sn_alpha,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'red')

mcmc_plot(Sn_alpha, type= 'trace')
mcmc_plot(Sn_alpha, type= 'acf')

# pp_check
color_scheme_set("darkgray")
pp_check(Sn_alpha, ndraws = 30)+ # predicted vs. observed values
  xlab(expression(S[n])) + ylab("Density")+
  theme_classic()+ 
  theme(legend.position = 'none') # predicted vs. observed values

summary(Sn_alpha)

conditional_effects(Sn_alpha, effects = 'LUI:Treatment')

# 4. Rarefied richness (Sn) Considering Treatment and Land Use Intensity (LUI)
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


# equally abundant species in a perfectly even community (ENSPIE) Considering Treatment and Land Use Intensity (LUI)
# ENSPIE_alpha <- brm(
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
# save(ENSPIE_alpha, file = 'ENSPIE_alpha.Rdata')

load('ENSPIE_alpha.Rdata')

mcmc_plot(object = ENSPIE_alpha, 
          type = 'areas', 
          prob= 0.95)+
  geom_vline(xintercept = 0, col= 'red')

mcmc_plot(ENSPIE_alpha, type= 'trace')
mcmc_plot(ENSPIE_alpha, type= 'acf')


# pp_check
color_scheme_set("darkgray")
pp_check(ENSPIE_alpha, ndraws = 30)+ # predicted vs. observed values
  # xlab( "ENSPIE") + 
 xlab(expression(ENS[PIE]))+
  ylab("Density")+
  theme_classic()+ 
  theme(legend.position = 'none') # predicted vs. observed values

summary(ENSPIE_alpha)

conditional_effects(ENSPIE_alpha, effects= 'LUI:Treatment')  # conditional effects


# load models----
load(file='N.alpha.Rdata')
load(file='S.alpha.Rdata')
load(file='Sn_alpha.Rdata')
load(file='ENSPIE_alpha.Rdata')

# make df for figures----
N.alpha.ce <- conditional_effects(N.alpha)
N.alpha.df <- as.data.frame(N.alpha.ce$`LUI:Treatment`)

S.alpha.ce <- conditional_effects(S.alpha)
S.alpha.df <- as.data.frame(S.alpha.ce$`LUI:Treatment`)

Sn_alpha.ce <- conditional_effects(Sn_alpha)
Sn_alpha.df <- as.data.frame(Sn_alpha.ce$`LUI:Treatment`)

ENSPIE_alpha.ce <- conditional_effects(ENSPIE_alpha)
ENSPIE_alpha.df <- as.data.frame(ENSPIE_alpha.ce$`LUI:Treatment`)

# plot----
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
    data = N.alpha.df,
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
    data = N.alpha.df,
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
  labs(subtitle= '(a)')+
  guides(fill= 'none') # remove a section of the legend, here fill= effect__

N

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
    data = S.alpha.df,
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
    data = S.alpha.df,
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
  labs(subtitle= '(b)')+
  guides(fill= 'none') # remove a section of the legend, here fill= effect__

S

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
    data = Sn_alpha.df,
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
    data = Sn_alpha.df,
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
  labs(subtitle= '(c)')+
  guides(fill= 'none')+ # remove a section of the legend, here fill= effect__
  ylab(expression(S[n]))

Sn

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
    data = ENSPIE_alpha.df,
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
    data = ENSPIE_alpha.df,
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
  labs(subtitle= '(d)')+
  guides(fill= 'none')+ # remove a section of the legend, here fill= effect__
  ylab(expression(ENS[PIE]))

ENSPIE


ENSPIE_legend <- ENSPIE + theme(legend.position="bottom")

legendfig1 <- extract_legend(ENSPIE_legend) # extract_legend, a custom function

figure2 <- (N|S)/(Sn|ENSPIE)/(legendfig1) + plot_layout(heights = c(10,10,2))

save(figure2, file='figure2.Rdata')

load('figure2.Rdata')
figure2

ggsave('figure2.jpg', figure2,
       width = 10,
       height = 6,
       dpi = 300)
