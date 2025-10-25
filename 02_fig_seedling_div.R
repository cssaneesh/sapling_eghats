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
    S_PIE = mobr::calc_PIE(abundance, replace = T),
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
  mutate(Sn = purrr::map(data, ~ mobr::rarefaction(.x$abundance, method = 'IBR', effort = unique(.x$minN)))) %>% 
  unnest(Sn)

# put Sn into the alpha_summary df
alpha_sum_sd <- inner_join(alpha_sum_sd %>% ungroup(), 
                            alpha_data_sd %>% 
                             dplyr::select(site, Treatment, Sn, village),
                            by = c('site', 'Treatment', 'village')
                           )
hist(alpha_sum_sd$Sn)

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
# save(N.alpha, file= 'output/N.alpha.Rdata')
# 
load('output/N.alpha.Rdata')

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

# expected decrease of seedlings in both present

N_bserved <- max(alpha_sum_sd$N)
# Calculate e^(-2.24), estimate of TreatmentCAFA:LUI
decrease <- exp(-2.24)
# Calculate the percentage decrease
(percentage_decrease <- 1 - decrease)* 100

# # expected decrease of seedlings in both absent

# LUI slope for CAFA is the sum of main LUI slope and the interaction term
# BETA CAFALUI = BETA LUI + BETA CAFA:LUI= -2.24 +(-2.32)= -4.56

BetaCAFALUI <- -2.24 + (-2.32) # BetaCAFA:LUI= -4.56 on the log scale
multiplicative_factor <- exp(BetaCAFALUI) # = 0.01046206
percentage_change <- round((1- multiplicative_factor) * 100) # = 99%

# 2. Species Richness (S) Considering Treatment and Land Use Intensity (LUI)

boxplot(S ~ village, data = alpha_sum_sd) # village as a random effect

S.alpha <-
  brm(
    S ~ Treatment * LUI + (1 | village),
    # family = poisson(), # poisson error for species richness
    family = student(),
    data = alpha_sum_sd,
    chains = 4,
    warmup = 1000,
    iter = 4000
  )
# save(S.alpha, file= 'output/S.alpha.Rdata')

load('output/S.alpha.Rdata')

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
#   # family = lognormal() ,
#   family = student(),
#   # family = mixture(student(), student()),
#   data = alpha_sum_sd,
#   cores = 4,
#   chains = 4,
#   warmup = 2000,
#   iter = 5000#,
#   # control = list(adapt_delta = 0.99, max_treedepth = 15)
# )
# save(Sn_alpha, file='output/Sn_alpha.Rdata')

load('output/Sn_alpha.Rdata')

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

hist(alpha_sum_sd$ENSPIE)

# equally abundant species in a perfectly even community (ENSPIE) Considering Treatment and Land Use Intensity (LUI)

# ENSPIE_alpha <- brm(
#   ENSPIE ~ Treatment * LUI + (1 | village),
#   # family= Gamma(),
#   # family = lognormal(),
#   family = student(),
#   data = alpha_sum_sd,
#   cores = 4,
#   chains = 4,
#   warmup = 1000,
#   iter = 4000,
#   # control = list(adapt_delta = 0.9)
# )
# save(ENSPIE_alpha, file = 'output/ENSPIE_alpha.Rdata')

load('output/ENSPIE_alpha.Rdata')

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
load(file='output/N.alpha.Rdata')
load(file='output/S.alpha.Rdata')
load(file='output/Sn_alpha.Rdata')
load(file='output/ENSPIE_alpha.Rdata')

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
  labs(y= 'Seedling individuals (N)', x= 'Land use intensity index',
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

# fig 2----
ENSPIE_legend <- ENSPIE + theme(legend.position="bottom")+
  # Use scale_color_manual to define the new legend labels
  scale_color_manual(
    name = "Treatment", # Keep the legend title if desired
    # Assign the new labels to the original factor levels in the correct order
    values = c("Control" = "#440154",  # Specify colors if needed, otherwise use the defaults
               "CPFA" = "#21908c",
               "CAFA" = "#fde725"),
    labels = c("Control" = "Both present",
               "CPFA" = "Fire present",
               "CAFA" = "Both excluded")
  )

  

legendfig1 <- extract_legend(ENSPIE_legend) # extract_legend, a custom function

figure2 <- (N|S)/(Sn|ENSPIE)/(legendfig1) + plot_layout(heights = c(10,10,2))

save(figure2, file='output/figure2.Rdata')

load('output/figure2.Rdata')
figure2

ggsave('output/figure2.jpg', figure2,
       width = 10,
       height = 6,
       dpi = 300)


# slope-----
# load models
load(file='output/N.alpha.Rdata')
load(file='output/S.alpha.Rdata')
load(file='output/Sn_alpha.Rdata')
load(file='output/ENSPIE_alpha.Rdata')

# make dfs
N.alpha_fitted.df <- cbind(N.alpha$data, fitted(N.alpha, re_formula = NA)) %>% 
  as_tibble() %>% left_join(alpha_sum_sd)

N.alpha_post.df <- as_draws_df(N.alpha, subset= floor(runif(n= 1000, 1, max = 2000))) %>% 
  select(contains('b_'))

head(N.alpha_post.df)

colnames(N.alpha_post.df)

n.alpha_post <- N.alpha_post.df %>% as_tibble() %>% 
  mutate(ctrl_slope= (`b_LUI`), # to get the slope for treat
         cpfa_slope= (`b_LUI`+ `b_TreatmentCPFA:LUI`),
         cafa_slope= (`b_LUI`+ `b_TreatmentCAFA:LUI`)
         
  ) %>% 
  dplyr::select(c(ctrl_slope, cpfa_slope, cafa_slope))

# view(n.alpha_post)
colnames(n.alpha_post)

N_control_slope <- n.alpha_post %>% 
  mutate(response = 'N', Treatment= 'Control',  slope = mean(ctrl_slope),
         slope_lower = quantile(ctrl_slope, probs = 0.025),
         slope_upper = quantile(ctrl_slope, probs = 0.975)) %>% 
  dplyr::select(c(response, Treatment, slope, slope_lower, slope_upper )) %>% 
  distinct()

# view(N_control_slope)

N_cpfa_slope <- n.alpha_post %>% 
  mutate(response = 'N', Treatment= 'CPFA',  slope = mean(cpfa_slope),
         slope_lower = quantile(cpfa_slope, probs = 0.025),
         slope_upper = quantile(cpfa_slope, probs = 0.975)) %>% 
  dplyr::select(c(response, Treatment, slope, slope_lower, slope_upper )) %>% 
  distinct()

# view(N_cpfa_slope)

N_cafa_slope <- n.alpha_post %>% 
  mutate(response = 'N', Treatment= 'CAFA',  slope = mean(cafa_slope),
         slope_lower = quantile(cafa_slope, probs = 0.025),
         slope_upper = quantile(cafa_slope, probs = 0.975)) %>% 
  dplyr::select(c(response, Treatment, slope, slope_lower, slope_upper )) %>% 
  distinct()

# view(N_cafa_slope)

# ----
S.alpha_post.df <- as_draws_df(S.alpha, subset= floor(runif(S= 1000, 1, max = 2000))) %>% 
  select(contains('b_'))

head(S.alpha_post.df, 2)

colnames(S.alpha_post.df)

S.alpha_post <- S.alpha_post.df %>% as_tibble() %>% 
  mutate(ctrl_slope= (`b_LUI`), # to get the slope for treat
         cpfa_slope= (`b_LUI`+ `b_TreatmentCPFA:LUI`),
         cafa_slope= (`b_LUI`+ `b_TreatmentCAFA:LUI`)
         
  ) %>% 
  dplyr::select(c(ctrl_slope, cpfa_slope, cafa_slope))

# view(S.alpha_post)
colnames(S.alpha_post)

S_control_slope <- S.alpha_post %>% 
  mutate(response = 'S', Treatment= 'Control',  slope = mean(ctrl_slope),
         slope_lower = quantile(ctrl_slope, probs = 0.025),
         slope_upper = quantile(ctrl_slope, probs = 0.975)) %>% 
  dplyr::select(c(response, Treatment, slope, slope_lower, slope_upper )) %>% 
  distinct()

# view(S_control_slope)

S_cpfa_slope <- S.alpha_post %>% 
  mutate(response = 'S', Treatment= 'CPFA',  slope = mean(cpfa_slope),
         slope_lower = quantile(cpfa_slope, probs = 0.025),
         slope_upper = quantile(cpfa_slope, probs = 0.975)) %>% 
  dplyr::select(c(response, Treatment, slope, slope_lower, slope_upper )) %>% 
  distinct()

# view(S_cpfa_slope)

S_cafa_slope <- S.alpha_post %>% 
  mutate(response = 'S', Treatment= 'CAFA',  slope = mean(cafa_slope),
         slope_lower = quantile(cafa_slope, probs = 0.025),
         slope_upper = quantile(cafa_slope, probs = 0.975)) %>% 
  dplyr::select(c(response, Treatment, slope, slope_lower, slope_upper )) %>% 
  distinct()

# view(S_cafa_slope)

# Sn-----
Sn.alpha_post.df <- as_draws_df(Sn_alpha, subset= floor(runif(S= 1000, 1, max = 2000))) %>% 
  select(contains('b_'))

head(Sn.alpha_post.df, 2)

colnames(Sn.alpha_post.df)

Sn.alpha_post <- Sn.alpha_post.df %>% as_tibble() %>% 
  mutate(ctrl_slope= (`b_LUI`), # to get the slope for treat
         cpfa_slope= (`b_LUI`+ `b_TreatmentCPFA:LUI`),
         cafa_slope= (`b_LUI`+ `b_TreatmentCAFA:LUI`)
         
  ) %>% 
  dplyr::select(c(ctrl_slope, cpfa_slope, cafa_slope))

# view(Sn.alpha_post)
colnames(Sn.alpha_post)

Sn_control_slope <- Sn.alpha_post %>% 
  mutate(response = 'Sn', Treatment= 'Control',  slope = mean(ctrl_slope),
         slope_lower = quantile(ctrl_slope, probs = 0.025),
         slope_upper = quantile(ctrl_slope, probs = 0.975)) %>% 
  dplyr::select(c(response, Treatment, slope, slope_lower, slope_upper )) %>% 
  distinct()

# view(Sn_control_slope)

Sn_cpfa_slope <- Sn.alpha_post %>% 
  mutate(response = 'Sn', Treatment= 'CPFA',  slope = mean(cpfa_slope),
         slope_lower = quantile(cpfa_slope, probs = 0.025),
         slope_upper = quantile(cpfa_slope, probs = 0.975)) %>% 
  dplyr::select(c(response, Treatment, slope, slope_lower, slope_upper )) %>% 
  distinct()

# view(Sn_cpfa_slope)

Sn_cafa_slope <- Sn.alpha_post %>% 
  mutate(response = 'Sn', Treatment= 'CAFA',  slope = mean(cafa_slope),
         slope_lower = quantile(cafa_slope, probs = 0.025),
         slope_upper = quantile(cafa_slope, probs = 0.975)) %>% 
  dplyr::select(c(response, Treatment, slope, slope_lower, slope_upper )) %>% 
  distinct()

# view(Sn_cafa_slope)

# ENSPIE
ENSPIE.alpha_post.df <- as_draws_df(ENSPIE_alpha, subset= floor(runif(S= 1000, 1, max = 2000))) %>% 
  select(contains('b_'))
head(ENSPIE.alpha_post.df)

colnames(ENSPIE.alpha_post.df)

ENSPIE.alpha_post <- ENSPIE.alpha_post.df %>% as_tibble() %>% 
  mutate(ctrl_slope= (`b_LUI`), # to get the slope for treat
         cpfa_slope= (`b_LUI`+ `b_TreatmentCPFA:LUI`),
         cafa_slope= (`b_LUI`+ `b_TreatmentCAFA:LUI`)
         
  ) %>% 
  dplyr::select(c(ctrl_slope, cpfa_slope, cafa_slope))

# view(ENSPIE.alpha_post)
colnames(ENSPIE.alpha_post)

ENSPIE_control_slope <- ENSPIE.alpha_post %>% 
  mutate(response = 'ENSPIE', Treatment= 'Control',  slope = mean(ctrl_slope),
         slope_lower = quantile(ctrl_slope, probs = 0.025),
         slope_upper = quantile(ctrl_slope, probs = 0.975)) %>% 
  dplyr::select(c(response, Treatment, slope, slope_lower, slope_upper )) %>% 
  distinct()

# view(ENSPIE_control_slope)

ENSPIE_cpfa_slope <- ENSPIE.alpha_post %>% 
  mutate(response = 'ENSPIE', Treatment= 'CPFA',  slope = mean(cpfa_slope),
         slope_lower = quantile(cpfa_slope, probs = 0.025),
         slope_upper = quantile(cpfa_slope, probs = 0.975)) %>% 
  dplyr::select(c(response, Treatment, slope, slope_lower, slope_upper )) %>% 
  distinct()

# view(ENSPIE_cpfa_slope)

ENSPIE_cafa_slope <- ENSPIE.alpha_post %>% 
  mutate(response = 'ENSPIE', Treatment= 'CAFA',  slope = mean(cafa_slope),
         slope_lower = quantile(cafa_slope, probs = 0.025),
         slope_upper = quantile(cafa_slope, probs = 0.975)) %>% 
  dplyr::select(c(response, Treatment, slope, slope_lower, slope_upper )) %>% 
  distinct()

# view(ENSPIE_cafa_slope)

# Slope for reporting-----
N_slope <- bind_rows(N_control_slope, N_cpfa_slope, N_cafa_slope ) %>% 
  mutate(Treatment = factor(Treatment )) %>% 
  mutate(Treatment = fct_relevel(Treatment, c("Control", "CPFA", "CAFA")))

S_slope <- bind_rows(S_control_slope, S_cpfa_slope, S_cafa_slope )%>% 
  mutate(Treatment = factor(Treatment )) %>% 
  mutate(Treatment = fct_relevel(Treatment, c("Control", "CPFA", "CAFA")))

Sn_slope <- bind_rows(Sn_control_slope, Sn_cpfa_slope, Sn_cafa_slope )%>% 
  mutate(Treatment = factor(Treatment )) %>% 
  mutate(Treatment = fct_relevel(Treatment, c("Control", "CPFA", "CAFA")))

ENSPIE_slope <- bind_rows(ENSPIE_control_slope, 
                          ENSPIE_cpfa_slope, 
                          ENSPIE_cafa_slope )%>% 
  mutate(Treatment = factor(Treatment )) %>% 
  mutate(Treatment = fct_relevel(Treatment, c("Control", "CPFA", "CAFA")))

# Table----

Table1 <- bind_rows(N_slope, S_slope, Sn_slope,
                   ENSPIE_slope )

Table1
# expected decrease of seedlings
(N_observed <- mean(alpha_sum_sd$N))
# Calculate e^(-4.56), estimate of CAFA:LUI
(decrease <-  exp(Table1[3,3]))
# Calculate the percentage decrease
(percentage_decrease <- 1 - decrease)
# N_exptected= N_observed*decrease
(N_exptected <- N_observed * (decrease + exp(Table1[1,3])))

# Slope S upp figs-----
# S1
N_slopeFig <- ggplot()+
  geom_point(data= N_slope,aes(x= Treatment, y= slope, col= Treatment ),
             size= 2)+
  geom_errorbar(data= N_slope, aes(x= Treatment, 
                                   ymin= slope_lower,
                                   ymax= slope_upper,
                                   col= Treatment),
                width= 0, linewidth = 0.7) + 
  geom_hline(yintercept = 0, 
             lty= 2)+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position=" ")+
  labs(subtitle= '(a)')+
  guides(fill= 'none')+ # remove a section of the legend, here fill= effect__
  labs(y = "Slope: N")+
  # Use scale_x_discrete to modify the x-axis labels
  scale_x_discrete(
    # The 'breaks' argument should list your original factor levels
    breaks = c("Control", "CPFA", "CAFA"),
    # The 'labels' argument provides the new names in the same order
    labels = c("Both present", "Fire present", "Both excluded")
  )

N_slopeFig

S_slopeFig <- ggplot()+
  geom_point(data= S_slope, aes(x= Treatment, y= slope, col= Treatment ),
             size= 2)+
  geom_errorbar(data= S_slope, aes(x= Treatment, 
                                   ymin= slope_lower,
                                   ymax= slope_upper,
                                   col= Treatment),
                width= 0, linewidth = 0.7) + 
  geom_hline(yintercept = 0, 
             lty= 2)+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(subtitle= '(b)')+
  guides(fill= 'none')+ # remove a section of the legend, here fill= effect__
  labs(y = "Slope: S ")+
  # Use scale_x_discrete to modify the x-axis labels
  scale_x_discrete(
    # The 'breaks' argument should list your original factor levels
    breaks = c("Control", "CPFA", "CAFA"),
    # The 'labels' argument provides the new names in the same order
    labels = c("Both present", "Fire present", "Both excluded")
  )

S_slopeFig

Sn_slopeFig <- ggplot()+
  geom_point(data= Sn_slope, aes(x= Treatment, y= slope, col= Treatment ),
             size= 2)+
  geom_errorbar(data= Sn_slope, aes(x= Treatment, 
                                    ymin= slope_lower,
                                    ymax= slope_upper,
                                    col= Treatment),
                width= 0, linewidth = 0.7) + 
  geom_hline(yintercept = 0, 
             lty= 2)+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position=" ")+
  labs(subtitle= '(c)')+
  guides(fill= 'none')+ # remove a section of the legend, here fill= effect__
  labs(y = expression(paste("Slope: ", S[n])))+
  # Use scale_x_discrete to modify the x-axis labels
  scale_x_discrete(
    # The 'breaks' argument should list your original factor levels
    breaks = c("Control", "CPFA", "CAFA"),
    # The 'labels' argument provides the new names in the same order
    labels = c("Both present", "Fire present", "Both excluded")
  )

Sn_slopeFig

ENSPIE_slopeFig <- ggplot()+
  geom_point(data= ENSPIE_slope, aes(x= Treatment, y= slope, col= Treatment ),
             size= 2)+
  geom_errorbar(data= ENSPIE_slope, aes(x= Treatment, 
                                        ymin= slope_lower,
                                        ymax= slope_upper,
                                        col= Treatment),
                width= 0, linewidth = 0.7) + 
  geom_hline(yintercept = 0, 
             lty= 2)+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position=" ")+
  labs(subtitle= '(d)')+
  guides(fill= 'none')+ # remove a section of the legend, here fill= effect__
  labs(y = expression(paste("Slope: ", ENS[PIE])))+
  # Use scale_x_discrete to modify the x-axis labels
  scale_x_discrete(
    # The 'breaks' argument should list your original factor levels
    breaks = c("Control", "CPFA", "CAFA"),
    # The 'labels' argument provides the new names in the same order
    labels = c("Both present", "Fire present", "Both excluded")
  )+
  # Use scale_color_manual to define the new legend labels
  scale_color_manual(
    name = "Treatment", # Keep the legend title if desired
    # Assign the new labels to the original factor levels in the correct order
    values = c("Control" = "#440154",  # Specify colors if needed, otherwise use the defaults
               "CPFA" = "#21908c",
               "CAFA" = "#fde725"),
    labels = c("Control" = "Both present",
               "CPFA" = "Fire present",
               "CAFA" = "Both excluded")
  )

ENSPIE_slopeFig

N_slopeFig 
S_slopeFig
Sn_slopeFig
ENSPIE_slopeFig

# extract legend
ENSPIE_legend <- ENSPIE_slopeFig + theme(legend.position="bottom")
legendfigS1 <- extract_legend(ENSPIE_legend) 

# plot
figureS2 <- (N_slopeFig|S_slopeFig)/(Sn_slopeFig|ENSPIE_slopeFig) + plot_layout(heights = c(10,10,2))

figureS2

save(figureS2, file='output/figureS2.Rdata')

load('output/figureS2.Rdata')

figureS2

ggsave('output/figureS2.jpg', figureS2,
       width = 10,
       height = 6,
       dpi = 300)

