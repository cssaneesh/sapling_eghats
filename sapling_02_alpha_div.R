source('sapling_01_data.R')

# alpha diversity----
alpha_sum_sap <- visit_01.lui %>%
  group_by(site, Treatment, sci.name, village, 
           Goat, # relative number of goat for each village
           Trenches, # relative area (m) of trenches for each site
           # LUI
           ) %>%
  count(sci.name, name = 'abundance') %>%
  ungroup() %>%
  # calculate metrics for each site
  group_by(site, village, Treatment, 
           Goat,
           Trenches,
           # LUI
           ) %>%
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
  mutate(Treatment = fct_relevel(Treatment, c("Control", "CPFA", "CAFA")))%>% 
  # add the minimum number of individuals for calculating IBR
  mutate(minN = min(N))

View(alpha_sum_sap)
head(alpha_sum_sap)
tail(alpha_sum_sap)

# check are these data balanced? No, 28 sites in CAFA, 14 sites in CPFA and 13 sites in control.
# this is important for gamma- and beta-diversity comparisons
alpha_sum_sap %>% group_by(Treatment) %>% 
  summarise(N_sites=n_distinct(site))

# individual based rarefaction before fitting models----

alpha_data_sap <- visit_01.lui %>% # sap= saplings
  # first collate the transects at each unique location
  group_by(site, Treatment, sci.name) %>%
  count(sci.name, name = 'abundance') %>%
  ungroup() %>%
  inner_join(alpha_sum_sap %>% ungroup() %>% 
             dplyr::select(site, 
                           village, 
                           Goat,
                           Trenches,
                           # LUI, 
                           minN
                           ),
           by = c('site')) %>% 
  # next for calculating Sn
  group_by(site, 
           Goat,
           Trenches,
           # LUI
           ) %>% 
  nest(data=c(sci.name, abundance, minN)) %>% 
  ungroup() %>% 
  mutate(Sn = purrr::map(data, ~mobr::rarefaction(.x$abundance, method = 'IBR', effort = unique(.x$minN)))) %>% 
  unnest(Sn)
 
names(alpha_data_sap)

# put Sn into the alpha_summary df
alpha_sap_data <- inner_join(alpha_sum_sap %>% ungroup(), 
                             alpha_data_sap %>% 
                              dplyr::select(site, Treatment, Sn, 
                                            Goat,
                                            Trenches,
                                            # LUI
                                            ),
                            # by = c('site', 'Treatment', 'LUI')
                            )

names(alpha_sap_data)

# alpha_adul_sap <- alpha_sap_data %>% 
#   left_join(adult.dat, multiple = 'all') # count and abundance of adult trees 

# head(alpha_adul_sap, 3)
# tail(alpha_adul_sap, 3)

# exploratory plots----
# number of saplings vs. treatment, number of adult trees and LUI
mycol <- custom_theme <- function() {
  library(ggplot2)
  
  theme_set(
    theme_bw(base_size = 18) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour = "black", fill = "white"),
        legend.position = "bottom"
      )
  )
  
  scale_color_viridis <- function(discrete = TRUE, option = "D") {
    ggplot2::scale_color_viridis(discrete = discrete, option = option)
  }
  
  scale_fill_viridis <- function(discrete = TRUE, option = "D") {
    ggplot2::scale_fill_viridis(discrete = discrete, option = option)
  }
}

mycol() # my viridis function

# N ~ Treatment
ggplot(alpha_sap_data) +
  geom_point(aes (
    x = Treatment,
    y = N,
    group = Treatment,
    fill = Treatment,
    col = Treatment
  ),
  position = position_jitter()) +
  geom_boxplot(aes(x = Treatment,
                   y = N,
                   fill = Treatment), alpha = 0.5) +
  scale_color_viridis(discrete = T, option = 'D') +
  ylim (0, 40)

# N~ Treatment * Goat
ggplot(alpha_sap_data, aes(
  x= Goat, 
  y= N, 
  col= Treatment
  ))+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_color_viridis(discrete = T, option = 'D') +
  ylim(0,30)

# N ~ Treatment * Trenches
ggplot(alpha_sap_data, aes(
  x= Trenches, 
  y= N, 
  col= Treatment # we have different slopes and intercepts 
  ))+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_color_viridis(discrete = T, option = 'D') +
  ylim(0,50)

# S ~ Treatment
ggplot(alpha_sap_data) +
  geom_point(aes (
    x = Treatment,
    y = S,
    group = Treatment,
    fill = Treatment,
    col = Treatment
  ),
  position = position_jitter()) +
  geom_boxplot(aes(x = Treatment,
                   y = S,
                   fill = Treatment
                   ), alpha = 0.5) +
  scale_color_viridis(discrete = T, option = 'D') +
  coord_cartesian(ylim = c(0,8))
  # ylim (0, 8)

# S ~ Treatment * Goat
ggplot(alpha_sap_data, aes(
  x= Goat, 
  y= S, 
  col= Treatment))+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_color_viridis(discrete = T, option = 'D')+
  coord_cartesian(ylim = c(0,7))
 
# S ~ Treatment * Trenches 
ggplot(alpha_sap_data, aes(
  x= Trenches, 
  y= S, 
  col= Treatment
  ))+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_color_viridis(discrete = T, option = 'D')+
  ylim(0,7)



# For model selection----
# Number of saplings----
boxplot(N ~ site, data = alpha_sap_data)
boxplot(N ~ village, data = alpha_sap_data) # for random effect

hist(alpha_sap_data$N)

# N.Treat <-
#   brm(
#     N ~ Treatment + (1 | village),
#     family = poisson(),
#     data = alpha_sap_data,
#     cores = 4,
#     chains = 4,
#     control = list(adapt_delta = 0.9)
#   )
# save(N.Treat, file= 'N.Treat.Rdata')

load('N.Treat.Rdata')

summary(N.Treat)
conditional_effects(N.Treat)


# N.Treat_Trench <- # without interaction
#   brm(
#     N ~ Treatment + Trenches + (1 | village),
#     family = poisson(),
#     data = alpha_sap_data,
#     cores = 4,
#     chains = 4,
#     control = list(adapt_delta = 0.9)
#   )
# save(N.Treat_Trench, file= 'N.Treat_Trench.Rdata')

load('N.Treat_Trench.Rdata')

summary(N.Treat_Trench)
conditional_effects(N.Treat_Trench)
conditional_effects(N.Treat_Trench, effects= 'Treatment:Trenches')

# N.Treat.Trench <- #with interaction
#   brm(
#     N ~ Treatment * Trenches + (1 | village),
#     family = poisson(),
#     data = alpha_sap_data,
#     cores = 4,
#     chains = 4,
#     control = list(adapt_delta = 0.9)
#   )
# save(N.Treat.Trench, file= 'N.Treat.Trench.Rdata')

load('N.Treat.Trench.Rdata')
summary(N.Treat.Trench)
conditional_effects(N.Treat.Trench)
conditional_effects(N.Treat.Trench, effects = 'Trenches:Treatment')


# 






# model convergence
mcmc_plot(N.alpha.sap,
          type = 'trace')

mcmc_plot(N.alpha.sap,
          type = 'acf_bar')

mcmc_plot(N.alpha.sap,
          type = 'areas',
          prob = 0.95) +
  geom_vline(xintercept = 0, col = 'grey')


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
conditional_effects(N.alpha.sap, effects = 'Treatment:Trenches')

# ce= conditional effects of treatment
N.alpha.sap_ce_t <- conditional_effects(N.alpha.sap, effects = 'Treatment')
N.alpha.sap_ce_lui <- conditional_effects(N.alpha.sap, effects = 'LUI:Treatment')
N.alpha.sap_ce_Nu.adul <- conditional_effects(N.alpha.sap, effects = 'Nu.adu:Treatment')


# N ~ treatment
ggplot() +
  geom_point(
    data = alpha_adul_sap,
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
    data = N.alpha.sap_ce_t$Treatment,
    # conditional effect
    aes(x = Treatment, # ce of the predicting variable
        y = estimate__,
        col = Treatment),
    size = 3
  ) +
  geom_errorbar(
    data = N.alpha.sap_ce_t$Treatment,
    aes(
      x = Treatment,
      ymin = lower__,
      ymax = upper__,
      col = Treatment
    ),
    linewidth = 1.3,
    width = 0.1
  )+ labs(y= 'Number of sapings', x= ' ',
         subtitle = 'N ~ Treatment * LUI + Nu.adu + (1 | village)')+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") + ylim (0,150)

# N ~ Treatment * LUI
ggplot() +
  geom_point(
    data = alpha_adul_sap,
    # raw data
    aes(x = LUI, # predicting variable
        y = N, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_line(
    data = N.alpha.sap_ce_lui$LUI,
    # conditional effect
    aes(
      x = LUI,
      # ce of the predicting variable
      y = estimate__,
      group = effect2__,
      color = effect2__
    ),
    linewidth = 1
  )+
  geom_ribbon(
    data = N.alpha.sap_ce_lui$LUI,
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
  ) +
  labs(y= 'Number of saplinglings', x= 'Land use intensity index',
       subtitle = 'N ~ Treatment * LUI + Nu.adu + (1 | village)')+
  coord_cartesian(ylim = c(0,50))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom")+
  guides(fill= 'none') # remove a section of the legend, here fill= effect__

# N ~ number of adult trees
ggplot() +
  geom_point(
    data = alpha_adul_sap,
    # raw data
    aes(x = LUI, # predicting variable
        y = N, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_line(
    data = N.alpha.sap_ce_Nu.adul$Nu.adu,
    # conditional effect
    aes(x = Nu.adu, # ce of the predicting variable
        y = estimate__,  group= effect2__, color =effect2__),
    linewidth = 1)+
  geom_ribbon(
    data = N.alpha.sap_ce_Nu.adul$Nu.adu,
    # conditional effect
    aes(x = Nu.adu, # ce of the predicting variable
        ymin= lower__,
        ymax= upper__,
        y = estimate__,  group= effect2__, fill =effect2__), alpha= 0.5
  ) +
  labs(y= 'Number of seedlings', x= 'Number of adult trees') +
  coord_cartesian(ylim = c(0,50))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom")+ guides(fill= 'none')

  

# number of species vs treatment and LUI
# poisson error for species richness

S.alpha.rich.sap <-
  brm(
    S ~ Treatment * LUI + Sp.adu + (1 | village),
    family = poisson(),
    data = alpha_adul_sap,
    cores = 4,
    chains = 4,
    control = list(adapt_delta = 0.9)
  )
save(S.alpha.rich.sap, file= 'S.alpha.rich.sap.Rdata')

load('S.alpha.rich.sap.Rdata')

pp_check(S.alpha.rich.sap)
plot(S.alpha.rich.sap)


# model convergence
mcmc_plot(S.alpha.rich.sap,
          type = 'trace')

mcmc_plot(S.alpha.rich.sap,
          type = 'acf_bar')

mcmc_plot(S.alpha.rich.sap,
          type = 'areas',
          prob = 0.95) +
  geom_vline(xintercept = 0, col = 'grey')


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
S.alpha.rich.sap_ce_t <- conditional_effects(S.alpha.rich.sap, effects = 'Treatment')
S.alpha.rich.sap_ce_lui <- conditional_effects(S.alpha.rich.sap, effects = 'LUI:Treatment')
S.alpha.rich.sap_ce_Sp.adul <- conditional_effects(S.alpha.rich.sap, effects = 'Sp.adu:Treatment')

# S ~ Treatment * LUI + Sp.adu + (1 | village)
ggplot() +
  geom_point(
    data = alpha_adul_sap, # observed data
    # raw data
    aes(x = Treatment, # predicting variable
        y = S, # response variable
    ),
    color = "grey",
    size = 1.2,
    alpha = 0.9,
    position = position_jitter(width = 0.05, height = 0.45)
  ) + geom_point(
    data = S.alpha.rich.sap_ce_t$Treatment,
    # conditional effect
    aes(x = Treatment, # ce of the predicting variable
        y = estimate__,
        col = Treatment),
    size = 3
  )+ geom_errorbar(
    data = S.alpha.rich.sap_ce_t$Treatment,
    aes(
      x = Treatment,
      ymin = lower__,
      ymax = upper__,
      col = Treatment
    ),
    linewidth = 1.3,
    width = 0.1
  ) + labs(y= 'Number of sapings species', x= ' ',
          subtitle = 'S ~ Treatment * LUI + Sp.adu + (1 | village)')+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") + ylim (0,7.5)

# S ~ LUI: Treatment
ggplot() +
  geom_point(
    data = alpha_adul_sap,
    # raw data
    aes(x = LUI, # predicting variable
        y = N, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_line(
    data = S.alpha.rich.sap_ce_lui$LUI,
    # conditional effect
    aes(
      x = LUI,
      # ce of the predicting variable
      y = estimate__,
      group = effect2__,
      color = effect2__
    ),
    linewidth = 1
  )+
  geom_ribbon(
    data = S.alpha.rich.sap_ce_lui$LUI,
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
  ) +
  labs(y= 'Number of sapling species', x= 'Land use intensity index',
       subtitle = 'S ~ Treatment * LUI + Sp.adu + (1 | village)')+
  coord_cartesian(ylim = c(0,20))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom")+
  guides(fill= 'none') # remove a section of the legend, here fill= effect__


# Sn ~ richness of adult tree species

# Rarefied richness vs treatment, LUI, number of adult species and village as random effect
# lognormal for Sn

Sn_alpha.sap <- brm(Sn ~ Treatment * LUI + Sp.adu + (1 | village) ,
                            family = lognormal(),
                            data = alpha_adul_sap,
                            cores = 4,
                            chains = 5,
                            control = list(adapt_delta = 0.9)
                              )
save(Sn_alpha.sap, file='Sn_alpha.sap.Rdata')

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
Sn_alpha.sap_ce_t <- conditional_effects(Sn_alpha.sap, effects = 'Treatment')
Sn_alpha.sap_ce_lui <- conditional_effects(Sn_alpha.sap, effects = 'LUI:Treatment')
Sn_alpha.sap_ce_Sp.adul <- conditional_effects(Sn_alpha.sap, effects = 'Sp.adu:Treatment')

# Sn ~ Treatment * LUI + Sp.adu + (1 | village)
ggplot() +
  geom_point(
    data = alpha_adul_sap, # observed data
    # raw data
    aes(x = Treatment, # predicting variable
        y = S, # response variable
    ),
    color = "grey",
    size = 1.2,
    alpha = 0.9,
    position = position_jitter(width = 0.05, height = 0.45)
  ) + geom_point(
    data = Sn_alpha.sap_ce_t$Treatment,
    # conditional effect
    aes(x = Treatment, # ce of the predicting variable
        y = estimate__,
        col = Treatment),
    size = 3
  )+ geom_errorbar(
    data = Sn_alpha.sap_ce_t$Treatment,
    aes(
      x = Treatment,
      ymin = lower__,
      ymax = upper__,
      col = Treatment
    ),
    linewidth = 1.3,
    width = 0.1
  ) + labs(y= 'Sn', x= ' ',
           subtitle = 'Sn ~ Treatment * LUI + Sp.adu + (1 | village)')+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") + ylim (0,7.5)

# Sn ~ LUI: Treatment
ggplot() +
  geom_point(
    data = alpha_adul_sap,
    # raw data
    aes(x = LUI, # predicting variable
        y = N, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_line(
    data = Sn_alpha.sap_ce_lui$LUI,
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
    data = Sn_alpha.sap_ce_lui$LUI,
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
  ) +
  labs(y= 'Sn', x= 'Land use intensity index',
       subtitle = 'Sn ~ Treatment * LUI + Sp.adu + (1 | village)')+
  coord_cartesian(ylim = c(0,50))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom")+
  guides(fill= 'none') # remove a section of the legend, here fill= effect__

# ENSPIE
ENSPIE_alpha.sap <- brm(ENSPIE ~ Treatment * LUI + Nu.adu + (1 | village) ,
                         family = lognormal(),
                         data = alpha_adul_sap,
                         cores = 4,
                         chains = 4,
                         control = list(adapt_delta = 0.9)
)
save(ENSPIE_alpha.sap, file = 'ENSPIE_alpha.sap.Rdata')

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
ENSPIE_alpha.sap_ce_t <- conditional_effects(ENSPIE_alpha.sap, effects = 'Treatment')
ENSPIE_alpha.sap_ce_lui <- conditional_effects(ENSPIE_alpha.sap, effects = 'LUI:Treatment')
ENSPIE_alpha.sap_ce_Nu.adul <- conditional_effects(ENSPIE_alpha.sap, effects = 'Nu.adu:Treatment')

# ENSPIE ~ treatment
ggplot() +
  geom_point(
    data = alpha_adul_sap, # observed data
    # raw data
    aes(x = Treatment, # predicting variable
        y = S, # response variable
    ),
    color = "grey",
    size = 1.2,
    alpha = 0.9,
    position = position_jitter(width = 0.05, height = 0.45)
  ) + geom_point(
    data = ENSPIE_alpha.sap_ce_t$Treatment,
    # conditional effect
    aes(x = Treatment, # ce of the predicting variable
        y = estimate__,
        col = Treatment),
    size = 3
  ) + geom_errorbar(
    data = ENSPIE_alpha.sap_ce_t$Treatment,
    aes(
      x = Treatment,
      ymin = lower__,
      ymax = upper__,
      col = Treatment
    ),
    linewidth = 1.3,
    width = 0.1
  ) + labs(y= 'ENSPIE', x= ' ',
           subtitle = 'ENSPIE ~ Treatment * LUI + Sp.adu + (1 | village)')+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") + ylim (0,7.5)

# ENSPIE ~ LUI: Treatment
ggplot() +
  geom_point(
    data = alpha_adul_sap,
    # raw data
    aes(x = LUI, # predicting variable
        y = N, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_line(
    data = ENSPIE_alpha.sap_ce_lui$LUI,
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
    data = ENSPIE_alpha.sap_ce_lui$LUI,
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
  ) +
  labs(y= 'ENSPIE', x= 'Land use intensity index',
       subtitle = 'ENSPIE ~ Treatment * LUI + Sp.adu + (1 | village)')+
  coord_cartesian(ylim = c(0,50))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom")+
  guides(fill= 'none') # remove a section of the legend, here fill= effect__


