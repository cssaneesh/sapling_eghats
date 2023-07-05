
# What is the proportion of saplings affected by disturbance 
# (browsing, trampling) and water stress and how does it vary for species?

source('00_sapling_data.R')

# Goat causing browsing and trampling
names(sap_status)
head(sap_status, 3)

# treatment and number of goats in a village as predictors of browsing
# sap_browse <- brm(Browsing ~ Treatment * Goat + (1|village), data= sap_status,
#                   family = bernoulli(link = "logit"),
#                   chains = 4,
#                   warmup = 1000,
#                   iter = 2000,
#                   thin = 1
#                   )
# save(sap_browse, file= 'sap_browse.Rdata')

load('sap_browse.Rdata')
pp_check(sap_browse)

# Model convergence
mcmc_plot(sap_browse,
          type = 'trace')

mcmc_plot(sap_browse,
          type = "acf_bar")

mcmc_plot(sap_browse,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')


summary(sap_browse)
conditional_effects(sap_browse)
conditional_effects(sap_browse, effects = 'Treatment:Goat')
browsing.goat.df <- conditional_effects(sap_browse, effects = 'Treatment:Goat')

browsing.goat.df
# result?
# the influence of the treatments on browsing by goats is consistent 
# regardless of the number of goats present.

# trampling
# treatment and number of goats in a village as predictors of trampling
# sap_tramp <- brm(Trampling ~ Treatment * Goat + (1|village), data= sap_status,
#                  family = bernoulli(link = "logit"),
#                  chains = 4,
#                  warmup = 1000,
#                  iter = 2000,
#                  thin = 1
#                  )
# save(sap_tramp, file= 'sap_tramp.Rdata')

load('sap_tramp.Rdata')
pp_check(sap_tramp)

# Model convergence
mcmc_plot(sap_tramp,
          type = 'trace')

mcmc_plot(sap_tramp,
          type = "acf_bar")

mcmc_plot(sap_tramp,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')


summary(sap_tramp)
conditional_effects(sap_tramp)

conditional_effects(sap_tramp, effects = 'Treatment:Goat')
trampling.goat.df <- conditional_effects(sap_tramp, effects = 'Treatment:Goat')

# partly dried plants observed due to water stress in sites with trenches
# sap_waterstress <- brm(Wat.stress ~ Treatment * Trenches+ (1|site), data= sap_status,
#                  family = bernoulli(link = "logit"),
#                  chains = 4,
#                  warmup = 1000,
#                  iter = 2000,
#                  thin = 1
#                  )
# save(sap_waterstress, file= 'sap_waterstress.Rdata')

load('sap_waterstress.Rdata')
pp_check(sap_waterstress)

# Model convergence
mcmc_plot(sap_waterstress,
          type = 'trace')

mcmc_plot(sap_waterstress,
          type = "acf_bar")

mcmc_plot(sap_waterstress,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')


summary(sap_waterstress)
conditional_effects(sap_waterstress)

waterstress.trench.df <- conditional_effects(sap_waterstress, effects = 'Treatment:Trenches' )


# sapstatus.no.dist <- brm(None ~ Treatment + (1|site), data= sap_status,
#                  family = bernoulli(link = "logit"),
#                  chains = 4,
#                  warmup = 1000,
#                  iter = 2000,
#                  thin = 1
#                  )
# save(sapstatus.no.dist, file= 'sapstatus.no.dist.Rdata')

load('sapstatus.no.dist.Rdata')
pp_check(sapstatus.no.dist)

# Model convergence
mcmc_plot(sapstatus.no.dist,
          type = 'trace')

mcmc_plot(sapstatus.no.dist,
          type = "acf_bar")

mcmc_plot(sapstatus.no.dist,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')


summary(sapstatus.no.dist)
conditional_effects(sapstatus.no.dist, effects = 'Treatment')

sapstatus.no.dist.df <- conditional_effects(sapstatus.no.dist, effects = 'Treatment')

# saplings with no disturbance and water stress is spread across all treatments.


# plot
browsing.goat.ce <- conditional_effects(sap_browse, effects = 'Treatment')
browsing.goat.df <- as.data.frame(browsing.goat.ce$Treatment)

trampling.goat.ce <- conditional_effects(sap_tramp, effects = 'Treatment')
trampling.goat.df <- as.data.frame(trampling.goat.ce$Treatment)

waterstress.trench.ce <- conditional_effects(sap_waterstress, effects = 'Treatment' )
waterstress.trench.df <- as.data.frame(waterstress.trench.ce$Treatment)

sapstatus.no.dist.de <- conditional_effects(sapstatus.no.dist, effects = 'Treatment')
sapstatus.no.dist.df <- as.data.frame(sapstatus.no.dist.de$Treatment)


browsing.goat <- ggplot() +
  geom_point(
    data = sap_status,
    # raw data
    aes(x = Treatment, # predicting variable
        y = Browsing, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_point(
    data = browsing.goat.df,
    aes(x = Treatment, y = estimate__, colour = Treatment),
    size = 3
  ) +
  geom_errorbar(
    data = browsing.goat.df,
    aes(
      x = Treatment,
      ymin = lower__,
      ymax = upper__,
      colour = Treatment
    ),
    linewidth = 1.3,
    width = 0.1
  )+
  coord_cartesian(ylim = c(0, 1))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(y= 'Browsing'  , subtitle= 'a)')+
  guides(fill= 'none')

browsing.goat

trampling.goat <- ggplot() +
  geom_point(
    data = sap_status,
    # raw data
    aes(x = Treatment, # predicting variable
        y = Trampling, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_point(
    data = trampling.goat.df,
    aes(x = Treatment, y = estimate__, colour = Treatment),
    size = 3
  ) +
  geom_errorbar(
    data = trampling.goat.df,
    aes(
      x = Treatment,
      ymin = lower__,
      ymax = upper__,
      colour = Treatment
    ),
    linewidth = 1.3,
    width = 0.1
  )+
  coord_cartesian(ylim = c(0, 1))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(y= 'Trampling'  , subtitle= 'b)')+
  guides(fill= 'none')

trampling.goat

waterstress.trench <- ggplot() +
  geom_point(
    data = sap_status,
    # raw data
    aes(x = Treatment, # predicting variable
        y = Wat.stress, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_point(
    data = waterstress.trench.df,
    aes(x = Treatment, y = estimate__, colour = Treatment),
    size = 3
  ) +
  geom_errorbar(
    data = waterstress.trench.df,
    aes(
      x = Treatment,
      ymin = lower__,
      ymax = upper__,
      colour = Treatment
    ),
    linewidth = 1.3,
    width = 0.1
  )+
  coord_cartesian(ylim = c(0, 1))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(y= 'Water stress'  , subtitle= 'c)')+
  guides(fill= 'none')

waterstress.trench

no.stress.plot <- ggplot() +
  geom_point(
    data = sap_status,
    # raw data
    aes(x = Treatment, # predicting variable
        y = None, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_point(
    data = sapstatus.no.dist.df,
    aes(x = Treatment, y = estimate__, colour = Treatment),
    size = 3
  ) +
  geom_errorbar(
    data = sapstatus.no.dist.df,
    aes(
      x = Treatment,
      ymin = lower__,
      ymax = upper__,
      colour = Treatment
    ),
    linewidth = 1.3,
    width = 0.1
  ) +
  coord_cartesian(ylim = c(0, 1))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(y= 'No stress & disturbance'  , subtitle= 'd)')+
  guides(fill= 'none')

no.stress.plot

