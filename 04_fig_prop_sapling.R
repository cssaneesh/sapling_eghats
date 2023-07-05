source('00_sapling_data.R')

names(sap_status)
com.species %>% group_by(Treatment, sci.name) %>% 
  summarise(n()) # %>% View()


# species vs browsing----
# sp.browse <- brm(Browsing ~ sci.name+ (1|site), data= com.species,
#                   family = bernoulli(link = "logit"),
#                   chains = 4,
#                   warmup = 1000,
#                   iter = 4000,
#                   thin = 1
#                   )
# save(sp.browse, file= 'sp.browse.Rdata')

load(file= 'sp.browse.Rdata')
pp_check(sp.browse)
plot(sp.browse)

mcmc_plot(sp.browse, type= 'areas', prob= 0.95)+
  geom_vline(xintercept = 0, col= 'grey')

summary(sp.browse)
conditional_effects(sp.browse)

# tramp.browse----
# tramp.browse <- brm(Trampling ~ sci.name+ (1|site), data= com.species,
#                  family = bernoulli(link = "logit"),
#                  chains = 4,
#                  warmup = 1000,
#                  iter = 4000,
#                  thin = 1
# )
# save(tramp.browse, file= 'tramp.browse.Rdata')

load(file= 'tramp.browse.Rdata')
pp_check(tramp.browse)
plot(tramp.browse)

mcmc_plot(tramp.browse, type= 'areas', prob= 0.95)+
  geom_vline(xintercept = 0, col= 'grey')

summary(tramp.browse)
conditional_effects(tramp.browse)

# water.stress----
names(com.sp)
# water.stress <- brm(Wat.stress ~ sci.name+ (1|site), data= com.species,
#                  family = bernoulli(link = "logit"),
#                  chains = 4,
#                  warmup = 1000,
#                  iter = 4000,
#                  thin = 1
# )
# save(water.stress, file= 'water.stress.Rdata')

load(file= 'water.stress.Rdata')
pp_check(water.stress)
plot(water.stress)

mcmc_plot(water.stress, type= 'areas', prob= 0.95)+
  geom_vline(xintercept = 0, col= 'grey')

summary(water.stress)
conditional_effects(water.stress)

# no.stress----
names(com.sp)
# no.stress.sp <- brm(None ~ sci.name+ (1|site), data= com.species,
#                  family = bernoulli(link = "logit"),
#                  chains = 4,
#                  warmup = 1000,
#                  iter = 5000,
#                  thin = 1
# )
# save(no.stress.sp, file= 'no.stress.sp.Rdata')

load(file= 'no.stress.sp.Rdata')
pp_check(no.stress.sp)
plot(no.stress.sp)

mcmc_plot(no.stress.sp, type= 'areas', prob= 0.95)+
  geom_vline(xintercept = 0, col= 'grey')

summary(no.stress.sp)
conditional_effects(no.stress.sp)

# model without interaction with species----
sapstatus.no.dist <- brm(None ~ Treatment + (1|site),
                         data= com.species, # common species found all treatments with more than 2 indivisuals
                         # data= sap_status, # all species
                 family = bernoulli(link = "logit"),
                 chains = 4,
                 warmup = 1000,
                 iter = 4000,
                 thin = 1
                 )
save(sapstatus.no.dist, file= 'sapstatus.no.dist.Rdata')

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
conditional_effects(sapstatus.no.dist)


# plot----
# model, ce, df, 
load(file= 'sp.browse.Rdata')
load(file= 'tramp.browse.Rdata')
load(file= 'water.stress.Rdata')
load(file= 'no.stress.Rdata')
load(file= 'sapstatus.no.dist.Rdata')


sp.browse.ce <- conditional_effects(sp.browse)
sp.browse.df <- as.data.frame(sp.browse.ce$sci.name)

tramp.browse.ce <- conditional_effects(tramp.browse)
tramp.browse.df <- as.data.frame(tramp.browse.ce$sci.name)

water.stress.ce <- conditional_effects(water.stress)
water.stress.df <- as.data.frame(water.stress.ce$sci.name)

no.stress.ce <- conditional_effects(no.stress)
no.stress.df <- as.data.frame(no.stress.ce$sci.name)

sapstatus.no.dist.ce <- conditional_effects(sapstatus.no.dist)
sapstatus.no.dist.df <- as.data.frame(sapstatus.no.dist.ce$Treatment)



fig.a <- ggplot() +
  geom_point(
    data = com.species,
    # raw data
    aes(x = sci.name, # predicting variable
        y = None, # response variable
        col = sci.name),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_point(
    data = sp.browse.df,
    aes(x = sci.name, y = estimate__, colour = sci.name),
    size = 3
  ) +
  geom_errorbar(
    data = sp.browse.df,
    aes(
      x = sci.name,
      ymin = lower__,
      ymax = upper__,
      colour = sci.name
    ),
    linewidth = 1.3,
    width = 0.1
  ) +
  coord_flip()+
  ylim(0,1)+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(y= 'Browsed', x= ' '  , subtitle= 'a)')+
  guides(fill= 'none')

fig.a

fig.b <- ggplot() +
  geom_point(
    data = com.species,
    # raw data
    aes(x = sci.name, # predicting variable
        y = None, # response variable
        col = sci.name),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_point(
    data = tramp.browse.df,
    aes(x = sci.name, y = estimate__, colour = sci.name),
    size = 3
  ) +
  geom_errorbar(
    data = tramp.browse.df,
    aes(
      x = sci.name,
      ymin = lower__,
      ymax = upper__,
      colour = sci.name
    ),
    linewidth = 1.3,
    width = 0.1
  ) +
  coord_flip()+
  ylim(0,1)+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(y= 'Trampled', x= ' '  , subtitle= 'b)')+
  guides(fill= 'none')

fig.b

fig.c <- ggplot() +
  geom_point(
    data = com.species,
    # raw data
    aes(x = sci.name, # predicting variable
        y = None, # response variable
        col = sci.name),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_point(
    data = water.stress.df,
    aes(x = sci.name, y = estimate__, colour = sci.name),
    size = 3
  ) +
  geom_errorbar(
    data = water.stress.df,
    aes(
      x = sci.name,
      ymin = lower__,
      ymax = upper__,
      colour = sci.name
    ),
    linewidth = 1.3,
    width = 0.1
  ) +
  coord_flip()+
  ylim(0,1)+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(y= 'Water stress', x= ' '  , subtitle= 'c)')+
  guides(fill= 'none')

fig.c

fig.d <- ggplot() +
  geom_point(
    data = com.species,
    # raw data
    aes(x = sci.name, # predicting variable
        y = None, # response variable
        col = sci.name),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_point(
    data = no.stress.df,
    aes(x = sci.name, y = estimate__, colour = sci.name),
    size = 3
  ) +
  geom_errorbar(
    data = no.stress.df,
    aes(
      x = sci.name,
      ymin = lower__,
      ymax = upper__,
      colour = sci.name
    ),
    linewidth = 1.3,
    width = 0.1
  ) +
  # coord_cartesian(xlim = c(0, 1), ) + 
  coord_flip()+
  ylim(0, 1)+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(x= 'No stress', y= ' '  , subtitle= 'd)')+
  guides(fill= 'none')

fig.d


fig.a
fig.b
fig.c
fig.d

fig.abcd <- (fig.a|fig.b)/(fig.c|fig.d)
fig.abcd

no.stress.prop <- ggplot() +
  geom_point(
    data = sap_status,
    # raw data
    aes(x = Treatment, # predicting variable
        y = None, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.3,
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
  labs(y= 'Proportion'  , subtitle= 'a)')+
  guides(fill= 'none')


no.stress.prop

no.stress.sp <- ggplot() +
  geom_point(
    data = com.species,
    # raw data
    aes(x = sci.name, # predicting variable
        y = None, # response variable
        col = sci.name),
    size = 1.5,
    alpha = 0.3,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_point(
    data = no.stress.df,
    aes(x = sci.name, y = estimate__, colour = sci.name),
    size = 3
  ) +
  geom_errorbar(
    data = no.stress.df,
    aes(
      x = sci.name,
      ymin = lower__,
      ymax = upper__,
      colour = sci.name
    ),
    linewidth = 1.3,
    width = 0.1
  ) +
  # coord_cartesian(xlim = c(0, 1), ) + 
  coord_flip()+
  ylim(0, 1)+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(x= 'Species', y= ' '  , subtitle= 'b)')+
  guides(fill= 'none')

no.stress.sp

fig4 <- (no.stress.prop|no.stress.sp)
save(fig4, file= 'fig4.Rdata')

load(file= 'fig4.Rdata')
fig4
