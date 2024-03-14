source('00_sapling_data.R')

names(sap_status)
names(com.species)

com.species %>% group_by(Treatment, Species) %>% 
  summarise(n()) # %>% View()

com.species %>% group_by(Treatment) %>% 
  summarise(N_sites=n_distinct(site))

# figure 4----

# Modeling of disturbance probability in response to Treatment

# dist.prob_treat <- brm(Disturbance ~ Treatment + (1|Species), # dist.prob_treat=disturbance probability in response to Treatment
#                          data= com.species, # common species found all treatments with more than 5 individuals
#                          # data= sap_status, # all species
#                  family = bernoulli(link = "logit"),
#                  chains = 4,
#                  warmup = 1000,
#                  iter = 4000,
#                  thin = 1,
#                  control = list(adapt_delta = 0.99) # divergent 5 at 0.9!
#                  )
# save(dist.prob_treat, file= 'dist.prob_treat.Rdata')

load(file= 'dist.prob_treat.Rdata')

# pp_check
color_scheme_set("darkgray")
pp_check(dist.prob_treat, ndraws = 30)+ # predicted vs. observed values
  xlab( "Healthy saplings with no disturbances") + ylab("Density")+
  theme_classic()+ 
  theme(legend.position = 'none')
  
# Model convergence
mcmc_plot(dist.prob_treat,
          type = 'trace')

mcmc_plot(dist.prob_treat,
          type = "acf_bar")

mcmc_plot(dist.prob_treat,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')

summary(dist.prob_treat)
conditional_effects(dist.prob_treat)


fixef(dist.prob_treat)
ranef(dist.prob_treat) # for plot
coef(dist.prob_treat)

# we found there is no difference in proportion of saplings being disturbed in treatments

# Modeling of disturbance probability in response to Species ----
# dist.prob_sp <- brm(
#     Disturbance ~ Species + (1 |site),
#     # dist.prob_sp=disturbance probability in response to species
#     data = com.species,
#     family = bernoulli(link = "logit"),
#     chains = 4,
#     warmup = 1000,
#     iter = 5000,
#     thin = 1,
#     control = list(adapt_delta = 0.9)
#   )

# save(dist.prob_sp, file= 'dist.prob_sp.Rdata')

load(file= 'dist.prob_sp.Rdata')

# pp_check
color_scheme_set("darkgray")
pp_check(dist.prob_sp, ndraws = 30)+ # predicted vs. observed values
  xlab( "Healthy saplings species with no disturbances") + ylab("Density")+
  theme_classic()+ 
  theme(legend.position = 'none')

# Model convergence
mcmc_plot(dist.prob_sp,
          type = 'trace')

mcmc_plot(dist.prob_sp,
          type = "acf_bar")

mcmc_plot(dist.prob_sp,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')

summary(dist.prob_sp)
conditional_effects(dist.prob_sp)


load(file= 'dist.prob_treat.Rdata')
load(file= 'dist.prob_sp.Rdata')

# Given log odds for Treatments
# dist.prob_treat
library(broom.mixed)
df <- as.data.frame(tidy(dist.prob_treat))
df %>% 
  mutate(estimate = exp(estimate) / (1 + exp(estimate))*100) %>% 
  mutate(probability= round(estimate)) %>% 
  select(effect, probability) %>% 
  filter(effect== 'fixed')


# Given log odds for Species
# dist.prob_sp

df1 <- as.data.frame(tidy(dist.prob_sp))
df1 %>% 
  mutate(estimate = exp(estimate) / (1 + exp(estimate))*100) %>% 
  mutate(probability= round(estimate, 0)) %>% 
  select(effect, probability) %>% 
  filter(effect== 'fixed')

# make df for figures
# load(file= 'dist.prob_treat.Rdata')
# load(file= 'dist.prob_sp.Rdata')

dist.prob_treat.ce <- conditional_effects(dist.prob_treat)
dist.prob_treat.df <- as.data.frame(dist.prob_treat.ce$Treatment)


prop_treat <- ggplot() +
  geom_point(
    data = sap_status,
    # raw data
    aes(x = Treatment, # predicting variable
        y = Disturbance, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.3,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_point(
    data = dist.prob_treat.df,
    aes(x = Treatment, y = estimate__, colour = Treatment),
    size = 3
  ) +
  geom_errorbar(
    data = dist.prob_treat.df,
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
  labs(y= 'Proportion of undisturbed saplings per site', subtitle= '(a)')+
  guides(fill= 'none')


prop_treat

# make df for figures
dist.prob_sp.ce <- conditional_effects(dist.prob_sp)
dist.prob_sp.df <- as.data.frame(dist.prob_sp.ce$Species)
fitted_values1 <- fitted(dist.prob_sp)
average_effect1 <- mean(fitted_values1) # 0.21, for hline

prop_sp <- ggplot() +
  geom_point(
    data = com.species,
    # raw data
    aes(x = Species, # predicting variable
        y = Disturbance, # response variable
        col = Species),
    size = 1.5,
    alpha = 0.3,
    position = position_jitter(width = 0.05, height = 0.45)
  ) +
  geom_point(
    data = dist.prob_sp.df,
    aes(x = Species, y = estimate__, colour = Species),
    size = 3
  ) +
  geom_errorbar(
    data = dist.prob_sp.df,
    aes(
      x = Species,
      ymin = lower__,
      ymax = upper__,
      colour = Species
    ),
    linewidth = 1.3,
    width = 0.1
  ) +
  geom_hline(yintercept = average_effect1, linetype= 'dashed', col= 'black', alpha= 0.8, linewidth= 0.8)+ # mean(estimate__)= 0.19
  # coord_cartesian(xlim = c(0, 1), ) + 
  coord_flip()+
  ylim(0, 1)+ 
  # scale_color_viridis(discrete = T, option="A")  + 
  # scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(x= 'Species', y= 'Proportion of undisturbed \nsaplings for each species'  , subtitle= '(b)')+
  guides(fill= 'none')

prop_sp

figure4 <- (prop_treat | prop_sp)
figure4

save(figure4, file= 'figure4.Rdata')

load(file= 'figure4.Rdata')
figure4

ggsave('figure4.jpg', figure4,
       width = 10,
       height = 6,
       dpi = 300)





