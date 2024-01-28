source('00_sapling_data.R')

# models for figure 5-----
# 1. Modeling relative growth in RCD (Root Collar Diameter) for treatment effects across sites 
# for common species with no disturbances

# Number of sites with common species and without disturbance
rgr_com.species %>% group_by(Treatment) %>% distinct(site) %>% summarise(sites=n())

boxplot(rgr_rcd ~ site, data = rgr_com.species) # using site as a random effect


# rgr.RCD_treat <- brm(rgr_rcd ~ Treatment + (1|site), data= rgr_com.species,
#                  family = student(),
#                  chains = 4,
#                  warmup = 1000,
#                  iter = 5000,
#                  thin = 1
#                  )
# save(rgr.RCD_treat, file= 'rgr.RCD_treat.Rdata')

load(file = 'rgr.RCD_treat.Rdata')

# pp_check
color_scheme_set("darkgray")
pp_check(rgr.RCD_treat, ndraws = 30)+ # predicted vs. observed values
  xlab( "Relative growth (RCD)") + ylab("Density")+
  theme_classic()+ 
  theme(legend.position = 'none') 

mcmc_plot(rgr.RCD_treat, type= 'areas', prob= 0.95)+
  geom_vline(xintercept = 0, col= 'red')

mcmc_plot(rgr.RCD_treat, type= 'trace')
mcmc_plot(rgr.RCD_treat, type= 'acf')

summary(rgr.RCD_treat)
conditional_effects(rgr.RCD_treat)

# 2. Modeling relative growth in RCD for species effects across sites 
# rgr.RCD_sp <- brm(rgr_rcd ~ Species + (1|site), data= rgr_com.species,
#                     family = student(),
#                     chains = 4,
#                     warmup = 1000,
#                     iter = 4000,
#                     thin = 1
# )
# save(rgr.RCD_sp, file= 'rgr.RCD_sp.Rdata')

load(file= 'rgr.RCD_sp.Rdata')

# ppcheck
color_scheme_set('darkgray')
pp_check(rgr.RCD_sp, ndraws = 30)+ # predicted vs. observed values
  xlab('Relative growth rate (RCD) across all healthy saplings')+ ylab('Density')+
  theme_classic()+ 
  theme(legend.position = 'none')

mcmc_plot(rgr.RCD_sp, type= 'areas', prob= 0.95)+
  geom_vline(xintercept = 0, col= 'red')

mcmc_plot(rgr.RCD_sp, type= 'trace')
mcmc_plot(rgr.RCD_sp, type= 'acf')

summary(rgr.RCD_sp)
conditional_effects(rgr.RCD_sp)

# 3. Modeling relative growth in height for treatment effects across sites
# rgr.H_treat <- brm(rgrH ~ Treatment + (1|site), data= rgr_com.species,
#                     family = student(),
#                     chains = 4,
#                     warmup = 1000,
#                     iter = 4000,
#                     thin = 1
# )
# save(rgr.H_treat, file= 'rgr.H_treat.Rdata')

load(file= 'rgr.H_treat.Rdata')

# pp_check
color_scheme_set("darkgray")
pp_check(rgr.H_treat, ndraws = 30)+ # predicted vs. observed values
  xlab( "Relative growth (height)") + ylab("Density")+
  theme_classic()+ 
  theme(legend.position = 'none') 

mcmc_plot(rgr.H_treat, type= 'areas', prob= 0.95)+
  geom_vline(xintercept = 0, col= 'red')

mcmc_plot(rgr.H_treat, type= 'trace')
mcmc_plot(rgr.H_treat, type= 'acf')

summary(rgr.H_treat)
conditional_effects(rgr.H_treat)

# 4. Modeling relative growth in height for species effects across sites 
# rgr.H_sp <- brm(rgrH ~ Species + (1|site), data= rgr_com.species,
#                      family = student(),
#                      chains = 4,
#                      warmup = 1000,
#                      iter = 4000,
#                      thin = 1
# )
# save(rgr.H_sp, file= 'rgr.H_sp.Rdata')

load(file= 'rgr.H_sp.Rdata')

# ppcheck
color_scheme_set('darkgray')
pp_check(rgr.H_sp, ndraws = 30)+ # predicted vs. observed values
  xlab('Relative growth rate (height) across all healthy saplings')+ ylab('Density')+
  theme_classic()+ 
  theme(legend.position = 'none')

mcmc_plot(rgr.H_sp, type = 'areas', prob= 0.95)+
  geom_vline(xintercept = 0, col= 'red')

mcmc_plot(rgr.H_sp, type = 'trace')
mcmc_plot(rgr.H_sp, type= 'acf')

summary(rgr.H_sp)
conditional_effects(rgr.H_sp)


# df for plots----
load(file = 'rgr.RCD_treat.Rdata')
load(file= 'rgr.RCD_sp.Rdata')
load(file= 'rgr.H_treat.Rdata')
load(file= 'rgr.H_sp.Rdata')


rgr.RCD_treat.ce <- conditional_effects(rgr.RCD_treat)
rgr.RCD_treat.df <- as.data.frame(rgr.RCD_treat.ce$Treatment)

rgr.RCD_sp.ce <- conditional_effects(rgr.RCD_sp)
rgr.RCD_sp.df <- as.data.frame(rgr.RCD_sp.ce$Species)

rgr.H_treat.ce <- conditional_effects(rgr.H_treat)
rgr.H_treat.df <- as.data.frame(rgr.H_treat.ce$Treatment)

rgr.H_sp.ce <- conditional_effects(rgr.H_sp)
rgr.H_sp.df <- as.data.frame(rgr.H_sp.ce$Species)


# Final fig 5----
# fig 5a 
rgrrcd.a <- ggplot(data = rgr.RCD_treat.df,
                          aes(x = Treatment,
                              y = estimate__,
                              colour = Treatment)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_errorbar(
    aes(
      # x = Treatment,
      ymin = lower__,
      ymax = upper__,
      # group = Treatment,
      # colour = Species
    ),
    # position = position_dodge(width = 0.5),
    linewidth = 0.9,
    width = 0.1
  ) +
  labs(y= 'Relative Growth: \n Root Collar Diameter', subtitle= '(a)') + 
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")


rgrrcd.a

# fig 5b

rgr.RCD_sp.df %>% summarise(mean(estimate__)) # for vertical line

rgrrcd.b.sp <- ggplot(data = rgr.RCD_sp.df,
                       aes(y = Species,
                           x = estimate__,
                           colour = Species)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_errorbar(
    aes(
      # x = Treatment,
      xmin = lower__,
      xmax = upper__,
      # group = Treatment,
      # colour = Species
    ),
    # position = position_dodge(width = 0.5),
    linewidth = 0.9,
    width = 0.1
  ) + 
  geom_vline(xintercept = 0.70, col= 'black', linetype= 'dashed', alpha= 0.8, linewidth= 0.8)+ # 0.70 is overall mean growth
  labs(x= 'Relative Growth: Root Collar Diameter', y= "Species", subtitle= '(b)')+
  theme(legend.position = 'none')+
  # scale_color_viridis(discrete = T, option="D")  + 
  # scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")

rgrrcd.b.sp

# fig 5c

rgrh.c <- ggplot(data = rgr.H_treat.df,
                        aes(x = Treatment,
                            y = estimate__,
                            colour = Treatment)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_errorbar(
    aes(
      # x = Treatment,
      ymin = lower__,
      ymax = upper__,
      # group = Treatment,
      # colour = Species
    ),
    # position = position_dodge(width = 0.5),
    linewidth = 0.9,
    width = 0.1
  ) +
  labs(y= 'Relative Growth: \n Height', subtitle= '(c)')+scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")

rgrh.c


# fig 5d
rgr.H_sp.df %>% summarise(mean(estimate__)) # for vertical line

rgrh.d.sp <- ggplot(data = rgr.H_sp.df,
                     aes(y = Species,
                         x = estimate__,
                         colour = Species)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_errorbar(
    aes(
      # x = Treatment,
      xmin = lower__,
      xmax = upper__,
      # group = Treatment,
      # colour = Species
    ),
    # position = position_dodge(width = 0.5),
    linewidth = 0.9,
    width = 0.1
  ) +
  geom_vline(xintercept = 0.70, linetype= 'dashed', col= 'black', alpha= 0.8, linewidth= 0.8)+ # 0.7056 is overall mean growth
  labs(x= 'Relative Growth: Height', y= "Species", subtitle= '(d)')+
  theme(legend.position = 'none')+
  # scale_color_viridis(discrete = T, option="D")  + 
  # scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")

rgrh.d.sp


figure5 <- (rgrrcd.a|rgrrcd.b.sp)/(rgrh.c|rgrh.d.sp)+ plot_layout(heights = c(10,10,2))
figure5

save(figure5, file= 'figure5.Rdata')

load(file= 'figure5.Rdata')
figure5

ggsave('figure5.jpg', figure5,
       width = 10,
       height = 6,
       dpi = 300)




