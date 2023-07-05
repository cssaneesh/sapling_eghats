source('00_sapling_data.R')

# all species 992= observation
# all species no distrubance= 211 observation

rgr %>% group_by(Treatment,sci.name) %>% 
  count(sci.name) #%>% View()

# rgr model with all species for no stress----
allsp.nodistr <- rgr %>% filter(sap.status== 'none')

names(allsp.nodistr)

# rgr rcd ~ treatment
# allsp.nodistr.rcd <- brm(
#   rgr_rcd ~ Treatment + (1 | site),
#   data = allsp.nodistr,
#   family = student(),
#   chains = 4,
#   warmup = 1000,
#   iter = 4000,
#   thin = 1
# )
# save(allsp.nodistr.rcd, file= 'allsp.nodistr.rcd.Rdata')

load(file= 'allsp.nodistr.rcd.Rdata')
pp_check(allsp.nodistr.rcd)

mcmc_plot(allsp.nodistr.rcd, type = 'areas', 
          prob= 0.95,
          )+ # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')

mcmc_plot(allsp.nodistr.rcd, type = 'trace')

summary(allsp.nodistr.rcd)
conditional_effects(allsp.nodistr.rcd)

# rcd ~ species
# allsp.nodistr.rcd.sp <- brm(
#   rgr_rcd ~ sci.name + (1 | site),
#   data = allsp.nodistr,
#   family = student(),
#   chains = 4,
#   warmup = 1000,
#   iter = 4000,
#   thin = 1
# )
# save(allsp.nodistr.rcd.sp, file= 'allsp.nodistr.rcd.sp.Rdata')

load(file= 'allsp.nodistr.rcd.sp.Rdata')

pp_check(allsp.nodistr.rcd.sp)

mcmc_plot(allsp.nodistr.rcd.sp, type= 'areas', prob= 0.95)+
  geom_vline(xintercept = 0, col= 'grey')

mcmc_plot(allsp.nodistr.rcd.sp, type= 'trace')
  
summary(allsp.nodistr.rcd.sp)
conditional_effects(allsp.nodistr.rcd.sp)

# rgr height ~ Treatment
# allsp.nodistr.h <- brm(
#   rgrH ~ Treatment + (1 | site),
#   data = allsp.nodistr,
#   family = student(),
#   chains = 4,
#   warmup = 1000,
#   iter = 4000,
#   thin = 1
# )
# save(allsp.nodistr.h, file= 'allsp.nodistr.h.Rdata')

load(file= 'allsp.nodistr.h.Rdata')
pp_check(allsp.nodistr.h)

mcmc_plot(allsp.nodistr.h, type= 'areas', prob= 0.95)+
  geom_vline(xintercept = 0, col= 'grey')

mcmc_plot(allsp.nodistr.h, type= 'trace')

summary(allsp.nodistr.h)
conditional_effects(allsp.nodistr.h)

# rgr height ~ species
# allsp.nodistr.h.sp <- brm(
#   rgrH ~ sci.name + (1 | site),
#   data = allsp.nodistr,
#   family = student(),
#   chains = 4,
#   warmup = 1000,
#   iter = 4000,
#   thin = 1
# )
# save(allsp.nodistr.h.sp, file= 'allsp.nodistr.h.sp.Rdata')

load(file= 'allsp.nodistr.h.sp.Rdata')
pp_check(allsp.nodistr.h.sp)

mcmc_plot(allsp.nodistr.h.sp, type= 'areas', prob= 0.95)+
  geom_vline(xintercept = 0, col= 'grey')

mcmc_plot(allsp.nodistr.h.sp, type= 'trace')

summary(allsp.nodistr.h.sp)
conditional_effects(allsp.nodistr.h.sp)

# plot----
load(file= 'allsp.nodistr.rcd.Rdata')
load(file= 'allsp.nodistr.rcd.sp.Rdata')
load(file= 'allsp.nodistr.h.Rdata')
load(file= 'allsp.nodistr.h.sp.Rdata')

allsp.nodistr.rcd.ce <- conditional_effects(allsp.nodistr.rcd)
allsp.nodistr.rcd.df <- as.data.frame(allsp.nodistr.rcd.ce$Treatment)

allsp.nodistr.rcd.sp.ce <- conditional_effects(allsp.nodistr.rcd.sp)
allsp.nodistr.rcd.sp.df <- as.data.frame(allsp.nodistr.rcd.sp.ce$sci.name)

allsp.nodistr.h.ce <- conditional_effects(allsp.nodistr.h)
allsp.nodistr.h.df <- as.data.frame(allsp.nodistr.h.ce$Treatment)

allsp.nodistr.h.sp.ce <- conditional_effects(allsp.nodistr.h.sp)
allsp.nodistr.h.sp.df <- as.data.frame(allsp.nodistr.h.sp.ce$sci.name)

# plot

rcdrgr.allTreat <- ggplot(data = allsp.nodistr.rcd.df,
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
  labs(y= 'RGR (Root collar diameter)')+scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")

rcdrgr.allSp <- ggplot(data = allsp.nodistr.rcd.sp.df,
       aes(y = sci.name,
           x = estimate__,
           colour = sci.name)) +
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
  labs(x= 'RGR (Root collar diameter)', y= " ")+
  theme(legend.position = 'none')+scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")

rcdH.allTreat <- ggplot(data = allsp.nodistr.h.df,
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
  labs(y= 'RGR (Height)')+scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")

rcdH.allSp <- ggplot(data = allsp.nodistr.h.sp.df,
       aes(y = sci.name,
           x = estimate__,
           colour = sci.name)) +
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
  labs(x= 'RGR (Height)', y= " ")+
  theme(legend.position = 'none')+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")

rcdrgr.allTreat
rcdrgr.allSp
rcdH.allTreat
rcdH.allSp

fig5allsp <- (rcdrgr.allTreat|rcdrgr.allSp)/(rcdH.allTreat|rcdH.allSp)+ plot_layout(heights = c(10,10,2))
save(fig5allsp, file= 'fig5allsp.Rdata')

load(file= 'fig5allsp.Rdata')
fig5allsp


# rgr model with common species with no stress----
# all species 992
# Common species without stress & disturbance = 176 (992-816= 176)
Common.species <- rgr %>% filter(sci.name %in% c('Acacia chundra',
                                                 'Cassia fistula',
                                                 'Chloroxylon swietenia',
                                                 'Dalbergia paniculata',
                                                 'Dolichandrone atrovirens',
                                                 'Wrightia tinctoria')) %>% 
  filter(sap.status=='none')

# rgr ~ Treatment
# com.no.dist <- brm(rgr_rcd ~ Treatment + (1|site), data= Common.species,
#                  family = student(),
#                  chains = 4,
#                  warmup = 1000,
#                  iter = 4000,
#                  thin = 1
#                  )
# save(com.no.dist, file= 'com.no.dist.Rdata')

load(file = 'com.no.dist.Rdata')
pp_check(com.no.dist)

mcmc_plot(com.no.dist, type= 'areas', prob= 0.95)+
  geom_vline(xintercept = 0, col= 'red')+
  xlim(-0.5, 5)

mcmc_plot(com.no.dist, type= 'trace')
mcmc_plot(com.no.dist, type= 'acf')

summary(com.no.dist)
conditional_effects(com.no.dist)

# rgr_rcd ~ sci.name
# com.no.dist.sp <- brm(rgr_rcd ~ sci.name + (1|site), data= Common.species,
#                     family = student(),
#                     chains = 4,
#                     warmup = 1000,
#                     iter = 4000,
#                     thin = 1
# )
# save(com.no.dist.sp, file= 'com.no.dist.sp.Rdata')

load(file= 'com.no.dist.sp.Rdata')
pp_check(com.no.dist.sp)

mcmc_plot(com.no.dist.sp, type= 'areas', prob= 0.95)+
  geom_vline(xintercept = 0, col= 'red')+
  xlim(-0.5, 10)

mcmc_plot(com.no.dist.sp, type= 'trace')
mcmc_plot(com.no.dist.sp, type= 'acf')

summary(com.no.dist.sp)
conditional_effects(com.no.dist.sp)

# rgrH ~ Treatment
# com.no.dist.rgrH <- brm(rgrH ~ Treatment + (1|site), data= Common.species,
#                     family = student(),
#                     chains = 4,
#                     warmup = 1000,
#                     iter = 4000,
#                     thin = 1
# )
# save(com.no.dist.rgrH, file= 'com.no.dist.rgrH.Rdata')

load(file= 'com.no.dist.rgrH.Rdata')
pp_check(com.no.dist.rgrH)

mcmc_plot(com.no.dist.rgrH, type= 'areas', prob= 0.95)+
  geom_vline(xintercept = 0, col= 'red')

mcmc_plot(com.no.dist.rgrH, type= 'trace')
mcmc_plot(com.no.dist.rgrH, type= 'acf')

summary(com.no.dist.rgrH)
conditional_effects(com.no.dist.rgrH)

# rgrH ~ sci.name
# com.no.dist.rgrH.sp <- brm(rgrH ~ sci.name + (1|site), data= Common.species,
#                      family = student(),
#                      chains = 4,
#                      warmup = 1000,
#                      iter = 4000,
#                      thin = 1
# )
# save(com.no.dist.rgrH.sp, file= 'com.no.dist.rgrH.sp.Rdata')

load(file= 'com.no.dist.rgrH.sp.Rdata')
pp_check(com.no.dist.rgrH.sp)

mcmc_plot(com.no.dist.rgrH.sp, type = 'areas', prob= 0.95)+
  geom_vline(xintercept = 0, col= 'red')+
  xlim(-0.5, 5)

mcmc_plot(com.no.dist.rgrH.sp, type = 'trace')
mcmc_plot(com.no.dist.rgrH.sp, type= 'acf')

summary(com.no.dist.rgrH.sp)
conditional_effects(com.no.dist.rgrH.sp)


# plot----
load(file = 'com.no.dist.Rdata')
load(file= 'com.no.dist.sp.Rdata')
load(file= 'com.no.dist.rgrH.Rdata')
load(file= 'com.no.dist.rgrH.sp.Rdata')

com.no.dist.ce <- conditional_effects(com.no.dist)
com.no.dist.df <- as.data.frame(com.no.dist.ce$Treatment)

com.no.dist.sp.ce <- conditional_effects(com.no.dist.sp)
com.no.dist.sp.df <- as.data.frame(com.no.dist.sp.ce$sci.name)

com.no.dist.rgrH.ce <- conditional_effects(com.no.dist.rgrH)
com.no.dist.rgrH.df <- as.data.frame(com.no.dist.rgrH.ce$Treatment)

com.no.dist.rgrH.sp.ce <- conditional_effects(com.no.dist.rgrH.sp)
com.no.dist.rgrH.sp.df <- as.data.frame(com.no.dist.rgrH.sp.ce$sci.name)

# fig 5 com sp

rgrrcd.a <- ggplot(data = com.no.dist.df,
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
  labs(y= 'RGR (Root collar diameter)')+scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")

rgrrcd.a

rgrrcd.b.sp <- ggplot(data = com.no.dist.sp.df,
                       aes(y = sci.name,
                           x = estimate__,
                           colour = sci.name)) +
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
  labs(x= 'RGR (Root collar diameter)', y= " ")+
  theme(legend.position = 'none')+scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")

rgrrcd.b.sp

rgrh.c <- ggplot(data = com.no.dist.rgrH.df,
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
  labs(y= 'RGR (Height)')+scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")

rgrh.c

rgrh.d.sp <- ggplot(data = com.no.dist.rgrH.sp.df,
                     aes(y = sci.name,
                         x = estimate__,
                         colour = sci.name)) +
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
  labs(x= 'RGR (Height)', y= " ")+
  theme(legend.position = 'none')+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")

rgrh.d.sp


fig5withcomsp <- (rgrrcd.a|rgrrcd.b.sp)/(rgrh.c|rgrh.d.sp)+ plot_layout(heights = c(10,10,2))
save(fig5withcomsp, file= 'fig5withcomsp.Rdata')

load(file= 'fig5withcomsp.Rdata')
fig5withcomsp


