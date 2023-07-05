source('00_sapling_data.R')

names(visit_01.lui)
names(visit_02.lui)

# DAG
sapling_dag <- dagitty('dag{
    Treatment-> RGR;
    Trench -> RGR;
    Treatment -> Livestock;
    Livestock-> Browsing;
    Livestock -> Trampling;
    Trampling -> RGR;
    Browsing -> RGR
    
}')

coordinates(sapling_dag) <-
  list( x=c(Treatment=1,  # column 1
            Livestock= 1, # column 2
            Trench= 1.5, # column 2
            RGR=2, # column 2
            Browsing= 2,
            Trampling= 3
  ),
  y=c(Treatment=0, # middle row/0 
      RGR=0, # middle row/0 
      Livestock= -1, # above middle row -1
      Trench= 1, # below the middle row/1
      Browsing= -1.5,
      Trampling= -1
  ))

plot(sapling_dag)

glimpse(rgr)

# Models----
# Relative growth Figure 5
# rcd ~ treatment * goat

# mod.rgr.treat.goat <-
#   brm(
#     rgr_rcd ~ Treatment * Goat + (1 | site),
#     data =  rgr,
#     family = gaussian(),
#     warmup = 1000,
#     iter = 4000,
#     chains = 4,
#     control = list(adapt_delta=0.95)
#   )
# save(mod.rgr.treat.goat, file='mod.rgr.treat.goat.Rdata')

load('mod.rgr.treat.goat.Rdata')
pp_check(mod.rgr.treat.goat)

# Model convergence
mcmc_plot(mod.rgr.treat.goat,
          type = 'trace')

mcmc_plot(mod.rgr.treat.goat,
          type = "acf_bar")

mcmc_plot(mod.rgr.treat.goat,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')

summary(mod.rgr.treat.goat)
conditional_effects(mod.rgr.treat.goat)
conditional_effects(mod.rgr.treat.goat, effects = 'Treatment:Goat')
sap.tret.goat <- conditional_effects(mod.rgr.treat.goat, effects = 'Goat:Treatment')

# To plot
# make df

rcd.Goat <- ggplot() +
  geom_point(
    data = rgr,
    # raw data
    aes(x = Goat, # predicting variable
        y = rgr_rcd, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  )+
  geom_line(
    data = sap.tret.goat$Goat,
    # conditional effect
    aes(
      x = Goat,
      # ce of the predicting variable
      y = estimate__,
      group = effect2__,
      color = effect2__
    ),
    linewidth = 1
  )+
  geom_ribbon(
    data = sap.tret.goat$Goat,
    # conditional effect
    aes(
      x = Goat,
      # ce of the predicting variable
      ymin= lower__,
      ymax= upper__,
      y = estimate__,
      group = effect2__,
      fill = effect2__
    ), alpha= 0.3
  )+
  labs(y= 'Relative growth (RCD)', x= 'Proportion of livestock',
       subtitle = ' ')+
  coord_cartesian(ylim = c(0,2), xlim = c(0.45, 2.01))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(subtitle= 'a)')+
  guides(fill= 'none')

rcd.Goat

# rcd ~ treatment * trench
# mod.rgr.treat.trench <-
#   brm(
#     rgr_rcd ~ Treatment * Trenches + (1 | site),
#     data =  rgr,
#     family = gaussian(),
#     warmup = 1000,
#     iter = 4000,
#     chains = 4,
#     control = list(adapt_delta=0.95)
#   )
# save(mod.rgr.treat.trench, file='mod.rgr.treat.trench.Rdata')

load('mod.rgr.treat.trench.Rdata')
pp_check(mod.rgr.treat.trench)

# Model convergence
mcmc_plot(mod.rgr.treat.trench,
          type = 'trace')

mcmc_plot(mod.rgr.treat.trench,
          type = "acf_bar")

mcmc_plot(mod.rgr.treat.trench,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')

summary(mod.rgr.treat.trench)
conditional_effects(mod.rgr.treat.trench)
conditional_effects(mod.rgr.treat.trench, effects = 'Treatment:Trenches')
sap.tret.trench <- conditional_effects(mod.rgr.treat.trench, effects = 'Trenches:Treatment')

rcd.Trench <- ggplot() +
  geom_point(
    data = rgr,
    # raw data
    aes(x = Trenches, # predicting variable
        y = rgr_rcd, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  )+
  geom_line(
    data = sap.tret.trench$Trench,
    # conditional effect
    aes(
      x = Trenches,
      # ce of the predicting variable
      y = estimate__,
      group = effect2__,
      color = effect2__
    ),
    linewidth = 1
  )+
  geom_ribbon(
    data = sap.tret.trench$Trench,
    # conditional effect
    aes(
      x = Trenches,
      # ce of the predicting variable
      ymin= lower__,
      ymax= upper__,
      y = estimate__,
      group = effect2__,
      fill = effect2__
    ), alpha= 0.3
  )+
  labs(y= 'Relative growth (RCD)', x= 'Relative area of trenches',
       subtitle = ' ')+
  coord_cartesian(ylim = c(0,2), xlim = c(0.3, 4.5))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(subtitle= 'b)')+
  guides(fill= 'none')

rcd.Trench


# rgrH----
# rgrh ~ treatment
# mod.rgrH.treat <-
#   brm(
#     rgrH ~ Treatment + (1 | site),
#     data =  rgr,
#     family = gaussian(),
#     warmup = 1000,
#     iter = 4000,
#     chains = 4,
#     control = list(adapt_delta=0.95)
#   )
# save(mod.rgrH.treat, file='mod.rgrH.treat.Rdata')

load('mod.rgrH.treat.Rdata')
pp_check(mod.rgrH.treat)

# Model convergence
mcmc_plot(mod.rgrH.treat,
          type = 'trace')

mcmc_plot(mod.rgrH.treat,
          type = "acf_bar")

mcmc_plot(mod.rgrH.treat,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')

summary(mod.rgrH.treat)
conditional_effects(mod.rgrH.treat)


# rgrh ~ treatment * goat
# mod.rgrH.treat.goat <-
#   brm(
#     rgrH ~ Treatment * Goat + (1 | village),
#     data =  rgr,
#     family = gaussian(),
#     warmup = 1000,
#     iter = 4000,
#     chains = 4,
#     control = list(adapt_delta=0.95)
#   )
# save(mod.rgrH.treat.goat, file='mod.rgrH.treat.goat.Rdata')

load('mod.rgrH.treat.goat.Rdata')
pp_check(mod.rgrH.treat.goat)

# Model convergence
mcmc_plot(mod.rgrH.treat.goat,
          type = 'trace')

mcmc_plot(mod.rgrH.treat.goat,
          type = "acf_bar")

mcmc_plot(mod.rgrH.treat.goat,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')

summary(mod.rgrH.treat.goat)
conditional_effects(mod.rgrH.treat.goat)
conditional_effects(mod.rgrH.treat.goat, effects = 'Treatment:Goat')
rgH.Goat.df <- conditional_effects(mod.rgrH.treat.goat, effects = 'Goat:Treatment')


rgrH.Goat <- ggplot() +
  geom_point(
    data = rgr,
    # raw data
    aes(x = Goat, # predicting variable
        y = rgrH, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  )+
  geom_line(
    data = rgH.Goat.df$Goat,
    # conditional effect
    aes(
      x = Goat,
      # ce of the predicting variable
      y = estimate__,
      group = effect2__,
      color = effect2__
    ),
    linewidth = 1
  ) +
  geom_ribbon(
    data = rgH.Goat.df$Goat,
    # conditional effect
    aes(
      x = Goat,
      # ce of the predicting variable
      ymin= lower__,
      ymax= upper__,
      y = estimate__,
      group = effect2__,
      fill = effect2__
    ), alpha= 0.3
  ) +
  labs(y= 'Relative growth (height)', x= 'Proportion of livestock',
       subtitle = ' ')+
  coord_cartesian(ylim = c(-3,2), xlim = c(0.45, 2))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(subtitle= 'c)')+
  guides(fill= 'none')

rgrH.Goat

# rgrh ~ treatment * trench
# mod.rgrH.treat.trench <-
#   brm(
#     rgrH ~ Treatment * Trenches + (1 | site),
#     data =  rgr,
#     family = gaussian(),
#     warmup = 1000,
#     iter = 4000,
#     chains = 4,
#     control = list(adapt_delta=0.95)
#   )
# save(mod.rgrH.treat.trench, file='mod.rgrH.treat.trench.Rdata')

load('mod.rgrH.treat.trench.Rdata')
pp_check(mod.rgrH.treat.trench)

# Model convergence
mcmc_plot(mod.rgrH.treat.trench,
          type = 'trace')

mcmc_plot(mod.rgrH.treat.trench,
          type = "acf_bar")

mcmc_plot(mod.rgrH.treat.trench,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'grey')

summary(mod.rgrH.treat.trench)
conditional_effects(mod.rgrH.treat.trench)
conditional_effects(mod.rgrH.treat.trench, effects = 'Treatment:Trenches')
rgrH.Trench.df <- conditional_effects(mod.rgrH.treat.trench, effects = 'Trenches:Treatment')

rgrH.Trench <- ggplot() +
  geom_point(
    data = rgr,
    # raw data
    aes(x = Trenches, # predicting variable
        y = rgrH, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  )+
  geom_line(
    data = rgrH.Trench.df$Trench,
    # conditional effect
    aes(
      x = Trenches,
      # ce of the predicting variable
      y = estimate__,
      group = effect2__,
      color = effect2__
    ),
    linewidth = 1
  )+
  geom_ribbon(
    data = rgrH.Trench.df$Trench,
    # conditional effect
    aes(
      x = Trenches,
      # ce of the predicting variable
      ymin= lower__,
      ymax= upper__,
      y = estimate__,
      group = effect2__,
      fill = effect2__
    ), alpha= 0.3
  )+
  labs(y= 'Relative growth (height)', x= 'Relative area of trenches',
       subtitle = ' ')+
  coord_cartesian(ylim = c(-3,2), xlim = c(0.2, 4.2))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(subtitle= 'd)')+
  guides(fill= 'none')

rgrH.Trench


# library("gridExtra")
# Create user-defined function, which extracts legends from ggplots
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

rgr.with_legend <- rgrH.Trench + theme(legend.position="bottom")

legendfig5 <- extract_legend(rgr.with_legend)

figure5 <- (rcd.Goat|rcd.Trench)/(rgrH.Goat|rgrH.Trench)/(legendfig5)+plot_layout(heights = c(10,10,2)) 

save(figure5, file='figure5.Rdata')

load('figure5.Rdata')
figure5

# saplings with normal growth----
sapstu.none.df <- rgr %>% 
  filter(sap.status=='none') 

# mod.rgrrcd.treat.none <-
#   brm(
#     rgr_rcd ~ Treatment + (1 | site),
#     data =  sapstu.none.df,
#     family = gaussian(),
#     warmup = 1000,
#     iter = 4000,
#     chains = 4,
#     control = list(adapt_delta=0.95)
#   )
# 
# save(mod.rgrrcd.treat.none, file= 'mod.rgrrcd.treat.none.Rdata')

load('mod.rgrrcd.treat.none.Rdata')

mcmc_plot(mod.rgrrcd.treat.none, type = 'areas', prob= 0.95)+
  geom_vline(xintercept = 0, col= 'red')

conditional_effects(mod.rgrrcd.treat.none)

no.stress_rcd.df <- conditional_effects(mod.rgrrcd.treat.none, effects = 'Treatment' )

no.stress.rcd <- ggplot() +
  geom_point(
    data = sapstu.none.df,
    # raw data
    aes(x = Treatment, # predicting variable
        y = rgr_rcd, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  )+
  geom_point(
    data = no.stress_rcd.df$Treatment,
    aes(x = Treatment, y = estimate__, colour = Treatment),
    size = 3
  ) +
  geom_errorbar(
    data = no.stress_rcd.df$Treatment,
    aes(
      x = Treatment,
      ymin = lower__,
      ymax = upper__,
      colour = Treatment
    ),
    linewidth = 1.3,
    width = 0.1
  )+
  coord_cartesian(ylim = c(0, 2))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(y= 'Relative growth (RCD)'  , subtitle= 'a)')+
  guides(fill= 'none')

no.stress.rcd


# rgrH
# mod.rgrH.treat.none <-
#   brm(
#     rgrH ~ Treatment + (1 | site),
#     data =  sapstu.none.df,
#     family = gaussian(),
#     warmup = 1000,
#     iter = 4000,
#     chains = 4,
#     control = list(adapt_delta=0.95)
#   )
# 
# save(mod.rgrH.treat.none, file= 'mod.rgrH.treat.none.Rdata')

load('mod.rgrH.treat.none.Rdata')

mcmc_plot(mod.rgrH.treat.none, type = 'areas', prob= 0.95)+
  geom_vline(xintercept = 0, col= 'red')

conditional_effects(mod.rgrH.treat.none)

no.stress_rgrH.df <- conditional_effects(mod.rgrH.treat.none, effects = 'Treatment' )

no.stress.rhrH <- ggplot() +
  geom_point(
    data = sapstu.none.df,
    # raw data
    aes(x = Treatment, # predicting variable
        y = rgr_rcd, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  )+
  geom_point(
    data = no.stress_rgrH.df$Treatment,
    aes(x = Treatment, y = estimate__, colour = Treatment),
    size = 3
  ) +
  geom_errorbar(
    data = no.stress_rgrH.df$Treatment,
    aes(
      x = Treatment,
      ymin = lower__,
      ymax = upper__,
      colour = Treatment
    ),
    linewidth = 1.3,
    width = 0.1
  )+
  coord_cartesian(ylim = c(0, 2))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(y= 'Relative growth (Height)'  , subtitle= 'b)')+
  guides(fill= 'none')

no.stress.rhrH

no.stress_legend <- no.stress.rhrH + theme(legend.position="bottom")

legendfig5a <- extract_legend(no.stress_legend)

figure5a <- (no.stress.rcd|no.stress.rhrH)/(legendfig5) + plot_layout(heights = c(10,2)) 

save(figure5a, file='figure5a.Rdata')

load('figure5a.Rdata')
figure5a


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

browsing.goat <- ggplot() +
  geom_point(
    data = sap_status,
    # raw data
    aes(x = Goat, # predicting variable
        y = Browsing, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  )+
  geom_line(
    data = browsing.goat.df$Goat,
    # conditional effect
    aes(
      x = Goat,
      # ce of the predicting variable
      y = estimate__,
      group = effect2__,
      color = effect2__
    ),
    linewidth = 1
  )+
geom_ribbon(
  data = browsing.goat.df$Goat,
  # conditional effect
  aes(
    x = Goat,
    # ce of the predicting variable
    ymin= lower__,
    ymax= upper__,
    y = estimate__,
    group = effect2__,
    fill = Treatment
  ), alpha= 0.3
)+
labs(
  # y= ' ', x= ' ',
     subtitle = ' ')+
  coord_cartesian(ylim = c(-0.2, 0.9), xlim = c(0.5, 2))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(subtitle= 'a)')+
  guides(fill= 'none')

browsing.goat

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

trampling.goat <- ggplot() +
  geom_point(
    data = sap_status,
    # raw data
    aes(x = Goat, # predicting variable
        y = Trampling, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  )+
  geom_line(
    data = trampling.goat.df$Goat,
    # conditional effect
    aes(
      x = Goat,
      # ce of the predicting variable
      y = estimate__,
      group = effect2__,
      color = effect2__
    ),
    linewidth = 1
  )+
  geom_ribbon(
    data = trampling.goat.df$Goat,
    # conditional effect
    aes(
      x = Goat,
      # ce of the predicting variable
      ymin= lower__,
      ymax= upper__,
      y = estimate__,
      group = effect2__,
      fill = Treatment
    ), alpha= 0.3
  )+
  labs(
    # y= ' ', x= ' ',
    subtitle = ' ')+
  coord_cartesian(ylim = c(-0.2, 0.9), xlim = c(0.5, 2))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(subtitle= 'b)')+
  guides(fill= 'none')

trampling.goat


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

waterstress.trench <- ggplot() +
  geom_point(
    data = sap_status,
    # raw data
    aes(x = Trenches, # predicting variable
        y = Wat.stress, # response variable
        col = Treatment),
    size = 1.5,
    alpha = 0.5,
    position = position_jitter(width = 0.05, height = 0.45)
  )+
  geom_line(
    data = waterstress.trench.df$Trenches,
    # conditional effect
    aes(
      x = Trenches,
      # ce of the predicting variable
      y = estimate__,
      group = effect2__,
      color = effect2__
    ),
    linewidth = 1
  )+
  geom_ribbon(
    data = waterstress.trench.df$Trenches,
    # conditional effect
    aes(
      x = Trenches,
      # ce of the predicting variable
      ymin= lower__,
      ymax= upper__,
      y = estimate__,
      group = effect2__,
      fill = Treatment
    ), alpha= 0.3
  )+
  labs(
    # y= ' ', x= ' ',
    subtitle = ' ')+
  coord_cartesian(ylim = c(-0.2, 0.9), xlim = c(0.5, 2))+
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(subtitle= 'c)')+
  guides(fill= 'none')

waterstress.trench


sapstatus.no.dist <- brm(None ~ Treatment + (1|site), 
                         # data= sap_status,
                         data= freq.sp, # from fig_4.R
                 family = bernoulli(link = "logit"),
                 chains = 4,
                 warmup = 1000,
                 iter = 5000,
                 thin = 1
                 )
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
    data = sapstatus.no.dist.df$Treatment,
    aes(x = Treatment, y = estimate__, colour = Treatment),
    size = 3
  ) +
  geom_errorbar(
    data = sapstatus.no.dist.df$Treatment,
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


stress_legend <- waterstress.trench + theme(legend.position="bottom")

legendfig5b <- extract_legend(stress_legend)

figure5b <- (browsing.goat|trampling.goat)/(waterstress.trench|no.stress.plot)/(legendfig5b) + plot_layout(heights = c(10,10,2)) 

save(figure5b, file='figure5b.Rdata')

load('figure5b.Rdata')
figure5b






