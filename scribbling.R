# To test codes!!
N.alpha_sd_ce_lui <- conditional_effects(N.alpha_sd, effects = 'Treatment:LUI')
N.alpha_sd_ce_lui_df <- as.data.frame(N.alpha_sd_ce_lui$`LUI:Treatment`)

p <- ggplot() +
  geom_point(
    data = alpha_sum_sd,
    aes(
      x = LUI,
      y = N,
      group = Treatment,
      col = Treatment
    ),
    size = 1.5,
    alpha = 0.5,
    position = position_jitterdodge(jitter.width = 0.05,
                                    jitter.height = 0.45)
  ) 

p

p1 <- p +
  geom_point(
    data = N.alpha_sd_ce_lui$`Treatment:LUI`,
    aes(
      x = LUI,
      y = estimate__,
      group = effect2__,
      color = effect2__
    ),
    size = 3,
    alpha = 0.8,
    position = position_jitterdodge(jitter.width = 0.75)
  )
p1   


p1 + geom_errorbar(
  data = N.alpha_sd_ce_lui$`Treatment:LUI`,
  aes(
    x = LUI,
    ymin = lower__,
    ymax = upper__,
    # group = effect2__,
    # color = effect2__
  ),
  position = position_dodge(width = 0.5),
  linewidth = 0.8,
  width = 0.1,
  # alpha = 0.8
)+scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position=" ") + ylim (0,150)


  
  
  
  
  
