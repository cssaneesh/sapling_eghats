source('seedling_01_data.R')

# categorize LUI into three groups
seedling.dat <- seedling.dat %>%
  mutate(LUI = case_when(
    LUI > 0 & LUI <= 0.60 ~ 'low',
    LUI > 0.60 & LUI <= 1.20 ~ 'medium',
    LUI > 1.20 ~ 'high'
  ))

# count sites for alpha
seedling.dat %>%
  select(site, Treatment, LUI) %>% 
  distinct(site, Treatment, LUI) %>% 
  group_by(Treatment) %>% 
  count (LUI, name = 'no.sites')


seedling.dat %>%
  select(site, Treatment, LUI) %>% 
  distinct(site, Treatment, LUI) %>% 
  group_by(Treatment) %>% 
  count (LUI, name = 'no.sites') %>% ggplot(aes(x= LUI, y= no.sites, fill= Treatment))+
  geom_bar(stat = 'identity', position = 'dodge')

# Bootstrap sampling
gamma_data <- seedling.dat %>% # alpha_summary_sd, sd= seedling
  # filter(site!= 'APA19_CPFA') %>% # to avoid zero N and inf ENSPIE
  # filter(seedling>0) %>% 
  # filter(sci.name!= 'Senna siamea') %>%  # introduced ornamental tree
  filter(LUI!= 'high') %>% 
  group_by(site) %>% 
  summarise(tot.adult= sum(adult)) %>% # add the total number of adults in each site
  left_join(seedling.dat %>% select(!adult), multiple = "all") %>% 
  group_by(site, Treatment, sci.name, LUI, tot.adult, village) %>%
  summarise(abundance= sum(seedling)) %>% # abundance of seedling
  ungroup() %>%
  filter(abundance>0) %>% 
  # calculate metrics for each site
  group_by(site, Treatment, LUI, sci.name, tot.adult) %>% # Emma removed abundance here
  summarise(N = sum(abundance)) %>%  
  # total number of saplings
  ungroup() %>% 
  group_by(Treatment, LUI, site) %>% 
  nest(data=c(sci.name,  N)) %>% # Emma added abundance here
  ungroup() %>% 
  mutate(Treatment = factor(Treatment)) %>% # to order treatments in the plot
  mutate(Treatment = fct_relevel(Treatment, c("Control", "CPFA", "CAFA"))) %>%
  arrange(Treatment, LUI)

# count sites from alpha without high LUI
gamma_data%>%
  select(site, Treatment, LUI) %>% 
  distinct(site, Treatment, LUI) %>% 
  group_by(Treatment) %>% 
  count (LUI, name = 'no.sites')

# for n_samples, get 3 sites (alpha data) from CAFA
# n_sites = 3 # which is the minimum number of sites per category
# n_samps = 200
# 
# gamma_metrics <- tibble()
# 
# for (i in 1:n_samps) {
#   print(i)
#   # get these n_Site rows and calculate alpha S
#   alpha_sub_samp <- gamma_data %>%
#     # from each group
#     group_by(Treatment, LUI) %>%
#     # get 10 rows
#     sample_n(n_sites, replace = F) %>%
#     # unnest
#     unnest(cols = c(data)) %>%
#     ungroup() %>%
#     # calculate PIE, S for each Site
#     group_by(Treatment, LUI, site) %>%
#     mutate(
#       alphaS = n_distinct(sci.name),
#       alphaN = sum(N),
#       alpha_Spie = vegan::diversity(N, index = 'invsimpson')
#     ) %>%
#     ungroup() %>%
#     # get the minimum N and mean S for each treatment
#     group_by(Treatment, LUI) %>%
#     mutate(min_alpha_N = min(alphaN),
#            mean_alpha_S = mean(alphaS),
#            mean_alpha_Spie = mean(alpha_Spie)) %>%
#     ungroup()
# 
#   alpha_Sn_sub_samp <- alpha_sub_samp %>%
#     group_by(Treatment, LUI, site) %>%
#     nest( data = c(sci.name, N, min_alpha_N) ) %>%
#     mutate(Sn = purrr::map(data, ~mobr::rarefaction(.x$N, method = 'IBR',
#                                                     effort = unique(.x$min_alpha_N)))) %>%
#     ungroup() %>%
#     unnest(Sn) %>%
#     group_by(Treatment, LUI ) %>%
#     mutate(mean_alpha_Sn = mean(Sn))
# 
#   # aggregate same sub sample for gamma calculations
#   sub_samp <- alpha_sub_samp %>%
#     # aggregate data to gamma scale
#     group_by(Treatment, LUI, sci.name) %>%
#     summarise(N = sum(N)) %>%
#     ungroup() %>%
#     # get minimum N for Sn
#     group_by(Treatment, LUI) %>%
#     mutate(
#       totalN = sum(N)
#     ) %>%
#     ungroup() %>%
#     mutate(minN = min(totalN))
# 
#   # calculate Sn(s)
#   gamma_Sn_sub_samp <- sub_samp %>%
#     # add min_alpha_N for rarefying gamma down to an alpha-scale N
#     left_join(alpha_sub_samp %>%
#                 dplyr::distinct(Treatment, LUI, min_alpha_N),
#               by = c('Treatment', 'LUI') ) %>%
#     group_by(Treatment, LUI) %>%
#     # gamma Sn (for gamma-scale and alpha-scale minN)
#     nest(  data = c(sci.name, N, minN, min_alpha_N ) ) %>%
#     mutate(Sn = purrr::map(data, ~mobr::rarefaction(.x$N,
#                                                     method = 'IBR',
#                                                     effort = unique(.x$minN))),
#            Sn_alpha = purrr::map(data, ~mobr::rarefaction(.x$N,
#                                                           method = 'IBR',
#                                                           effort = unique(.x$min_alpha_N)))) %>%
#     unnest( c(Sn, Sn_alpha) )
# 
#   # calculate the metrics we want
#   gamma_metrics <- gamma_metrics %>%
#     bind_rows(
#       sub_samp %>%
#         group_by(Treatment, LUI) %>%
#         summarise( totalN = sum(N),
#                    S = n_distinct(sci.name),
#                    ENSPIE = vegan::diversity(N, index = 'invsimpson')
#         )  %>%
#         left_join( alpha_sub_samp %>%
#                      select(Treatment, LUI, mean_alpha_S, mean_alpha_Spie) %>%
#                      distinct() %>% mutate(alpha_S = mean_alpha_S,
#                                            alpha_Spie = mean_alpha_Spie) %>%
# 
#                      left_join( gamma_Sn_sub_samp %>%
#                                   select(Treatment, LUI, Sn, Sn_alpha) %>%
#                                   distinct() %>% mutate(gamma_Sn = Sn,
#                                                         gamma_Sn_alphaN = Sn_alpha) %>%
# 
#                                   left_join( alpha_Sn_sub_samp  %>%
#                                                select(Treatment, LUI,  mean_alpha_Sn) %>%
#                                                distinct() %>% mutate( alpha_Sn = mean_alpha_Sn) %>%
# 
#                                                # add counter for sample based rarefaction
#                                                mutate(resample = i))
#                      ) ) )
# }
# 
# save(gamma_metrics, file= 'gamma_metrics.Rdata')

load('gamma_metrics.Rdata')
# View(gamma_metrics)

# summarise the resamples
gamma_boot_results <- gamma_metrics %>% 
  # calculate beta-diversities (beta=gamma/alpha) 
  mutate(beta_S = S/alpha_S,
         beta_S_PIE = ENSPIE/alpha_Spie,
         beta_Sn = gamma_Sn_alphaN/alpha_Sn) %>% 
  group_by(Treatment, LUI) %>% 
  summarise(N_mu = mean(totalN),
            N_median = median(totalN),
            N_Q95 = quantile(totalN, probs = 0.95, names = F),
            N_Q5 = quantile(totalN, probs = 0.05, names = F),
            S_mu = mean(S),
            S_median = median(S),
            S_Q95 = quantile(S, probs = 0.95, names = F),
            S_Q5 = quantile(S, probs = 0.05, names = F),
            ENSPIE_mu = mean(ENSPIE),
            ENSPIE_median = median(ENSPIE),
            ENSPIE_Q95 = quantile(ENSPIE, probs = 0.95, names = F),
            ENSPIE_Q5 = quantile(ENSPIE, probs = 0.05, names = F),
            Sn_median = median(gamma_Sn),
            Sn_Q95 = quantile(gamma_Sn, probs = 0.95, names = F),
            Sn_Q5 = quantile(gamma_Sn, probs = 0.05, names = F),
            # and the beta = gamma/alpha diversitites
            beta_S_median = median(beta_S),
            beta_S_Q95 = quantile(beta_S, probs = 0.95, names = F),
            beta_S_Q5 = quantile(beta_S, probs = 0.05, names = F),
            beta_S_PIE_median = median(beta_S_PIE),
            beta_S_PIE_Q95 = quantile(beta_S_PIE, probs = 0.95, names = F),
            beta_S_PIE_Q5 = quantile(beta_S_PIE, probs = 0.05, names = F),
            beta_Sn_median = median(beta_Sn),
            beta_Sn_Q95 = quantile(beta_Sn, probs = 0.95, names = F),
            beta_Sn_Q5 = quantile(beta_Sn, probs = 0.05, names = F)) 


mycol() # my viridis function

# plot results
# gamma_S_all----
gamma_S_all <- ggplot() +
  geom_point(
    data = gamma_boot_results,
    aes(
      x = Treatment,
      group = LUI,
      y = S_median,
      colour = Treatment,
      shape = LUI
    ),
    position = position_dodge(width = 0.5),
    size = 4
  ) +
  geom_errorbar(
    data = gamma_boot_results,
    aes(
      x = Treatment,
      group = LUI,
      ymin = S_Q5,
      ymax = S_Q95,
      colour = Treatment
    ),
    position = position_dodge(width = 0.5),
    linewidth = 1.3,
    width = 0.1
  ) +
  labs(x = '',
       y = 'S' # Richness
  ) +
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), 
                                  panel.grid.minor = element_blank(), 
                                  strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(subtitle= 'a)')+
  guides(fill= 'none')
  
 gamma_S_all

# gamma_S_PIE_all----
gamma_S_PIE_all <- ggplot() +
  geom_point(
    data = gamma_boot_results,
    aes(
      x = Treatment,
      y = ENSPIE_median,
      group = LUI,
      colour = Treatment,
      shape = LUI
    ),
    position = position_dodge(width = 0.5),
    size = 4
  ) +
  geom_errorbar(
    data = gamma_boot_results,
    aes(
      x = Treatment,
      ymin = ENSPIE_Q5,
      ymax = ENSPIE_Q95,
      group = LUI,
      colour = Treatment
    ),
    position = position_dodge(width = 0.5),
    linewidth = 1.3,
    width = 0.1
  ) +
  labs(x = '',
       y = expression(paste(S[PIE]))
  ) +
   scale_color_viridis(discrete = T, option="D")  + 
   scale_fill_viridis(discrete = T, option="D")  + 
   theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), 
                                   panel.grid.minor = element_blank(), 
                                   strip.background = element_rect(colour="black", fill="white"),
                                   legend.position="none")+
   labs(subtitle= 'b)')+
   guides(fill= 'none')

gamma_S_PIE_all

# beta_S_all----
beta_S_all <- ggplot() +
  geom_point(data = gamma_boot_results,
             aes(x = Treatment, 
                 y = beta_S_median,
                 group = LUI,
                 colour = Treatment,
                 shape = LUI),
             position = position_dodge(width = 0.5),
             size = 4) +
  geom_errorbar(data = gamma_boot_results,
                aes(x = Treatment, 
                    ymin = beta_S_Q5, 
                    ymax = beta_S_Q95,
                    group = LUI,
                    colour = Treatment
                ),
                position = position_dodge(width = 0.5),
                linewidth = 1.3,
                width = 0.1) +
  labs(x = '',
       y = expression(paste(italic(beta),'-S'))
  ) +
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), 
                                  panel.grid.minor = element_blank(), 
                                  strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(subtitle= 'c)')+
  guides(fill= 'none')

beta_S_all

# beta_S_PIE_all----
beta_S_PIE_all <- ggplot() +
  geom_point(data = gamma_boot_results,
             aes(x = Treatment, 
                 y = beta_S_PIE_median,
                 group= LUI,
                 colour = Treatment,
                 shape= LUI),
             position = position_dodge(width = 0.5),
             size = 4) +
  geom_errorbar(data = gamma_boot_results,
                aes(x = Treatment, 
                    ymin = beta_S_PIE_Q5, 
                    ymax = beta_S_PIE_Q95, 
                    colour = Treatment,
                    group= LUI),
                position = position_dodge(width = 0.5),
                linewidth = 1.3,
                width = 0.1) +
  labs(x = '',
       y = expression(paste(italic(beta), '-', S[PIE]))
  ) +
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), 
                                  panel.grid.minor = element_blank(), 
                                  strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none")+
  labs(subtitle= 'd)')+
  guides(fill= 'none')

beta_S_PIE_all

# for legend
fig3leg <- beta_S_PIE_all+ theme(legend.position = 'bottom')
# Use extract_legend function
legend <- extract_legend(fig3leg)



fig3 <- (gamma_S_all|gamma_S_PIE_all)/(beta_S_all|beta_S_PIE_all)/(legend)

save(fig3, file= 'fig3.Rdata')

load('fig3.Rdata')
fig3



