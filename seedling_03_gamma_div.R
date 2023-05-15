source('seedling_01_data.R')

# Comparing number of sites in treatments
seedling.dat %>% 
  filter(seedling >0) %>% 
  group_by(Treatment) %>% 
  summarise(N_sites=n_distinct(site)) # We have more samples in CAFA


# categorize LUI into three groups

seedling.dat <-
  seedling.dat %>%
  mutate(lui_cat = as.numeric(cut_number(LUI, 3))) %>%
  mutate(lui_cat = as.factor(lui_cat)) %>%
  mutate(lui_cat = recode(
    lui_cat,
    `1` = 'low',
    `2` = 'medium',
    `3` = 'high'
  ))

seedling.dat %>% 
  filter(seedling >0) %>% 
  group_by(Treatment) %>% 
  count (lui_cat, name = 'no.sites')

seedling.dat %>% 
  filter(seedling >0) %>% 
  group_by(Treatment) %>% 
  count (lui_cat, name = 'no.sites') %>% 
  ggplot()+
  geom_bar(aes(x= lui_cat, y= `no.sites`, fill= Treatment), stat = 'identity', position = 'dodge')



# Bootstrap sampling

gamma_data <- seedling.dat %>% # alpha_summary_sd, sd= seedling
  filter(site!= 'APA19_CPFA') %>% # to avoid zero N and inf ENSPIE
  filter(seedling>0) %>% 
  filter(sci.name!= 'Senna siamea') %>%  # introduced ornamental tree
  group_by(site) %>% 
  summarise(tot.adult= sum(adult)) %>% # add the total number of adults in each site
  left_join(seedling.dat %>% select(!adult), multiple = "all") %>% 
  group_by(site, treatment, sci.name, LUI, tot.adult, village) %>%
  summarise(abundance= sum(seedling)) %>% # abundance of seedling
  ungroup() %>%
  filter(abundance>0) %>% 
  # calculate metrics for each site
  group_by(site, treatment, LUI, sci.name, tot.adult, abundance) %>%
  summarise (N = sum(abundance)) %>% 
  # total number of saplings
  ungroup() %>% 
  group_by(treatment, site) %>% 
  nest(data=c(sci.name, N)) %>% 
  ungroup() %>% 
  mutate(Treatment = factor(treatment)) %>% # to order treatments in the plot
  mutate(Treatment = fct_relevel(treatment, c("Control", "CPFA", "CAFA"))) %>%
  arrange(Treatment)

# for n_samples, get 10 sites (alpha data) from CAFA

n_sites = 11
n_samps =200

gamma_metrics <- tibble()

for (i in 1:n_samps) {
  print(i)
  # get these n_Site rows and calculate alpha S
  alpha_sub_samp <- gamma_data %>%
    # from each group
    group_by(treatment) %>%
    # get 10 rows
    sample_n(n_sites, replace = F) %>%
    # unnest
    unnest() %>%
    # calculate PIE, S for each Site
    group_by(treatment, site) %>%
    mutate(
      alphaS = n_distinct(sci.name),
      alpha_Spie = vegan::diversity(N, index = 'invsimpson')
    ) %>%
    ungroup() %>%
    # get the minimum N and mean S for each treatment
    group_by(treatment) %>%
    mutate(mean_alpha_S = mean(alphaS),
           mean_alpha_Spie = mean(alpha_Spie)) %>%
    ungroup()
  # aggregate same sub sample for gamma calculations
  sub_samp <- alpha_sub_samp %>%
    # aggregate data to gamma scale
    group_by(treatment, sci.name) %>%
    summarise(sp.treat.count = sum(N)) %>%
    ungroup() %>% 
    # get minimum N for Sn
    group_by(treatment) %>%
    mutate(
      trt_count = sum(sp.treat.count),
      gamma_rel_count = (sp.treat.count / trt_count)
    ) %>%
    ungroup() %>%
    mutate(minrel = min(gamma_rel_count))
  # calculate the metrics we want
  gamma_metrics <- gamma_metrics %>%
    bind_rows(
      sub_samp %>%
        group_by(treatment) %>%
        summarise(
          S = n_distinct(sci.name),
          ENSPIE = vegan::diversity(gamma_rel_count, index = 'invsimpson')
        )  %>%
        # add counter for sample based rarefaction
        left_join(
          alpha_sub_samp %>%
            select(treatment, mean_alpha_S, mean_alpha_Spie) %>%
            distinct() %>%
            group_by(treatment) %>%
            mutate(
              alpha_S = mean_alpha_S,
              alpha_Spie = mean_alpha_Spie,
              resample = i
            )
        )
    )
}

save(gamma_metrics, file= 'gamma_metrics.Rdata')

load('gamma_metrics.Rdata')

gamma_boot_results <-
  gamma_metrics %>% # calculate beta-diversities (beta = gamma/alpha)
  mutate(beta_S = S / alpha_S,
         beta_S_PIE = ENSPIE / alpha_Spie) %>%
  group_by(treatment) %>%
  summarise(
    S_mean = mean(S),
    S_median = median(S),
    S_Q95 = quantile(S, probs = 0.95, names = F),
    S_Q5 = quantile(S, probs = 0.05, names = F),
    ENSPIE_mean = mean(ENSPIE),
    ENSPIE_median = median(ENSPIE),
    ENSPIE_Q95 = quantile(ENSPIE, probs = 0.95, names = F),
    ENSPIE_Q5 = quantile(ENSPIE, probs = 0.05, names = F),
    beta_S_mean = mean(beta_S),
    beta_S_median = median(beta_S),
    beta_S_Q95 = quantile(beta_S, probs = 0.95, names = F),
    beta_S_Q5 = quantile(beta_S, probs = 0.05, names = F),
    beta_S_PIE_mean = mean(beta_S_PIE),
    beta_S_PIE_median = median(beta_S_PIE),
    beta_S_PIE_Q95 = quantile(beta_S_PIE, probs = 0.95, names = F),
    beta_S_PIE_Q5 = quantile(beta_S_PIE, probs = 0.05, names = F)
  ) %>% 
  # to order treatments in the plot
  mutate(Treatment = fct_relevel(treatment, c("Control", "CPFA", "CAFA"))) %>% 
  arrange(Treatment)


# Gamma----
gamma <-
  gamma_boot_results %>% select(treatment, S_median , S_Q5, S_Q95) %>%
  rename(
    Treatment = treatment,
    Estimate = S_median,
    Lower = S_Q5,
    Upper = S_Q95
  ) %>%
  mutate_if(is.numeric, round, 2) %>% mutate('Scale' = rep('Gamma', 3)) %>% gt()


gamma

# plot results
gamma_S_plot <- ggplot() +
  geom_point(data = gamma_boot_results,
             aes(x = Treatment, y = S_median, colour = Treatment),
             size = 4) +
  geom_errorbar(data = gamma_boot_results,
                aes(x = Treatment, ymin = S_Q5, ymax = S_Q95, 
                    colour = Treatment),
                linewidth = 1.3,
                width = 0.1) +
  scale_colour_grey() +
  labs(x = '',
       y = 'Species richness (S)'
  ) +
  theme_bw() +
  theme(legend.position = 'none', 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        plot.tag.position = c(0.3, 0.8))

gamma_S_plot

gamma_S_PIE <-
  gamma_boot_results %>% select(treatment, ENSPIE_median , ENSPIE_Q5, ENSPIE_Q95) %>%
  rename( Treatment= treatment,
    Estimate = ENSPIE_median,
         Lower = ENSPIE_Q5,
         Upper = ENSPIE_Q95) %>%
  mutate_if(is.numeric, round, 2) %>% mutate('Scale' = rep('Gamma', 3)) %>% gt()

gamma_S_PIE

gamma_S_PIE_plot <- ggplot() +
  geom_point(data = gamma_boot_results,
             aes(x = Treatment, y = ENSPIE_median, colour = Treatment),
             size = 4) +
  geom_errorbar(data = gamma_boot_results,
                aes(x = Treatment, ymin = ENSPIE_Q5, ymax = ENSPIE_Q95, 
                    colour = Treatment),
                linewidth = 1.3,
                width = 0.1) +
  scale_colour_grey() +
  labs(x = '',
       y = expression(paste(S[PIE]))) +
  theme_bw() +
  theme(legend.position = 'none', 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        plot.tag.position = c(0.3, 0.8))

gamma_S_PIE_plot

# Beta----
beta_S <-
  gamma_boot_results %>% select(treatment, beta_S_median , beta_S_Q5, beta_S_Q95) %>%
  rename(
    Treatment = treatment,
    Estimate = beta_S_median,
    Lower = beta_S_Q5,
    Upper = beta_S_Q95
  ) %>%
  mutate_if(is.numeric, round, 2) %>% mutate('Scale'= rep('Beta', 3)) %>% gt()

beta_S

beta_S.plot <- gamma_boot_results %>% ggplot() +
  geom_point(aes(x = treatment, y = beta_S_median, colour = treatment), size = 4) +
  geom_errorbar(
    aes(x = treatment, ymin = beta_S_Q5, ymax = beta_S_Q95, col= treatment),
    linewidth = 1,
    width = 0.1
  )+
  scale_colour_grey() +
  labs(title = " ",
       x = ' ',
       y = expression(paste(italic(beta), "- species diversity (S)"))) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = 'none',
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    plot.tag.position = c(0.3, 0.8)
  ) +
  theme(
    panel.grid.major = element_line(colour = "gray86", linewidth = 0.1),
    panel.background = element_rect(fill = "white")
  ) #+ labs(subtitle = 'b)')

beta_S.plot


beta_ENSPIE <-
  gamma_boot_results %>% select(treatment, beta_S_PIE_median , beta_S_PIE_Q5, beta_S_PIE_Q95) %>%
  rename(
    Treatment = treatment,
    Estimate = beta_S_PIE_median,
    Lower = beta_S_PIE_Q5,
    Upper = beta_S_PIE_Q95
  ) %>%
  mutate_if(is.numeric, round, 2) %>% mutate('Scale'= rep('Beta', 3)) %>% gt()

beta_ENSPIE

names(gamma_boot_results)

beta_S_PIE_all <- ggplot() +
    geom_point(data = gamma_boot_results,
               aes(x = Treatment, y = beta_S_PIE_median, colour = Treatment),
               size = 4) +
    geom_errorbar(data = gamma_boot_results,
                  aes(x = Treatment, ymin = beta_S_PIE_Q5, ymax = beta_S_PIE_Q95, 
                      colour = Treatment),
                  linewidth = 1.3,
                  width = 0.1) +
    scale_colour_grey() +
    labs(x = '',
         y = expression(paste(italic(beta), '-', S[PIE]))) +
    theme_bw() +
    theme(legend.position = 'none', 
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 18),
          plot.tag.position = c(0.3, 0.8))

beta_S_PIE_all

