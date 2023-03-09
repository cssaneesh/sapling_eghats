source('sapling_01_data.R')

# ref: 06a_gamma_scale_all_fishres.R

# We have more samples in CAFA

visit_01.lui %>% group_by(treatment, village) %>%
    distinct(treatment) %>% group_by(treatment) %>% count(treatment, name= 'sites')

# Solution, bootstrap sampling

gamma_data <- visit_01.lui %>%
  group_by(site, treatment, LUI, sci.name) %>%
  count(sci.name, name = 'abundance') %>%
  ungroup() %>%
  # calculate metrics for each site
  group_by(site, treatment, LUI, sci.name, abundance) %>%
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

n_sites = 10
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
  #mutate(Treatment = factor(treatment)) %>% # to order treatments in the plot
  mutate(Treatment = fct_relevel(treatment, c("Control", "CPFA", "CAFA"))) %>% 
  arrange(Treatment)

# Beta----
beta.sap <- gamma_boot_results %>% select(treatment, beta_S_mean , beta_S_Q5, beta_S_Q95) %>%
  rename(Estimate = beta_S_mean,
         Lower = beta_S_Q5,
         Upper = beta_S_Q95) %>%
  mutate_if(is.numeric, round, 2) %>% mutate('Scale'= rep('Beta', 3))

beta.sap

# Gamma----
gamma.sap <- gamma_boot_results %>% select(treatment, S_mean , S_Q5, S_Q95) %>%
  rename(Estimate = S_mean,
         Lower = S_Q5,
         Upper = S_Q95) %>%
  mutate_if(is.numeric, round, 2) %>% mutate('Scale'= rep('Gamma', 3))

gamma.sap

# plot results
(gamma_S_all <- ggplot() +
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
        plot.tag.position = c(0.3, 0.8)))

(gamma_S_PIE_all <- ggplot() +
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
        plot.tag.position = c(0.3, 0.8)))


(beta_S_all <- ggplot() +
  geom_point(data = gamma_boot_results,
             aes(x = Treatment, y = beta_S_median, colour = Treatment),
             size = 4) +
  geom_errorbar(data = gamma_boot_results,
                aes(x = Treatment, ymin = beta_S_Q5, ymax = beta_S_Q95, 
                    colour = Treatment),
                linewidth = 1.3,
                width = 0.1) +
  scale_colour_grey() +
  labs(x = '',
       y = expression(paste(italic(beta),'-S'))) +
  theme_bw() +
  theme(legend.position = 'none', 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        plot.tag.position = c(0.3, 0.8)))

(beta_S_PIE_all <- ggplot() +
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
        plot.tag.position = c(0.3, 0.8)))


