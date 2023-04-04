source('seedling_01_data.R')

source('seedling_01_data.R')

# Comparing number of sites in treatments
seedling.dat %>% 
  filter(seedling >0) %>% 
  group_by(treatment) %>% 
  summarise(N_sites=n_distinct(site)) # We have more samples in CAFA

seedling.dat.lui <- seedling.dat %>% 
  mutate(LUI.fct= as.numeric(cut_number(LUI, 3))) %>% 
  mutate(LUI.fct = as.factor(LUI.fct)) %>%
  mutate(LUI.fct = recode(LUI.fct, `1` = "low", `2` = "medium", `3` = "high"))
# Solution, bootstrap sampling

gamma_data.lui <- seedling.dat.lui %>% # alpha_summary_sd, sd= seedling
  filter(seedling >0) %>% 
  filter(sci.name!= 'Senna siamea') %>%  # introduced ornamental tree
  group_by(site, LUI.fct, sci.name) %>%
  summarise(abundance= sum(seedling)) %>%
  ungroup() %>%
  # calculate metrics for each site
  group_by(site, LUI.fct, sci.name, abundance) %>%
  summarise (N = sum(abundance)) %>% 
  # total number of saplings
  ungroup() %>% 
  group_by(LUI.fct, site) %>% 
  nest(data=c(sci.name, N)) %>% 
  ungroup() %>% 
  mutate(LUI.fct = factor(LUI.fct)) %>% # to order treatments in the plot
  mutate(LUI.fct = fct_relevel(LUI.fct, c("low", "medium", "high"))) %>%
  arrange(LUI.fct)

# for n_samples, get 10 sites (alpha data) from CAFA

n_sites = 10
n_samps =200

gamma_metrics_lui <- tibble()

for (i in 1:n_samps) {
  print(i)
  # get these n_Site rows and calculate alpha S
  alpha_sub_samp <- gamma_data.lui %>%
    # from each group
    group_by(LUI.fct) %>%
    # get 10 rows
    sample_n(n_sites, replace = F) %>%
    # unnest
    unnest() %>%
    # calculate PIE, S for each Site
    group_by(LUI.fct, site) %>%
    mutate(
      alphaS = n_distinct(sci.name),
      alpha_Spie = vegan::diversity(N, index = 'invsimpson')
    ) %>%
    ungroup() %>%
    # get the minimum N and mean S for each LUI.fct
    group_by(LUI.fct) %>%
    mutate(mean_alpha_S = mean(alphaS),
           mean_alpha_Spie = mean(alpha_Spie)) %>%
    ungroup()
  # aggregate same sub sample for gamma calculations
  sub_samp <- alpha_sub_samp %>%
    # aggregate data to gamma scale
    group_by(LUI.fct, sci.name) %>%
    summarise(sp.treat.count = sum(N)) %>%
    ungroup() %>% 
    # get minimum N for Sn
    group_by(LUI.fct) %>%
    mutate(
      trt_count = sum(sp.treat.count),
      gamma_rel_count = (sp.treat.count / trt_count)
    ) %>%
    ungroup() %>%
    mutate(minrel = min(gamma_rel_count))
  # calculate the metrics we want
  gamma_metrics_lui <- gamma_metrics_lui %>%
    bind_rows(
      sub_samp %>%
        group_by(LUI.fct) %>%
        summarise(
          S = n_distinct(sci.name),
          ENSPIE = vegan::diversity(gamma_rel_count, index = 'invsimpson')
        )  %>%
        # add counter for sample based rarefaction
        left_join(
          alpha_sub_samp %>%
            select(LUI.fct, mean_alpha_S, mean_alpha_Spie) %>%
            distinct() %>%
            group_by(LUI.fct) %>%
            mutate(
              alpha_S = mean_alpha_S,
              alpha_Spie = mean_alpha_Spie,
              resample = i
            )
        )
    )
}

save(gamma_metrics_lui, file= 'gamma_metrics_lui.Rdata')

load('gamma_metrics_lui.Rdata')

gamma_boot_results <-
  gamma_metrics_lui %>% # calculate beta-diversities (beta = gamma/alpha)
  mutate(beta_S = S / alpha_S,
         beta_S_PIE = ENSPIE / alpha_Spie) %>%
  group_by(LUI.fct) %>%
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
  #mutate(LUI.fct = factor(LUI.fct)) %>% # to order treatments in the plot
  mutate(LUI.fct = fct_relevel(LUI.fct, c("low", "medium", "high"))) %>% 
  arrange(LUI.fct)


# Gamma----
gamma <- gamma_boot_results %>% select(LUI.fct, S_median , S_Q5, S_Q95) %>%
  rename(Estimate = S_median,
         Lower = S_Q5,
         Upper = S_Q95) %>%
  mutate_if(is.numeric, round, 2) %>% mutate('Scale'= rep('Gamma', 3))

gamma

# plot results
gamma_S_plot <- ggplot() +
  geom_point(data = gamma_boot_results,
             aes(x = LUI.fct, y = S_median, colour = LUI.fct),
             size = 4) +
  geom_errorbar(data = gamma_boot_results,
                aes(x = LUI.fct, ymin = S_Q5, ymax = S_Q95, 
                    colour = LUI.fct),
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
        plot.tag.position = c(0.3, 0.8)) #+
#geom_hline(yintercept = 7, lty=2, col= 'red')

gamma_S_plot


gamma_S_PIE <- gamma_boot_results %>% select(LUI.fct, ENSPIE_median , ENSPIE_Q5, ENSPIE_Q95) %>%
  rename(Estimate = ENSPIE_median,
         Lower = ENSPIE_Q5,
         Upper = ENSPIE_Q95) %>%
  mutate_if(is.numeric, round, 2) %>% mutate('Scale'= rep('Gamma', 3))

gamma_S_PIE

gamma_S_PIE_plot <- ggplot() +
  geom_point(data = gamma_boot_results,
             aes(x = LUI.fct, y = ENSPIE_median, colour = LUI.fct),
             size = 4) +
  geom_errorbar(data = gamma_boot_results,
                aes(x = LUI.fct, ymin = ENSPIE_Q5, ymax = ENSPIE_Q95, 
                    colour = LUI.fct),
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
beta_S <- gamma_boot_results %>% select(LUI.fct, beta_S_median , beta_S_Q5, beta_S_Q95) %>%
  rename(Estimate = beta_S_median,
         Lower = beta_S_Q5,
         Upper = beta_S_Q95) %>%
  mutate_if(is.numeric, round, 2) %>% mutate('Scale'= rep('Beta', 3))

beta_S

beta_S.plot <- gamma_boot_results %>% ggplot() +
  geom_point(aes(x = LUI.fct, y = beta_S_median, colour = LUI.fct), size = 4) +
  geom_errorbar(
    aes(x = LUI.fct, ymin = beta_S_Q5, ymax = beta_S_Q95),
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


beta_S_PIE_all <- ggplot() +
  geom_point(data = gamma_boot_results,
             aes(x = LUI.fct, y = beta_S_PIE_median, colour = LUI.fct),
             size = 4) +
  geom_errorbar(data = gamma_boot_results,
                aes(x = LUI.fct, ymin = beta_S_PIE_Q5, ymax = beta_S_PIE_Q95, 
                    colour = LUI.fct),
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

