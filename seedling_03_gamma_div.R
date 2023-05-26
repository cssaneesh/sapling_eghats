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
  filter(lui_cat!= 'high') %>% # tEmma added this
  group_by(site) %>% 
  summarise(tot.adult= sum(adult)) %>% # add the total number of adults in each site
  left_join(seedling.dat %>% select(!adult), multiple = "all") %>% 
  group_by(site, Treatment, sci.name, lui_cat, tot.adult, village) %>%
  summarise(abundance= sum(seedling)) %>% # abundance of seedling
  ungroup() %>%
  filter(abundance>0) %>% 
  # calculate metrics for each site
  group_by(site, Treatment, lui_cat, sci.name, tot.adult) %>% # Emma removed abundance here
  summarise(N = sum(abundance)) %>%  
  # total number of saplings
  ungroup() %>% 
  group_by(Treatment, lui_cat, site) %>% 
  nest(data=c(sci.name,  N)) %>% # Emma added abundance here
  ungroup() %>% 
  mutate(Treatment = factor(Treatment)) %>% # to order treatments in the plot
  mutate(Treatment = fct_relevel(Treatment, c("Control", "CPFA", "CAFA"))) %>%
  arrange(Treatment, lui_cat)

View(gamma_data)

# for n_samples, get 10 sites (alpha data) from CAFA

n_sites = 3 # Emma changed to 3, which is the minimum number of sites per category
n_samps = 200

gamma_metrics <- tibble()

for (i in 1:n_samps) {
  print(i)
  # get these n_Site rows and calculate alpha S
  alpha_sub_samp <- gamma_data %>%
    # from each group
    group_by(Treatment, lui_cat) %>%
    # get 10 rows
    sample_n(n_sites, replace = F) %>%
    # unnest
    unnest(cols = c(data)) %>%
    ungroup() %>%
    # calculate PIE, S for each Site
    group_by(Treatment, lui_cat, site) %>%
    mutate(
      alphaS = n_distinct(sci.name),
      alphaN = sum(N),
      alpha_Spie = vegan::diversity(N, index = 'invsimpson')
    ) %>%
    ungroup() %>%
    # get the minimum N and mean S for each treatment
    group_by(Treatment, lui_cat) %>%
    mutate(min_alpha_N = min(alphaN),
          mean_alpha_S = mean(alphaS),
           mean_alpha_Spie = mean(alpha_Spie)) %>%
    ungroup()
  
  alpha_Sn_sub_samp <- alpha_sub_samp %>% 
    group_by(Treatment, lui_cat, site) %>% 
    nest( data = c(sci.name, N, min_alpha_N) ) %>% 
    mutate(Sn = purrr::map(data, ~mobr::rarefaction(.x$N, method = 'IBR',
                                                    effort = unique(.x$min_alpha_N)))) %>% 
    ungroup() %>% 
    unnest(Sn) %>% 
    group_by(Treatment, lui_cat ) %>% 
    mutate(mean_alpha_Sn = mean(Sn))
           
  # aggregate same sub sample for gamma calculations
  sub_samp <- alpha_sub_samp %>%
    # aggregate data to gamma scale
    group_by(Treatment, lui_cat, sci.name) %>%
    summarise(N = sum(N)) %>%
    ungroup() %>% 
    # get minimum N for Sn
    group_by(Treatment, lui_cat) %>%
    mutate(
      totalN = sum(N)
    ) %>%
    ungroup() %>%
    mutate(minN = min(totalN))
  
  # calculate Sn(s)
  gamma_Sn_sub_samp <- sub_samp %>% 
    # add min_alpha_N for rarefying gamma down to an alpha-scale N
    left_join(alpha_sub_samp %>% 
                dplyr::distinct(Treatment, lui_cat, min_alpha_N),
              by = c('Treatment', 'lui_cat') ) %>% 
    group_by(Treatment, lui_cat) %>% 
    # gamma Sn (for gamma-scale and alpha-scale minN)
    nest(  data = c(sci.name, N, minN, min_alpha_N ) ) %>% 
    mutate(Sn = purrr::map(data, ~mobr::rarefaction(.x$N, 
                                                    method = 'IBR',
                                                    effort = unique(.x$minN))),
           Sn_alpha = purrr::map(data, ~mobr::rarefaction(.x$N, 
                                                          method = 'IBR',
                                                          effort = unique(.x$min_alpha_N)))) %>% 
    unnest( c(Sn, Sn_alpha) )
  
  # calculate the metrics we want
  gamma_metrics <- gamma_metrics %>%
    bind_rows(
      sub_samp %>%
        group_by(Treatment, lui_cat) %>%
        summarise( totalN = sum(N),
          S = n_distinct(sci.name),
          ENSPIE = vegan::diversity(N, index = 'invsimpson')
        )  %>%
        left_join( alpha_sub_samp %>%
                      select(Treatment, lui_cat, mean_alpha_S, mean_alpha_Spie) %>%
                      distinct() %>% mutate(alpha_S = mean_alpha_S, 
                                            alpha_Spie = mean_alpha_Spie) %>%

                    left_join( gamma_Sn_sub_samp %>%
                      select(Treatment, lui_cat, Sn, Sn_alpha) %>%
                      distinct() %>% mutate(gamma_Sn = Sn,
                        gamma_Sn_alphaN = Sn_alpha) %>%

                    left_join( alpha_Sn_sub_samp  %>%
                      select(Treatment, lui_cat,  mean_alpha_Sn) %>%
                      distinct() %>% mutate( alpha_Sn = mean_alpha_Sn) %>%
                      
        # add counter for sample based rarefaction
        mutate(resample = i))
    ) ) )
}

View(gamma_metrics)

save(gamma_metrics, file= 'gamma_metrics.Rdata')

load('gamma_metrics.Rdata')

# summarise the resamples
gamma_boot_results <- gamma_metrics %>% 
  # calculate beta-diversities (beta=gamma/alpha) 
  mutate(beta_S = S/alpha_S,
         beta_S_PIE = ENSPIE/alpha_Spie,
         beta_Sn = gamma_Sn_alphaN/alpha_Sn) %>% 
  group_by(Treatment, lui_cat) %>% 
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

head(gamma_boot_results)

# plot results
gamma_S_all <- ggplot() +
  geom_point(data = gamma_boot_results,
             aes(x = Treatment, group = lui_cat, y = S_median, colour = Treatment,
                 shape= lui_cat),
             position = position_dodge(width = 0.5),
             size = 4) +
  geom_errorbar(data = gamma_boot_results,
                aes(x = Treatment, group = lui_cat, ymin = S_Q5, ymax = S_Q95,
                    colour = Treatment),
                position = position_dodge(width = 0.5),
                linewidth = 1.3,
                width = 0.1) +
  scale_colour_grey() +
  labs(x = '',
       y = 'Species richness (S)'#,
       # tag = '(b)'
  ) +
  theme_bw() +
  theme(legend.position = 'bottom', 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        plot.tag.position = c(0.3, 0.8))

gamma_S_all

# Saneesh needs to edit from here

gamma_S_PIE_all <- ggplot() +
  geom_point(data = gamma_boot_results,
             aes(x = Treatment, y = ENSPIE_median, colour = Treatment),
             size = 4) +
  geom_errorbar(data = gamma_boot_results,
                aes(x = Treatment, ymin = ENSPIE_Q5, ymax = ENSPIE_Q95, 
                    colour = Treatment),
                size = 1.3,
                width = 0.1) +
  scale_colour_grey() +
  labs(x = '',
       y = expression(paste(S[PIE]))#,
       # subtitle = 'All fish combined',
       # tag = '(a)'
  ) +
  theme_bw() +
  theme(legend.position = 'bottom', 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        plot.tag.position = c(0.3, 0.8))


beta_S_all <- ggplot() +
  geom_point(data = gamma_boot_results,
             aes(x = Treatment, y = beta_S_median, colour = Treatment),
             size = 4) +
  geom_errorbar(data = gamma_boot_results,
                aes(x = Treatment, ymin = beta_S_Q5, ymax = beta_S_Q95, 
                    colour = Treatment),
                size = 1.3,
                width = 0.1) +
  scale_colour_grey() +
  labs(x = '',
       y = expression(paste(italic(beta),'-S'))#,
       # tag = '(b)'
  ) +
  theme_bw() +
  theme(legend.position = 'none', 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        plot.tag.position = c(0.3, 0.8))

beta_S_PIE_all <- ggplot() +
  geom_point(data = gamma_boot_results,
             aes(x = Treatment, y = beta_S_PIE_median, colour = Treatment),
             size = 4) +
  geom_errorbar(data = gamma_boot_results,
                aes(x = Treatment, ymin = beta_S_PIE_Q5, ymax = beta_S_PIE_Q95, 
                    colour = Treatment),
                size = 1.3,
                width = 0.1) +
  scale_colour_grey() +
  labs(x = '',
       y = expression(paste(italic(beta), '-', S[PIE]))#,
       # subtitle = 'All fishes combined',
       # tag = '(a)'
  ) +
  theme_bw() +
  theme(legend.position = 'none', 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        plot.tag.position = c(0.3, 0.8))



