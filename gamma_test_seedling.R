# test
source('seedling_01_data.R')

# Comparing number of sites in treatments
seedling.dat %>% 
  filter(seedling >0) %>% 
  group_by(Treatment) %>% 
  summarise(N_sites=n_distinct(site)) # We have more samples in CAFA


# categorize LUI into three groups

lui_cat <-
  seedling.dat %>%
  filter(seedling > 0) %>%
  mutate(lui_cat = case_when(
    LUI > 0 & LUI <= 0.60 ~ 'low',
    LUI > 0.60 & LUI <= 1.20 ~ 'medium',
    LUI > 1.20 ~ 'high'
  ))

lui_cat %>% 
  filter(seedling >0) %>% 
  group_by(Treatment) %>% 
  count (lui_cat, name = 'no.sites')

lui_cat %>% 
  filter(seedling >0) %>% 
  group_by(Treatment) %>% 
  count (lui_cat, name = 'no.sites') %>% 
  ggplot()+
  geom_bar(aes(x= lui_cat, y= `no.sites`, fill= Treatment), stat = 'identity', position = 'dodge')

# Bootstrap sampling

gamma_data_prep <- lui_cat %>% # alpha_summary_sd, sd= seedling
  filter(lui_cat!= 'high') %>%
  group_by(site) %>% 
  summarise(tot.adult= sum(adult)) %>% # add the total number of adults in each site
  left_join(lui_cat %>% select(!adult), multiple = "all") %>% 
  group_by(site, Treatment, sci.name, lui_cat, village) %>%
  summarise(sp.abundance= sum(seedling), .groups = 'drop') %>% # abundance of seedling
  arrange(Treatment, lui_cat)
  
# count sites

gamma_data_prep%>%
  select(site, Treatment, lui_cat) %>% 
  distinct(site, Treatment, lui_cat) %>% 
  group_by(Treatment) %>% 
  count (lui_cat, name = 'no.sites')

gamma_data <- gamma_data_prep %>%   
  # calculate metrics for each site
  group_by(Treatment, lui_cat, site, sci.name) %>%
  summarise (N = sum(sp.abundance), .groups = 'drop') %>% 
  # total number of seedling species
  group_by(Treatment, site, lui_cat) %>% 
  nest(data=c(sci.name, N)) %>% 
  ungroup() %>% 
  arrange(Treatment, lui_cat)


gamma_data$data


gamma_data %>%
  group_by(Treatment) %>% 
  count (lui_cat, name = 'no.sites')

# for n_samples, get 6 sites (alpha data) from lui categor

n_sites = 3
n_samps =100

gamma_metrics <- tibble()

for (i in 1:n_samps) {
  print(i)
  # get these n_Site rows and calculate alpha S
  alpha_sub_samp <- gamma_data %>%
    # from each group
    group_by(Treatment, lui_cat) %>%
    # get xx rows (n_sites)
    sample_n(n_sites, replace = F) %>%
    # unnest
    unnest(cols = c(data)) %>%
    # calculate N, PIE, S for each Site
    group_by(Treatment, site, lui_cat) %>%
    mutate(
      alphaS = n_distinct(sci.name),
      alphaN = sum(N),
      alpha_Spie = vegan::diversity(N, index = 'invsimpson')
    ) %>%
    ungroup() %>%
    # get the minimum N and mean S for each treatment
    group_by(Treatment, # lui_cat
             ) %>%
    mutate(
      min_alpha_N = min(alphaN),
      mean_alpha_S = mean(alphaS),
      mean_alpha_Spie = mean(alpha_Spie)
    ) %>%
    ungroup()
  
  # need alpha Sn for beta-Sn (see Chase et al. 2018 Ecol Lett for beta-Sn introduction, nothing is going on
  # so we chose not to present it here)
  alpha_Sn_sub_samp <- alpha_sub_samp %>%
    group_by(Treatment, site, lui_cat) %>%
    nest(data=c(N, min_alpha_N)) %>%
    mutate(Sn = purrr::map(data, ~mobr::rarefaction(.x$N, method = 'IBR',
                                                    effort = unique(.x$min_alpha_N)))) %>%
    ungroup() %>%
    unnest(Sn) %>%
    group_by(Treatment, lui_cat) %>%
    mutate(mean_alpha_Sn = mean(Sn))

  # aggregate same sub sample for gamma calculations
  sub_samp <- alpha_sub_samp %>%
    # aggregate data to gamma scale
    group_by(Treatment, sci.name, lui_cat) %>%
    summarise(N = sum(N), .groups = 'drop') %>% 
    # get minimum N for Sn
    group_by(Treatment, lui_cat) %>% 
    mutate(totalN= sum(N)) %>% 
    ungroup() %>% 
    mutate(minN = min(totalN))
  
  # calculate Sn(s)
  
  gamma_Sn_sub_samp <- sub_samp %>%
    # add min_alpha_N for rarefying gamma down to an alpha-scale N
    left_join(alpha_sub_samp %>%
                dplyr::distinct(Treatment, lui_cat, min_alpha_N),
              by= join_by(Treatment, lui_cat)) %>%
    group_by(Treatment) %>%
    # gamma Sn (for gamma-scale and alpha-scale minN)
    nest(data = c(N, minN, min_alpha_N)) %>%
    mutate(
      Sn = purrr::map(data, ~ mobr::rarefaction(
        .x$N,
        method = 'IBR',
        effort = unique(.x$minN)
      )),
      Sn_alpha = purrr::map(data, ~ mobr::rarefaction(
        .x$N,
        method = 'IBR',
        effort = unique(.x$min_alpha_N)
      ))
    ) %>%
    unnest(Sn, Sn_alpha)
  
  # calculate the metrics we want
  gamma_metrics <- gamma_metrics %>% 
    bind_rows(sub_samp %>% 
                group_by(Treatment, lui_cat) %>% 
                summarise(totalN = sum(N),
                          S = n_distinct(sci.name),
                          ENSPIE = vegan::diversity(N, index = 'invsimpson'),
                          S_PIE = mobr::calc_PIE(N, ENS = T)#, .groups = 'drop'
                          ) %>% 
              # add counter for sample based rarefaction
              mutate(gamma_Sn = gamma_Sn_sub_samp$Sn,
                     gamma_Sn_alphaN = gamma_Sn_sub_samp$Sn_alpha,
                     alpha_S = unique(alpha_sub_samp$mean_alpha_S),
                     alpha_Spie = unique(alpha_sub_samp$mean_alpha_Spie),
                     alpha_Sn = unique(alpha_Sn_sub_samp$mean_alpha_Sn),
                     resample = i)
    )
}
