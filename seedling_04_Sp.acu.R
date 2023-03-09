# data----
source('seedling_01_data.R')
# Analysis----
Site_prep_iNext <- seedling.dat %>%
  mutate(species = sci.name) %>%
  arrange(site, treatment) %>%
  select(-c(sci.name, adult, adu.stat, LUI)) %>%
  mutate(pres = as.numeric(1)) %>% 
  mutate(treatment = factor(treatment)) %>% # to order treatments in the plot
  mutate(treatment = fct_relevel(treatment, c("Control","CPFA","CAFA"))) %>% 
  arrange(treatment)

Site.list <- Site_prep_iNext %>%
  split(.$treatment)

Site.matrix.list <- purrr::map(
  Site.list,
  ~ .x %>%
    select(species, site, pres) %>%
    distinct() %>%
    spread(key = site, value = pres) %>%
    replace(is.na(.), 0) %>%
    column_to_rownames(var = "species")
)

#  Taxonomic diversity
TD_treat_out <-
  iNEXT3D(
    data = Site.matrix.list,
    diversity = 'TD',
    q = c(0, 1, 2),
    datatype = 'incidence_raw',
    size = c(1:50),
    nboot = 0
  )

TD_treat_out$DataInfo
# Assemblage = the treatment or groups
# T = Reference sample size
# U = Total number of incidents
# S.obs = Observed species richness
# SC = Sample coverage

TD_treat_out$AsyEst # to see the asymptotic diversity estimates


# Make df for ploting----
Site.TD.df <- TD_treat_out %>%
  purrr::pluck("iNextEst", "size_based")

Site_info <- Site_prep_iNext %>%
  distinct() %>% mutate(Assemblage = as.character(treatment))

Site.hill.TD <- Site.TD.df %>% left_join(Site_info, multiple = 'all') %>%
  mutate(Order.q  = case_when(Order.q  == "0" ~ "q = 0", # q=0 species richness
                              Order.q == "1" ~ "q = 1", # q=1 Shannon diversity
                              Order.q == "2" ~ "q = 2")) %>% # q=2 Simpson diversity or evenness
  filter(!Order.q == "q = 1")

df.point <-
  Site.hill.TD[which(Site.hill.TD$Method == "Observed"), ]

df.line <-
  Site.hill.TD[which(Site.hill.TD$Method != "Observed"), ]

df.line$Method <- factor(df.line$Method,
                         c("Rarefaction", "Extrapolation"))

# Plot----
treatment_colors <- c("Control" = "#3b5d4d", "CPFA" = "#c5af99","CAFA" = "#ffd365")

fig_6 <- ggplot(Site.hill.TD ,
                aes(x = nt, y = qD,   color = treatment)) +
  facet_wrap( ~ Order.q) +
  geom_point(aes(),
             shape = 1,
             size = 3,
             data = df.point) +
  geom_line(aes(linetype = Method), lwd = 0.75, data = df.line) +
  labs(
    x = "Number of sites",
    y = "Taxonomic diversity",) +
  scale_color_manual(values = treatment_colors) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size = 8)) +
  guides(col = guide_legend(ncol = 15)) +
  theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  theme(
    panel.grid.major = element_line(colour = "gray86", linewidth = 0.1),
    panel.background = element_rect(fill = "white")
  )

fig_6

(
  fig_6 +
    theme(plot.caption = element_text(
      size = 8, face = "italic",
      hjust = 0.0
    )) +theme(
      legend.position = c(1.15, .95),
      legend.justification = c('right', 'top')
    ) +
    theme(legend.background = element_rect(fill = NA))
)
