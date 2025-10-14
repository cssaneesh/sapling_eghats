source('00_seedling_data.R')

# Analysis----
Site_prep_iNext <- seedling.dat %>%
  filter(seedling>0) %>% 
  filter(sci.name!= 'Senna siamea') %>%  # introduced ornamental tree
  mutate(species = sci.name) %>%
  arrange(site, Treatment) %>%
  filter(LUI < 1.20) %>% # medium LUI
  select(-c(sci.name, LUI, Goat, Trenches)) %>%
  mutate(pres = as.numeric(1)) %>% 
  mutate(Treatment = factor(Treatment)) %>% # to order Treatments in the plot
  mutate(Treatment = fct_relevel(Treatment, c("Control","CPFA","CAFA"))) %>% 
  arrange(Treatment)

Site.list <- Site_prep_iNext %>%
  split(.$Treatment)

Site.matrix.list <- purrr::map(
  Site.list,
  ~ .x %>%
    select(species, 
           site, 
           pres,
           # seedling
           ) %>%
    distinct() %>%
    spread(key = site, 
           value = pres,
           # value = seedling
           ) %>%
    replace(is.na(.), 0) %>%
    column_to_rownames(var = "species")
)  

#  Taxonomic diversity
TD_treat_out <-
  iNEXT3D(
    data = Site.matrix.list,
    diversity = 'TD',
    q = c(0, 1, 2),
    # datatype= 'abundance' ,
    # datatype = 'incidence_freq',
    datatype = 'incidence_raw',
    size = c(1:30),
    nboot = 0
  )

TD_treat_out$TDInfo
# Assemblage = the Treatment or groups
# T = Reference sample size
# U = Total number of incidents
# S.obs = Observed species richness
# SC = Sample coverage

TD_treat_out$TDAsyEst # to see the asymptotic diversity estimates

# table of diversity estimates
TD_treat_out$TDAsyEst %>% filter(qTD == 'Simpson diversity') %>%  # q=2 (Simpson diversity) 
select(-s.e., -qTD.LCL, -qTD.UCL) %>% 
  rename (Treatment = Assemblage) %>% 
  mutate(Treatment= fct_relevel(Treatment, c("Control", "CPFA", "CAFA"))) %>% 
  arrange(Treatment) %>% 
  gt()

# Make df for ploting----
# Extract from the list=TD_treat_out "TDiNextEst" and "size_based" components
# TD_treat_out$TDiNextEst

Site.TD.df <- TD_treat_out %>% # Create a new dataframe named Site.TD.df for plotting
  purrr::pluck("TDiNextEst", "size_based")
# we will use Site.TD.df for further visualization or analysis

Site_info <- Site_prep_iNext %>%
  distinct() %>% mutate(Assemblage = as.character(Treatment))

Site.hill.TD <-
  Site.TD.df %>% left_join(Site_info, by = join_by("Assemblage")) %>%
  mutate(Order.q  = case_when(Order.q  == "0" ~ "(a)", # q=0 species richness
                              Order.q == "1" ~ "q = 1", # q=1 Shannon diversity
                              Order.q == "2" ~ "(b)")) %>% # q=2 Simpson diversity or evenness
  filter(!Order.q == "q = 1")

df.point <-
  Site.hill.TD[which(Site.hill.TD$Method == "Observed"), ]

df.line <-
  Site.hill.TD[which(Site.hill.TD$Method != "Observed"), ]

df.line$Method <- factor(df.line$Method,
                         c("Rarefaction", "Extrapolation"))

# Figure 3----
Treatment_colors <- c("Control" = "#3b5d4d", "CPFA" = "#c5af99","CAFA" = "#ffd365")

acc.curve <- ggplot(Site.hill.TD ,
                aes(x = mT, y = qTD,   color = Treatment)) +
  facet_wrap( ~ Order.q) +
  geom_point(aes(),
             shape = 1,
             size = 3,
             data = df.point) +
  geom_line(aes(linetype = Method), lwd = 0.75, data = df.line) +
  labs(
    x = "Number of sites",
    y = "Taxonomic diversity",) +
  # scale_color_manual(values = Treatment_colors) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size = 8)) +
  guides(col = guide_legend(ncol = 15)) +
  theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  theme(
    panel.grid.major = element_line(colour = "gray86", linewidth = 0.1),
    panel.background = element_rect(fill = "white")
  )

acc.curve


  p_with_subtitle <- ggdraw(acc.curve) +
  draw_label("a", x = 0.05, y = 0.95, hjust = 0, vjust = 1, size = 12) +
  draw_label("b", x = 0.52, y = 0.95, hjust = 0, vjust = 1, size = 12)

  p_with_subtitle  
  
  acc.curve_1 <- acc.curve +
  scale_color_viridis(discrete = T, option = "D")  +
  scale_fill_viridis(discrete = T, option = "D")  +
  theme_bw(base_size = 14) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(colour =
                                      "black", fill = "white"),
    # legend.position = c(x=1.32, y=.96),
    legend.position = c(x=1.4, y=.97),
    legend.justification = c('right', 'top'),
    legend.background = element_rect(fill = NA)
  ) +
  guides(fill = 'none')+ theme(strip.text.x = element_blank())+ # to remove the grey box
  ggtitle('', subtitle = '(a)')+
  theme(plot.subtitle = element_text(hjust = 1, vjust = 3))+
  labs(title = '', subtitle = '')+ # to create space for a and b in the upcoming step.
    # Use scale_color_manual to define the new legend labels
    scale_color_manual(
      name = "Treatment", # Keep the legend title if desired
      # Assign the new labels to the original factor levels in the correct order
      values = c("Control" = "#440154",  # Specify colors if needed, otherwise use the defaults
                 "CPFA" = "#21908c",
                 "CAFA" = "#fde725"),
      labels = c("Control" = "Both present",
                 "CPFA" = "Fire present",
                 "CAFA" = "Both excluded")
    )

acc.curve_1

figure3 <- acc.curve_1+
  theme(
    strip.background.x = element_blank(), # to get rid of grey box with species names
    strip.text.x = element_text(hjust = 0, size = 14)
  )

figure3

save(figure3, file= 'figure3.Rdata')

load(file= 'figure3.Rdata')
figure3

ggsave('figure3.jpg', figure3,
       width = 10,
       height = 6,
       dpi = 300)
