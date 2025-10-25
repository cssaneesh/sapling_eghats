# Packages----
rm(list = ls())
## install the latest version of iNEXT.3D from github##
# install.packages('devtools')
# library(devtools)
# install_github('AnneChao/iNEXT.3D')

# load the following packages, install if not installed :)
library(tidyverse)
library(readr)
library(brms)
library(tidybayes) # to make the qq plot of residuals of the model
library(patchwork)
library(iNEXT.3D)
library(viridis)
library(gt)
library(plotly)
library(gridExtra)
library(cowplot)
library(bayesplot)
library(broom.mixed)


# seedling data----
seedling.dat <- read.csv(file = 'seedling.dat.csv')

seedling.dat <- seedling.dat %>%
  mutate(Treatment = factor(Treatment)) %>%
  mutate(Treatment = fct_relevel(Treatment, c('Control', 'CPFA', 'CAFA'))) %>%
  arrange(Treatment)

names(seedling.dat)

# seedling species----
# seedling_sp.list <- seedling.dat %>% select(sci.name) %>% 
#   rename(scientificName= sci.name) %>%  arrange(scientificName) %>% 
#   distinct() %>% mutate(id = row_number()) %>% select(id, scientificName)
# write.csv(seedling_sp.list, file= 'seedling_sp.list.csv')

# no of sites----
seedling.dat %>%
  group_by(Treatment, village) %>%
  distinct(Treatment) %>% group_by(Treatment) %>%
  count(Treatment, name = 'sites')

seedling.dat %>% 
  filter(LUI <= 0.60) # %>% view() = low


seedling.dat %>% 
  filter(LUI >= 0.61,LUI <= 1.20 ) # %>% view() = medium

seedling.dat %>% 
  filter(LUI >= 1.21) # %>% view() = high

summary(seedling.dat$LUI)


# Functions----
# Create a function, which extracts legends from ggplot
extract_legend <- function(my_ggplot) {
  # library(gridExtra)
  step1 <- ggplot_gtable(ggplot_build(my_ggplot))
  step2 <-
    which(sapply(step1$grobs, function(x)
      x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

# my col function
mycol <- custom_theme <- function() {
  library(ggplot2)
  
  theme_set(
    theme_bw(base_size = 18) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour = "black", fill = "white"),
        legend.position = "bottom"
      )
  )
  
  scale_color_viridis <- function(discrete = TRUE,
                                  option = "D") {
    ggplot2::scale_color_viridis(discrete = discrete, option = option)
  }
  
  scale_fill_viridis <- function(discrete = TRUE,
                                 option = "D") {
    ggplot2::scale_fill_viridis(discrete = discrete, option = option)
  }
}


# sup figure

names(seedling.dat)

figureS3 <- seedling.dat %>% 
  group_by(Treatment,sci.name) %>% 
  summarise(total_count = sum(seedling), .groups = 'drop') %>% 
  group_by(sci.name) %>% 
  mutate(relative_count = total_count / sum(total_count)) %>%
  ungroup() %>% filter(!sci.name %in% c('Acacia nilotica','Albizia amara', 'Pongamia pinnata')) %>% 
  filter(relative_count < 1) %>% #view()
  ggplot(aes(x = sci.name, y = relative_count, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "The distribution of each species across the three treatments.",
    x = "Species",
    y = "Proportion of Species Population Found in Treatment",
    fill = "Treatment"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14)) +
  # theme_bw(base_size=13 )+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(
    name = "Treatment",
    
    # 1. Correctly provide the NEW TEXT labels (what you want to see)
    labels = c("Control" = "Both present",
               "CPFA" = "Fire present",
               "CAFA" = "Both excluded"),
    
    # 2. **CRITICAL FIX:** Explicitly provide the COLORS (the values)
    #    mapped to the original factor levels to avoid the "Insufficient values" error.
    values = c("Control" = "#f8766d", 
               "CPFA" = "#00ba38", 
               "CAFA" = "#619cff")
  )

figureS3

ggsave('output/figureS3.jpg', figureS3,
       width = 10,
       height = 6,
       dpi = 300)


