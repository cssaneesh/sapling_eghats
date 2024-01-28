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
