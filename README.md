# Promoting tree diversity and growth rates by reducing disturbances.   [![DOI](https://zenodo.org/badge/12345.svg)](https://zenodo.org/badge/latestdoi/12345)

Saneesh Cherapurath Soman, Emma Ladouceur, Tiffany M. Knight (20xx) Promoting tree diversity and growth rates by reducing disturbances.
Journal of Ecologixxx Aplxxxx. Article DOI: [12345](https://doi.org/1234) & Data DOI: [12345](https://doi.org/12345)  


_Find below the description of the data and scripts used in this study_

## Data
1. `seedling.dat.csv` encompasses information related to seedlings in various sites. The dataset includes the following columns:

      1. **Treatment:** The type of treatment applied.
      2. **Village:** The specific village where the observation was made.
      3. **Site:** The location or site within the village.
    4. **Sci.name:** Scientific name of the observed seedling.
    5. **Seedling:** Number of seedling.
    6. **LUI:** Land Use Intensity index.
    7. **Goat:** Relative number of goats to a site.
    8. **Trenches:** Relative area of trenches to a site.
  
2. `rgr.csv` encompasses information related  to sapling's relative Growth and Health. The dataset includes the following columns:

    1. **Treatment:** The type of treatment applied to the saplings.
    2. **Village:** The village where the observations were recorded.
    3. **Site:** The specific site within the village where saplings were monitored.
    4. **Species:** The species of saplings being studied.
    5. **Disturbance:** The presence or absence of disturbances affecting saplings.
    6. **rgrH:** A metric representing sapling growth (height measured in m).
    7. **rgr_rcd:** Another metric related to sapling growth (rcd= root collar diameter, measured in cm).
    8. **sap_health:** The health status of saplings (based on browsing, trampling and water stress).
## Questions and R scripts*
Q1: What is the tree seedling diversity and abundance at the α-scale in response to treatment and LUI?   
_Script1:_  `02_fig_seedling_div.R` - Analysis and to make **Figure 2** Diversity for the α-scale. (a) species abundance, (b) species richness, (c) rarefied richness and (d) Effective Number of Species for Probability of Interspecific Encounter.    
Q2:  What is the species accumulation from individual sites (α-scale) to broader treatment levels (γ-scale)?  
_Script2:_  `03_fig_seedling_Sp.acu.R` -  Analysis and to make **Figure 3** Species richness (a) and species evenness (b) accumulation curves across sites for each treatment.  
Q3:   What is the proportion of saplings that had a presence of disturbance, specifically browsing, trampling, and water stress?    
_Script3:_   `04_fig_prop_sapling.R` -  Analysis and to make **Figure 4** The effect of experimental treatments on the proportion of healthy saplings (a) and the proportion of healthy saplings for each species within sites (b).   
Q4:   What is the variations in the relative growth of saplings, specifically changes in root collar diameter (RCD) and height?    
_Script4:_  `05_fig_rgr_sapling.R`  - Analysis and to make **Figure 5** The relative growth of saplings in terms of root collar diameter (RCD) and height, without disturbance (a) and (c). Species-specific responses are shown in (b) and (d).  
    _*R packages used_   
        library(bayesplot)    
        library(brms)    
        library(cowplot)    
        library(dagitty)    
        library(gridExtra)    
        library(gt)    
        library(iNEXT.3D)    
        library(patchwork)    
        library(plotly)    
        library(readr)    
        library(readxl)    
        library(tidybayes)    
        library(tidyverse)    
        library(viridis)    

The above scripts generates figures and exicution steps are given below:  
**Execution Steps**  
     - Run `source()` at the beginning of the scripts to load the necessary packages and data.  
     - To explore the raw data and examine the Bayesian model, follow these options:  
     - Load a pre-fitted model: Specific Bayesian models are provided in the script, commented out (#). Uncomment and run the relevant section to load the pre-fitted model.  
     - Model Fitting: If interested in fitting the Bayesian model from scratch, uncomment the model fitting section and run. Note that Bayesian model fitting may require substantial computation time.  
     - Generate Figures: Once the model is loaded or fitted, proceed to create figures using the codes given.  


**Saneesh Cherapurath Soman, Emma Ladouceur, Tiffany M. Knight (20xx) Promoting tree diversity and growth rates by reducing disturbances.
Journal of Ecologixxx Aplxxxx. Article DOI: [12345](https://doi.org/1234)**



