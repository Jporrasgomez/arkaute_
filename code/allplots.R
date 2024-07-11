




library(tidyverse) # manage data
library(DT) # visualise tables in html
library(viridis) # use color-blind friendly palettes
library(ggrepel) # avoid overlapping texts in the figures
library(codyn) # calculate community dynamics metrics
library(vegan) # calculate community ecology metrics
library(ggpubr)
library(stringr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(readr)
library(ggpubr)
library(tidyverse)
library(gridExtra)



source("code/flora_bio_div_ab.R")
source("code/species_composition_analysis.R")

theme_set(theme_bw()+ theme(legend.position = "NULL"))


ggDynamics
ggCVgrid
ggCVgrid_squared
ggDynamics_evenness
ggCVgrid_evenness
ggCVgrid_evenness_squared
ggS0
ggRRcontrol
ggRRcv
ggRRcv_squared
ggRRcontrol_evenness
ggRRcv_eveness
ggRRcv_eveness_squared
ggRRwp
ggRRcv_wp
ggRRcv_wp_squared
ggRRwp_evenness
ggRRcv_wp_evenness
ggRRcv_wp_evenness_squared

ggturnover
ggpcoa_hell
ggpcoa_hell_alltreatments
ggpcoa_clouds 



