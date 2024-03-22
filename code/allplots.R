




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
ggCVgrid_sqrt
ggDynamics_evenness
ggCVgrid_evenness
ggCVgrid_evenness_sqrt
ggS0
ggRRcontrol
ggRRcv
ggRRcv_sqrt
ggRRcontrol_evenness
ggRRcv_eveness
ggRRcv_eveness_sqrt
ggRRwp
ggRRcv_wp
ggRRcv_wp_sqrt
ggRRwp_evenness
ggRRcv_wp_evenness
ggRRcv_wp_evenness_sqrt

ggturnover
ggpcoa_hell
ggpcoa_hell_alltreatments
ggpcoa_clouds 



