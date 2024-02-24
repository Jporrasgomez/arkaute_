
# Cosas que hacer ####
#Hacer comprobaciones de los datos cada vez que se transformen las bases de datos


#Packages   ####
library(stringr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(readr)
library(ggpubr)
library(tidyverse)
library(gridExtra)

source('code/tools/basicFun.R')
source("code/first_script.R")



### OUTLIERS ##### (check "Dynamics and outliers.Rmd" for more info)

############### Transformation database ############

## Se trabaja con "flora" pero podríamos trabajar con "flora1". El problema de "flora1" es que, al eliminar los NA de biomasa,
# no tenemos datos para ninguna variable de los muestreos 0, 1 y 2. 
 #Removing species amaranthus



#Agrupar por sampling, plot y treatment primero. 
#Aquí se aplica, para biomasa, el siguiente criterio: se estima la biomasa de cada especie multiplicando su abundancia por 
#la masa promedio de los individuos medidos para dicha especie. 

flora_samplings <-  flora %>%
  group_by(sampling, date, month, treatment, plot) %>%
  reframe(biomass = sum(biomass, na.rm = T),
          #total abundance per plot (m2) (replicate of treatment)
          n_species = n_species,  #total number of species per plot
          abundance = sum(abundance, na.rm = T)) %>% # total coverage of plot
  distinct(sampling, date, month, plot, treatment, biomass, n_species, abundance)

radcoeff_df <- read.csv("data/radcoeff_df.csv") %>%
  select(-X) 

radcoeff_df$plot <- as.factor(radcoeff_df$plot)
radcoeff_df$sampling <- as.factor(radcoeff_df$sampling)
radcoeff_df$treatment <- as.factor(radcoeff_df$treatment) 
radcoeff_df$treatment <- factor(radcoeff_df$treatment, levels = c("c", "w", "p", "wp"))
radcoeff_df$sampling <- factor(radcoeff_df$sampling, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))  #Sort from lowest to highest
  
flora_samplings <- merge(flora_samplings, radcoeff_df)                                                              

hist(flora_samplings$n_species)
hist(flora_samplings$abundance)
hist(flora_samplings$biomass)


#Hacer comprobaciones de los datos en esta base de datos


flora_treatments <-  flora_samplings %>%
  group_by(treatment) %>%
  reframe(biomass = mean(biomass, na.rm = T), 
          n_species = mean(n_species, na.rm = T), 
          abundance = mean(abundance, na.rm = T)) %>%
  distinct(treatment, biomass, n_species, abundance)                                  

# Gráficos por muestreo y tratamiento####

# Other way with facet_grid

theme_set(theme_bw()+ theme(legend.position = "NULL"))
ggDynamics <- 
ggarrange(
  
ggplot(flora_samplings, aes(x = sampling, y = n_species, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Richness") +
  facet_grid(~ treatment)  + 
  scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8),

ggplot(flora_samplings, aes(x = sampling, y = abundance, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Abundance") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8),

ggplot(flora_samplings, aes(x = sampling, y = biomass, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Biomass") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8),

nrow=3, ncol=1)

# !! Eliminar plot 13 sampling 2 para coeficientes de evenness, o revisar. 
ggDynamics_evenness <- 
  ggarrange(
    
    ggplot(flora_samplings, aes(x = sampling, y = Y_zipf, fill = treatment)) +
      geom_boxplot() +
      labs(x = " ", y = "Y_zipf") +
      facet_grid(~ treatment)  + 
      scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8),
    
    ggplot(flora_samplings, aes(x = sampling, y = mu_log, fill = treatment)) +
      geom_boxplot() +
      labs(x = " ", y = "mu_log") +
      facet_grid(~ treatment) + 
      scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8),
    
    ggplot(flora_samplings, aes(x = sampling, y = sigma_log, fill = treatment)) +
      geom_boxplot() +
      labs(x = " ", y = "sigma_log") +
      facet_grid(~ treatment) + 
      scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8),
    
    nrow=3, ncol=1)


##### RESPONSE RATIO  Log(RR); RR = variable at point i / variable at point reference #######################


RR_treatments <- flora_samplings
samps <- unique(RR_treatments$sampling)

#100% puedo reducir todas las lineas de código al incluir un vector que incluya las variabes. Tipo:
variables <- c(colnames(flora_samplings[, 7:11]))

for (i in 1:length(samps)) {
  subset_c <- subset(RR_treatments, sampling == samps[i] & treatment == "c")
  subset_w <- subset(RR_treatments, sampling == samps[i] & treatment == "w")
  subset_p <- subset(RR_treatments, sampling == samps[i] & treatment == "p")
  subset_wp <- subset(RR_treatments, sampling == samps[i] & treatment == "wp")
  RR_treatments$RR_abundance_C[RR_treatments$sampling == samps[i]] <-
    round(log(RR_treatments$abundance[RR_treatments$sampling == samps[i]] /mean(subset_c$abundance)),2)  # we take reference values as the mean of the 4 replicates
  RR_treatments$RR_abundance_W[RR_treatments$sampling == samps[i]] <-
    round(log(RR_treatments$abundance[RR_treatments$sampling == samps[i]]/mean(subset_w$abundance)),2)
  RR_treatments$RR_abundance_P[RR_treatments$sampling == samps[i]] <-
    round(log(RR_treatments$abundance[RR_treatments$sampling == samps[i]]/mean(subset_p$abundance)),2)
  RR_treatments$RR_abundance_WP[RR_treatments$sampling == samps[i]] <-
    round(log(RR_treatments$abundance[RR_treatments$sampling == samps[i]]/mean(subset_wp$abundance)),2)

  RR_treatments$RR_richness_C[RR_treatments$sampling == samps[i]] <-
    round(log(RR_treatments$n_species[RR_treatments$sampling == samps[i]]/mean(subset_c$n_species)),2)
  RR_treatments$RR_richness_W[RR_treatments$sampling == samps[i]] <-
    round(log(RR_treatments$n_species[RR_treatments$sampling == samps[i]]/mean(subset_w$n_species)),2)
  RR_treatments$RR_richness_P[RR_treatments$sampling == samps[i]] <-
    round(log(RR_treatments$n_species[RR_treatments$sampling == samps[i]]/mean(subset_p$n_species)),2)
  RR_treatments$RR_richness_WP[RR_treatments$sampling == samps[i]] <-
    round(log(RR_treatments$n_species[RR_treatments$sampling == samps[i]]/mean(subset_wp$n_species)),2)
  
  RR_treatments$RR_yzipf_C[RR_treatments$sampling == samps[i]] <-
    round(log(RR_treatments$Y_zipf[RR_treatments$sampling == samps[i]] /mean(subset_c$Y_zipf)),2)
  RR_treatments$RR_yzipf_W[RR_treatments$sampling == samps[i]] <-
    round(log(RR_treatments$Y_zipf[RR_treatments$sampling == samps[i]] /mean(subset_w$Y_zipf)),2)
  RR_treatments$RR_yzipf_P[RR_treatments$sampling == samps[i]] <-
    round(log(RR_treatments$Y_zipf[RR_treatments$sampling == samps[i]] /mean(subset_p$Y_zipf)),2)
  RR_treatments$RR_yzipf_WP[RR_treatments$sampling == samps[i]] <-
    round(log(RR_treatments$Y_zipf[RR_treatments$sampling == samps[i]] /mean(subset_wp$Y_zipf)),2)
  
  RR_treatments$RR_mulog_C[RR_treatments$sampling == samps[i]] <-
    round(log(RR_treatments$mu_log[RR_treatments$sampling == samps[i]] /mean(subset_c$mu_log)),2)
  RR_treatments$RR_mulog_W[RR_treatments$sampling == samps[i]] <-
    round(log(RR_treatments$mu_log[RR_treatments$sampling == samps[i]] /mean(subset_w$mu_log)),2)
  RR_treatments$RR_mulog_P[RR_treatments$sampling == samps[i]] <-
    round(log(RR_treatments$mu_log[RR_treatments$sampling == samps[i]] /mean(subset_p$mu_log)),2)
  RR_treatments$RR_mulog_WP[RR_treatments$sampling == samps[i]] <-
    round(log(RR_treatments$mu_log[RR_treatments$sampling == samps[i]] /mean(subset_wp$mu_log)),2)
  
  RR_treatments$RR_sigmalog_C[RR_treatments$sampling == samps[i]] <-
    round(log(RR_treatments$sigma_log[RR_treatments$sampling == samps[i]] /mean(subset_c$sigma_log)),2)
  RR_treatments$RR_sigmalog_W[RR_treatments$sampling == samps[i]] <-
    round(log(RR_treatments$sigma_log[RR_treatments$sampling == samps[i]] /mean(subset_w$sigma_log)),2)
  RR_treatments$RR_sigmalog_P[RR_treatments$sampling == samps[i]] <-
    round(log(RR_treatments$sigma_log[RR_treatments$sampling == samps[i]] /mean(subset_p$sigma_log)),2)
  RR_treatments$RR_sigmalog_WP[RR_treatments$sampling == samps[i]] <-
    round(log(RR_treatments$sigma_log[RR_treatments$sampling == samps[i]] /mean(subset_wp$sigma_log)),2)
  
  
  
  rm(subset_c)
  rm(subset_w)
  rm(subset_p)
  rm(subset_wp)
}


# RR: Control as reference of W, P and WP

ggRRcontrol<- 
ggarrange(
  ggplot(RR_treatments[RR_treatments$treatment != "c", ], aes(x = sampling, y = RR_abundance_C, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "logRR(abundance)") +
    facet_grid(~ treatment) + 
    scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
    geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
  
  ggplot(RR_treatments[RR_treatments$treatment != "c", ], aes(x = sampling, y = RR_richness_C, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "logRR(richness)") +
    facet_grid(~ treatment) + 
    scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
    geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
  
  nrow = 2, ncol = 1)

ggRRcontrol_evenness<- 
  ggarrange(
    ggplot(RR_treatments[RR_treatments$treatment != "c", ], aes(x = sampling, y = RR_yzipf_C, fill = treatment)) +
      geom_boxplot() +
      labs(x = " ", y = "logRR(Y)") +
      facet_grid(~ treatment) + 
      scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    ggplot(RR_treatments[RR_treatments$treatment != "c", ], aes(x = sampling, y = RR_mulog_C, fill = treatment)) +
      geom_boxplot() +
      labs(x = " ", y = "logRR(mu)") +
      facet_grid(~ treatment) + 
      scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    ggplot(RR_treatments[RR_treatments$treatment != "c", ], aes(x = sampling, y = RR_sigmalog_C, fill = treatment)) +
      geom_boxplot() +
      labs(x = " ", y = "logRR(sigma)") +
      facet_grid(~ treatment) + 
      scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    nrow = 3, ncol = 1)


# P and W as references of WP. 

RR_wp_ab <- pivot_longer(RR_treatments, cols = c("RR_abundance_W", "RR_abundance_P"), 
                         names_to = "RR_ab_treatment", values_to = "RR_ab_values")
RR_wp_ab <- select(RR_wp_ab, -all_of(grep("RR_richness", names(RR_wp_ab), value = TRUE)))

RR_wp_rich <- pivot_longer(RR_treatments, cols = c("RR_richness_W", "RR_richness_P"), 
                           names_to = "RR_rich_treatment", values_to = "RR_rich_values")
RR_wp_rich <- select(RR_wp_rich, -all_of(grep("RR_abundance", names(RR_wp_rich), value = TRUE)))

ggRRwp <- 
ggarrange(
ggplot(subset(RR_wp_ab, treatment == "wp"), aes(x = sampling, y = RR_ab_values, fill = RR_ab_treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "loggRR(abundance)") +
  facet_grid(~ RR_ab_treatment) + 
  scale_fill_manual(values = c("RR_abundance_W" = "pink", "RR_abundance_P" = "seagreen2"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),

ggplot(subset(RR_wp_rich, treatment == "wp"), aes(x = sampling, y = RR_rich_values, fill = RR_rich_treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "logRR(richness)") +
  facet_grid(~ RR_rich_treatment) + 
  scale_fill_manual(values = c("RR_richness_W" = "pink", "RR_richness_P" = "seagreen2"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),

nrow = 2, ncol = 1)


# COEFFICIENT OF VARIATION: CV = Standard deviation(x) / mean(x) ##########

flora_cv <- summarise(group_by(flora_samplings, sampling, treatment),
                               mean_biomass = mean(biomass, na.rm = T),
                               sd_biomass = sd(biomass, na.rm = T),
                               mean_richness = mean(n_species, na.rm = T),
                               sd_richness = sd(n_species, na.rm = T),
                               mean_abundance = mean(abundance, na.rm = T), 
                               sd_abundance = sd(abundance, na.rm = T))

flora_cv$CV_biomass <- round(flora_cv$mean_biomass/flora_cv$sd_biomass, 2) 
flora_cv$CV_richness <- round(flora_cv$mean_richness/flora_cv$sd_richness, 2)
flora_cv$CV_abundance <- round(flora_cv$mean_abundance/flora_cv$sd_abundance, 2)


ggCVgrid <- 
  ggarrange(
    
    ggplot(flora_cv, aes(x = sampling, y = CV_abundance, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      facet_grid(~ treatment)+
      labs(x = " ", y = "CV abundance") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8),
    
    ggplot(flora_cv, aes(x = sampling, y = CV_richness, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      facet_grid(~ treatment)+
      labs(x = " ", y = "CV richness ") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8),
    
    nrow = 2, ncol = 1)
 
# Response ratio of CV  

for (i in 1:length(samps)) {
  subset_c <- subset(flora_cv, sampling == samps[i] & treatment == "c")
  subset_w <- subset(flora_cv, sampling == samps[i] & treatment == "w")
  subset_p <- subset(flora_cv, sampling == samps[i] & treatment == "p")
  subset_wp <- subset(flora_cv, sampling == samps[i] & treatment == "wp")
  flora_cv$RR_CV_abundance_C[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$CV_abundance[flora_cv$sampling == samps[i]] /mean(subset_c$CV_abundance)),2) # Here it sais mean but it is only the mean of 1 value
  flora_cv$RR_CV_richness_C[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$CV_richness[flora_cv$sampling == samps[i]]/mean(subset_c$CV_richness)),2)
  flora_cv$RR_CV_abundance_W[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$CV_abundance[flora_cv$sampling == samps[i]]/mean(subset_w$CV_abundance)),2)
  flora_cv$RR_CV_richness_W[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$CV_richness[flora_cv$sampling == samps[i]]/mean(subset_w$CV_richness)),2)
  flora_cv$RR_CV_abundance_P[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$CV_abundance[flora_cv$sampling == samps[i]]/mean(subset_p$CV_abundance)),2)
  flora_cv$RR_CV_richness_P[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$CV_richness[flora_cv$sampling == samps[i]]/mean(subset_p$CV_richness)),2)
  flora_cv$RR_CV_abundance_WP[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$CV_abundance[flora_cv$sampling == samps[i]]/mean(subset_wp$CV_abundance)),2)
  flora_cv$RR_CV_richness_WP[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$CV_richness[flora_cv$sampling == samps[i]]/mean(subset_wp$CV_richness)),2)
  
  rm(subset_c)
  rm(subset_w)
  rm(subset_p)
  rm(subset_wp)
}


ggRRcv_control <- 
ggarrange(
  ggplot(flora_cv[flora_cv$treatment != "c", ], aes(x = sampling, y = RR_CV_abundance_C, color = treatment, group = treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(~ treatment)+
    labs(x = " ", y = "logRR(CV abundance)") + 
    scale_color_manual(values = c("c" = "green4", "p" = "blue3", "w" = "red4", "wp" = "purple2"))+
    geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
  
ggplot(flora_cv[flora_cv$treatment != "c", ], aes(x = sampling, y = RR_CV_richness_C, color = treatment, group = treatment)) +
  geom_point() +
  geom_line() +
  facet_grid(~ treatment)+
  labs(x = " ", y = "logRR(CV richness)") + 
  scale_color_manual(values = c("c" = "green4", "p" = "blue3", "w" = "red4", "wp" = "purple2"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),

nrow=2, ncol=1)
  
  
# LogRR (CV) Reference = w and p. Comparison with wp

RR_cv_wp_ab <- pivot_longer(flora_cv, cols = c("RR_CV_abundance_W", "RR_CV_abundance_P"), 
                            names_to = "RR_ab_treatment", values_to = "RR_ab_values")
RR_cv_wp_ab <- select(RR_cv_wp_ab, -all_of(grep("RR_richness", names(RR_cv_wp_ab), value = TRUE)))

RR_cv_wp_rich <- pivot_longer(flora_cv, cols = c("RR_CV_richness_W", "RR_CV_richness_P"), 
                              names_to = "RR_rich_treatment", values_to = "RR_rich_values")
RR_cv_wp_rich <- select(RR_cv_wp_rich, -all_of(grep("RR_abundance", names(RR_cv_wp_rich), value = TRUE)))

ggRRcv_wp <- 
ggarrange(
  ggplot(subset(RR_cv_wp_ab, treatment == "wp"), aes(x = sampling, y = RR_ab_values, color = RR_ab_treatment, group = RR_ab_treatment)) +
    geom_point() +
    geom_line() + 
    labs(x = " ", y = "logRR(CV abundance)") +
    facet_grid(~ RR_ab_treatment) + 
    scale_color_manual(values = c("RR_CV_abundance_W" = "pink4", "RR_CV_abundance_P" = "seagreen4"))+
    geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
  
  ggplot(subset(RR_cv_wp_rich, treatment == "wp"), aes(x = sampling, y = RR_rich_values, color = RR_rich_treatment, group = RR_rich_treatment)) +
    geom_point() +
    geom_line() +
    labs(x = " ", y = "logRR(CV richness)") +
    facet_grid(~ RR_rich_treatment) + 
    scale_color_manual(values = c("RR_CV_richness_W" = "pink4", "RR_CV_richness_P" = "seagreen4"))+
    geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
  
  nrow = 2, ncol = 1)


# ALL PLOTS ###############

ggDynamics
ggS0

ggRRsampling0 

ggRRcontrol
ggRRcv_control

ggRRwp
ggRRcv_wp

ggCVgrid

#tries that I would discard
ggCVsameplot
ggRRcv_try





