
# Cosas que hacer ####
#Hacer comprobaciones de los datos cada vez que se transformen las bases de datos


#Packages   ####
library(lubridate)
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



#Checking if we have mistakes (missing more than 2 variables with NA's)
#View(flora %>%
#  filter(rowSums(is.na(select(., Dm, Db, Cm, Cb))) > 2))
#Estan todos los datos de los muestreos 0, 1 y 2 y los 
#26 datos del muestreo 3 donde no cogíamos medidas si la abundancia era menor de 5.

#Añadir el numero de individuos que hay por sampling, plot y species. 
#Esto era necesario antes porque aplicabamos el criterio de "Si el numero de individuos es menor o igual que 4, la biomasa total
#de la especie será la suma de las biomasas unitarias de cada individuo. Si es mayor de 4, se calculará estimando con la abundancia"

#Dividir entre 100 los valores de abundancia para que sean valores de %
flora$abundance <- round(flora$abundance/100, 3)


flora <- flora %>%
  group_by(plot, sampling, species) %>%
  mutate(n_individuals = n()) %>%
  ungroup()

# Ecuación biomasa####                 
#Transforming diameters into circumferences
flora$cm <- round(ifelse(!is.na(flora$Dm), flora$Dm * pi, flora$Cm), 2)
flora$cb <- round(ifelse(!is.na(flora$Db), flora$Db * pi, flora$Cb), 2)

flora$Ah <- ((flora$cm)^2)/4*pi
flora$Ab <- ((flora$cb)^2)/4*pi

#Application of equation proposed by paper Perrone R. et al. 2020

d <- 1.96
z <- 2/3
flora$x <- (flora$height/2)*(flora$Ab + flora$Ah)
flora$biomass <- d*(flora$x^z)

### OUTLIERS ##### (check "Dynamics and outliers.Rmd" for more info)

flora1 <- flora[which(flora$x < (as.numeric(quantile(flora$x, na.rm = TRUE)[4] + (1.5 * IQR(flora$x, na.rm = TRUE))))),] 
flora3 <- flora[which(flora$x < (as.numeric(quantile(flora$x, na.rm = TRUE)[4] + (3 * IQR(flora$x, na.rm = TRUE))))),] 

#List of outliers: 
flora1_outl <- flora[which(flora$x > (as.numeric(quantile(flora$x, na.rm = TRUE)[4] + (1.5 * IQR(flora$x, na.rm = TRUE))))),] 
flora3_outl <- flora[which(flora$x > (as.numeric(quantile(flora$x, na.rm = TRUE)[4] + (3 * IQR(flora$x, na.rm = TRUE))))),] 


############### Transformation database ############

## Se trabaja con "flora" pero podríamos trabajar con "flora1". El problema de "flora1" es que, al eliminar los NA de biomasa,
# no tenemos datos para ninguna variable de los muestreos 0, 1 y 2. 

#Agrupar por sampling, plot y treatment primero. 
#Aquí se aplica, para biomasa, el siguiente criterio: se estima la biomasa de cada especie multiplicando su abundancia por 
#la masa promedio de los individuos medidos para dicha especie. 

flora <- flora %>%
  group_by(sampling, datenew, month, treatment, plot, abundance, species) %>%
  reframe(biomass = mean(biomass, na.rm = TRUE) * abundance) %>%
  distinct(sampling, datenew, month, plot, treatment, abundance, species, biomass)
#Mirar si la distribución de los datos de biomasa tienen sentido una vez aplicada la estimacion en base a la abundancia
hist(flora1$biomass)
hist(log(flora1$biomass))

#Transformo en NA los valores 0 de biomasa, que corresponden a los datos de los samplings 0, 1, 2 y los 26 datos del 3. 
#Esto es para el gráfico, pero hay que ver las correlaciones entre H, cb, cm...
#! Esdte paso no es necesario ahora con flora1, ya que al generar la df hicimos un na.rm = T
#flora1$biomass <- ifelse(flora1$biomass == 0, NA, flora1$biomass)

# Bases de datos nuevas ####
#Añado el numero de especies por sampling y plot
flora <- flora %>%
  group_by(plot, sampling) %>%
  mutate(n_species = if_else(is.na(species), NA, n())) %>%
  ungroup()

flora_samplings <-  flora %>%
  group_by(sampling, datenew, month, treatment, plot) %>%
  reframe(biomass = sum(biomass, na.rm = T), 
          n_species = n_species, 
          abundance = sum(abundance, na.rm = T)) %>%
  distinct(sampling, datenew, month, plot, treatment, biomass, n_species, abundance)

hist(flora_samplings$n_species)
hist(flora_samplings$abundance)
hist(flora_samplings$biomass)

#Adding zipf data about RADs
zipf_df <- read.csv("data/zipf_df.csv")
zipf_df <- select(zipf_df, -X)
zipf_df$plot <- as.factor(zipf_df$plot)
zipf_df$sampling <- as.factor(zipf_df$sampling)
zipf_df$treatment <- as.factor(zipf_df$treatment)

flora_samplings <- right_join(zipf_df, flora_samplings)

#Hacer comprobaciones de los datos en esta base de datos


flora_treatments <-  flora_samplings %>%
  group_by(treatment) %>%
  reframe(biomass = mean(biomass, na.rm = T), 
          n_species = mean(n_species, na.rm = T), 
          abundance = mean(abundance, na.rm = T)) %>%
  distinct(treatment, biomass, n_species, abundance)                                  

# Gráficos por muestreo y tratamiento####

# Other way with facet_grid

ggDynamics <- 
ggarrange(
  
ggplot(flora_samplings, aes(x = sampling, y = n_species, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Richness") +
  facet_grid(~ treatment)  + 
  scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  theme(legend.position = "NULL"),

ggplot(flora_samplings, aes(x = sampling, y = abundance, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Abundance") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  theme(legend.position = "NULL"),

ggplot(flora_samplings, aes(x = sampling, y = Y_zipf, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Y_zipf (RAD") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  theme(legend.position = "NULL"),


nrow=3, ncol=1)


## Comprobación rápida. Cómo difieren entre sí los muestreos 0 de cada variable en cada muestreo?

ggS0 <- 
ggarrange(
ggplot(subset(flora_samplings, flora_samplings$sampling == "0"), aes(x = treatment, y = abundance, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Abundance at sampling 0")+
  scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple"))+
  theme(legend.position = "NULL"),

ggplot(subset(flora_samplings, flora_samplings$sampling == "0"), aes(x = treatment, y = n_species, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Richness at sampling 0")+
  scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple"))+
  theme(legend.position = "NULL"),

nrow = 1, ncol = 2)

par(mfrow = c(1,2))
hist(subset(flora_samplings, flora_samplings$sampling == "0")$n_species)
hist(subset(flora_samplings, flora_samplings$sampling == "0")$abundance)
par(mfrow = c(1,1))

##### RESPONSE RATIO  Log(RR); RR = variable at point i / variable at point reference #######################


# Reference point: SAMPLING 0 #

fs_control <- subset(flora_samplings, treatment == "c")
fs_warming <- subset(flora_samplings, treatment == "w")
fs_pert<- subset(flora_samplings, treatment == "p")
fs_wp <- subset(flora_samplings, treatment == "wp")

#loop that iterates over every dataframe of treatments
list<- list(fs_control, fs_warming, fs_pert, fs_wp)
for (i in seq_along(list)){
  list[[i]]$RR_ref_richness <- rep(list[[i]]$n_species[which(list[[i]]$sampling == "0")], (nrow(list[[i]])/4))
  list[[i]]$RR_richness <- round(log(list[[i]]$n_species/list[[i]]$RR_ref_richness), 2)
  list[[i]]$RR_ref_abundance <- rep(list[[i]]$abundance[which(list[[i]]$sampling == "0")], (nrow(list[[i]])/4))
  list[[i]]$RR_abundance <- round(log(list[[i]]$abundance/list[[i]]$RR_ref_abundance), 2)
  list[[i]]$RR_ref_Yzipf <- rep(list[[i]]$Y_zipf[which(list[[i]]$sampling == "0")], (nrow(list[[i]])/4))
  list[[i]]$RR_Yzipf <- round(log(list[[i]]$Y_zipf/list[[i]]$RR_ref_Yzipf), 2)
}

RR_flora_samplings <- rbind(list[[1]], list[[2]], list[[3]], list[[4]])

ggRRsampling0 <- 
ggarrange(
ggplot(RR_flora_samplings, aes(x = sampling, y = RR_richness, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "RR_richness") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  theme(legend.position = "NULL"),

ggplot(RR_flora_samplings, aes(x = sampling, y = RR_abundance, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "RR_abundance") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  theme(legend.position = "NULL"), 

ncol = 1, nrow = 2)

# RR REFERENCE : CONTROL.#####

RR_treatments <- flora_samplings
samps <- unique(RR_treatments$sampling)

# Esto funciona, pero no sé introducirlo al loop
#RR_treatments$RR_ref_ab[which(RR_treatments$sampling == "0")] <- 
                      #(subset(RR_treatments, sampling == "0" & treatment == "c" ))$abundance

# TREATMENTS VS CONTROL: Reference = control (RR_abundance_C and RR_richness_C)
RR_treatments$RR_ref_ab_C <- NA
for (i in samps) {
  subset_data <- subset(RR_treatments, sampling == i & treatment == "c")
  RR_treatments$RR_ref_ab_C[RR_treatments$sampling == i] <-
    rep(subset_data$abundance, length(which(RR_treatments$sampling == i)))
}

RR_treatments$RR_abundance_C <- round(log(RR_treatments$abundance/RR_treatments$RR_ref_ab_C), 2)

RR_treatments$RR_ref_richness_C <- NA
for (i in samps) {
  subset_data <- subset(RR_treatments, sampling == i & treatment == "c")
  RR_treatments$RR_ref_richness_C[RR_treatments$sampling == i] <-
    rep(subset_data$n_species, length(which(RR_treatments$sampling == i)))
}

RR_treatments$RR_richness_C <- round(log(RR_treatments$n_species/RR_treatments$RR_ref_richness_C), 2)

# Representación gráfica 

RR_treatments_noC <- RR_treatments[RR_treatments$treatment != "c", ]

ggRRcontrol <- 
  ggarrange(
    ggplot(RR_treatments_noC, aes(x = sampling, y = RR_abundance_C, fill = treatment)) +
      geom_boxplot() +
      labs(x = " ", y = "logRR(abundance)") +
      facet_grid(~ treatment) + 
      scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
      theme(legend.position = "NULL"),
    
    ggplot(RR_treatments_noC, aes(x = sampling, y = RR_richness_C, fill = treatment)) +
      geom_boxplot() +
      labs(x = " ", y = "logRR(richness)") +
      facet_grid(~ treatment) + 
      scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
      theme(legend.position = "NULL"),
    
    nrow = 2, ncol = 1)


# RR Treatments interactions #########

# TREATMENTS VS WARMING: Reference = warming (RR_abundance_W and RR_richness_W)
RR_treatments$RR_ref_ab_W <- NA
samps <- unique(RR_treatments$sampling)
for (i in samps) {
  subset_data <- subset(RR_treatments, sampling == i & treatment == "w")
  RR_treatments$RR_ref_ab_W[RR_treatments$sampling == i] <-
    rep(subset_data$abundance, length(which(RR_treatments$sampling == i)))
}

RR_treatments$RR_abundance_W <- round(log(RR_treatments$abundance/RR_treatments$RR_ref_ab_W), 2)

RR_treatments$RR_ref_richness_W <- NA
for (i in samps) {
  subset_data <- subset(RR_treatments, sampling == i & treatment == "w")
  RR_treatments$RR_ref_richness_W[RR_treatments$sampling == i] <-
    rep(subset_data$n_species, length(which(RR_treatments$sampling == i)))
}

RR_treatments$RR_richness_W <- round(log(RR_treatments$n_species/RR_treatments$RR_ref_richness_W), 2)


# TREATMENTS VS Perturbation: Reference = perturbation (RR_abundance_P and RR_richness_P)
RR_treatments$RR_ref_ab_P <- NA
for (i in samps) {
  subset_data <- subset(RR_treatments, sampling == i & treatment == "p")
  RR_treatments$RR_ref_ab_P[RR_treatments$sampling == i] <-
    rep(subset_data$abundance, length(which(RR_treatments$sampling == i)))
}
RR_treatments$RR_abundance_P <- round(log(RR_treatments$abundance/RR_treatments$RR_ref_ab_P), 2)

RR_treatments$RR_ref_richness_P <- NA
for (i in samps) {
  subset_data <- subset(RR_treatments, sampling == i & treatment == "p")
  RR_treatments$RR_ref_richness_P[RR_treatments$sampling == i] <-
    rep(subset_data$n_species, length(which(RR_treatments$sampling == i)))
}

RR_treatments$RR_richness_P <- round(log(RR_treatments$n_species/RR_treatments$RR_ref_richness_P), 2)


# TREATMENTS VS warming+perturbation: Reference = wp (RR_abundance_WP and RR_richness_WP)
RR_treatments$RR_ref_ab_WP <- NA
for (i in samps) {
  subset_data <- subset(RR_treatments, sampling == i & treatment == "wp")
  RR_treatments$RR_ref_ab_WP[RR_treatments$sampling == i] <-
    rep(subset_data$abundance, length(which(RR_treatments$sampling == i)))
}
RR_treatments$RR_abundance_WP <- round(log(RR_treatments$abundance/RR_treatments$RR_ref_ab_WP), 2)

RR_treatments$RR_ref_richness_WP <- NA
for (i in samps) {
  subset_data <- subset(RR_treatments, sampling == i & treatment == "wp")
  RR_treatments$RR_ref_richness_WP[RR_treatments$sampling == i] <-
    rep(subset_data$n_species, length(which(RR_treatments$sampling == i)))
}

RR_treatments$RR_richness_WP <- round(log(RR_treatments$n_species/RR_treatments$RR_ref_richness_WP), 2)




# COmo hacer para poner como referencia WP. Hay que transformar base de datos para tener como levels de un factor a 
# RR_abundance_W y RR_abundance_P
# Why still legend??

RR_wp <- select(RR_treatments, -all_of(grep("RR_ref", names(RR_treatments), value = TRUE)), -biomass)

##ERROR

RR_wp_ab <- pivot_longer(RR_wp, cols = c("RR_abundance_W", "RR_abundance_P"), 
                         names_to = "RR_ab_treatment", values_to = "RR_ab_values")
RR_wp_ab <- select(RR_wp_ab, -all_of(grep("RR_richness", names(RR_wp_ab), value = TRUE)))

RR_wp_rich <- pivot_longer(RR_wp, cols = c("RR_richness_W", "RR_richness_P"), 
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
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  theme(legend.position = "NULL"),

ggplot(subset(RR_wp_rich, treatment == "wp"), aes(x = sampling, y = RR_rich_values, fill = RR_rich_treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "logRR(richness)") +
  facet_grid(~ RR_rich_treatment) + 
  scale_fill_manual(values = c("RR_richness_W" = "pink", "RR_richness_P" = "seagreen2"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  theme(legend.position = "NULL"),

nrow = 2, ncol = 1)



# COEFFICIENT OF VARIATION: CV = Standard deviation(x) / mean(x) ##########

flora_cv <- summarise(group_by(flora_samplings, sampling, treatment),
                               mean_biomass = mean(biomass, na.rm = T),
                               sd_biomass = s.err.na(biomass),
                               mean_richness = mean(n_species, na.rm = T),
                               sd_richness = s.err.na(n_species),
                               mean_abundance = mean(abundance, na.rm = T), 
                               sd_abundance = s.err.na(abundance))

flora_cv$CV_biomass <- round(flora_cv$mean_biomass/flora_cv$sd_biomass, 2) 
flora_cv$CV_richness <- round(flora_cv$mean_richness/flora_cv$sd_richness, 2)
flora_cv$CV_abundance <- round(flora_cv$mean_abundance/flora_cv$sd_abundance, 2)

ggCVsameplot <- 
ggarrange(
ggplot(flora_cv, aes(x = sampling, y = CV_richness, color = treatment, group = treatment)) +
  geom_point() +
  geom_line() +
  labs(x = " ", y = " ") + 
  scale_color_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red4", "wp" = "purple2"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  theme(legend.position = "right"),

ggplot(flora_cv, aes(x = sampling, y = CV_abundance, color = treatment, group = treatment)) +
  geom_point() +
  geom_line() +
  labs(x = " ", y = " ") + 
  scale_color_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red4", "wp" = "purple2"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  theme(legend.position = "right"),
labels = c("Richness", "Abundance"),
nrow = 1, ncol = 2)

ggCVgrid <- 
  ggarrange(
    
    ggplot(flora_cv, aes(x = sampling, y = CV_abundance, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      facet_grid(~ treatment)+
      labs(x = " ", y = "CV abundance") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      theme(legend.position = "NULL"),
    
    ggplot(flora_cv, aes(x = sampling, y = CV_richness, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      facet_grid(~ treatment)+
      labs(x = " ", y = "CV richness ") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      theme(legend.position = "NULL"),
    
    nrow = 2, ncol = 1)
 
  
# LogRR (CV) Reference = control 

flora_cv$RR_ref_ab_C <- NA
for (i in samps) {
  subset_data <- subset(flora_cv, sampling == i & treatment == "c")
  flora_cv$RR_ref_ab_C[flora_cv$sampling == i] <-
    rep(subset_data$CV_abundance, length(which(flora_cv$sampling == i)))
}

flora_cv$RR_abundance_C <- round(log(flora_cv$CV_abundance/flora_cv$RR_ref_ab_C), 2)

flora_cv$RR_ref_richness_C <- NA
for (i in samps) {
  subset_data <- subset(flora_cv, sampling == i & treatment == "c")
  flora_cv$RR_ref_richness_C[flora_cv$sampling == i] <-
    rep(subset_data$CV_richness, length(which(flora_cv$sampling == i)))
}

flora_cv$RR_richness_C <- round(log(flora_cv$CV_richness/flora_cv$RR_ref_richness_C), 2)

# LogRR (CV) Reference = warming 
flora_cv$RR_ref_ab_W <- NA
samps <- unique(flora_cv$sampling)
for (i in samps) {
  subset_data <- subset(flora_cv, sampling == i & treatment == "w")
  flora_cv$RR_ref_ab_W[flora_cv$sampling == i] <-
    rep(subset_data$CV_abundance, length(which(flora_cv$sampling == i)))
}

flora_cv$RR_abundance_W <- round(log(flora_cv$CV_abundance/flora_cv$RR_ref_ab_W), 2)

flora_cv$RR_ref_CV_richness_W <- NA
for (i in samps) {
  subset_data <- subset(flora_cv, sampling == i & treatment == "w")
  flora_cv$RR_ref_CV_richness_W[flora_cv$sampling == i] <-
    rep(subset_data$CV_richness, length(which(flora_cv$sampling == i)))
}

flora_cv$RR_richness_W <- round(log(flora_cv$CV_richness/flora_cv$RR_ref_CV_richness_W), 2)


# LogRR (CV) Reference = perturbation 
flora_cv$RR_ref_ab_P <- NA
for (i in samps) {
  subset_data <- subset(flora_cv, sampling == i & treatment == "p")
  flora_cv$RR_ref_ab_P[flora_cv$sampling == i] <-
    rep(subset_data$CV_abundance, length(which(flora_cv$sampling == i)))
}
flora_cv$RR_abundance_P <- round(log(flora_cv$CV_abundance/flora_cv$RR_ref_ab_P), 2)

flora_cv$RR_ref_CV_richness_P <- NA
for (i in samps) {
  subset_data <- subset(flora_cv, sampling == i & treatment == "p")
  flora_cv$RR_ref_CV_richness_P[flora_cv$sampling == i] <-
    rep(subset_data$CV_richness, length(which(flora_cv$sampling == i)))
}

flora_cv$RR_richness_P <- round(log(flora_cv$CV_richness/flora_cv$RR_ref_CV_richness_P), 2)


# LogRR (CV) Reference = warming + perturbation
flora_cv$RR_ref_ab_WP <- NA
for (i in samps) {
  subset_data <- subset(flora_cv, sampling == i & treatment == "wp")
  flora_cv$RR_ref_ab_WP[flora_cv$sampling == i] <-
    rep(subset_data$CV_abundance, length(which(flora_cv$sampling == i)))
}
flora_cv$RR_abundance_WP <- round(log(flora_cv$CV_abundance/flora_cv$RR_ref_ab_WP), 2)

flora_cv$RR_ref_CV_richness_WP <- NA
for (i in samps) {
  subset_data <- subset(flora_cv, sampling == i & treatment == "wp")
  flora_cv$RR_ref_CV_richness_WP[flora_cv$sampling == i] <-
    rep(subset_data$CV_richness, length(which(flora_cv$sampling == i)))
}

flora_cv$RR_richness_WP <- round(log(flora_cv$CV_richness/flora_cv$RR_ref_CV_richness_WP), 2)

#I delete treatment c 

flora_cv_noC <- flora_cv[flora_cv$treatment != "c", ]

#CV plots


#Este es muy dificil de leer en verdad
ggRRcv_try <- 
ggplot(flora_cv_noC , aes(x = sampling, y = RR_richness_C, color = treatment, group = treatment)) +
  geom_point() +
  geom_line() +
  labs(x = " ", y = "CV Richness") + 
  scale_color_manual(values = c("c" = "green4", "p" = "blue3", "w" = "red4", "wp" = "purple2"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  theme(legend.position = "NULL")

ggRRcv_control <- 
ggarrange(
  ggplot(flora_cv_noC, aes(x = sampling, y = RR_abundance_C, color = treatment, group = treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(~ treatment)+
    labs(x = " ", y = "logRR(CV abundance)") + 
    scale_color_manual(values = c("c" = "green4", "p" = "blue3", "w" = "red4", "wp" = "purple2"))+
    geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
    theme(legend.position = "NULL"),
  
ggplot(flora_cv_noC, aes(x = sampling, y = RR_richness_C, color = treatment, group = treatment)) +
  geom_point() +
  geom_line() +
  facet_grid(~ treatment)+
  labs(x = " ", y = "logRR(CV richness)") + 
  scale_color_manual(values = c("c" = "green4", "p" = "blue3", "w" = "red4", "wp" = "purple2"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  theme(legend.position = "NULL"),

nrow=2, ncol=1)
  
  

# LogRR (CV) Reference = w and p. Comparison with wp

RR_cv_wp <- select(flora_cv_noC, -all_of(grep("RR_ref", names(flora_cv_noC), value = TRUE)))

RR_cv_wp_ab <- pivot_longer(RR_cv_wp, cols = c("RR_abundance_W", "RR_abundance_P"), 
                            names_to = "RR_ab_treatment", values_to = "RR_ab_values")
RR_cv_wp_ab <- select(RR_cv_wp_ab, -all_of(grep("RR_richness", names(RR_cv_wp_ab), value = TRUE)))

RR_cv_wp_rich <- pivot_longer(RR_cv_wp, cols = c("RR_richness_W", "RR_richness_P"), 
                              names_to = "RR_rich_treatment", values_to = "RR_rich_values")
RR_cv_wp_rich <- select(RR_cv_wp_rich, -all_of(grep("RR_abundance", names(RR_cv_wp_rich), value = TRUE)))

ggRRcv_wp <- 
ggarrange(
  ggplot(subset(RR_cv_wp_ab, treatment == "wp"), aes(x = sampling, y = RR_ab_values, color = RR_ab_treatment, group = RR_ab_treatment)) +
    geom_point() +
    geom_line() + 
    labs(x = " ", y = "logRR(CV abundance)") +
    facet_grid(~ RR_ab_treatment) + 
    scale_color_manual(values = c("RR_abundance_W" = "pink4", "RR_abundance_P" = "seagreen4"))+
    geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
    theme(legend.position = "NULL"),
  
  ggplot(subset(RR_cv_wp_rich, treatment == "wp"), aes(x = sampling, y = RR_rich_values, color = RR_rich_treatment, group = RR_rich_treatment)) +
    geom_point() +
    geom_line() +
    labs(x = " ", y = "logRR(CV richness)") +
    facet_grid(~ RR_rich_treatment) + 
    scale_color_manual(values = c("RR_richness_W" = "pink4", "RR_richness_P" = "seagreen4"))+
    geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
    theme(legend.position = "NULL"),
  
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





