


rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

source("code/first_script.R")


# Configuración del estílo de los gráficos
theme_set(theme_bw()+ theme(legend.position = "NULL"))


# Bases de datos con las que vamos a trabajar
flora_biomass <- flora %>%
  filter(!sampling %in% c("0", "1", "2", "12"))          

flora_morph <- flora_complete

###### Agregando por réplicas ####

### Biomasa

RRatio <-  flora_biomass %>%
  group_by(sampling, date, month, treatment, plot) %>%
  reframe(biomass =  biomass_total, #total biomass per plot (m2) including the number of individual estimation
          n_species = n_species,  #total number of species per plot
          abundance = abundance_total) %>% # total coverage of plot
  distinct(sampling, date, month, plot, treatment, biomass, n_species, abundance)


samps <- unique(flora$sampling)
RRatio$c_meanbiomass <- NA
for (i in 1:length(samps)) {
  subset_c <- subset(RRatio, sampling == samps[i] & treatment == "c")
  
  RRatio$c_meanbiomass[RRatio$sampling == samps[i]] <- mean(subset_c$biomass)
}


RRatio <- RRatio %>%
  filter(!treatment %in% "c")

RRatio$logRR_biomass <- round(log(RRatio$biomass / RRatio$c_meanbiomass), 2)


ggplot(RRatio, aes(x = sampling, y = logRR_biomass, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "logRR(biomass)") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) 

### Valores morfológicos de la ecuación y biomasa en crudo (sin aplicar el análisis de los individuos)

RRatio_morph <-  flora_morph %>%
  group_by(sampling, date, month, treatment, plot) %>%
  reframe(height_mean = mean(height, na.rm = T),
          Ah_mean = mean(Ah, na.rm = T), 
          Ab_mean = mean(Ab, na.rm = T),
          n_species = n_species,  #total number of species per plot
          abundance = sum(abundance, na.rm = T),
          biomass_i = sum(biomass_i, na.rm =T)) %>% # total coverage of plot
  distinct(sampling, date, month, plot, treatment, height_mean, Ah_mean, Ab_mean, n_species, biomass_i, abundance)



samps <- unique(RRatio_morph$sampling)
RRatio_morph$c_meanheight <- NA
for (i in 1:length(samps)) {
  subset_c <- subset(RRatio_morph, sampling == samps[i] & treatment == "c")
  
  RRatio_morph$c_meanheight[RRatio_morph$sampling == samps[i]] <- mean(subset_c$height_mean)
  RRatio_morph$c_meanAh[RRatio_morph$sampling == samps[i]] <- mean(subset_c$Ah_mean)
  RRatio_morph$c_meanAb[RRatio_morph$sampling == samps[i]] <- mean(subset_c$Ab_mean)
  RRatio_morph$c_meanbiomass[RRatio_morph$sampling == samps[i]] <- mean(subset_c$biomass_i)
}


RRatio_morph <- RRatio_morph %>%
  filter(!treatment %in% "c")

RRatio_morph$logRR_height <- round(log(RRatio_morph$height_mean/ RRatio_morph$c_meanheight), 2)
RRatio_morph$logRR_Ah <- round(log(RRatio_morph$Ah_mean/ RRatio_morph$c_meanAh), 2)
RRatio_morph$logRR_Ab <- round(log(RRatio_morph$Ab_mean/ RRatio_morph$c_meanAb), 2)
RRatio_morph$logRR_biomass_i <- round(log(RRatio_morph$biomass_i/ RRatio_morph$c_meanbiomass), 2)


# Biomasa en crudo

ggplot(RRatio_morph, aes(x = sampling, y = logRR_biomass_i, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "logRR(biomass_i)") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8)


# Valores morfológicos

ggarrange(
ggplot(RRatio_morph, aes(x = sampling, y = logRR_height, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "logRR(height)") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 

ggplot(RRatio_morph, aes(x = sampling, y = logRR_Ah, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "logRR(Ah)") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 

ggplot(RRatio_morph, aes(x = sampling, y = logRR_Ab, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "logRR(Ab)") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 

ncol = 1, nrow = 3)



######### SIN AGREGAR POR RÉPLICAS #############

# Biomasa

RRatio_nonaggr <- flora_biomass

samps <- unique(flora$sampling)
RRatio_nonaggr$c_meanbiomass <- NA
for (i in 1:length(samps)) {
  subset_c <- subset(RRatio_nonaggr, sampling == samps[i] & treatment == "c")
  
  RRatio_nonaggr$c_meanbiomass[RRatio_nonaggr$sampling == samps[i]] <- mean(subset_c$biomass)
}


RRatio_nonaggr <- RRatio_nonaggr %>%
  filter(!treatment %in% "c")

RRatio_nonaggr$logRR_biomass <- round(log(RRatio_nonaggr$biomass / RRatio_nonaggr$c_meanbiomass), 2)


ggplot(RRatio_nonaggr, aes(x = sampling, y = logRR_biomass, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "logRR(biomass) non aggregated replicates") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) 


# Valores morfológicos de la ecuación de biomasa y biomasa en crudo 

RRatio_morph_nonaggr <- flora_morph


samps <- unique(RRatio_morph_nonaggr$sampling)

for (i in 1:length(samps)) {
  subset_c <- subset(RRatio_morph_nonaggr, sampling == samps[i] & treatment == "c")
  
  RRatio_morph_nonaggr$c_meanheight[RRatio_morph_nonaggr$sampling == samps[i]] <- mean(subset_c$height)
  RRatio_morph_nonaggr$c_meanAh[RRatio_morph_nonaggr$sampling == samps[i]] <- mean(subset_c$Ah)
  RRatio_morph_nonaggr$c_meanAb[RRatio_morph_nonaggr$sampling == samps[i]] <- mean(subset_c$Ab)
  RRatio_morph_nonaggr$c_meanbiomass[RRatio_morph_nonaggr$sampling == samps[i]] <- mean(subset_c$biomass_i)
}


RRatio_morph_nonaggr <- RRatio_morph_nonaggr %>%
  filter(!treatment %in% "c")

RRatio_morph_nonaggr$logRR_height <- round(log(RRatio_morph_nonaggr$height/ RRatio_morph_nonaggr$c_meanheight), 2)
RRatio_morph_nonaggr$logRR_Ah <- round(log(RRatio_morph_nonaggr$Ah/ RRatio_morph_nonaggr$c_meanAh), 2)
RRatio_morph_nonaggr$logRR_Ab <- round(log(RRatio_morph_nonaggr$Ab/ RRatio_morph_nonaggr$c_meanAb), 2)
RRatio_morph_nonaggr$logRR_biomass_i <- round(log(RRatio_morph_nonaggr$biomass_i/ RRatio_morph_nonaggr$c_meanbiomass), 2)


ggplot(RRatio_morph_nonaggr, aes(x = sampling, y = logRR_biomass_i, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "logRR(biomass_i) NonAgg") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8)

ggarrange(
  ggplot(RRatio_morph_nonaggr, aes(x = sampling, y = logRR_height, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "logRR(height) NonAgg") +
    facet_grid(~ treatment) + 
    scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 
  
  ggplot(RRatio_morph_nonaggr, aes(x = sampling, y = logRR_Ah, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "logRR(Ah) NonAgg") +
    facet_grid(~ treatment) + 
    scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 
  
  ggplot(RRatio_morph_nonaggr, aes(x = sampling, y = logRR_Ab, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "logRR(Ab) NonAgg") +
    facet_grid(~ treatment) + 
    scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 
  
  ncol = 1, nrow = 3)


