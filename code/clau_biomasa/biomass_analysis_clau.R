


rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)
pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr) #Cargamos los paquetes que necesitamos

source("code/first_script.R")



# Configuración del estílo de los gráficos
theme_set(theme_bw()+ theme(legend.position = "NULL"))


# Bases de datos con las que vamos a trabajar
flora_biomass <- flora %>%
  filter(!sampling %in% c("0", "1", "2", "12"))          

flora_morph <- flora_complete

### Dinámicas

biomass_dynamics <- flora_biomass
mean_sd_data_dynamics<- biomass_dynamics %>%
  group_by(treatment, sampling) %>%
  summarize(mean_biomass = mean(biomass_total),
            sd_biomass = sd(biomass_total))


treatment_labs_dynamics <- c("Control", "Warming", "Perturbation", "Warming and perturbation")
names(treatment_labs_dynamics) <- c("c","w", "p", "wp")



ggplot(mean_sd_data_dynamics, aes(x = as.factor(sampling), y = mean_biomass, group = sampling)) +
  facet_grid(~ treatment, labeller = labeller(treatment = treatment_labs_dynamics)) +
  geom_errorbar(aes(ymin = mean_biomass - sd_biomass, ymax = mean_biomass + sd_biomass,
                    color = treatment),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75) +
  geom_point(data = biomass_dynamics, aes(x = as.factor(sampling), y = biomass_total, color = treatment),
             position = position_dodge(width = 0.5), size = 2, alpha = 0.4) +
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  #scale_colour_manual(values = met.brewer("VanGogh2", 4)) +
  scale_colour_manual(values = met.brewer("VanGogh3", 4)) +
  labs(x = "Sampling time", y = "Species biomass (log response ratio)") +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 12))




### Log Response Ratio Biomasa

RRatio <-  flora_biomass %>%
  group_by(sampling, date, month, treatment, plot) %>%
  reframe(biomass =  biomass_total, #total biomass per plot (m2) including the number of individual estimation
          n_species = n_species,  #total number of species per plot
          abundance = abundance_total) %>% # total coverage of plot
  distinct(sampling, date, month, plot, treatment, biomass, n_species, abundance)


## Hacemos un loop para calcular el response ratio pero vamos a calcular la referencia de c, w y p. 

samps <- unique(flora$sampling)


for (i in 1:length(samps)) {
  subset_c <- subset(RRatio, sampling == samps[i] & treatment == "c")
  RRatio$ref_c_meanbiomass[RRatio$sampling == samps[i]] <- mean(subset_c$biomass)
  
  subset_w <- subset(RRatio, sampling == samps[i] & treatment == "w")
  RRatio$ref_w_meanbiomass[RRatio$sampling == samps[i]] <- mean(subset_w$biomass)
  
  subset_p <- subset(RRatio, sampling == samps[i] & treatment == "p")
  RRatio$ref_p_meanbiomass[RRatio$sampling == samps[i]] <- mean(subset_p$biomass)

}


RRatio_ref_c <- RRatio %>%
  filter(!treatment %in% "c")
RRatio_ref_c$logRR_biomass <- round(log(RRatio_ref_c$biomass / RRatio_ref_c$ref_c_meanbiomass), 2)
mean_sd_data_ref_c <- RRatio_ref_c %>%
  group_by(treatment, sampling) %>%
  summarize(mean_biomass = mean(logRR_biomass),
            sd_biomass = sd(logRR_biomass))
#mean_sd_data_ref_C$mean_biomass[which(mean_sd_data$mean_biomass == -Inf)] <- NA
#mean_sd_data_ref_C$sd_biomass[which(is.nan(mean_sd_data$sd_biomass))] <- NA
# Changes in the data to plot
#RRatio_ref_c$logRR_biomass[which(RRatio_ref_c$logRR_biomass == -Inf)] <- NA
#RRatio_ref_c$treatment <- factor(RRatio_ref_c$treatment, levels = c("w", "p", "wp"))

RRatio_ref_w <- RRatio %>%
  filter(!treatment %in% c( "w", "c", "p"))
RRatio_ref_w$logRR_biomass <- round(log(RRatio_ref_w$biomass / RRatio_ref_w$ref_w_meanbiomass), 2)
mean_sd_data_ref_w <- RRatio_ref_w %>%
  group_by(treatment, sampling) %>%
  summarize(mean_biomass = mean(logRR_biomass),
            sd_biomass = sd(logRR_biomass))

RRatio_ref_p <- RRatio %>%
  filter(!treatment %in% c("p", "c", "w"))
RRatio_ref_p$logRR_biomass <- round(log(RRatio_ref_p$biomass / RRatio_ref_p$ref_p_meanbiomass), 2)
mean_sd_data_ref_p <- RRatio_ref_p %>%
  group_by(treatment, sampling) %>%
  summarize(mean_biomass = mean(logRR_biomass),
            sd_biomass = sd(logRR_biomass))


#Graficos con script de Rodrigo
library(MetBrewer)
#LogRRatio donde el control es la referencia de los 3 tratamientos
treatment.labs <- c("Warming", "Perturbation", "Warming and perturbation")
names(treatment.labs) <- c("w", "p", "wp")



##Ggplot logRR con REFRENCIA = CONTROL


ggplot(mean_sd_data_ref_c, aes(x = as.factor(sampling), y = mean_biomass, group = sampling)) +
  facet_grid(~ treatment, labeller = labeller(treatment = treatment.labs)) +
  geom_errorbar(aes(ymin = mean_biomass - sd_biomass, ymax = mean_biomass + sd_biomass,
                    color = treatment),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75) +
  geom_point(data = RRatio_ref_c, aes(x = as.factor(sampling), y = logRR_biomass, color = treatment),
             position = position_dodge(width = 0.5), size = 2, alpha = 0.4) +
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.7) +
  scale_colour_manual(values = met.brewer("VanGogh2", 3)) +
  labs(x = "Sampling time", y = "Species biomass (log response ratio)") +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 12))


##Ggplot logRR(wp/w)

ggplot(mean_sd_data_ref_w, aes(x = as.factor(sampling), y = mean_biomass, group = sampling)) +
  facet_grid(~ treatment, labeller = labeller(treatment = treatment.labs)) +
  geom_errorbar(aes(ymin = mean_biomass - sd_biomass, ymax = mean_biomass + sd_biomass,
                    color = treatment),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75) +
  geom_point(data = RRatio_ref_w, aes(x = as.factor(sampling), y = logRR_biomass, color = treatment),
             position = position_dodge(width = 0.5), size = 2, alpha = 0.4) +
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.7) +
  scale_colour_manual(values = met.brewer("VanGogh2", 3)) +
  labs(x = "Sampling time", y = "Species biomass (log response ratio (wp/w))") +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 15))

##Ggplot logRR(wp/p)

ggplot(mean_sd_data_ref_p, aes(x = as.factor(sampling), y = mean_biomass, group = sampling)) +
  facet_grid(~ treatment, labeller = labeller(treatment = treatment.labs)) +
  geom_errorbar(aes(ymin = mean_biomass - sd_biomass, ymax = mean_biomass + sd_biomass,
                    color = treatment),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75) +
  geom_point(data = RRatio_ref_p, aes(x = as.factor(sampling), y = logRR_biomass, color = treatment),
             position = position_dodge(width = 0.5), size = 2, alpha = 0.4) +
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.7) +
  scale_colour_manual(values = met.brewer("VanGogh2", 3)) +
  labs(x = "Sampling time", y = "Species biomass (log response ratio (wp/p))") +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 15))



## ANtiguos gráficos
#LogRRatio donde el control es la referencia de los 3 tratamientos
ggplot(RRatio_ref_c, aes(x = sampling, y = logRR_biomass, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Biomass: logRR(treatment/c)") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) 

#LogRRatio donde w es la referencia de wp. ¿Cómo afecta el tratamiento wp respecto a w?)  
ggplot(RRatio_ref_w, aes(x = sampling, y = logRR_biomass, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Biomass: logRR(wp/w)") +
  scale_fill_manual(values = c("wp" = "indianred3")) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) 

#LogRRatio donde p es la referencia de wp. ¿Cómo afecta el tratamiento wp respecto a p?) 
ggplot(RRatio_ref_p, aes(x = sampling, y = logRR_biomass, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Biomass: logRR(wp/p)") +
  scale_fill_manual(values = c("wp" = "maroon4")) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) 




### Valores morfológicos de la ecuación 

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

for (i in 1:length(samps)) {
  subset_c <- subset(RRatio_morph, sampling == samps[i] & treatment == "c")
  subset_w <- subset(RRatio_morph, sampling == samps[i] & treatment == "w")
  subset_p <- subset(RRatio_morph, sampling == samps[i] & treatment == "p")
  
  
  RRatio_morph$c_meanheight[RRatio_morph$sampling == samps[i]] <- mean(subset_c$height_mean)
  RRatio_morph$c_meanAh[RRatio_morph$sampling == samps[i]] <- mean(subset_c$Ah_mean)
  RRatio_morph$c_meanAb[RRatio_morph$sampling == samps[i]] <- mean(subset_c$Ab_mean)
  
  RRatio_morph$w_meanheight[RRatio_morph$sampling == samps[i]] <- mean(subset_w$height_mean)
  RRatio_morph$w_meanAh[RRatio_morph$sampling == samps[i]] <- mean(subset_w$Ah_mean)
  RRatio_morph$w_meanAb[RRatio_morph$sampling == samps[i]] <- mean(subset_w$Ab_mean)
  
  RRatio_morph$p_meanheight[RRatio_morph$sampling == samps[i]] <- mean(subset_p$height_mean)
  RRatio_morph$p_meanAh[RRatio_morph$sampling == samps[i]] <- mean(subset_p$Ah_mean)
  RRatio_morph$p_meanAb[RRatio_morph$sampling == samps[i]] <- mean(subset_p$Ab_mean)
  
}


RRatio_morph_ref_c <- RRatio_morph %>%
  filter(!treatment %in% "c")

RRatio_morph_ref_c$logRR_height <- round(log(RRatio_morph_ref_c$height_mean/ RRatio_morph_ref_c$c_meanheight), 2)
RRatio_morph_ref_c$logRR_Ah <- round(log(RRatio_morph_ref_c$Ah_mean/ RRatio_morph_ref_c$c_meanAh), 2)
RRatio_morph_ref_c$logRR_Ab <- round(log(RRatio_morph_ref_c$Ab_mean/ RRatio_morph_ref_c$c_meanAb), 2)

RRatio_morph_ref_w <- RRatio_morph %>%
  filter(!treatment %in% c( "w", "c", "p"))

RRatio_morph_ref_w$logRR_height <- round(log(RRatio_morph_ref_w$height_mean/ RRatio_morph_ref_w$w_meanheight), 2)
RRatio_morph_ref_w$logRR_Ah <- round(log(RRatio_morph_ref_w$Ah_mean/ RRatio_morph_ref_w$w_meanAh), 2)
RRatio_morph_ref_w$logRR_Ab <- round(log(RRatio_morph_ref_w$Ab_mean/ RRatio_morph_ref_w$w_meanAb), 2)

RRatio_morph_ref_p <- RRatio_morph %>%
  filter(!treatment %in% c( "w", "c", "p"))

RRatio_morph_ref_p$logRR_height <- round(log(RRatio_morph_ref_p$height_mean/ RRatio_morph_ref_p$p_meanheight), 2)
RRatio_morph_ref_p$logRR_Ah <- round(log(RRatio_morph_ref_p$Ah_mean/ RRatio_morph_ref_p$p_meanAh), 2)
RRatio_morph_ref_p$logRR_Ab <- round(log(RRatio_morph_ref_p$Ab_mean/ RRatio_morph_ref_p$p_meanAb), 2)



#LogRRatio donde se compara los 3 tratamientos con el control
ggarrange(
  ggplot(RRatio_morph_ref_c, aes(x = sampling, y = logRR_height, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "logRR(height)") +
    facet_grid(~ treatment) + 
    scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 
  
  ggplot(RRatio_morph_ref_c, aes(x = sampling, y = logRR_Ah, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "logRR(Ah)") +
    facet_grid(~ treatment) + 
    scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 
  
  ggplot(RRatio_morph_ref_c, aes(x = sampling, y = logRR_Ab, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "logRR(Ab)") +
    facet_grid(~ treatment) + 
    scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 
  
  ncol = 1, nrow = 3)

# LogRRatio donde se compara el tratamiento wp con w
ggarrange(
  ggplot(RRatio_morph_ref_w, aes(x = sampling, y = logRR_height, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "Height: logRR(wp/w)") +
    scale_fill_manual(values = c("wp" = "indianred3"))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 
  
  ggplot(RRatio_morph_ref_w, aes(x = sampling, y = logRR_Ah, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "Ah: logRR(wp/w)") +
    scale_fill_manual(values = c("wp" = "indianred3"))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 
  
  ggplot(RRatio_morph_ref_w, aes(x = sampling, y = logRR_Ab, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "Ab: logRR(wp/w)") +
    scale_fill_manual(values = c("wp" = "indianred3"))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 
  
  ncol = 1, nrow = 3)

#LogRRatio donde se compara el tratamiento wp con p
ggarrange(
  ggplot(RRatio_morph_ref_p, aes(x = sampling, y = logRR_height, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "Height: logRR(wp/p)") +
    scale_fill_manual(values = c("wp" = "maroon4"))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 
  
  ggplot(RRatio_morph_ref_p, aes(x = sampling, y = logRR_Ah, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "Ah: logRR(wp/p)") +
    scale_fill_manual(values = c("wp" = "maroon4"))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 
  
  ggplot(RRatio_morph_ref_p, aes(x = sampling, y = logRR_Ab, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "Ab: logRR(wp/p)") +
    scale_fill_manual(values = c("wp" = "maroon4"))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 
  
  ncol = 1, nrow = 3)





