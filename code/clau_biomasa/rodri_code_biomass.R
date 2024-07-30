





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


### Biomasa

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

RRatio_ref_w <- RRatio %>%
  filter(!treatment %in% c( "w", "c", "p"))
RRatio_ref_w$logRR_biomass <- round(log(RRatio_ref_w$biomass / RRatio_ref_w$ref_w_meanbiomass), 2)

RRatio_ref_p <- RRatio %>%
  filter(!treatment %in% c("p", "c", "w"))
RRatio_ref_p$logRR_biomass <- round(log(RRatio_ref_p$biomass / RRatio_ref_p$ref_p_meanbiomass), 2)



treatment.labs <- c("Warming", "Perturbation", "Warming and perturbation")
names(treatment.labs) <- c("w", "p", "wp")


# Changes in the data to plot
RRatio_ref_c$logRR_biomass[which(RRatio_ref_c$logRR_biomass == -Inf)] <- NA
RRatio_ref_c$treatment <- factor(RRatio_ref_c$treatment, levels = c("w", "p", "wp"))

# Calculate mean and standard deviation per sampling and treatment
mean_sd_data <- RRatio_ref_c %>%
  group_by(treatment, sampling) %>%
  summarize(mean_biomass = mean(logRR_biomass),
            sd_biomass = sd(logRR_biomass))
mean_sd_data$mean_biomass[which(mean_sd_data$mean_biomass == -Inf)] <- NA
mean_sd_data$sd_biomass[which(is.nan(mean_sd_data$sd_biomass))] <- NA


library(MetBrewer)
ggplot(mean_sd_data, aes(x = as.factor(sampling), y = mean_biomass, group = sampling)) +
  facet_grid(~ treatment, labeller = labeller(treatment = treatment.labs)) +
  
  geom_errorbar(aes(ymin = mean_biomass - sd_biomass, ymax = mean_biomass + sd_biomass,
                    color = treatment),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75) +
  
  geom_point(data = RRatio_ref_c, aes(x = as.factor(sampling), y = logRR_biomass, color = treatment),
             position = position_dodge(width = 0.5), size = 2, alpha = 0.4) +
  
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  
  geom_line() +
  
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.7) +
  
  scale_colour_manual(values = met.brewer("VanGogh2", 3)) +
  
  labs(x = "Sampling time", y = "Species biomass (log response ratio)") +
  
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 15))



#LogRRatio donde el control es la referencia de los 3 tratamientos
ggplot(RRatio_ref_c, aes(x = sampling, y = logRR_biomass, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Biomass: logRR(treatment/c)") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) 