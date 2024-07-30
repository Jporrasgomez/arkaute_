


rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
pacman::p_load(dplyr,ggplot2,tidyverse)

source("code/first_script.R")
datos <- flora

datos$plot <- as.factor(datos$plot)
datos$sampling <- as.factor(datos$sampling)
datos$code <- as.factor(datos$code)

datos <- datos %>%
  filter(!sampling %in% c("0", "1", "2", "12"))           ## * He quitado el sampling 12 porque no tenemos datos de biomasa


# Configuración del estílo de los gráficos
theme_set(theme_bw()+ theme(legend.position = "NULL"))



RRatio <-  datos %>%
  group_by(sampling, date, month, treatment, plot) %>%
  reframe(biomass = sum(biomass, na.rm = T), #total abundance per plot (m2) (replicate of treatment)
          n_species = n_species,  #total number of species per plot
          abundance = sum(abundance, na.rm = T)) %>% # total coverage of plot
  distinct(sampling, date, month, plot, treatment, biomass, n_species, abundance)


samps <- unique(RRatio$sampling)
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








