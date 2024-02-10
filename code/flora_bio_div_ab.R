
# Cosas que hacer ####
#MIrar si hay correlaciones entre alturas y circunferencias 
#Hacer comprobaciones de los datos cada vez que se transformen las bases de datos
#Cambiar los nombres de "sampling X" por "sX_month"
#Buscar otra manera de representar las dinámicas sin boxplots. 
#Dividir la base de datos en Biomasa, Diversidad y Abundancia o algo así para que
#el tratamiento de los gaps en la base de biomasa no afecte a los demás



#Explorar a nivel de plot, muestreos y tratamientos la variabilidad de biomasa, abundancia y diversidad

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
  mutate(n_species = n()) %>%
  ungroup()

flora_samplings <-  flora %>%
  group_by(sampling, datenew, month, treatment, plot) %>%
  reframe(biomass = sum(biomass, na.rm = T), 
          n_species = n_species, 
          abundance = sum(abundance, na.rm = T)) %>%
  distinct(sampling, datenew, month, plot, treatment, biomass, n_species, abundance)
#Hacer comprobaciones de los datos en esta base de datos


flora_treatments <-  flora_samplings %>%
  group_by(treatment) %>%
  reframe(biomass = mean(biomass, na.rm = T), 
          n_species = mean(n_species, na.rm = T), 
          abundance = mean(abundance, na.rm = T)) %>%
  distinct(treatment, biomass, n_species, abundance)                                  

# Gráficos por muestreo y tratamiento####

# Other way with facet_grid

ggplot(flora_samplings, aes(x = sampling, y = n_species, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Richness") +
  facet_grid(~ treatment)  + 
  scale_fill_manual(values = c("c" = "green", "p" = "blue", "w" = "red", "wp" = "purple"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  theme(legend.position = "bottom", 
        axis.text = element_text(size = 12))

ggplot(flora_samplings, aes(x = sampling, y = biomass, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Biomass") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("c" = "green", "p" = "blue", "w" = "red", "wp" = "purple"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  theme(legend.position = "bottom", 
        axis.text = element_text(size = 12))

ggplot(flora_samplings, aes(x = sampling, y = abundance, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Abundance") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("c" = "green", "p" = "blue", "w" = "red", "wp" = "purple"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  theme(legend.position = "bottom", 
        axis.text = element_text(size = 12))


## RESPONSE RATIO ####

# To calculate a response ratio, we divide a datapoint by the reference datapoint. 

# Reference point: SAMPLING 0 ####

fs_control <- subset(flora_samplings, treatment == "c")
fs_warming <- subset(flora_samplings, treatment == "w")
fs_pert<- subset(flora_samplings, treatment == "p")
fs_wp <- subset(flora_samplings, treatment == "wp")

#loot that iterates over every dataframe of treatments
list<- list(fs_control, fs_warming, fs_pert, fs_wp)
for (i in seq_along(list)){
  list[[i]]$RR_ref_richness <- rep(list[[i]]$n_species[which(list[[i]]$sampling == "0")], (nrow(list[[i]])/4))
  list[[i]]$RR_richness <- round(log(list[[i]]$n_species/list[[i]]$RR_ref_richness), 2)
  list[[i]]$RR_ref_abundance <- rep(list[[i]]$abundance[which(list[[i]]$sampling == "0")], (nrow(list[[i]])/4))
  list[[i]]$RR_abundance <- round(log(list[[i]]$abundance/list[[i]]$RR_ref_abundance), 2)
}

RR_flora_samplings <- rbind(list[[1]], list[[2]], list[[3]], list[[4]])

ggarrange(
ggplot(RR_flora_samplings, aes(x = sampling, y = RR_richness, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "RR_richness") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("c" = "green", "p" = "blue", "w" = "red", "wp" = "purple"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  theme(legend.position = "bottom", 
        axis.text = element_text(size = 12)),

ggplot(RR_flora_samplings, aes(x = sampling, y = RR_abundance, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "RR_abundance") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("c" = "green", "p" = "blue", "w" = "red", "wp" = "purple"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  theme(legend.position = "bottom", 
        axis.text = element_text(size = 12)),

ncol = 1, nrow = 2)

#REFERENCE : CONTROL. HAY QUE PENSAR BIEN ESTO!!!


#Intento de hacerlo sin pivot.wider
RR_treatments <- flora_samplings
RR_treatments$RR_ref_ab <- NA

# Esto funciona, pero no sé introducirlo al loop
#RR_treatments$RR_ref_ab[which(RR_treatments$sampling == "0")] <- 
                      #(subset(RR_treatments, sampling == "0" & treatment == "c" ))$abundance

#chat gpt lo ha modificado a esto: 
samps <- unique(RR_treatments$sampling)
for (i in samps) {
  subset_data <- subset(RR_treatments, sampling == i & treatment == "c")
  RR_treatments$RR_ref_ab[RR_treatments$sampling == i] <-
    rep(subset_data$abundance, length(which(RR_treatments$sampling == i)))
}

RR_treatments$RR_abundance <- round(log(RR_treatments$abundance/RR_treatments$RR_ref_ab), 2)

RR_treatments$RR_ref_richness <- NA
for (i in samps) {
  subset_data <- subset(RR_treatments, sampling == i & treatment == "c")
  RR_treatments$RR_ref_richness[RR_treatments$sampling == i] <-
    rep(subset_data$n_species, length(which(RR_treatments$sampling == i)))
}

RR_treatments$RR_richness <- round(log(RR_treatments$n_species/RR_treatments$RR_ref_richness), 2)

#Cómo representarlo gráficamente?




# Hata ahora lo he hecho de eta manera, pero se puede hacer de otra

 fs_abundance <-  pivot_wider(flora_samplings, names_from = treatment, values_from = abundance)
 
 # Hacer lo siguiente en un loop :
 fs_abundance$RR_ref <- NA
 
 # ESTO FUNCIONA: 
 fs_abundance$RR_ref[which(fs_abundance$sampling == "0")] <-
   +     rep(na.omit(fs_abundance$c[which(fs_abundance$sampling =="0")])[1:4], 
             length(fs_abundance$sampling[which(fs_abundance$sampling == "0")])/4) # Se divide entre cuatro (número de filas del contenido de c)
 # LOOP!!!
 samps <- unique(fs_abundance$sampling)
 for (i in seq_along(samps)){
   fs_abundance$RR_ref[which(fs_abundance$sampling == samps[i])] <- # rellenar la columna RR_ref de cada muestreo
     rep(na.omit(fs_abundance$c[which(fs_abundance$sampling == samps[i])])[1:4], # con el contenido de la columna c, omitiendo NA
         length(fs_abundance$sampling[which(fs_abundance$sampling == samps[i])])/4) #repitiendolo tantas veces como numero de filas contenga cada muestreo, dividido entre 4 (numero de filas del contenido de c) 
 }
 
fs_abundance$RR_abundance <- fs_abundance$c/fs_abundance$RR_ref
fs_abundance$RR_abundance <- fs_abundance$w/fs_abundance$RR_ref
fs_abundance$RR_abundance <- fs_abundance$p/fs_abundance$RR_ref
fs_abundance$RR_abundance <- fs_abundance$wp/fs_abundance$RR_ref


 
 