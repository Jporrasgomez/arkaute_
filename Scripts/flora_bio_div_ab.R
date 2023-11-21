
# Cosas que hacer ####
#Rank abundance distribution
#MIrar si hay correlaciones entre alturas y circunferencias 
#Hacer un loop con distintos valores de Z para ver las diferencias. Explorar cómo cambian los datos
#utilizando distintos valores de z. 
#Mirar si podemos utilizar varias ecuaciones en función de la especie. 
#Mirar diferencias en la distribución de los datos de biomasa una vez aplicada la ecuación
#y después de calcular las biomasas por especie y plot. Lo esperable es que una vez estimada la biomasa en base a la abundancia
# el histograma sea más "alargado" en la cola.

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
source('Scripts/basicFun.R')

# Opening and transforming data ####
flora_raw <- read.csv("Data/flora_db.csv")
summary(flora_raw)
str(flora_raw)

#!!! Transformar a factorial los characteres

#Ordenamos los muestreos por orden (añadir más levels a medida que vaya habiendo más muestreos)
desired_order <- c("sampling 0", "sampling 1", "sampling 2", "sampling 3", "sampling 4", "sampling 5", "sampling 6", 
                   "sampling 7", "sampling 8", "sampling 9", "sampling 10")

flora_raw$sampling <- factor(flora_raw$sampling, levels = desired_order)


ggplot(flora_raw, aes(x = abundance)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "", y = "Frequency") +
  ggtitle("Abundance") + 
  theme_minimal()

ggplot(flora_raw, aes(x = height)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "", y = "Frequency") +
  ggtitle("Height") + 
  theme_minimal()

ggplot(flora_raw, aes(x = Cb)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "", y = "Frequency") +
  ggtitle("Cb") + 
  coord_cartesian(xlim = c(0, 20)) +
  theme_minimal()

ggplot(flora_raw, aes(x = Cm)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "", y = "Frequency") +
  ggtitle("Cm") + 
  coord_cartesian(xlim = c(0, 10)) +
  theme_minimal()

ggplot(flora_raw, aes(x = Db)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "", y = "Frequency") +
  ggtitle("Db") + 
  theme_minimal()

ggplot(flora_raw, aes(x = Dm)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "", y = "Frequency") +
  ggtitle("Dm") + 
  theme_minimal()


#mirar histogramas. Son datos asimétricos - Distribucion log normal. ¿Transformar?
#Escalar eje X para ver mejor los histogramas
hist(flora_raw$abundance)
hist(flora_raw$height)
hist(flora_raw$Cm)
hist(flora_raw$Cb)
hist(flora_raw$Dm)
hist(flora_raw$Db)

hist(log(flora_raw$abundance))
hist(log(flora_raw$height))
hist(log(flora_raw$Cm))
hist(log(flora_raw$Cb))
hist(log(flora_raw$Dm))
hist(log(flora_raw$Db))


#Sumar 0.01 cm a los diámetros por el error del calibre con el que medimos
flora_raw$Dm <- flora_raw$Dm + 0.01
flora_raw$Db <- flora_raw$Db + 0.01

#Dividir entre 100 los valores de abundancia para que sean valores de %
flora_raw$abundance <- round(flora_raw$abundance/100, 3)

#Checking if we have mistakes (missing more than 2 variables with NA's)
View(flora_raw %>%
  filter(rowSums(is.na(select(., Dm, Db, Cm, Cb))) > 2))
#Estan todos los datos de los muestreos 0, 1 y 2 y los 
#26 datos del muestreo 3 donde no cogíamos medidas si la abundancia era menor de 5.


#Añadir la fecha. De momento no sé si es necesario. 
sampling_dates <- read.csv("Data/sampling_dates.csv")
summary(sampling_dates)
str(sampling_dates)
sampling_dates$datenew <-  ymd(sampling_dates$date)
sampling_dates$month <- month(sampling_dates$datenew)
sampling_dates$date <- NULL
sampling_dates$micro.sampling <- NULL
sampling_dates$N.micro <- NULL

sampling_dates <- sampling_dates %>%
  mutate(across(where(is.character), as.factor))
  
flora_raw <- right_join(flora_raw, sampling_dates, by = join_by(sampling))
summary(flora_raw)

flora <- flora_raw %>% select(sampling, plot, treatment, species, abundance, height, Cb, Db, Cm, Dm, datenew, month)

#Añadir el numero de individuos que hay por sampling, plot y species. 

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

#Application of equation proposed by paper 
#Me invento unos valores de d y z. Es importante el valor de z porque al ser un coeficiente de potencia
#puede alterar mucho las diferencias que haya entre los valores de biomasa
#Hacer un loop con distintos valores de Z para ver las diferencias
d <- 0.11
z <- 0.5
flora$biomass <- d*(((flora$height/6)*(flora$Ab + 2*flora$Ah + sqrt(flora$Ab*flora$Ah)))^z)

#Agrupar por sampling, plot y treatment primero. 
#Aquí se aplica, para biomasa, el siguiente criterio: Si el numero de individuos medidos es
#igual o superior a 5, la biomasa total de la especie se estima calculando la media de los 
#individuos medidos para luego multiplicarla por su abundancia. Si es menor de 5, la biomasa de la especie
#se calculará sumando los datos individuales de cada individuo medido. 

flora <- flora %>%
  group_by(sampling, datenew, month, treatment, plot, abundance, species) %>%
  reframe(biomass = mean(biomass, na.rm = TRUE) * abundance) %>%
  distinct(sampling, datenew, month, plot, treatment, abundance, species, biomass)
#Mirar si la distribución de los datos de biomasa tienen sentido una vez aplicada la estimacion en base a la abundancia

#Transformo en NA los valores 0 de biomasa, que corresponden a los datos de los samplings 0, 1, 2 y los 26 datos del 3. 
flora$biomass <- ifelse(flora$biomass == 0, NA, flora$biomass)

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



ggsamplings_biomass <- 
  
ggplot(flora_samplings, aes(x = sampling, y = biomass, color = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Biomass") +
  scale_color_manual(values = c("c" = "blue", "p" = "green", "w" = "red", "wp" = "purple")) +
  theme_minimal() +
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  theme(legend.position = "none", 
        axis.text = element_text(size = 12)) +
    coord_cartesian(ylim = c(0, 5))

ggsamplings_diversity <-
  
  ggplot(flora_samplings, aes(x = sampling, y = n_species, color = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Diversity") +
  scale_color_manual(values = c("c" = "blue", "p" = "green", "w" = "red", "wp" = "purple")) + 
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text = element_text(size = 12)) 

ggsamplings_abundance <- 
 
 ggplot(flora_samplings, aes(x = sampling, y = abundance, color = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Abundance") +
  scale_color_manual(values = c("c" = "blue", "p" = "green", "w" = "red", "wp" = "purple")) +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text = element_text(size = 12)) +
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8)


ggtreat_biomass <- ggplot(flora_samplings, aes(x = treatment, y = biomass, color = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Biomass") +
  scale_color_manual(values = c("c" = "blue", "p" = "green", "w" = "red", "wp" = "purple")) +
  theme_minimal() + 
  coord_cartesian(ylim = c(0, 5))+
  theme(legend.position = "none")

ggtreat_diversity <- ggplot(flora_samplings, aes(x = treatment, y = n_species, color = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Diversity") +
  scale_color_manual(values = c("c" = "blue", "p" = "green", "w" = "red", "wp" = "purple")) +
  theme_minimal()+
  theme(legend.position = "none")

ggtreat_abundance <- ggplot(flora_samplings, aes(x = treatment, y = abundance, color = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Abundance") +
  scale_color_manual(values = c("c" = "blue", "p" = "green", "w" = "red", "wp" = "purple")) +
  theme_minimal()+
  theme(legend.position = "none")



ggbiomass<- ggarrange(ggsamplings_biomass, ggtreat_biomass,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)
         
ggdiversity<- ggarrange(ggsamplings_diversity, ggtreat_diversity,
                      labels = c("A", "B"),
                      ncol = 2, nrow = 1)

ggabundance<- ggarrange(ggsamplings_abundance, ggtreat_abundance,
                      labels = c("A", "B"),
                      ncol = 2, nrow = 1)

ggbiomass
ggdiversity
ggabundance


ggsamplings <- ggarrange(ggsamplings_biomass, ggsamplings_diversity, ggsamplings_abundance,
                         labels = c("A", "B", "C"),
                         ncol = 1, nrow = 3)

ggtreatment <- ggarrange(ggtreat_biomass, ggtreat_diversity, ggtreat_abundance,
                         labels = c("A", "B", "C"),
                         ncol = 3, nrow = 1)
ggsamplings
ggtreatment


# Biomasa vs Diversidad (Structure vs Function) ####


ggplot(flora_samplings, aes(x = n_species, y = biomass, color = treatment)) +
  stat_smooth() +
  labs(x = "Diversity", y = "Biomass") +
  scale_color_manual(values = c("c" = "blue", "p" = "green", "w" = "red", "wp" = "purple")) +
  theme_minimal() +
  coord_cartesian(ylim = c(-1, 15))

ggplot(flora, aes(x = species, y = abundance, color = treatment)) + 
  geom_boxplot() + 
  labs(x = "Rank", y = "Abundance") +
  scale_color_manual(values = c("c" = "blue", "p" = "green", "w" = "red", "wp" = "purple")) +
  theme_minimal()

ggplot(flora_samplings, aes(x = n_species, y = abundance, color = treatment)) +
  geom_smooth()+
  labs(x = "Diversity", y = "Abundance") +
  scale_color_manual(values = c("c" = "blue", "p" = "green", "w" = "red", "wp" = "purple")) +
  theme_minimal()

#Hacer Rank abundance distribution ¿Cómo?



# Diferencias estadísticas ####

#La distribución de los datos no es normal, y el test ANOVA asume normalidad de los datos.
# O transformar los datos o utilizar test estadisticos no paramétricos. 

#Between treatments
summary(aov(biomass ~ treatment, data = flora_samplings))
TukeyHSD(aov(biomass ~ treatment, data = flora_samplings))

summary(aov(n_species ~ treatment, data = flora_samplings))
TukeyHSD(aov(n_species ~ treatment, data = flora_samplings))

summary(aov(abundance ~ treatment, data = flora_samplings))
TukeyHSD(aov(abundance ~ treatment, data = flora_samplings))

#Between samplings
summary(aov(biomass ~ sampling, data = flora_samplings))
TukeyHSD(aov(biomass ~ sampling, data = flora_samplings))

summary(aov(n_species ~ sampling, data = flora_samplings))
TukeyHSD(aov(n_species ~ sampling, data = flora_samplings))


summary(aov(abundance ~ sampling, data = flora_samplings))
TukeyHSD(aov(abundance ~ sampling, data = flora_samplings))





