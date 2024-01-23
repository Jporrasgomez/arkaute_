
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

# Opening and transforming data(opening_floradata.R) ####
flora_raw <- read.csv("data/flora_db.csv")
summary(flora_raw)
str(flora_raw)

flora_raw <- flora_raw %>%
  mutate(across(where(is.character), as.factor))

#Ordenamos los muestreos por orden (añadir más levels a medida que vaya habiendo más muestreos)
desired_order <- c("s0_may", "s1_may", "s2_jun", "s3_jun", "s4_jul", "s5_jul", "s6_aug", 
                   "s7_sep", "s8_sep", "s9_oct", "s10_oct", "s11_nov")

flora_raw$sampling <- factor(flora_raw$sampling, levels = desired_order)

hist(log(flora_raw$abundance))
hist(log(flora_raw$height))
hist(log(flora_raw$Cb))
hist(log(flora_raw$Cm))
hist(log(flora_raw$Db))
hist(log(flora_raw$Dm))


# Modifying database
#Sumar 0.01 cm a los diámetros por el error del calibre con el que medimos
flora_raw$Dm <- flora_raw$Dm + 0.01
flora_raw$Db <- flora_raw$Db + 0.01

#Dividir entre 100 los valores de abundancia para que sean valores de %
flora_raw$abundance <- round(flora_raw$abundance/100, 3)

#Checking if we have mistakes (missing more than 2 variables with NA's)
#View(flora_raw %>%
#  filter(rowSums(is.na(select(., Dm, Db, Cm, Cb))) > 2))
#Estan todos los datos de los muestreos 0, 1 y 2 y los 
#26 datos del muestreo 3 donde no cogíamos medidas si la abundancia era menor de 5.


#Añadir la fecha. De momento no sé si es necesario. 
sampling_dates <- read.csv("data/sampling_dates.csv")
summary(sampling_dates)
str(sampling_dates)
sampling_dates$datenew <-  ymd(sampling_dates$date)
sampling_dates$month <- month(sampling_dates$datenew)
sampling_dates$date <- NULL
sampling_dates$micro.sampling <- NULL
sampling_dates$N.micro <- NULL

sampling_dates <- sampling_dates %>%
  mutate(across(where(is.character), as.factor))
print(sampling_dates)
  
flora_raw <- right_join(flora_raw, sampling_dates, by = join_by(sampling))
summary(flora_raw)

flora <- flora_raw %>% select(sampling, plot, treatment, species, abundance, height, Cb, Db, Cm, Dm, datenew, month)

#Añadir el numero de individuos que hay por sampling, plot y species. 
#Esto era necesario antes porque aplicabamos el criterio de "Si el numero de individuos es menor o igual que 4, la biomasa total
#de la especie será la suma de las biomasas unitarias de cada individuo. Si es mayor de 4, se calculará estimando con la abundancia"

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

hist(flora$x)
summary(flora$x)
boxplot(flora$x)
#There is a problem with the distribution of the data. Is extremely assymetrical. 
#If we see the outliers in the boxplot we cannot even see the boxplot, just the distribution of outliers. 

range(flora$x, na.rm = T)
median(flora$x, na.rm = TRUE)
quantile(flora$x, na.rm = T)
quantile(flora$x, na.rm = TRUE, probs = 0.95)

#Between the 75% quantile and the 100% one there is a difference of e^4

#We can try to take out the outliers from flora. There are 2 ways: either we reject outliers (1.5*IQR) or 
#we reject the extreme outliers (3*IQR)
#!! También quitamos los NA


flora1 <- flora[which(flora$x < (as.numeric(quantile(flora$x, na.rm = TRUE)[4] + (1.5 * IQR(flora$x, na.rm = TRUE))))),] 
flora3 <- flora[which(flora$x < (as.numeric(quantile(flora$x, na.rm = TRUE)[4] + (3 * IQR(flora$x, na.rm = TRUE))))),] 

#List of outliers: 
flora1_outl <- flora[which(flora$x > (as.numeric(quantile(flora$x, na.rm = TRUE)[4] + (1.5 * IQR(flora$x, na.rm = TRUE))))),] 
flora3_outl <- flora[which(flora$x > (as.numeric(quantile(flora$x, na.rm = TRUE)[4] + (3 * IQR(flora$x, na.rm = TRUE))))),] 
unique(flora1$species)
unique(flora3$species)
length(flora$plot)
length(flora1$plot)
length(flora3$plot)


#The amount of data points that we are removing are the following: 
length(flora1$x) - (length(flora1_outl$x) + length(flora[which(is.na(flora$x)) , ]$x))
length(flora3$x) - (length(flora3_outl$x) + length(flora[which(is.na(flora$x)) , ]$x))
length(flora$x)
length(flora1_outl$x)

print(ggarrange(
ggplot(flora, aes(y = x)) +
  geom_boxplot(), 
ggplot(flora1, aes(y = x)) +
  geom_boxplot(),
ggplot(flora3, aes(y = x)) +
  geom_boxplot(),
labels = c("A", "B", "C"),
ncol = 3, nrow = 1))



par(mfrow = c(1, 3))
hist(flora$x)
hist(flora1$x)
hist(flora3$x)
par(mfrow = c(1, 1))


unique(flora1$species)

length(which(flora$x > (as.numeric(quantile(flora$x, na.rm = TRUE)[4] + (3 * IQR(flora$x, na.rm = TRUE)))))) /
  nrow(flora) * 100

flora_noNA<- flora[which(!is.na(flora$x)) , ]$x

x <- seq(round(min(flora[which(!is.na(flora$x)) , ]$x), 0), round(max(flora[which(!is.na(flora$x)) , ]$x), 0), 25)

write.table(flora1$x, "data/x_values.txt", sep = "\t", row.names = FALSE)


## A PARTIR DE AQUÍ TRABAJO CON "flora1"

#Agrupar por sampling, plot y treatment primero. 
#Aquí se aplica, para biomasa, el siguiente criterio: se estima la biomasa de cada especie multiplicando su abundancia por 
#la masa promedio de los individuos medidos para dicha especie. 

flora1 <- flora1 %>%
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
flora1 <- flora1 %>%
  group_by(plot, sampling) %>%
  mutate(n_species = n()) %>%
  ungroup()

flora1_samplings <-  flora1 %>%
  group_by(sampling, datenew, month, treatment, plot) %>%
  reframe(biomass = sum(biomass, na.rm = T), 
          n_species = n_species, 
          abundance = sum(abundance, na.rm = T)) %>%
  distinct(sampling, datenew, month, plot, treatment, biomass, n_species, abundance)
#Hacer comprobaciones de los datos en esta base de datos


flora1_treatments <-  flora1_samplings %>%
  group_by(treatment) %>%
  reframe(biomass = mean(biomass, na.rm = T), 
          n_species = mean(n_species, na.rm = T), 
          abundance = mean(abundance, na.rm = T)) %>%
  distinct(treatment, biomass, n_species, abundance)                                  

# Gráficos por muestreo y tratamiento####


print(ggarrange(

ggsamplings_biomass <- 
  
ggplot(flora1_samplings, aes(x = sampling, y = biomass, color = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Biomass") +
  scale_color_manual(values = c("c" = "blue", "p" = "green", "w" = "red", "wp" = "purple")) +
  theme_minimal() +
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  theme(legend.position = "none", 
        axis.text = element_text(size = 12)),

ggsamplings_diversity <-
  
  ggplot(flora1_samplings, aes(x = sampling, y = n_species, color = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Diversity") +
  scale_color_manual(values = c("c" = "blue", "p" = "green", "w" = "red", "wp" = "purple")) + 
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text = element_text(size = 12)),

ggsamplings_abundance <- 
 
 ggplot(flora1_samplings, aes(x = sampling, y = abundance, color = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Abundance") +
  scale_color_manual(values = c("c" = "blue", "p" = "green", "w" = "red", "wp" = "purple")) +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text = element_text(size = 12)) +
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8),

labels = c("A", "B", "C"),
ncol = 1, nrow = 3))



print(ggarrange(
  
  ggtreat_biomass <-
  ggplot(flora1_samplings, aes(x = treatment, y = biomass, color = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Biomass") +
  scale_color_manual(values = c("c" = "blue", "p" = "green", "w" = "red", "wp" = "purple")) +
  theme_minimal() + 
  theme(legend.position = "none"),

ggtreat_diversity <- 
  ggplot(flora1_samplings, aes(x = treatment, y = n_species, color = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Diversity") +
  scale_color_manual(values = c("c" = "blue", "p" = "green", "w" = "red", "wp" = "purple")) +
  theme_minimal()+
  theme(legend.position = "none"),

ggtreat_abundance <- 
  ggplot(flora1_samplings, aes(x = treatment, y = abundance, color = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Abundance") +
  scale_color_manual(values = c("c" = "blue", "p" = "green", "w" = "red", "wp" = "purple")) +
  theme_minimal()+
  theme(legend.position = "none"),

labels = c("A", "B", "C"),
ncol = 3, nrow = 1))












