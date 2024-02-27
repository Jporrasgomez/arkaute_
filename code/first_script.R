

library(lubridate)
library(dplyr)
library(reshape2)
library(tidyverse)



flora_raw <- read.csv("data/flora_db_raw.csv") # Opening and transforming data(opening_floradata.R) ####

flora_raw <- flora_raw %>%
  mutate(across(where(is.character), as.factor))

flora_raw$plot <- factor(flora_raw$plot)


desired_order <- c("0", "1", "2", "3", "4", "5", "6",   #Sort from lowest to highest
                   "7", "8", "9", "10", "11")
desired_order_treat <- c("c", "w", "p", "wp")           #Sort from lowest to highest


flora_raw$sampling <- factor(flora_raw$sampling, levels = desired_order)
flora_raw$treatment <- factor(flora_raw$treatment, levels = desired_order_treat)
flora_raw <- select(flora_raw, -date)


# Adding dates
sampling_dates <- read.csv("data/sampling_dates.csv")
sampling_dates$sampling <- factor(sampling_dates$sampling)

sampling_dates$datenew <-  ymd(sampling_dates$date)
sampling_dates$month <- month(sampling_dates$datenew, label = TRUE)
sampling_dates$day <- day(sampling_dates$datenew)
sampling_dates$year <- year(sampling_dates$datenew)
sampling_dates$date <- sampling_dates$datenew
sampling_dates$micro.sampling <- NULL
sampling_dates$N.micro <- NULL


sampling_dates <- sampling_dates %>%
  mutate(across(where(is.character), as.factor))

flora_raw <- right_join(flora_raw, sampling_dates, by = join_by(sampling))


flora <- flora_raw %>% select(sampling, plot, treatment, species, abundance, height, Cb, Db, Cm, Dm, date, month)
flora <- flora[flora$species != "am", ] # Taking out super extreme outlier
flora <- flora[!apply(is.na(flora), 1, all), ]# removing NA full lines 


anyNA(flora_raw)

#Checking if we have mistakes (missing more than 2 variables with NA's)
#View(flora %>%
#  filter(rowSums(is.na(select(., Dm, Db, Cm, Cb))) > 2))
#Estan todos los datos de los muestreos 0, 1 y 2 y los 
#26 datos del muestreo 3 donde no cogíamos medidas si la abundancia era menor de 5.


#Changes into flora

flora <- flora %>%
  group_by(plot, sampling, species) %>%
  mutate(n_individuals = n()) %>% #number of individuals per species
  ungroup()


#Sumar 0.01 cm a los diámetros por el error del calibre con el que medimos
flora$Dm <- flora$Dm + 0.01
flora$Db <- flora$Db + 0.01

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

flora <- flora %>%
  group_by(sampling, date, month, treatment, plot, abundance, species) %>%
  reframe(biomass = mean(biomass, na.rm = TRUE) * (abundance/100)) %>%  #Calculating biomass per species, plot and sampling
  distinct(sampling, date, month, plot, treatment, abundance, species, biomass)
#Mirar si la distribución de los datos de biomasa tienen sentido una vez aplicada la estimacion en base a la abundancia

#Transformo en NA los valores 0 de biomasa, que corresponden a los datos de los samplings 0, 1, 2 y los 26 datos del 3. 
#Esto es para el gráfico, pero hay que ver las correlaciones entre H, cb, cm...
#! Esdte paso no es necesario ahora con flora1, ya que al generar la df hicimos un na.rm = T
#flora1$biomass <- ifelse(flora1$biomass == 0, NA, flora1$biomass)


flora <- flora %>%
  group_by(plot, sampling) %>%
  mutate(n_species = if_else(is.na(species), NA, n())) %>% #adding number of species per plot and sampling
  ungroup()


species_code <- read.csv("data/species_code.csv")

flora$code <- flora$species
flora <- select(flora, date, month, sampling, plot, treatment, code, abundance, biomass, n_species)

species_code <- select(species_code, species, code, family, genus_level, species_level)
species_code <- species_code %>%
  mutate(across(where(is.character), as.factor))


# WE WORK WITH IDENTIFIED SPECIES!!

flora <- merge(flora, species_code, by = "code") # at this step, all species that are non identified in "species_code" are depleted !!!!!
rm(species_code)

 

#flora %>% write.csv("data/flora_db.csv")

rm(flora_raw)
rm(sampling_dates)
rm(desired_order)
rm(desired_order_treat)

