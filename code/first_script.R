
# Revisar samplin 15, plot 5, especie shar. Le faltan datos.
# Reisar sampling 17, plot 3, especie apar. Le faltan datos. 


pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr)


# Usamos amaranthus? Outliers? el que??


flora_raw <- read.csv("data/flora_db_raw.csv") # Opening and transforming data(opening_floradata.R) ####

flora_raw <- flora_raw %>%
  mutate(across(where(is.character), as.factor))

flora_raw$plot <- factor(flora_raw$plot)


flora_raw$sampling <- factor(flora_raw$sampling, levels = sort(unique(flora_raw$sampling)))
flora_raw$treatment <- factor(flora_raw$treatment, levels =  c("c", "w", "p", "wp") )
flora_raw$plot <- factor(flora_raw$plot, levels = sort(unique(flora_raw$plot)))

flora_raw <- select(flora_raw, -date, -category, -OBS, -ni.agrupado, -familia, -species_old_1, -species_old_2)


# Adding dates
sampling_dates <- read.csv("data/sampling_dates.csv")
sampling_dates$sampling <- factor(sampling_dates$sampling)

sampling_dates$datenew <-  ymd(sampling_dates$date)
sampling_dates$month <- month(sampling_dates$datenew, label = TRUE)
sampling_dates$day <- day(sampling_dates$datenew)
sampling_dates$year <- year(sampling_dates$datenew)
sampling_dates$date <- sampling_dates$datenew
sampling_dates$micro_sampling <- NULL
sampling_dates$label_micro <- NULL


sampling_dates <- sampling_dates %>%
  mutate(across(where(is.character), as.factor))

flora_raw <- right_join(flora_raw, sampling_dates, by = join_by(sampling))


flora <- flora_raw %>% select(sampling, plot, treatment, code, abundance, height, Cb, Db, Cm, Dm, date, month)
#flora <- flora[flora$species != "am", ] # Taking out super extreme outlier
#flora <- flora[!apply(is.na(flora), 1, all), ]# removing NA full lines 


anyNA(flora_raw)

#Checking if we have mistakes (missing more than 2 variables with NA's)
#View(flora %>%
#  filter(rowSums(is.na(select(., Dm, Db, Cm, Cb))) > 2))
#Estan todos los datos de los muestreos 0, 1 y 2 y los 
#26 datos del muestreo 3 donde no cogíamos medidas si la abundancia era menor de 5.


#Adding species information
species_code <- read.csv("data/species_code.csv")
species_code <- select(species_code, species, code, family, genus_level, species_level)
species_code <- species_code %>%
  mutate(across(where(is.character), as.factor))


# WE WORK WITH IDENTIFIED SPECIES!!


flora <- merge(flora, species_code, by = "code") 


## Simplificación de base de datos donde hacemos: 
### 1) Calculamos la media de los valores morfológicos por especie, plot y sampling
### 2) Corrección de los grupos de asteraceae, poaceae y otros que contienen varios valores morfológicos y abundancias 
###   sampling y plot ya que en su día las diferenciabamos en campo. Al calcular la media de la abundancia, corregimos este problema 
flora <- flora %>%   
  group_by(code, sampling, plot, treatment, date, month, species, family, genus_level, species_level) %>%
  summarize(abundance = mean(abundance, na.rm = T),
            height = mean(height, na.rm = T),
            Cb = mean(Cb, na.rm = T),
            Db = mean(Db, na.rm = T),
            Cm = mean(Cm, na.rm = T),
            Dm = mean(Dm, na.rm = T))



# RICHNESSS
flora_richness <- summarise(group_by(flora, plot, sampling),
                             richness = n_distinct(code, na.rm = T)) ##adding number of species

flora <- merge(flora, flora_richness)

# ABUNDANCE

flora_abundance <- summarise(group_by(flora, plot, sampling),
                             abundance_total = sum(abundance, na.rm = T)) ##adding number of species

flora <- merge(flora, flora_abundance)


# BIOMASS 

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
flora$biomass_i <- d*(flora$x^z)


# Incluimos numero de individuos/m2 medidos posteriormente en el campo

nind_lm_data <- read.csv("data/nind_lm_data.csv")

flora <- merge(flora, nind_lm_data, by = "code")

flora$nind_m2 <- flora$intercept + flora$abundance * flora$slope
flora <- flora %>%
  mutate(nind_m2 = ifelse(nind_m2 < 0, 0.1, nind_m2))

flora$biomass <- flora$biomass_i *flora$nind_m2


flora_biomass_total <- summarise(group_by(flora, plot, sampling),
                                 biomass_total =  sum(biomass, na.rm = TRUE))


flora <- merge(flora, flora_biomass_total)





flora_complete <- flora %>% select(sampling, date, month, treatment, plot, abundance, code, family, genus_level, species_level, height, Ah, Ab, biomass_i, nind_m2, intercept, slope, r_squared, p_value, biomass, richness, abundance_total, biomass_total)


flora <- flora %>% select(sampling, date, month, treatment, plot, abundance, code, family, genus_level, species_level, richness, abundance_total, biomass_total)
  
#Mirar si la distribución de los datos de biomasa tienen sentido una vez aplicada la estimacion en base a la abundancia


#Especies para las cuales no tenemos datos

#flora %>% write.csv("data/flora_db.csv", row.names  = F)
#flora_complete %>% write.csv("data/flora_complete_db.csv", row.names  = F)


rm(species_code)
rm(flora_raw)
rm(sampling_dates)
rm(flora_richness)
rm(flora_abundance)
rm(flora_biomass_total)
rm(d)
rm(z)
rm(nind_lm_data)


