
#Ver correlacion con X (modeo)
#

library(stringr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(readr)
library(ggpubr)
library(tidyverse)
library(ggplot2)
library(gridExtra)


flora_raw<- read.csv("data/flora_db.csv")
flora_raw <- flora_raw %>%
  mutate(across(where(is.character), as.factor))
str(flora_raw)
length(unique(flora_raw$species))

flora_raw$Dm <- flora_raw$Dm + 0.01
flora_raw$Db <- flora_raw$Db + 0.01


pi <- 3.1416

flora_raw$cm <- round(ifelse(!is.na(flora_raw$Dm), flora_raw$Dm * pi, flora_raw$Cm), 2)
flora_raw$cb <- round(ifelse(!is.na(flora_raw$Db), flora_raw$Db * pi, flora_raw$Cb), 2)

flora <- flora_raw %>% select("species", "sampling", "height", "cb", "cm")



# Filter out rows with missing values in height or cb
#flora_filtered <- flora %>% filter(!is.na(height) & !is.na(cb) & !is.na(cb))




#List of species from samplings 0, 1 and 2: UTILIZAR "unique" instead of "levels"

selected_samplings0 <- c("s_0", "s_1", "s_2")
flora_s012 <- flora[flora$sampling %in% selected_samplings0, ]

#Comprobacion numero de especies. Hacer con "unique". La función "levels" tiene algo así como "memoria" y te da el valor de la database original
#Hay 53 especies a diagnosticar en los primeros 3 samplings. 
length(unique(flora_s012$species))
length(levels(flora_s012$species))

#Se puede hacer esto: 
flora_s012 <- droplevels(flora_s012)
#De esta manera se eliminan los levels que no están ya presentes. Ahora aparece 48 levels en el environment y si le pregunto por levels
#me responde bien, igual que con uniques
length(levels(flora_s012$species)) - length(unique(flora_s012$species))


#Crear base de datos solo con las especies presentes en los samplings 0, 1 y 2. 
flora_s012_listofspecies <- unique(flora_s012$species)
species_s012 <- expand.grid(flora_s012_listofspecies)
colnames(species_s012) <- c("species")


#Extracting rows from "flora" that matches with the species found within flora_s012
flora_species_s012 <- merge(flora, species_s012)
#Ahora tengo que quitar los samplings 0 1 y 2 para que no apliquen al modelo los NA's que contienen
selected_samplings <- c("s_3", "s_4", "s_5", "s_6", "s_7", "s_8", "s_9", "s_10", "s_11")
flora_morph_data <- flora_species_s012[flora_species_s012$sampling %in% selected_samplings, ]

  
#Comprobacion numero de especies. Hay 34 en vez de 53.  
length(unique(flora_morph_data$species))

#En vez de 53 salen 34. Hay 14 especies presentes en los muestreos 0, 1 y 2 para los cuales no hay datos
#en los sguientes muestreos ¿cuales?: 
#Primero tengo que droplevear el factor de species porque vuelve a aparecer el 110. 
flora_species_s012$species <- droplevels(flora_species_s012$species)
flora_morph_data$species <- droplevels(flora_morph_data$species)
sp1 <- unique(flora_species_s012$species)
sp2 <- unique(flora_morph_data$species)
sp3 <- setdiff(sp1, sp2)
#Estas son las especies de las que no podremos generar datos: 
sp3


#Añado el valor de X (modelo de la ecuacion de biomasa)

flora_morph_data$Ah <- ((flora_morph_data$cm)^2)/4*pi
flora_morph_data$Ab <- ((flora_morph_data$cb)^2)/4*pi

d <- 1.96
z <- 2/3
flora_morph_data$x <- (flora_morph_data$height/2)*(flora_morph_data$Ab + flora_morph_data$Ah)
flora_morph_data$biomass <- d*(flora_morph_data$x^z)

#CB vs HEIGHT #########
#Intento de data.frame:

# Count the number of rows for each species and delete those that have less than 2 rows (2 measurements) 
species_lower2<- flora_morph_data %>%
  group_by(species) %>%
  summarise(row_count = n()) %>%
  filter(row_count < 2)

flora_morph_data <- anti_join(flora_morph_data, species_lower2, by = "species")

summary(flora_morph_data)
#Hay NA's. Hay que quitarlos. 

flora_morph_data<- flora_morph_data %>% filter(!is.na(height) & !is.na(cb) & !is.na(cm))
summary(flora_morph_data)
library(purrr)


# CB vs HEIGHT #####

# Define a function to extract information for a given species
cb_df_function <- function(species) {
  subset_data <- filter(flora_morph_data, species == !!species)
  
  # Check if there are non-missing values for both height and cb
  if (sum(!is.na(subset_data$height) & !is.na(subset_data$cb)) > 0) {
    lm_model <- lm(cb ~ height, data = subset_data)
    
    return(data.frame(
      species = species,
      cb_R_squared = summary(lm_model)$r.squared,
      cb_P_value = summary(lm_model)$coefficients[2, 4],
      num_measurements = nrow(subset_data)
    ))
  } else {
    # If there are no non-missing values, return NA or any other indicator
    return(data.frame(
      species = species,
      cb_R_squared = NA,
      cb_P_value = NA, 
      num_measurements = 0
    ))
  }
}

# Apply the function to each species level and bind the results
cb_df_correlation <- map_dfr(sp2, cb_df_function)


# CM #####

# Define a function to extract information for a given species
cm_df_function <- function(species) {
  subset_data <- filter(flora_morph_data, species == !!species)
  
  # Check if there are non-missing values for both height and cm
  if (sum(!is.na(subset_data$height) & !is.na(subset_data$cm)) > 0) {
    lm_model <- lm(cm ~ height, data = subset_data)
    
    return(data.frame(
      species = species,
      cm_R_squared = summary(lm_model)$r.squared,
      cm_P_value = summary(lm_model)$coefficients[2, 4],
      num_measurements = nrow(subset_data)
    ))
  } else {
    # If there are no non-missing values, return NA or any other indicator
    return(data.frame(
      species = species,
      cm_R_squared = NA,
      cm_P_value = NA, 
      num_measurements = 0
    ))
  }
}

# Apply the function to each species level and bind the results
cm_df_correlation <- map_dfr(sp2, cm_df_function)


# X vs HEIGHT #####

# Define a function to extract information for a given species

x_df_function <- function(species) {
  subset_data <- filter(flora_morph_data, species == !!species)
  
  # Check if there are non-missing values for both height and cm
  if (sum(!is.na(subset_data$height) & !is.na(subset_data$x)) > 0) {
    lm_model <- lm(x ~ height, data = subset_data)
    
    return(data.frame(
      species = species,
      x_R_squared = summary(lm_model)$r.squared,
      x_P_value = summary(lm_model)$coefficients[2, 4],
      num_measurements = nrow(subset_data)
    ))
  } else {
    # If there are no non-missing values, return NA or any other indicator
    return(data.frame(
      species = species,
      x_R_squared = NA,
      x_P_value = NA, 
      num_measurements = 0
    ))
  }
}

# Apply the function to each species level and bind the results
x_df_correlation <- map_dfr(sp2, x_df_function)


#Merging correlation data
correlation_data <- merge(cm_df_correlation, cb_df_correlation)
correlation_data <- merge(correlation_data, x_df_correlation)

correlation_data %>% write.csv("results/correlation_results.csv")

#Conclusión. No hay correlación para cb y cm, pero si que hay algo para X. 

#Comprobaciones las que sí que tienen correlacion con más de 10 medidas



#Linum bienne
print(ggarrange(
  
ggplot(subset(flora_morph_data, species == "libi"), aes(x = cb, y = height)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = "Linum bienne",
      subtitle = paste(
        "n:", nrow(subset(flora_morph_data, species == "libi")),
        ", R-squared:", round(summary(lm(cb ~ height, data = subset(flora_morph_data, species == "libi")))$r.squared, 3),
        ", p-value:", format(summary(lm(cb ~ height, data = subset(flora_morph_data, species == "libi")))$coefficients[2, "Pr(>|t|)"], scientific = TRUE, digits = 3)
      )
    ),
  
  
ggplot(subset(flora_morph_data, species == "libi"), aes(x = cm, y = height)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = " ",
      subtitle = paste(
        "n:", nrow(subset(flora_morph_data, species == "libi")),
        ", R-squared:", round(summary(lm(cm ~ height, data = subset(flora_morph_data, species == "libi")))$r.squared, 3),
        ", p-value:", format(summary(lm(cm ~ height, data = subset(flora_morph_data, species == "libi")))$coefficients[2, "Pr(>|t|)"], scientific = TRUE, digits = 3)
      )
    ),

labels = c(" ", " "),
ncol = 2, nrow = 1))


