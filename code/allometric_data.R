
#Crear una lista con las especies de los muestreos 0, 1 y 2 de las que no se tienen datos. Para centrarme en esas especies

library(stringr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(readr)
library(ggpubr)
library(tidyverse)
source('code/tools/basicFun.R')
library(ggplot2)
library(gridExtra)


flora_raw<- read.csv("data/flora_db_new.csv")

flora_raw$Dm <- flora_raw$Dm + 0.01
flora_raw$Db <- flora_raw$Db + 0.01


pi <- 3.1416

flora_raw$cm <- round(ifelse(!is.na(flora_raw$Dm), flora_raw$Dm * pi, flora_raw$Cm), 2)
flora_raw$cb <- round(ifelse(!is.na(flora_raw$Db), flora_raw$Db * pi, flora_raw$Cb), 2)

flora <- flora_raw %>% select("species", "height", "cb", "cm")

flora$species <- as.factor(flora$species)

# Filter out rows with missing values in height or cb
flora_filtered <- flora %>% filter(!is.na(height) & !is.na(cb) & !is.na(cb))

# Count the number of rows for each species
species_counts<- flora_filtered %>%
  group_by(species) %>%
  summarise(row_count = n()) %>%
  filter(row_count < 4)

flora_filtered <- anti_join(flora_filtered, species_counts, by = "species")

length(levels(flora_filtered$species))
#Salen 110 especies. DE QUEEEEEEEE



#CB vs HEIGHT
#De momento no funciona nada :D

#Intentos de hacer 1 por 1

r_squared <- summary(lm(height ~ cb , data = subset(flora_filtered, species == "bepe")))$r.squared
p_value <- summary(lm(height ~ cb , data = subset(flora_filtered, species == "bepe")))$coefficients[2, 4] 
num_measurements <- nrow(subset(flora_filtered, species == "bepe"))  # Get the number of measurements

ggplot(subset(flora_filtered, species == "bepe"), aes(x = cb, y = height)) +
  geom_point() +  # Add scatter plot
  geom_smooth(method = "lm", se = FALSE) +  
  labs(title = "Bellis perennis", subtitle = paste("n:", nrow(subset(flora_filtered, species == "bepe")) , 
                                   "   R-squared:", round(summary(lm(height ~ cb , data = subset(flora_filtered, species == "bepe")))$r.squared, 3), 
                                   "   p-value (cb):", format(((lm(height ~ cb , data = subset(flora_filtered, species == "bepe")))$coefficients[2, 4]), scientific = TRUE, digits = 3)))

#Intentos de hacer todas de golpe

plots_cb_species <- lapply(levels(flora_filtered$species), function(s) {
  subset_data <- flora_filtered[flora_filtered$species == s, ]  # Subset data for the current species
  
  lm_model <- lm(height ~ cb , data = subset_data)
    
  r_squared <- summary(lm_model)$r.squared
  p_value <- summary(lm_model)$coefficients[2, 4] 
  num_measurements <- nrow(subset_data)  # Get the number of measurements
  
  plot <- ggplot(subset_data, aes(x = cb, y = height)) +
    geom_point() +  # Add scatter plot
    geom_smooth(method = "lm", se = FALSE) +  
    labs(title = s, subtitle = paste("n:", num_measurements, 
                                     "   R-squared:", round(r_squared, 3), 
                                     "   p-value (cb):", format(p_value, scientific = TRUE, digits = 3)))  # Add species name, number of measurements, R-squared, and p-value as title
  
  return(plot)
})


cb_plots <- do.call(gridExtra::grid.arrange, plots_cb_species)
print(cb_plots)


 #CM vs HEIGHT

plots_cm_species <- lapply(levels(flora$species), function(s) {
  subset_data <- flora[flora$species == s, ]  # Subset data for the current species
  
  lm_model <- lm(height ~ cm , data = subset_data)
  r_squared <- summary(lm_model)$r.squared
  p_value <- summary(lm_model)$coefficients[2, 4] 
  num_measurements <- nrow(subset_data)  # Get the number of measurements
  
  plot <- ggplot(subset_data, aes(x = cm, y = height)) +
    geom_point() +  # Add scatter plot
    geom_smooth(method = "lm", se = FALSE) +  
    labs(title = s, subtitle = paste("n:", num_measurements, 
                                     "   R-squared:", round(r_squared, 3), 
                                     "   p-value (cm):", format(p_value, scientific = TRUE, digits = 3)))  # Add species name, number of measurements, R-squared, and p-value as title
  
  return(plot)
})


cm_plots <- do.call(gridExtra::grid.arrange, plots_cm_species)
print(cm_plots)



library(ggplot2)
library(gridExtra)

