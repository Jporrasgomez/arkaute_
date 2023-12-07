
#Crear una lista con las especies de los muestreos 0, 1 y 2 de las que no se tienen datos. Para centrarme en esas especies
#no Está funcionando!

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

flora <- flora_raw %>% select("species", "sampling", "height", "cb", "cm")

flora$species <- as.factor(flora$species)

# Filter out rows with missing values in height or cb
flora_filtered <- flora %>% filter(!is.na(height) & !is.na(cb) & !is.na(cb))

# Count the number of rows for each species
species_counts<- flora_filtered %>%
  group_by(species) %>%
  summarise(row_count = n()) %>%
  filter(row_count < 2)


flora_filtered <- anti_join(flora_filtered, species_counts, by = "species")

length(levels(flora_filtered$species))
#Salen 110 especies. DE QUEEEEEEEE

#List of species from samplings 0, 1 and 2: 

flora_s0 <- subset(flora, sampling == "sampling 0")
flora_s1 <- subset(flora, sampling ==  "sampling 1")
flora_s2 <- subset(flora, sampling == "sampling 2")

flora_s012 <- merge(flora_s0, flora_s1, all = T)
flora_s012 <- merge (flora_s012, flora_s2, all = T)

rm(flora_s0)
rm(flora_s1)
rm(flora_s2)

#Comprobacion numero de especies. SALE 110. MAL
length(levels(flora_s012$species))


#Extracting species_s012 from "flora_filtered"NO ESTÁ BIEN HECHO!!!!!!!!
flora_species_s012 <- right_join(flora_filtered, species_s012, by = "species") %>% 
  filter(!is.na(height) & !is.na(cb) & !is.na(cb))
#Comprobacion numero de especies. SAle 110. MAL
length(levels(flora_species_s012$species))



#CB vs HEIGHT
#Intento de data.frame:

library(purrr)

# Define a function to extract information for a given species
get_species_info <- function(species) {
  subset_data <- filter(flora_species_s012, species == !!species)
  lm_model <- lm(height ~ cb, data = subset_data)
  
  data.frame(
    species = species,
    r_squared = summary(lm_model)$r.squared,
    p_value = summary(lm_model)$coefficients[2, "Pr(>|t|)"],
    num_measurements = nrow(subset_data)
  )
}

# Get the unique species levels
species_levels <- levels(flora_species_s012$species)

# Apply the function to each species level and bind the results
results_df <- map_dfr(species_levels, get_species_info)

# View the resulting data frame
print(results_df)

#De momento no funciona nada :D

#Intentos de hacer 1 por 1

#Lm regression
summary(lm(height ~ cb , data = subset(flora_species_s012, species == "bepe")))
#r_squared
summary(lm(height ~ cb , data = subset(flora_species_s012, species == "bepe")))$r.squared
#p_value
summary(lm(height ~ cb , data = subset(flora_species_s012, species == "bepe")))$coefficients[2, 4] 
#num_measurements
nrow(subset(flora_species_s012, species == "bepe"))  # Get the number of measurements

#Graphs: for p-value, it doesn't properly work with "gom_smooth" so it has to be modified that way (Chat gpt)
ggplot(subset(flora_species_s012, species == "bepe"), aes(x = cb, y = height)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Bellis perennis",
    subtitle = paste(
      "n:", nrow(subset(flora_species_s012, species == "bepe")),
      ", R-squared:", round(summary(lm(height ~ cb, data = subset(flora_species_s012, species == "bepe")))$r.squared, 3),
      ", p-value (cb):", format(summary(lm(height ~ cb, data = subset(flora_species_s012, species == "bepe")))$coefficients[2, "Pr(>|t|)"], scientific = TRUE, digits = 3)
    )
  )

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

