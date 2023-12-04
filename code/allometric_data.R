

library(stringr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(readr)
library(ggpubr)
library(tidyverse)
source('Scripts/basicFun.R')


plots <- read.csv("data/plots.csv")
#IMPORTANT! Change the value of the DATE everytime a new set of data is opened.It depends on the day you took the data from the sensors. 
plots$file_code <- paste0("data_", plots$sensor_code, "_2023_11_23_0.csv")

allom <- read.csv("data/flora_db_new.csv")
str(allom)
summary(allom)

pi <- 3.1416

allom$bc <- round(ifelse(is.na(allom$bc_c), pi * allom$bc_d, allom$bc_c), 2)
allom$mc <- round(ifelse(is.na(allom$mc_c), pi * allom$mc_d, allom$mc_c), 2)

allom <- allom %>% select("species", "height", "bc", "mc")

allom$species <- as.factor(allom$species)


library(ggplot2)
library(gridExtra)



#BC vs HEIGHT

plots_bc_species <- lapply(levels(allom$species), function(s) {
  subset_data <- allom[allom$species == s, ]  # Subset data for the current species
  
  lm_model <- lm(height ~ bc , data = subset_data)
    
  r_squared <- summary(lm_model)$r.squared
  p_value <- summary(lm_model)$coefficients[2, 4] 
  num_measurements <- nrow(subset_data)  # Get the number of measurements
  
  plot <- ggplot(subset_data, aes(x = bc, y = height)) +
    geom_point() +  # Add scatter plot
    geom_smooth(method = "lm", se = FALSE) +  
    labs(title = s, subtitle = paste("n:", num_measurements, 
                                     "   R-squared:", round(r_squared, 3), 
                                     "   p-value (bc):", format(p_value, scientific = TRUE, digits = 3)))  # Add species name, number of measurements, R-squared, and p-value as title
  
  return(plot)
})


bc_plots <- do.call(gridExtra::grid.arrange, plots_bc_species)
print(bc_plots)


 #MC vs HEIGHT

plots_mc_species <- lapply(levels(allom$species), function(s) {
  subset_data <- allom[allom$species == s, ]  # Subset data for the current species
  
  lm_model <- lm(height ~ mc , data = subset_data)
  r_squared <- summary(lm_model)$r.squared
  p_value <- summary(lm_model)$coefficients[2, 4] 
  num_measurements <- nrow(subset_data)  # Get the number of measurements
  
  plot <- ggplot(subset_data, aes(x = mc, y = height)) +
    geom_point() +  # Add scatter plot
    geom_smooth(method = "lm", se = FALSE) +  
    labs(title = s, subtitle = paste("n:", num_measurements, 
                                     "   R-squared:", round(r_squared, 3), 
                                     "   p-value (mc):", format(p_value, scientific = TRUE, digits = 3)))  # Add species name, number of measurements, R-squared, and p-value as title
  
  return(plot)
})


mc_plots <- do.call(gridExtra::grid.arrange, plots_mc_species)
print(mc_plots)



library(ggplot2)
library(gridExtra)

