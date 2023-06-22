

library(lubridate)
library(stringr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(readr)
library(ggpubr)
library(tidyverse)
source('Scripts/basicFun.R')


plots <- read.csv("Data/Plots.csv")
#IMPORTANT! Change the value of the DATE everytime a new set of data is opened.It depends on the day you took the data from the sensors. 
plots$file_code <- paste0("data_", plots$sensor_code, "_2023_04_18_0.csv")

allom <- read.csv("Data/allometric_data.csv")
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
  
  lm_model <- lm(height ~ bc + mc, data = subset_data)
  r_squared <- summary(lm_model)$r.squared
  p_value <- summary(lm_model)$coefficients[2, 4] 
  
  plot <- ggplot(subset_data, aes(x = bc, y = height)) +
    geom_point() +  # Add scatter plot
    geom_smooth(method = "lm", se = FALSE) +  
    labs(title = s, subtitle = paste("R-squared:", round(r_squared, 3), "   p-value (bc):", 
                                     format(p_value, scientific = TRUE, digits = 3)))  # Add species name as title
  
  return(plot)
})

bc_plots <- do.call(gridExtra::grid.arrange, plots_bc_species)
print(bc_plots)


#MC vs HEIGHT

plots_mc_species <- lapply(levels(allom$species), function(s) {
  subset_data <- allom[allom$species == s, ]  # Subset data for the current species
  
  lm_model <- lm(height ~ mc + mc, data = subset_data)
  r_squared <- summary(lm_model)$r.squared
  p_value <- summary(lm_model)$coefficients[2, 4] 
  
  plot <- ggplot(subset_data, aes(x = mc, y = height)) +
    geom_point() +  # Add scatter plot
    geom_smooth(method = "lm", se = FALSE) +  
    labs(title = s, subtitle = paste("R-squared:", round(r_squared, 3), "   p-value (mc):", 
                                     format(p_value, scientific = TRUE, digits = 3)))  # Add species name as title
  
  return(plot)
})

mc_plots <- do.call(gridExtra::grid.arrange, plots_mc_species)
print(mc_plots)




library(ggplot2)
library(gridExtra)

# Specify the species name
species_name <- "avst"  # Replace with the desired species name

# Subset the data for the specific species
subset_data <- allom[allom$species == species_name, ]

# Perform linear regression for mc
lm_model_mc <- lm(height ~ mc, data = subset_data)
r_squared_mc <- summary(lm_model_mc)$r.squared
p_value_mc <- summary(lm_model_mc)$coefficients[2, 4]

# Create plot for mc
plot_mc <- ggplot(subset_data, aes(x = mc, y = height)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(subtitle = paste("R-squared (mc):", round(r_squared_mc, 3), "   p-value (mc):", format(p_value_mc, scientific = TRUE, digits = 3)))

# Perform linear regression for bc
lm_model_bc <- lm(height ~ bc, data = subset_data)
r_squared_bc <- summary(lm_model_bc)$r.squared
p_value_bc <- summary(lm_model_bc)$coefficients[2, 4]

# Create plot for bc
plot_bc <- ggplot(subset_data, aes(x = bc, y = height)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(subtitle = paste("R-squared (bc):", round(r_squared_bc, 3), "   p-value (bc):", format(p_value_bc, scientific = TRUE, digits = 3)))

# Combine both plots under the same title
combined_plot <- grid.arrange(plot_mc, plot_bc, ncol = 1)

# Add title using ggtitle function
combined_plot <- combined_plot + ggtitle(species_name)

# Display the combined plot
print(combined_plot)
