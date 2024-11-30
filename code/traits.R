
library(dplyr)

traits <- read.csv("data/mean_traits_species.csv")
species_code <- read.csv("data/species_code.csv")

traits <- merge(traits, species_code, by = "species") 
str(traits)
summary(traits)
