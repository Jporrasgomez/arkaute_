


library(tidyverse) # manage data
library(DT) # visualise tables in html
library(viridis) # use color-blind friendly palettes
library(ggrepel) # avoid overlapping texts in the figures
library(codyn) # calculate community dynamics metrics
library(vegan) # calculate community ecology metrics
library(eulerr) # calculate Venn and Euler diagrams
library(ggplot2)


source("code/first_script.R")
species_code <- read.csv("data/species_code.csv")

flora$code <- flora$species
flora <- select(flora, date, month, sampling, plot, treatment, code, abundance)

species_code <- select(species_code, species, code, family, genus_level, species_level)
species_code <- species_code %>%
  mutate(across(where(is.character), as.factor))
 
#Flora 0 are all species, even those that we have problem to identify
flora0 <- merge(flora, species_code, by = "code", all = TRUE)
#View(subset(flora0, is.na(species)))
unique((subset(flora0, is.na(species)))$code)
#terminar de revisar especies aqui

# We are going to work without those species until we know what to do about them

flora <- merge(flora, species_code, by = "code")

flora <- flora[!duplicated(flora), ]



#Calcular, para cada especie, su abundancia media por tratamiento y muestreo 
species_ab <-  summarise(group_by(flora, date, month, code, sampling, treatment, species, family,  genus_level, species_level),
                              abundance = (sum(abundance, na.rm = T)/4)) #mean abundance by treatment and sampling in a square meter (?)
totals_df <- summarise(group_by(species_ab, sampling, treatment), #adding number of species per treatment and sampling to species_ab
                          n_species = n_distinct(code),
                          total_abundance = sum(abundance))
species_ab <- merge(species_ab, totals_df)
  


ggrichness <- 
  ggplot(species_ab, aes(x = date, y = n_species, color = treatment)) +
  geom_line() +
  geom_point(size = 3) +
  geom_smooth(se = F) +
  facet_grid(~ treatment)+
  geom_text(aes(label = n_species), nudge_y = 1) + # adding the number of species as a label
  scale_color_manual(values = c("c" = "darkolivegreen4", "p" = "#1C86EE", "w" = "red3", "wp" = "purple3"))+
  geom_vline(aes(xintercept = 1.5 , color = "red4"), linetype = "dashed") + # indicate perturbation
  guides(color = guide_legend(title = NULL))+
  theme(legend.position = "NULL")+
  labs(y = "richness")
ggrichness


# Hacer otro día los RADs con ID debajo #####

# TURNOVER #####

# T = (E + C)/R, where T is the percentage turnover rate, E is the number of taxa that went extinct between two time points,
# C is the number of taxa that colonised the community between the same two time points, and R is the total number of species
# (i.e. the richness of the pool of species conformed by the two samples). 

species_ab$sampling <- as.numeric(species_ab$sampling -1) # fuction codyn::turnover needs time variable to be numeric
species_turnover <- species_ab %>% 
  codyn::turnover(time.var = "sampling",
                  species.var = "code",
                  abundance.var = "abundance",
                  replicate.var = "treatment")



ggplot(species_turnover, aes(x = sampling, y = total, color = treatment)) +
  geom_point() + geom_line() +
  scale_color_viridis_d(direction = -1) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) + 
  facet_grid(~ treatment)+
  scale_color_manual(values = c("c" = "darkolivegreen4", "p" = "#1C86EE", "w" = "red3", "wp" = "purple3"))+
  labs(title = "Community turnover relative to the preceding sample",
       x = "Year",
       y = "Turnover") +
  ylim(0, 1) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1), legend.position = "NULL")
  



sp_cols <- species_ab %>% 

  turnover(time.var = "year",
           species.var = "code",
           abundance.var = "total_abundance",
           metric = "appearance")




