


library(tidyverse) # manage data
library(DT) # visualise tables in html
library(viridis) # use color-blind friendly palettes
library(ggrepel) # avoid overlapping texts in the figures
library(codyn) # calculate community dynamics metrics
library(vegan) # calculate community ecology metrics
library(eulerr) # calculate Venn and Euler diagrams


source("code/first_script.R")
species_code <- read.csv("data/species_code.csv")


theme_set(theme_minimal()+
            theme(axis.title = element_text(size = 15),
                  axis.line = element_line(color = "black",
                                           linewidth = 0.5),
                  panel.grid.major = element_blank(),
                  axis.text = element_text(color = "black", size = 12),
                  strip.text.x = element_text(size = 12),
                  axis.ticks = element_line(color = "black")))


flora$code <- flora$species
flora <- select(flora, sampling, plot, treatment, code, abundance)

species_code <- select(species_code, species, code, family, genus_level, species_level)
species_code <- species_code %>%
  mutate(across(where(is.character), as.factor))
 
#Flora 0 are all species, even those that we have problem to identify
flora0 <- merge(flora, species_code, by = "code", all = TRUE)
View(subset(flora0, is.na(species)))
unique((subset(flora0, is.na(species)))$code)
#terminar de revisar especies aqui

# We are going to work without those species until we know what to do about them

flora <- merge(flora, species_code, by = "code")
anyNA(flora)


flora <- flora %>%
  group_by(sampling, datenew, month, treatment, plot, abundance, species) %>%
  reframe(biomass = mean(biomass, na.rm = TRUE) * abundance) %>%
  distinct(sampling, datenew, month, plot, treatment, abundance, species, biomass)


#Calcular abundancia total para cada especie en cada muestreo y tratamiento
flora_samplings <-  flora %>%
  group_by(sampling, datenew, month, treatment, plot, species,  genus_level) %>%
  reframe(biomass = sum(biomass, na.rm = T), 
          n_species = n_species, 
          abundance = sum(abundance, na.rm = T)) %>%
  distinct(sampling, datenew, month, plot, treatment, biomass, n_species, abundance)




ants_long %>% 
  group_by(year) %>% 
  summarise(n_species = n_distinct(code)) %>% # count the number of different species by year
  ggplot(aes(x = year, y = n_species)) +
  geom_line() +
  geom_point(size = 3) +
  geom_smooth(se = F) +
  #making the plot fancier:
  geom_text(aes(label = n_species), nudge_y = 1) + # adding the number of species as a label
  geom_vline(aes(xintercept = 2005, color = "logging"), linetype = "dashed") + # indicate logging year
  guides(color = guide_legend(title = NULL)) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2)) +
  # a sequence taking the limitis of the axis
  labs(y = "richness")


