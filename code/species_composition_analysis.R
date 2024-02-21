


library(tidyverse) # manage data
library(DT) # visualise tables in html
library(viridis) # use color-blind friendly palettes
library(ggrepel) # avoid overlapping texts in the figures
library(codyn) # calculate community dynamics metrics
library(vegan) # calculate community ecology metrics
library(eulerr) # calculate Venn and Euler diagrams
library(ggplot2)
library(ggthemes)


source("code/first_script.R")
species_code <- read.csv("data/species_code.csv")

flora$code <- flora$species
flora <- select(flora, date, month, sampling, plot, treatment, code, abundance)

species_code <- select(species_code, species, code, family, genus_level, species_level)
species_code <- species_code %>%
  mutate(across(where(is.character), as.factor))
 
#Flora 0 are all species, even those that we have problem to identify
#flora0 <- merge(flora, species_code, by = "code", all = TRUE)
#View(subset(flora0, is.na(species)))
unique((subset(flora0, is.na(species)))$code)
#terminar de revisar especies aqui

# WE WORK WITH IDENTIFIED SPECIES!!

flora <- merge(flora, species_code, by = "code")
rm(species_code)

flora <- flora[!duplicated(flora), ] # we remove all duplicated coming from biomass measurements 



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
  labs(y = "Richness") +
  theme_bw() + 
  theme(legend.position = "NULL")
ggrichness


# Hacer otro día los RADs con ID debajo #####

# TURNOVER ##### package "codyn"

# T = (E + C)/R, where T is the percentage turnover rate, E is the number of taxa that went extinct between two time points,
# C is the number of taxa that colonised the community between the same two time points, and R is the total number of species
# (i.e. the richness of the pool of species conformed by the two samples). 

species_ab$sampling <- as.numeric(species_ab$sampling) # fuction codyn::turnover needs time variable to be numeric
species_ab$sampling <- species_ab$sampling - 1 # when transforming to numeric, it transform factor into their position. And sampling 0 goes into position 1

sp_total_turnover <- species_ab %>% 
  codyn::turnover(time.var = "sampling",
                  species.var = "code",
                  abundance.var = "abundance",
                  metric = "total",# calculates summed appearances and disappearances relative to total species richness across both time periods.
                  replicate.var = "treatment")

sp_appear <- species_ab %>% 
  turnover(time.var = "sampling",
           species.var = "code",
           abundance.var = "abundance",
           metric = "appearance", # Calculates the number of species that appeared in the second 
           # time period relative to total species richness across both time periods
           replicate.var = "treatment")

sp_disappear <- species_ab %>% 
  turnover(time.var = "sampling",
           species.var = "code",
           abundance.var = "abundance",
           metric = "disappearance", # Calculates the number of species that disappeared in the second
           # time period relative to total species richness across both time periods.
           replicate.var = "treatment")


sp_turnover <- left_join(sp_appear, sp_disappear)
sp_turnover <- pivot_longer(sp_turnover, cols = -c(sampling, treatment) , names_to = "metric", values_to = "rate")

ggturnover <- 
  ggplot(sp_turnover, aes(x = sampling, y = rate)) +
  facet_grid(~ treatment) +
  geom_col(aes(fill = metric)) +
  geom_point(data = sp_total_turnover, aes(y = total)) +
  geom_line(data = sp_total_turnover, aes(y = total)) +
  scale_fill_viridis_d(begin = 0.5) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) + 
  ylim(0,1) + 
  labs(title = "Community turnover relative to the preceding sampling",
       x = "Sampling",
       y = "Turnover") +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))+
  theme_bw()

ggturnover


#ABUNDANCE-BASED ANALYSIS. Package "vegan" #####

species_ab_c <- subset(species_ab, treatment == "c")
species_ab_w <- subset(species_ab, treatment == "w")
species_ab_p <- subset(species_ab, treatment == "p")
species_ab_wp <- subset(species_ab, treatment == "wp")


sp_wide_c <- species_ab_c %>%
  pivot_wider(id_cols = c(sampling, treatment),
              names_from = species,
              values_from = abundance,
              values_fill = list(abundance = 0),
              names_sep = " ") %>% 
  column_to_rownames(var = "sampling")

sp_wide_w <- species_ab_w %>%
  pivot_wider(id_cols = c(sampling, treatment),
              names_from = species,
              values_from = abundance,
              values_fill = list(abundance = 0),
              names_sep = " ") %>% 
  column_to_rownames(var = "sampling")

sp_wide_p <- species_ab_p %>%
  pivot_wider(id_cols = c(sampling, treatment),
              names_from = species,
              values_from = abundance,
              values_fill = list(abundance = 0),
              names_sep = " ") %>% 
  column_to_rownames(var = "sampling")

sp_wide_wp <- species_ab_wp %>%
  pivot_wider(id_cols = c(sampling, treatment),
              names_from = species,
              values_from = abundance,
              values_fill = list(abundance = 0),
              names_sep = " ") %>% 
  column_to_rownames(var = "sampling")


#Control, por ejemplo




