# to do's
# quitar flechas de los plots
# Elegimos Bray-curtis porque funciona mejor con los "dobles 0s". Esto es, cuando una especie no está presente 
# entre tratamientos


library(tidyverse) # manage data
library(DT) # visualise tables in html
library(viridis) # use color-blind friendly palettes
library(ggrepel) # avoid overlapping texts in the figures
library(codyn) # calculate community dynamics metrics
library(vegan) # calculate community ecology metrics
library(eulerr) # calculate Venn and Euler diagrams
library(ggplot2)
library(ggthemes)
library(ggpubr)

theme_set(theme_bw()+ theme(legend.position = "NULL"))


source("code/first_script.R")



#Calcular, para cada especie, su abundancia media por tratamiento y muestreo 
species_ab <-  summarise(group_by(flora, date, month, code, sampling, treatment, species, family,  genus_level, species_level),
                              abundance = (sum(abundance, na.rm = T)/4)) #mean abundance by treatment and sampling in a square meter (?)
totals_df <- summarise(group_by(species_ab, sampling, treatment), #adding number of species per treatment and sampling to species_ab
                          n_species = n_distinct(code),
                          total_abundance = sum(abundance))
species_ab <- merge(species_ab, totals_df)
  


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




#ABUNDANCE-BASED ANALYSIS. Package "vegan" #####

species_ab_c <- subset(species_ab, treatment == "c")
species_ab_w <- subset(species_ab, treatment == "w")
species_ab_p <- subset(species_ab, treatment == "p")
species_ab_wp <- subset(species_ab, treatment == "wp")



sp_wide_c <- species_ab_c %>%
  pivot_wider(id_cols = sampling,
              names_from = code,
              values_from = abundance,
              values_fill = list(abundance = 0)) %>% 
  column_to_rownames(var = "sampling") %>% 
  arrange(as.numeric(rownames(.)))

sp_wide_w <- species_ab_w %>%
  pivot_wider(id_cols = sampling,
              names_from = code,
              values_from = abundance,
              values_fill = list(abundance = 0)) %>% 
  column_to_rownames(var = "sampling") %>% 
  arrange(as.numeric(rownames(.)))

sp_wide_p <- species_ab_p %>%
  pivot_wider(id_cols = sampling,
              names_from = code,
              values_from = abundance,
              values_fill = list(abundance = 0)) %>% 
  column_to_rownames(var = "sampling") %>% 
  arrange(as.numeric(rownames(.)))

sp_wide_wp <- species_ab_wp %>%
  pivot_wider(id_cols = sampling,
              names_from = code,
              values_from = abundance,
              values_fill = list(abundance = 0)) %>% 
  column_to_rownames(var = "sampling") %>% 
  arrange(as.numeric(rownames(.)))


#HELLINGER

pcoa_hell_c <- sp_wide_c %>% 
  na.omit() %>% 
  vegan::vegdist(method = "hellinger") %>%  # we use Hellinger because it works better with double 0's
  cmdscale(eig = T) 
var_exp_hell_c <- pcoa_hell_c$eig[1:2]/sum(pcoa_hell_c$eig[pcoa_hell_c$eig > 0])

pcoa_samplings_hell_c <- pcoa_hell_c$points %>% 
  as.data.frame() 
pcoa_species_hell_c <- cor(sp_wide_c, pcoa_samplings_hell_c) %>% 
  as.data.frame()


pcoa_hell_w <- sp_wide_w %>% 
  na.omit() %>% 
  vegan::vegdist(method = "hellinger") %>%
  cmdscale(eig = T) 
var_exp_hell_w <- pcoa_hell_w$eig[1:2]/sum(pcoa_hell_w$eig[pcoa_hell_w$eig > 0])
pcoa_samplings_hell_w <- pcoa_hell_w$points %>% 
  as.data.frame() 
pcoa_species_hell_w <- cor(sp_wide_w, pcoa_samplings_hell_w) %>% 
  as.data.frame()

pcoa_hell_p <- sp_wide_p %>% 
  na.omit() %>% 
  vegan::vegdist(method = "hellinger") %>%
  cmdscale(eig = T) 
var_exp_hell_p <- pcoa_hell_p$eig[1:2]/sum(pcoa_hell_p$eig[pcoa_hell_p$eig > 0])
pcoa_samplings_hell_p <- pcoa_hell_p$points %>% 
  as.data.frame() 
pcoa_species_hell_p <- cor(sp_wide_p, pcoa_samplings_hell_p) %>% 
  as.data.frame()

pcoa_hell_wp <- sp_wide_wp %>% 
  na.omit() %>% 
  vegan::vegdist(method = "hellinger") %>%
  cmdscale(eig = T) 
var_exp_hell_wp <- pcoa_hell_wp$eig[1:2]/sum(pcoa_hell_wp$eig[pcoa_hell_wp$eig > 0])
pcoa_samplings_hell_wp <- pcoa_hell_wp$points %>% 
  as.data.frame() 
pcoa_species_hell_wp <- cor(sp_wide_wp, pcoa_samplings_hell_wp) %>% 
  as.data.frame()


gg_hell_c <-
  ggplot() +
  #geom_segment(data = pcoa_species_hell_c %>% 
                # rownames_to_column(var = "sp"),
               #aes(x = 0, y = 0, xend = V1, yend = V2),
               #color = "grey",
               #arrow = arrow()) +
  geom_text_repel(data = pcoa_species_hell_c %>% 
                    rownames_to_column(var = "sp"),
                  aes(x = V1, y = V2, label = sp),
                  color = "grey",
                  max.overlaps = 30) +
  geom_point(data = pcoa_samplings_hell_c %>% 
               rownames_to_column(var = "sampling"),
             aes(x = V1, y = V2),
             size = 2) +
  geom_text_repel(data = pcoa_samplings_hell_c %>% 
                    rownames_to_column(var = "sampling"),
                  aes(x = V1, y = V2, label = sampling),
                  max.overlaps = 13) +
  geom_path(data = pcoa_samplings_hell_c %>% 
              rownames_to_column(var = "sampling"),
            aes(x = V1, y = V2)) +
  geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = 0), color = "red", linetype = "dashed") +
  labs(title = "PCoA using Hellinger distance: control",
       subtitle = paste0("Variance explained = ", round(sum(var_exp_hell_c)*100), "%"),
       x = paste0(round(var_exp_hell_c[1]*100), "% var"),
       y = paste0(round(var_exp_hell_c[2]*100), "% var"))


gg_hell_w <-
  ggplot() +
  #geom_segment(data = pcoa_species_hell_c %>% 
  # rownames_to_column(var = "sp"),
  #aes(x = 0, y = 0, xend = V1, yend = V2),
  #color = "grey",
  #arrow = arrow()) +
  geom_text_repel(data = pcoa_species_hell_w %>% 
                    rownames_to_column(var = "sp"),
                  aes(x = V1, y = V2, label = sp),
                  color = "grey",
                  max.overlaps = 30) +
  geom_point(data = pcoa_samplings_hell_w %>% 
               rownames_to_column(var = "sampling"),
             aes(x = V1, y = V2),
             size = 2) +
  geom_text_repel(data = pcoa_samplings_hell_w %>% 
                    rownames_to_column(var = "sampling"),
                  aes(x = V1, y = V2, label = sampling),
                  max.overlaps = 13) +
  geom_path(data = pcoa_samplings_hell_w %>% 
              rownames_to_column(var = "sampling"),
            aes(x = V1, y = V2)) +
  geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = 0), color = "red", linetype = "dashed") +
  labs(title = "PCoA using Hellinger distance: warming",
       subtitle = paste0("Variance explained = ", round(sum(var_exp_hell_w)*100), "%"),
       x = paste0(round(var_exp_hell_w[1]*100), "% var"),
       y = paste0(round(var_exp_hell_w[2]*100), "% var"))




gg_hell_p <-
  ggplot() +
  #geom_segment(data = pcoa_species_hell_c %>% 
  # rownames_to_column(var = "sp"),
  #aes(x = 0, y = 0, xend = V1, yend = V2),
  #color = "grey",
  #arrow = arrow()) +
  geom_text_repel(data = pcoa_species_hell_p %>% 
                    rownames_to_column(var = "sp"),
                  aes(x = V1, y = V2, label = sp),
                  color = "grey",
                  max.overlaps = 30) +
  geom_point(data = pcoa_samplings_hell_p %>% 
               rownames_to_column(var = "sampling"),
             aes(x = V1, y = V2),
             size = 2) +
  geom_text_repel(data = pcoa_samplings_hell_p %>% 
                    rownames_to_column(var = "sampling"),
                  aes(x = V1, y = V2, label = sampling),
                  max.overlaps = 13) +
  geom_path(data = pcoa_samplings_hell_p %>% 
              rownames_to_column(var = "sampling"),
            aes(x = V1, y = V2)) +
  geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = 0), color = "red", linetype = "dashed") +
  labs(title = "PCoA using Hellinger distance: perturbation",
       subtitle = paste0("Variance explained = ", round(sum(var_exp_hell_p)*100), "%"),
       x = paste0(round(var_exp_hell_p[1]*100), "% var"),
       y = paste0(round(var_exp_hell_p[2]*100), "% var"))


gg_hell_wp <-
  ggplot() +
  #geom_segment(data = pcoa_species_hell_c %>% 
  # rownames_to_column(var = "sp"),
  #aes(x = 0, y = 0, xend = V1, yend = V2),
  #color = "grey",
  #arrow = arrow()) +
  geom_text_repel(data = pcoa_species_hell_wp %>% 
                    rownames_to_column(var = "sp"),
                  aes(x = V1, y = V2, label = sp),
                  color = "grey",
                  max.overlaps = 30) +
  geom_point(data = pcoa_samplings_hell_wp %>% 
               rownames_to_column(var = "sampling"),
             aes(x = V1, y = V2),
             size = 2) +
  geom_text_repel(data = pcoa_samplings_hell_wp %>% 
                    rownames_to_column(var = "sampling"),
                  aes(x = V1, y = V2, label = sampling),
                  max.overlaps = 13) +
  geom_path(data = pcoa_samplings_hell_wp %>% 
              rownames_to_column(var = "sampling"),
            aes(x = V1, y = V2)) +
  geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = 0), color = "red", linetype = "dashed") +
  labs(title = "PCoA using Hellinger distance: warming&perturbation",
       subtitle = paste0("Variance explained = ", round(sum(var_exp_hell_wp)*100), "%"),
       x = paste0(round(var_exp_hell_wp[1]*100), "% var"),
       y = paste0(round(var_exp_hell_wp[2]*100), "% var"))


ggpcoa_hell <- ggarrange(gg_hell_c, gg_hell_w, gg_hell_p, gg_hell_wp, 
                         ncol = 2, nrow = 2)




# ALL TREATMENTS IN 1 PLOT ##########

sp_wide <- species_ab %>%
  pivot_wider(id_cols = c(sampling, treatment),
              names_from = code,
              values_from = abundance,
              values_fill = list(abundance = 0))

# create a distance matrix using Hellinger distances
abundance_data <- sp_wide %>% select(-treatment, -sampling)
distance_matrix <- abundance_data %>% 
  na.omit() %>% 
  vegan::vegdist(method = "hellinger") 
pcoa_result <- cmdscale(distance_matrix)

pcoa_hell <- cmdscale(distance_matrix, eig = T)
var_exp_hell <- pcoa_hell$eig[1:2]/sum(pcoa_hell$eig[pcoa_hell$eig > 0])

# plot the PCoA using ggplot


pcoa_df <- data.frame(
  PC1 = pcoa_result[, 1],
  PC2 = pcoa_result[, 2],
  treatment = sp_wide$treatment, #Cómo sabe R donde meter los niveles?
  sampling = sp_wide$sampling #Cómo sabe R donde meter los niveles?
)



#ggpcoa_hell_alltreatments<- 
  
ggplot(pcoa_df, aes(x = PC1, y = PC2, color = treatment)) +
  geom_point(size = 1.5) +
  geom_text_repel(aes(x = PC1, y = PC2, label = sampling), max.overlaps = 100) +
  #geom_path()+ #no  funciona
  geom_hline(aes(yintercept = 0), color = "red2", linetype = "dashed") +
  geom_vline(aes(xintercept = 0), color = "red2", linetype = "dashed") +
  scale_color_manual(values = c("c" = "darkolivegreen4", "p" = "#1C86EE", "w" = "red3", "wp" = "purple"))+
  theme_minimal() +
  labs(title = "PCoA using Hellinger distance",
       subtitle = paste0("Variance explained = ", round(sum(var_exp_hell)*100), "%"),
       x = paste0(round(var_exp_hell[1]*100), "% var"),
       y = paste0(round(var_exp_hell[2]*100), "% var"))+
  theme(legend.position = "bottom")




#A PCoA per SAMPLING

spab0 <- subset(flora, sampling == "0")

spab0_wide<- spab0 %>%
  pivot_wider(id_cols = c(plot, treatment),
              names_from = code,
              values_from = abundance,
              values_fill = list(abundance = 0))# o no.. esto no se puede hacer porque hay especies para el mismo plot y muestreo
# con varios datos de abundancia. Son errores de trasncripción de datos... :( vamos a tener que revisar todos estos errores. 


#Plots : 

ggturnover
ggpcoa_hell
ggpcoa_hell_alltreatments



















