


library(tidyverse)
library(ggpubr)
library(lubridate)
library(stringr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(readr)
library(ggpubr)
library(tidyverse)
library(gridExtra)

flora_raw <- read.csv("data/flora_db.csv")
flora_raw <- flora_raw %>%
  mutate(across(where(is.character), as.factor))
desired_order <- c("s0_may", "s1_may", "s2_jun", "s3_jun", "s4_jul", "s5_jul", "s6_aug", 
                   "s7_sep", "s8_sep", "s9_oct", "s10_oct", "s11_nov")
flora_raw$sampling <- factor(flora_raw$sampling, levels = desired_order)

sampling_dates <- read.csv("data/sampling_dates.csv")
summary(sampling_dates)
str(sampling_dates)
sampling_dates$datenew <-  ymd(sampling_dates$date)
sampling_dates$month <- month(sampling_dates$datenew)
sampling_dates$date <- NULL
sampling_dates$micro.sampling <- NULL
sampling_dates$N.micro <- NULL

sampling_dates <- sampling_dates %>%
  mutate(across(where(is.character), as.factor))
print(sampling_dates)

flora_raw <- right_join(flora_raw, sampling_dates, by = join_by(sampling))

flora <- flora_raw %>% select(sampling, plot, treatment, species, abundance, height, Cb, Db, Cm, Dm, datenew, month)


flora$sampling <- factor(flora$sampling, levels = unique(flora$sampling))
flora$treatment <- factor(flora$treatment, levels = unique(flora$treatment))


#Create a color palette for treatments
treatment_colors <- c("c" = "green", "w" = "red", "p" = "blue", "wp" = "purple")

# Create a bar plot for species composition by sampling and treatment
ggplot(flora, aes(x = sampling, y = abundance, fill = treatment)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~species, scales = "free_y") +
  labs(title = "Species Composition Analysis",
       x = "Sampling",
       y = "Abundance",
       fill = "Treatment") +
  scale_fill_manual(values = treatment_colors) +  # Set custom colors
  theme_minimal()

library(vegan)


# Subset the data for the specific sampling
sampling_data <- subset(flora, sampling == "s9_oct")

sampling_data <- summarise(group_by(sampling_data, sampling, treatment, species,),
                                    abundance = mean(abundance, na.rm = T))

# Create a numeric matrix for species abundance data
abundance_matrix <- reshape2::dcast(sampling_data, species ~ treatment, value.var = "abundance", fill = 0)

# Create a distance matrix based on species composition
dist_matrix <- vegan::vegdist(abundance_matrix[, -1])  # Exclude the species column

# Perform NMDS
nmds_result <- vegan::metaMDS(dist_matrix)

# Extract NMDS coordinates
nmds_coords <- as.data.frame(scores(nmds_result))

# Add row numbers as identifiers
nmds_coords$row_number <- seq_len(nrow(nmds_coords))
sampling_data$row_number <- seq_len(nrow(sampling_data))

# Update treatment variable based on information from sampling_data
merged_data$treatment <- sampling_data$treatment[match(merged_data$species, sampling_data$species)]

# Create a ggplot scatter plot
ggplot(merged_data, aes(x = NMDS1, y = NMDS2, color = treatment)) +
  geom_point(size = 3) +
  geom_text(aes(label = species), hjust = 0, vjust = 0) +
  labs(title = "NMDS Plot for Species Composition at sampling 10 (October)",
       x = "NMDS1",
       y = "NMDS2",
       color = "Treatment") +
  scale_color_manual(values = treatment_colors) +  # Set custom colors
  theme_minimal()
#
