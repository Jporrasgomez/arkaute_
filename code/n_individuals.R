
rm(list = ls(all.names = TRUE))
library(dplyr)
library(ggplot2)
library(broom)


nind <- read.csv("data/n_individuals.csv")
source("code/first_script_old.R")

nind$code <- as.factor(nind$code)

nind <- select(nind, sampling, plot, code, nind_m2, abundance)
flora_nind <-  flora %>% select(code, family) %>% distinct(code, .keep_all = TRUE)

nind <- nind %>%
  group_by(sampling, plot, code) %>%
  summarize(nind_m2 = sum(nind_m2), abundance = sum(abundance))  ## Las especies de asteraceaes que hemos agrupado por familia se recalculan sus individuos y abundancias aquí

#PODEMOS AÑADIR MÁS INFORMACIÓN DE NUMERO DE INDIVIDUOS!!!

flora_raw <- read.csv("data/flora_db_raw.csv")
flora_raw <- select(flora_raw, -date, -category, -OBS, -ni.agrupado, -familia, -species_old_1, -species_old_2, -height, - Cb, -Cm, -Db, -Dm)

#Durante todo el muestreo, si había más de 5 individuos, no mediamos más. Por lo que, si hay menos de 5 individuos, ese número es el número de individuos por 
# metro 2 que había. Esto se ha hecho siempre, pero realmente nos interesa solo hasta el muestreo 11, ya que a partir del 12 empezamos a contar individuos. 

flora_raw <- flora_raw %>%
  filter(!sampling %in% c("12", "13", "14", "15", "16", "17"))  %>%
  group_by(sampling, plot, code) %>%
  mutate(n_individuals = n()) %>% #number of individuals per species
  ungroup() %>%   
  group_by(sampling, plot, code,abundance) %>%
  summarize(n_individuals_mean = mean(n_individuals, na.rm = T))  #corrección de asteraceae y poaceae #corrección de asteraceae y poaceae

n_individuals_old_data <- flora_raw %>%
  filter(n_individuals_mean < 5)
names(n_individuals_old_data)[names(n_individuals_old_data) == "n_individuals_mean"] <- "nind_m2"

nind <- bind_rows(nind, n_individuals_old_data)

 
#ESTOS DATOS SE USARÁN PARA EL TFM DE CLAUDIA. Es estima, para cada especie, un numero promedio de individuos por metro cuadrado y se multiplicará por la biomasa
#### promedio por individuo. 
nind_mean <- nind %>%
 group_by(code) %>%
 summarize(mean_nind_m2 = mean(nind_m2)) 
species_NAnind <- anti_join(flora_nind, nind_mean) #Especies para las cuales no tenemos datos




### Regresión lineal. 

#Se podría considerar para la tesis ajustar distintos modelos para algunas especies



flora_nind$code <- as.character(flora_nind$code)
nind$code <- as.character(nind$code)

code_levels <- unique(nind$code)
gglist <- list()

nind_lm_data <- matrix(nrow = length(code_levels), ncol = 6)
colnames(nind_lm_data) <- c("code", "intercept", "slope", "r_squared", "p_value", "n_observations")
nind_lm_data <- as.data.frame(nind_lm_data)

counter <- 0

for (i in 1: length(code_levels)) {
  
  nind_i <- subset(nind, code == code_levels[i])
  lm_i <- lm(nind_m2 ~ abundance, data = nind_i)
  
  # Extract coefficients, R^2, and p-value
  lm_i_summary <- summary(lm_i)
  lm_i_tidy <- tidy(lm_i)
  lm_i_glance <- glance(lm_i)
  
  intercept_i <- lm_i_tidy$estimate[1]
  slope_i <- lm_i_tidy$estimate[2]
  r_squared_i <- lm_i_glance$r.squared
  p_value_i <- lm_i_tidy$p.value[2]
  n_observations_i <- nrow(nind_i)
  
  counter <- counter + 1
  
  gglist[[counter]] <- ggplot(nind_i, aes(x = abundance, y = nind_m2)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = paste("Linear Relationship for", code_levels[i]),
         subtitle = paste("Equation: y =", round(intercept_i, 2), "+", round(slope_i, 2), "* x\n",
                          "R^2:", round(r_squared_i, 2), ", p-value:", round(p_value_i, 4), "\n",
         "Number of observations", n_observations_i),
         x = "Abundance",
         y = "Numbers of individual per m2") +
    theme_minimal()
  
  nind_lm_data$code[counter] <- code_levels[i]
  nind_lm_data$intercept[counter] <- intercept_i
  nind_lm_data$slope[counter] <- slope_i
  nind_lm_data$r_squared[counter] <- r_squared_i
  nind_lm_data$p_value[counter] <- p_value_i
  nind_lm_data$n_observations[counter] <- n_observations_i

  
}


print(gglist[1:length(code_levels)])

#Checking species with low info: 

gglist[[which(code_levels == "amsp")]]
gglist[[which(code_levels == "brasiccaceae")]]

#There are some species with negative slope:

gglist[[which(code_levels == "pore")]]
gglist[[which(code_levels == "vear")]]
gglist[[which(code_levels == "cadi")]]
gglist[[which(code_levels == "rucr")]]

#What do we do with these? 

#There are some intercepts that are negative. Lets transform these into 0. 

nind_lm_data <- nind_lm_data %>%
  mutate(slope = ifelse(is.na(slope), 0, slope))

hist(nind_lm_data$slope, 20)
hist(nind_lm_data$r_squared, 20)
hist(nind_lm_data$n_observations, 20)


nind_lm_data %>% write.csv("data/nind_lm_data.csv", row.names = FALSE)


