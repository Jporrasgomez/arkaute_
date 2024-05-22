
#GRÁFICOS DEL CRECIMIENTO DE ESPECIES POR PLOT
library(ggplot2)
library(tidyverse)
library(dplyr)

#anotaciones####
# Hay que añadir una transformación antes de plasmar la biomasa. Suma? Media? Depende del dato que estamos cogiendo en el campo
# Creo que puede molar ver un gráfico en el que veamos todas las especies que tenemos en arkaute y como varian por muestreo en cada tratamiento. 

# Opening data

datos <- read.csv("data/flora_db.csv")
datos$plot <- as.factor(datos$plot)
datos$sampling <- as.factor(datos$sampling)
datos$code <- as.factor(datos$code)

datos <- datos %>%
  filter(!sampling %in% c("0", "1", "2"))




theme_set(theme_bw())

#Aquí faltaría un paso para calcular la biomasa total por especie y plot. Abundancias? Numero de individuos * abundancias?

#Loop para el total biomass por sampling y plot

plots <- unique(datos$plot)
samps <- unique(datos$sampling)
datos$total_biomass <- NA

for (i in 1:length(plots)){
  for(j in 1:length(samps)){
    
    subset <- subset(datos, plot == plots[i] & sampling == samps[j])
    subset_totalb <- summarize(group_by(subset, plot, sampling),
                               total_biomass = sum(biomass, na.rm = T))
    
    datos[which(datos$plot == plots[i] & datos$sampling == samps[j]), ]$total_biomass <- subset_totalb$total_biomass
    
  }
}


# Loop design


gglist <- list()
count <- 0

for(i in 1:length(plots)){
  
  count <- count + 1
  
  plot_i <- subset(datos, plot == plots[i])
  
 gglist[[count]] <- ggplot(plot_i)+ 
    geom_line(aes(x = sampling, y = log(biomass), group = code, color = code)) +
    geom_line(aes(x = sampling, y = log(total_biomass), group = 1), color = "black", size = 0.8, linetype = "dotdash") +
   labs( x = "Sampling", y = "log(Biomass)", title = paste("Plot", plot_i$plot, ",", "Treatment", plot_i$treatment, sep = " "))
  
}


gglist[[1]]
gglist[[2]]
gglist[[3]]
gglist[[4]]
gglist[[5]]
gglist[[6]]
gglist[[7]] #Revisar total biomass de Plot9 
gglist[[8]] 
gglist[[9]]
gglist[[10]]
gglist[[11]] #Revisar total biomass de Plot8
gglist[[12]]
gglist[[13]]
gglist[[14]]
gglist[[15]]
gglist[[16]]




plot1 <- subset(datos, plot == "1")
subsetp1 <- subset(plot1, sampling == "3")
subsetp1_totalb <- summarize(group_by(subsetp1, plot, sampling),
                      total_biomass = sum(biomass, na.rm = T))
subsetp1$total_biomass <- subsetp1_totalb$total_biomass

datos[which(datos$plot == "1" & datos$sampling == "3"), ]$total_biomass <- subsetp1_totalb$total_biomass
  
ggplot(plot1) + 
  geom_line(aes(x = sampling, y = log(biomass), group = code, color = code)) +
  geom_line(aes(x = sampling, y = log(total_biomass), group = 1), color = "black", size = 0.8, linetype = "dotdash" ) +
  labs(x = "Sampling", y = "log(Biomass)")

