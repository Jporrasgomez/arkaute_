
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

# Loop design

plots <- unique(datos$plot)
gglist <- list()
count <- 0

for(i in 1:length(plots)){
  
  count <- count + 1
  
  plot_i <- subset(datos, plot == plots[i])
  
 gglist[[count]] <- ggplot(plot1, aes(x = sampling, y = log(biomass), group = code, color = code)) + 
    geom_line() +
   labs( x = "Sampling", y = "log(Biomass)", title = paste("Plot", plot_i$plot, ",", "Treatment", plot_i$treatment, sep = " "))
  
}


