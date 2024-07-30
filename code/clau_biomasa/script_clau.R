
#GRÁFICOS DEL CRECIMIENTO DE ESPECIES POR PLOT
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)

#Cosas que hacer####
# Biomasa está pendiente. Depende del dato que estamos cogiendo en el campo
# Poner en orden de mayor a menor biomasa las especies de la leyenda
# Ordenar los plots en el factor "Plot" para que salgan ordenados en la gglist
# Creo que puede molar ver un gráfico en el que veamos todas las especies que tenemos en arkaute y como varian por muestreo en cada tratamiento. 

# Opening data #####

datos <- read.csv("data/flora_db.csv")

datos$plot <- as.factor(datos$plot)
datos$sampling <- as.factor(datos$sampling)
datos$code <- as.factor(datos$code)

datos <- datos %>%
  filter(!sampling %in% c("0", "1", "2", "12"))

# Configuración del estílo de los gráficos
theme_set(theme_bw())


#Loop para el total biomass por sampling y plot

plots <- sort(unique(datos$plot))
samps <- sort(unique(datos$sampling))
datos$total_biomass <- NA   #El espacio de almacenamiento donde vamos a guardar los resutlados del loop

for (i in 1:length(plots)){
  for(j in 1:length(samps)){
    
    subset_ij <- subset(datos, plot == plots[i] & sampling == samps[j])
    subset_totalb <- sum(subset_ij$biomass, na.rm = T)
    datos[which(datos$plot == plots[i] & datos$sampling == samps[j]), ]$total_biomass <- subset_totalb
    
  }
}

# Loop design para los ggplots

gglist <- list() #Creas un espacio de almacenamiento vacío para guardar los resultados del loop. 
count <- 0

for(i in 1:length(plots)){
  
  count <- count + 1
  
  plot_i <- subset(datos, plot == plots[i])
  
 gglist[[count]] <- ggplot(plot_i)+ 
    geom_line(aes(x = sampling, y = log(biomass), group = code, color = code)) +
    geom_line(aes(x = sampling, y = log(total_biomass), group = 1), color = "black", size = 0.8, linetype = "dotdash") +
   labs( x = "Muestreo", y = "log Biomasa", title = paste("Plot", plot_i$plot, ",", "Tratamiento", plot_i$treatment, sep = " ")) +
   
 theme(
   legend.text = element_text(size = 7),   # Ajusta el tamaño del texto de la leyenda
   legend.title = element_text(size = 9)  # Ajusta el tamaño del título de la leyenda
 )
  
}


gglist[[1]]
gglist[[2]]
gglist[[3]]
gglist[[4]]
gglist[[5]]
gglist[[6]]
gglist[[7]] #Revisar total biomass de Plot9 
gglist[[8]] #Revisar la línea rosa para que no aparezca por encima de la biomasa total
gglist[[9]]
gglist[[10]]
gglist[[11]] #Revisar total biomass de Plot8
gglist[[12]]
gglist[[13]]
gglist[[14]]
gglist[[15]]
gglist[[16]]


#para utilizar arrange pero no puedo 
install.packages("vctrs")
packageVersion("vctrs")

install.packages("ggpubr")

install.packages("gridExtra")
library(gridExtra)

#voy a usar grid.arrange que entiendo que hace lo mismo que arrange

grid.arrange( gglist[[1]], gglist[[8]], gglist[[9]], gglist[[16]],
           
           ncol = 2, nrow = 2) #Tratamiento warming 

grid.arrange( gglist[[3]], gglist[[6]], gglist[[10]], gglist[[15]],
              
              ncol = 2, nrow = 2) #Tratamiento perturbación

grid.arrange( gglist[[2]], gglist[[7]], gglist[[11]], gglist[[14]],
              
              ncol = 2, nrow = 2) #Tratamiento control

grid.arrange( gglist[[4]], gglist[[5]], gglist[[12]], gglist[[13]],
              
              ncol = 2, nrow = 2) #Tratamiento térmico y perturbación


## Trayectorias de biomasa por tratamiento 


datos_sampling <- summarise(group_by(datos, treatment, sampling, plot),
                            total_biomass = sum(biomass, na.rm = T))


ggplot(datos_sampling, aes(x = sampling, y = total_biomass, fill = treatment)) + #fill es para agrupar por tratamiento 
  geom_boxplot() +
  facet_wrap(~ treatment) + #un gráfico para cada tratamiento
  labs(title = "Trayectorias de recuperación",
       x = "Sampling", #llamar al eje x 
       y = "Biomasa total", #llamar al eje y 
       fill = "Tratamiento") + # Cambiar el título de la leyenda) +
  theme_minimal() +
  scale_y_log10() +
  scale_fill_manual(values = c("c" = "darkolivegreen2", "w" = "indianred2", "wp" = "purple", "p" = "skyblue2")) +
  theme(plot.title = element_text(size = 10, hjust = 0.5),     # Tamaño del título
        axis.title.x = element_text(size = 8),    # Tamaño del título del eje x
        axis.title.y = element_text(size = 8),    # Tamaño del título del eje y
        axis.text.x = element_text(size = 6),     # Tamaño del texto del eje x
        axis.text.y = element_text(size = 6),
        legend.title = element_text(size = 8),  # Tamaño del título de la leyenda
        legend.text = element_text(size = 6))  # Tamaño del texto de la leyenda# Tamaño del texto del eje y
View(datos_sampling)






#Reponse ratio respecto al control####


#Primero voy a hacer la media del control en un dataframe nuevo (tengo que usar tidyverse pero ya está instalado)

# Crear un nuevo dataframe con la media de total_biomass agrupando por sampling solo para el tratamiento control
datos_control <- datos_sampling %>%
  filter(treatment == "c") %>%
  group_by(sampling) %>%
  summarize(mean_biomass = mean(total_biomass, na.rm = TRUE))

#voy a unir los dos dataframes x la columna sampling con la función merge
combined_data <- merge(datos_sampling, datos_control, by = "sampling")

#ahora hago una nueva columna (biomasa relativa) 
combined_data$relative_biomass <- log(combined_data$total_biomass / combined_data$mean_biomass) ##Aquí falta el log




# creo un nuevo dataframe para filtrar los datos y omitir el control
filtered_data <- combined_data %>%
  filter(treatment != "c")

# gráfico de cajas y bigotes (boxplot)
ggplot(filtered_data, aes(x = sampling, y = relative_biomass, fill = treatment)) +
  geom_boxplot() +
  facet_wrap(~ treatment) +
  labs(title = "Trayectorias de recuperación",
       x = "Muestreo",
       y = "Biomasa relativa total",
       fill = "Tratamiento") +
  theme_minimal() +
  scale_fill_manual(values = c("w" = "indianred2", "wp" = "purple", "p" = "skyblue2")) +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6))
