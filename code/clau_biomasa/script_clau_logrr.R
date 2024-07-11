


rm(list = ls(all.names = TRUE))
#Cajas y bigotes biomasa en el tiempo, hacer 1 gráfico por tratamiento###


#Gráficos biomasa en el tiempo, uno por tratamiento####
#GRÁFICOS DEL CRECIMIENTO DE ESPECIES POR PLOT
library(ggplot2)
library(tidyverse)
library(dplyr)

#anotaciones####
# Biomasa está pendiente. Depende del dato que estamos cogiendo en el campo
# Creo que puede molar ver un gráfico en el que veamos todas las especies que tenemos en arkaute y como varian por muestreo en cada tratamiento. 

# Opening data #####
datos <- read.csv("data/flora_db.csv")

datos$plot <- as.factor(datos$plot)
datos$sampling <- as.factor(datos$sampling)
datos$code <- as.factor(datos$code)

datos <- datos %>%
  filter(!sampling %in% c("0", "1", "2", "12"))           ## * He quitado el sampling 12 porque no tenemos datos de biomasa


# Configuración del estílo de los gráficos
theme_set(theme_bw())


#Loop para el total biomass por sampling y plot

plots <- sort(unique(datos$plot))
samps <- sort(unique(datos$sampling)) #mirar si se puede poner el sort
datos$total_biomass <- NA   #El espacio de almacenamiento donde vamos a guardar los resutlados del loop

for (i in 1:length(plots)){
  for(j in 1:length(samps)){
    
    subset_ij <- subset(datos, plot == plots[i] & sampling == samps[j])
    subset_totalb <- sum(subset_ij$biomass, na.rm = T)
    datos[which(datos$plot == plots[i] & datos$sampling == samps[j]), ]$total_biomass <- subset_totalb
    
  }
}



#Checking dynamics of biomass by agregating ALL BIOMASS DATA FROM SPECIES
ggplot(datos, aes(x = sampling, y = biomass, fill = treatment)) + #fill es para agrupar por tratamiento 
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


#Biomasa total
ggplot(datos, aes(x = sampling, y = total_biomass, fill = treatment)) + #fill es para agrupar por tratamiento 
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


# Calcular la media de total_biomasa por treatment y sampling
datos_media <- aggregate(total_biomass ~ treatment + sampling, data = datos, FUN = mean, na.rm = TRUE) ## !! aqui habría que cambiar el nombre de total_biomass por mean_biomass



##loop####   ####################################################################################################### por qué hacer el loop otra vez?
##primero creo el primer loop
#datos <- read.csv("data/flora_db.csv")
#
#datos$plot <- as.factor(datos$plot)
#datos$sampling <- as.factor(datos$sampling)
#datos$code <- as.factor(datos$code)
#
#datos <- datos %>%
#  filter(!sampling %in% c("0", "1", "2"))
#
## Configuración del estílo de los gráficos
#theme_set(theme_bw())
#
#
##Loop para el total biomass por sampling y plot
#
#plots <- sort(unique(datos$plot))
#samps <- sort(unique(datos$sampling))
#datos$total_biomass <- NA   #El espacio de almacenamiento donde vamos a guardar los resutlados del loop
#
#for (i in 1:length(plots)){
#  for(j in 1:length(samps)){
#    
#    subset_ij <- subset(datos, plot == plots[i] & sampling == samps[j])
#    
#    subset_totalb <- summarize(group_by(subset_ij, plot, sampling),
#                               total_biomass = sum(biomass, na.rm = T))
#    
#    datos[which(datos$plot == plots[i] & datos$sampling == samps[j]), ]$total_biomass <- subset_totalb$total_biomass
#    
#  }
#}
#
## Loop design para los ggplots
#
#gglist <- list() #Creas un espacio de almacenamiento vacío para guardar los resultados del loop. 
#count <- 0
#
#for(i in 1:length(plots)){
#  
#  count <- count + 1
#  
#  plot_i <- subset(datos, plot == plots[i])
#  
#  gglist[[count]] <- ggplot(plot_i)+ 
#    geom_line(aes(x = sampling, y = log(biomass), group = code, color = code)) +
#    geom_line(aes(x = sampling, y = log(total_biomass), group = 1), color = "black", size = 0.8, linetype = "dotdash") +
#    labs( x = "Sampling", y = "log(Biomass)", title = paste("Plot", plot_i$plot, ",", "Treatment", plot_i$treatment, sep = " "))
#  
#}

#voy a intentar hacer un loop primero creo un espacio vació para la proporción y luego hago que se dividan entre el c según el sampling
datos_media$media <- NA   #El espacio de almacenamiento donde vamos a guardar los resutlados

# Repetir (loop) sobre cada valor único de 'sampling'
for (s in unique(datos_media$sampling)) {
  # Obtener el total_biomass para el tratamiento 'c' en el sampling actual
  c_biomass <- datos_media$total_biomass[datos_media$sampling == s & datos_media$treatment == "c"]  # **Este dato es el mismo que total_biomassm
  
  # Calcular la proporción para los tratamientos 'p', 'w', 'wp' en el mismo sampling
  datos_media$media[datos_media$sampling == s & datos_media$treatment != "c"] <-                            ### ** Esto en vez de llamarlo media deberias llamarlo response ratio
    datos_media$total_biomass[datos_media$sampling == s & datos_media$treatment != "c"] / c_biomass
}

# Filtrar los tratamientos diferentes de 'c'
result_data <- datos_media[datos_media$treatment != "c", ]

# Mostrar el resultado
print(result_data)
# Cargar paquetes   ##  **No hace falta cargar paquetes si ya están en el script
##library(dplyr)
##library(ggplot2)

# Aplicar logaritmo en base 10 a la columna 'media'

result_data$logmedia <- log10(result_data$media)                 #### ** Y eso log response ratio (logrr) Y utilizamos "log" creo que "log10" es Ln


# Eliminar la columna log10_media de result_data
result_data <- subset(result_data, select = -log10_media)      ### ** Esta línea no funciona



library(ggplot2)
# Definir los colores para cada tratamiento
colores <- c("p" = "skyblue2", "w" = "indianred2", "wp" = "purple")

# Crear el gráfico ggplot con geom_line
ggplot(result_data, aes(x = sampling, y = logmedia, color = treatment, group = treatment)) +
  geom_line() +
  geom_point() +
  labs(title = "Recuperación respecto al control",
       x = "Sampling",
       y = "Biomasa total (log10)",
       color = "Tratamiento") +
  theme_minimal() +
  scale_color_manual(values = colores) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7))

#Ahora para que aparezca el tratamiento c en el gráfico y se entienda el cambio ####
# Calcular la media de total_biomass por treatment y sampling
datos_media <- aggregate(total_biomass ~ treatment + sampling, data = datos, FUN = mean, na.rm = TRUE)

# Crear un espacio vacío para la proporción
datos_media$media <- NA   # El espacio de almacenamiento donde vamos a guardar los resultados

# Repetir (loop) sobre cada valor único de 'sampling'
for (s in unique(datos_media$sampling)) {
  # Obtener el total_biomass para el tratamiento 'c' en el sampling actual
  c_biomass <- datos_media$total_biomass[datos_media$sampling == s & datos_media$treatment == "c"]
  
  # Calcular la proporción para el tratamiento 'c' en el mismo sampling
  datos_media$media[datos_media$sampling == s & datos_media$treatment == "c"] <- 1
  
  # Calcular la proporción para los tratamientos 'p', 'w', 'wp' en el mismo sampling
  datos_media$media[datos_media$sampling == s & datos_media$treatment != "c"] <-
    datos_media$total_biomass[datos_media$sampling == s & datos_media$treatment != "c"] / c_biomass
}

# Mostrar el resultado
print(datos_media)

# Cargar paquetes
library(dplyr)
library(ggplot2)

# Aplicar logaritmo en base 10 a la columna 'media'
datos_media$logmedia <- log10(datos_media$media)
# Definir los colores para cada tratamiento
colores <- c("c" = "darkolivegreen2", "p" = "skyblue2", "w" = "indianred2", "wp" = "purple")

# Crear el gráfico ggplot con geom_line
ggplot(datos_media, aes(x = sampling, y = logmedia, color = treatment, group = treatment)) +
  geom_line() +
  geom_point() +
  labs(title = "Trayectorias de recuperación",
       x = "Muestreo",
       y = "Biomasa total (log10)",
       color = "Tratamiento") +
  theme_minimal() +
  scale_color_manual(values = colores) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

View(datos_media)








