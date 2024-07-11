#GRÁFICOS DEL CRECIMIENTO DE ESPECIES POR PLOT
library(ggplot2)
library(tidyverse)
library(dplyr)

datos <- read.csv("C:/RTFM/data/flora_db.csv")

#anotaciones####
#sherardia armensis, ranunculus uno tiene una n y el otro dos
#Plot 1 ####
#primero voy a filtrar P3
plot1 <- subset(datos, treatment == "w" & plot==1)
#poner sampling como factor 
plot1$sampling <- as.factor(plot1$sampling)

#especies por orden alfabético para que aparezcan así en la leyenda
plot1$species <- factor(plot1$species, levels = sort(unique(plot1$species)))

# Sumar la biomasa por 'sampling' y añadir las filas directamente a `plot2`
plot1 <- plot1 %>%
  group_by(sampling) %>%
  summarise(biomass = sum(biomass), .groups = 'drop') %>%
  mutate(species = "Total") %>%
  bind_rows(plot1, .) %>%
  arrange

# Utilizar subset para quitar las filas donde la columna species tiene valores NA
plot1 <- subset(plot1, !is.na(species))

#primer gráfico para ver las especies que hay y luego ordenarlas a mano (no se poner el total abajo de otra forma)
ggplot(plot1, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "P1", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes
#orden 
orden_c1 <- c(
  "Aphanes arvensis",
  "Avena sterilis",
  "Bellis perennis",
  "Bromus hordeaceus",
  "Bromus sterilis",
  "Carex divulsa",
  "Cerastium glomeratum",
  "Crepis capillaris",
  "Cynosurus echinatus",
  "Dactylis glomerata",
  "Festuca arundinacea",
  "Geranium dissectum",
  "Hypochaeris radicata",
  "Medicago arabica",
  "Medicago lupulina",
  "Plantago lanceolata",
  "Poa annua",
  "Ranunculus parviflorus",
  "Sherardia arvensis",
  "Torilis sp",
  "Trifolium dubium",
  "Vicia sativa",  
  "Total"
)
#colores 
colores_p1 <- c(
  "Aphanes arvensis" = "#98F5FF",      # Tomato
  "Avena sterilis" = "#0000FF",        # Repetido color de Aphanes arvensis (Tomato)
  "Bellis perennis" = "#A020F0",       # Violetred3
  "Bromus hordeaceus" = "maroon1",     # Hotpink
  "Bromus sterilis" = "violetred",       # Mediumpurple
  "Carex divulsa" = "pink",         # Repetido color de Aphanes arvensis (Tomato)
  "Cerastium glomeratum" = "tomato",  # Purple4
  "Crepis capillaris" = "red3",     # Repetido color de Aphanes arvensis (Tomato)
  "Cynosurus echinatus" = "darkgoldenrod",   # Orchid
  "Dactylis glomerata" = "#FFB90F",    # Similar a Light Blue
  "Festuca arundinacea" = "darkorange",   # Repetido color de Aphanes arvensis (Tomato)
  "Geranium dissectum" = "salmon1",    # Aquamarine3
  "Hypochaeris radicata" = "sienna",  # Similar a Darkcyan
  "Medicago arabica" = "yellow2",      # Similar a Mediumseagreen
  "Medicago lupulina" = "seagreen",     # Repetido color de Aphanes arvensis (Tomato)
  "Plantago lanceolata" = "green4",   # Similar a Lightseagreen
  "Poa annua" = "green2",      # Yellow2
  "Ranunculus parviflorus" = "olivedrab3",# Pumpkin
  "Sherardia arvensis" = "palegreen",    # Lightsalmon
  "Torilis sp" = "turquoise",         # Orange
  "Trifolium dubium" = "royalblue",      # Tan3
  "Vicia sativa" = "skyblue" , 
  "Total" = "black"

)
# Asegurarse de que 'species' es un factor con el orden deseado
plot1$species <- factor(plot1$species, levels = orden_c1)
#gráfico 
ggplot(plot1, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "P1", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  scale_color_manual(values = colores_p1) + #para poner los colores que he elegido
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes


#Plot 2####
#primero voy a filtrar C2
plot2 <- subset(datos, treatment == "c" & plot==2)

#para transformarlo en unos datos con menos columnas 
plot2 <- plot2[, c("sampling", "species", "code", "biomass")]

#ordenarlos según sampling 
plot2 <- plot2[order(plot2$sampling), ]

#poner sampling como factor 
plot2$sampling <- as.factor(plot2$sampling)


# Asegurarte de que 'species' es un factor y establecer los niveles en orden alfabético
plot2$species <- factor(plot2$species, levels = sort(unique(plot2$species)))

# Sumar la biomasa por 'sampling' y añadir las filas directamente a `plot2`
plot2 <- plot2 %>%
  group_by(sampling) %>%
  summarise(biomass = sum(biomass), .groups = 'drop') %>%
  mutate(species = "Total") %>%
  bind_rows(plot2, .) %>%
  arrange(sampling)

# Asegurarse de que 'species' esté en el orden correcto con 'Total' al final
plot2$species <- factor(plot2$species, levels = orden_c2)

# Utilizar subset para quitar las filas donde la columna species tiene valores NA
plot2 <- subset(plot2, !is.na(species))

#poner el eje y en escala logarítmica para que los cambios se vean mejor
#sino Dactylis tiene un porcentaje mucho mayor y hace que se vea peor la diferencia con las demás

ggplot(plot2, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "C2", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  theme(plot.title = element_text(hjust = 0.1, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes

# Volver a hacer el gráfico 

ggplot(plot2, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "C2", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  theme(plot.title = element_text(hjust = 0.6, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes

#ahora querría que estuviese Total al final y que estuviera en negro para distinguirse (slo se hacerlo a mano)

#personalizar
#1 el orden d las especies
orden_c2 <- c("Aphanes arvensis", "Bellis perennis","Bromus hordaceus", "Bromus sterelis", "Cerastium glomeratum","Cynosorum echinatus", 
                  "Dactylis glomerata", "Gaudian fragilis", "Geranium dissectum", "Geranium molle", "Hordeum murinum",
                    "Hypocaeris radicata","Linum bienne", "Medicago arabica", "Plantago lanceolata", "Poa annua", "Ranunculus bulbosus", "Ranunculus parviflorus",
                    "Sherardia arvensis", "Torilis", "Trifolium dubium", "Trifolium fragiferum", "Trifolium repens", "Vicia sativa", "Total")
#2 los colores para que el total quede negro 
# Vector de colores existente, corregido con algunos nombres y colores adecuados
colores <- c(
  "Aphanes arvensis" = "#FF6347",      # Tomate
  "Bellis perennis" = "#CD3278",       # Violetred3
  "Bromus hordaceus" = "#FF69B4",      # Hotpink
  "Bromus sterelis" = "#9b59b6",       # Mediumpurple
  "Cerastium glomeratum" = "#551A8B",  # Purple4
  "Cynosurus echinatus" = "#DA70D6",   # Orchid
  "Dactylis glomerata" = "#3498db",    # Similar a Light Blue
  "Gaudinia fragilis" = "#00008B",     # Darkblue
  "Geranium dissectum" = "#66CDAA",    # Aquamarine3
  "Geranium molle" = "cyan2",        # Cyan
  "Hordeum murinum" = "#2E8B57",       # Seagreen
  "Hypocaeris radicata" = "#1ABC9C",   # Similar a Darkcyan
  "Linum bienne" = "green2",          # Darkseagreen2
  "Medicago arabica" = "#27AE60",      # Similar a Mediumseagreen
  "Plantago lanceolata" = "#16A085",   # Similar a Lightseagreen
  "Poa annua" = "darkgoldenrod1",             # Yellow2
  "Ranunculus bulbosus" = "#FFD700",   # Gold
  "Ranunculus parviflorus" = "#D35400",# Pumpkin
  "Sherardia arvensis" = "#FFA07A",    # Lightsalmon
  "Torilis" = "darkorange",               # Orange
  "Trifolium dubium" = "#CD853F",      # Tan3
  "Trifolium fragiferum" = "#DEB887",  # Burlywood
  "Trifolium repens" = "#8B4513",      # Chocolate4
  "Vicia sativa" = "#FF6347",          # Tomato
  "Total" = "black"                  # Black
)
#gráfico 
ggplot(plot2, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "C2", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  scale_color_manual(values = colores) + #para poner los colores que he elegido
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes


library(ggplot2)

# Gráfico con ggplot2
ggplot(plot2, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  scale_color_manual(values = colores) +  # Aplicar la paleta de colores personalizada
  labs(title = "C2", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes

#Plot 3####
library(tidyverse)
#primero voy a filtrar P3
plot3 <- subset(datos, treatment == "p" & plot==3)
#poner sampling como factor 
plot3$sampling <- as.factor(plot3$sampling)

#especies por orden alfabético para que aparezcan así en la leyenda
plot3$species <- factor(plot3$species, levels = sort(unique(plot3$species)))

# Crear el gráfico
ggplot(plot3, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "P3", x = "Sampling", y = "Biomasa", color = "Especies") +  
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes

#poner el eje y en escala logarítmica para que los cambios se vean mejor

ggplot(plot3, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "P3", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes

# Sumar la biomasa por 'sampling' y añadir las filas directamente a `plot2`
plot3 <- plot3 %>%
  group_by(sampling) %>%
  summarise(biomass = sum(biomass), .groups = 'drop') %>%
  mutate(species = "Total") %>%
  bind_rows(plot3, .) %>%
  arrange(sampling)

# Volver a hacer el gráfico 

ggplot(plot3, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "P3", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes

orden_p3 <- c("Aphanes arvensis", "Bellis perennis", "Bromus hordaceus", "Bromus sterelis", 
              "Chenopodium sp", "Convolvulus arvensis", "Dactylis glomerata", "Elymus repens", "Festuca arundinacea",
              "Geranium dissectum", "Hypochaeris radicata", "Kickxia spuria", "Lysimachia arvensis", 
              "Medicago arabica",  "Medicago lupulina","Plantago lanceolata", "Poa annua",  "Potentilla reptans","Ranunculus parviflorus", 
               "Sherardia arvensis", "Torilis",  
              "Trifolium fragiferum", "Verbena officinalis",  "Veronica persica","Vicia sativa", "Total")
# Definir el vector de colores para las especies
colores3 <- c(
  "Aphanes arvensis" = "#FF6347",      # Tomate
  "Bellis perennis" = "#CD3278",       # Violetred3
  "Bromus hordaceus" = "#FF69B4",      # Hotpink
  "Bromus sterelis" = "#9B59B6",       # Mediumpurple
  "Chenopodium sp" = "#551A8B",        # Purple4 (asigné un color basado en tu sugerencia de color para Cerastium glomeratum)
  "Convolvulus arvensis" = "#DA70D6",  # Orchid (color cambiado para ajustar a la lista)
  "Dactylis glomerata" = "#3498DB",    # Similar a Light Blue
  "Elymus repens" = "#2E8B57",         # Seagreen (color cambiado para ajustar a la lista)
  "Festuca arundinacea" = "#1ABC9C",   # Similar a Darkcyan (color cambiado para ajustar a la lista)
  "Geranium dissectum" = "blue",    # Aquamarine3
  "Hypochaeris radicata" = "#27AE60",  # Similar a Mediumseagreen (color cambiado para ajustar a la lista)
  "Kickxia spuria" = "#16A085",        # Similar a Lightseagreen (color nuevo agregado)
  "Lysimachia arvensis" = "cyan2",     # Cyan
  "Medicago arabica" = "red2",      # Similar a Mediumseagreen
  "Medicago lupulina" = "green2",      # Green (color nuevo agregado)
  "Plantago lanceolata" = "#16A085",   # Similar a Lightseagreen
  "Poa annua" = "darkgoldenrod1",      # Darkgoldenrod1
  "Potentilla reptans" = "#FFD700",    # Gold (color nuevo agregado)
  "Ranunculus parviflorus" = "#D35400",# Pumpkin
  "Sherardia arvensis" = "#FFA07A",    # Lightsalmon
  "Torilis" = "darkorange",            # Darkorange
  "Trifolium fragiferum" = "#DEB887",  # Burlywood
  "Verbena officinalis" = "#CD853F",   # Tan3 (color nuevo agregado)
  "Veronica persica" = "#8B4513",      # Chocolate4 (color nuevo agregado)
  "Vicia sativa" = "#FF6347",          # Tomato
  "Total" = "#000000"                  # Black
)

# Gráfico con ggplot2
ggplot(plot3, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  scale_color_manual(values = colores3) +  # Aplicar la paleta de colores personalizada
  labs(title = "P3", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes

# Utilizar subset para quitar las filas donde la columna species tiene valores NA
plot3 <- subset(plot3, !is.na(species))

#library(ggplot2)

# Asegúrate de que 'plot3' es tu dataframe
# Ajustar el factor de la columna 'species' con los niveles en el orden deseado
plot3$species <- factor(plot3$species, levels = orden_p3)

# Utilizar subset para quitar las filas donde la columna species tiene valores NA
plot3 <- subset(plot3, !is.na(species))

# Creación del gráfico
ggplot(plot3, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  scale_color_manual(values = colores3) +  # Aplicar la paleta de colores personalizada
  labs(title = "P3", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes

#Plot 4####
library(tidyverse)
#primero voy a filtrar P3
plot4 <- subset(datos, treatment == "wp" & plot==4)
#poner sampling como factor 
plot4$sampling <- as.factor(plot4$sampling)

#especies por orden alfabético para que aparezcan así en la leyenda
plot4$species <- factor(plot4$species, levels = sort(unique(plot4$species)))

# Sumar la biomasa por 'sampling' y añadir las filas directamente a `plot2`
plot4 <- plot4 %>%
  group_by(sampling) %>%
  summarise(biomass = sum(biomass), .groups = 'drop') %>%
  mutate(species = "Total") %>%
  bind_rows(plot4, .) %>%
  arrange

# Utilizar subset para quitar las filas donde la columna species tiene valores NA
plot4 <- subset(plot4, !is.na(species))


# Volver a hacer el gráfico 

ggplot(plot4, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "WP4", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes

#ahora ordenarlas y poner total al final 
orden_p4 <- c("Avena sterilis", "Bellis perennis", "Bromus hordaceus", "Bromus sterelis", 
              "Capsella bursa-pastoris","Carex divulsa", "Chenopodium sp", "Dactylis glomerata", "Elymus repens", "Festuca arundinacea",
              "Gaudina fragilis","Geranium dissectum", "Lysmachia arvensis",  
              "Medicago arabica",  "Mercurialis annua","Plantago lanceolata", "Ranunculus bulbosus", "Ranunculus parviflorus", 
              "Sherardia arvensis", "Torilis",   "Trifolium fragiferum", "Trifolium repens",
            "Veronica persica","Vicia sativa", "Total")

# Definir el vector de colores para las especies, cambiar el nombre para no solapar valores
# Vector de especies con el color asignado
colores4 <- c(
  "Avena sterilis" = "#FF6347",         # Tomate (color elegido para esta especie)
  "Bellis perennis" = "pink2",        # Violetred3
  "Bromus hordaceus" = "#FF69B4",       # Hotpink
  "Bromus sterelis" = "violetred",        # Mediumpurple
  "Capsella bursa-pastoris" = "#9B59B6",# Purple4 (color cambiado para otra especie no listada)
  "Carex divulsa" = "#DA70D6",          # Orchid (color ajustado para esta especie)
  "Chenopodium sp" = "#551A8B",         # Purple4
  "Dactylis glomerata" = "blue",     # Similar a Light Blue
  "Elymus repens" = "#3498DB",          # Seagreen
  "Festuca arundinacea" = "cyan",    # Similar a Darkcyan
  "Gaudina fragilis" = "green3",       # Aquamarine3 (nuevo color para esta especie)
  "Geranium dissectum" = "#458B00",     # Aquamarine3
  "Lysmachia arvensis" = "palegreen2",       # Cyan
  "Medicago arabica" = "yellow3",       # Similar a Mediumseagreen
  "Mercurialis annua" = "darkgoldenrod",       # Green (color nuevo agregado)
  "Plantago lanceolata" = "#FFD700",    # Similar a Lightseagreen
  "Ranunculus bulbosus" = "sienna",    # Gold (nuevo color para esta especie, no listado antes)
  "Ranunculus parviflorus" = "#D35400", # Pumpkin
  "Sherardia arvensis" = "#FFA07A",     # Lightsalmon
  "Torilis" = "darkorange",             # Darkorange
  "Trifolium fragiferum" = "#DEB887",   # Burlywood
  "Trifolium repens" = "#8B4513",       # Chocolate4
  "Veronica persica" = "brown",       # Chocolate4 (color reutilizado)
  "Vicia sativa" = "tomato",           # Tomato
  "Total" = "#000000"                   # Black
)

# Ajustar el factor de la columna 'species' con los niveles en el orden deseado
plot4$species <- factor(plot4$species, levels = orden_p4)


#gráfico 
ggplot(plot4, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "WP4", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  scale_color_manual(values = colores4) + #para poner los colores que he elegido
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes

#sale una línea que no comprendo intento de quitarla 
# Verificar que todos los niveles del factor tienen un color asignado
levels(plot4$species)[!levels(plot4$species) %in% names(colores4)]
#quitar los NA de nuevo 
plot4 <- plot4[!is.na(plot4$species), ]
#revisar unos de bind_rows y mutate
plot4 <- plot4 %>%
  group_by(sampling) %>%
  summarise(biomass = sum(biomass), .groups = 'drop') %>%
  mutate(species = "Total") %>%
  bind_rows(plot4, .) %>%
  arrange(sampling)  # Asegúrate de ordenar por alguna columna relevante
#hacer un gráfico nuevo a ver si sale 
ggplot(plot4, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  scale_color_manual(values = colores4) +
  labs(title = "WP4", x = "Sampling", y = "Biomasa", color = "Especies") +
  scale_y_log10() +
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        legend.text = element_text(size = 7),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))
#reestablecer el orden 
# Reestableciendo los niveles del factor para asegurar el orden deseado
plot4$species <- factor(plot4$species, levels = orden_p4)
#Plot5####
library(tidyverse)
#primero voy a filtrar P3
plot5 <- subset(datos, treatment == "wp" & plot==5)
#poner sampling como factor 
plot5$sampling <- as.factor(plot5$sampling)

#especies por orden alfabético para que aparezcan así en la leyenda
plot5$species <- factor(plot5$species, levels = sort(unique(plot5$species)))

# Sumar la biomasa por 'sampling' y añadir las filas directamente a `plot2`
plot5 <- plot5 %>%
  group_by(sampling) %>%
  summarise(biomass = sum(biomass), .groups = 'drop') %>%
  mutate(species = "Total") %>%
  bind_rows(plot5, .) %>%
  arrange

# Utilizar subset para quitar las filas donde la columna species tiene valores NA
plot5 <- subset(plot5, !is.na(species))


# Volver a hacer el gráfico 

ggplot(plot5, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "WP5", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes

#ahora ordenarlas y poner total al final 
orden_p5 <- c("Bellis perennis",  "Bromus sterelis", 
              "Capsella bursa-pastoris","Carex divulsa", "Chenopodium sp", "Convolvulus arvensis", "Dactylis glomerata", "Elymus repens", "Festuca arundinacea",
             "Geranium dissectum",
              "Medicago arabica", "Plantago lanceolata", "Poa annua","Ranunculus bulbosus", "Ranunculus parviflorus", 
              "Sherardia arvensis", "Torilis",    "Trifolium repens",
             "Vicia sativa", "Total")

# Definir el vector de colores para las especies, cambiar el nombre para no solapar valores
# Vector de especies con el color asignado
colores5 <- c(
  "Bellis perennis" = "deeppink",
  "Bromus sterelis" = "violetred3",
  "Capsella bursa-pastoris" = "slateblue2",
  "Carex divulsa" = "pink3",
  "Chenopodium sp" = "#551A8B",
  "Convolvulus arvensis" = "#9A32CD", # Color añadido como ejemplo
  "Dactylis glomerata" = "blue",
  "Elymus repens" = "#3498DB",
  "Festuca arundinacea" = "cyan2",
  "Geranium dissectum" = "#458B00",
  "Medicago arabica" = "yellow3",
  "Plantago lanceolata" = "#FFC125",
  "Poa annua" = "green",  # Color añadido como ejemplo
  "Ranunculus bulbosus" = "sienna",
  "Ranunculus parviflorus" = "#D35400",
  "Sherardia arvensis" = "red2",
  "Torilis" = "darkorange",
  "Trifolium repens" = "#8B4513",
  "Vicia sativa" = "tomato",
  "Total" = "#000000"
)
# Ajustar el factor de la columna 'species' con los niveles en el orden deseado
plot5$species <- factor(plot5$species, levels = orden_p5)


#gráfico 
ggplot(plot5, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "WP5", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  scale_color_manual(values = colores5) + #para poner los colores que he elegido
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes

#Plot 6####
library(tidyverse)
#primero voy a filtrar P3
plot5 <- subset(datos, treatment == "wp" & plot==5)
#poner sampling como factor 
plot5$sampling <- as.factor(plot5$sampling)

#especies por orden alfabético para que aparezcan así en la leyenda
plot5$species <- factor(plot5$species, levels = sort(unique(plot5$species)))

# Sumar la biomasa por 'sampling' y añadir las filas directamente a `plot2`
plot5 <- plot5 %>%
  group_by(sampling) %>%
  summarise(biomass = sum(biomass), .groups = 'drop') %>%
  mutate(species = "Total") %>%
  bind_rows(plot5, .) %>%
  arrange

# Utilizar subset para quitar las filas donde la columna species tiene valores NA
plot5 <- subset(plot5, !is.na(species))


# Volver a hacer el gráfico 

ggplot(plot5, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "WP5", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes

#ahora ordenarlas y poner total al final 
orden_p5 <- c("Bellis perennis",  "Bromus sterelis", 
              "Capsella bursa-pastoris","Carex divulsa", "Chenopodium sp", "Convolvulus arvensis", "Dactylis glomerata", "Elymus repens", "Festuca arundinacea",
              "Geranium dissectum",
              "Medicago arabica", "Plantago lanceolata", "Poa annua","Ranunculus bulbosus", "Ranunculus parviflorus", 
              "Sherardia arvensis", "Torilis",    "Trifolium repens",
              "Vicia sativa", "Total")

# Definir el vector de colores para las especies, cambiar el nombre para no solapar valores
# Vector de especies con el color asignado
colores5 <- c(
  "Bellis perennis" = "deeppink",
  "Bromus sterelis" = "violetred3",
  "Capsella bursa-pastoris" = "mediumpurple3",
  "Carex divulsa" = "pink3",
  "Chenopodium sp" = "#551A8B",
  "Convolvulus arvensis" = "#9A32CD", # Color añadido como ejemplo
  "Dactylis glomerata" = "blue",
  "Elymus repens" = "#3498DB",
  "Festuca arundinacea" = "cyan2",
  "Geranium dissectum" = "#458B00",
  "Medicago arabica" = "yellow3",
  "Plantago lanceolata" = "#FFC125",
  "Poa annua" = "green",  # Color añadido como ejemplo
  "Ranunculus bulbosus" = "sienna",
  "Ranunculus parviflorus" = "#D35400",
  "Sherardia arvensis" = "#FFA07A",
  "Torilis" = "darkorange",
  "Trifolium repens" = "#8B4513",
  "Vicia sativa" = "tomato",
  "Total" = "#000000"
)
# Ajustar el factor de la columna 'species' con los niveles en el orden deseado
plot5$species <- factor(plot5$species, levels = orden_p5)


#gráfico 
ggplot(plot5, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "WP5", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  scale_color_manual(values = colores5) + #para poner los colores que he elegido
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9)) 

#primero voy a filtrar P3
plot6 <- subset(datos, treatment == "p" & plot==6)
#poner sampling como factor 
plot6$sampling <- as.factor(plot6$sampling)

#especies por orden alfabético para que aparezcan así en la leyenda
plot6$species <- factor(plot6$species, levels = sort(unique(plot6$species)))

# Sumar la biomasa por 'sampling' y añadir las filas directamente a `plot2`
plot6 <- plot6 %>%
  group_by(sampling) %>%
  summarise(biomass = sum(biomass), .groups = 'drop') %>%
  mutate(species = "Total") %>%
  bind_rows(plot6, .) %>%
  arrange

# Utilizar subset para quitar las filas donde la columna species tiene valores NA
plot6 <- subset(plot6, !is.na(species))

#primer gráfico para ver las especies que hay y luego ordenarlas a mano (no se poner el total abajo de otra forma)
ggplot(plot6, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "P6", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes

#ahora ordenarlas y poner total al final 
orden_p6 <- c("Bellis perennis", "Convolvulus arvensis", "Cynosurus echinatus", "Dactilys glomerata", 
              "Elymus repens", "Festuca arundinacea", "Geranium dissectum", "Medicago arabica", 
              "Medicago lupulina", "Plantago lanceolata", "Potentilla reptans", "Rannunculus parviflorus", 
              "Sherardia arvensis", "Torilis", "Trifolium dubium", "Trifolium repens", 
              "Verbena officinalis", "Veronica persica", "Vicia sativa", "Total")
colores6 <- c(
  "Bellis perennis" = "deeppink",
  "Bromus sterelis" = "violetred3",
  "Capsella bursa-pastoris" = "slateblue2",
  "Carex divulsa" = "pink3",
  "Chenopodium sp" = "#551A8B",
  "Convolvulus arvensis" = "#9A32CD",
  "Dactylis glomerata" = "blue",
  "Elymus repens" = "#3498DB",
  "Festuca arundinacea" = "cyan2",
  "Geranium dissectum" = "#458B00",
  "Medicago arabica" = "yellow3",
  "Plantago lanceolata" = "#FFC125",
  "Poa annua" = "green",
  "Ranunculus bulbosus" = "sienna",
  "Ranunculus parviflorus" = "#D35400",
  "Sherardia arvensis" = "red2",
  "Torilis sp" = "darkorange",
  "Trifolium repens" = "#8B4513",
  "Vicia sativa" = "tomato",
  "Total" = "#000000",
  # Añadiendo los colores para las especies adicionales no incluidas originalmente
  "Cynosurus echinatus" = "orchid",
  "Potentilla reptans" = "lightgreen",
  "Medicago lupulina" = "goldenrod",
  "Trifolium dubium" = "skyblue",
  "Verbena officinalis" = "lightcoral",
  "Veronica persica" = "plum"
)
# Ajustar el factor de la columna 'species' con los niveles en el orden deseado
plot6$species <- factor(plot6$species, levels = orden_p6)

#gráfico 
ggplot(plot5, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "P6", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  scale_color_manual(values = colores5) + #para poner los colores que he elegido
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes



#Plot 7####
plot7 <- subset(datos, treatment == "c" & plot==7)
#poner sampling como factor 
plot7$sampling <- as.factor(plot7$sampling)

#especies por orden alfabético para que aparezcan así en la leyenda
plot7$species <- factor(plot7$species, levels = sort(unique(plot7$species)))

# Sumar la biomasa por 'sampling' y añadir las filas directamente a `plot2`
plot7 <- plot7 %>%
  group_by(sampling) %>%
  summarise(biomass = sum(biomass), .groups = 'drop') %>%
  mutate(species = "Total") %>%
  bind_rows(plot7, .) %>%
  arrange

# Utilizar subset para quitar las filas donde la columna species tiene valores NA
plot7 <- subset(plot7, !is.na(species))
# Asegurarse de que 'species' esté en el orden correcto con 'Total' al final
plot7$species <- factor(plot7$species, levels = orden_p7)


#primer gráfico para ver las especies que hay y luego ordenarlas a mano (no se poner el total abajo de otra forma)
ggplot(plot7, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "C7", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes
#ordenar
orden_p7 <- c(
  "Anacamptis pyramidalis",
  "Bellis perennis",
  "Bromus hordeaceus",
  "Convolvulus arvensis",
  "Elymus repens",
  "Festuca arundinacea",
  "Geranium dissectum",
  "Medicago arabica",
  "Ophrys apifera",
  "Plantago lanceolata",
  "Poa annua",
  "Potentilla reptans",
  "Rannunculus parviflorus",
  "Ranunculus bulbosus",
  "Rumex crispus",
  "Sherardia armensis",
  "Torilis sp",
  "Vicia sativa",
  "Total"  # Asegurarse que Total está al final
)
colores7 <- c(
  "Anacamptis pyramidalis" = "#FF6347",    # Un color genérico para esta especie
  "Bellis perennis" = "#CD3278",
  "Bromus hordeaceus" = "#FF69B4",
  "Convolvulus arvensis" = "#9b59b6",      # Un color genérico para esta especie
  "Elymus repens" = "#551A8B",             # Un color genérico para esta especie
  "Festuca arundinacea" = "#27AE60",
  "Geranium dissectum" = "#66CDAA",
  "Medicago arabica" = "#1ABC9C",
  "Ophrys apifera" = "#3498db",            # Un color genérico para esta especie
  "Plantago lanceolata" = "#16A085",
  "Poa annua" = "darkgoldenrod1",
  "Potentilla reptans" = "#DA70D6",        # Un color genérico para esta especie
  "Rannunculus parviflorus" = "#D35400",
  "Ranunculus bulbosus" = "#FFD700",
  "Rumex crispus" = "#8B4513",             # Un color genérico para esta especie
  "Sherardia armensis" = "#FFA07A",
  "Torilis sp" = "darkorange",
  "Vicia sativa" = "#CD853F",
  "Total" = "black"
)

#gráfico 
ggplot(plot7, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "P7", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  scale_color_manual(values = colores7) + #para poner los colores que he elegido
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes
#Plot 8 ####
plot8 <- subset(datos, treatment == "w" & plot==8)
#poner sampling como factor 
plot8$sampling <- as.factor(plot8$sampling)

#especies por orden alfabético para que aparezcan así en la leyenda
plot8$species <- factor(plot8$species, levels = sort(unique(plot8$species)))

# Sumar la biomasa por 'sampling' y añadir las filas directamente a `plot2`
plot8 <- plot8 %>%
  group_by(sampling) %>%
  summarise(biomass = sum(biomass), .groups = 'drop') %>%
  mutate(species = "Total") %>%
  bind_rows(plot8, .) %>%
  arrange


#primer gráfico para ver las especies que hay y luego ordenarlas a mano (no se poner el total abajo de otra forma)
ggplot(plot8, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "W8", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes

orden_w8 <- c(
  "Aphanes arvensis",
  "Bellis perennis",
  "Bromus hordeaceus",
  "Cerastium glomeratum",
  "Convolvulus arvensis",
  "Elymus repens",
  "Festuca arundinacea",
  "Geranium dissectum",
  "Medicago arabica",
  "Poa annua",
  "Potentilla reptans",
  "Ranunculus bulbosus",
  "Ranunculus parviflorus",
  "Sherardia arvensis",
  "Torilis sp",
  "Trifolium repens",
   "Total"
  )

colores8 <- c(
  "Aphanes arvensis" = "#FF6347",    # Un color genérico para esta especie
  "Bellis perennis" = "#CD3278",
  "Bromus hordeaceus" = "#FF69B4",
  "Cerastium glomeratum" = "#9b59b6", # Un color genérico para esta especie
  "Convolvulus arvensis" = "purple4", # Mismo color que en la lista extendida
  "Elymus repens" = "blue",
  "Festuca arundinacea" = "#27AE60",
  "Geranium dissectum" = "#66CDAA",
  "Medicago arabica" = "#1ABC9C",
  "Poa annua" = "green3",
  "Potentilla reptans" = "darkgoldenrod1",
  "Ranunculus bulbosus" = "#FFD700",
  "Ranunculus parviflorus" = "#D35400",
  "Sherardia armensis" = "coral",
  "Torilis sp" = "darkorange",
  "Trifolium repens" = "chocolate4", # Un color genérico para esta especie
  "Vicia sativa" = "brown4" # Mismo color asignado en la lista extendida
  ,
  "Total" = "black")

# Asegurarse de que 'species' esté en el orden correcto con 'Total' al final
plot8$species <- factor(plot8$species, levels = orden_w8)
# Utilizar subset para quitar las filas donde la columna species tiene valores NA
plot8 <- subset(plot8, !is.na(species))

#gráfico 
ggplot(plot8, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "W8", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  scale_color_manual(values = colores8) + #para poner los colores que he elegido
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes

#Plot 9 ####
plot9 <- subset(datos, treatment == "w" & plot==9)
#poner sampling como factor 
plot9$sampling <- as.factor(plot9$sampling)

#especies por orden alfabético para que aparezcan así en la leyenda
plot9$species <- factor(plot9$species, levels = sort(unique(plot9$species)))

# Sumar la biomasa por 'sampling' y añadir las filas directamente a `plot9`
plot9 <- plot9 %>%
  group_by(sampling) %>%
  summarise(biomass = sum(biomass), .groups = 'drop') %>%
  mutate(species = "Total") %>%
  bind_rows(plot9, .) %>%
  arrange
#desglosarlo y ponerlo en Apuntes

#primer gráfico para ver las especies que hay y luego ordenarlas a mano (no se poner el total abajo de otra forma)
ggplot(plot9, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "W9", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes
orden_w9 <- c(
  "Bellis perennis",
  "Bromus hordeaceus",
  "Carex divulsa",
  "Cerastium glomeratum",
  "Convolvulus arvensis",
  "Cynosurus echinatus",
  "Dactilys glomerata",
  "Elymus repens",
  "Festuca arundinacea",
  "Gaudinia fragilis",
  "Geranium dissectum",
  "Hypochaeris radicata",
  "Ophrys apifera",
  "Poa annua",
  "Ranunculus bulbosus",
  "Ranunculus parviflorus",
  "Sherardia arvensis",
  "Torilis sp",
  "Trifolium fragiferum",
  "Trifolium repens",
  "Veronica arvensis",
  "Total"
)

colores9 <- c(
  "Bellis perennis" = "violetred",
  "Bromus hordeaceus" = "#FF69B4",
  "Carex divulsa" = "pink",
  "Cerastium glomeratum" = "#9b59b6",
  "Convolvulus arvensis" = "purple4",
  "Cynosurus echinatus" = "blue",
  "Dactilys glomerata" = "blue4",
  "Elymus repens" = "deepskyblue4",
  "Festuca arundinacea" = "deepskyblue",
  "Gaudinia fragilis" = "lightskyblue",
  "Geranium dissectum" = "#66CDAA",
  "Hypochaeris radicata" = "palegreen3",
  "Ophrys apifera" = "olivedrab3",
  "Poa annua" = "seagreen",
  "Ranunculus bulbosus" = "yellow2",
  "Ranunculus parviflorus" = "darkgoldenrod2",
  "Sherardia arvensis" = "gold2",
  "Torilis sp" = "darkorange",
  "Trifolium fragiferum" = "chocolate",
  "Trifolium repens" = "sienna",
  "Veronica arvensis" = "brown4",
  "Total" = "black"
)
# Asegurarse de que 'species' esté en el orden correcto con 'Total' al final
plot9$species <- factor(plot9$species, levels = orden_w9)
# Utilizar subset para quitar las filas donde la columna species tiene valores NA
plot9 <- subset(plot9, !is.na(species))

#gráfico 
ggplot9 <- ggplot(plot9, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "W9", x = "Sampling", y = "Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  scale_color_manual(values = colores9) + #para poner los colores que he elegido
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes

#Plot 10####
plot10 <- subset(datos, treatment == "p" & plot==10)
#poner sampling como factor 
plot10$sampling <- as.factor(plot10$sampling)

#especies por orden alfabético para que aparezcan así en la leyenda
plot10$species <- factor(plot10$species, levels = sort(unique(plot10$species)))

# Sumar la biomasa por 'sampling' y añadir las filas directamente a `plot2`
plot10 <- plot10 %>%
  group_by(sampling) %>%
  summarise(biomass = sum(biomass), .groups = 'drop') %>%
  mutate(species = "Total") %>%
  bind_rows(plot10, .) %>%
  arrange


#primer gráfico para ver las especies que hay y luego ordenarlas a mano (no se poner el total abajo de otra forma)
ggplot(plot10, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "P10", x = "Sampling", y = "Log Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes

orden_p10 <- c(
  "Bellis perennis",
  "Bromus hordeaceus",
  "Chenopodium sp",
  "Convolvulus arvensis",
  "Elymus repens",
  "Festuca arundinacea",
  "Geranium dissectum",
  "Helminthotheca sp",
  "Kickxia spuria",
  "Linum bienne",
  "Lysimachia arvensis",
  "Medicago arabica",
  "Mercurialis annua",
  "Plantago lanceolata",
  "Potentilla reptans",
  "Ranunculus parviflorus",
  "Sherardia arvensis",
  "Torilis sp",
  "Verbena officinalis",
  "Veronica persica",
  "Vicia sativa",
  "Total"
)
colores10 <- c(
  "Bellis perennis" = "pink2",
  "Bromus hordeaceus" = "magenta2",
  "Chenopodium sp" = "violetred4",
  "Convolvulus arvensis" = "violetred",
  "Elymus repens" = "darkorchid2",
  "Festuca arundinacea" = "#551A8B",
  "Geranium dissectum" = "blue3",
  "Helminthotheca sp" = "blue",
  "Kickxia spuria" = "#6495ED",
  "Linum bienne" = "deepskyblue3",
  "Lysimachia arvensis" = "deepskyblue",
  "Medicago arabica" = "#1ABC9C",
  "Mercurialis annua" = "green4",
  "Plantago lanceolata" = "seagreen3",
  "Potentilla reptans" = "green",
  "Ranunculus parviflorus" = "olivedrab2",
  "Sherardia armensis" = "yellow2",
  "Torilis sp" = "darkorange",
  "Verbena officinalis" = "#FF4500",
  "Veronica persica" = "#8B0000",
  "Vicia sativa" = "#CD853F",
  "Total" = "black"
)

# Asegurarse de que 'species' esté en el orden correcto con 'Total' al final
plot10$species <- factor(plot10$species, levels = orden_p10)
# Utilizar subset para quitar las filas donde la columna species tiene valores NA
plot10 <- subset(plot10, !is.na(species))

#gráfico 
ggplot(plot10, aes(x = sampling, y = biomass, color = species, group = species)) +
  geom_point() +  # Puntos para cada observación
  geom_line() +   # Líneas conectando los puntos
  labs(title = "P10", x = "Sampling", y = "Log Biomasa", color = "Especies") +  
  scale_y_log10() +  # Transforma el eje y a escala logarítmica
  scale_color_manual(values = colores10) + #para poner los colores que he elegido
  theme(plot.title = element_text(hjust = 0.5, size = 13),   # Tamaño y alineación del título del gráfico
        legend.text = element_text(size = 7),               # Tamaño del texto de la leyenda
        axis.text = element_text(size = 8),                 # Tamaño del texto de las etiquetas de los ejes
        axis.title = element_text(size = 9))               # Tamaño del texto de los títulos de los ejes



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

datos <- read.csv("C:/RTFM/data/flora_db.csv")

datos$plot <- as.factor(datos$plot)
datos$sampling <- as.factor(datos$sampling)
datos$code <- as.factor(datos$code)

datos <- datos %>%
  filter(!sampling %in% c("0", "1", "2"))


# Configuración del estílo de los gráficos
theme_set(theme_bw())


#Loop para el total biomass por sampling y plot

plots <- sort(unique(datos$plot))
samps <- sort(unique(datos$sampling)) #mirar si se puede poner el sort
datos$total_biomass <- NA   #El espacio de almacenamiento donde vamos a guardar los resutlados del loop

for (i in 1:length(plots)){
  for(j in 1:length(samps)){
    
    subset_ij <- subset(datos, plot == plots[i] & sampling == samps[j])
    subset_totalb <-sum(subset_ij$biomass, na.rm = T)
    datos[which(datos$plot == plots[i] & datos$sampling == samps[j]), ]$total_biomass <- subset_totalb
    
  }
}

# Cargar paquetes
library(dplyr)

library(ggplot2)


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
datos_media <- aggregate(total_biomass ~ treatment + sampling, data = datos, FUN = mean, na.rm = TRUE)

#loop####
#primero creo el primer loop
datos <- read.csv("data/flora_db.csv")

datos$plot <- as.factor(datos$plot)
datos$sampling <- as.factor(datos$sampling)
datos$code <- as.factor(datos$code)

datos <- datos %>%
  filter(!sampling %in% c("0", "1", "2"))

# Configuración del estílo de los gráficos
theme_set(theme_bw())


#Loop para el total biomass por sampling y plot

plots <- sort(unique(datos$plot))
samps <- sort(unique(datos$sampling))
datos$total_biomass <- NA   #El espacio de almacenamiento donde vamos a guardar los resutlados del loop

for (i in 1:length(plots)){
  for(j in 1:length(samps)){
    
    subset_ij <- subset(datos, plot == plots[i] & sampling == samps[j])
    
    subset_totalb <- summarize(group_by(subset_ij, plot, sampling),
                               total_biomass = sum(biomass, na.rm = T))
    
    datos[which(datos$plot == plots[i] & datos$sampling == samps[j]), ]$total_biomass <- subset_totalb$total_biomass
    
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
    labs( x = "Sampling", y = "log(Biomass)", title = paste("Plot", plot_i$plot, ",", "Treatment", plot_i$treatment, sep = " "))
  
}

#voy a intentar hacer un loop primero creo un espacio vació para la proporción y luego hago que se dividan entre el c según el sampling
datos_media$media <- NA   #El espacio de almacenamiento donde vamos a guardar los resutlados

# Repetir (loop) sobre cada valor único de 'sampling'
for (s in unique(datos_media$sampling)) {
  # Obtener el total_biomass para el tratamiento 'c' en el sampling actual
  c_biomass <- datos_media$total_biomass[datos_media$sampling == s & datos_media$treatment == "c"]
  
  # Calcular la proporción para los tratamientos 'p', 'w', 'wp' en el mismo sampling
  datos_media$media[datos_media$sampling == s & datos_media$treatment != "c"] <-
    datos_media$total_biomass[datos_media$sampling == s & datos_media$treatment != "c"] / c_biomass
}

# Filtrar los tratamientos diferentes de 'c'
result_data <- datos_media[datos_media$treatment != "c", ]

# Mostrar el resultado
print(result_data)
# Cargar paquetes
library(dplyr)
library(ggplot2)

# Aplicar logaritmo en base 10 a la columna 'media'
result_data$logmedia <- log10(result_data$media)


# Eliminar la columna log10_media de result_data
result_data <- subset(result_data, select = -log10_media)



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




