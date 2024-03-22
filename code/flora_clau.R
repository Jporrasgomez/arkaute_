

library(dplyr)
library(ggplot2)


# Cargar archivo CSV
datos <- read.csv("data/flora_db.csv")

#comprobación de datos
str(datos)
summary(datos)
head(datos)

hist(datos$abundance)

#Hay muchas variables que son "character". Es mejor que sean "factor" para que esten categorizadas
datos$sampling <- as.factor(datos$sampling)
datos$plot <- as.factor(datos$plot)
datos$treatment <- as.factor(datos$treatment)

# Agrupar por tratamiento,sampling y plot y luego calcular la suma de la abundancia
# de esta manera, calculamos 4 puntos de abundancia por muestreo y tratamiento. Con estos 4 puntos
# se crearán los boxplots. 

#2 formas de crear la base de datos: usando %>% o no

suma_abundancia1 <- summarise(group_by(datos, sampling, treatment, plot),
                              suma_abundancia = sum(abundance, na.rm = T))
suma_abundancia <- datos %>%
  group_by(sampling, treatment, plot) %>%
  summarize(suma_abundancia = sum(abundance, na.rm = TRUE))


# Para ver los resultados
print(media_abundancia)


# Filtrar los datos para el tratamiento 'w'
# De nuevo, 2 formas de hacerlo: 

tratamiento_w <- filter(suma_abundancia, treatment == "w")

tratamiento_w <- suma_abundancia %>%
  filter(treatment == 'w')
str(tratamiento_w)


# 1 plot por tratamiento: 

ggplot(tratamiento_w, aes(x = sampling, y = suma_abundancia)) +
  geom_boxplot(fill = "indianred3") +  # Color rojo para las cajas
  labs(x = "Sampling", y = "Abundancia") +  # Etiquetas de los ejes
  theme_minimal() +
  ggtitle("W") +  # Título centrado y en negrita
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))  # Ajuste del título centrado y en negrita



#All plots at once


ggplot(suma_abundancia, aes(x = sampling, y = suma_abundancia, fill = treatment)) +
  geom_boxplot() +  # Color rojo para las cajas
  facet_grid (~treatment)+
  labs(x = "Sampling", y = "Abundancia") +  # Etiquetas de los ejes
  scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple"))+
  theme_minimal()+  # Título centrado y en negrita
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))  # Ajuste del título centrado y en negrita



