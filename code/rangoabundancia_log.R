install.packages(lubridate)
library(lubridate)
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")
install.packages("tidyr")
install.packages("gridExtra")
library(gridExtra)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
summary(flora_db)
#dividir la columna abundance entre 100
flora_db$abundance <- flora_db$abundance / 100
#eliminar las columnas que no voy a usar
flora_db <- subset(flora_db, select = -c(height, Cb, Db, Cm, Dm, category, OBS))
summary(flora_db)
# Crear y guardar archivos para cada tratamiento

flora_db$treatment <- trimws(flora_db$treatment)
treatment_c <- flora_db[flora_db$treatment == "c", ]
write.csv(treatment_c, file = "flora_c.csv", row.names = FALSE)
treatment_wp <- flora_db[flora_db$treatment == "wp", ]
write.csv(treatment_wp, file = "flora_wp.csv", row.names = FALSE)
treatment_w <- flora_db[flora_db$treatment == "w", ]
write.csv(treatment_w, file = "flora_w.csv", row.names = FALSE)
treatment_p <- flora_db[flora_db$treatment == "p", ]
write.csv(treatment_p, file = "flora_p.csv", row.names = FALSE)
#he eliminado la columna sampliing en cada archivo

treatment_c <- treatment_c[, -which(names(treatment_c) == "sampling")]
treatment_c$plot <- gsub("plot2", "plot 2", treatment_c$plot)
treatment_p <- treatment_p[, -which(names(treatment_p) == "sampling")]
treatment_wp <- treatment_wp[, -which(names(treatment_wp) == "sampling")]
treatment_w <- treatment_w[, -which(names(treatment_w) == "sampling")]
# Convertir 'abundance' a numérico para asegurarse de que es un tipo de dato numérico
treatment_c$abundance <- as.numeric(treatment_c$abundance)
treatment_p$abundance<-as.numeric(treatment_p$abundance)
treatment_w$abundance <- as.numeric(treatment_w$abundance)
treatment_wp$abundance<-as.numeric(treatment_wp$abundance)
# Calcular la suma de abundancias por especie y plot
abundance_by_species_plot <- aggregate(abundance ~ species + plot, data = treatment_c, sum)
abundance_by_species_plot_p <- aggregate(abundance ~ species + plot, data = treatment_p, sum)
abundance_by_species_plot_w <- aggregate(abundance ~ species + plot, data = treatment_w, sum)
abundance_by_species_plot_wp <- aggregate(abundance ~ species + plot, data = treatment_wp, sum)
# Calcular la abundancia total por plot
total_abundance_by_plot <- aggregate(abundance ~ plot, data = abundance_by_species_plot, sum)
total_abundance_by_plot_p <- aggregate(abundance ~ plot, data = abundance_by_species_plot_p, sum)
total_abundance_by_plot_w<- aggregate(abundance ~ plot, data = abundance_by_species_plot_w, sum)
total_abundance_by_plot_wp <- aggregate(abundance ~ plot, data = abundance_by_species_plot_wp, sum)
# Fusionar los resultados para obtener un marco de datos con abundancia total por plot
abundance_data_c <- merge(abundance_by_species_plot, total_abundance_by_plot, by = "plot", suffixes = c("", "_total"))
abundance_data_p <- merge(abundance_by_species_plot_p, total_abundance_by_plot_p, by = "plot", suffixes = c("", "_total"))
abundance_data_w <- merge(abundance_by_species_plot_w, total_abundance_by_plot_w, by = "plot", suffixes = c("", "_total"))
abundance_data_wp <- merge(abundance_by_species_plot_wp, total_abundance_by_plot_wp, by = "plot", suffixes = c("", "_total"))
# Calcular la abundancia relativa
abundance_data_c$relative_abundance <- abundance_data_c$abundance / abundance_data_c$abundance_total
abundance_data_c$relative_abundance <- abundance_data_c$abundance / abundance_data_c$abundance_total
abundance_data_p$relative_abundance <- abundance_data_p$abundance / abundance_data_p$abundance_total
abundance_data_p$relative_abundance <- abundance_data_p$abundance / abundance_data_p$abundance_total
abundance_data_w$relative_abundance <- abundance_data_w$abundance / abundance_data_w$abundance_total
abundance_data_w$relative_abundance <- abundance_data_w$abundance / abundance_data_w$abundance_total
abundance_data_wp$relative_abundance <- abundance_data_wp$abundance / abundance_data_wp$abundance_total
abundance_data_wp$relative_abundance <- abundance_data_wp$abundance / abundance_data_wp$abundance_total
#para crear una nueva columna en la que la abundancia relativa este en escala logaritmica
abundance_data_c$log_relative_abundance <- log(abundance_data_c$relative_abundance)
abundance_data_p$log_relative_abundance <- log(abundance_data_p$relative_abundance)
abundance_data_w$log_relative_abundance <- log(abundance_data_w$relative_abundance)
abundance_data_wp$log_relative_abundance <- log(abundance_data_wp$relative_abundance)
# Convertir 'abundance' y 'abundance_total' a numérico
abundance_data_c$log_relative_abundance <- as.numeric(abundance_data_c$log_relative_abundance)
abundance_data_c$abundance_total <- as.numeric(abundance_data_c$abundance_total)
abundance_data_p$log_relative_abundance <- as.numeric(abundance_data_p$log_relative_abundance)
abundance_data_p$abundance_total <- as.numeric(abundance_data_p$abundance_total)
abundance_data_w$log_relative_abundance <- as.numeric(abundance_data_w$log_relative_abundance)
abundance_data_w$abundance_total <- as.numeric(abundance_data_w$abundance_total)
abundance_data_wp$log_relative_abundance <- as.numeric(abundance_data_wp$log_relative_abundance)
abundance_data_wp$abundance_total <- as.numeric(abundance_data_wp$abundance_total)
# Ordenar el data frame por plot y abundancia relativa descendente
abundance_data_c <- abundance_data_c[order(abundance_data_c$plot, -abundance_data_c$log_relative_abundance),]
abundance_data_p <- abundance_data_p[order(abundance_data_p$plot, -abundance_data_p$log_relative_abundance),]
abundance_data_w <- abundance_data_w[order(abundance_data_w$plot, -abundance_data_w$log_relative_abundance),]
abundance_data_wp <- abundance_data_wp[order(abundance_data_wp$plot, -abundance_data_wp$log_relative_abundance),]
# Crear la columna 'rango' por plot
abundance_data_c$rango <- ave(seq_along(abundance_data_c$log_relative_abundance), abundance_data_c$plot, FUN = seq_along)
abundance_data_p$rango <- ave(seq_along(abundance_data_p$log_relative_abundance), abundance_data_p$plot, FUN = seq_along)
abundance_data_w$rango <- ave(seq_along(abundance_data_w$log_relative_abundance), abundance_data_w$plot, FUN = seq_along)
abundance_data_wp$rango <- ave(seq_along(abundance_data_wp$log_relative_abundance), abundance_data_wp$plot, FUN = seq_along)
#probar a hacer graficos con esto a ver que sale
rankabundance_logc<-ggplot(abundance_data_c, aes(x = rango, y = log_relative_abundance, group = plot, color = plot)) +
  geom_point() +
  geom_line() +
  labs(title = "Rank abundance-c",
       x = "Rank",
       y = "Relative Abundance") +
  theme_minimal()
print(rankabundance_logc)
rankabundance_logp<-ggplot(abundance_data_p, aes(x = rango, y = log_relative_abundance, group = plot, color = plot)) +
  geom_point() +
  geom_line() +
  labs(title = "Rank abundance-p",
       x = "Rank",
       y = "Relative Abundance") +
  theme_minimal()
print(rankabundance_logp)
rankabundance_logw<-ggplot(abundance_data_w, aes(x = rango, y = log_relative_abundance, group = plot, color = plot)) +
  geom_point() +
  geom_line() +
  labs(title = "Rank abundance-w",
       x = "Rank",
       y = "Relative Abundance") +
  theme_minimal()
print(rankabundance_logw)
rankabundance_logwp<-ggplot(abundance_data_wp, aes(x = rango, y = log_relative_abundance, group = plot, color = plot)) +
  geom_point() +
  geom_line() +
  labs(title = "Rank abundance-wp",
       x = "Rank",
       y = "Relative Abundance") +
  theme_minimal()
print(rankabundance_logwp)

#ahora intentando hacer medias y desviacion
# Configurar los datos para el gráfico
media_por_rangocmedia <- aggregate(log_relative_abundance ~ rango, data = abundance_data_c, mean)
media_por_rangopmedia <- aggregate(log_relative_abundance ~ rango, data = abundance_data_p, mean)
media_por_rangowmedia <- aggregate(log_relative_abundance ~ rango, data = abundance_data_w, mean)
media_por_rangowpmedia <- aggregate(log_relative_abundance ~ rango, data = abundance_data_wp, mean)

desviacion_por_rangocmedia <- aggregate(log_relative_abundance ~ rango, data = abundance_data_c, sd)
desviacion_por_rangopmedia <- aggregate(log_relative_abundance ~ rango, data = abundance_data_p, sd)
desviacion_por_rangowmedia <- aggregate(log_relative_abundance ~ rango, data = abundance_data_w, sd)
desviacion_por_rangowpmedia <- aggregate(log_relative_abundance ~ rango, data = abundance_data_wp, sd)

resultadoscmedia <- cbind(media_por_rangocmedia, Desviacion = desviacion_por_rangocmedia$log_relative_abundance)
resultadospmedia <- cbind(media_por_rangopmedia, Desviacion = desviacion_por_rangopmedia$log_relative_abundance)
resultadoswmedia <- cbind(media_por_rangowmedia, Desviacion = desviacion_por_rangowmedia$log_relative_abundance)
resultadoswpmedia <- cbind(media_por_rangowpmedia, Desviacion = desviacion_por_rangowpmedia$log_relative_abundance)

# Graficar puntos y líneas con desviación estándar
rankabundance_cmedialog <- ggplot(resultadoscmedia, aes(x = rango, y = log_relative_abundance, group = 1)) +
  geom_line(color = "blue") +  # Color de la línea
  geom_point(color = "blue") +  # Color de los puntos
  geom_errorbar(aes(ymin = log_relative_abundance - Desviacion, ymax = log_relative_abundance + Desviacion), 
                width = 0.2, color = "red") +  # Color de las barras de error
  labs(title = "Rank abundance-c",
       x = "Rank", y = "Relative abundance") 

# Mostrar el gráfico
print(rankabundance_cmedialog)
rankabundance_pmedialog <- ggplot(resultadospmedia, aes(x = rango, y = log_relative_abundance, group = 1)) +
  geom_line(color = "blue") +  # Color de la línea
  geom_point(color = "blue") +  # Color de los puntos
  geom_errorbar(aes(ymin = log_relative_abundance - Desviacion, ymax = log_relative_abundance + Desviacion), 
                width = 0.2, color = "red") +  # Color de las barras de error
  labs(title = "Rank abundance-p",
       x = "Rank", y = "Relative abundance") 
print(rankabundance_pmedialog)
rankabundance_wmedialog <- ggplot(resultadoswmedia, aes(x = rango, y = log_relative_abundance, group = 1)) +
  geom_line(color = "blue") +  # Color de la línea
  geom_point(color = "blue") +  # Color de los puntos
  geom_errorbar(aes(ymin = log_relative_abundance - Desviacion, ymax = log_relative_abundance + Desviacion), 
                width = 0.2, color = "red") +  # Color de las barras de error
  labs(title = "Rank abundance-w",
       x = "Rank", y = "Relative abundance") 
print(rankabundance_wmedialog)
rankabundance_pmedialog <- ggplot(resultadospmedia, aes(x = rango, y = log_relative_abundance, group = 1)) +
  geom_line(color = "blue") +  # Color de la línea
  geom_point(color = "blue") +  # Color de los puntos
  geom_errorbar(aes(ymin = log_relative_abundance - Desviacion, ymax = log_relative_abundance + Desviacion), 
                width = 0.2, color = "red") +  # Color de las barras de error
  labs(title = "Rank abundance-p",
       x = "Rank", y = "Relative abundance") 
print(rankabundance_pmedialog)
rankabundance_wpmedialog <- ggplot(resultadoswpmedia, aes(x = rango, y = log_relative_abundance, group = 1)) +
  geom_line(color = "blue") +  # Color de la línea
  geom_point(color = "blue") +  # Color de los puntos
  geom_errorbar(aes(ymin = log_relative_abundance - Desviacion, ymax = log_relative_abundance + Desviacion), 
                width = 0.2, color = "red") +  # Color de las barras de error
  labs(title = "Rank abundance-wp",
       x = "Rank", y = "Relative abundance") 
print(rankabundance_wpmedialog)

#intentando ahora con la escala en e, partiendo de los datos de abundancia relativca y reescalando
mediac <- aggregate(relative_abundance ~ rango, data = abundance_data_c, mean)
mediap <- aggregate(relative_abundance ~ rango, data = abundance_data_p, mean)
mediaw <- aggregate(relative_abundance ~ rango, data = abundance_data_w, mean)
mediawp <- aggregate(relative_abundance ~ rango, data = abundance_data_wp, mean)

desviacionc <- aggregate(relative_abundance ~ rango, data = abundance_data_c, sd)
desviacionp <- aggregate(relative_abundance ~ rango, data = abundance_data_p, sd)
desviacionw <- aggregate(relative_abundance ~ rango, data = abundance_data_w, sd)
desviacionwp <- aggregate(relative_abundance ~ rango, data = abundance_data_wp, sd)

cmedia <- cbind(mediac, Desviacion = desviacionc$relative_abundance)
pmedia <- cbind(mediap, Desviacion = desviacionp$relative_abundance)
wmedia <- cbind(mediaw, Desviacion = desviacionw$relative_abundance)
wpmedia <- cbind(mediawp, Desviacion = desviacionwp$relative_abundance)

#graficar sin la desviacion a ver que sale
logc<-ggplot(cmedia, aes(x = rango, y = relative_abundance)) +
  geom_point(color = "red") +  
  geom_line(color = "blue") +   # Línea azul
  scale_y_log10() +  # Escala logarítmica en el eje y
  labs(title = "RAD-c", x = "Rank", y = "Relative abundance")
print(logc)
logp<-ggplot(pmedia, aes(x = rango, y = relative_abundance)) +
  geom_point(color = "red") +  
  geom_line(color = "blue") +   # Línea azul
  scale_y_log10() +  # Escala logarítmica en el eje y
  labs(title = "RAD-p", x = "Rank", y = "Relative abundance")
print(logp)
logw<-ggplot(wmedia, aes(x = rango, y = relative_abundance)) +
  geom_point(color = "red") +  
  geom_line(color = "blue") +   # Línea azul
  scale_y_log10() +  # Escala logarítmica en el eje y
  labs(title = "RAD-w", x = "Rank", y = "Relative abundance")
print(logw)
logwp<-ggplot(wpmedia, aes(x = rango, y = relative_abundance)) +
  geom_point(color = "red") +  
  geom_line(color = "blue") +   # Línea azul
  scale_y_log10() +  # Escala logarítmica en el eje y
  labs(title = "RAD-wp", x = "Rank", y = "Relative abundance")
print(logwp)
#reescalando todos para que aparezcan con la misma escala

logc2 <- ggplot(cmedia, aes(x = rango, y = relative_abundance)) +
  geom_point(color = "red") +  
  geom_line(color = "blue") +
  scale_y_log10() +  # Escala logarítmica en base a "e"
  labs(title = "RAD-c", x = "Rank", y = "Relative abundance")

logc2 + coord_cartesian(ylim = c(1e-04, 1))
logp2 <- ggplot(pmedia, aes(x = rango, y = relative_abundance)) +
  geom_point(color = "red") +  
  geom_line(color = "blue") +
  scale_y_log10() +  # Escala logarítmica en base a "e"
  labs(title = "RAD-p", x = "Rank", y = "Relative abundance")

logp2 + coord_cartesian(ylim = c(1e-04, 1))
logw2 <- ggplot(wmedia, aes(x = rango, y = relative_abundance)) +
  geom_point(color = "red") +  
  geom_line(color = "blue") +
  scale_y_log10() +  # Escala logarítmica en base a "e"
  labs(title = "RAD-w", x = "Rank", y = "Relative abundance")

logw2 + coord_cartesian(ylim = c(1e-04, 1))
logwp2 <- ggplot(wpmedia, aes(x = rango, y = relative_abundance)) +
  geom_point(color = "red") +  
  geom_line(color = "blue")  +
  scale_y_log10() +  # Escala logarítmica en base a "e"
  labs(title = "RAD-wp", x = "Rank", y = "Relative abundance")

logwp2 + coord_cartesian(ylim = c(1e-04, 1))

#los mismos gráficos pero con barras de error
logc3 <- ggplot(cmedia, aes(x = rango, y = relative_abundance)) +
  geom_point(color = "blue") +  
  geom_line(color = "blue") +  
  geom_errorbar(aes(ymin = relative_abundance - Desviacion, ymax = relative_abundance + Desviacion), 
                                             width = 0.2, color = "red") +
  scale_y_log10() +  # Escala logarítmica en base a "e"
  labs(title = "RAD-c", x = "Rank", y = "Relative abundance")

logc3 + coord_cartesian(ylim = c(1e-04, 1))
logp3 <- ggplot(pmedia, aes(x = rango, y = relative_abundance)) +
  geom_point(color = "blue") +  
  geom_line(color = "blue") +
  geom_errorbar(aes(ymin = relative_abundance - Desviacion, ymax = relative_abundance + Desviacion), 
                width = 0.2, color = "red") +
  scale_y_log10() +  # Escala logarítmica en base a "e"
  labs(title = "RAD-p", x = "Rank", y = "Relative abundance")

logp3 + coord_cartesian(ylim = c(1e-04, 1))
logw3 <- ggplot(wmedia, aes(x = rango, y = relative_abundance)) +
  geom_point(color = "blue") +  
  geom_line(color = "blue") +
  geom_errorbar(aes(ymin = relative_abundance - Desviacion, ymax = relative_abundance + Desviacion), 
                width = 0.2, color = "red") +
  scale_y_log10() +  # Escala logarítmica en base a "e"
  labs(title = "RAD-w", x = "Rank", y = "Relative abundance")

logw3 + coord_cartesian(ylim = c(1e-04, 1))
logwp3 <- ggplot(wpmedia, aes(x = rango, y = relative_abundance)) +
  geom_point(color = "blue") +  
  geom_line(color = "blue") +
  geom_errorbar(aes(ymin = relative_abundance - Desviacion, ymax = relative_abundance + Desviacion), 
                width = 0.2, color = "red") +
  scale_y_log10() +  # Escala logarítmica en base a "e"
  labs(title = "RAD-wp", x = "Rank", y = "Relative abundance")
logwp3 + coord_cartesian(ylim = c(1e-04, 1))