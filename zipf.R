install.packages("lubridate")
library(lubridate)
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")
install.packages("tidyr")
install.packages("gridExtra")
install.packages("vegan")
install.packages("tidyverse")
library(gridExtra)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(vegan)
library(tidyverse)
####carga de la base de datos
summary(flora_db)
#dividir la columna abundance entre 100 para tener la abundancia sobre 1
flora_db$abundance <- flora_db$abundance / 100
#eliminar las columnas que no voy a usar
flora_db <- subset(flora_db, select = -c(height, Cb, Db, Cm, Dm, category, OBS,date))
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
summary(treatment_c)
######

####preparacion del archivo
treatment_c <- treatment_c %>%
  group_by(plot) %>%
  mutate(abundance_total = sum(abundance))
treatment_c <- treatment_c %>%
  group_by(plot, species) %>%
  mutate(abundance_species = sum(abundance))
treatment_c <- treatment_c %>%
  select(-abundance_total_by_species)
treatment_c <- distinct(treatment_c)
treatment_c <- treatment_c %>%
  select(-abundance)
summary(treatment_c)

treatment_c <- treatment_c %>%
  mutate(abundance_relative = abundance_species / abundance_total)
treatment_c <- treatment_c %>%
  select(-treatment, -abundance_total, -abundance_species)

####eliminar filas repes
treatment_c <- unique(treatment_c)
summary(treatment_c)
treatment_p<- unique(treatment_p)
summary(treatment_p)
treatment_w<- unique(treatment_w)
summary(treatment_w)
treatment_wp<- unique(treatment_wp)
summary(treatment_wp)

treatment_p <- treatment_p %>%
  group_by(plot) %>%
  mutate(abundance_total = sum(abundance))
treatment_p <- treatment_p %>%
  group_by(plot, species) %>%
  mutate(abundance_species = sum(abundance))

treatment_p <- distinct(treatment_p)
treatment_p <- treatment_p %>%
  select(-abundance)
summary(treatment_p)

treatment_p <- treatment_p %>%
  mutate(abundance_relative = abundance_species / abundance_total)
treatment_p <- treatment_p %>%
  select(-treatment, -abundance_total, -abundance_species)

treatment_wp <- treatment_wp %>%
  group_by(plot) %>%
  mutate(abundance_total = sum(abundance))
treatment_wp <- treatment_wp %>%
  group_by(plot, species) %>%
  mutate(abundance_species = sum(abundance))

treatment_wp <- distinct(treatment_wp)
treatment_wp <- treatment_wp %>%
  select(-abundance)
summary(treatment_wp)

treatment_wp <- treatment_wp %>%
  mutate(abundance_relative = abundance_species / abundance_total)
treatment_wp <- treatment_wp %>%
  select(-treatment, -abundance_total, -abundance_species)

treatment_w <- treatment_w %>%
  group_by(plot) %>%
  mutate(abundance_total = sum(abundance))
treatment_w <- treatment_w %>%
  group_by(plot, species) %>%
  mutate(abundance_species = sum(abundance))

treatment_w <- distinct(treatment_w)
treatment_w <- treatment_w %>%
  select(-abundance)
summary(treatment_w)

treatment_w <- treatment_w %>%
  mutate(abundance_relative = abundance_species / abundance_total)
treatment_w <- treatment_w%>%
  select(-treatment, -abundance_total, -abundance_species)



# Itera sobre cada plot y guarda en un archivo separado
treatment_c$plot <- trimws(treatment_c$plot)
plotc2 <- treatment_c[treatment_c$plot == "plot 2", ]
write.csv(plotc2, file = "plotc2.csv", row.names = FALSE)

treatment_c$plot <- trimws(treatment_c$plot)
plotc7 <- treatment_c[treatment_c$plot == "plot 7", ]
write.csv(plotc7, file = "plotc7.csv", row.names = FALSE)

treatment_c$plot <- trimws(treatment_c$plot)
plotc11 <- treatment_c[treatment_c$plot == "plot 11", ]
write.csv(plotc11, file = "plotc11.csv", row.names = FALSE)

treatment_c$plot <- trimws(treatment_c$plot)
plotc14 <- treatment_c[treatment_c$plot == "plot 14", ]
write.csv(plotc14, file = "plotc14.csv", row.names = FALSE)

####lo mismo con perturbacion
treatment_p$plot <- trimws(treatment_p$plot)
plotp3 <- treatment_p[treatment_p$plot == "plot 3",]
write.csv(plotp3, file = "plotp3.csv", row.names = FALSE)

treatment_p$plot <- trimws(treatment_p$plot)
plotp6 <- treatment_p[treatment_p$plot == "plot 6",]
write.csv(plotp6, file = "plotp6.csv", row.names = FALSE)

treatment_p$plot <- trimws(treatment_p$plot)
plotp10 <- treatment_p[treatment_p$plot == "plot 10",]
write.csv(plotp10, file = "plotp10.csv", row.names = FALSE)

treatment_p$plot <- trimws(treatment_p$plot)
plotp15 <- treatment_p[treatment_p$plot == "plot 15",]
write.csv(plotp15, file = "plotp15.csv", row.names = FALSE)

plotp3 <- subset(plotp3, select = c("abundance_relative", "species"))
plotp6<-subset(plotp6,select = c("abundance_relative","species"))
plotp15<-subset(plotp15,select = c("abundance_relative","species"))
plotp15<-subset(plotp15,select = c("abundance_relative","species"))
summary(plotp3)
#######lo  mismo con tratamiento termico
treatment_w$plot <- trimws(treatment_w$plot)
plotw1 <- treatment_w[treatment_w$plot == "plot 1",]
write.csv(plotw1, file = "plotw1.csv", row.names = FALSE)

treatment_w$plot <- trimws(treatment_w$plot)
plotw8<- treatment_w[treatment_w$plot == "plot 8",]
write.csv(plotw8, file = "plotw8.csv", row.names = FALSE)

treatment_w$plot <- trimws(treatment_w$plot)
plotw9 <- treatment_w[treatment_w$plot == "plot 9",]
write.csv(plotw9, file = "plotw9.csv", row.names = FALSE)

treatment_w$plot <- trimws(treatment_w$plot)
plotw16 <- treatment_w[treatment_w$plot == "plot 16",]
write.csv(plotw16, file = "plotw16.csv", row.names = FALSE)

plotw1 <- subset(plotw1, select = c("abundance_relative", "species"))
plotw8<-subset(plotw8,select = c("abundance_relative","species"))
plotw9<-subset(plotw9,select = c("abundance_relative","species"))
plotw16<-subset(plotw16,select = c("abundance_relative","species"))
summary(plotw1)

#######igual con tratamiento termico+perturbacion
treatment_wp$plot <- trimws(treatment_wp$plot)
plotwp4 <- treatment_wp[treatment_wp$plot == "plot 4",]
write.csv(plotwp4, file = "plotwp4.csv", row.names = FALSE)

treatment_wp$plot <- trimws(treatment_wp$plot)
plotwp5<- treatment_wp[treatment_wp$plot == "plot 5",]
write.csv(plotwp5, file = "plotwp5.csv", row.names = FALSE)

treatment_wp$plot <- trimws(treatment_wp$plot)
plotwp12 <- treatment_wp[treatment_wp$plot == "plot 12",]
write.csv(plotwp12, file = "plotwp12.csv", row.names = FALSE)

treatment_wp$plot <- trimws(treatment_wp$plot)
plotwp13 <- treatment_wp[treatment_wp$plot == "plot 13",]
write.csv(plotwp13, file = "plotwp13.csv", row.names = FALSE)

plotw1 <- subset(plotw1, select = c("abundance_relative", "species"))
plotwp5<-subset(plotwp5,select = c("abundance_relative","species"))
plotwp12<-subset(plotwp12,select = c("abundance_relative","species"))
plotwp13<-subset(plotwp13,select = c("abundance_relative","species"))
summary(plotw1)


# Función para obtener los límites del eje y
get_y_limits <- function(rad_model) {
  y_limits <- c(min(rad_model$y), max(rad_model$y))
  return(y_limits)
}
######zipf graficos control
RADmodel_plotc2 <- radfit(plotc2$abundance_relative)
plot(RADmodel_plotc2)
Radmodel_plot2 <- radfit(plotc2$abundance_relative)
plot(Radmodel_plot2,main="plot 2-c")
summary(plotc2)

RADmodel_plotc7 <- radfit(plotc7$abundance_relative)
plot(RADmodel_plotc7)
Radmodel_plot7 <- radfit(plotc7$abundance_relative)
plot(Radmodel_plot7,main="plot 7-c",  ylim = y_limits_c7)
summary(plotc7)

RADmodel_plotc11 <- radfit(plotc11$abundance_relative)
plot(RADmodel_plotc11)
Radmodel_plot11 <- radfit(plotc11$abundance_relative)
plot(Radmodel_plot11,main="plot 11-c",  ylim = y_limits_c11)
summary(plotc11)

RADmodel_plotc14 <- radfit(plotc14$abundance_relative)
plot(RADmodel_plotc14)
Radmodel_plot14 <- radfit(plotc14$abundance_relative)
plot(Radmodel_plot14,main="plot 14-c",  ylim = y_limits_c14)
summary(plotc14)
######zipf graficos perturbacion
RADmodel_plotp3 <- radfit(plotp3$abundance_relative)
plot(RADmodel_plotp3)
Radmodel_plot3 <- radfit(plotp3$abundance_relative)
plot(Radmodel_plot3,main="plot 3-p",  ylim = y_limits_p3)
summary(plotp3)
RADmodel_plotp6 <- radfit(plotp6$abundance_relative)
plot(RADmodel_plotp6)
Radmodel_plot6 <- radfit(plotp6$abundance_relative)
plot(Radmodel_plot6,main="plot 6-p",  ylim = y_limits_p6)
summary(plotp6)
RADmodel_plotp10 <- radfit(plotp10$abundance_relative)
plot(RADmodel_plotp10)
Radmodel_plot10 <- radfit(plotp10$abundance_relative)
plot(Radmodel_plot10,main="plot 10-p",  ylim = y_limits_p10)
summary(plotp10)
RADmodel_plotp15 <- radfit(plotp15$abundance_relative)
plot(RADmodel_plotp15)
Radmodel_plot15 <- radfit(plotp15$abundance_relative)
plot(Radmodel_plot15,main="plot 15-p",  ylim = y_limits_p15)
summary(plotp15)
######zipf graficos termico
RADmodel_plotw1 <- radfit(plotw1$abundance_relative)
plot(RADmodel_plotw1)
Radmodel_plotw1 <- radfit(plotw1$abundance_relative)
plot(Radmodel_plotw1,main="plot 1-w",  ylim = y_limits_w1)
summary(plotw1)
RADmodel_plotw8 <- radfit(plotw8$abundance_relative)
plot(RADmodel_plotw8)
Radmodel_plotw8 <- radfit(plotw8$abundance_relative)
plot(Radmodel_plotw8,main="plot 8-w",  ylim = y_limits_w8)
summary(plotw8)
RADmodel_plotw9 <- radfit(plotw9$abundance_relative)
plot(RADmodel_plotw9)
Radmodel_plotw9 <- radfit(plotw9$abundance_relative)
plot(Radmodel_plotw9,main="plot 9-w",  ylim = y_limits_w9)
summary(plotw9)
RADmodel_plotw16 <- radfit(plotw16$abundance_relative)
plot(RADmodel_plotw16)
Radmodel_plotw16 <- radfit(plotw16$abundance_relative)
plot(Radmodel_plotw16,main="plot 16-w",  ylim = y_limits_w16)
summary(plotw16)

######zipf graficos perturbacion+termico
RADmodel_plotwp4 <- radfit(plotwp4$abundance_relative)
plot(RADmodel_plotwp4)
Radmodel_plotwp4 <- radfit(plotwp4$abundance_relative)
plot(Radmodel_plotwp4,main="plot 4-wp",  ylim = y_limits_wp4)
summary(plotwp4)

RADmodel_plotwp5 <- radfit(plotwp5$abundance_relative)
plot(RADmodel_plotwp5)
Radmodel_plotwp5 <- radfit(plotwp5$abundance_relative)
plot(Radmodel_plotwp5,main="plot 5-wp",  ylim = y_limits_wp5)
summary(plotwp5)

RADmodel_plotwp12 <- radfit(plotwp12$abundance_relative)
plot(RADmodel_plotwp12)
Radmodel_plotwp12 <- radfit(plotwp12$abundance_relative)
plot(Radmodel_plotwp12,main="plot 12-wp",  ylim = y_limits_wp12)
summary(plotwp12)

RADmodel_plotwp13 <- radfit(plotwp13$abundance_relative)
plot(RADmodel_plotwp13)
Radmodel_plotwp13 <- radfit(plotwp13$abundance_relative)
plot(Radmodel_plotwp13,main="plot 13-wp",  ylim = y_limits_wp13)
summary(plotwp13)

#####zipf muestero x tratamiento
####hacer un archivo por sampling
treatment_c$sampling <- trimws(treatment_c$sampling)
sampling0c <- treatment_c[treatment_c$sampling == "s0", ]
treatment_c$sampling <- trimws(treatment_c$sampling)
sampling1c <- treatment_c[treatment_c$sampling == "s1", ]
treatment_c$sampling <- trimws(treatment_c$sampling)
sampling2c <- treatment_c[treatment_c$sampling == "s2", ]
treatment_c$sampling <- trimws(treatment_c$sampling)
sampling3c <- treatment_c[treatment_c$sampling == "s3", ]
treatment_c$sampling <- trimws(treatment_c$sampling)
sampling4c <- treatment_c[treatment_c$sampling == "s4", ]
treatment_c$sampling <- trimws(treatment_c$sampling)
sampling5c <- treatment_c[treatment_c$sampling == "s5", ]
treatment_c$sampling <- trimws(treatment_c$sampling)
sampling6c <- treatment_c[treatment_c$sampling == "s6", ]
treatment_c$sampling <- trimws(treatment_c$sampling)
sampling7c <- treatment_c[treatment_c$sampling == "s7", ]
treatment_c$sampling <- trimws(treatment_c$sampling)
sampling8c <- treatment_c[treatment_c$sampling == "s8", ]
treatment_c$sampling <- trimws(treatment_c$sampling)
sampling9c <- treatment_c[treatment_c$sampling == "s9", ]
treatment_c$sampling <- trimws(treatment_c$sampling)
sampling10c <- treatment_c[treatment_c$sampling == "s10", ]
treatment_c$sampling <- trimws(treatment_c$sampling)
sampling11c <- treatment_c[treatment_c$sampling == "s11", ]

treatment_p$sampling <- trimws(treatment_p$sampling)
sampling0p <- treatment_p[treatment_p$sampling == "s0", ]
treatment_p$sampling <- trimws(treatment_p$sampling)
sampling1p<- treatment_p[treatment_p$sampling == "s1", ]
treatment_p$sampling <- trimws(treatment_p$sampling)
sampling2p <- treatment_p[treatment_p$sampling == "s2", ]
treatment_p$sampling <- trimws(treatment_p$sampling)
sampling3p <- treatment_p[treatment_p$sampling == "s3", ]
treatment_p$sampling <- trimws(treatment_p$sampling)
sampling4p <- treatment_p[treatment_p$sampling == "s4", ]
treatment_p$sampling <- trimws(treatment_p$sampling)
sampling5p <- treatment_p[treatment_p$sampling == "s5", ]
treatment_p$sampling <- trimws(treatment_p$sampling)
sampling6p <- treatment_p[treatment_p$sampling == "s6", ]
treatment_p$sampling <- trimws(treatment_p$sampling)
sampling7p <- treatment_p[treatment_p$sampling == "s7", ]
treatment_p$sampling <- trimws(treatment_p$sampling)
sampling8p <- treatment_p[treatment_p$sampling == "s8", ]
treatment_p$sampling <- trimws(treatment_p$sampling)
sampling9p <- treatment_p[treatment_p$sampling == "s9", ]
treatment_p$sampling <- trimws(treatment_p$sampling)
sampling10p <- treatment_p[treatment_p$sampling == "s10", ]
treatment_p$sampling <- trimws(treatment_p$sampling)
sampling11p <- treatment_p[treatment_p$sampling == "s11", ]

treatment_w$sampling <- trimws(treatment_w$sampling)
sampling0w <- treatment_w[treatment_w$sampling == "s0", ]
treatment_w$sampling <- trimws(treatment_w$sampling)
sampling1w <- treatment_w[treatment_w$sampling == "s1", ]
treatment_w$sampling <- trimws(treatment_w$sampling)
sampling2w <- treatment_w[treatment_w$sampling == "s2", ]
treatment_w$sampling <- trimws(treatment_w$sampling)
sampling3w <- treatment_w[treatment_w$sampling == "s3", ]
treatment_w$sampling <- trimws(treatment_w$sampling)
sampling4w <- treatment_w[treatment_w$sampling == "s4", ]
treatment_w$sampling <- trimws(treatment_w$sampling)
sampling5w <- treatment_w[treatment_w$sampling == "s5", ]
treatment_w$sampling <- trimws(treatment_w$sampling)
sampling6w <- treatment_w[treatment_w$sampling == "s6", ]
treatment_w$sampling <- trimws(treatment_w$sampling)
sampling7w <- treatment_w[treatment_w$sampling == "s7", ]
treatment_w$sampling <- trimws(treatment_w$sampling)
sampling8w <- treatment_w[treatment_w$sampling == "s8", ]
treatment_w$sampling <- trimws(treatment_w$sampling)
sampling9w <- treatment_w[treatment_w$sampling == "s9", ]
treatment_w$sampling <- trimws(treatment_w$sampling)
sampling10w <- treatment_w[treatment_w$sampling == "s10", ]
treatment_w$sampling <- trimws(treatment_w$sampling)
sampling11w <- treatment_w[treatment_w$sampling == "s11", ]

treatment_wp$sampling <- trimws(treatment_wp$sampling)
sampling0wp <- treatment_wp[treatment_wp$sampling == "s0", ]
treatment_wp$sampling <- trimws(treatment_wp$sampling)
sampling1wp <- treatment_wp[treatment_wp$sampling == "s1", ]
treatment_wp$sampling <- trimws(treatment_wp$sampling)
sampling2wp <- treatment_wp[treatment_wp$sampling == "s2", ]
treatment_wp$sampling <- trimws(treatment_wp$sampling)
sampling3wp <- treatment_wp[treatment_wp$sampling == "s3", ]
treatment_wp$sampling <- trimws(treatment_wp$sampling)
sampling4wp <- treatment_wp[treatment_wp$sampling == "s4", ]
treatment_wp$sampling <- trimws(treatment_wp$sampling)
sampling5wp <- treatment_wp[treatment_wp$sampling == "s5", ]
treatment_wp$sampling <- trimws(treatment_wp$sampling)
sampling6wp <- treatment_wp[treatment_wp$sampling == "s6", ]
treatment_wp$sampling <- trimws(treatment_wp$sampling)
sampling7wp <- treatment_wp[treatment_wp$sampling == "s7", ]
treatment_wp$sampling <- trimws(treatment_wp$sampling)
sampling8wp <- treatment_wp[treatment_wp$sampling == "s8", ]
treatment_wp$sampling <- trimws(treatment_wp$sampling)
sampling9wp <- treatment_wp[treatment_wp$sampling == "s9", ]
treatment_wp$sampling <- trimws(treatment_wp$sampling)
sampling10wp <- treatment_wp[treatment_wp$sampling == "s10", ]
treatment_wp$sampling <- trimws(treatment_wp$sampling)
sampling11wp <- treatment_wp[treatment_wp$sampling == "s11", ]

####sumar abundancias de especies del tratamiento entero de cada uno de los samplings
# Calcular la suma de abundancias por especie 
abundance_by_speciesc0 <- aggregate(abundance ~ species, data = sampling0c, sum)
abundance_by_speciesc1 <- aggregate(abundance ~ species, data = sampling1c, sum)
abundance_by_speciesc2 <- aggregate(abundance ~ species, data = sampling2c, sum)
abundance_by_speciesc3 <- aggregate(abundance ~ species, data = sampling3c, sum)
abundance_by_speciesc4 <- aggregate(abundance ~ species, data = sampling4c, sum)
abundance_by_speciesc5 <- aggregate(abundance ~ species, data = sampling5c, sum)
abundance_by_speciesc6 <- aggregate(abundance ~ species, data = sampling6c, sum)
abundance_by_speciesc7 <- aggregate(abundance ~ species, data = sampling7c, sum)
abundance_by_speciesc8 <- aggregate(abundance ~ species, data = sampling8c, sum)
abundance_by_speciesc9 <- aggregate(abundance ~ species, data = sampling9c, sum)
abundance_by_speciesc10 <- aggregate(abundance ~ species, data = sampling10c, sum)
abundance_by_speciesc11 <- aggregate(abundance ~ species, data = sampling11c, sum)

abundance_by_speciesp0 <- aggregate(abundance ~ species, data = sampling0p, sum)
abundance_by_speciesp1 <- aggregate(abundance ~ species, data = sampling1p, sum)
abundance_by_speciesp2 <- aggregate(abundance ~ species, data = sampling2p, sum)
abundance_by_speciesp3 <- aggregate(abundance ~ species, data = sampling3p, sum)
abundance_by_speciesp4 <- aggregate(abundance ~ species, data = sampling4p, sum)
abundance_by_speciesp5 <- aggregate(abundance ~ species, data = sampling5p, sum)
abundance_by_speciesp6 <- aggregate(abundance ~ species, data = sampling6p, sum)
abundance_by_speciesp7 <- aggregate(abundance ~ species, data = sampling7p, sum)
abundance_by_speciesp8 <- aggregate(abundance ~ species, data = sampling8p, sum)
abundance_by_speciesp9 <- aggregate(abundance ~ species, data = sampling9p, sum)
abundance_by_speciesp10 <- aggregate(abundance ~ species, data = sampling10p, sum)
abundance_by_speciesp11 <- aggregate(abundance ~ species, data = sampling11p, sum)

abundance_by_speciesw0 <- aggregate(abundance ~ species, data = sampling0w, sum)
abundance_by_speciesw1 <- aggregate(abundance ~ species, data = sampling1w, sum)
abundance_by_speciesw2 <- aggregate(abundance ~ species, data = sampling2w, sum)
abundance_by_speciesw3 <- aggregate(abundance ~ species, data = sampling3w, sum)
abundance_by_speciesw4 <- aggregate(abundance ~ species, data = sampling4w, sum)
abundance_by_speciesw5 <- aggregate(abundance ~ species, data = sampling5w, sum)
abundance_by_speciesw6 <- aggregate(abundance ~ species, data = sampling6w, sum)
abundance_by_speciesw7 <- aggregate(abundance ~ species, data = sampling7w, sum)
abundance_by_speciesw8 <- aggregate(abundance ~ species, data = sampling8w, sum)
abundance_by_speciesw9 <- aggregate(abundance ~ species, data = sampling9w, sum)
abundance_by_speciesw10 <- aggregate(abundance ~ species, data = sampling10w, sum)
abundance_by_speciesw11 <- aggregate(abundance ~ species, data = sampling11w, sum)

abundance_by_specieswp0 <- aggregate(abundance ~ species, data = sampling0wp, sum)
abundance_by_specieswp1 <- aggregate(abundance ~ species, data = sampling1wp, sum)
abundance_by_specieswp2 <- aggregate(abundance ~ species, data = sampling2wp, sum)
abundance_by_specieswp3 <- aggregate(abundance ~ species, data = sampling3wp, sum)
abundance_by_specieswp4 <- aggregate(abundance ~ species, data = sampling4wp, sum)
abundance_by_specieswp5 <- aggregate(abundance ~ species, data = sampling5wp, sum)
abundance_by_specieswp6 <- aggregate(abundance ~ species, data = sampling6wp, sum)
abundance_by_specieswp7 <- aggregate(abundance ~ species, data = sampling7wp, sum)
abundance_by_specieswp8 <- aggregate(abundance ~ species, data = sampling8wp, sum)
abundance_by_specieswp9 <- aggregate(abundance ~ species, data = sampling9wp, sum)
abundance_by_specieswp10 <- aggregate(abundance ~ species, data = sampling10wp, sum)
abundance_by_specieswp11 <- aggregate(abundance ~ species, data = sampling11wp, sum)

###calculo de abundancias absolutas y relativas
##calculo de la abundancia total en cada tratamiento y luego de las abundancias relativas
totalabundance0c <- sum(abundance_by_speciesc0$abundance)
totalabundance1c <- sum(abundance_by_speciesc1$abundance)
totalabundance2c <- sum(abundance_by_speciesc2$abundance)
totalabundance3c <- sum(abundance_by_speciesc3$abundance)
totalabundance4c <- sum(abundance_by_speciesc4$abundance)
totalabundance5c <- sum(abundance_by_speciesc5$abundance)
totalabundance6c <- sum(abundance_by_speciesc6$abundance)
totalabundance7c <- sum(abundance_by_speciesc7$abundance)
totalabundance8c <- sum(abundance_by_speciesc8$abundance)
totalabundance9c <- sum(abundance_by_speciesc9$abundance)
totalabundance10c <- sum(abundance_by_speciesc10$abundance)
totalabundance11c <- sum(abundance_by_speciesc11$abundance)

totalabundance0p<-sum(abundance_by_speciesp0$abundance)
totalabundance1p <- sum(abundance_by_speciesp1$abundance)
totalabundance2p <- sum(abundance_by_speciesp2$abundance)
totalabundance3p <- sum(abundance_by_speciesp3$abundance)
totalabundance4p <- sum(abundance_by_speciesp4$abundance)
totalabundance5p <- sum(abundance_by_speciesp5$abundance)
totalabundance6p <- sum(abundance_by_speciesp6$abundance)
totalabundance7p <- sum(abundance_by_speciesp7$abundance)
totalabundance8p <- sum(abundance_by_speciesp8$abundance)
totalabundance9p <- sum(abundance_by_speciesp9$abundance)
totalabundance10p <- sum(abundance_by_speciesp10$abundance)
totalabundance11p <- sum(abundance_by_speciesp11$abundance)

totalabundance0w <- sum(abundance_by_speciesw0$abundance)
totalabundance1w <- sum(abundance_by_speciesw1$abundance)
totalabundance2w <- sum(abundance_by_speciesw2$abundance)
totalabundance3w <- sum(abundance_by_speciesw3$abundance)
totalabundance4w <- sum(abundance_by_speciesw4$abundance)
totalabundance5w <- sum(abundance_by_speciesw5$abundance)
totalabundance6w <- sum(abundance_by_speciesw6$abundance)
totalabundance7w <- sum(abundance_by_speciesw7$abundance)
totalabundance8w <- sum(abundance_by_speciesw8$abundance)
totalabundance9w <- sum(abundance_by_speciesw9$abundance)
totalabundance10w <- sum(abundance_by_speciesw10$abundance)
totalabundance11w <- sum(abundance_by_speciesw11$abundance)

totalabundance0wp <- sum(abundance_by_specieswp0$abundance)
totalabundance1wp <- sum(abundance_by_specieswp1$abundance)
totalabundance2wp <- sum(abundance_by_specieswp2$abundance)
totalabundance3wp <- sum(abundance_by_specieswp3$abundance)
totalabundance4wp <- sum(abundance_by_specieswp4$abundance)
totalabundance5wp <- sum(abundance_by_specieswp5$abundance)
totalabundance6wp <- sum(abundance_by_specieswp6$abundance)
totalabundance7wp <- sum(abundance_by_specieswp7$abundance)
totalabundance8wp <- sum(abundance_by_specieswp8$abundance)
totalabundance9wp <- sum(abundance_by_specieswp9$abundance)
totalabundance10wp <- sum(abundance_by_specieswp10$abundance)
totalabundance11wp <- sum(abundance_by_specieswp11$abundance)
#####calculo de abundancias relativas
# Paso 2: Calcular la abundancia relativa (abundancia de cada especie/abundancia total del tratamiento)
abundance_by_speciesc0$abundance_relative <- abundance_by_speciesc0$abundance / totalabundance0c
abundance_by_speciesc1$abundance_relative<-abundance_by_speciesc1$abundance/totalabundance1c
abundance_by_speciesc2$abundance_relative<-abundance_by_speciesc2$abundance/totalabundance2c
abundance_by_speciesc3$abundance_relative<-abundance_by_speciesc3$abundance/totalabundance3c
abundance_by_speciesc4$abundance_relative<-abundance_by_speciesc4$abundance/totalabundance4c
abundance_by_speciesc5$abundance_relative<-abundance_by_speciesc5$abundance/totalabundance5c
abundance_by_speciesc6$abundance_relative<-abundance_by_speciesc6$abundance/totalabundance6c
abundance_by_speciesc7$abundance_relative<-abundance_by_speciesc7$abundance/totalabundance7c
abundance_by_speciesc8$abundance_relative<-abundance_by_speciesc8$abundance/totalabundance8c
abundance_by_speciesc9$abundance_relative<-abundance_by_speciesc9$abundance/totalabundance9c
abundance_by_speciesc10$abundance_relative<-abundance_by_speciesc10$abundance/totalabundance10c
abundance_by_speciesc11$abundance_relative<-abundance_by_speciesc11$abundance/totalabundance11c

abundance_by_speciesp0$abundance_relative <- abundance_by_speciesp0$abundance / totalabundance0c
abundance_by_speciesp1$abundance_relative<-abundance_by_speciesp1$abundance/totalabundance1p
abundance_by_speciesp2$abundance_relative<-abundance_by_speciesp2$abundance/totalabundance2p
abundance_by_speciesp3$abundance_relative<-abundance_by_speciesp3$abundance/totalabundance3p
abundance_by_speciesp4$abundance_relative<-abundance_by_speciesp4$abundance/totalabundance4p
abundance_by_speciesp5$abundance_relative<-abundance_by_speciesp5$abundance/totalabundance5p
abundance_by_speciesp6$abundance_relative<-abundance_by_speciesp6$abundance/totalabundance6p
abundance_by_speciesp7$abundance_relative<-abundance_by_speciesp7$abundance/totalabundance7p
abundance_by_speciesp8$abundance_relative<-abundance_by_speciesp8$abundance/totalabundance8p
abundance_by_speciesp9$abundance_relative<-abundance_by_speciesp9$abundance/totalabundance9p
abundance_by_speciesp10$abundance_relative<-abundance_by_speciesp10$abundance/totalabundance10p
abundance_by_speciesp11$abundance_relative<-abundance_by_speciesp11$abundance/totalabundance11p

abundance_by_speciesw0$abundance_relative <- abundance_by_speciesw0$abundance /totalabundance0w
abundance_by_speciesw1$abundance_relative<-abundance_by_speciesw1$abundance/totalabundance1w
abundance_by_speciesw2$abundance_relative<-abundance_by_speciesw2$abundance/totalabundance2w
abundance_by_speciesw3$abundance_relative<-abundance_by_speciesw3$abundance/totalabundance3w
abundance_by_speciesw4$abundance_relative<-abundance_by_speciesw4$abundance/totalabundance4w
abundance_by_speciesw5$abundance_relative<-abundance_by_speciesw5$abundance/totalabundance5w
abundance_by_speciesw6$abundance_relative<-abundance_by_speciesw6$abundance/totalabundance6w
abundance_by_speciesw7$abundance_relative<-abundance_by_speciesw7$abundance/totalabundance7w
abundance_by_speciesw8$abundance_relative<-abundance_by_speciesw8$abundance/totalabundance8w
abundance_by_speciesw9$abundance_relative<-abundance_by_speciesw9$abundance/totalabundance9w
abundance_by_speciesw10$abundance_relative<-abundance_by_speciesw10$abundance/totalabundance10w
abundance_by_speciesw11$abundance_relative<-abundance_by_speciesw11$abundance/totalabundance11w

abundance_by_specieswp0$abundance_relative <- abundance_by_specieswp0$abundance / totalabundance0wp
abundance_by_specieswp1$abundance_relative<-abundance_by_specieswp1$abundance/totalabundance1wp
abundance_by_specieswp2$abundance_relative<-abundance_by_specieswp2$abundance/totalabundance2wp
abundance_by_specieswp3$abundance_relative<-abundance_by_specieswp3$abundance/totalabundance3wp
abundance_by_specieswp4$abundance_relative<-abundance_by_specieswp4$abundance/totalabundance4wp
abundance_by_specieswp5$abundance_relative<-abundance_by_specieswp5$abundance/totalabundance5wp
abundance_by_specieswp6$abundance_relative<-abundance_by_specieswp6$abundance/totalabundance6wp
abundance_by_specieswp7$abundance_relative<-abundance_by_specieswp7$abundance/totalabundance7wp
abundance_by_specieswp8$abundance_relative<-abundance_by_specieswp8$abundance/totalabundance8wp
abundance_by_specieswp9$abundance_relative<-abundance_by_specieswp9$abundance/totalabundance9wp
abundance_by_specieswp10$abundance_relative<-abundance_by_specieswp10$abundance/totalabundance10wp
abundance_by_specieswp11$abundance_relative<-abundance_by_specieswp11$abundance/totalabundance11wp

#######diagramas zipf por muesteo y por tratamiento
RADmodel_samplingc0 <- radfit(abundance_by_speciesc0$abundance_relative)
plot(RADmodel_samplingc0)
RADmodel_samplingc0 <- radfit(abundance_by_speciesc0$abundance_relative)
plot(RADmodel_samplingc0,main="sampling 0-c")
