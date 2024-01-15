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

######zipf graficos control
RADmodel_plotc2 <- radfit(plotc2$abundance_relative)
plot(RADmodel_plotc2)
Radmodel_plot2 <- radfit(plotc2$abundance_relative)
plot(Radmodel_plot2,main="plot 2-c")
summary(plotc2)

RADmodel_plotc7 <- radfit(plotc7$abundance_relative)
plot(RADmodel_plotc7)
Radmodel_plot7 <- radfit(plotc7$abundance_relative)
plot(Radmodel_plot7,main="plot 7-c")
summary(plotc7)

RADmodel_plotc11 <- radfit(plotc11$abundance_relative)
plot(RADmodel_plotc11)
Radmodel_plot11 <- radfit(plotc11$abundance_relative)
plot(Radmodel_plot11,main="plot 11-c")
summary(plotc11)

RADmodel_plotc14 <- radfit(plotc14$abundance_relative)
plot(RADmodel_plotc14)
Radmodel_plot14 <- radfit(plotc14$abundance_relative)
plot(Radmodel_plot14,main="plot 14-c")
summary(plotc14)
######zipf graficos perturbacion
RADmodel_plotp3 <- radfit(plotp3$abundance_relative)
plot(RADmodel_plotp3)
Radmodel_plot3 <- radfit(plotp3$abundance_relative)
plot(Radmodel_plot3,main="plot 3-p")
summary(plotp3)
RADmodel_plotp6 <- radfit(plotp6$abundance_relative)
plot(RADmodel_plotp6)
Radmodel_plot6 <- radfit(plotp6$abundance_relative)
plot(Radmodel_plot6,main="plot 6-p")
summary(plotp6)
RADmodel_plotp10 <- radfit(plotp10$abundance_relative)
plot(RADmodel_plotp10)
Radmodel_plot10 <- radfit(plotp10$abundance_relative)
plot(Radmodel_plot10,main="plot 10-p")
summary(plotp10)
RADmodel_plotp15 <- radfit(plotp15$abundance_relative)
plot(RADmodel_plotp15)
Radmodel_plot15 <- radfit(plotp15$abundance_relative)
plot(Radmodel_plot15,main="plot 15-p")
summary(plotp15)
######zipf graficos termico
RADmodel_plotw1 <- radfit(plotw1$abundance_relative)
plot(RADmodel_plotw1)
Radmodel_plotw1 <- radfit(plotw1$abundance_relative)
plot(Radmodel_plotw1,main="plot 1-w")
summary(plotw1)
RADmodel_plotw8 <- radfit(plotw8$abundance_relative)
plot(RADmodel_plotw8)
Radmodel_plotw8 <- radfit(plotw8$abundance_relative)
plot(Radmodel_plotw8,main="plot 8-w")
summary(plotw8)
RADmodel_plotw9 <- radfit(plotw9$abundance_relative)
plot(RADmodel_plotw9)
Radmodel_plotw9 <- radfit(plotw9$abundance_relative)
plot(Radmodel_plotw9,main="plot 9-w")
summary(plotw9)
RADmodel_plotw16 <- radfit(plotw16$abundance_relative)
plot(RADmodel_plotw16)
Radmodel_plotw16 <- radfit(plotw16$abundance_relative)
plot(Radmodel_plotw16,main="plot 16-w")
summary(plotw16)

######zipf graficos perturbacion+termico
RADmodel_plotwp4 <- radfit(plotwp4$abundance_relative)
plot(RADmodel_plotwp4)
Radmodel_plotwp4 <- radfit(plotwp4$abundance_relative)
plot(Radmodel_plotwp4,main="plot 4-wp")
summary(plotwp4)

RADmodel_plotwp5 <- radfit(plotwp5$abundance_relative)
plot(RADmodel_plotwp5)
Radmodel_plotwp5 <- radfit(plotwp5$abundance_relative)
plot(Radmodel_plotwp5,main="plot 5-wp")
summary(plotwp5)

RADmodel_plotwp12 <- radfit(plotwp12$abundance_relative)
plot(RADmodel_plotwp12)
Radmodel_plotwp12 <- radfit(plotwp12$abundance_relative)
plot(Radmodel_plotwp12,main="plot 12-wp")
summary(plotwp12)

RADmodel_plotwp13 <- radfit(plotwp13$abundance_relative)
plot(RADmodel_plotwp13)
Radmodel_plotwp13 <- radfit(plotwp13$abundance_relative)
plot(Radmodel_plotwp13,main="plot 13-wp")
summary(plotwp13)

