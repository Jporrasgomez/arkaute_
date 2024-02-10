

library(lubridate)
library(dplyr)
library(reshape2)
library(tidyverse)


# Opening and transforming data(opening_floradata.R) ####
flora_raw <- read.csv("data/flora_db_raw.csv")

flora_raw <- flora_raw %>%
  mutate(across(where(is.character), as.factor))

flora_raw$plot <- factor(flora_raw$plot)

#Ordenamos los muestreos por orden (añadir más levels a medida que vaya habiendo más muestreos)
desired_order <- c("0", "1", "2", "3", "4", "5", "6", 
                   "7", "8", "9", "10", "11")

flora_raw$sampling <- factor(flora_raw$sampling, levels = desired_order)


# Adding dates
sampling_dates <- read.csv("data/sampling_dates.csv")
sampling_dates$sampling <- factor(sampling_dates$sampling)

sampling_dates$datenew <-  ymd(sampling_dates$date)
sampling_dates$month <- month(sampling_dates$datenew)
sampling_dates$date <- NULL
sampling_dates$micro.sampling <- NULL
sampling_dates$N.micro <- NULL

sampling_dates <- sampling_dates %>%
  mutate(across(where(is.character), as.factor))

flora_raw <- right_join(flora_raw, sampling_dates, by = join_by(sampling))


flora <- flora_raw %>% select(sampling, plot, treatment, species, abundance, height, Cb, Db, Cm, Dm, datenew, month)

#Sumar 0.01 cm a los diámetros por el error del calibre con el que medimos
flora$Dm <- flora$Dm + 0.01
flora$Db <- flora$Db + 0.01


#flora %>% write.csv("data/flora_db.csv")

rm(flora_raw)
rm(sampling_dates)
rm(desired_order)


