#load packages
library(tidyverse)
library(vegan)
library(ggpubr)

#load database
source("code/first_script.R")
#subset df sampling (do not run if the whole database is needed)
#flora <- flora[which(flora$sampling == "sampling 2"),]

#reshape df (species as columns, treatments as rows)
flora <- flora %>% select(plot,sampling, treatment, species, abundance)

#We've detected that "radfit" does not like decimals. THe only decimal numbers that we have in the
#database are between 0 and 1. So we round these numbers to 1. 
flora <- mutate(flora, abundance = ifelse(abundance < 1, 1, abundance))


# Which model we are going to use for the entire dataset? #########

#We have to delete sampling = 1 because there are no species at treatment p and wp. If i delete only s = 1 for t = p and wp
# and I leave t= c and w, the loop doesn't work

flora_no1 <- flora[flora$sampling != "1", ]
#Sampling 1 gives problems since there are no species for p and wp

rad_dfplot <- matrix(nrow = (length(unique(flora_no1$sampling))*length(unique(flora_no1$plot))), ncol = 6)
colnames(rad_dfplot) <- c("sampling", "plot", "AIC_pree", "AIC_log", "AIC_zipf", "AIC_man")
rad_dfplot <-  as.data.frame(rad_dfplot)

count <- 0
for (i in 1:length(unique(flora_no1$sampling))){
  for (j in 1:length(unique(flora_no1$plot))){
    
    count <- count + 1
    subset_data <- subset(flora_no1, sampling == unique(flora_no1$sampling)[i] & plot == unique(flora_no1$plot)[j])
    subrad <- summarise(group_by(subset_data, species),
                        abundance = round(mean(abundance), 0))
    subrad <- pivot_wider(subrad, names_from = species, values_from = abundance, values_fill = 0)
    subrad <- as.data.frame(subrad)
    rad_sub <- radfit(subrad)
    
    rad_dfplot$sampling[count] <- unique(flora_no1$sampling)[i]
    rad_dfplot$plot[count] <- unique(flora_no1$plot)[j]
    rad_dfplot$AIC_pree[count] <- rad_sub$models$Preemption$aic
    rad_dfplot$AIC_log[count] <- rad_sub$models$Lognormal$aic
    rad_dfplot$AIC_zipf[count] <- rad_sub$models$Zipf$aic
    rad_dfplot$AIC_man[count] <- rad_sub$models$Mandelbrot$aic
    
  }
}

rad_dfplot <- pivot_longer(rad_dfplot, cols = c("AIC_pree", "AIC_log","AIC_zipf","AIC_man"), 
                           names_to = "model", values_to = "AIC")
ggplot(rad_dfplot, aes(x = model, y = AIC))+
  geom_boxplot()

# No differences. We decide to use zipf because it only has one explanatory coefficient of the curve (gamma)


# Applying Zipf to the dataset: ############

radcoeff_df <- matrix(nrow = (length(unique(flora_no1$sampling))*length(unique(flora_no1$plot))), ncol = 5)
colnames(radcoeff_df) <- c("plot", "sampling", "Y_zipf", "mu_log", "sigma_log")
radcoeff_df <- as.data.frame(radcoeff_df)

count = 0
for(i in 1:length(unique(flora_no1$sampling))){
  for(j in 1:length(unique(flora_no1$plot))){
    
    count <- count + 1
    subset_data <- subset(flora_no1, sampling == unique(flora_no1$sampling)[i] & plot == unique(flora_no1$plot)[j])
    subrad <- summarise(group_by(subset_data, species),
                        abundance = round(mean(abundance), 0))
    subrad <- pivot_wider(subrad, names_from = species, values_from = abundance, values_fill = 0)
    subrad <- as.data.frame(subrad)
    rad_sub <- radfit(subrad)
    
    radcoeff_df$plot[count] <- unique(flora_no1$plot)[j]
    radcoeff_df$sampling[count] <- unique(flora_no1$sampling)[i]
    radcoeff_df$Y_zipf[count] <- rad_sub$models$Zipf$coefficients[2]
    radcoeff_df$mu_log[count] <- rad_sub$models$Lognormal$coefficients[1]
    radcoeff_df$sigma_log[count] <- rad_sub$models$Lognormal$coefficients[2]
  }
}


#Plot 15 (t = p) del sampling 3 aparece como NA. Hacer individual dar valores de i = 5 y j = 7
# (*la posicion de los niveles no corresponde con el valor del muestreo o plot). Da valores de "inf". 


# Al hacer el loop los samplings van del 1 al 12 en vez del 0 al 11
radcoeff_df$sampling <- factor(as.integer(radcoeff_df$sampling) - 1,
                           levels = 0:max(as.integer(radcoeff_df$sampling)) - 1)
radcoeff_df$plot <- as.factor(radcoeff_df$plot )
radcoeff_df$sampling <- as.factor(radcoeff_df$sampling)

plots <- read.csv("data/plots.csv") %>%
  select(nplot, treatment_code)
colnames(plots) <- c("plot", "treatment")


radcoeff_df <- merge(radcoeff_df, plots, by = "plot")

# Adding by hand sampling == 1 and treatments w and c. ######

flora_s1cw <- flora[(flora$sampling == 1 & (flora$treatment == "c" | flora$treatment == "w")), ]

radcoeff_s1cw <- matrix(nrow = (length(unique(flora_s1cw$sampling))*length(unique(flora_s1cw$plot))), ncol = 5)
colnames(radcoeff_s1cw) <- c("plot", "sampling", "Y_zipf", "mu_log", "sigma_log")
radcoeff_s1cw <- as.data.frame(radcoeff_s1cw)

count = 0 
for(i in 1:length(unique(flora_s1cw$plot))){
  count <- count + 1
  subset_data <- subset(flora_s1cw, sampling == "1" & plot == unique(flora_s1cw$plot)[i])
  subrad <- summarise(group_by(subset_data, species),
                      abundance = round(mean(abundance), 0))
  subrad <- pivot_wider(subrad, names_from = species, values_from = abundance, values_fill = 0)
  subrad <- as.data.frame(subrad)
  rad_sub <- radfit(subrad)
  
  radcoeff_s1cw$plot[count] <- unique(flora_s1cw$plot)[i]
  radcoeff_s1cw$sampling[count] <- "1"
  radcoeff_s1cw$Y_zipf[count] <- rad_sub$models$Zipf$coefficients[2]
  radcoeff_s1cw$mu_log[count] <- rad_sub$models$Lognormal$coefficients[1]
  radcoeff_s1cw$sigma_log[count] <- rad_sub$models$Lognormal$coefficients[2]
}

#15 warnings due to plot 1. But it works

radcoeff_s1cw$plot <- as.factor(radcoeff_s1cw$plot )
radcoeff_s1cw$sampling <- as.factor(radcoeff_s1cw$sampling)

radcoeff_s1cw <- merge(radcoeff_s1cw, plots, by = "plot")


#Adding muestreo 1 de c y w. 

radcoeff_df <- rbind(radcoeff_df, radcoeff_s1cw)
radcoeff_df$treatment <- as.factor(radcoeff_df$treatment)


radcoeff_df %>% write.csv("data/radcoeff_df.csv")


ggarrange(
ggplot(radcoeff_df, aes(x = sampling, y = Y_zipf, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Y_zipf") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  theme(legend.position = "NULL"),

ggplot(radcoeff_df, aes(x = sampling, y = mu_log, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "mu_log") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  theme(legend.position = "NULL"),

ggplot(radcoeff_df, aes(x = sampling, y = sigma_log, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "sigma_log") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  theme(legend.position = "NULL"),
nrow = 3, ncol = 1)


# RR REFERENCE : CONTROL.#####

RR_radcoeff <- radcoeff_df
samps <- unique(RR_radcoeff$sampling)

# Esto funciona, pero no sé introducirlo al loop
#RR_radcoeff$RR_ref_ab[which(RR_radcoeff$sampling == "0")] <- 
#(subset(RR_radcoeff, sampling == "0" & treatment == "c" ))$abundance

# TREATMENTS VS CONTROL: Reference = control (RR_abundance_C and RR_richness_C)
RR_radcoeff$RR_ref_Yzipf_C <- NA
for (i in seq_along(samps)) {
  subset_data <- subset(RR_radcoeff, sampling == samps[i] & treatment == "c")
  yzipf_ref_C <- mean(subset_data$Y_zipf)
  
  RR_radcoeff$RR_ref_Yzipf_C[RR_radcoeff$sampling == samps[i]] <-
    rep(yzipf_ref_C, length(which(RR_radcoeff$sampling == samps[i])))
}

RR_radcoeff$RR_Y_zipf_C <- round(log(RR_radcoeff$Y_zipf/RR_radcoeff$RR_ref_Yzipf_C), 2)

RR_radcoeff$RR_ref_mulog_C <- NA
for (i in samps) {
  subset_data <- subset(RR_radcoeff, sampling == i & treatment == "c")
  RR_radcoeff$RR_ref_mulog_C[RR_radcoeff$sampling == i] <-
    rep(subset_data$mu_log, length(which(RR_radcoeff$sampling == i)))
}

RR_radcoeff$RR_mulog_C <- round(log(RR_radcoeff$mu_log/RR_radcoeff$RR_ref_mulog_C), 2)

RR_radcoeff$RR_ref_sigmalog_C <- NA
for (i in samps) {
  subset_data <- subset(RR_radcoeff, sampling == i & treatment == "c")
  RR_radcoeff$RR_ref_sigmalog_C[RR_radcoeff$sampling == i] <-
    rep(subset_data$sigma_log, length(which(RR_radcoeff$sampling == i)))
}

RR_radcoeff$RR_sigmalog_C <- round(log(RR_radcoeff$sigma_log/RR_radcoeff$RR_ref_sigmalog_C), 2)


