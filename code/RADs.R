#load packages
library(tidyverse)
library(vegan)

#load database
source("code/first_script.R")
#subset df sampling (do not run if the whole database is needed)
#flora <- flora[which(flora$sampling == "sampling 2"),]

#reshape df (species as columns, treatments as rows)
flora <- flora %>% select(plot,sampling, treatment, species, abundance)

#We've detected that "radfit" does not like decimals. THe only decimal numbers that we have in the
#database are between 0 and 1. So we round these numbers to 1. 

flora <- mutate(flora, abundance = ifelse(abundance < 1, 1, abundance))

#Summarising: calculating abundances per sampling and treatment by aggregating the 4 replicates/plots

# !! CHECKING: Not sure of this. Check with Dani or Rodrigo. 
# Mean abundance of 4 replicates per treatment and sampling.
flora_t <- summarise(group_by(flora, treatment, sampling, species),
                   abundance = round((mean(abundance, na.rm = T))))
#Summing each mean value per sampling (cumulative abundance through time) 
flora_t <- summarise(group_by(flora_t, treatment, species),
                     abundance = round((sum(abundance, na.rm = T))))

flora_t_wid <- pivot_wider(flora_t, names_from = species, values_from = abundance, values_fill = 0)
flora_t_wid <- as.data.frame(flora_t_wid)
#remove treatment from column and add it as rowname
rownames(flora_t_wid) <- flora_t_wid$treatment
flora_t_wid <- flora_t_wid[,-1]

#RADs (run only one treatment at a time to see AIC)
# 1 = control, 2 = perturbation, 3 = warming, 4 = both

plot(radfit(flora_t_wid))

(rad_c <- radfit(round(flora_t_wid[1,]))); plot(rad_c)
# best fitting model: Preemption
# aparecen 32 warnings


(rad_p <- radfit(round(flora_t_wid[2,]))); plot(rad_p)
# best fitting model: Preemption

(rad_w <- radfit(round(flora_t_wid[3,]))); plot(rad_w)
# best fitting model: Preemption

(rad_wp <- radfit(round(flora_t_wid[4,]))); plot(rad_wp)
# best fitting model: Mandelbrot


# There is no consensus for the best fitting model per treatment. So we have to apply the "radfit" function
# to the whole dataset, aggregating also the treatments. #

# # !! CHECKING
# mean abundance for each species within each sampling and treatment (mean for plots)
flora_all <- summarise(group_by(flora, sampling, treatment, species),
                     abundance = round((mean(abundance, na.rm = T))))
# mean abundance for each species within each sampling (mean for treatments)
flora_all <- summarise(group_by(flora_all, sampling, species),
                       abundance = round((mean(abundance, na.rm = T))))
#cumulative abundance for each species through samplings
flora_all <- summarise(group_by(flora_all, species),
                       abundance = round((sum(abundance, na.rm = T))))

flora_all <- pivot_wider(flora_all, names_from = species, values_from = abundance, values_fill = 0)
flora_all <- as.data.frame(flora_all)
#remove treatment from column and add it as rowname
rownames(flora_all) <- "all"
flora_all <- flora_all[,-1]

(rad_all <- radfit(flora_all)); plot(rad_all)

#best fitting model: Mandelbrot

#Since 3 out 4 treatments had lognormal as best-fitting model and the general application of radfit
# to the data provides lognormal as the best-fitting model, lognormal is going to be the model used for the
# evenness analysis

# Chosen model: lognormal

#We need to generate a data.frame to apply this code: 

# ggplot(flora_samplings, aes(x = sampling, y = evenness_index, color = treatment)) +
#  geom_boxplot() +
#  labs(x = " ", y = "Evenness in abundance distribution") +
#  facet_grid(~ treatment) + 
#  scale_color_manual(values = c("c" = "green", "p" = "blue", "w" = "red", "wp" = "purple"))+
#  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
#  theme(legend.position = "bottom", 
#        axis.text = element_text(size = 12))

#Try to create a loop

# individual content of loop ?

subset_try <- subset(flora, treatment == "w" & sampling == "4")
subtry <- summarise(group_by(subset_try, species), #mean of replicates per sampling
                    abundance = round((mean(abundance, na.rm = T))))

subtry <- pivot_wider(subtry, names_from = species, values_from = abundance, values_fill = 0)
subtry <- as.data.frame(subtry)
#remove treatment from column and add it as rowname
rownames(subtry) <- "wp"
rad_try <- radfit(subtry)

rad_try
(rad_try <- radfit(subtry)); plot(rad_try)

rad_try$models$Lognormal

rad_try$models$Lognormal$aic
rad_try$models$Lognormal$coefficients[1]
rad_try$models$Lognormal$coefficients[2]
rad_try$models$Lognormal$deviance
rad_try$models$Lognormal

rad_try$models$Zipf$coefficients[2]


flora_no1 <- flora[flora$sampling != "1", ]

#Create data.frame to fill with data. 
rad_df <- matrix(nrow = (length(unique(flora_no1$sampling)) * length(unique(flora_no1$treatment))), ncol = 6)
colnames(rad_df) <- c("treatment", "sampling", "AIC_pree", "AIC_log", "AIC_zipf", "AIC_man")
rad_df <- as.data.frame(rad_df)


f <- 0
for(i in 1:length(unique(flora_no1$treatment))){
  for(j in 1:length(unique(flora_no1$sampling))){
    
    f <- f + 1
    subset_data <- subset(flora_no1, treatment == unique(flora_no1$treatment)[i] & sampling == unique(flora_no1$sampling)[j])
    
    sub <- summarise(group_by(subset_data, species), abundance = round((mean(abundance, na.rm = T))))
    sub <- pivot_wider(sub, names_from = species, values_from = abundance, values_fill = 0)
    sub <- as.data.frame(sub)
    rownames(sub) <- unique(flora_no1$treatment)[i]
    rad_sub <- radfit(sub)
    
    rad_df$treatment[f] <- unique(flora_no1$treatment)[i]
    rad_df$sampling[f] <- unique(flora_no1$sampling)[j]
    rad_df$AIC_pree[f] <- rad_sub$models$Preemption$aic
    rad_df$AIC_log[f] <- rad_sub$models$Lognormal$aic
    rad_df$AIC_zipf[f] <- rad_sub$models$Zipf$aic
    rad_df$AIC_man[f] <- rad_sub$models$Mandelbrot$aic
  }
}


rad_df <- pivot_longer(rad_df, cols = c("AIC_pree", "AIC_log","AIC_zipf","AIC_man"), 
             names_to = "model", values_to = "AIC")


ggplot(rad_df, aes(x = model, y = AIC)) +
  geom_boxplot()


# Hacer loop a nivel de plot*sampling para aprender yo a hacer el loop

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
    rownames(subrad) <- unique(flora_no1$sampling)[i]
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

# Even less differences!


# NOW: LETS DO A LOOP TO TAKE ALL ZIPF PAR 1!!!!

zipf_df <- matrix(nrow = (length(unique(flora_no1$sampling))*length(unique(flora_no1$plot))), ncol = 3)
colnames(zipf_df) <- c("plot", "sampling", "Y_zipf")
zipf_df <- as.data.frame(zipf_df)

count = 0
for(i in 1:length(unique(flora_no1$sampling))){
  for(j in 1:length(unique(flora_no1$plot))){
    
    count <- count + 1
    subset_data <- subset(flora_no1, sampling == unique(flora_no1$sampling)[i] & plot == unique(flora_no1$plot)[j])
    subrad <- summarise(group_by(subset_data, species),
                        abundance = round(mean(abundance), 0))
    subrad <- pivot_wider(subrad, names_from = species, values_from = abundance, values_fill = 0)
    subrad <- as.data.frame(subrad)
    rownames(subrad) <- unique(flora_no1$sampling)[i]
    rad_sub <- radfit(subrad)
    
    zipf_df$plot[count] <- unique(flora_no1$plot)[j]
    zipf_df$sampling[count] <- unique(flora_no1$sampling)[i]
    zipf_df$Y_zipf[count] <- rad_sub$models$Zipf$coefficients[2]
  }
}

plots <- read.csv("data/plots.csv") %>%
  select(nplot, treatment_code)
colnames(plots) <- c("plot", "treatment")

zipf_df <- merge(zipf_df, plots, by = "plot")


# por qué sale mal?
ggplot(zipf_df, aes(x = sampling, y = Y_zipf, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Y_zipf") +
  facet_grid(~ treatment) + 
  scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  theme(legend.position = "NULL")
