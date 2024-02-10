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

subset_try <- subset(flora, treatment == "w" & sampling == "4jul")
subtry <- summarise(group_by(subset_try, treatment, sampling, species),
                     abundance = round((mean(abundance, na.rm = T))))
#Summing each mean value per sampling (cumulative abundance through time) 
subtry <- summarise(group_by(subtry, treatment, species),
                     abundance = round((sum(abundance, na.rm = T))))

subtry <- pivot_wider(subtry, names_from = species, values_from = abundance, values_fill = 0)
subtry <- as.data.frame(subtry)
#remove treatment from column and add it as rowname
rownames(subtry) <- subtry$treatment
subtry <- subtry[,-1]
rad_try <- radfit(subtry)
rad_try
(rad_try <- radfit(subtry)); plot(rad_try)



 #Create data.frame to fill with data. ONE PER MODEL. ES MUY DIFICIL SI NO. 


rad_df_Lognormal <-  matrix(nrow = (length(unique(flora$sampling))*length(unique(flora$plot))) , ncol = 9) # samplings
colnames(rad_df_Lognormal) <- c("treatment", "sampling","model","par1","par2","par3","Deviance","AIC","BIC")
rad_df_Lognormal <- as.data.frame(rad_df_Lognormal)


# My try

for( treat in unique(flora$treatment)){
  for( samp in unique(flora$sampling)){
    subset_data <- subset(flora, treatment == treat & sampling == samp)
    subtry <- summarise(group_by(subset_data, treatment, sampling, species),
                        abundance = round((mean(abundance, na.rm = T))))
    subtry <- summarise(group_by(subtry, treatment, species),
                        abundance = round((sum(abundance, na.rm = T))))
    subtry <- pivot_wider(subtry, names_from = species, values_from = abundance, values_fill = 0)
    subtry <- as.data.frame(subtry)
    rownames(subtry) <- subtry$treatment
    subtry <- subtry[,-1]
    rad_try <- radfit(subtry)
    
    results <- rad_try$models$Lognormal
  
    result_df <- data.frame(
      treatment = treat,
      sampling = samp,
      model = results$model,
      par1 = results$coefficients[1],
      par2 = results$coefficients[2],
      par3 = NA,
      Deviance = results$deviance,
      AIC = results$aic,
      BIC = NA
    )
    rad_df_Lognormal <- rbind(rad_df_Lognormal, result_df)
  }
}





















#Chat gpt try1: 

# Iterate over treatments
for (treat in levels(flora$treatment)) {
  # Iterate over samplings
  for (samp in levels(flora$sampling)) {
    # Subset data for treatment and sampling
    subset_data <- subset(flora, treatment == treat & sampling == samp)
    # Summarize data
    subtry <- summarise(group_by(subset_data, treatment, sampling, species),
                        abundance = round((mean(abundance, na.rm = TRUE))))
    # Summing each mean value per sampling (cumulative abundance through time)
    subtry <- summarise(group_by(subtry, treatment, species),
                        abundance = round((sum(abundance, na.rm = TRUE))))
    # Convert to wide format
    subtry <- pivot_wider(subtry, names_from = species, values_from = abundance, values_fill = 0)
    subtry <- as.data.frame(subtry)
    # Remove treatment from column and add it as rowname
    rownames(subtry) <- subtry$treatment
    # Apply radfit
    rad_try <- radfit(subtry)
    # Extract results
    rad_df_Lognormal$treatment <- subset_data$treatment
    rad_df_Lognormal$sampling <- subset_data$sampling
    rad_df_Lognormal$model <- "lognormal"
    rad_df_Lognormal$par1 <-  rad_try$models$Lognormal$coefficients[1]
    rad_df_Lognormal$par2 <-  rad_try$models$Lognormal$coefficients[2]
    rad_df_Lognormal$par3 <-  "NA"
    rad_df_Lognormal$Deviance <- rad_try$models$Lognormal$deviance
    rad_df_Lognormal$AIC <- rad_try$models$Lognormal$aic
    rad_df_Lognormal$BIC <-"?????"
    
    # Append results to rad_df
    rad_df <- rbind(rad_df, result_df)
  }
}

# Reset row names
row.names(rad_df) <- NULL

# Show the resulting dataframe
head(rad_df)



#DANI way: 

rad_c = matrix(nrow=length(unique(data_c$sampling))*length(unique(data_c$plot)), ncol=10) # samplings
colnames(rad_c)=c("treatment","sampling","plot","Species_richness","Mean_abundance","Zipf_p1","Zipf_gamma","Preemp_alpha","Lognormal_mu","Lognormal_sigma") #,"Gambin_alpha")
rad_c=as.data.frame(rad_c)

cont=0 # Loop counter
sortplots_c <- sort(unique(data_c$plot)) # Sort plot numbers within treatments (necessary for the loop)
for (i in 0:(length(unique(data_c$sampling))-1)){
  for (j in 1:c(length(unique(data_c$plot)))){
    sub <- filter (data_c, sampling==i & plot==sortplots_c[j]) #sub-dataset with sampling i and column of abundances
    radfit1 <- rad.zipf(sub$abundance)
    radfit2 <- rad.preempt(sub$abundance)
    radfit3 <- rad.lognormal(sub$abundance)
    #    gambin <- fit_abundances(sub$abundance)
    
    cont=cont+1
    rad_c[cont,1]="control"
    rad_c[cont,2]=as.numeric(i)
    rad_c[cont,3]=sortplots_c[j]
    rad_c[cont,4]=length(unique(sub$species)) # Species richness
    rad_c[cont,5]=mean(sub$abundance) # Mean biomass
    rad_c[cont,6]=radfit1$coefficients[1] # Zipf model (p1 parameter)
    rad_c[cont,7]=radfit1$coefficients[2] # Zipf model (gamma parameter)
    rad_c[cont,8]=radfit2$coefficients[1] # Preemption model (alpha parameter)
    rad_c[cont,9]=radfit3$coefficients[1] # Lognormal model (mu parameter)
    rad_c[cont,10]=radfit3$coefficients[2] # Lognormal model (sigma parameter)
    #    rad_c[cont,11]=gambin$alpha
  }
}
