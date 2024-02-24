


RR_treatments$RR_ref_ab_C <- NA
RR_treatments$RR_ref_richness_C <- NA
for (i in samps) {
  subset_data <- subset(RR_treatments, sampling == i & treatment == "c")
  RR_treatments$RR_ref_ab_C[RR_treatments$sampling == i] <- mean(subset_data$abundance)
  RR_treatments$RR_ref_richness_C[RR_treatments$sampling == i] <- mean(subset_data$n_species)
}

RR_treatments$RR_abundance_C <- round(log(RR_treatments$abundance/RR_treatments$RR_ref_ab_C), 2)
RR_treatments$RR_richness_C <- round(log(RR_treatments$n_species/RR_treatments$RR_ref_richness_C), 2)



# Representación gráfica 

RR_treatments_noC <- RR_treatments[RR_treatments$treatment != "c", ]

theme_set(theme_bw()+ theme(legend.position = "NULL"))

  ggarrange(
    ggplot(RR_treatments_noC, aes(x = sampling, y = RR_abundance_C, fill = treatment)) +
      geom_boxplot() +
      labs(x = " ", y = "logRR(abundance)") +
      facet_grid(~ treatment) + 
      scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    ggplot(RR_treatments_noC, aes(x = sampling, y = RR_richness_C, fill = treatment)) +
      geom_boxplot() +
      labs(x = " ", y = "logRR(richness)") +
      facet_grid(~ treatment) + 
      scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    nrow = 2, ncol = 1)

  
  

   flora_s_c <- subset(flora_samplings, treatment == "c")
   
   
  ggarrange(
   ggplot(flora_s_c, aes(x = sampling, y = abundance)) +
     geom_boxplot() +
     labs(x = " ", y = "Abundance") +
     geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
     theme(legend.position = "NULL"),
   
  ggplot(flora_s_c, aes(x = sampling, y = n_species)) +
    geom_boxplot() +
    labs(x = " ", y = "Richness") +
    geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
    theme(legend.position = "NULL"),
  
  nrow = 1, ncol = 2)
  
  
  