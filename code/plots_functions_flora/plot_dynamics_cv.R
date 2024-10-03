



# load packages
library(MetBrewer)
library(tidyverse)

# function to plot recovery dynamics (output is a ggplot object)
plot_dynamics_cv <- function(mean_data, type) {
  
  # set theme for the plot
  theme_set(theme_bw() +
              theme(axis.title.x = element_blank(),
                    legend.position = "NULL",
                    panel.grid = element_blank(),
                    strip.background = element_blank(),
                    strip.text = element_text(face = "bold"),
                    text = element_text(size = 11)))
  
  # define y axis title
  
  if (type == "abundance") {
    ytitle <- "CV of abundance"
  } else {
    if (type == "richness") {
      ytitle <- "CV of richness"
    } else {
      if (type == "sigma_log") {
        ytitle <- "CV of Sigma for Log model RADs"
      } else {
        if (type == "mu_log") {
          ytitle <- "CV of Mu for Log model RADs"
        } else {
          if (type == "Y_zipf") {
            ytitle <- "CV of Gamma for Zipf model RADs"
          } else {
            if (type == "biomass") {
              ytitle <- "CV of community biomass"
            } else{
              stop("type must be one of the following: richness, abundance, sigma_log, mu_log, Y_zipf, biomass_total")
            }
          }
        }
      }
    }
  }
  
  
  
  
  # define treatment names
  treatment_labs_dynamics <- c("Control", "Warming", "Perturbation", "Warming and perturbation")
  names(treatment_labs_dynamics) <- c("c","w", "p", "wp")
  
  
  # plot
  
  
  graph <- 
    
    ggplot(mean_data, aes(x = as.factor(sampling),
                          y = .data[[paste0("cv_", type)]],
                          group = sampling)) +
    
    facet_grid(~ treatment, labeller = labeller(treatment = treatment_labs_dynamics)) +  
    
    geom_path(group = 1, aes(color = treatment), linewidth = 0.8) +
    
    geom_point(aes(color = treatment), position = position_dodge(width = 0.5), size = 1.5, alpha = 0.2) +
    
    geom_smooth( se = T, aes(color = treatment))+
    
    geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
    
    
    #scale_colour_manual(values = met.brewer("VanGogh3",4)) +
    scale_colour_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red3", "wp" = "purple2")) +
    
    scale_x_discrete(breaks = levels(as.factor(mean_data$sampling))[seq(1, length(levels(as.factor(mean_data$sampling))), by = 2)]) +
    
    labs(y = ytitle)
  
  return(graph)
  
} #end of plot_recovery

# end of the script