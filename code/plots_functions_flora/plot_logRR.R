

# load packages
library(MetBrewer)
library(tidyverse)

# function to plot recovery dynamics (output is a ggplot object)
plot_logRR <- function(full_data, mean_data, type) {
  
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
    ytitle <- "Abundance (LogRR)"
  } else {
    if (type == "richness") {
      ytitle <- "Richness (LogRR)"
    } else {
      if (type == "sigma_log") {
        ytitle <- "Sigma for Log model RADs (LogRR)"
      } else {
        if (type == "mu_log") {
          ytitle <- "Mu for "
        } else {
          if (type == "Y_zipf") {
            ytitle <- "Gamma for Zipf model RADs (LogRR)"
          } else {
            if (type == "biomass") {
              ytitle <- "Community biomass (LogRR)"
            } else{
              stop("type must be one of the following: richness, abundance, sigma_log, mu_log, Y_zipf, biomass_total")
            }
          }
        }
      }
    }
  }
      
    
  
  
  # define treatment names
  treatment_labs_RR<- c("Warming", "Perturbation", "Warming and perturbation")
  names(treatment_labs_RR) <- c("w", "p", "wp")
  
  # plot
  
  
  
  graph <- 
    
    ggplot(mean_data, aes(x = as.factor(sampling),
                          y = .data[[paste0("mean_", type)]],
                          group = sampling)) +
    
    facet_grid(~ treatment, labeller = labeller(treatment = treatment_labs_RR)) +  
    
    geom_errorbar(aes(ymin = mean_data[[paste0("mean_", type)]] - mean_data[[paste0("sd_", type)]],
                      ymax = mean_data[[paste0("mean_", type)]] + mean_data[[paste0("sd_", type)]],
                      color = treatment, alpha = 0.5),
                  position = position_dodge(width = 0.5), width = 0.25, size = 0.75, alpha = 0.4) +
    
    geom_point(data = full_data, aes(x = as.factor(sampling),
                                     y = .data[[paste0("logRR_", type)]],
                                     color = treatment),
               position = position_dodge(width = 0.5), size = 1.5, alpha = 0.2) +
    
    geom_path(group = 1, aes(color = treatment), linewidth = 0.8) +
    
    geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
    
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.7) +
    
    #scale_colour_manual(values = met.brewer("VanGogh2", 4)) +
    
    scale_colour_manual(values = c("p" = "blue4", "w" = "red3", "wp" = "purple2")) +
    
    
    scale_x_discrete(breaks = levels(as.factor(mean_data$sampling))[seq(1, length(levels(as.factor(mean_data$sampling))), by = 2)]) +
    
    labs(y = ytitle)
  
  return(graph)
  
} #end of plot_recovery

# end of the script