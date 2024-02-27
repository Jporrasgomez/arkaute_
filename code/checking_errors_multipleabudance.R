



source("code/first_script.R")

library(dplyr)


library(dplyr)

# Filter the dataframe for a specific plot and sampling
plot_sampling_data <- flora %>%
  filter(plot == "1" & sampling == "1")

# Check if any code has multiple values of abundance
multiple_abundance <- plot_sampling_data %>%
  group_by(code) %>%
  summarise(n_abundance = n_distinct(abundance)) %>%
  filter(unique_abundance > 1)
multiple_abundance$plot <- "1"
multiple_abundance$sampling <- "1"

# Print the result
print(multiple_abundance)

#loop: 



























# Get unique values of plot and sampling
unique_plots <- unique(flora$plot)
unique_sampling <- unique(flora$sampling)

# Initialize an empty list to store results
multiple_abundance_list <- list()

# Loop through all combinations of plot and sampling
for (p in unique_plots) {
  for (s in unique_sampling) {
    # Filter the dataframe for the current plot and sampling
    plot_sampling_data <- flora %>%
      filter(plot == as.character(p) & sampling == as.character(s))
    
    # Check if any code has multiple values of abundance
    multiple_abundance <- plot_sampling_data %>%
      group_by(code) %>%
      summarise(unique_abundance = n_distinct(abundance)) %>%
      filter(unique_abundance > 1)
    
    # Store the result in the list
    multiple_abundance_list[[paste("Plot", p, "Sampling", s)]] <- multiple_abundance
  }
}

# Print the results
for (i in 1:length(multiple_abundance_list)) {
  if (!is_empty(multiple_abundance_list[[i]])) {
    print(paste(names(multiple_abundance_list)[i], ":", multiple_abundance_list[[i]]$code))
  }
}
