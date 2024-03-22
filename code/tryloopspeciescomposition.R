

treats <- unique(flora$treatment)
list1 <- list()
gglist1 <- list()
count = 0


for(i in 1: length(treats)){
  
count = count + 1

list1[[count]] <- subset(species_ab, treatment == treats[i])


sp_wide <- list1[[count]] %>%
  pivot_wider(id_cols = sampling,
              names_from = code,
              values_from = abundance,
              values_fill = list(abundance = 0)) %>% 
  column_to_rownames(var = "sampling") %>% 
  arrange(as.numeric(rownames(.)))

pcoa_hell <- sp_wide %>% 
  na.omit() %>% 
  vegan::vegdist(method = "hellinger") %>%  # we use Hellinger because it works better with double 0's
  cmdscale(eig = T) 
var_exp_hell <- pcoa_hell$eig[1:2]/sum(pcoa_hell$eig[pcoa_hell$eig > 0])

pcoa_samplings_hell<- pcoa_hell$points %>% 
  as.data.frame() 
pcoa_species_hell<- cor(sp_wide, pcoa_samplings_hell) %>% 
  as.data.frame()


gglist1[[count]] <-
  ggplot() +
  #geom_segment(data = pcoa_species_hell_c %>% 
  # rownames_to_column(var = "sp"),
  #aes(x = 0, y = 0, xend = V1, yend = V2),
  #color = "grey",
  #arrow = arrow()) +
  geom_text_repel(data = pcoa_species_hell %>% 
                    rownames_to_column(var = "sp"),
                  aes(x = V1, y = V2, label = sp),
                  color = "grey",
                  max.overlaps = 30) +
  geom_point(data = pcoa_samplings_hell %>% 
               rownames_to_column(var = "sampling"),
             aes(x = V1, y = V2),
             size = 2) +
  geom_text_repel(data = pcoa_samplings_hell %>% 
                    rownames_to_column(var = "sampling"),
                  aes(x = V1, y = V2, label = sampling),
                  max.overlaps = 13) +
  geom_path(data = pcoa_samplings_hell %>% 
              rownames_to_column(var = "sampling"),
            aes(x = V1, y = V2)) +
  geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = 0), color = "red", linetype = "dashed") +
  labs(title = paste("PCoA using Hellinger distance:", treats[i], sep = " "),
       subtitle = paste0("Variance explained = ", round(sum(var_exp_hell)*100), "%"),
       x = paste0(round(var_exp_hell[1]*100), "% var"),
       y = paste0(round(var_exp_hell[2]*100), "% var"))

}



ggarrange(
  gglist1[[2]], gglist1[[3]], gglist1[[1]], gglist1[[4]],
  ncol = 2, nrow = 2)  # fatal error


