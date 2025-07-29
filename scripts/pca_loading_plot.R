loadings_df <- read.csv("PCA_loadings_PC1_9.csv")
rownames(loadings_df) <- loadings_df[, 1] 
loadings_df <- loadings_df[, -1]  


library(ggplot2)
library(tidyr)
library(dplyr)

top_loadings <- loadings_df %>%
  select(PC1, PC2,PC3,PC4) %>%
  mutate(Variable = rownames(.)) %>%
  pivot_longer(cols = c(PC1, PC2,PC3,PC4), names_to = "PC", values_to = "Loading")


top_vars <- top_loadings %>%
  group_by(PC) %>%
  slice_max(order_by = abs(Loading), n = 20)


ggplot(top_vars, aes(x = reorder(Variable, Loading), y = Loading, fill = PC)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~PC, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),  
        strip.text = element_text(size = 12)) +  
  labs(title = "Top 20 Loadings for PC1 and PC2",
       x = "Variable", y = "Loading")



