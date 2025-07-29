library(corrplot)
library(Hmisc)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(factoextra)
library(dplyr)


Trace_element_data <- read.csv("../processed_data/Trace_element_data.csv")
Biochemical_data <- read.csv("../processed_data/Biochemical_data.csv")
fat <- read.csv("../processed_data/fat_data.csv")
fatty_acid <- read.csv("../processed_data/fattyacid_content_wide.csv")


df1 <- merge(Trace_element_data, Biochemical_data, by = "ID")
df2 <- merge(df1, fat, by = "ID")
merge_data <- merge(df2, fatty_acid, by = "ID")
colnames(merge_data)


old_names <- colnames(merge_data)


clean_names <- gsub("\\.\\.\\.", "", old_names)             
clean_names <- gsub("\\.mg\\.g\\.", "", clean_names)        
clean_names <- gsub("\\.g\\.kg\\.", "", clean_names)         
clean_names <- gsub("\\.mg\\.kg\\.", "", clean_names)       
clean_names <- gsub("\\.mg\\.g", "", clean_names)           
clean_names <- gsub("\\.g\\.kg", "", clean_names)            
clean_names <- gsub("\\.g\\.100g\\.", "", clean_names)      


colnames(merge_data) <- clean_names
colnames(merge_data)
# write.csv(merge_data, "./merge_data.csv", row.names = FALSE)


result <- data.frame(
  Quality_Index = colnames(merge_data[,-1]),
  Minimum = NA,
  Maximum = NA,
  Mean = NA,
  SD = NA,
  CV_percent = NA,
  Genetic_Diversity_Index = NA
)


for (i in seq_along(colnames(merge_data[,-1]))) {
  var <- colnames(merge_data[,-1])[i]
  values <- merge_data[[var]]
  result$Minimum[i] <- min(values, na.rm = TRUE)
  result$Maximum[i] <- max(values, na.rm = TRUE)
  result$Mean[i] <- mean(values, na.rm = TRUE)
  sd_val <- sd(values, na.rm = TRUE)
  result$SD[i] <- sd_val
  result$CV_percent[i] <- sd_val / result$Mean[i] * 100
  result$Genetic_Diversity_Index[i] <- sd_val / (result$Maximum[i] - result$Minimum[i])
}

write.csv(result, "./Statistics_analysis_of_quality_index.csv", row.names = FALSE)


cor_data <- merge_data[,-1]


cor_result <- rcorr(as.matrix(cor_data), type = "pearson")
cor_matrix <- cor_result$r
p_matrix <- cor_result$P


write.csv(cor_matrix, "./Correlation_matrix.csv")
write.csv(p_matrix, "./Correlation_p_values.csv")


corrplot(cor_matrix, method = "color", type = "lower",
         tl.col = "black", tl.cex = 0.7, number.cex = 0.2,
         addCoef.col = "black", diag = FALSE,
         col = colorRampPalette(c("blue", "white", "red"))(200))


corrplot(cor_matrix, method = "color", type = "lower",
         tl.col = "black", tl.cex = 0.7, number.cex = 0.2,
         p.mat = p_matrix, sig.level = 0.05, insig = "blank",
         addCoef.col = "black", diag = FALSE,
         col = colorRampPalette(c("lightblue", "white", "pink"))(200))



anno_raw <- read.csv("../processed_data/pheno.csv")
anno_raw$group <- paste(anno_raw$area, anno_raw$Country, sep = "_")
anno <- anno_raw[, c("ID", "group")]
anno

combined_data <- merge(merge_data, anno, by = "ID")
combined_data


pca_result <- prcomp(cor_data, scale. = TRUE)


explained_var <- summary(pca_result)$importance

variance_ratio <- explained_var[2, ]
cumulative_ratio <- explained_var[3, ]


write.csv(data.frame(PC = names(variance_ratio),
                     Variance_Explained = variance_ratio,
                     Cumulative = cumulative_ratio),
          "./PCA_variance_explained.csv", row.names = FALSE)


loadings <- pca_result$rotation

loadings_subset <- loadings[, 1:9]

write.csv(loadings_subset, "./PCA_loadings_PC1_9.csv")


pca_df <- as.data.frame(pca_result$x)
pca_df$group <- combined_data$group
pca_df$ID <- combined_data$ID


ggplot(pca_df, aes(x = PC1, y = PC2, color = group)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(group = group), linetype = 3) +
  theme_classic() +
  labs(title = "PCA by Group",
       x = paste0("PC1 (", round(variance_ratio[1] * 100, 1), "%)"),
       y = paste0("PC2 (", round(variance_ratio[2] * 100, 1), "%)"))



