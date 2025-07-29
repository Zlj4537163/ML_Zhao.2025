# 加载相关包
if (!require("corrplot")) install.packages("corrplot")
if (!require("Hmisc")) install.packages("Hmisc")
library(corrplot)
library(Hmisc)
library(FactoMineR)
library(factoextra)
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("factoextra")) install.packages("factoextra")
library(ggplot2)
library(factoextra)
library(dplyr)

# -------------------------------
# 1. 读取数据
# -------------------------------
Trace_element_data <- read.csv("../processed_data/Trace_element_data.csv")
Biochemical_data <- read.csv("../processed_data/Biochemical_data.csv")
fat <- read.csv("../processed_data/fat_data.csv")
fatty_acid <- read.csv("../processed_data/fattyacid_content_wide.csv")

# 合并数据
df1 <- merge(Trace_element_data, Biochemical_data, by = "ID")
df2 <- merge(df1, fat, by = "ID")
merge_data <- merge(df2, fatty_acid, by = "ID")
colnames(merge_data)

# 原始列名
old_names <- colnames(merge_data)

# 清洗列名：去除单位、小数点、多余的符号
clean_names <- gsub("\\.\\.\\.", "", old_names)              # 去掉“...”
clean_names <- gsub("\\.mg\\.g\\.", "", clean_names)         # 去掉“.mg.g.”
clean_names <- gsub("\\.g\\.kg\\.", "", clean_names)         # 去掉“.g.kg.”
clean_names <- gsub("\\.mg\\.kg\\.", "", clean_names)        # 去掉“.g.kg.”
clean_names <- gsub("\\.mg\\.g", "", clean_names)            # 去掉“.mg.g”
clean_names <- gsub("\\.g\\.kg", "", clean_names)            # 去掉“.g.kg”
clean_names <- gsub("\\.g\\.100g\\.", "", clean_names)       # 去掉“.g.100g."

# 替换列名
colnames(merge_data) <- clean_names
colnames(merge_data)

# write.csv(merge_data, "./merge_data.csv", row.names = FALSE)

# -------------------------------
# 2. 统计描述分析
# -------------------------------
# 创建空的结果数据框
result <- data.frame(
  Quality_Index = colnames(merge_data[,-1]),
  Minimum = NA,
  Maximum = NA,
  Mean = NA,
  SD = NA,
  CV_percent = NA,
  Genetic_Diversity_Index = NA
)

# 批量计算
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

# 保存结果
write.csv(result, "./Statistics_analysis_of_quality_index.csv", row.names = FALSE)

# -------------------------------
# 3. 相关性分析与可视化
# -------------------------------


# 选择数值变量（除去ID）
cor_data <- merge_data[,-1]

# 计算相关性矩阵与p值矩阵
cor_result <- rcorr(as.matrix(cor_data), type = "pearson")
cor_matrix <- cor_result$r
p_matrix <- cor_result$P

# 保存相关性矩阵
write.csv(cor_matrix, "./Correlation_matrix.csv")
write.csv(p_matrix, "./Correlation_p_values.csv")

# -------------------------------
# 4. 绘制热图（相关性矩阵）
# -------------------------------
# 全部展示相关性热图
corrplot(cor_matrix, method = "color", type = "lower",
         tl.col = "black", tl.cex = 0.7, number.cex = 0.2,
         addCoef.col = "black", diag = FALSE,
         col = colorRampPalette(c("blue", "white", "red"))(200))

# 显著性过滤后的热图（p < 0.05）
corrplot(cor_matrix, method = "color", type = "lower",
         tl.col = "black", tl.cex = 0.7, number.cex = 0.2,
         p.mat = p_matrix, sig.level = 0.05, insig = "blank",
         addCoef.col = "black", diag = FALSE,
         col = colorRampPalette(c("lightblue", "white", "pink"))(200))

# -------------------------------
# 5. 主成分分析（PCA）
# -------------------------------

# 读取分组信息
anno_raw <- read.csv("../processed_data/pheno.csv")
anno_raw$group <- paste(anno_raw$area, anno_raw$Country, sep = "_")
anno <- anno_raw[, c("ID", "group")]
anno

combined_data <- merge(merge_data, anno, by = "ID")
combined_data

# 使用 prcomp 进行中心化和标准化后的 PCA
pca_result <- prcomp(cor_data, scale. = TRUE)

# 主成分贡献率（标准差的平方 / 总变异）
explained_var <- summary(pca_result)$importance
# 提取贡献率（方差解释比例）
variance_ratio <- explained_var[2, ]
cumulative_ratio <- explained_var[3, ]

# 保存贡献率
write.csv(data.frame(PC = names(variance_ratio),
                     Variance_Explained = variance_ratio,
                     Cumulative = cumulative_ratio),
          "./PCA_variance_explained.csv", row.names = FALSE)

# 因子载荷（变量在主成分上的权重）
loadings <- pca_result$rotation

# 选择前几个主成分（例如PC1~PC3）
loadings_subset <- loadings[, 1:9]

# 保存因子载荷
write.csv(loadings_subset, "./PCA_loadings_PC1_9.csv")

# -------------------------------
# 6. 绘制 PCA 图 
# -------------------------------

pca_df <- as.data.frame(pca_result$x)
pca_df$group <- combined_data$group
pca_df$ID <- combined_data$ID

# 绘制 PC1 vs PC2 图
ggplot(pca_df, aes(x = PC1, y = PC2, color = group)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(group = group), linetype = 3) +
  theme_classic() +
  labs(title = "PCA by Group",
       x = paste0("PC1 (", round(variance_ratio[1] * 100, 1), "%)"),
       y = paste0("PC2 (", round(variance_ratio[2] * 100, 1), "%)"))



