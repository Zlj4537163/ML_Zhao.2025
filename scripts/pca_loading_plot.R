# 假设你已经读入数据到变量 `loadings_df`
# 示例数据读入方式
loadings_df <- read.csv("PCA_loadings_PC1_9.csv")
rownames(loadings_df) <- loadings_df[, 1]  # 设置第一列为行名
loadings_df <- loadings_df[, -1]  # 去掉第一列（假设是行名）

# 提取 PC1 和 PC2
library(ggplot2)
library(tidyr)
library(dplyr)

top_loadings <- loadings_df %>%
  select(PC1, PC2,PC3,PC4) %>%
  mutate(Variable = rownames(.)) %>%
  pivot_longer(cols = c(PC1, PC2,PC3,PC4), names_to = "PC", values_to = "Loading")

# 按绝对值选取前20大变量
top_vars <- top_loadings %>%
  group_by(PC) %>%
  slice_max(order_by = abs(Loading), n = 20)

# 绘图
ggplot(top_vars, aes(x = reorder(Variable, Loading), y = Loading, fill = PC)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~PC, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),  # 加大字体
        strip.text = element_text(size = 12)) +  # 分面标签
  labs(title = "Top 20 Loadings for PC1 and PC2",
       x = "Variable", y = "Loading")



