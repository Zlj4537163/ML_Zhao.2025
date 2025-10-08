# 加载必要的包
library(ape)

# 读取注释数据（假设为anno.csv）
# anno <- read.csv("anno.csv")

# 读取性状数据（注意：示例数据包含头部行）
# trait_data <- read.csv("merged_data.csv", row.names = 1)  # 使用第一列作为行名

# 数据预处理
# 移除空行/列（如果需要）
# trait_data <- na.omit(trait_data)

# 标准化数据（重要步骤，使不同量纲的性状可比较）
scaled_data <- scale(trait_data) 

# 检查样本ID匹配
common_ids <- intersect(rownames(trait_data), anno$ID)
trait_data <- trait_data[rownames(trait_data) %in% common_ids, ]
anno <- anno[anno$ID %in% common_ids, ]

# 准备聚类
dist_matrix <- dist(scaled_data)       # 计算距离矩阵
hc <- hclust(dist_matrix, "ward.D2")   # 使用Ward方法聚类

# 生成系统发育树对象
phylo_tree <- as.phylo(hc)

# 创建更丰富的颜色调色板，避免重复
create_distinct_colors <- function(n) {
  # 避免使用rainbow()函数，改用更稳定的颜色生成方式
  if (n <= 8) {
    # 使用R基本颜色的小调色板
    return(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", 
             "#FF7F00", "#FFFF33", "#A65628", "#F781BF")[1:n])
  } else {
    # 使用RColorBrewer的Set3调色板
    return(colorRampPalette(brewer.pal(min(12, n), "Set3"))(n))
  }
}

# 设置群体颜色 (确保每个群体有唯一颜色)
library(RColorBrewer)
n_groups <- length(unique(anno$group))
group_colors <- create_distinct_colors(n_groups)
names(group_colors) <- unique(anno$group)

# 为树尖（样本）设置颜色
tip_colors <- group_colors[anno$group[match(phylo_tree$tip.label, anno$ID)]]

# 绘制圆形树状图
plot(phylo_tree, 
     type = "fan",        # 圆形布局
     tip.color = tip_colors,  # 按群体着色样本点
     edge.width = 1,    # 分支宽度
     no.margin = TRUE,    # 无页边距
     label.offset = 1,  # 标签偏移量
     cex = 1)           # 标签大小

# 添加标题
# title("Safflower Accessions Clustering", line = -1)

# 添加图例 (检查是否有重复颜色)
legend_colors <- unique(group_colors)

# 创建图例的标签名称 (确保唯一性)
legend_labels <- names(group_colors)

# 绘制图例 (移除重复项)
legend("bottomleft", 
       legend = legend_labels, 
       fill = legend_colors,
       title = "Geographic Group",
       bty = "n",         # 无边框
       cex = 0.8,         # 文字大小
       inset = c(0, 0.05))

