library(tidyverse)

# 读取数据
data <- read.csv("../processed_data/fattyacid_content.csv", header = FALSE)

# 整理为长格式
long_data <- data %>%
  pivot_longer(-V1, names_to = "original", values_to = "content") %>%  # 展开每列
  filter(!is.na(content)) %>%  # 去掉 NA
  filter(str_detect(content, ":|：")) %>%  # 保留包含冒号的
  mutate(
    fatty_acid = str_replace(content, "[:：][^:：]*$", ""),  # 去除最后一个冒号和后面的数值
    value = str_extract(content, "(?<=[:：])[^:：]+$")  # 取最后一个冒号后的数值
  ) %>%
  select(Sample = V1, fatty_acid, value) %>%
  mutate(value = as.numeric(value))  # 数值转为数值类型

long_data <- na.omit(long_data)
# 查看结果
head(long_data)

library(tidyr)

wide_data <- long_data %>%
  pivot_wider(
    names_from = Sample,
    values_from = value,
    values_fn = sum,    # 重复值相加，避免生成 list
    values_fill = 0     # 缺失填 0
  )

# 查看前几行
wide_data

# 保存为 CSV 文件
write.csv(wide_data, "../processed_data/fattyacid_content_wide.csv")
write.csv(long_data, "../processed_data/fattyacid_content_long.csv")
