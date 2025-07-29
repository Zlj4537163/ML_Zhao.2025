library(tidyverse)

data <- read.csv("../processed_data/fattyacid_content.csv", header = FALSE)


long_data <- data %>%
  pivot_longer(-V1, names_to = "original", values_to = "content") %>%  
  filter(!is.na(content)) %>%  
  filter(str_detect(content, ":|：")) %>%  
  mutate(
    fatty_acid = str_replace(content, "[:：][^:：]*$", ""), 
    value = str_extract(content, "(?<=[:：])[^:：]+$")  
  ) %>%
  select(Sample = V1, fatty_acid, value) %>%
  mutate(value = as.numeric(value)) 

long_data <- na.omit(long_data)

head(long_data)

library(tidyr)

wide_data <- long_data %>%
  pivot_wider(
    names_from = Sample,
    values_from = value,
    values_fn = sum,    
    values_fill = 0     
  )


write.csv(wide_data, "../processed_data/fattyacid_content_wide.csv")
write.csv(long_data, "../processed_data/fattyacid_content_long.csv")
