library(tidyverse)
library(ggplot2)
results <- list()

# 建立資料框架,包含GPA資訊
data <- tibble(
  年度 = as.factor(rep(c(2020, 2021, 2022, 2023), each = 3)),
  科目 = rep(c("經濟", "金融", "其他"), times = 4),
  學分數 = c(4, 5, 2, 7, 5, 3, 5, 6, 4, 3, 4, 5),
  GPA = c(runif(12, min = 2.0, max = 4.0))
)

data$科目 <- factor(data$科目, levels = c("經濟", "金融", "其他"))

# 指定顏色和Alpha透明度
color_palette <- c("經濟" = "#FF7F00", "金融" = "#32CD32", "其他" = "#5579a3")
alpha_palette <- c("經濟" = 0.9, "金融" = 0.9, "其他" = 0.9)

# 繪製視覺化成績單
results$plot <- ggplot(data, aes(x = 年度, y = 學分數, fill = 科目, alpha = GPA)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75, aes(group = factor(科目, levels = c("經濟", "金融", "其他")))) +  
  scale_fill_manual(values = color_palette) +
  scale_alpha_continuous(range = c(0.6, 1)) + # 調整 alpha 範圍
  scale_x_discrete(breaks = c(as.character(unique(data$年度)), "2024.06"), labels = c("2020", "2021", "2022", "2023", "2024.06")) +  # 添加 x 軸標籤
  labs(x = "年度", y = "學分數", fill = NULL, alpha = "GPA") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # 重置格線格式
        axis.line.x = element_line(color = "black"),  # 顯示 x 軸線
        axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = 0, r = 0, b = 0, l = 10)))  # 調整 x 軸標籤角度和位置

# 顯示圖型
results$plot


