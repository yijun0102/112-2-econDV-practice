library(tidyverse)
library(dplyr)
library(ggplot2)

# 讀取資料
scores <- read_csv("/Users/yeyijun/Documents/EconDV/112-2-econDV-practice/score.csv")

# 按學年分組
scores_by_year <- scores %>%
  mutate(學年 = as.integer(學年),
         加權GPA = 學分 * GPA) %>%
  group_by(學年) %>%
  group_nest() %>%
  ungroup()

results <- list()

for (i in seq_along(scores_by_year$data)) {
  year_data <- scores_by_year$data[[i]]
  year <- scores_by_year$學年[i]
  
  # 計算領域加權學分
  finance_credits <- year_data %>% 
    dplyr::filter(領域 %in% c("金融", "綜合")) %>%
    pull(學分) %>%
    sum()
  
  econ_credits <- year_data %>%
    dplyr::filter(領域 %in% c("經濟", "綜合")) %>% 
    pull(學分) %>%
    sum()
  
  others_credits <- year_data %>%
    dplyr::filter(領域 == "其他") %>%
    pull(學分) %>%
    sum()
  
  # 計算領域加權GPA
  finance_sum_GPA <- year_data %>% dplyr::filter(領域 %in% c("金融", "綜合")) %>% pull(加權GPA) %>% sum(na.rm = TRUE)
  econ_sum_GPA <- year_data %>% dplyr::filter(領域 %in% c("經濟", "綜合")) %>% pull(加權GPA) %>% sum(na.rm = TRUE)
  others_sum_GPA <- year_data %>% dplyr::filter(領域 == "其他") %>% pull(加權GPA) %>% sum(na.rm = TRUE)
  
  # 計算領域平均GPA
  finance_GPA <- finance_sum_GPA / finance_credits
  econ_GPA <- econ_sum_GPA / econ_credits
  others_GPA <- others_sum_GPA / others_credits
  
  # 將處理後的數據存入results
  results[[as.character(year)]] <- tibble(
    finance_credits, econ_credits, others_credits,
    finance_sum_GPA, econ_sum_GPA, others_sum_GPA,
    finance_GPA, econ_GPA, others_GPA
  )
}

# 建立繪圖數據
plot_data <- tibble(
  年度 = rep(year, 3),
  科目 = factor(rep(c("經濟", "金融", "其他"), each = 1), levels = c("經濟", "金融", "其他")),
  學分數 = c(econ_credits, finance_credits, others_credits),
  GPA = c(econ_GPA, finance_GPA, others_GPA)
)

# 指定顏色和Alpha透明度
color_palette <- c("經濟" = "#FF7F00", "金融" = "#32CD32", "其他" = "#5579a3")
alpha_palette <- c("經濟" = 0.9, "金融" = 0.9, "其他" = 0.9)

# 繪製視覺化成績單
results[[as.character(year)]] <- ggplot(plot_data, aes(x = 年度, y = 學分數, fill = 科目, alpha = GPA)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75, aes(group = 科目)) +
  scale_fill_manual(values = color_palette) +
  scale_alpha_continuous(range = c(0.6, 1)) +
  scale_x_discrete(labels = as.character(year)) +
  labs(x = "年度", y = "學分數", fill = NULL, alpha = "GPA") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = 0, r = 0, b = 0, l = 10)))

results[[as.character(110)]]

'''
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
'''

