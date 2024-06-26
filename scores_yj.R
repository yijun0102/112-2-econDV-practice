library(tidyverse)
library(dplyr)
library(ggplot2)

results <- list()
# 讀取資料
scores <- read_csv("/Users/yeyijun/Documents/EconDV/112-2-econDV-practice/score.csv")

# 按學年分組
scores_by_year <- scores %>%
  mutate(學年 = as.integer(學年),
         加權GPA = 學分 * GPA) %>%
  group_by(學年) %>%
  group_nest() %>%
  ungroup()

# 計算每個學年的領域加權學分和平均GPA
year_data <- map2(scores_by_year$data, scores_by_year$學年, function(year_data, year) {
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
  finance_sum_GPA <- year_data %>% dplyr::filter(領域 %in% c("金融", "綜合")) %>% pull(加權GPA) %>% sum(na.rm = TRUE)
  econ_sum_GPA <- year_data %>% dplyr::filter(領域 %in% c("經濟", "綜合")) %>% pull(加權GPA) %>% sum(na.rm = TRUE)
  others_sum_GPA <- year_data %>% dplyr::filter(領域 == "其他") %>% pull(加權GPA) %>% sum(na.rm = TRUE)
  finance_GPA <- finance_sum_GPA / finance_credits
  econ_GPA <- econ_sum_GPA / econ_credits
  others_GPA <- others_sum_GPA / others_credits
  
  tibble(
    學年 = year,
    領域 = factor(rep(c("經濟", "金融", "其他"), each = 1), levels = c("經濟", "金融", "其他")),
    學分數 = c(econ_credits, finance_credits, others_credits),
    GPA = c(econ_GPA, finance_GPA, others_GPA)
  )
})

# 合併數據
plot_data <- bind_rows(year_data)

# 指定顏色
color_palette <- c("經濟" = "#ed7d32", "金融" = "#71ad47", "其他" = "#4472c4")

# 繪製長條圖
results$plot <- ggplot(plot_data, aes(x = 學年, y = 學分數, fill = 領域, alpha = GPA)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = color_palette) +
  scale_alpha_continuous(range = c(0.7, 1)) +
  scale_x_continuous(breaks = unique(plot_data$學年), labels = unique(plot_data$學年)) +
  labs(x = "學年", y = "學分數", fill = NULL, alpha = "GPA") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray93", size = 0.5),
        axis.line.x = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = 0, r = 0, b = 0, l = 10)),
        axis.title.y = element_text(angle = 0, vjust = 0.9))

# 顯示圖型
results$plot

