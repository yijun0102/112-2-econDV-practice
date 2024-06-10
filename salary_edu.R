library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(scales) 

# 讀取數據
salary <- read_csv("/Users/yeyijun/Documents/EconDV/112-2-econDV-practice/salary.csv")

# 轉換數據格式
salary_long <- salary %>%
  pivot_longer(cols = c("國中以下", "高中職", "大學", "研究所"),
               names_to = "Education",
               values_to = "Salary")

# 按照指定順序重新排序Education
salary_long$Education <- factor(salary_long$Education, levels = c("國中以下", "高中職", "大學", "研究所"))

# 顏色設定
color_map <- c("研究所" = "#BB5A5A", "大學" = "#E79E85", "高中職" = "#EACEB4", "國中以下" = "#F2E9D0")

# 按照指定順序重新排序Education
salary_long$Education <- factor(salary_long$Education, levels = rev(c("研究所", "大學", "高中職", "國中以下")))

# 繪製密度圖
plot <- ggplot(salary_long, aes(x = Salary, fill = Education, color = Education)) +
  geom_density(alpha = 0.3, size = 1.5) +
  scale_fill_manual(values = color_map) +
  scale_color_manual(values = color_map) +
  labs(title = "不同教育程度的薪資分布",
       subtitle = "民國111年",
       x = "薪資（萬元/年）",
       y = "人數（%）",
       caption = "資料來源：中華民國統計資訊網") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1),
        plot.caption = element_text(hjust = 1, size = 8, face = "italic", margin = margin(t = 10)),
        axis.line = element_line(size = 0.5, color = "grey"),
        axis.ticks = element_line(size = 0.5, color = "grey")) + 
  scale_y_continuous(labels = scales::percent) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

print(plot)


