# 引入資料 ----
library(readr)
table <- read_csv("~/Downloads/weather.csv")
head(table)

temp <- table %>%
  select(觀測時間, `測站`, `平均氣溫(℃)`)
head(temp)

library(dplyr)
library(tidyr)

# 转换为宽格式
temp <- pivot_wider(temp, names_from = 測站, values_from = `平均氣溫(℃)`)
head(temp)

rate <- temp %>%
  arrange(觀測時間) %>%
  mutate(across(c("臺北", "臺南", "臺中", "臺東"), 
                ~( . - lag(.) ) / lag(.), 
                .names = "變化率_{.col}"))
head(rate)

## 變化率改成長格式 -----
rate <- rate %>% 
  pivot_longer(cols = starts_with("變化率"),
               names_to = "地區",
               values_to = "變化率")
head(rate)

## 作圖 ------
rate %>%
  ggplot(aes(x = 觀測時間, y = 變化率, color = 地區)) +
  geom_line() +
  labs(x = "日期（2010年1月到2023年12月）",
       y = "各地氣溫變化率", 
       color = "地區") +
  theme_minimal()

## 作圖 ------
ggplot(temp, aes(x = 觀測時間)) +
  geom_line(aes(y = 臺北, color = "臺北"), alpha = 1) +
  geom_line(aes(y = 臺中, color = "臺中"), alpha = 1) +
  geom_line(aes(y = 臺南, color = "臺南"), alpha = 1) +
  geom_line(aes(y = 臺東, color = "臺東"), alpha = 1) +
  labs(title = "各地月均溫", x = "日期", y = "溫度") +
  scale_color_manual(values = c("臺北" = "skyblue", "臺中" = "green", "臺南" = "orange", "臺東" = "purple"),
                     breaks = c("臺北", "臺中", "臺南", "臺東"),
                     name = "城市") +
  #coord_cartesian(ylim = c(5, NA)) +  # 设置y轴的范围从5开始
  theme_minimal()


