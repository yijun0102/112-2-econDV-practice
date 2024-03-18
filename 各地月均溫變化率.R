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


