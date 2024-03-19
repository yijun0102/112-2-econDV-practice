# 引入資料 ----
library(readr)
tp <- read_csv("~/Downloads/taipei.csv")
tc <- read_csv("~/Downloads/taichung.csv")
tn <- read_csv("~/Downloads/tainan.csv")
tt <- read_csv("~/Downloads/taitung.csv")

# 创建一个包含四个数据框的列表
data_list <- list(tp, tc, tn, tt)

# 循环对每个数据框进行操作
for (i in seq_along(data_list)) {
  data_list[[i]] <- data_list[[i]] %>%
    mutate(日溫差 = `絕對最高氣溫(℃)` - `絕對最低氣溫(℃)`) %>%
    select(觀測時間, 測站, 日溫差)
}

# 使用 bind_rows() 函数合并四个数据框
data <- bind_rows(data_list)
head(data)

# 转换为宽格式
data <- pivot_wider(data, names_from = 測站, values_from = 日溫差)
head(data)

## 計算變化率
daily <- data %>%
  arrange(觀測時間) %>%
  mutate(across(c("臺北", "臺中", "臺南", "臺東"), 
                ~( . - lag(.) ) / lag(.), 
                .names = "變化率_{.col}"))
head(daily)

## 變化率改成長格式 -----
daily <- daily %>% 
  pivot_longer(cols = starts_with("變化率"),
               names_to = "地區",
               values_to = "變化率")
head(daily)

## 作圖 ------
daily %>%
  ggplot(aes(x = 觀測時間, y = 變化率, color = 地區)) +
  geom_line() +
  labs(x = "日期",
       y = "溫差變化率", 
       color = "地區") +
  theme_minimal()

## 作圖 ------
ggplot(data, aes(x = 觀測時間)) +
  geom_line(aes(y = 臺北, color = "臺北"), alpha = 1) +
  geom_line(aes(y = 臺中, color = "臺中"), alpha = 1) +
  geom_line(aes(y = 臺南, color = "臺南"), alpha = 1) +
  geom_line(aes(y = 臺東, color = "臺東"), alpha = 1) +
  labs(title = "各地溫差圖", x = "日期", y = "日溫差") +
  scale_color_manual(values = c("臺北" = "skyblue", "臺中" = "lightgreen", "臺南" = "orange", "臺東" = "pink"),
                     breaks = c("臺北", "臺中", "臺南", "臺東"),
                     name = "地區") +
  theme_minimal()


