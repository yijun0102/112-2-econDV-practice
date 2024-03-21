library(tidyverse)

# 創建一個空的列表，用於存儲所有學年的 CSV 下載網址
csv_urls <- list()

# 迴圈生成所有學年的 CSV 下載網址
for (year in 104:112) {
  # 生成學年的 CSV 下載網址並添加到列表中
  csv_urls[[as.character(year)]] <- paste0("https://stats.moe.gov.tw/files/ebook/native/", year, "/", year, "native_A1-1.csv")
}

# 將結果存入results中
results <- list(csv_urls = csv_urls)
print(csv_urls)

## download and import ----
# 建立一個空的list來存儲下載的資料
data_list <- list()

# 遍歷所有學年的 CSV 下載網址
for (year in names(csv_urls)) {
  # 排除110年，單獨處理
  if (year == 110) {
    # 下載 CSV 文件，並指定文件編碼為 BIG5
    download.file(csv_urls[[year]], destfile = paste0(year, ".csv"))
    
    # 讀取 CSV 文件並儲存到列表中
    data_list[[year]] <- read.csv(paste0(year, ".csv"), fileEncoding = "BIG5")
  } else {
    # 下載並讀取其他學年的 CSV 文件
    download.file(csv_urls[[year]], destfile = paste0(year, ".csv"))
    data_list[[year]] <- read.csv(paste0(year, ".csv"))
  }
}

'''
for (year in names(csv_urls)) {
  # 下載 CSV 文件
  download.file(csv_urls[[year]], destfile = paste0(year, ".csv"))
  
  # 讀取 CSV 文件並存儲到列表中
  data_list[[year]] <- read.csv(paste0(year, ".csv"))
}

# 將結果存儲在環境中已有的list物件results中
results$data_list <- data_list
'''

library(purrr)
library(dplyr)

# 使用imap()函數向每個DataFrame添加"學年度"欄位
data_list <- imap(data_list, ~ mutate(.x, 學年度 = as.integer(gsub("\\D", "", .y))))

# 將結果存儲在環境中已有的list物件results中
results$data_list_with_year <- data_list

# merge ----
# 垂直合併data_list中的每個DataFrame
combined_data <- bind_rows(data_list)

# 將結果存儲在環境中已有的list物件results中
results$combined_data <- combined_data

view(combined_data)

## 畫圖
library(ggplot2)

# 計算每个學年各學制的人數總和
sum_data <- combined_data %>%
  group_by(學年度) %>%
  summarise(
    在學學生人數_博士班 = sum(在學學生人數_博士班),
    在學學生人數_碩士班 = sum(在學學生人數_碩士班),
    在學學生人數_學士班 = sum(在學學生人數_學士班)
  ) %>%
  pivot_longer(cols = starts_with("在學學生人數_"), names_to = "學制", values_to = "在學學生人數")

# 重新定義學制levels
sum_data$學制 <- factor(sum_data$學制, levels = c("在學學生人數_博士班", "在學學生人數_碩士班", "在學學生人數_學士班"))

'''
# 繪製堆積長條圖
ggplot(sum_data, aes(x = 在學學生人數, y = factor(學年度), fill = 學制)) +
  geom_bar(stat = "identity") +
  labs(
    title = "各學年學制人數佔比",
    x = "在學學生人數",
    y = "學年度",
    fill = "學制"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
'''

# 繪製 ggplot2 互動式圖表
library(plotly)

gg <- ggplot(sum_data, aes(x = 在學學生人數, y = factor(學年度), fill = 學制)) +
  geom_bar(stat = "identity") +
  labs(
    title = "各學年學制人數佔比",
    x = "在學學生人數",
    y = "學年度",
    fill = "學制"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 將 ggplot2 圖表轉換為 Plotly 圖表
plotly_chart <- ggplotly(gg)

# 顯示 Plotly 圖表
plotly_chart

