library(dplyr)
library(reader)
library(ggplot2)
library(plotly)

results <- list()

# 讀取資料
house <- read_csv("/Users/yeyijun/Documents/EconDV/112-2-econDV-practice/房價所得比2002-2022.csv")
birth <- read_csv("/Users/yeyijun/Documents/EconDV/112-2-econDV-practice/生育率2002-2022.csv")

#head(birth)

# 移除空值欄位
house[house == "---"] <- NA
house <- house %>%
  select_if(function(col) !all(is.na(col)))

# 計算每四筆資料加總
house <- house %>%
  mutate(年度季別 = as.numeric(substring(年度季別, 1, 3)) + 1911) %>%
  group_by(年度季別) %>%
  summarise_all(mean)

# 重新命名欄位
names(house)[1] <- "年份"
head(house)

# 移除空值欄位
birth <- birth %>%
  select(-金門縣, -連江縣)
names(birth)[2] <- "全國"
names(birth)[4] <- "台北市"
names(birth)[6] <- "台中市"
names(birth)[7] <- "台南市"
names(birth)[17] <- "台東縣"
head(birth)

# 取得城市清單
cities <- colnames(house)[-1]

# 建立空的資料框架
data <- data.frame()

# 使用迴圈整合資料
for (city in cities) {
  temp <- data.frame(
    house = house[[city]],
    birth = birth[[city]]
  )
  temp$city <- city
  data <- rbind(data, temp)
}

plot <- plot_ly(data, x = ~house, y = ~birth, color = ~city, text = ~city) %>%
  add_markers() %>%
  layout(
    xaxis = list(title = "房價所得比"),
    yaxis = list(title = "生育率"),
    showlegend = FALSE
  )

plot



'''
house_changes <- house %>%
  arrange(年份) %>%
  mutate(across(-年份, ~(. - lag(.))/lag(.)*100))
  
birth_changes <- birth %>%
  arrange(年份) %>%
  mutate(across(-年份, ~(. - lag(.))/lag(.)*100))

# 秀出結果
print(house_changes)
print(birth_changes)
'''



