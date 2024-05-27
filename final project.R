library(dplyr)
library(readr)
library(ggplot2)
library(plotly)

# 讀取資料
house_Q <- read_csv("/Users/yeyijun/Documents/EconDV/112-2-econDV-practice/房價所得比2002-2022.csv")
birth <- read_csv("/Users/yeyijun/Documents/EconDV/112-2-econDV-practice/生育率2002-2022.csv")

# 移除空值欄位
house_Q[house_Q == "---"] <- NA
house_Q <- house_Q %>%
  select_if(function(col) !all(is.na(col)))

# 計算每四筆資料加總
house <- house_Q %>%
  mutate(年度季別 = as.numeric(substring(年度季別, 1, 3)) + 1911) %>%
  group_by(年度季別) %>%
  summarise_all(mean, na.rm = TRUE)

# 重新命名欄位
names(house)[1] <- "年份"

# 移除空值欄位
birth <- birth %>%
  select(-金門縣, -連江縣)
names(birth)[2] <- "全國"
names(birth)[4] <- "台北市"
names(birth)[6] <- "台中市"
names(birth)[7] <- "台南市"
names(birth)[17] <- "台東縣"

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

# 設定顏色
data$color <- "#c2c1be"
data$color[data$city == "全國"] <- '#2A9D8f'
data$color[data$city == "台北市"] <- "#E76F51"

data$label <- "其他城市"
data$label[data$city == "全國"] <- "全國"
data$label[data$city == "台北市"] <- "台北市"
      

# 繪製互動式點狀圖
plot <- plot_ly() %>%
  add_trace(
    data = data %>% filter(label == "其他城市"),
    x = ~house, y = ~birth, type = 'scatter', mode = 'markers',
    marker = list(color = "#c2c1be", size = 12, opacity = 0.3),
    name = "其他城市"
  ) %>%
  add_trace(
    data = data %>% filter(label == "全國"),
    x = ~house, y = ~birth, type = 'scatter', mode = 'markers',
    marker = list(color = "#2A9D8f", size = 12, opacity = 0.7),
    name = "全國"
  ) %>%
  add_trace(
    data = data %>% filter(label == "台北市"),
    x = ~house, y = ~birth, type = 'scatter', mode = 'markers',
    marker = list(color = "#E76F51", size = 12, opacity = 0.7),
    name = "台北市"
  ) %>%
  layout(
    xaxis = list(title = "<b>房價所得比(年)<b>", showline = TRUE, zeroline = TRUE),
    yaxis = list(title = "<b>生育率(‰)<b>", angle = 90, showline = TRUE, zeroline = TRUE),
    legend = list(traceorder = "reversed"),
    showlegend = TRUE,
    title = list(
      text = "<b>生育率與房價所得比的關聯</b><br><span style='font-size:17px;'>台灣各縣市近二十年</span>",
      x = 0.01,
      y = 0.95,
      xanchor = 'left',
      yanchor = 'top',
      font = list(size = 20)
    ),
    margin = list(t = 100, b = 100),
    annotations = list(
      list(
        x = -0.05,
        y = -0.15,
        xref = "paper",
        yref = "paper",
        text = "資料來源：內政部不動產資訊平台、行政院主計處",
        showarrow = FALSE,
        xanchor = 'left',
        yanchor = 'bottom'
      )
    )
  )

# 設定 x 範圍
x_range <- seq(min(data$house, na.rm = TRUE), max(data$house, na.rm = TRUE), length.out = 100)

# 全國迴歸線
data_national <- data %>% filter(city == "全國")
lm_national <- lm(birth ~ house, data = data_national)
national_y_range <- predict(lm_national, newdata = data.frame(house = x_range))
plot <- plot %>%
  add_lines(
    x = x_range, y = national_y_range,
    name = NULL, line = list(color = '#2A9D8f', width = 4), showlegend = FALSE
  ) %>%
  add_annotations(
    x = x_range[100], y = national_y_range[100], text = "<b> 全國<b>",
    showarrow = FALSE, font = list(color = '#2A9D8f', size = 16), xanchor = 'left'
  )

# 台北迴歸線
data_taipei <- data %>% filter(city == "台北市")
lm_taipei <- lm(birth ~ house, data = data_taipei)
taipei_y_range <- predict(lm_taipei, newdata = data.frame(house = x_range))
plot <- plot %>%
  add_lines(
    x = x_range, y = taipei_y_range,
    name = NULL, line = list(color = '#FFF', width = 5), showlegend = FALSE
  ) %>%
  add_lines(
    x = x_range, y = taipei_y_range,
    name = NULL, line = list(color = '#E76F51', width = 4), showlegend = FALSE
  ) %>%
  add_annotations(
    x = x_range[100], y = taipei_y_range[100], text = "<b> 台北<b>",
    showarrow = FALSE, font = list(color = '#E76F51', size = 16), xanchor = 'left'
  )

# 顯示圖表
plot


#############################plt2
# 繪製房價-時間關係圖
house_Q$year[house_Q$年度季別] <- "substr(年度季別, 1, 3) + 1911)"

# 繪製房價-時間關係圖
plot2 <- plot_ly() %>%
  add_trace(
    data = house_Q %>% select(-全國, -台北市) %>% pivot_longer(-c(年度季別, year), names_to = "city", values_to = "value"),
    x = ~年度季別, y = ~value, type = 'scatter', mode = 'lines',
    line = list(color = "rgba(194, 193, 190, 0.3)", width = 5),
    name = ~city,
    showlegend = FALSE
  ) %>%
  add_trace(
    data = house_Q,
    x = ~年度季別, y = ~台北市, type = 'scatter', mode = 'lines',
    line = list(color = "#E76F51", width = 5),
    name = "台北市"
  ) %>%
  add_trace(
    data = house_Q,
    x = ~年度季別, y = ~全國, type = 'scatter', mode = 'lines',
    line = list(color = "#2A9D8f", width = 5),
    name = "全國"
  ) %>%
  layout(
    xaxis = list(
      title = "年度季別",
      showline = TRUE,
      zeroline = TRUE,
      tickangle = 45
    ),
    yaxis = list(title = "房價所得比", showline = TRUE, zeroline = TRUE, automargin = TRUE),
    showlegend = TRUE,
    title = list(
      text = "<b>房價趨勢圖</b><br><span style='font-size:15px;'>台灣近20年</span>",
      x = 0.01,
      y = 0.97,
      xanchor = 'left',
      yanchor = 'top',
      font = list(size = 20)
    ),
    margin = list(t = 100, b = 100),
    annotations = list(
      list(
        x = -0.06,
        y = -0.15,
        xref = "paper",
        yref = "paper",
        text = "資料來源：內政部不動產資訊平台",
        showarrow = FALSE,
        xanchor = 'left',
        yanchor = 'bottom'
      )
    )
  )

# 顯示圖表
plot2
