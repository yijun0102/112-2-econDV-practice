'''
install.packages("conflicted")
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
'''

# 匯入使用的套件
library(tidyverse)
library(lubridate)

# 建立資料框
exchange_rate <- tibble(
  date = rep(seq(as.Date("1960-01-01"), as.Date("1962-02-01"), by = "months"), 3),
  country = rep(c("美元", "日元", "英鎊"), each = 26),
  rate = runif(78, 20, 40)
)

# 計算匯率升值率 
exchange_rate <- exchange_rate %>%
  arrange(country, date) %>%
  group_by(country) %>%
  mutate(rise_rate = (rate - lag(rate)) / lag(rate))

head(exchange_rate)
glimpse(exchange_rate)

# 繪製折線圖
ggplot(exchange_rate, aes(x = date, y = rate, color = country)) +
  geom_line() +
  labs(x = "日期", y = "對台幣匯率升值率", color = "國家") +
  theme_minimal()

#########################################
#引入資料程式碼
library(readr)
exchangeRate <- read_csv("~/Downloads/BP01M01.csv")
dplyr::glimpse(exchangeRate)

# 假设exchangeRate是您的数据框或列表，包含日期字符串的列名为日期
exchangeRate$期間 <- as.Date(paste0(exchangeRate$期間, "01"), format = "%YM%m%d")

# 計算新台幣對美元的匯率
ntd_usd_rate <- exchangeRate$`新台幣NTD/USD`

# 計算其他貨幣的匯率，對新台幣的匯率，並儲存到新的欄位
exchangeRate <- exchangeRate %>%
  mutate(`日圓` = ntd_usd_rate / `日圓JPY/USD`,
         `英鎊` = ntd_usd_rate / (1/`英鎊USD/GBP`),
         `港幣` = ntd_usd_rate / `港幣HKD/USD`,
         `韓元` = ntd_usd_rate / `韓元KRW/USD`,
         `美元` = `新台幣NTD/USD`)

head(exchangeRate)
glimpse(exchangeRate)

#刪除不必要的欄位
new_exchangeRate <- exchangeRate %>%
  select(-`新台幣NTD/USD`, -`日圓JPY/USD`, -`英鎊USD/GBP`, -`港幣HKD/USD`, -`韓元KRW/USD`)
#  select(`期間`,`日圓`, `港幣`, `美元`, `英鎊`, `韓元`)

#成長率繪圖
exchangeRate_growth <- new_exchangeRate %>%
  arrange(期間) %>%
  mutate(across(c("美元", "日圓", "英鎊", "港幣", "韓元"), 
                ~( . - lag(.) ) / lag(.), 
                .names = "成長率_{.col}"))

head(exchangeRate_growth)

# 改為長格式
exchangeRate_growth <- exchangeRate_growth %>% 
  pivot_longer(cols = starts_with("成長率"),
               names_to = "country",
               values_to = "升值率")

head(exchangeRate_growth)

# 繪變化圖
exchangeRate_growth %>%
  ggplot(exchangeRate_growth, aes(x = 期間, y = country, color = currency)) +
  geom_line() +
  labs(title = "Exchange Rate Growth Rate Trends",
       x = "Date",
       y = "Growth Rate (%)",
       color = "Currency") +
  theme_minimal()

exchangeRate_long <- new_exchangeRate %>%
  gather(currency, rate, -期間)

#走勢繪圖
ggplot(exchangeRate_long, aes(x = 期間, y = rate, color = currency)) +
  geom_line() +
  labs(title = "Exchange Rate Trends",
       x = "Date",
       y = "Exchange Rate",
       color = "Currency")

