
# Download an show OTC options pricing snapshot data ----------------------

date <- "2018-06-14"

# download pricing data from interotc site
df_raw <- read.table(file = "clipboard", sep = "\t",
                     fileEncoding = "native.enc", header=TRUE, stringsAsFactors = FALSE)

attr(df_raw, "source") <- "https://derivatives.interotc.com.cn/cmis-web/a/product/index#ckbj"

rds_path <- sprintf("~/R/data/interotc/options/%s.rds", date)
rds_write(df_raw, rds_path)

# clean data and show
df_raw = rds_read("~/R/data/interotc/options/2018-06-14.rds")

numeric_cols <- c("交易期限", "行权价格", "障碍价格", "参考卖价")

df <- df_raw %>%
  as_tibble() %>%
  mutate_at(numeric_cols, readr::parse_number)

df %>%
  filter(期权结构 == "香草型" & 行权价格 == 100) %>%
  spread("挂钩标的", "参考卖价") %>%
  write.csv(file = "clipboard", fileEncoding = "native.enc", row.names = FALSE)

# | 期权结构 | 期权类型 | 交易期限 | 行权价格 | 障碍价格 | 障碍事件 | 沪深300指数 | 上证50指数 | 中证500指数 |
# | -------- | -------- | -------- | -------- | -------- | -------- | ----------- | ---------- | ----------- |
# | 香草型   | 看跌     | 90       | 100      | NA       |          | 5.6         | 5.37       | 5.88        |
# | 香草型   | 看涨     | 90       | 100      | NA       |          | 4.39        | 4.54       | 4.67        |

df %>%
  filter(期权结构 == "单鲨型") %>%
  spread("挂钩标的", "参考卖价") %>%
  write.csv(file = "clipboard", fileEncoding = "native.enc", row.names = FALSE)

# | 期权结构 | 期权类型 | 交易期限 | 行权价格 | 障碍价格 | 障碍事件   | 沪深300指数 | 上证50指数 | 中证500指数 |
# | -------- | -------- | -------- | -------- | -------- | ---------- | ----------- | ---------- | ----------- |
# | 单鲨型   | 看跌     | 90       | 100      | 85       | 敲出无补偿 | 3.21        | 2.96       | 3.07        |
# | 单鲨型   | 看涨     | 90       | 100      | 115      | 敲出无补偿 | 2.29        | 2.39       | 2.24        |
