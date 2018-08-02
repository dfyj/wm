# setwd("~/R/src/qe/R/one-off/高信百诺/")

# load("./data/nav_stats.RData")

# 参数 ----------------------------------------------------------------------

end_date <- "2018-05-31"

universe <- tribble(
  ~code,
  "XT102034.XT",# 产品
  "000480.OF",  # 基准 1
  "H00300.CSI"  # 基准 2
)

universe <- universe %>%
  mutate(
    name = mem_wsd(code, "sec_name", end_date, end_date)$Data$SEC_NAME,
    inception_date = wind_num_to_date(
      mem_wsd(code, "fund_setupdate", end_date, end_date)$Data$FUND_SETUPDATE
    ))

start_date <-
  universe$inception_date[1]
# "2014-01-30"
# "2017-12-29"

# 导入净值数据 ------------------------------------------------------------------

codes <- pull(universe[c(1,3), "code"])

nav <- wsd_price(universe$code, universe[[1, "inception_date"]], end_date)

nav_ts <- nav %>% select(-field) %>% spread(code, value) %>% df_to_xts()

ret <- nav_ts %>%
  Return.calculate() %>%
  `[`(, universe$code)

# 年化收益 ~ 年化波动 -------------------------------------------------------------

ret_intersect <- ret %>%
  xts_to_df() %>%
  {dplyr::filter_if(., is.numeric, dplyr::all_vars(!is.na(.)))}  %>%
  df_to_xts()

f <- function(name, i){
  # nav_metrics_alpha_beta(ret_intersect) %>%
  nav_metrics_risk_return(ret_intersect["2017", c(i,3)], "tibble")
}

product_names <- universe$name[1:2]

stats <- product_names %>%
  imap(., f) %>%
  setNames(product_names) %>%
  bind_rows(.id = "产品")

stats %>%
  write.csv(file = "clipboard", fileEncoding = "native.enc", row.names = FALSE)

stats[] %>% select(
  "产品",
  "年化收益",
  "年化波动"
) %>%
  ggplot() +
  geom_point(aes(年化波动, 年化收益, color = 产品), size = 2) +
  ggtitle("风险收益分布——与指数比较") +
  labs(
    color = "") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(xlim = c(0.1, 0.3), ylim = c(0, 0.3)) +
  ggrepel::geom_text_repel(aes(年化波动, 年化收益, label = 产品)) +
  plot_theme_no_legend()

ggsave("plots/风险收益分布——与指数比较.jpg")

# 年化收益 ~ 年化波动 - 3个品种 ------------------------------------------------------

stats[] %>% select(
  "产品",
  "年化收益",
  "年化波动"
) %>%
  ggplot() +
  geom_point(aes(年化波动, 年化收益, color = 产品), size = 2) +
  ggtitle("风险收益分布——与东方红新动力比较") +
  labs(
    caption = str_c("数据日期: ", str_c(c(start(Ra), end(Ra)), collapse = " 至 ")),
    color = "") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(xlim = c(0.1, 0.3), ylim = c(0, 0.3)) +
  ggrepel::geom_text_repel(aes(年化波动, 年化收益, label = 产品)) +
  plot_theme_no_legend()

ggsave("plots/风险收益分布——与东方红新动力比较.jpg")

# 年化收益 ~ 年化波动 - 2017年 ------------------------------------------------------

Ra <- Return.calculate(nav %>% na.omit())["2017"]
Rb <- Ra[, ncol(Ra)]

stats <- as.list(Ra) %>%
  map(~ pu_return_stats(., Rb)) %>%
  setNames(universe$name) %>%
  bind_rows(.id = "产品")

stats[] %>% select(
  "产品",
  "年化收益",
  "年化波动"
) %>%
  ggplot() +
  geom_point(aes(年化波动, 年化收益, color = 产品), size = 2) +
  ggtitle("风险收益分布——2017年") +
  labs(
    caption = str_c("数据日期: ", str_c(c(start(Ra), end(Ra)), collapse = " 至 ")),
    color = "") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(xlim = c(0, 0.3), ylim = c(0, 0.8)) +
  ggrepel::geom_text_repel(aes(年化波动, 年化收益, label = 产品)) +
  plot_theme_no_legend()

ggsave("plots/风险收益分布——2017年.jpg")

# 3年滚动年化收益 ----------------------------------------------------------------

Ra_log <- Return.calculate(nav, "log")
coredata(Ra_log)[is.nan(Ra_log)] <- NA
rolling_1y_ret_annual <- exp(rollapply(Ra_log, 52, mean) * 52) - 1
rolling_3y_ret_annual <- exp(rollapply(Ra_log, 52*3, mean, align = "left")
                             * 52) - 1

f_subset_xts <- function(ts){
  date_range <- ts[, 1] %>% na.omit() %>% index() %>% range()
  ts[str_c(date_range, collapse = "/")]
}

f_subset_xts(rolling_3y_ret_annual) %>%
  setNames(universe$name) %>%
  xts_to_df() %>%
  mutate(DATETIME = as.Date(DATETIME)) %>%
  gather("key", "value", -DATETIME) %>%
  ggplot() +
  ggtitle("未来3年滚动年化收益") +
  geom_line(aes(DATETIME, value, color = key), size = 1) +
  labs(
    # caption = str_c("数据日期: ", str_c(c(start_date, end_date), collapse = " 至 ")),
    # caption = str_c("数据日期: 成立日", str_c(c(start_date, end_date), collapse = " 至 ")),
    color = "") +
  scale_y_continuous(labels = scales::percent) +
  plot_theme_no_axis_title()

ggsave("plots/3年滚动年化收益.jpg")

f_subset_xts(rolling_3y_ret_annual) %>%
  dygraph(main = "") %>%
  dyRangeSelector()


# 滚动风险 --------------------------------------------------------------------

rolling_1y_vol_annual <- rollapply(Ra_log, 52, sd) * sqrt(52)

f_subset_xts(rolling_1y_vol_annual) %>%
  setNames(universe$name) %>%
  xts_to_df() %>%
  mutate(DATETIME = as.Date(DATETIME)) %>%
  gather("key", "value", -DATETIME) %>%
  ggplot() +
  ggtitle("历史风险") +
  geom_line(aes(DATETIME, value, color = key), size = 1) +
  labs(
    caption = "指标说明：滚动1年期收益标准差，周频率",
    # caption = str_c("数据日期: 成立日", str_c(c(start_date, end_date), collapse = " 至 ")),
    color = "") +
  scale_y_continuous(labels = scales::percent) +
  plot_theme_no_axis_title()

ggsave("plots/历史风险.jpg")

bm_vol <- rolling_1y_vol_annual[, ncol(rolling_1y_vol_annual)]

rolling_1y_vol_annual_rel <- rolling_1y_vol_annual %>%
  as.list %>%
  map(~ ./bm_vol) %>%
  bind_cols() %>%
  xts(order.by = index(rolling_1y_vol_annual))

f_subset_xts(rolling_1y_vol_annual_rel) %>%
  setNames(universe$name) %>%
  xts_to_df() %>%
  mutate(DATETIME = as.Date(DATETIME)) %>%
  gather("key", "value", -DATETIME) %>%
  ggplot() +
  ggtitle("历史风险—相对市场") +
  geom_line(aes(DATETIME, value, color = key), size = 1) +
  labs(
    caption = "指标说明：滚动1年期收益标准差，产品除以沪深300指数，周频率",
    # caption = str_c("数据日期: 成立日", str_c(c(start_date, end_date), collapse = " 至 ")),
    color = "") +
  scale_y_continuous(labels = scales::percent) +
  plot_theme_no_axis_title()

ggsave("plots/历史风险—相对市场.jpg")

f_subset_xts(rolling_1y_vol_annual) %>%
  dygraph() %>%
  dyRangeSelector()

save.image("./data/nav_stats.RData")
