
# Import portfolios -------------------------------------------------------

prt1 <- rds_read("~/R/projects/wm/R/portfolio-analysis/高信百诺1期/stock_prt.rds")
prt1 <- prt1 %>% mutate(date = get_month_end(date))
dates <- unique(prt1$date)

prod_code <- "000480.OF"
prt2 <- prt_from_wind(prod_code, dates)
# wsd_xc(prod_code, "sec_name")

f_weight_summary <- function(prt){
  prt %>%
    group_by(date) %>%
    mutate(
      cum_weight = order_by(desc(weight), cumsum(weight)),
      weight_rank = min_rank(desc(weight)),
      top10_weight = weight * (weight_rank <= 10)
    ) %>%
    summarise(
      股票权重 = sum(weight),
      `10大重仓股权重` = sum(top10_weight),
      股票个数 = n()
    )
}

weight_dist <- list(
  "高信百诺1期" = prt1,
  "东方红新动力" = prt2
) %>%
  map_df(f_weight_summary, .id = "产品")

stats_comp <- weight_dist %>%
  # mutate(qtr = lubridate::quarter(date) %% 2 == 1)
  mutate(股票权重 = case_when(
    lubridate::quarter(date) %% 2 == 1 ~ `10大重仓股权重`,
    TRUE ~ 股票权重
  )) %>%
  select("date", "产品", "股票权重", "10大重仓股权重") %>%
  gather("key", "value", -c("date", "产品")) %>%
  spread("产品", "value") %>%
  arrange(key, date)

col_map <- list(
  日期 = sym("date"),
  指标 = sym("key")
  )

stats_comp %>%
  rename(!!!col_map)
  # rename(日期 = date)

period_tbl <-
  left_join(
    tibble(start = dates),
    tibble(
      end = qtr_end(first(dates), Sys.Date()) %>% as.character(),
      start = lag(end)
    ))


f_prt_ret <- function(prt) {
  left_join(prt, period_tbl, by = c("date" = "start")) %>%
    # filter(code == "000651.SZ") %>%
    mutate(ret = map2_dbl(
      code,
      end,
      ~ wsd_xc(.x, "pct_chg", .y, "Period=Q;Days=Alldays")
    ))
}

f_prod_ret <- function(code) {
  .wsd_tidy(
    code,
    "nav_adj",
    first(period_tbl$start),
    last(period_tbl$end),
    "Period=Q;Days=Alldays"
  ) %>%
    df_to_xts("value") %>%
    Return.calculate() %>%
    setNames("产品收益")
}

prt1_ret <- f_prt_ret(prt1)
prt2_ret <- f_prt_ret(prt2)

prod1_ret <- f_prod_ret("XT102034.XT")
prod2_ret <- f_prod_ret("000480.OF")

f_merge_ret <- function(prt_ret, prod_ret){
  prt_ret %>%
    group_by(end) %>%
    summarise(sum(weight * ret / 100)) %>%
    df_to_xts() %>%
    setNames("组合收益") %>%
    merge(prod_ret, join = "left")
}

f_merge_ret(prt1_ret, prod1_ret)
f_merge_ret(prt2_ret, prod2_ret)

# prt1_ret %>%
prt2_ret %>%
  group_by(date) %>%
  summarise(cor(weight, ret))

prt1_ret %>%
  group_by(date) %>%
  top_n(5, weight)
