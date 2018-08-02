
# 统计指标 --------------------------------------------------------------------

aggr_df <- tribble(
  ~指标, ~f, ~args,
  "全部股票仓位", df_group_sum, list(quo(date), quo(weight)),
  "前10重仓股仓位", df_group_top_n_sum, list(gp = quo(date), n = 10, value = quo(weight))
)

f_aggr <- function(prt) {
  invoke_map(aggr_df$f, df = prt1, aggr_df$args) %>%
    reduce(full_join, by = "date") %>%
    setNames(c("日期", aggr_df$指标))
}

# 导入组合 -------------------------------------------------------

prt1 <- rds_read("~/R/projects/wm/R/portfolio-analysis/高信百诺1期/stock_prt.rds")
prt1 <- prt1 %>% mutate(date = get_month_end(date))
dates <- unique(prt1$date)

prod_code <- "000480.OF"
prt2 <- prt_from_wind(prod_code, dates)


# 计算 ----------------------------------------------------------------------

list(prt1 = prt1, prt2 = prt2) %>%
  map_df(f_aggr, .id = "产品")


# Use nested portfolio ----------------------------------------------------

prt1_nest <-
prt1 %>% nest(-date)

prt1_nest[[1, "data"]]

f <- function(data){ sum(data$weight)}

prt1_nest %>%
  mutate(total_weight = map_dbl(data, f))

prt1_nest %>%
  mutate(total_weight = map_dbl(data, ~ sum(.$weight)))

f <- function(data){ sum((top_n(data, 10, weight))$weight)}

prt1_nest %>%
  mutate(top10_weight = map_dbl(data, f))

prt1_nest %>%
  mutate(top10_weight = map_dbl(data, ~ sum(top_n(., 10, weight)$weight)))


f_weight_stats <- function(df) {
  df %>%
    summarise(
      top10w = sum(top_n(., 10, weight)$weight),
      w = sum(.$weight)
      )
}

prt1_nest %>% mutate(stats = map(data, f)) %>% unnest(stats)

prt2 %>% nest(-date) %>% mutate(stats = map(data, f)) %>% unnest(stats)

map_ts <- function(prt, f){
  prt %>% nest(-date) %>% mutate(.stats = map(data, f)) %>% unnest(.stats)
}

list(prt1 = prt1, prt2 = prt2) %>%
  map_df(~map_ts(., f_weight_stats), .id = "产品")

f_size_chg <- function(df, df_lag) {
  if(any(is.na(df), is.na(df_lag)))
    return(tibble(size_chg = 0L))
  tibble(size_chg = nrow(df) - nrow(df_lag))
}

prt1_nest %>% mutate(size_chg = map2(data, lag(data), f_size_chg))

map_ts_lag <- function(prt, f){
  prt %>% nest(-date) %>% mutate(.stats = map2(data, lag(data), f)) %>% unnest(.stats)
}

list(prt1 = prt1, prt2 = prt2) %>%
  map_df(~map_ts_lag(., f_size_chg), .id = "产品")

