setwd("~/R/src/qe/R/one-off/高信百诺/")

source("./functions.R")


# 参数 ----------------------------------------------------------------------

as_of <- "2018-05-31"

product_name <- "高信百诺1期"

bm_fund <- "000480.OF"
bm_name <- .wsd_tidy(bm_fund, "sec_name", as_of, as_of)$value

# get product NAV
data_file <- "./extdata/高信百诺1期2017年季末持仓.xls"

raw_data <- import_holdings(data_file)

# raw_data %>% group_by(date) %>% summarise(sum(weight))

holding <- clean_holdings(raw_data)

stock_holding <- holding %>%
  mutate(
    weight = weight * 100,
    sec_type = .wsd_vec_multi_code(code, "sec_type", as_of)
    ) %>%
  filter(sec_type == "普通股") %>%
  select(-sec_type) %>%
  nest(-date)

f_calc_beta <- function(date, data){
  mutate(data, beta = .wsd_tidy(data$code, "beta_100w", date, date)$value)
}
stock_holding %>%
  mutate(
    data2 = map2(date, data, f_calc_beta)
    # beta = .wsd_tidy(data$code, "beta_100w", date, date)$value
    # nr = map_dbl(date, length)
    )
df2 <- .Last.value
df2 %>% unnest() %>%
  # filter(name == "格力电器")
  filter(date == "2017-12-29")
filter(code == "002415.SZ")


stock_holding <- holding %>%
  filter(sec_type == "普通股") %>%
  select(-sec_type)

stock_holding %>%
  group_by(date) %>%
  summarise(weight = sum(weight))


# 比较基准的持仓 -----------------------------------------------------------------

date_range <- range(holding$date)

bm_fund_holding <- get_wind_holdings(bm_fund, date_range[1], date_range[2])


# 前十重仓股占股票市值比 -------------------------------------------------------------

full_join(
holding %>%
  filter(code != "511880.SH") %>%
  nest(-date) %>%
  # mutate(top10_pct = map(data, f_top_n_pct))
  mutate(weight = map_dbl(data, top_n_pct)) %>%
  select(-data) %>%
  setNames(c("date", product_name)),
bm_fund_holding %>%
  group_by(date) %>%
  mutate(weight = proportiontototalstockinvestments/100) %>%
  top_n(10, weight) %>%
  summarise(sum(weight)) %>%
  setNames(c("date", bm_name)),
by = "date")

hld_nest <- holding %>%
  filter(code != "511880.SH") %>%
  nest(-date)

start_date <- hld_nest[[1, "date"]]
end_date <- hld_nest[[2, "date"]]
data <- hld_nest[[1, "data"]]

hld_nest <- hld_nest %>%
  mutate(end_date = lead(date))
hld_nest[nrow(hld_nest), "end_date"] = as_of

hld_nest_test <- hld_nest
hld_nest_test[[1, "data"]] <- hld_nest_test[[1, "data"]][1:2,]

f <- function(date, end_date, data){
  wsd_vol(data$code, date, end_date)
}

hld_nest_test[1,] %>%
  pmap(f)

out <- hld_nest %>%
  mutate(vol = pmap(., f))

out[[1, "vol"]]



hld_nest[1, ] %>%
  map2(date, data, f)
# nest(holding, -date)[[1, "data"]]

# list(
#   product = holding %>% filter(code != "511880.SH"),
#   bm = bm_fund_holding
#   ) %>%
#   bind_rows(.id = "fund") %>%
#   group_by(fund, date) %>%
#   top_n(10, weight) %>%
#   summarise(weight = sum(weight), count = n()) %>%
#   select(date, fund, weight) %>%
#   spread(fund, weight)


# holding %>% dplyr::filter_all(any_vars(is.na(.)))

# bm_fund_holding %>% group_by(date) %>% summarise(w = sum(weight), n())

