code <- "000480.OF"
dates <- qtr_end("2017-01-01", "2017-12-31")

# date <- last(dates)
# hld <-
#   wind_fund_holdings(date, code) %>%
#   select(code = wind_code, name = stock_name, weight = proportiontonetvalue)

prt <- prt_from_wind(code, dates)

prt %>%
  group_by(date) %>%
  summarise(weight = sum(weight), count = n())

prt_nest <- prt %>%
  nest(-date)

prt_nest %>%
  mutate(nr = map_dbl(data, ~ sum(.$weight)))

stock_hld_nest <- stock_holding %>%
  nest(-date)

prt_nest %>%
  # stock_hld_nest %>%
  mutate(
    weight = map_dbl(data, ~ sum(.$weight)),
    count = map_dbl(data, nrow)
    )

date <- stock_hld_nest$date[[1]]
data <- stock_hld_nest$data[[1]]

calc_total_weight <- function(date, data){
  if(lubridate::quarter(date) %% 2 == 1){
    # odd-quarter, only sum up top 10 holdings
    sum(top_n(data, 10, weight)$weight)
  }
  else{
    sum(data$weight)
  }
}

smry <- list(
  port1 = prt_nest,
  port2 = stock_hld_nest
) %>%
  map_df(
    ~ mutate(.,
             weight = map2_dbl(date, data, calc_total_weight),
             weight2 = map_dbl(data, ~ sum(.$weight)),
             count = map_dbl(data, nrow)
    ),
    .id = "product"
  )

smry %>%
  select(date, product, weight) %>%
  spread(product, weight)
