
# Import ------------------------------------------------------------------

codes <- c(
  "000300.SH",
  "CBA00101.CS"

  # "H00300.CSI"
  # "070005.OF",
)

# start_date <- "2006-04-07"
start_date <- "2018-01-01"
end_date <- "2018-07-24"

daily_price <- wsd_price_xts(codes, start_date, end_date, options = "period=D;PriceAdj=F")

# trade_price <- wsd_price_xts(codes, start_date, end_date, options = "period=Q;PriceAdj=F")


# rebalance dates ---------------------------------------------------------

# quarterly rebalance
rebal_dates <-
  qtr_end(start_date, end_date) %>%
  bizdays::adjust.previous() %>%
  keep(~ . >= start_date && .<= end_date)

# add last trading date
pricing_dates <-
  c(rebal_dates, last(index(daily_price))) %>% unique()

ep <-
  xts(1:nrow(daily_price), order.by = index(daily_price))[pricing_dates] %>%
  as.vector()

ep


# target weights ----------------------------------------------------------

tw <-
  c(0.5, 0.5) %>%
  rep(each = length(pricing_dates)) %>%
  matrix(ncol = length(codes)) %>%
  xts(order.by = pricing_dates) %>%
  setNames(codes)


# asset returns -----------------------------------------------------------

trade_price_on_rebal_dates <-
  trade_price[pricing_dates]

asset_ret <-
  trade_price_on_rebal_dates %>%
  Return.calculate()


# calc portfolio return ---------------------------------------------------

tw.lag <-
tw %>%
  lag.xts()

# merge(ret, tw.lag)

weighted_ret <-
  asset_ret * tw.lag

portfolio_ret <-
  weighted_ret %>% xts_rowapply(sum, "portfolio")

merge(
  asset_ret,
  portfolio_ret
) %>%
  # Return.cumulative()
  chart.CumReturns(wealth.index = TRUE, legend.loc = "topright")

# calc portfolio return using list columns ---------------------------------------------------

f_nest <- function(ts, name){
  name <- enquo(name)

  ts %>%
    xts_to_df() %>%
    nest(-DATETIME, .key = !!name)
}

f_port_ret <- function(w, r){
  if(is.na(w) || is.na(r))
    return(NA)

  tcrossprod(as.matrix(w), as.matrix(r))
}

f_merge_tbl <- function()

port_ret <-
  list(weight = tw, ret = asset_ret) %>%
  imap(f_nest) %>%
  reduce(full_join, by = .datetime_col) %>%
  mutate(port_ret = map2_dbl(lag(weight), ret, f_port_ret))

merge(
  asset_ret,
  df_to_xts(port_ret, "port_ret")
) %>%
  # Return.cumulative()
  chart.CumReturns(wealth.index = TRUE, legend.loc = "topright")

port_ret[[1, "weight"]]

df_t_bind_cols <- function(...){
  list(...) %>%
    keep(is.data.frame) %>%
    map(t) %>%
    bind_cols()
}

ans <-
port_ret[1:3, ] %>%
  mutate(data = pmap(., df_t_bind_cols))

(l <- ans[[1, "data"]])

