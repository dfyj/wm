
# Import ------------------------------------------------------------------

codes <- c(
  "000300.SH"
  # "H00300.CSI"
  # "070005.OF",
  # "CBA00101.CS"
)
start_date <- "2006-04-07"
end_date <- "2018-07-24"

# price <- wsd_price_xts(codes, start_date, end_date)
# ret <- price %>% Return.calculate()
# annual_factor <- xts_annual_factor(ret)

ohlcv <- wsd_ohlc(codes, start_date, end_date)

rds.path <- glue::glue("./aa/{codes[[1]]}@ohlcv.rds")

rds_write(ohlcv, rds.path)

# Plot ----------------------------------------------------------------

ohlcv <- rds_read(ohlcv, rds.path)

ohlcv %>%
  dygraph_ohlcv()


# Backtest ----------------------------------------------------------------



windows <- c(13, 26, 52)
cols <- str_c("", windows, "W")

f <- function(window){
  runMean(ret, window) / runSD(ret, window) * sqrt(annual_factor)
}

stats_ts <-
  windows %>%
  map(f) %>%
  reduce(merge.xts) %>%
  setNames(cols)

dygraph_ts <-
  merge(stats_ts, price) %>%
  setNames(c(cols, "price"))

dygraph_ts %>%
  dygraph() %>%
  dySeries("price", axis = "y2", strokeWidth = 2) %>%
  dyOptions(colors = c("blue", "red", "green", "black")) %>%
  dyRangeSelector()


f <- function(x){
  x %>%
    xts_to_df() %>%
    summarise_if(is.numeric, mean, na.rm = TRUE) %>%
    setNames(letters[1:3]) %>%
    mutate(`a/b` = a/b, `a/c` = a/c) %>%
    unlist()
}

vol.13w["2016"] %>%
  f

vol.13w[] %>%
  apply.yearly(f)

(vol.13w[, 1] / vol.13w[, 3]) %>%
  mean(na.rm = TRUE)

# vol.13w <-
#   ret[, 1] %>%
#   runSD(n = 13) %>%
#   `*`(sqrt(52))
#
# vol.13w_2 <-
#   ret[, 1] %>%
#   rollapplyr(13, sd) %>%
#   `*`(sqrt(52))

# merge(vol.merge.13w, vol.13w_2) %>%
#   headtail()
#
# microbenchmark::microbenchmark(
#   times = 3L,
#   ret[, 1] %>% runSD(n = 13)
# )
#
# microbenchmark::microbenchmark(
#   times = 3L,
#   ret[, ] %>% rollapplyr(13, sd)
# )
#
# microbenchmark::microbenchmark(
#   times = 3L,
#   ret[, 1] %>% rollapplyr(13, sd)
# )
#
# ret[1:50, ] %>% rollapplyr(13, sd)
