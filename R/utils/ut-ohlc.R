codes <- "000905.SH"
fields <- c("open", "high", "low", "close")
start <- "2018-01-01"
end <- "2018-06-20"
options = "period=W;PriceAdj=F"

ohlc <-
wsd_ohlc(codes, start, end)

ohlc %>%
  select(-code) %>%
  df_to_xts() %>%
  dygraph() %>%
  dyLegend(show = "onmouseover") %>%
  dyAxis("x", rangePad = 10) %>%
  dyCandlestick()
