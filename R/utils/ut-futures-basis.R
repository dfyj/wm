library(dygraphs)

main <- function(index_code, futures_code, start_date, end_date){
  price <- wsd_price_xts(
    c(index_code, futures_code),
    start_date,
    end_date,
    "period=D;PriceAdj=F"
    )

  f <- function(x) (x[[2]] / x[[1]] - 1) * 10000

  basis <- zoo::merge.zoo(price, xts_rowapply(price, f, "basis"))

  # basis %>%
  #   write.csv(file = "clipboard", fileEncoding = "native.enc", row.names = FALSE)

  basis %>%
    dygraph() %>%
    dySeries("basis", axis = "y2", strokeWidth = 2) %>%
    dyOptions(colors = c("blue", "green", "red")) %>%
    dyAxis("y2", valueRange = c(-300, 0)) %>%
    dyRangeSelector()
}

# main("000300.SH", "IF1808.CFE", "2018-06-01", "2018-07-24")

main("000905.SH", "IC1808.CFE", "2018-06-01", "2018-07-24")
