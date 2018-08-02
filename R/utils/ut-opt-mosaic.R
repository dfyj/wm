library(RQuantLib)

f <- function(...){
  call <- EuropeanOption("call", ...)$value
  tibble(call = call)
}

crossing(
  underlying = 100,
  strike = underlying * 1.05,
  dividendYield = 0.,
  maturity = 5,
  volatility = 0.04,
  riskFreeRate = seq(0, by = 0.005, length.out = 6)
) %>%
  mutate(value = pmap(., f)) %>%
  unnest() %>%
  write.csv(file = "clipboard", fileEncoding = "native.enc", row.names = FALSE)

