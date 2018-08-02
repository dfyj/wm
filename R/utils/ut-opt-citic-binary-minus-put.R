library(RQuantLib)

S0 = 100
K = 80
divYld = 0.04
Rf = 0.05
maturity = 0.5
vol = 0.3

BinaryOption("cash", "call", "european", S0, K, divYld, Rf, maturity, vol, 1)$value

f <- function(K, maturity, vol){
  binOpt <- BinaryOption("cash", "call", "european", S0, K, divYld, Rf, maturity, vol, 1)$value
  put <- EuropeanOption("put", S0, K, divYld, Rf, maturity, vol)$value
  total <- (6.25 + S0 - K) * binOpt - (put +S0 - K)
  tibble(bin = binOpt, put = put, total = total)
}

f(K, maturity, vol)

crossing(K = S0-20, maturity = 0.5, vol = 2:4/10) %>%
  mutate(value = pmap(., f)) %>%
  unnest()

