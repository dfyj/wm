
# code <- "004477.OF"
code <- "001878.OF"

fields <- c("fund_benchmark", "fund_benchindexcode", "sec_name")
meta <- wsd_meta(code, fields)
bm_code <- meta[["fund_benchindexcode"]]

args <- tibble(
  codes = c(code, bm_code),
  start = wsd_start_date(codes),
  # start = "2017-05-01",
  end = "2018-07-09",
  options = "period=W;PriceAdj=F"
)

ret <-
  invoke(wsd_return_xts, args)

ret_bear <-
  ret[ret[, bm_code]<0, ]

ret %>%
  as_tibble() %>%
  mutate(!!bm_code < 0)

ret_split <- split(ret, ifelse(ret[, bm_code]>0, "bull", "bear"))

res <-
  ret_split %>%
  map_df(nav_metrics_risk_return, return_type = "tibble", .id = "市场")

res %>%
  write.csv(file = "clipboard", fileEncoding = "native.enc", row.names = FALSE)

res <- ret %>%
  nav_metrics_risk_return

res %>%
  rbind() %>%
  as_tibble() %>%
  write.csv(file = "clipboard", fileEncoding = "native.enc", row.names = FALSE)

ans <-
  apply.yearly(ret[], nav_metrics_risk_return)

ans %>%
  xts_to_df() %>%
  write.csv(file = "./foo.csv", fileEncoding = "native.enc", row.names = FALSE)

# ret["2018/2018-06-01"] %>%
# {
#   map_df(
#     list(
#       累积收益 = Return.cumulative,
#       年化收益 = Return.annualized,
#       年化波动 = sd.annualized
#     ), function(f) f(.) %>% as.data.frame,
#     .id = "指标"
#   )
# }

# ret["2018/2018-06-01"] %>% index %>% range()

# cor(ret)

