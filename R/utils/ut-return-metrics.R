# codes = c("XT102034.XT", "H00300.CSI")
# codes = c("001309.OF", "H00300.CSI")
codes = c("110011.OF", "H00300.CSI")

args <- tibble(
  codes = codes,
  start = wsd_start_date(codes),
  # start = "2017-05-01",
  end = "2018-06-27",
  options = "period=W;PriceAdj=F"
)

ret <-
  invoke(wsd_return_xts, args)

ans <-
  apply.yearly(ret[], nav_metrics_risk_return)

ans %>%
  xts_to_df() %>%
  # rio::export("clipboard")
  # write.csv(file = "./foo.csv", fileEncoding = "native.enc", row.names = FALSE)
  write.csv(file = "clipboard", fileEncoding = "native.enc", row.names = FALSE)

ret["2018/2018-06-01"] %>%
{
  map_df(
    list(
      累积收益 = Return.cumulative,
      年化收益 = Return.annualized,
      年化波动 = sd.annualized
    ), function(f) f(.) %>% as.data.frame,
    .id = "指标"
  )
}

# ret["2018/2018-06-01"] %>% index %>% range()

# cor(ret)

