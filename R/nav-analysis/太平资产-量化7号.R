nav <- readr::read_rds("~/R/projects/wm/R/nav-analysis/太平资产/extdata/太平资产-量化7号.rds")

nav_bm_xts <- tibble(
    code = "H00300.CSI", # product and benchmark
    start = first(nav$date),
    end = last(nav$date),
    options = "period=D;PriceAdj=F"
  ) %>%
{invoke(wsd_price_xts, .)}

nav_xts <- merge.zoo(
  df_to_xts(nav), nav_bm, join = "left") %>%
  setNames(c("product", "benchmark"))

ret_xts <- nav_xts %>% Return.calculate() %>% na.omit()

ret_xts["2018-03/2018-05"] %>%
  apply.monthly(nav_metrics_alpha_beta) %>%
  xts_to_df() %>%
  {dplyr::filter_if(., is.numeric, dplyr::any_vars(!is.na(.)))}

.Last.value %>%
  write.csv(file = "clipboard", fileEncoding = "native.enc", row.names = FALSE)

