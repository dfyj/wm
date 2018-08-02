
path <- "C:/Users/Kun/Documents/R/projects/wm/R/attr/牟合/20180726-东方证券-牟合产品未扣费每日单位净值.xlsx"

bm <- "000905.SH"

f_import_nav <- function(prod){
  readxl::read_excel(path, sheet = prod) %>%
    setNames(c("DATETIME", "nav")) %>%
    mutate(DATETIME = as.character(DATETIME))
}

products <- readxl::excel_sheets(path)

nav_df_all <-
  products %>%
  setNames(products) %>%
  map_df(f_import_nav, .id = "product")

nav_xts_all <-
  nav_df_all %>%
  distinct() %>%
  spread("product", "nav") %>%
  df_to_xts()

nav_xts_all %>%
  setNames(1:ncol(nav_xts_all)) %>%
  dygraph() %>%
  dyCrosshair() %>%
  dyRangeSelector()

nav_xts_all %>%
  Return.calculate() %>%
  # SharpeRatio.annualized() %>%
  # DownsideDeviation() %>%
  PerformanceAnalytics::StdDev()



nav_df_all[4187:4188, ]

prod <- products[[1]]

nav_df <- f_import_nav(prod)

nav.weekly <-
  nav_df %>%
  df_to_xts() %>%
  apply.weekly(last)

ret.weekly <-
  nav.weekly %>%
  Return.calculate()

bm.ret.weekly <-
  wsd_return_xts(bm, start(nav.weekly), end(nav.weekly))

Ra <- ret.weekly
Rb <- bm.ret.weekly

cbind(
metrics_weekly_ret(ret.weekly),
metrics_bm_based(Ra, Rb)
) %>%
  write.csv(file = "foo.csv", fileEncoding = "native.enc", row.names = FALSE)

df <-
ret.weekly %>%
  setNames("Ra") %>%
  xts_to_df() %>%
  mutate(min_rank = min_rank(desc(Ra)))


f <- function(df){
  df %>%
    summarise(start = min(DATETIME), end = max(DATETIME))
}

date_range <-
df_list %>% map_df(f, .id = "product") %>%
  summarise(start = min(start), end = max(end))


Rb <- wsd_return_xts(bm, date_range$start, date_range$end)

date_range$start

df %>% summarise(start = min(DATETIME), end = max(DATETIME))

df <-
df %>%
  setNames(c("DATETIME", "nav")) %>%
  mutate(DATETIME = as.character(DATETIME))


