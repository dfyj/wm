
# 测试产品的alpha/beta及其显著性 ----------------------------------------------------

main <- function(nav_xts, bm) {
  freq <- periodicity(nav_xts)$scale

  nav_aligned <- nav_xts[] %>% xts_align_regularly(freq)

  bm_ret <- md_asset_return(bm, start(nav_xts), end(nav_xts), freq)

  funds <- nav_aligned %>% names()

  f_calc_alpha_beta <- function(fund) {
    nav <- nav_aligned[, fund] %>% na.omit()
    fund_ret <- nav %>% Return.calculate()
    merge_ret <- merge(fund_ret, bm_ret)
    merge_ret %>% attr_alpha_beta() %>% as_tibble()
  }

  f_calc_ret_metrics <- function(fund) {
    nav <- nav_aligned[, fund] %>% na.omit()
    fund_ret <- nav %>% Return.calculate()
    tibble(annual_ret = Return.annualized(fund_ret)[[1, 1]])
  }

  # fund <- "华信赢鑫14号"
  # f_calc_alpha_beta(fund)

  annual_ret <-
    tibble(fund = funds) %>%
    mutate(attr = map(fund, f_calc_ret_metrics)) %>%
    unnest()

  attr_summary <-
    tibble(fund = funds) %>%
    mutate(attr = map(fund, f_calc_alpha_beta))

  # attr_summary[[3, "attr"]]

  attr_summary_unnest <- attr_summary %>% unnest()

  attr_summary_unnest %>%
    write.csv(file = "clipboard",
              fileEncoding = "native.enc",
              row.names = FALSE)

  # f_extract <- function(attr, i, measure) {
  #   attr[[i, measure]]
  # }

  # attr_summary %>%
  #   mutate(alpha = map_dbl(attr, f_extract, 1, "annual"))

  # merge_ret %>%
  #   setNames(c("fund", "bm")) %>%
  #   coredata() %>%
  #   as_tibble() %>%
  #   ggplot() +
  #   geom_point(aes(bm, fund))
  #
  # nav_aligned[, "牟合资产方盖01号"] %>% na.omit()
}

.proj_dir <- "./attr/牟合"

file.rds <- "C:/db/nav/牟合/20180723-东方证券-牟合产品每周单位净值.rds"

nav <- rds_read(file.rds)

f_nav_to_ret <- function(df){
  df %>%
    df_to_xts("nav") %>%
    Return.calculate() %>%
    xts_to_df()
}

nav %>%
  group_by(code) %>%
  # summarise(first(DATETIME, order_by = DATETIME))
  top_n(3, DATETIME) %>%
  arrange(code, DATETIME)

nav_xts <- nav %>%
  spread(code, nav) %>%
  df_to_xts()

# nav_xts %>% headtail()

bm <- "H00300.CSI"

main(nav_xts, bm)
