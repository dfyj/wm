#'Plot NAV relative to benchmark
#'
#' @param products
#' @param bm
#' @param as_of
#'
#' @return
#' @export
#'
#' @examples
#' as_of <- "2018-06-15"
#' products <- c("XT102034.XT",
#'               "000480.OF")
#' bm <- "H00300.CSI"
#' ut_plot_relative_performance(products, bm, as_of)
ut_plot_relative_performance <- function(products, bm, as_of){
  codes <- c(products, bm)

  # price df
  price <-
    wsd_price_full_history(codes, as_of)

  # price xts
  price_xts <-
    price %>%
    {
      dplyr::filter_if(., is.numeric, dplyr::any_vars(!is.nan(.)))
    }  %>%
    spread(code, value) %>%
    df_to_xts()

  # NAV relative to benchmark
  rel_nav_xts <-
    map(products, ~ nav_rel_nav(price_xts[, .], price_xts[, bm])) %>%
    reduce(merge.zoo)

  # relative NAV "hinged" on first(longest) series
  for(i in 2:ncol(rel_nav_xts))
    rel_nav_xts[, i] <- nav_hinge_short_series_to_long(rel_nav_xts[, 1], rel_nav_xts[, i])

  # plot
  rel_nav_xts %>%
    # setNames(wsd_xc(colnames(.), "sec_name")) %>%
    dygraph() %>%
    dyRangeSelector()
}
