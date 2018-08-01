

# Portfolio related utilities ---------------------------------------------

#' Portfolio from Wind
#'
#' @param code product Wind code
#' @param dates
#'
#' @return data frame of [date, code, name, weight]
#' @export
#'
#' @examples
#' prod_code <- "000480.OF"
#' dates <- qtr_end("2017-01-01", "2017-12-31")
#' prt <- prt_from_wind(code, dates)
prt_from_wind <- function(prod_code, dates) {
  purrr::map_df(
    dates %>% setNames(dates),
    ~ wind_fund_holdings(., prod_code) %>%
      select(
        code = stock_code,
        name = stock_name,
        weight = proportiontonetvalue
      ),
    .id = "date"
  ) %>%
    dplyr::mutate(weight = weight / 100) %>%
    tibble::as_tibble()
}
