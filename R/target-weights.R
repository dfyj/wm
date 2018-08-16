#' equal-weights portfolio
#'
#' @param price
#' @param reb_dates
#'
#' @return
#' @export
#'
#' @examples
#' tw_equal_weights(price, reb_dates)
tw_equal_weights <- function(price, reb_dates){
  price[reb_dates] * 0 + 1/ncol(price)
}

#' risk-parity portfolio
#'
#' @param vol
#' @param reb_dates
#'
#' @return
#' @export
#'
#' @examples
#' tw_risk_parity(vol, reb_dates)
tw_risk_parity <- function(vol, reb_dates){
  tw <- 1 / na.omit(vol[reb_dates])
  sweep(tw, 1, xts_rowapply(tw, sum), FUN = "/")
}
