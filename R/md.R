
#' Asset return
#'
#' @param code
#' @param start
#' @param end
#' @param freq
#'
#' @return
#' @export
#'
#' @examples
#' md_asset_return(code, start, end, freq = "weekly")
#' md_asset_return("000300.SH", "2018-07-01", "2018-07-24", freq = "weekly")*100
md_asset_return <- function(code, start, end, freq = .frequency_list){
  freq <- match.arg(freq)
  wind_option <- sprintf("period=%s;PriceAdj=F", str_sub(freq, end = 1L))
  wsd_return_xts(code, start, end, wind_option)
}
