

#' Convert significance vector to symbols
#'
#' @param significance
#'
#' @return
#' @export
#'
#' @examples
#' cf <- lm(y ~ x, data = df) %>% summary.lm() %>% coef()
#' significance <- cf[, "Pr(>|t|)"]
#' significance_to_symbol(significance)
significance_to_symbol <- function(significance) {
  stats::symnum(
    significance,
    corr = FALSE,
    na = FALSE,
    cutpoints = .significance_config$cutpoints,
    symbols = .significance_config$symbols
  )
}


