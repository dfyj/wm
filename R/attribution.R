#' alpha/beta attribution
#'
#' @param ret 2-column xts{asset return, benchmark return}
#'
#' @return
#' @export
#'
#' @examples
#' a_b <- attr_alpha_beta(Ra, Rb)
attr_alpha_beta <- function(Ra, Rb){
  ret <- merge.xts(Ra, Rb, join = "left")

  model.lm <-
    ret %>%
    coredata() %>%
    as.data.frame() %>%
    setNames(c("y", "x")) %>%
    {lm(y ~ x, data = .)}

  metrics <-
    model.lm %>%
    summary.lm() %>%
    coef() %>%
    as.data.frame() %>%
    setNames(c("mean", "std", "t", "sig"))

  # annualization factor for alpha
  annual_fac <- xts_annual_factor(ret)

  metrics %>%
    tibble::add_column(component = c("alpha", "beta"), .before = 1) %>%
    tibble::add_column(annual_factor = c(annual_fac, 1), .before = 2) %>%
    mutate(
      annual = mean * annual_factor,  # simple average
      # annual = exp(log(1+mean) * annual_factor) - 1,  # geomeric average
      sig_sym = significance_to_symbol(sig)
    ) %>%
    select(-annual_factor)
}

#' estimate magnitude and significance of alpha
#'
#' @param ret 1-column xts{asset return}
#' @param Rf annualized risk free rate that alpha is tested against, default = 0
#'
#' @return
#' @export
#'
#' @examples
#' alpah_summary <- attr_alpha(ret)
attr_alpha <- function(ret, Rf = 0){
  # unannualized risk free rate
  Rf_ <- Rf / xts_annual_factor(ret)

  model.lm <-
    ret %>%
    coredata() %>%
    as.data.frame() %>%
    setNames(c("y")) %>%
    {lm(y - Rf_ ~ 1, data = .)}

  metrics <-
    model.lm %>%
    summary.lm() %>%
    coef() %>%
    as.data.frame() %>%
    setNames(c("mean", "std", "t", "sig"))

  # annualization factor for alpha
  annual_fac <- xts_annual_factor(ret)

  metrics %>%
    tibble::add_column(component = c("alpha"), .before = 1) %>%
    tibble::add_column(annual_factor = c(annual_fac), .before = 2) %>%
    mutate(
      annual = mean * annual_factor,  # simple average
      # annual = exp(log(1+mean) * annual_factor) - 1,  # geomeric average
      sig_sym = significance_to_symbol(sig)
    ) %>%
    select(-annual_factor)
}
