# NAV related utilities ---------------------------------------------------

#' Relative NAV
#' defined as price_a/price_b, starting from 1
#'
#' @param price_a product nav, xts
#' @param price_b benchmark nav, xts
#'
#' @return
#' @export
#'
#' @examples
nav_rel_nav <- function(price_a, price_b){
  log_diff <- na.omit(log(price_a) - log(price_b))
  coredata(log_diff) <- apply(coredata(log_diff), 1,
                              function(x) x - coredata(xts::first(log_diff)))
  exp(log_diff)
}

#' "Hinge" short-series NAV to long-series so they have same value
#' on day-one of short-series
#'
#' @param nav_l long series NAV
#' @param nav_s short series NAV
#'
#' @return
#' @export
#'
#' @examples
nav_hinge_short_series_to_long <- function(nav_l, nav_s){
  # start date of short series
  start_s <- nav_s %>% na.omit() %>% xts::first() %>% index
  nav_s * nav_l[start_s, 1][[1]]
}

#' Calc alpha/beta decomposition metrics
#'
#' @param x
#'
#' @return named numeric vector of metrics
#' @export
#'
#' @examples
#' | Beta | 相关系数 | 产品收益 | 指数收益 | Beta收益 | Alpha收益 |
#' | ---- | -------- | -------- | -------- | -------- | --------- |
#' | 0.66 | 64%      | 201%     | 37%      | 24%      | 177%      |
#'
nav_metrics_alpha_beta <- function(ret) {
  col_names <- c("Beta", "相关系数", "产品收益", "指数收益", "Beta收益", "Alpha收益", "跟踪误差")

  if(nrow(ret) < 15)
    return(purrr::rep_along(col_names, NA) %>% setNames(col_names))

  Ra <- ret[, 1]
  Rb <- ret[, 2]

  SFM_table <- PerformanceAnalytics::table.SFM(Ra, Rb)
  cum_ret <- PerformanceAnalytics::Return.cumulative(ret)

  tibble(
    beta = SFM_table["Beta", ],
    correlation = SFM_table["Correlation", ],
    ret = cum_ret[[1]],
    bm_ret = cum_ret[[2]]
  ) %>%
    dplyr::mutate(
      beta_ret = beta * bm_ret,
      alpha_ret = ret - beta_ret,
      TE = sd(Ra - Rb * beta)
    ) %>%
    unlist() %>%
    setNames(col_names)
}

#' Calc risk/return metrics
#'
#' @param ret 2-column xts of product an benchmark return
#'
#' @return named numeric vector or a one-row tibble of calculated metrics
#' @export
#'
#' @examples
#'
#' | 年化收益 | 基准年化收益 | 夏普率 | 基准夏普率 | 年化波动 | 基准年化波动 | Beta | 熊市Beta | 牛市Beta | 主动收益 | 跟踪误差 | 信息比率 | 年化Alpha | 相对基准波动 | 样本数数 |
#' | -------- | ------------ | ------ | ---------- | -------- | ------------ | ---- | -------- | -------- | -------- | -------- | -------- | --------- | ------------ | ---------|
#' | 16.1%    | 4.3%         | 0.72   | 0.20       | 22.4%    | 21.7%        | 0.66 | 0.80     | 0.46     | 11.8%    | 18.7%    | 0.63     | 13.2%     | 1.03         | 30       |
#'
nav_metrics_risk_return <- function(ret, return_type = c("vector", "tibble")){
  return_type <- match.arg(return_type)

  Ra <- ret[, 1]
  Rb <- ret[, 2]
  beta <- PerformanceAnalytics::CAPM.beta(Ra, Rb)
  alpha <- Ra - Rb * beta

  vec <- list(
    年化收益 = PerformanceAnalytics::Return.annualized(Ra),
    基准年化收益 = PerformanceAnalytics::Return.annualized(Rb),
    夏普率 = PerformanceAnalytics::SharpeRatio(Ra, FUN = "StdDev", annualize = TRUE),
    基准夏普率 = PerformanceAnalytics::SharpeRatio(Rb, FUN = "StdDev", annualize = TRUE),
    年化波动 = PerformanceAnalytics::sd.annualized(Ra),
    基准年化波动 = PerformanceAnalytics::sd.annualized(Rb),
    Beta = beta,
    熊市Beta = PerformanceAnalytics::CAPM.beta.bear(Ra, Rb),
    牛市Beta = PerformanceAnalytics::CAPM.beta.bull(Ra, Rb),
    主动收益 = PerformanceAnalytics::ActivePremium(Ra, Rb),
    跟踪误差 = PerformanceAnalytics::TrackingError(Ra, Rb),
    信息比率 = PerformanceAnalytics::InformationRatio(Ra, Rb)
  ) %>%
    lapply(as.vector) %>%
    as.data.frame() %>%
    mutate(
      年化Alpha = PerformanceAnalytics::Return.annualized(alpha),
      # 年化Alpha = 年化收益 - 基准年化收益 * Beta,
      年化Alpha波动 = PerformanceAnalytics::sd.annualized(alpha),
      年化Alpha夏普率 = 年化Alpha / 年化Alpha波动,
      相对基准波动 = 年化波动 / 基准年化波动,
      样本数 = length(Ra)
    ) %>%
    unlist()

  if(return_type == "vector")
    vec
  else
    vec %>% rbind %>% as_tibble
}

