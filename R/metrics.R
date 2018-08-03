
# fund metrics ------------------------------------------------------------

#' Fund metrics that only depend on weekly return series
#'
#' @param Ra weekly NAV, 1-column xts
#'
#' @return
#' @export
#'
#' @examples

metrics_risk_ret <- function(ret, Rf = 0){
  stopifnot(is.xts(ret), ncol(ret) == 1, xts_freq(ret) == "weekly")

  ret <- na.omit(ret)

  ret_df <- ret %>% setNames("Ra") %>% xts_to_df()

  ret_up_dn <- ret %>% split(ifelse(ret >= 0, "up", "dn"))

  DD <- table.Drawdowns(ret, top = 1)

  alpha_sig <- attr_alpha(ret, Rf)

  ret_df %>%
    summarise(
      交易周总数 = n(),
      盈利交易周数 = sum(Ra>0),
      亏损交易周数 = sum(Ra<0),
      胜率 = 盈利交易周数 / 交易周总数,
      盈利周单周平均盈利 = sum(Ra * (Ra>0)) / 盈利交易周数,
      亏损周单周平均亏损 = sum(Ra * (Ra<0)) / 亏损交易周数,
      单周盈亏比 = 盈利周单周平均盈利 / 亏损周单周平均亏损 * (-1),
      单周最大涨幅 = max(Ra),
      单周最大上涨日期 = as.character(DATETIME[min_rank(desc(Ra)) == 1]),
      区间最大回撤 = DD$Depth,
      区间最大回撤日期 = as.character(DD$Trough),
      最大回撤周期 = DD$`To Trough`,
      累计收益率 = Return.cumulative(ret)[[1, 1]],
      年化复合收益率 = Return.annualized(ret)[[1, 1]],
      年化波动率 =  sd.annualized(ret),
      年化上行波动率 =  ifelse(length(ret_up_dn[["up"]]) <= 1, NA, sd.annualized(ret_up_dn[["up"]])),
      年化下行波动率 =  ifelse(length(ret_up_dn[["dn"]]) <= 1, NA, sd.annualized(ret_up_dn[["dn"]])),
      峰度 = kurtosis(ret),
      偏度 = skewness(ret),
      VaR0.95 = VaR(ret, p = 0.95),
      VaR0.99 = VaR(ret, p = 0.99),
      Sharpe = SharpeRatio(ret, Rf = 0, p = 0.95, FUN = "StdDev", annualize = TRUE),
      Sortino = annualize_vol(SortinoRatio(ret), 'weekly'),
      Calmar = CalmarRatio(ret),
      年化绝对收益 = annualize_ret(alpha_sig[["mean"]],'weekly'),
      绝对收益显著性 = alpha_sig[["sig_sym"]]
    )
}

#' Fund metrics based on CAPM model
#'
#' @param Ra fund return
#' @param Rb benchmark return
#'
#' @return
#' @export
#'
#' @examples
#' metrics_risk_CAMP(Ra, Rb)
metrics_risk_CAPM <- function(Ra, Rb){
  stopifnot(is.xts(Ra), is.xts(Rb), ncol(Rb) == 1)

  # Align Ra and Rb
  merged <- merge.xts(Ra, Rb, join = "left" ) %>%
    na.omit()
  Ra <- merged[, -ncol(merged)]
  Rb <- merged[,  ncol(merged)]

  alpha_beta_sig = attr_alpha_beta(Ra,Rb)

  tibble(
    年化超额收益 = annualize_ret(alpha_beta_sig[[1, "mean"]],'weekly'),
    超额收益显著性 = alpha_beta_sig[[1, "sig_sym"]],
    beta = alpha_beta_sig[[2, "mean"]],
    beta显著性 = alpha_beta_sig[[2, "sig_sym"]],
    IR = InformationRatio(Ra, Rb),
    相关系数 = cor(Ra, Rb, method = "pearson", use = "pairwise.complete.obs")[[1]]
  )
}



#' Fund metrics based on TM model
#'
#' @param Ra fund return
#' @param Rb benchmark return
#'
#' @return
#' @export
#'
#' @examples
#' metrics_risk_TM(Ra, Rb)
metrics_risk_TM <- function(Ra, Rb){
  stopifnot(is.xts(Ra), is.xts(Rb), ncol(Rb) == 1)

  # Align Ra and Rb
  merged <- merge.xts(Ra, Rb, join = "left" ) %>%
    na.omit()
  Ra <- merged[, -ncol(merged)]
  Rb <- merged[,  ncol(merged)]

  mt <- MarketTiming(Ra, Rb)

  tibble(

    TM.beta1 = mt[[1, "Beta"]],
    TM.beta2 = mt[[1, "Gamma"]],
    TM.alpha = mt[[1, "Alpha"]]
  )
}

#' annualize return
#'
#' @param ret
#' @param freq frequency of retturn
#'
#' @return
#' @export
#'
#' @examples
annualize_ret <- function(ret, freq){
  annualized_ret <- ret * .frequency_to_annual_factor[[freq]]
}

#' annualize volatility
#'
#' @param vol
#' @param freq frequency of volatility
#'
#' @return
#' @export
#'
#' @examples
annualize_vol <- function(vol, freq){
  annualized_vol <- vol * sqrt(.frequency_to_annual_factor[[freq]])
}
