
# fund metrics ------------------------------------------------------------

#' Fund metrics that only depend on weekly return series
#'
#' @param Ra weekly NAV, 1-column xts
#'
#' @return
#' @export
#'
#' @examples
metrics_weekly_ret <- function(ret){
  stopifnot(is.xts(ret), ncol(ret) == 1, xts_freq(ret) == "weekly")

  ret <- na.omit(ret)

  ret_df <- ret %>% setNames("Ra") %>% xts_to_df()

  ret_up <- ret[ret > 0]
  ret_dn <- ret[ret < 0]

  DD <- table.Drawdowns(ret, top = 1)

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
      年化上行波动率 =  sd.annualized(ret_up),
      年化下行波动率 =  sd.annualized(ret_dn),
      峰度 = kurtosis(ret),
      偏度 = skewness(ret),
      VaR0.95 = VaR(ret, p = 0.95),
      VaR0.99 = VaR(ret, p = 0.99),
      Sharpe = SharpeRatio(ret, Rf = 0, p = 0.95,FUN = "StdDev"),
      Sortino = SortinoRatio(ret),
      Calmar = CalmarRatio(ret)
    )
}

#' Fund metrics based on a benchmark
#'
#' @param Ra fund return
#' @param Rb benchmark return
#'
#' @return
#' @export
#'
#' @examples
#' metrics_bm_based(Ra, Rb)
metrics_bm_based <- function(Ra, Rb){
  stopifnot(is.xts(Ra), is.xts(Rb), ncol(Rb) == 1)

  # Align Ra and Rb
  merged <- merge.xts(Ra, Rb, join = "left" ) %>%
    na.omit()
  Ra <- merged[, -ncol(merged)]
  Rb <- merged[,  ncol(merged)]

  mt <- MarketTiming(Ra, Rb)

  tibble(
    Beta = CAPM.beta(Ra, Rb),
    IR = InformationRatio(Ra, Rb),
    相关系数 = cor(Ra, Rb, method = "pearson", use = "pairwise.complete.obs")[[1]],

    # 择时择股a（TM模型）
    beta1 = mt[[1, "Beta"]],
    beta2 = mt[[1, "Gamma"]],
    alpha = mt[[1, "Alpha"]]
  )
}
