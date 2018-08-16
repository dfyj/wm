
#' List of columns in percentage format
#'
#' @return
#' @export
#'
#' @examples
#' rpt_percentage_cols()
rpt_percentage_cols <- function() {
  c(
    # "产品",
    # "类型",
    # "起始日",
    # "净值截止日",
    # "交易周总数",
    # "盈利交易周数",
    # "亏损交易周数",
    "胜率",
    "盈利周单周平均盈利",
    "亏损周单周平均亏损",
    "单周盈亏比",
    "单周最大涨幅",
    # "单周最大上涨日期",
    "区间最大回撤",
    # "区间最大回撤日期",
    # "最大回撤周期",
    "累计收益率",
    "年化复合收益率",
    "年化波动率",
    "年化上行波动率",
    "年化下行波动率",
    # "峰度",
    # "偏度",
    "VaR0.95",
    "VaR0.99",
    # "Sharpe",
    # "Sortino",
    # "Calmar",
    "年化绝对收益",
    # "绝对收益显著性",
    "年化超额收益",
    # "超额收益显著性",
    # "beta",
    # "beta显著性",
    # "IR",
    "相关系数",
    # "TM.beta1",
    # "TM.beta2",
    "TM.alpha"
  )
}

#' List of columns to show in fund metrics table
#'
#' @return
#' @export
#'
#' @examples
#' rpt_metrics_cols()
rpt_metrics_cols <- function() {
  c(
    "类型",
    "产品",
    "起始日",
    "净值截止日",
    # "交易周总数",
    # "盈利交易周数",
    # "亏损交易周数",
    "胜率",
    # "盈利周单周平均盈利",
    # "亏损周单周平均亏损",
    # "单周盈亏比",
    # "单周最大涨幅",
    # "单周最大上涨日期",
    "区间最大回撤",
    "区间最大回撤日期",
    "最大回撤周期",
    "累计收益率",
    "年化复合收益率",
    "年化波动率",
    # "年化上行波动率",
    # "年化下行波动率",
    # "峰度",
    # "偏度",
    # "VaR0.95",
    # "VaR0.99",
    "Sharpe",
    "Sortino",
    "Calmar",
    "年化绝对收益",
    "绝对收益显著性",
    "年化超额收益",
    "超额收益显著性",
    "beta",
    # "beta显著性",
    "IR"
    # "相关系数",
    # "TM.beta1",
    # "TM.beta2",
    # "TM.alpha"
  )
}
