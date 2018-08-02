library(readxl)


# import ------------------------------------------------------------------

nav_file_path <- "./attr/致远产品/致远产品日净值表20180727（仅供内部参考）.xlsx"

bm <- "000905.SH"

products <- readxl::excel_sheets(nav_file_path)

f_read_sheets <- function(prod){
  df <- read_excel(
    nav_file_path,
    sheet = prod,
    range = cell_cols(2:4),
    col_types = c("text", "numeric", "numeric")) %>%
    setNames(c("DATETIME", "UNIT_NAV", "CUM_NAV")) %>%
    mutate(DATETIME = as.character(lubridate::parse_date_time(DATETIME, "%y%m%d")))

  if(nrow(filter(df, !is.na(CUM_NAV))) <= 0)
    df$CUM_NAV <- df$UNIT_NAV
  df <- df[,c(1,3)]
}

nav_df_list <- products %>%
  map(f_read_sheets) %>%
  set_names(products)

nav_df <-
  nav_df_list %>%
  bind_rows( , .id = "product")

# diagnostics

# df %>%
#   group_by(product) %>%
#   summarise(
#     min(DATETIME),
#     max(DATETIME),
#     min(CUM_NAV),
#     max(CUM_NAV),
#     n()
#   ) %>%
#   View()

Rb <- wsd_return_xts(bm, min(nav_df$DATETIME), max(nav_df$DATETIME))

# calc portflio metrics ---------------------------------------------------
f_get_weekly_ret <- function(df) {
  df %>%
    df_to_xts() %>%
    xts::apply.weekly(xts::last) %>%
    Return.calculate
}

f_calc_port_metrics <-  function(df, Rb) {
  ret.weekly <-
    df %>%
    f_get_weekly_ret

  bind_cols(
    summarise(df,
              起始日 = min(DATETIME),
              净值截止日 = max(DATETIME)),
    metrics_risk_ret(ret.weekly),
    metrics_risk_CAMP(ret.weekly, Rb),
    metrics_risk_TM(ret.weekly, Rb)
  )
}

# df <- nav_df_list[[1]]
# f_calc_port_metrics(df, Rb)

risk_ret_result <- map_df(nav_df_list, f_calc_port_metrics, Rb, .id = "产品")


# attribution based on alpha and beta------------------------------------------------------

f_calc_alpha_beta <-  function(df, Rb) {
  ret.weekly <-
    df %>%
    f_get_weekly_ret

  alpha_beta_sig = attr_alpha_beta(ret.weekly,Rb)

  bind_cols(
    summarise(df,
              起始日 = min(DATETIME),
              净值截止日 = max(DATETIME)),
    alpha = alpha_beta_sig[1, "mean"],
    alpha显著性 = alpha_beta_sig[1, "sig"],
    beta = alpha_beta_sig[2, "mean"],
    beta显著性 = alpha_beta_sig[2, "sig"]
  )
}

alpha_beta_result <- map_df(nav_df_list, f_calc_alpha_beta, Rb, .id = "产品")


# attribution based on alpha_only -----------------------------------------

f_calc_alpha_only <-  function(df, Rf = 0) {
  ret.weekly <-
    df %>%
    f_get_weekly_ret

  alpha_sig = attr_alpha(ret.weekly,Rf)

  bind_cols(
    summarise(df,
              起始日 = min(DATETIME),
              净值截止日 = max(DATETIME)),
    alpha = alpha_sig["mean"],
    alpha显著性 = alpha_sig["sig"]
  )
}

alpha_only_result <- map_df(nav_df_list, f_calc_alpha_only, Rf = 0, .id = "产品")



# export data -------------------------------------------------------------

.dir <- dirname(nav_file_path)

risk_ret_result %>%
  write.csv(
    file.path(.dir, "致远组合风险收益分析.csv"),
    fileEncoding = "gbk", row.names = FALSE)

alpha_beta_result %>%
  write.csv(
    file.path(.dir, "致远组合alpha_beta分析.csv"),
    fileEncoding = "gbk", row.names = FALSE)

alpha_only_result %>%
  write.csv(
    file.path(.dir, "致远组合alpha_only分析.csv"),
    fileEncoding = "gbk", row.names = FALSE)

risk_ret_result %>%
  rds_write(
    file.path(.dir, "致远组合风险收益分析.rds"))

alpha_beta_result %>%
  rds_write(
    file.path(.dir, "致远组合alpha_beta分析.rds"))

alpha_only_result %>%
  rds_write(
    file.path(.dir, "致远组合alpha_only分析.rds"))


nav_df %>%
  rds_write(
    file.path(.dir, "NAV.rds"))


