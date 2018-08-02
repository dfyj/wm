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

f_calc_port_metrics <-  function(df, Rb, Rf = 0) {
  ret.weekly <-
    df %>%
    f_get_weekly_ret

  bind_cols(
    summarise(df,
              起始日 = min(DATETIME),
              净值截止日 = max(DATETIME)),
    metrics_risk_ret(ret.weekly),
    metrics_risk_CAPM(ret.weekly, Rb),
    metrics_risk_TM(ret.weekly, Rb),
    metrics_alpha_only(ret.weekly, Rf)
  )
}

# df <- nav_df_list[[1]]
# f_calc_port_metrics(df, Rb)

result <- map_df(nav_df_list, f_calc_port_metrics, Rb, .id = "产品")


# export data -------------------------------------------------------------

.dir <- dirname(nav_file_path)

result %>%
  write.csv(
    file.path(.dir, "致远组合分析.csv"),
    fileEncoding = "gbk", row.names = FALSE)

result %>%
  rds_write(
    file.path(.dir, "致远组合风险收益分析.rds"))

nav_df %>%
  rds_write(
    file.path(.dir, "NAV.rds"))


