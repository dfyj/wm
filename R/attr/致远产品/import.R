library(readxl)


# import ------------------------------------------------------------------

nav_file_path <- "./attr/致远产品/致远产品日净值表20180727（仅供内部参考）.xlsx"

bm <- "000905.SH"

products <- readxl::excel_sheets(nav_file_path)

f_read_sheets <- function(prod){
  df <- read_excel(
    path,
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
  bind_rows(, .id = "product")

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

f_calc_port_metrics <-  function(df, Rb) {
  ret.weekly <-
    df %>%
    df_to_xts() %>%
    apply.weekly(last) %>%
    PerformanceAnalytics::Return.calculate()

  bind_cols(
    summarise(df,
              起始日 = min(DATETIME),
              净值截止日 = max(DATETIME)),
    metrics_weekly_ret(ret.weekly),
    metrics_bm_based(ret.weekly, Rb)
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
  rds_write(file.path(.dir,"致远组合分析.rds"))

nav_df %>%
  rds_write(
    file.path(.dir, "NAV.rds"))


# attribution -------------------------------------------------------------

f_get_weekly_ret <- function(df) {
  df %>%
    df_to_xts() %>%
    xts::apply.weekly(xts::last) %>%
    PerformanceAnalytics::Return.calculate
}

list_ret.week <-
  nav_df_list[
    #   c(
    #   "致远进取一号",
    #   "致远稳健一号"
    # )
    ] %>%
  map(f_get_weekly_ret)

list_ret.week %>%
  # map_df(attr_alpha, .id = "product")
  map_df(attr_alpha_beta, Rb, .id = "product")

