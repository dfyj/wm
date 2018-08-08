library(dplyr)
library(tidyr)
# import ------------------------------------------------------------------

nav_file_path <- "./attr/锐天/锐天净值更新20180720.xlsx"

bm <- "000905.SH"

nav_df <-
  read_excel(nav_file_path, col_types = c("text", rep("numeric", 13)))

name_vector <- colnames(nav_df)
name_vector <- name_vector[seq(1, ncol(nav_df) - 1, 2)]

nav_df <- nav_df[-1, ] %>%
  select(1, seq(2, ncol(nav_df), 2)) %>%
  setNames(c("DATETIME", name_vector)) %>%
  mutate(DATETIME = as.character(lubridate::parse_date_time(DATETIME, "%y%m%d")))

nav_df[nav_df <= 0] <- NA
nav_df_list <- nav_df %>%
  gather(product, "val", 2:ncol(nav_df))

nav_df_list <- nav_df_list%>%
  split(nav_df_list$product) %>%
  lapply(function(x) x[!(names(x) %in% c("product"))])
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
  df <- df %>%
    na.omit()
  ret.weekly <-
    df %>%
    f_get_weekly_ret

  bind_cols(
    summarise(df,
              起始日 = min(DATETIME),
              净值截止日 = max(DATETIME)),
    metrics_risk_ret(ret.weekly),
    metrics_risk_CAPM(ret.weekly, Rb),
    metrics_risk_TM(ret.weekly, Rb)
  )
}

# df <- nav_df_list[[1]]
# f_calc_port_metrics(df, Rb)

result <- map_df(nav_df_list, f_calc_port_metrics, Rb, .id = "产品")


# export data -------------------------------------------------------------

.dir <- dirname(nav_file_path)

result %>%
  write.csv(
    file.path(.dir, "锐天组合分析.csv"),
    fileEncoding = "gbk", row.names = FALSE)

result %>%
  rds_write(
    file.path(.dir, "锐天组合风险收益分析.rds"))

nav_df %>%
  rds_write(
    file.path(.dir, "NAV.rds"))


