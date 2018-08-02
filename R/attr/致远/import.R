library(readxl)


path <- "C:/OSC/wm/R/attr/致远产品/致远产品日净值表20180727（仅供内部参考）.xlsx"

bm <- "000905.SH"

products <- readxl::excel_sheets(path)
#products %>% filter(products)
# prod <- products[1]
# f_import_nav <- function(prod){
#   readxl::read_excel(path, sheet = prod) %>%
#     setNames(c("NAME","DATETIME", "unit_nav","cum_nav")) %>%
#     mutate(DATETIME = as.character(DATETIME))
# }
# if(nrow(filter(df,!is.na(x)))<= 0)

products <- readxl::excel_sheets(path)

read_sheets <- function(prod){
  df <- read_excel(path,sheet = prod,range = cell_cols(2:4),
                   col_types = c("guess", "numeric","numeric"))%>%
    setNames(c("DATETIME", "UNIT_NAV","CUM_NAV")) %>%
    mutate(DATETIME = as.character(DATETIME)) %>%
    mutate(DATETIME = as.character((lubridate::parse_date_time(DATETIME,"%y%m%d"))))
  if(nrow(filter(df,!is.na(CUM_NAV))) <= 0)
    df$CUM_NAV <- df$UNIT_NAV
  df <- df[,c(1,3)]
}

df_list <- products %>%
  map(read_sheets) %>%
  set_names(products)


df <- products %>%
  map_df(read_sheets)
# as.date(date_range$start)
# lubridate::parse_date_time("20180101", "%y%m%d") %>% as.character()

dt_range <- function(df){
  df %>%
    mutate(start = min(DATETIME), end = max(DATETIME))
}

date_range <- df_list %>%
  map_df(dt_range, .id = "product") %>%
  summarise(start = min(start), end = max(end))

Rb <- wsd_return_xts(bm,date_range$start,date_range$end)


# ret <- df_list[[prod]] %>%
#   df_to_xts()%>%
#   apply.weekly(last) %>%
#   PerformanceAnalytics::Return.calculate()%>%
#   metrics_weekly_ret() %>%
#   metrics_bm_based(.,Rb)

#analysize the portflios
f <-  function(df, Rb){
  ret <-
  df %>%
    df_to_xts() %>%
    apply.weekly(last) %>%
    PerformanceAnalytics::Return.calculate()
  start =   (df %>%
    summarise( 起始日 = min(DATETIME)))
  end = (df %>%
             summarise(净值截止日 = max(DATETIME)))
  bind_cols(
    start,
    end,
    metrics_weekly_ret(ret),
    metrics_bm_based(ret, Rb)
  )
  }


# f(df_list[["致远激进三号"],Rb)
# df <- df_list[["致远激进三号"]]
# %>%
#   df_to_xts() %>%
#   apply.weekly(last)


# result<- map(df_list, safely(f), Rb)
result<- map_df(df_list, f, Rb, .id = "产品")
# result %>%
  # transpose()

#export data
path  <-  "C:/OSC/wm/R/attr/致远产品"
write.csv(result,file = file.path(path,"致远组合分析.csv"),fileEncoding= "gbk",row.names = FALSE)
rds_write(result,file.path(path,"致远组合分析.rds"))
rds_write(df_list,file.path(path,"NAV.rds"))

