(.root_db <- "~/R/src/qe/R/nav-analysis/太平资产/")

(raw_data_file <- "~/R/projects/wm/R/nav-analysis/太平资产/extdata/太平资产-量化7号.xlsx")

raw_data <-
raw_data_file %>%
  rio::import()

raw_data <- rio::import(raw_data_file)

nav <- raw_data %>%
  select(date = 净值日期, nav = 累计净值) %>%
  mutate(date = as.character(lubridate::ymd(date)))

rds_data_file <-
  raw_data_file %>% str_replace("\\.[a-zA-Z]+$", ".rds")

readr::write_rds(nav, rds_data_file)

