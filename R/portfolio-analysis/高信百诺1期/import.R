is_not_na <- function(x) {
  !all(is.na(x))
}

#' Import holdings data
#'
#' @param data_file data file path
import_holdings <- function(data_file) {
  raw_data <- data_file %>%
    readxl::read_excel()

  dates <-
    colnames(raw_data) %>%
    as.numeric() %>% as.Date(, origin = "1899-12-30")

  tb <- tibble(
    date = dates[!is.na(dates)] %>% as.character(),
    start_col = seq_along(dates)[!is.na(dates)]
  )

  .f_get_holding <- function(date, start_col) {
    raw_data[2:nrow(raw_data), start_col:(start_col+1)] %>%
      setNames(c("name", "weight")) %>%
      mutate_at(2, as.numeric) %>%
      mutate(date = date) %>%
      select(date, everything())
  }

  .f_get_holding(tb[1, ]$date, tb[1,]$start_col)

  pmap_dfr(tb, .f_get_holding) %>%
    na.omit() %>%
    filter(!(name == "总仓位"))
}

#' Clean up imported holdings data
#'
#' @param weights imported holdings data
#'
clean_holdings <- function(weights) {
  weights %>%
    group_by(date) %>%
    summarise(weight = sum(weight))

  sec_mapping <-
    mem_wset("sectorconstituent", "a001010100000000")$Data %>%
    as_tibble() %>%
    select(code = wind_code, name = sec_name)

  sec_mapping_extra <- tribble(~ code, ~ name,
                               "511880.SH", "银华日利")

  sec_mapping <- bind_rows(sec_mapping_extra, sec_mapping)

  weights <- weights %>%
    mutate(name = str_replace_all(name, "Ａ", "A")) %>%
    mutate(name = str_replace_all(name, " ", ""))

  left_join(weights, sec_mapping, by = "name") %>%
    select(date, code, name, weight)
}

data_file <-
"~/R/projects/wm/R/portfolio-analysis/高信百诺1期/高信百诺1期2017年季末持仓.xls"

raw_data <- import_holdings(data_file)

# raw_data %>% group_by(date) %>% summarise(sum(weight))

holding <- clean_holdings(raw_data)

stock_holding <- holding %>%
  mutate(
    sec_type = wsd_xc(code, "sec_type")
  ) %>%
  filter(sec_type == "普通股") %>%
  select(-sec_type)

rds_write(holding, "~/R/projects/wm/R/portfolio-analysis/高信百诺1期/prt.rds")
rds_write(stock_holding, "~/R/projects/wm/R/portfolio-analysis/高信百诺1期/stock_prt.rds")

