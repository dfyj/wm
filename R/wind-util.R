#' ---
#' title: Wind API related utilities
#' date: 2018-05-03
#' output:
#'    html_document:
#'      toc: true
#'      highlight: zenburn
#' ---

#' memoised wsd API
#' @export
mem_wsd <- memoise::memoise(WindR::w.wsd, cache = memoise::cache_filesystem("~/R/.rcache"))

#' memoised wsd API
#' @export
mem_edb <- memoise::memoise(WindR::w.edb, cache = memoise::cache_filesystem("~/R/.rcache"))

#' memoised wset API
#' @export
mem_wset <- memoise::memoise(WindR::w.wset, cache = memoise::cache_filesystem("~/R/.rcache"))

#' memoised wset API
#' @export
mem_wtdays <- memoise::memoise(WindR::w.tdays, cache = memoise::cache_filesystem("~/R/.rcache"))

#' wsd data for one code an one field
#'
#' @param code
#' @param field
#' @param start
#' @param end
#' @param options
#' @param name_value_col
#'
#' @return 2-column tibble {DATETIME, code or field}
#' @export
#'
#' @examples
.wsd_11 <- function(code, field, start, end, options = '', name_value_col = c('field', 'code')) {
  # value column use code or field?
  name_value_col <- match.arg(name_value_col)
  col_name <- ifelse(name_value_col == 'field', field, code)
  wsd_data <- mem_wsd(
    code,
    field,
    as.character(start),
    as.character(end),
    options)
  wsd_data$Data %>% setNames(c(.datetime_col, col_name)) %>%
    as_tibble()
}

.wsd_tidy <- function(code, field, start, end, options = "", value_col = "value") {
  wsd_data <- mem_wsd(code, field, as.character(start), as.character(end), options)
  wsd_data$Data %>% as_tibble() %>% setNames(c(.datetime_col, value_col)) %>%
    mutate(code = code, field = field) %>%
    select(.datetime_col, code, field, value_col)
}

.wsd_xts <- function(code, field, start, end, options = "") {
  .wsd_tidy(code, field, start, end, options) %>%
    filter_any_na_numeric() %>%
    select(DATETIME, value) %>%
    df_to_xts() %>%
    setNames(code)
}

#' Wsd data returned as a vector for a cross section of assets, single field
#' and single point in time
#'
#' @param codes asset codes
#' @param field data field
#' @param as_of as of date
#' @param options wsd options
#'
#' @return
#' @export
#'
#' @examples
#' wsd_xc(c("000001.SZ", "000001.SH", "000001.SZ"), "sec_name", as_of)
#' .wsd_vec("000001.SH", c("sec_name", "sec_type"), as_of)
wsd_xc <- function(codes, field, as_of = Sys.Date(), options = "") {
  uniq_codes <- unique(codes)
  wsd_data <- mem_wsd(uniq_codes, field, as_of, as_of, options)
  named_vec <- wsd_data$Data[, -1] %>% setNames(uniq_codes)
  named_vec[codes]
}

#' Title
#'
#' @param code
#' @param fields
#' @param as_of
#' @param options
#'
#' @return a named vector
#' @export
#'
#' @examples
#' code <- "004477.OF"
#' fields <- c("close", "fund_benchindexcode", "sec_name")
#' wsd_meta(code, fields)
wsd_meta <- function(code, fields, as_of = Sys.Date(), options = ""){
  wsd_data <- mem_wsd(code, fields, as_of, as_of, options)
  wsd_data$Data[, -1] %>% setNames(fields) %>% unlist()
}

#' wsd field for data start date for different security types
#'
#' @param codes
#'
#' @return
#' @export
#'
#' @examples
#' codes <- c(
#' "000300.SH",
#' "000001.SZ",
#' "000877.OF",
#' "XT102034.XT"
#' )
#' wsd_start_date_field(codes)
wsd_start_date_field <- function(codes) {
  wsd_xc(codes, "sec_type", Sys.Date()) %>%
    purrr::map_chr( ~
                      if(stringr::str_detect(., "指数"))
                        "BASEDATE"
                    else if(stringr::str_detect(., "基金") || stringr::str_detect(., "产品"))
                      "FUND_SETUPDATE"
                    else
                      "IPO_DATE")
}

#' wsd data start date for different security types
#'
#' @param codes
#' @param as_of
#'
#' @return
#' @export
#'
#' @examples
#' codes <- c(
#' "000300.SH",
#' "000001.SZ",
#' "000877.OF",
#' "XT102034.XT"
#' )
#' wsd_start_date(codes)
wsd_start_date <- function(codes, as_of = as.character(Sys.Date())){
  tibble::tibble(
    code = codes,
    field = wsd_start_date_field(codes),
    start = as_of,
    end = as_of
  ) %>%
    purrr::pmap_dfr(.wsd_tidy) %>%
    mutate(value = wind_num_to_date(value)) %>%
    pull(value)
}

#' Cache wsd closing prices for different security types
#' from first day available to as_of date
#'
#' @param codes
#' @param as_of
#' @param options
#'
#' @return
#' @export
#'
#' @examples
#' codes <- c(
#' "000300.SH",
#' "000001.SZ",
#' "000877.OF",
#' "XT102034.XT"
#' )
#' as_of <- "2018-05-31"
#' wsd_prices <- cache_wsd_price(codes, as_of, options = "period=W;PriceAdj=F")
cache_wsd_price <- function(codes, as_of, options = "period=W;PriceAdj=F"){
  tibble::tibble(
    code = codes,
    field = wsd_price_field(codes),
    start = wsd_start_date(codes, as_of),
    end = as_of,
    options = options
  ) %>%
    purrr::pmap_df(.wsd_tidy)
}

#' Convert Wind date from number to string
#'
#' @export
wind_num_to_date <- function(num) {
  as.Date(num, origin = '1899-12-30') %>% as.character()
}

#' wsd price field for given codes
#' Use close3 for asset prices so missing valus are NaN
#'
#' @param codes
#'
#' @return
#' @export
#'
#' @examples
#' codes <- c("100038.OF", "H00300.CSI", "885012.WI")
#' wsd_price_field(codes)
wsd_price_field <- function(codes){
  codes %>%
    stringr::str_extract(., stringr::regex("\\.[a-zA-Z]+$")) %>%
    {ifelse(. %in% c(".OF", ".XT"), "nav_adj", "close3")}
}

#' closing price or NAV series from wsd
#'
#' @param codes
#' @param start
#' @param end
#' @param options
#'
#' @return data frame

wsd_price <- function(codes, start, end, options = "period=W;PriceAdj=F"){
  tibble::tibble(code = codes) %>%
    dplyr::mutate(
      field = wsd_price_field(code),
      start = as.character(start),
      end = as.character(end),
      options = options
    ) %>%
    purrr::pmap_df(.wsd_tidy) %>%
    dplyr::select(-.data$field)
}

#' closing price or NAV series from wsd, aligned on first code
#'
#' @param codes
#' @param start
#' @param end
#' @param options
#'
#' @return xts
#'
#' @examples
#' codes <- c("000300.SH", "000877.OF")
#' start <- "2018-07-01"
#' end <- "2018-07-17"
#' p <- wsd_price_xts(codes, start, end, options = "period=W;PriceAdj=F")
wsd_price_xts <- function(codes, start, end, options = "period=W;PriceAdj=F"){
  tibble::tibble(code = codes) %>%
    dplyr::mutate(
      field = wsd_price_field(code),
      start = as.character(start),
      end = as.character(end),
      options = options
    ) %>%
    purrr::pmap(.wsd_xts) %>%
    purrr::reduce(xts::merge.xts, join = "left") %>%
    setNames(codes)
}

#' OHLC from wsd
#'
#' @param codes
#' @param start
#' @param end
#' @param options
#'
#' @return
#' @export
#'
#' @examples
#' codes <- "000905.SH"
#' start <- "2018-06-01"
#' end <- "2018-06-20"
#' options <- "period=W;PriceAdj=F"
#' wsd_ohlc(codes, start, end)
wsd_ohlc <- function(codes, start, end, fields = c("open", "high", "low", "close", "volume"), options = "period=W;PriceAdj=F") {
  tibble(
    code = codes,
    field = fields,
    start = start,
    end = end,
    options = options
  ) %>%
    pmap_df(.wsd_tidy) %>%
    spread(field, value) %>%
    select(.datetime_col, code, fields)
}

wsd_price_full_history <- function(codes, as_of, options = "period=W;PriceAdj=F"){
  as_of <- as.character(as_of)

  universe <- tibble::tibble(code = codes) %>%
    dplyr::mutate(
      field = wsd_price_field(code),
      start = wsd_start_date(code, as_of),
      end = as_of,
      options = options
    )

  universe %>%
    purrr::pmap_df(.wsd_tidy) %>%
    dplyr::select(-.data$field)
}

#' Extract Wind options period into number of days
#'
#' @param options
#'
#' @return
#' @export
#'
#' @examples
#' options <- "period=W;PriceAdj=F"
#' wind_option_to_period(options) == 7 # weekly
wind_option_to_period <- function(options){
  period <- options %>%
    toupper() %>%
    str_match("PERIOD=([A-Z])") %>%
    `[[`(1, 2)

  case_when(
    period == "W" ~ lubridate::weeks(),
    period == "M" ~ lubridate::period(num = 1, units = "month"),
    period == "Q" ~ lubridate::period(num = 3, units = "month"),
    period == "Y" ~ lubridate::years(),
    TRUE ~ lubridate::days()
  )
}

#' return from closing price or NAV series from wsd, aligned on first code
#'
#' @param codes
#' @param start
#' @param end
#' @param options
#' @return xts, one column per code
wsd_return_xts <- function(codes, start, end, options = "period=W;PriceAdj=F"){
  price_start <- as.Date(start) - wind_option_to_period(options)

  wsd_price_xts(codes, as.character(price_start), end, options) %>%
    PerformanceAnalytics::Return.calculate(.) %>%
    `[`(sprintf("%s/", as.character(start[[1]])))
}

#' Get fund holdings from Wind
#'
#' @param date
#' @param code
#'
#' @return
#' @export
#'
#' @examples
#' date <- "2017-12-31"
#' code <- "000480.OF"
#' hld <- wind_fund_holdings(date, code)
wind_fund_holdings <- function(date, code){
  option = sprintf('rptdate=%s;windcode=%s',
                   format(as.Date(date), "%Y%m%d"), code)
  mem_wset('allstockhelddetaill', option)$Data
}

#' #' Wrapper around WindR::w.wsd() to get a static value
#' #'
#' #' @export
#' #' @examples
#' #' code <- '100038.OF'
#' #' field <- 'sec_name'
#' #' wsd_get_static(code, field)
#' wsd_get_static <- function(code, field, date = "2018-05-31") {
#'   # Wind does not allow repeated code
#'   uniq_code <- unique(code)
#'   wsd_data <- mem_wsd(uniq_code, field, date, date, "")
#'   named_vec <- wsd_data$Data[, 2] %>% setNames(uniq_code)
#'   named_vec[code] %>% unname()
#' }

#' #' Download daily data from Wind and returns as xts
#' #'
#' #' @param code a single security code
#' #' @param field a single data field name
#' #' @param start start date
#' #' @param end end date
#' #' @param options options, default is empty
#' #'
#' #' @return an xts object
#' #' @export
#' #'
#' #' @examples
#' #' code <- "000300.SH"
#' #' field <- "close"
#' #' start <- "2018-05-01"
#' #' end <- "2018-05-10"
#' #' price <- wsd_get_xts(code, field, start, end)
#' wsd_get_xts <- function(code, field, start, end, options = ''){
#'   wsd_data <- mem_wsd(code, field, start, end, options)
#'   wsd_data$Data %>% df_to_xts()
#' }

#' #' Wrapper around WindR::w.wsd() to get time series data
#' #'
#' #' @description
#' #' code/field/start/end/options are organized into a table
#' #' that is passed to w.wsd()
#' #' @param tb data.frame[codes,fields,start,end,options], arguments passed to WindR::w.wsd().
#' #' @param name_value_col Value column names can be either code or field.
#' #' @return wsd returned data as a data.frame.
#' #' @export
#' #' @examples
#' #' tb <- tibble::tibble(
#' #' code = c('000300.SH', '000905.SH'),
#' #' field = "close",
#' #' start = '2017-12-20',
#' #' end = '2017-12-31',
#' #' options = ""
#' #' )
#' #' wsd_get_batch(tb, name_value_col = 'field')
#' wsd_get_batch <- function(tb, name_value_col = c('field', 'code')) {
#'   # value column use code or field?
#'   name_value_col <- match.arg(name_value_col)
#'   # Automatically override output column names based on code/field
#'   if ((dplyr::n_distinct(tb$code) == 1) && (dplyr::n_distinct(tb$field) > 1)) {
#'     name_value_col <- 'field'
#'   }
#'   else if ((dplyr::n_distinct(tb$code) > 1) && (dplyr::n_distinct(tb$field) == 1)) {
#'     name_value_col <- 'code'
#'   }
#'
#'   args <- tb %>%
#'     dplyr::mutate(name_value_col = name_value_col)
#'
#'   purrr::pmap(args, .wsd_11) %>%
#'     purrr::reduce(dplyr::full_join, by = .datetime_col)
#' }

#' #' fund NAV from Wind
#' #'
#' #' @param code
#' #' @param start_date
#' #' @param end_date
#' #' @param option
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #' code <- c("XT102034.XT", "000480.OF", "000300.SH")
#' #' wsd_nav(code, start_date, end_date, option = "period=W")
#' wsd_nav <- function(code, start_date, end_date, option = "period=W"){
#'   tb <- tibble(code = code) %>%
#'     mutate(
#'       field = wind_price_field(code),
#'       start = start_date,
#'       end = end_date,
#'       option = option
#'     )
#'   wsd_get_batch(tb) %>%
#'     df_to_xts() %>%
#'     setNames(code)
#' }

#'
#' #' fund volatility from Wind
#' #'
#' #' @param code
#' #' @param start_date
#' #' @param end_date
#' #' @param option
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #' code <- c("300059.SZ", "000480.OF", "000300.SH")
#' #' start_date <- "2018-01-01"
#' #' end_date <- "2018-05-31"
#' #' wsd_vol(code, start_date, end_date, option = "period=W")
#' wsd_vol <- function(code, start_date, end_date, option = "period=W"){
#'   wsd_price(code, start_date, end_date, option = "period=W") %>%
#'     select(-field) %>% spread(code, value) %>%
#'     PerformanceAnalytics::Return.calculate() %>%
#'     PerformanceAnalytics::StdDev.annualized()
#' }

#' Wrapper around WindR::w.wsd() to get time series data
#'
#' @param codes,fields,start,end,options Arguments passed to WindR::w.wsd().
#' @param name_value_col Value column names can be either code or field.
#' @return wsd returned data as a data.frame.
#' @export
#' @examples
#' start <- '2017-12-20'
#' end <- '2017-12-31'
#'
#' codes <- c(
#'   '000300.SH',
#'   '000905.SH'
#' )
#' fields <- 'close'
#' rv <- wsd_get(codes, fields, start, end)
#' codes <- c(
#'   '000300.SH',
#'   '000905.SH'
#' )
#' fields <- c('close', 'volume')
#' rv <- wsd_get(codes, fields, start, end)
wsd_get <- function(codes, fields, start, end, options = '', name_value_col = c('field', 'code')) {
  # output column use code or field?
  name_value_col <- match.arg(name_value_col)
  # Automatically override output column names based on code/field
  if ((length(codes) == 1) && (length(fields) > 1)) {
    name_value_col <- 'field'
  }
  else if ((length(codes) > 1) && (length(fields) == 1)) {
    name_value_col <- 'code'
  }

  args <- tibble::tibble(
    code = codes,
    field = fields,
    start = start,
    end = end,
    options = options,
    name_value_col
  )

  purrr::pmap(args, .wsd_11) %>%
    purrr::reduce(dplyr::full_join, by = .datetime_col)
}


#' Get index constituents
#'
#' @param index
#' @param date
#'
#' @return
#' @export
#'
#' @examples
#' date <- "2018-07-05"
#' index <- "000300.SH"
#' ic <- wset_index_const(index, date)
wset_index_const <- function(index, date){
  if(!is.character(date))
    date <- as.character(date)
  w_data <- mem_wset('sectorconstituent',
                     wind_option_list_to_str(list(date = date, windcode = index)))
  w_data$Data %>%
    as_tibble() %>%
    transmute(code = wind_code, name = sec_name)
}

#' Title
#'
#' @param code    fund code
#' @param bm_code default = fund_benchindexcode from Wind
#' @param start   default = first price available date from Wind
#' @param end     default = today
#' @param options default = "period=W;PriceAdj=F"
#'
#' @return
#' @export
#'
#' @examples
#' code <- "001878.OF"
#' ret <- wsd_asset_bm_return(code)
#' ret2 <- wsd_asset_bm_return(code, bm_code = "H00300.CSI")
wsd_asset_bm_return <- function(code, bm_code = NULL, start = NULL, end = NULL, options = "period=W;PriceAdj=F"){
  # Fall back to defaults if not set
  bm_code <- bm_code %||% wsd_meta(code, "fund_benchindexcode")
  start <- start %||% wsd_start_date(code)
  end <- end %||% Sys.Date()

  args <- tibble(
    codes = c(code, bm_code),
    start,
    end,
    options
  )

  invoke(wsd_return_xts, args) %>%
    xts_to_df() %>%
    filter_any_na_numeric() %>%
    df_to_xts()
}

#' trading days between start and end date
#' @return vector of Dates
#' @export
#' @examples
wind_tdays <- function(freq = c('D', 'W', 'M', 'Q', 'S', 'Y'), start = "2000-01-01", end = "2020-12-31") {
  freq <- match.arg(freq)
  mem_wtdays(start, end, "Period=" %+% freq)$Data$DATETIME
}
