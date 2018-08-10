
#' Convert data frame to xts
#'
#' @param df A data frame. First column is date/time
#' @return An xts object
#' @export
df_to_xts <- function(df){
  xts(df[, -1], dplyr::pull(df, 1) %>% as.Date)
}

#' Convert xts to data frame
#' @export
#' @examples
#' ts <- np
#' xts_to_df(ts) %>% select(1:3) %>% head(3)
xts_to_df <- function(ts, datetime_col = .datetime_col){
  ts %>%
    # xts::reclass() %>%
    as.data.frame() %>%
    tibble::rownames_to_column(datetime_col) %>%
    tibble::as_tibble()
}


translate_code <- function(code, to, is_index = FALSE) {
  parsed_code <- parse_stock_code(code, is_index)
  key <- switch(to,
                'tushares' = 'digits',
                'wsd' = 'full',
                'full')
  parsed_code[[key]]
}

#' convert '000001.SZ' or '000001' to '000001.SZ'
stock_code_full <- function(codes){
  suffix <- ifelse(startsWith(codes, '6'), 'SH', 'SZ')
  paste(codes, suffix, sep = ".")
}

#' convert '000001.SZ' or '000001' to
#' c(digits = '000001', suffix = 'SZ', full = '000001.SZ')
parse_stock_code <- function(code, is_index = FALSE) {
  digits = stringr::str_match(code, .six_digits_code_pattern)[[1]]
  if (is.na(digits)) {
    stop('code must start with 6 digits')
  }
  suffix <- ifelse(is_index || startsWith(digits, '6'), 'SH', 'SZ')
  full <- sprintf('%s.%s', digits, suffix)
  return(c(
    digits = digits,
    suffix = suffix,
    full = full
  ))
}

create_dir <- function(path) {
  dir_name <- dirname(path)
  if (!dir.exists(dir_name))
    dir.create(dir_name, recursive = TRUE)
}

#' list "dates" in given directory (e.g. yyyymmdd.csv)
#'
#' @examples
#' dir <- 'c:/db/csv/wsd/index_weights/000300.SH'
#' suffix = 'csv'
#' ls_dates(dir, suffix)
ls_dates <- function(dir, suffix = 'rds') {
  pattern <- sprintf('^([0-9]{8}).%s$', suffix)
  date_file_names <-  list.files(dir, pattern = pattern)
  stringr::str_match(date_file_names, pattern)[, 2] %>%
    as.Date(format = .DATE_FILE_NAME_FORMAT)
}

#' list "dates" between start and end in given directory
#' dir <- csv_dir_index_weights('000300')
#' e.g. yyyymmdd.csv
#'
#' @examples
#' dir <- 'c:/db/csv/wsd/index_weights/000300.SH'
#' start <- '2017-01-20'
#' end <- '2017-05-20'
#' ls_dates_between(dir, start, end)
ls_dates_between <- function(dir, start, end) {
  dates <- ls_dates(dir)
  dates[dplyr::between(dates, as.Date(start), as.Date(end))]
}

#' date on or before given date in given directory
#' in dir there are files named yyyymmdd.XXX
#'
#' @examples
#' dir <- 'c:/db/csv/wsd/index_weights/000300.SH'
#' as_of <- '2017-09-20'
#' ls_date_on_or_before(dir, as_of)
ls_date_on_or_before <- function(dir, as_of) {
  dates <- ls_dates(dir)
  Find(function(x)
    x <= as_of, dates, right = TRUE)
}

# tdays <- function(start, end, calendar = 'SSE') {
#   all_tdays <- get_all_tdays(calendar)
#   all_tdays[dplyr::between(all_tdays, as.Date(start), as.Date(end))]
# }

tday_on_or_before <- function(date) {
  WindR::w.tdaysoffset(0, date)$Data[[1]]
}

last_trade_dates <- function(dates){
  trade_days <- get_all_tdays()
  trade_days_xts <- xts(rep(0, length(trade_days)), order.by = trade_days)
  purrr::map(dates, ~ trade_days_xts %@% . %>% index()) %>%
    do.call('c', .)
}

#' get last available value in xts data
#' @export
#' @examples
#' xts_data %at% '2017-03-01'
'%at%' <- function(xts_data, date) {
  xts::last(xts_data['/' %+% date])
}

#' File base name without suffix
#' @export
file_base_name <- function(path) {
  basename(path) %>% str_replace('\\.\\w+$', '')
}

# get trade date
get_trade_date <- function(date, offset) {
  return(WindR::w.tdaysoffset(offset, date)$Data[1, 1])
}

#' Return head and tail rows of a data frame
#'
#' @param df data frame
#' @param n number of head and tail rows to return
#' @export
#'
headtail <- function(df, n = 3L) {
  df %>%
  {
    rbind(head(., n), tail(., n))
  }
}

#' a shorthand for join multiple strings into a single string
#'
#' @return Joined string
#' @export
'%+%' <- function(x, y) {
  stringr::str_c(x, y)
}

#' Convert wind api options string to list
#' @export
# wind_option_str_to_list('PriceAdj=F;currency=') %>% identical(list(PriceAdj = 'F', currency = ''))
wind_option_str_to_list <- function(options) {
  spl <- options %>%
    stringr::str_split(pattern = ';') %>%
    purrr::map(stringr::str_split, pattern = '=') %>%
    '[['(1) %>%
    purrr::transpose()
  if (length(spl) < 2)
    return(list())
  spl[[2]] %>% purrr::set_names(spl[[1]])
}

#' Convert wind api options string to list
#' @export
#' @examples
#' wind_option_list_to_str(list(PriceAdj = 'F', currency = '')) %>%  identical('PriceAdj=F;currency=')
wind_option_list_to_str <- function(options) {
  paste(names(options),
        options,
        sep = '=',
        collapse = ';')
}

wind_option_get <- function(options, field) {
  opt_li <- wind_option_str_to_list(options)
  names(opt_li) <- tolower(names(opt_li))
  return(opt_li[[tolower(field)]])
}

str_to_lines <- function(str, sep = ',') {
  str %>%
    str_split(sep) %>% '[['(1) %>%
    {
      sprintf("'%s',", .)
    } %>% cat(sep = '\n')
}

ifel <- function(condition, true, false) {
  if (condition)
    true
  else
    false
}

#' project a data.frame to larger column set
#' @export
#' @examples
#' to_cols <- mu
#' cols_to_copy <- 'date'
#' project_cols(df, to_cols, 0)
project_cols <-
  function(df,
           to_cols,
           cols_to_copy = NULL,
           fill = NA) {
    missing_cols <- setdiff(to_cols, names(df))
    missing_df <-
      as.list(purrr::rep_along(missing_cols, fill)) %>% setNames(missing_cols)
    if (!is.null(cols_to_copy))
      to_cols <- c(cols_to_copy, to_cols)
    cbind(df, missing_df) %>% dplyr::select(to_cols)
  }

#' Project a data frame to given rows
#' @examples
#' df <- tibble(x = letters[1:3], y = 1:3)
#' to_rows <- c('b', 'c', 'z')
#' key <- 'x'
#' project_rows(df, to_rows, key)
project_rows <- function(df, to_rows, key) {
  to_df <- tibble::tibble(to_rows) %>% setNames(key)
  stopifnot(key %in% colnames(df))
  dplyr::left_join(to_df, df, by = key)
}

#' rds data directory
#' @examples
#' rds_dir('sectorconstituent', '399106.SZ')
rds_dir <- function(...) {
  file.path('c:/db/rds', ...)
}

#' rds data path
#' @examples
#' rds_path('sectorconstituent', '399106.SZ')
rds_path <- function(..., suffix = '.rds', root = 'c:/db/rds') {
  path <- file.path(root, ...)
  if (!endsWith(path, suffix))
    path <- sprintf('%s%s', path, suffix)
  path
}


#' write rds data
#' #' @examples
#' rds_write(x, path)
rds_write <- function(...) {
  path <- list(...)[[2]]
  create_dir(path)
  readr::write_rds(...)
  path
}

rds_read <- function(...) {
  readr::read_rds(...)
}

top_bottom_n <- function(df, col, n = 5L) {
  df %>%
    tidyr::drop_na(col) %>%
    dplyr::arrange(dplyr::desc(eval(parse(text = col)))) %>%
    headtail(n)
}

component_extract <- function(lst, name, default) {
  if (is.null(lst))
    return(default)
  if (purrr::has_element(names(lst), name) && !is.na(lst[[name]]))
  {
    lst[[name]]
  }
  else{
    default
  }
}

df_sort <- function(df, keys) {
  purrr::lift(dplyr::arrange)(c(list(df), keys))
}

df_unique <- function(df, keys) {
  df %>% {
    purrr::lift(dplyr::distinct)(c(list(.), keys, .keep_all = TRUE))
  }
}

#' Filter out if any numeric column is NULL
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
filter_any_na_numeric <- function(df){
  df %>%
    dplyr::filter_if(., is.numeric, dplyr::all_vars(!is.na(.)))
}

#' Filter out if all numeric columns are NULL
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
filter_all_na_numeric <- function(df){
  df %>%
    dplyr::filter_if(., is.numeric, dplyr::any_vars(!is.na(.)))
}

#' Zip a list of data frames
#' @param li list of data.frame
#' @param key column to merge

#' @examples
#' li <- list(
#'   d1 = data.frame(x = letters[1:3], y = 1:3),
#'   d2 = data.frame(x = letters[1:3], y = 4:6)
#' )
#' key <- 'x'
#'
#' df_zip(li, key)
#' ==
#' x d1 d2
#' a  1  4
#' b  2  5
#' c  3  6
df_zip <- function(li, key) {
  purrr::map_at(li, 2:length(li), ~ select(., -!!rlang::sym(key))) %>%
    dplyr::bind_cols() %>%
    setNames(c(key, names(li)))
}

#' Top n weights as percent of total
#'
#' @param df
#' @param n
#' @param value
top_n_pct <- function(df, n = 10, value = "weight"){
  topn_value <- top_n(df, n, !!rlang::sym(value)) %>%
    summarise(sum(!!rlang::sym(value)))
  total_value <- df %>%
    summarise(sum(!!rlang::sym(value)))
  (topn_value / total_value)[[1]]
}


#' Sum of top n values
#'
#' @param vec
#'
#' @return
#' @export
#'
#' @examples
sum_top_n <- function(vec, n){
  top
}

#' transpose and cbind list of data frames
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
df_t_bind_cols <- function(...){
  list(...) %>%
    keep(is.data.frame) %>%
    map(t) %>%
    bind_cols()
}
