.tsdb_root <- function(){
  'C:/db/tsdb'
}


#' Parse Tsdb symbol
#'
#' @return c(db, dir, name) for valid symbol, NA otherwise
#'
#' @examples
#' symbol <- 'eq_000300.SH@@close'
#' .tsdb_parse_symbol(symbol)
.tsdb_parse_symbol <- function(symbol) {
  m <- stringr::str_match(symbol, '([a-zA-Z0-9]+)_(.+)@(.+)')
  if (is.na(m[[1]]))
    NA
  else
    m[-1] %>% setNames(c('db', 'dir', 'name'))
}

#' Returns file path of Tsdb symbol
#' @export
#' @examples
#' symbol <- 'eq_000300.SH@@close'
#' .tsdb_path(symbol)
.tsdb_path <- .tsdb_path <- function(symbol) {
  c(.tsdb_root(), .tsdb_parse_symbol(symbol)) %>%
    purrr::invoke(file.path, .) %>%
    sprintf('%s%s', ., '.rds')
}

#' Update Tsdb data
#' @export
#' @examples
#' tsdb_update(data, symbol)
tsdb_update <- function(data, symbol) {
  path <- .tsdb_path(symbol)
  rds_write(data, path)
}

#' Load Tsdb data
#' @export
#' @examples
#' symbol <- 'eq_000300.SH@@close'
#' tsdb(symbol)
#' tsdb(symbol, '2018-01-01', '2018-01-10')
#' tsdb(symbol, as.Date('2018-01-01'), '2018-01-10')
#' tsdb(symbol, '2018-01')
tsdb <- function(symbol, ...) {
  date_range <- list(...) %>% lapply(as.character) %>% str_c(collapse = '/')
  data <- rds_read(.tsdb_path(symbol))
  if (length(date_range))
    data[date_range]
  else
    data
}

#' Tsdb symbol for equities
#' @export
#' @examples
#' code <- '000300.SH'
#' field <- 'close'
#' tsdb_eq_symbol(code, field)
tsdb_eq_symbol <- function(code, field) {
  sprintf('eq_%s@%s', code, field)
}

#' Tsdb symbol for funds
#' @export
#' @examples
#' code <- 'XT1527428.XT'
#' field <- 'nav_adj'
#' tsdb_fund_symbol(code, field)
tsdb_fund_symbol <- function(code, field) {
  sprintf('fund_%s@%s', code, field)
}
