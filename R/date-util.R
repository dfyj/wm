#' Returns quarter ends between two dates
#'
#' @param start_date
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
qtr_end <- function(start_date, end_date) {
  seq.Date(as.Date(start_date), as.Date(end_date), by = 'month') %>%
    as.yearqtr() %>% as.Date(, frac = 1) %>% unique()
}

#' Month start for a given date
#'
#' @param date
#'
#' @return
#' @export
#'
#' @examples
get_month_start <- function(date) {
  date %>% zoo::as.yearmon() %>% as.Date(, frac = 0) %>% as.character()
}

#' Month end for a given date
#'
#' @param date
#'
#' @return
#' @export
#'
#' @examples
get_month_end <- function(date) {
  date %>% zoo::as.yearmon() %>% as.Date(, frac = 1) %>% as.character()
}

#' Returns month ends between two dates
#'
#' @param start_date
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
month_end <- function(start_date, end_date) {
  seq.Date(as.Date(start_date), as.Date(end_date), by = 'month') %>%
    as.yearmon() %>% as.Date(, frac = 1)
}

#' Return calendar start date of the year
#'
#' @param dates
#'
#' @return
#' @export
#'
#' @examples
start_of_year <- function(dates) {
  dates %>%
    as.Date() %>%
    lubridate::year() %>%
    lubridate::make_date(1, 1) %>%
    as.character()
}

date_to_str <- function(date) {
  format(date, '%Y-%m-%d')
}


# parse ISO8601 dates
parseISO8601 <- function(...) {
  li <- list(...)

  if (length(li) > 1) {
    return(c(li[[1]], li[[2]]))
  } else {
    li <- xts::.parseISO8601(li)
    ans <- c(lubridate::date(li[[1]]), lubridate::date(li[[2]]))
    return(ans)
  }
}

#' Convert a time period string to start date
#'
#' @param period_str
#' @param as_of
#'
#' @return
#' @export
#'
#' @examples
#' period_str <- "近5年"
#' period_str <- "今年以来"
#' period_str_to_start_date(period_str, as_of)
period_str_to_start_date <- function(period_str, as_of) {
  if (stringr::str_detect(period_str, "今年以来")) {
    return(start_of_year(as_of))
  }
  else {
    n_years <- stringr::str_match(period_str, "近([1-9]+)年")[, 2] %>% as.numeric()
    stopifnot(!is.na(n_years))
    (as.Date(as_of) - lubridate::years() * n_years) %>% as.character()
  }
}

#' Create and cache calendar
#'
#' @return
#' @export
#'
#' @examples
create_calendar <- function(){
  start.date = as.Date("2000-01-01")
  end.date = as.Date("2020-12-31")

  tdays <- wind_tdays(freq = "D", start = start.date, end = end.date)

  alldays <- seq.Date(from = start.date, to = end.date, by = "day")

  holidays <- dplyr::anti_join(tibble::tibble(date = alldays),
                               tibble::tibble(date = tdays),
                               by = "date")


  bizdays::create.calendar(
    "SSE",
    holidays = holidays$date,
    start.date = start.date,
    end.date = end.date,
    weekdays = c('saturday', 'sunday')
  )

  bizdays::save_calendar("SSE", file.path("~/.R/calendars/SSE.cal"))
}

