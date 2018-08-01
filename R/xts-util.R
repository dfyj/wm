

#' Title
#'
#' @param dates
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' dates <- c("2018-04-03", "2018-04-10", "2018-04-15") %>% as.Date()
#' xts_from_dates(dates)
#' xts_from_dates(dates, 1)
xts_from_dates <- function(dates, x = NULL) {
  if (is.null(x))
    return(xts(order.by = dates))

  if (length(x) == 1)
    x <- rep(x, length(dates))

  xts(x, order.by = dates)
}

#' Title
#'
#' @param ts
#' @param from
#' @param to
#'
#' @return
#' @export
#'
#' @examples
#' from_date <- "2018-04-05"
#' to_date <- "2018-04-13"
#' ts <- xts(rep(1, 3), order.by = c("2018-04-03", "2018-04-10", "2018-04-15") %>% as.Date())
#' sub_ts <- xts_bt_dates(ts, from_date, to_date, include_from = FALSE, include_to = FALSE)
#' sub_ts <- xts_bt_dates(ts, from_date, to_date, include_from = TRUE, include_to = FALSE)
#' sub_ts <- xts_bt_dates(ts, from_date, to_date, include_from = TRUE, include_to = TRUE)
#' list(from_date, to_date, ts, sub_ts)
xts_bt_dates <-
  function(ts,
           from_date = "",
           to_date = "",
           include_from = FALSE,
           include_to = FALSE) {
    from_date <- as.Date(from_date)
    to_date <- as.Date(to_date)

    if (include_from) {
      from_date <-
        ts[str_c("/", from_date, sep = "")] %>% tail(1) %>% index
      if (is_empty(from_date))
        stop("trying to include from_date that is beyond start of ts")
    }

    if (include_to) {
      to_date <- ts[str_c(to_date, "/", sep = "")] %>% head(1) %>% index
      if (is_empty(to_date))
        stop("trying to include to_date that is beyond end of ts")
    }
    ts[str_c(from_date, to_date, sep = "/")]
  }

#' Align one xts to another by date
#'
#' @param align_from xts object
#' @param align_to xts object
#'
#' @return
#' @export
#'
#' @examples
#' xts_align(align_from, align_to)
xts_align <- function(align_from, align_to) {
  align_to <-
    xts_bt_dates(
      align_to[, 1],
      start(align_from),
      end(align_from),
      include_from = FALSE,
      include_to = TRUE
    )

  xts::merge.xts(align_to, align_from) %>%
    zoo::na.locf() %>%
    `[`(zoo::index(align_to), -1)
}

xts_align_regularly <- function(align_from, freq = .frequency_list){
  freq <- match.arg(freq)
  f_apply <- .frequency_to_xts_apply_func[[freq]]

  align_to <-
    bizdays::bizseq(start(align_from), end(align_from), cal = bizdays::bizdays.options$get("default.calendar")) %>%
    xts_from_dates(1) %>%
    f_apply(xts::last)

  xts_align(align_from, align_to)
}

#' Apply function to xts rows
#'
#' @example
#' ts <-
#'   data.frame(x = 1:3, y = 3:5) %>%
#'   as.xts(order.by = timeBasedSeq("2018-01-01::2018-01-03"))

#' f <- function(x) c(sum(x), mean(x))
#' cols <- c("sum", "mean")
#' merge.zoo(ts, xts_rowapply(ts, f, cols))
xts_rowapply <- function(ts, f, cols, ...){
  ts %>% apply(1, f, ...) %>%
    t() %>%
    matrix(nrow = nrow(ts)) %>%
    as.xts(index(ts)) %>%
    setNames(cols)
}

#' Approximate time series periodicity
#'
#' @param ts
#'
#' @return
#' @export
#'
#' @examples
xts_freq <- function(ts){
  xts::periodicity(ts)$scale
}

#' Estimate annualization factor for given xts
#'
#' @param x an xts object
#'
#' @return
#' @export
#'
#' @examples
#' xts_annual_factor(ret)
xts_annual_factor <- function(ts){
  .frequency_to_annual_factor[[xts_freq(ts)]]
}

