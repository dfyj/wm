.f_init_positions <- function(tw, price, init_port_nav = 100){
  init_port_nav * tw / price
}

.f_update_positions <- function(positions, tw, price){
  sum(positions * price) * tw / price
}

#' Convert weights to positions
#'
#' @param weights
#' @param price
#' @param init_port_nav initial portfolio nav
#'
#' @return
#' @export
#'
#' @examples
port_weights_to_positions <- function(weights, price, init_port_nav = 100){
  positions <- weights + NA

  for (i in 1:nrow(weights)){
    date <- index(weights)[[i]]
    tw <- weights[date] %>% coredata()
    p <- price[date] %>% coredata()

    # init positions
    if(i == 1){
      window(positions, date) <- .f_init_positions(tw, p, init_port_nav)
    }
    # rebalance
    else{
      date0 <- index(weights)[[i-1]]
      pos0 <- positions[date0] %>% coredata()
      window(positions, date) <- .f_update_positions(pos0, tw, p)
    }
  }

  positions
}

#' Portfolio summary stats
#'
#' @param positions
#' @param price
#'
#' @return
#' @export
#'
#' @examples
port_summary <- function(positions, price){
  md <- xts_merge(positions, price) %>%
    map(na.locf)

  nav <- md$positions * md$price

  port_nav <- nav %>%
    xts_rowapply(sum)

  weights <- sweep(nav, 1, port_nav, FUN = `/`)

  shares_turnover <-
    diff(positions) %>%
    na.omit()

  dollar_turnover <-
    (shares_turnover * price)

  port_dollar_turnover <-
    dollar_turnover %>%
    abs() %>%
    xts_rowapply(sum)

  make_list(
    port_nav,
    nav,
    weights,
    port_dollar_turnover,
    shares_turnover,
    dollar_turnover
  )
}
