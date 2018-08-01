
#' Dygraph ohlcv chart
#'
#' @param ohlcv
#'
#' @return
#' @export
#'
#' @examples
#' dygraph_ohlcv(ohlcv)
dygraph_ohlcv <- function(ohlcv){
  stopifnot(all(
    is.data.frame(ohlcv),
    c(.datetime_col, .ohlcv_names) %in% colnames(ohlcv)
  ))

  ohlcv %>%
    select(c(.datetime_col, .ohlcv_names)) %>%
    df_to_xts() %>%
    dygraph() %>%
    dyAxis("y", valueRange = c(min(ohlcv$close)*0.7, max(ohlcv$close))) %>%
    dyAxis("y2", valueRange = c(0, 2*max(ohlcv$volume))) %>%
    dyCandlestickGroup(c('open', 'high', 'low', 'close')) %>%
    dyBarSeries("volume", axis = "y2") %>%
    dyRangeSelector() %>%
    dyCrosshair()
}

#' Plotly ohlc chart
#'
#' @param ohlc
#'
#' @return
#' @export
#'
#' @examples
#' plotly_ohlc(ohlcv %>% tail(100))
plotly_ohlc <- function(ohlc){
  stopifnot(all(
    is.data.frame(ohlc),
    c(.datetime_col, .ohlc_names) %in% colnames(ohlc)
    ))

  ohlc %>%
    plot_ly(x = ~DATETIME, type="candlestick",
            open = ~open, close = ~close,
            high = ~high, low = ~low,
            increasing = list(line = list(color = "red")),
            decreasing = list(line = list(color = "green"))
    )
}

#' Plotly ohlcv chart
#'
#' @param ohlcv
#'
#' @return
#' @export
#'
#' @examples
#' plotly_ohlcv(ohlcv %>% tail(100))
plotly_ohlcv <- function(ohlcv){
  stopifnot(all(
    is.data.frame(ohlcv),
    c(.ohlcv_names, .datetime_col) %in% colnames(ohlcv)
    ))

  plot.volume <- ohlcv %>%
    mutate(direction = ifelse(close > open, "up", "down")) %>%
    plot_ly(
      x = ~DATETIME,
      y = ~volume,
      type = 'bar',
      name = "Volume",
      color = ~direction,
      colors = colorRampPalette(c("green", "red"))(2))

  subplot(
    plotly_ohlc(ohlcv),
    plot.volume,
    heights = c(0.7,0.2),
    nrows = 2,
    shareX = TRUE,
    titleY = TRUE) %>%
    plotly::layout(showlegend = FALSE)
}

#' Plot theme with no axis title
#'
#' @return
#' @export
#'
#' @examples
plot_theme_no_axis_title <- function(){
  ggthemes::theme_economist() +
    theme(
      plot.caption = element_text(hjust = 0),
    aspect.ratio = 0.6,
    axis.title = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 0)
    )
}

#' Plot theme wth no legend
#'
#' @return
#' @export
#'
#' @examples
plot_theme_no_legend <- function(){
  ggthemes::theme_economist() +
    theme(
      plot.caption = element_text(hjust = 0),
      aspect.ratio = 0.6,
      panel.grid.minor.x = element_blank(),
      legend.position = "none",
      legend.title = element_text(size = 0)
    )
}
