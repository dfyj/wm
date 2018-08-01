
<<<<<<< HEAD
#' Plotly ohlc chart
#'
#' @param ohlc
#'
#' @return
#' @export
#'
#' @examples
#' plotly_ohlc(ohlcv)
plotly_ohlc <- function(ohlc){
  stopifnot(all(
    is.data.frame(ohlc),
    c(.ohlc_names, .datetime_col) %in% colnames(ohlc)
    ))

  ohlc %>%
    plot_ly(x = ~DATETIME, type="candlestick",
            open = ~open, close = ~close,
            high = ~high, low = ~low
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
    layout(leg)
}

=======
>>>>>>> 013440dc71df9a673fd7fa8009d1494ffd752f61
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
