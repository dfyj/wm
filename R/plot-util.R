
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
