# data frame related utils ------------------------------------------------

#' group + top_n + sum
#'
#' @param df data frame
#' @param gp group_by col
#' @param n top n
#' @param value value vol to sum
#'
#' @return
#' @export
#'
#' @examples
#' df_group_top_n_sum(df, date, 10, weight){
df_group_top_n_sum <- function(df, gp, n, value){
  gp <- rlang::enquo(gp)
  value <- rlang::enquo(value)
  output_col <- stringr::str_c("top", n, rlang::quo_name(value), sep = "_")
  # print(output_col)

  df %>%
    dplyr::group_by(!!gp) %>%
    dplyr::top_n(., n, !!value) %>%
    dplyr::summarise(!! output_col := sum(!!value))
}

#' group + sum
#'
#' @param df data frame
#' @param gp group_by col
#' @param value value vol to sum
#'
#' @return
#' @export
#'
#' @examples
#' df_group_top_n_sum(df, date, 10, weight){
df_group_sum <- function(df, gp, value){
  gp <- rlang::enquo(gp)
  value <- rlang::enquo(value)
  output_col <- rlang::quo_name(value)
  # print(output_col)

  df %>%
    dplyr::group_by(!!gp) %>%
    dplyr::summarise(!! output_col := sum(!!value))
}
