#' str_c but omit NA
#'
#' @export
#'
#' @examples
#' str_vec <- c(letters[1:3], NA)
#' str_c_omit_na(str_vec, collapse = ".") == "a.b.c"
#' str_c(str_vec, collapse = ".") == NA
str_c_omit_na <- function(..., sep = "", collapse = NULL){
  args <- unlist(...) %>% na.omit()
  stringr::str_c(args, sep = sep, collapse = collapse)
}

#' Read in a data.frame from a file with multiple header rows
#'
#' @param file data file path
#' @param n_header_row number of header rows
#' @param header_collapse string used to join multiple header rows
#'
#' @return
#' @export
#'
#' @examples
import_multiple_header_row <- function(file,
                                       n_header_row = 2,
                                       header_collapse = ".",
                                       ...)
{
  headers <- file %>% rio::import(col_names = FALSE, n_max = n_header_row, ...)
  col_names <- apply(headers, 2, pryr::partial(str_c_omit_na, collapse = header_collapse))
  file %>% rio::import(
    col_names = col_names,
    col_types = "text",
    skip = n_header_row,
    ...
  )
}

set_rownames <- function(df, rownames){
  rownames(df) <- rownames
  df
}
