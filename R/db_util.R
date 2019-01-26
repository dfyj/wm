#' connect to database
#'
#' @param con_opt dsn
#' @param config_file
#'
#' @return
#' @export
#'
#' @examples
#' dbcon("wdzx")
#' dbcon("research")
dbcon <-  function(con_opt = c("wdzx", "research"),
                   db_name = NULL,
                   config_file = "./conf/db-config.yml") {

  match.arg(con_opt)

  con <- getOption(con_opt)

  if (is.null(con) || !DBI::dbIsValid(con)) {

    config <- config::get(con_opt, file = config_file)

    con <- odbc::dbConnect(
      odbc::odbc(),
      dsn = config$dsn,
      UID = config$UID,
      PWD = config$PWD,
      database = db_name,
      encoding = "GBK"
    )

    message("set on connection to database")

    list <- list(con) %>% setNames(con_opt)
    options(list)

  }
  getOption(con_opt)
}



#' read table from database
#'
#' @param con connection to database
#' @param db_name
#' @param tb_name
#' @param options
#'
#' @return
#' @export
#'
#' @examples
#' con <- dbcon("research")
#' db_name <- "db_shop"
#' tb_name <- "tb_goods"
#' options <- NULL
#'table <- dbread(con, db_name, tb_name, options)
dbread <- function(con, db_name, tb_name,options){
  tbl(con, dbplyr::in_schema(db_name, tb_name))
}


list_tables <- function(con){
  DBI::dbListTables(con)
}

#' create a schema
#'
#' @param con
#' @param schema_name
#'
#' @return
#' @export
#'
#' @examples
create_schema <- function(con,schema_name){
  glue::glue("CREATE DATABASE {schema_name};") %>%
    DBI::dbSendQuery(con,.)
}


