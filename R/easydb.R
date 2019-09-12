#' Create a connection to a DBMS
#'
#' It's a simple wrapper around `DBI::dbConnect` that makes it easy to use specs
#' stored in a list and makes it easier to choose the relevant driver.
#'
#' @param drv a character used to match partially a DBMS name (ignoring case)
#'   and match it to a default driver, OR  *DBIDriver* object (as with `DBI::dbConnect`),
#'   OR a list of specs (in which case other parameters are ignored)
#' @param ... authentication arguments needed by the DBMS instance;
#'   supported parameters depend on driver and are enumerated in `?RSQLite::SQLite`,
#'   `?RMariaDB::MariaDB`, `RMySQL::MySQL`, `?odbc::odbc`, `ROracle::Oracle`,
#'   `?RPostgres::Postgres` or in the help file of the appropriate driver.
#'
#' @return an S4 object that inherits from DBIConnection. This object is used to
#'   communicate with the database engine.
#' @export
#'
#' @examples
#' # using only defaults
#' db_connect(path=":memory:")
#' # feeding a list of specs to the drv argument
#' db_connect(list(path=":memory:"))
#' # partial matching
#' db_connect(drv = "sqli", path=":memory:")
#' # explicit driver
#' db_connect(drv = RSQLite::SQLite(), path=":memory:")
db_connect <- function(
  drv = c("SQLite","MariaDB","MySQL", "Odbc", "Oracle", "PostgreSQL"), ...){
  if(is.list(drv)) {
    return(do.call(db_connect, drv))
  }

  if(is.character(drv)) {
    drv <- if(missing(drv)) tolower(match.arg(drv))
    else
      match.arg(tolower(drv),tolower(formals()$drv))
    drv = switch(drv,
                 sqlite = RSQLite::SQLite(),
                 mariadb = RMariaDB::MariaDB(),
                 mysql = RMySQL::MySQL(),
                 odbc = odbc::odbc(),
                 oracle = ROracle::Oracle(),
                 postgres = RPostgres::Postgres())
  }
  DBI::dbConnect(drv, ...)
}

#' Easy DB workflow using a few compact methods
#'
#' We define `$.DBIConnection()`, `$<-.DBIConnection()` and
#' `!.tbl_lazy()` to be able to :
#' * write in a data base with
#'   `con$foo <- my_data_frame` (where `con` is a connection built with
#'   `DBI::dbConnect()` or this package's db_connect), short for
#'   `DBI::dbWriteTable(con, "foo", my_data_frame)`
#' * build a lazy table with `con$foo`, short for `dplyr::tbl(con, "foo")`,
#'   so it can be used to do some server side data manipulation using *dplyr*
#' * collect a lazy table into the R session with `!my_lazy_table` (or directly
#'   `!con$foo`), equivalent to `dplyr::collect(my_lazy_table)` or
#'   `dplyr::collect(dplyr::tbl(con, "foo"))`
#' @aliases $.DBIConnection $<-.DBIConnection !.tbl_lazy
#' @name easydb
#' @examples
#' if(requireNamespace("RSQLite")){
#' db <- db_connect("sqlite", path = ":memory:")
#' db$foo <- head(cars,2)
#' db$foo
#' !db$foo
#' }
NULL

#' @export
#' @method $ DBIConnection
`$.DBIConnection` <- function(con, x){
  x <- strsplit(x, ".", fixed = TRUE)[[1]]
  if(length(x)==1) dplyr::tbl(con,x) else dplyr::tbl(con, dbplyr::in_schema(x[1], x[2]))
}

#' @export
#' @method [[ DBIConnection
`[[.DBIConnection` <- `$.DBIConnection`

#' @export
#' @method [ DBIConnection
`[.DBIConnection` <- function(con, x)
  stop("`[` is not supported on connections, try `$` or `[[`")

#' @export
#' @method $<- DBIConnection
`$<-.DBIConnection` <- function(con, x, value) {
  if(endsWith(x, ".")){
    if(is.null(value)){
      x <- substr(x,1, nchar(x)-1)
      # this is PostgreSQL syntax, not sure how it will translate
      # it doesn't work for SQLite
      DBI::dbExecute(con, paste0("DROP SCHEMA ", x ,";"))
      return(con)
    }
    # define a schema
    x <- substr(x,1, nchar(x)-1)
    DBI::dbExecute(con, paste0("ATTACH '", value, "' AS ", x))
    return(con)
  }
  x <- strsplit(x, ".", fixed = TRUE)[[1]]
  if(length(x)>1) x <- dbplyr::in_schema(x[1], x[2])

  if(is.null(value)) {
    dplyr::db_drop_table(con, x)
  } else if (inherits(value, "tbl_lazy")) {
    query <- sprintf("CREATE TABLE %s AS %s", x, dbplyr::sql_render(value))
    DBI::dbExecute(con, query)
  } else {
    if(!is.data.frame(value)){
      stop("`value` should be a data frame")
    }
    overwrite <- getOption("easydb.overwrite")
    table_exists <- dplyr::db_has_table(con, x)
    #table_exists <- dbplyr:::db_has_table.DBIConnection(con, x)
    if(overwrite && table_exists) dplyr::db_drop_table(con, x)
    dplyr::copy_to(con, value, x, temporary = FALSE)
  }
  con
}

#' @export
#' @method [[<- DBIConnection
`[[<-.DBIConnection` <- `$<-.DBIConnection`

#' @export
#' @method [<- DBIConnection
`[<-.DBIConnection` <- function(con, x, value)
  stop("`[<-` is not supported on connections, try `$<-` or `[[<-`")

#' @export
#' @method ! tbl_lazy
`!.tbl_lazy` <- dplyr::collect

#' @export
#' @method with DBIConnection
with.DBIConnection <- function(data, expr, ...){
  tables  <- dplyr::db_list_tables(con)
  con <- substitute(con)
  subst_list <- sapply(tables, function(x) bquote(.(con)[[.(x)]]))
  expr <- substitute(expr)
  expr <- do.call(substitute, list(expr, subst_list))
  eval_sub <- function(x, value) {
    mc <- match.call()
    if(is.symbol(substitute(x))){
      mc[[2]] <- call("[[",con,deparse(substitute(x)))
    }
    mc[[1]] <- quote(base::`<-`)
    eval.parent(mc)
  }
  eval(expr, enclos = parent.frame(),list(`<-` = eval_sub))
}

# so CMD check doesn't fail
db_has_table.DBIConnection <- getFromNamespace("db_has_table.DBIConnection", "dbplyr")
