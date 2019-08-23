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
`$.DBIConnection` <- dplyr::tbl

#' @export
#' @method $<- DBIConnection
`$<-.DBIConnection` <- function(con, x, value) {DBI::dbWriteTable(con,x,value); con}

#' @export
#' @method ! tbl_lazy
`!.tbl_lazy` <- dplyr::collect

