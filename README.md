
<!-- README.md is generated from README.Rmd. Please edit that file -->

# easydb

DBI and dplyr wrappers to write to DB, fetch and run data manipulation
operations on server side.

We define `$.DBIConnection()`, `$<-.DBIConnection()` and `!.tbl_lazy()`
to be able to :

  - write in a data base with `con$foo <- my_data_frame` (where `con` is
    a connection built with `DBI::dbConnect()` or this package’s
    db\_connect), short for `DBI::dbWriteTable(con, "foo",
    my_data_frame)`
  - build a lazy table with `con$foo`, short for `dplyr::tbl(con,
    "foo")`, so it can be used to do some server side data manipulation
    using *dplyr*
  - collect a lazy table into the R session with `!my_lazy_table` (or
    directly `!con$foo`), equivalent to `dplyr::collect(my_lazy_table)`
    or `dplyr::collect(dplyr::tbl(con, "foo"))`
  - We can access tables in a schema by using a `.` separator as we
    would in an SQL query. In that case `con$my_schema.my_table` is
    equivalent to `dplyr::tbl(con, dbplyr::in_schema("my_schema",
    "my_table"))`

## Installation

Install with :

    remotes::install_github("moodymudskipper/easydb")

## connect

We offer a simple wrapper around `DBI::dbConnect` that makes it easy to
use specs stored in a list and makes it easier to choose the relevant
driver.

``` r
library(easydb)
# Used just as DBI::dbConnect()
db <- db_connect(RSQLite::SQLite(), path = ":memory:")
# Using shorthand
db <- db_connect("sqlite", path = ":memory:")
# Using a list of specs
specs <- list(drv = "sqlite", path = ":memory:")
db <- db_connect(specs)
# Or just, for this specific case, just use the defaults and it will create
# an SQLite database in memory
db <- db_connect()
```

## Query

To write or fetch :

``` r
db$foo <- head(cars,2)
db$bar <- head(iris,2)
dplyr::db_list_tables(db)
#> [1] "bar"          "foo"          "sqlite_stat1" "sqlite_stat4"
db$foo
#> # Source:   table<foo> [?? x 2]
#> # Database: sqlite 3.22.0 []
#>   speed  dist
#>   <dbl> <dbl>
#> 1     4     2
#> 2     4    10

!db$foo
#> # A tibble: 2 x 2
#>   speed  dist
#>   <dbl> <dbl>
#> 1     4     2
#> 2     4    10
```

By default one can’t overwrite a table, it would need to be removed
first, which you can do by calling `db$table_to_delete <- NULL`

``` r
db$foo <- tail(cars,2)
#> Error: Table `foo` exists in database, and both overwrite and append are FALSE
db$foo <- NULL
db$foo <- tail(cars,2)
!db$foo
#> # A tibble: 2 x 2
#>   speed  dist
#>   <dbl> <dbl>
#> 1    24   120
#> 2    25    85
```

We can allow overwriting by setting the option `easydb.overwrite` to
`TRUE`.

``` r
options(easydb.overwrite = TRUE)
db$bar <- tail(iris,2)
!db$bar
#> # A tibble: 2 x 5
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species  
#>          <dbl>       <dbl>        <dbl>       <dbl> <chr>    
#> 1          6.2         3.4          5.4         2.3 virginica
#> 2          5.9         3            5.1         1.8 virginica
```
