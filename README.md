
<!-- README.md is generated from README.Rmd. Please edit that file -->

# easydb

DBI and dplyr wrappers to write to DB, fetch and run data manipulations
on server side.

We define `$.DBIConnection()`, `$<-.DBIConnection()` and `!.tbl_lazy()`
to be able to : \* write in a data base with `con$foo <- my_data_frame`
(where `con` is a connection built with `DBI::dbConnect()` or this
package’s db\_connect), short for `DBI::dbWriteTable(con, "foo",
my_data_frame)` \* build a lazy table with `con$foo`, short for
`dplyr::tbl(con, "foo")`, so it can be used to do some server side data
manipulation using *dplyr* \* collect a lazy table into the R session
with `!my_lazy_table` (or directly `!con$foo`), equivalent to
`dplyr::collect(my_lazy_table)` or `dplyr::collect(dplyr::tbl(con,
"foo"))`

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
```

## Query

To write or fetch :

``` r

db$foo <- head(cars,2)

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
