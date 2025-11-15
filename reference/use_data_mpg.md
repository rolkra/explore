# Use the mpg data set

This data set comes with the ggplot2 package. It contains a subset of
the fuel economy data that the EPA makes available on
https://fueleconomy.gov/. It contains only models which had a new
release every year between 1999 and 2008 - this was used as a proxy for
the popularity of the car.

## Usage

``` r
use_data_mpg()
```

## Value

Dataset

## See also

[`ggplot2::mpg`](https://ggplot2.tidyverse.org/reference/mpg.html)

## Examples

``` r
use_data_mpg()
#> # A tibble: 234 × 11
#>    manufacturer model      displ  year   cyl trans drv     cty   hwy fl    class
#>    <chr>        <chr>      <dbl> <int> <int> <chr> <chr> <int> <int> <chr> <chr>
#>  1 audi         a4           1.8  1999     4 auto… f        18    29 p     comp…
#>  2 audi         a4           1.8  1999     4 manu… f        21    29 p     comp…
#>  3 audi         a4           2    2008     4 manu… f        20    31 p     comp…
#>  4 audi         a4           2    2008     4 auto… f        21    30 p     comp…
#>  5 audi         a4           2.8  1999     6 auto… f        16    26 p     comp…
#>  6 audi         a4           2.8  1999     6 manu… f        18    26 p     comp…
#>  7 audi         a4           3.1  2008     6 auto… f        18    27 p     comp…
#>  8 audi         a4 quattro   1.8  1999     4 manu… 4        18    26 p     comp…
#>  9 audi         a4 quattro   1.8  1999     4 auto… 4        16    25 p     comp…
#> 10 audi         a4 quattro   2    2008     4 manu… 4        20    28 p     comp…
#> # ℹ 224 more rows
```
