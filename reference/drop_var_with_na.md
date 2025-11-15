# Drop all variables with NA-values

Drop all variables with NA-values

## Usage

``` r
drop_var_with_na(data)
```

## Arguments

- data:

  Data frame

## Value

Data frame

## Examples

``` r
data <- data.frame(a = 1:10, b = rep(NA,10))
drop_var_with_na(data)
#>     a
#> 1   1
#> 2   2
#> 3   3
#> 4   4
#> 5   5
#> 6   6
#> 7   7
#> 8   8
#> 9   9
#> 10 10
```
