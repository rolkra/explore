# Drop all variables with no variance

Drop all variables with no variance

## Usage

``` r
drop_var_no_variance(data)
```

## Arguments

- data:

  Data frame

## Value

Data frame

## Examples

``` r
data <- data.frame(a = 1:10, b = rep(1,10))
drop_var_no_variance(data)
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
