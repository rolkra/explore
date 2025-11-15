# Drop all observations with NA-values

Drop all observations with NA-values

## Usage

``` r
drop_obs_with_na(data)
```

## Arguments

- data:

  Data frame

## Value

Data frame

## Examples

``` r
data <- data.frame(a = 1:10, b = rep("A",10))
data[1,1] <- NA
drop_obs_with_na(data)
#>     a b
#> 2   2 A
#> 3   3 A
#> 4   4 A
#> 5   5 A
#> 6   6 A
#> 7   7 A
#> 8   8 A
#> 9   9 A
#> 10 10 A
```
