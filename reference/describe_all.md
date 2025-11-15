# Describe all variables of a dataset

Describe all variables of a dataset

## Usage

``` r
describe_all(data, out = "large")
```

## Arguments

- data:

  A dataset

- out:

  Output format ("small"\|"large")

## Value

Dataset (tibble)

## Examples

``` r
describe_all(iris)
#> # A tibble: 5 × 8
#>   variable     type     na na_pct unique   min  mean   max
#>   <chr>        <chr> <int>  <dbl>  <int> <dbl> <dbl> <dbl>
#> 1 Sepal.Length dbl       0      0     35   4.3  5.84   7.9
#> 2 Sepal.Width  dbl       0      0     23   2    3.06   4.4
#> 3 Petal.Length dbl       0      0     43   1    3.76   6.9
#> 4 Petal.Width  dbl       0      0     22   0.1  1.2    2.5
#> 5 Species      fct       0      0      3  NA   NA     NA  
```
