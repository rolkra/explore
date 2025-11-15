# Describe a dataset or variable

Describe a dataset or variable (depending on input parameters)

## Usage

``` r
describe(data, var, n, target, out = "text", ...)
```

## Arguments

- data:

  A dataset

- var:

  A variable of the dataset

- n:

  Weights variable for count-data

- target:

  Target variable (0/1 or FALSE/TRUE)

- out:

  Output format ("text"\|"list") of variable description

- ...:

  Further arguments

## Value

Description as table, text or list

## Examples

``` r
# Load package
library(magrittr)

# Describe a dataset
iris %>% describe()
#> # A tibble: 5 × 8
#>   variable     type     na na_pct unique   min  mean   max
#>   <chr>        <chr> <int>  <dbl>  <int> <dbl> <dbl> <dbl>
#> 1 Sepal.Length dbl       0      0     35   4.3  5.84   7.9
#> 2 Sepal.Width  dbl       0      0     23   2    3.06   4.4
#> 3 Petal.Length dbl       0      0     43   1    3.76   6.9
#> 4 Petal.Width  dbl       0      0     22   0.1  1.2    2.5
#> 5 Species      fct       0      0      3  NA   NA     NA  

# Describe a variable
iris %>% describe(Species)
#> variable = Species
#> type     = factor
#> na       = 0 of 150 (0%)
#> unique   = 3
#>  setosa     = 50 (33.3%)
#>  versicolor = 50 (33.3%)
#>  virginica  = 50 (33.3%)
iris %>% describe(Sepal.Length)
#> variable = Sepal.Length
#> type     = double
#> na       = 0 of 150 (0%)
#> unique   = 35
#> min|max  = 4.3 | 7.9
#> q05|q95  = 4.6 | 7.255
#> q25|q75  = 5.1 | 6.4
#> median   = 5.8
#> mean     = 5.843333
```
