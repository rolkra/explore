# Adds percentage to dplyr::count()

Adds variables total and pct (percentage) to dplyr::count()

## Usage

``` r
count_pct(data, ...)
```

## Arguments

- data:

  A dataset

- ...:

  Other parameters passed to count()

## Value

Dataset

## Examples

``` r
count_pct(iris, Species)
#>      Species  n total      pct
#> 1     setosa 50   150 33.33333
#> 2 versicolor 50   150 33.33333
#> 3  virginica 50   150 33.33333
```
