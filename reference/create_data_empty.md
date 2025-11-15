# Create an empty dataset

Create an empty dataset

## Usage

``` r
create_data_empty(obs = 1000, add_id = FALSE)
```

## Arguments

- obs:

  Number of observations

- add_id:

  Add an id

## Value

Dataset as tibble

## Examples

``` r
create_data_empty(obs = 100)
#> # A tibble: 100 × 0
create_data_empty(obs = 100, add_id = TRUE)
#> # A tibble: 100 × 1
#>       id
#>    <int>
#>  1     1
#>  2     2
#>  3     3
#>  4     4
#>  5     5
#>  6     6
#>  7     7
#>  8     8
#>  9     9
#> 10    10
#> # ℹ 90 more rows
```
