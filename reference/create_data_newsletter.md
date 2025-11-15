# Create data newsletter

Artificial data that can be used for unit-testing or teaching (fairness
& AI bias)

## Usage

``` r
create_data_newsletter(obs = 1000, add_id = FALSE, seed = 123)
```

## Arguments

- obs:

  Number of observations

- add_id:

  Add an id-variable to data?

- seed:

  Seed for randomization (integer)

## Value

A dataset as tibble

## Examples

``` r
create_data_newsletter()
#> # A tibble: 1,000 × 6
#>    sending_h message   age  send click   buy
#>        <int> <chr>   <int> <int> <dbl> <dbl>
#>  1        14 voucher    80     1     0     0
#>  2        14 news       25     1     0     0
#>  3        13 news       17     1     0     0
#>  4        13 news       78     1     0     0
#>  5        14 voucher    30     1     1     1
#>  6        16 news       76     1     0     0
#>  7        15 news       30     1     0     0
#>  8        12 news       64     1     1     0
#>  9        13 voucher    50     1     1     0
#> 10        14 news       72     1     0     0
#> # ℹ 990 more rows
```
