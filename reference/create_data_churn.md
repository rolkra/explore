# Create data churn

Artificial data that can be used for unit-testing or teaching

## Usage

``` r
create_data_churn(
  obs = 1000,
  target_name = "churn",
  factorise_target = FALSE,
  target1_prob = 0.4,
  add_id = FALSE,
  seed = 123
)
```

## Arguments

- obs:

  Number of observations

- target_name:

  Variable name of target

- factorise_target:

  Should target variable be factorised?

- target1_prob:

  Probability that target = 1

- add_id:

  Add an id-variable to data?

- seed:

  Seed for randomization (integer)

## Value

A dataset as tibble

## Examples

``` r
create_data_churn()
#> # A tibble: 1,000 × 9
#>    price type    usage shared device   newsletter language duration churn
#>    <dbl> <chr>   <dbl>  <int> <chr>         <int> <chr>       <int> <dbl>
#>  1    29 Premium  63        1 Computer          1 sp              7     0
#>  2    27 Regular  39        0 Tablet            1 sp             47     0
#>  3    29 Premium  87        1 Phone             1 sp             99     0
#>  4    11 Promo    29        0 Tablet            0 sp             33     1
#>  5    18 Promo    22.5      1 Computer          1 en             94     0
#>  6    21 Promo     8        1 Tablet            1 en             17     0
#>  7    19 Promo    56        0 Phone             0 fr             95     0
#>  8    13 Promo    94.5      1 Phone             1 en             92     0
#>  9    29 Premium  46        0 Phone             0 en             43     0
#> 10    22 Promo    76        0 Phone             1 de             16     1
#> # ℹ 990 more rows
```
