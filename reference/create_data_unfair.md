# Create data unfair

Artificial data that can be used for unit-testing or teaching (fairness
& AI bias)

## Usage

``` r
create_data_unfair(
  obs = 1000,
  target_name = "target_ind",
  factorise_target = FALSE,
  target1_prob = 0.25,
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
create_data_unfair()
#> # A tibble: 1,000 × 22
#>      age gender eye_color shoe_size    iq education income handset pet   smoking
#>    <int> <chr>  <chr>         <dbl> <dbl>     <int>  <dbl> <chr>   <chr>   <int>
#>  1    46 Female Blue           45.2   141        66   132  Apple   Other       0
#>  2    94 Female Green          37      71        41    95  Apple   Cat         0
#>  3    66 Male   Blue           45      80        49    18  Apple   Cat         0
#>  4    29 Male   Blue           45      74        49    54  Android Dog         0
#>  5    82 Female Blue           39     119        25    70  Apple   Cat         1
#>  6    57 Female Brown          38.2    95        68   128  Android No          0
#>  7    65 Female Brown          41.2    97        87   128. Other   Dog         1
#>  8    58 Male   Blue           46     135        46    32  Android No          0
#>  9    29 Male   Blue           40      88        78    82  Apple   No          0
#> 10    40 Male   Green          42     140        14    94  Apple   Cat         0
#> # ℹ 990 more rows
#> # ℹ 12 more variables: name_arabic <int>, outfit <chr>, glasses <int>,
#> #   tatoos <int>, kids <int>, bad_debt <dbl>, credit_card <chr>,
#> #   left_handed <int>, skin_color <chr>, religion <chr>, internet_gb <dbl>,
#> #   target_ind <int>
```
