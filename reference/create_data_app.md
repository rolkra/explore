# Create data app

Artificial data that can be used for unit-testing or teaching

## Usage

``` r
create_data_app(obs = 1000, add_id = FALSE, seed = 123)
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
create_data_app()
#> # A tibble: 1,000 × 7
#>    os       free downloads rating type     updates screen_sizes
#>    <chr>   <int>     <int>  <dbl> <chr>      <dbl>        <dbl>
#>  1 Android     1      5802      4 Kids          63            3
#>  2 iOS         1      5048      4 Media         58            2
#>  3 Android     1      4579      3 Other         62            3
#>  4 iOS         0      3449      4 Shopping      44            2
#>  5 Other       0      2464      1 Connect       24            1
#>  6 Android     1     11276      4 Learn         75            3
#>  7 iOS         0      4026      5 Shopping      34            1
#>  8 iOS         1      6841      5 Work          91            2
#>  9 iOS         1     10419      4 Learn         29            2
#> 10 Android     1      5421      1 Media         29            3
#> # ℹ 990 more rows
```
