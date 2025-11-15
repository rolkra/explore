# Create data person

Artificial data that can be used for unit-testing or teaching

## Usage

``` r
create_data_person(obs = 1000, add_id = FALSE, seed = 123)
```

## Arguments

- obs:

  Number of observations

- add_id:

  Add an id

- seed:

  Seed for randomization (integer)

## Value

A dataset as tibble

## Examples

``` r
create_data_person()
#> # A tibble: 1,000 × 15
#>      age gender eye_color shoe_size    iq education income handset pet  
#>    <int> <chr>  <chr>         <dbl> <dbl>     <int>  <dbl> <chr>   <chr>
#>  1    46 Female Blue           45.2   141        66   132  Apple   No   
#>  2    94 Female Green          37      71        41    95  Apple   Cat  
#>  3    66 Male   Brown          45      80        49    18  Apple   Other
#>  4    29 Male   Green          45      74        49    54  Android No   
#>  5    82 Female Brown          39     119        25    70  Apple   Dog  
#>  6    57 Female Brown          38.2    95        68   128  Android No   
#>  7    65 Female Green          41.2    97        87   128. Other   Cat  
#>  8    58 Male   Blue           46     135        46    32  Android Dog  
#>  9    29 Male   Blue           40      88        78    82  Apple   No   
#> 10    40 Male   Green          42     140        14    94  Apple   Other
#> # ℹ 990 more rows
#> # ℹ 6 more variables: favorite_pizza <chr>, favorite_icecream <chr>,
#> #   likes_garlic <int>, likes_sushi <int>, likes_beatles <int>,
#> #   likes_beer <int>
```
