# Create data esoteric

Random data that can be used for unit-testing or teaching

## Usage

``` r
create_data_esoteric(obs = 1000, add_id = FALSE, seed = 123)
```

## Arguments

- obs:

  Number of observations

- add_id:

  Add an id-variable to data?

- seed:

  Seed for randomization

## Value

A dataset as tibble

## Details

Variables in dataset:

- id = Identifier

- starsign = random starsign

- chinese = random chinese zodiac

- moon = random moon phase

- blood = random blood type

- fingers_crossed = random fingers crossed (1 = yes, 0 = no)

- success = random success (1 = yes, 0 = no)

## Examples

``` r
create_data_esoteric(obs = 100)
#> # A tibble: 100 × 6
#>    starsign chinese moon       blood fingers_crossed success
#>    <chr>    <chr>   <chr>      <chr>           <int>   <int>
#>  1 Leo      Monkey  Waxing (+) B+                  1       1
#>  2 Aquarius Dragon  New ( )    A+                  0       1
#>  3 Virgo    Horse   Waning (-) B+                  1       1
#>  4 Pisces   Rat     Waning (-) B+                  0       1
#>  5 Aries    Horse   Waning (-) 0+                  0       1
#>  6 Taurus   Pig     Full (O)   0+                  0       1
#>  7 Scorpio  Pig     Waxing (+) A+                  0       0
#>  8 Pisces   Monkey  Waxing (+) A+                  0       0
#>  9 Scorpio  Snake   Waxing (+) 0+                  0       0
#> 10 Libra    Tiger   Waxing (+) 0+                  0       1
#> # ℹ 90 more rows
```
