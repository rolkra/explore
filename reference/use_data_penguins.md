# Use the penguins data set

This data set comes with the palmerpenguins package. It contains
measurements for penguin species, island in Palmer Archipelago, size
(flipper length, body mass, bill dimensions), and sex.

## Usage

``` r
use_data_penguins(short_names = FALSE)
```

## Arguments

- short_names:

  Use short variable names

## Value

Dataset

## See also

[`palmerpenguins::penguins`](https://allisonhorst.github.io/palmerpenguins/reference/penguins.html)

## Examples

``` r
use_data_penguins()
#> # A tibble: 344 × 8
#>    species island    bill_length_mm bill_depth_mm flipper_length_mm body_mass_g
#>    <fct>   <fct>              <dbl>         <dbl>             <int>       <int>
#>  1 Adelie  Torgersen           39.1          18.7               181        3750
#>  2 Adelie  Torgersen           39.5          17.4               186        3800
#>  3 Adelie  Torgersen           40.3          18                 195        3250
#>  4 Adelie  Torgersen           NA            NA                  NA          NA
#>  5 Adelie  Torgersen           36.7          19.3               193        3450
#>  6 Adelie  Torgersen           39.3          20.6               190        3650
#>  7 Adelie  Torgersen           38.9          17.8               181        3625
#>  8 Adelie  Torgersen           39.2          19.6               195        4675
#>  9 Adelie  Torgersen           34.1          18.1               193        3475
#> 10 Adelie  Torgersen           42            20.2               190        4250
#> # ℹ 334 more rows
#> # ℹ 2 more variables: sex <fct>, year <int>
use_data_penguins(short_names = TRUE)
#> # A tibble: 344 × 8
#>    species island    bill_len bill_dep flipper_len body_mass sex     year
#>    <fct>   <fct>        <dbl>    <dbl>       <int>     <int> <fct>  <int>
#>  1 Adelie  Torgersen     39.1     18.7         181      3750 male    2007
#>  2 Adelie  Torgersen     39.5     17.4         186      3800 female  2007
#>  3 Adelie  Torgersen     40.3     18           195      3250 female  2007
#>  4 Adelie  Torgersen     NA       NA            NA        NA NA      2007
#>  5 Adelie  Torgersen     36.7     19.3         193      3450 female  2007
#>  6 Adelie  Torgersen     39.3     20.6         190      3650 male    2007
#>  7 Adelie  Torgersen     38.9     17.8         181      3625 female  2007
#>  8 Adelie  Torgersen     39.2     19.6         195      4675 male    2007
#>  9 Adelie  Torgersen     34.1     18.1         193      3475 NA      2007
#> 10 Adelie  Torgersen     42       20.2         190      4250 NA      2007
#> # ℹ 334 more rows
```
