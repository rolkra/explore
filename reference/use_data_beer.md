# Use the beer data set

This data set is an incomplete collection of popular beers in Austria,
Germany and Switzerland. Data are collected from various websites in
2023. Some of the collected data may be incorrect.

## Usage

``` r
use_data_beer()
```

## Value

Dataset as tibble

## Examples

``` r
use_data_beer()
#> # A tibble: 161 × 11
#>    name       brand country  year type  color_dark alcohol_vol_pct original_wort
#>    <chr>      <chr> <chr>   <dbl> <chr>      <dbl>           <dbl>         <dbl>
#>  1 Puntigame… Punt… Austria  2023 Rest           0             5.1          11.5
#>  2 Puntigame… Punt… Austria  2023 Alko…          0             0             5.1
#>  3 Puntigame… Punt… Austria  2023 Rest           0             5.2          12.1
#>  4 Puntigame… Punt… Austria  2023 Rest           0             6            13.8
#>  5 Puntigame… Punt… Austria  2023 Rest           0             4.9          11.5
#>  6 Goesser M… Goes… Austria  2023 Rest           0             5.2          11.9
#>  7 Goesser H… Goes… Austria  2023 Rest           0             4.4          11.1
#>  8 Goesser N… Goes… Austria  2023 Alko…          0             0.5           7  
#>  9 Goesser S… Goes… Austria  2023 Rest           0             5.7          13.2
#> 10 Goesser G… Goes… Austria  2023 Rest           0             5.3          12.2
#> # ℹ 151 more rows
#> # ℹ 3 more variables: energy_kcal_100ml <dbl>, carb_g_100ml <dbl>,
#> #   sugar_g_100ml <dbl>
```
