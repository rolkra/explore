# Describe

The explore package offers a simplified way to describe data.

- [`describe_tbl()`](../reference/describe_tbl.md) describes the table
  (number of rows, columns, …)
- [`describe_all()`](../reference/describe_all.md) returns a description
  of all variables as data frame
- [`describe()`](../reference/describe.md) returns a description of of a
  specific variable (or a description of all variables if no specific
  variable is passed to the function)
- [`describe_cat()`](../reference/describe_cat.md) returns a description
  of of a specific variable (forcing to handle the variable as
  categorical)
- [`describe_num()`](../reference/describe_num.md) returns a description
  of of a specific variable (forcing to handle the variable as
  numerical)

We use synthetic data in this example

``` r
library(dplyr)
library(explore)

data <- create_data_buy(obs = 100)
glimpse(data)
#> Rows: 100
#> Columns: 13
#> $ period          <int> 202012, 202012, 202012, 202012, 202012, 202012, 202012…
#> $ buy             <int> 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, …
#> $ age             <int> 48, 68, 45, 50, 59, 60, 66, 56, 70, 47, 71, 40, 47, 92…
#> $ city_ind        <int> 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, …
#> $ female_ind      <int> 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, …
#> $ fixedvoice_ind  <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, …
#> $ fixeddata_ind   <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ fixedtv_ind     <int> 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, …
#> $ mobilevoice_ind <int> 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, …
#> $ mobiledata_prd  <chr> "NO", "NO", "NO", "NO", "NO", "MOBILE STICK", "MOBILE …
#> $ bbi_speed_ind   <int> 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, …
#> $ bbi_usg_gb      <int> 79, 60, 82, 52, 54, 64, 52, 73, 36, 90, 78, 103, 52, 2…
#> $ hh_single       <int> 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
```

### Describe table

``` r
data %>% describe_tbl()
#> 100 observations with 13 variables
#> 0 observations containing missings (NA)
#> 0 variables containing missings (NA)
#> 2 variables with no variance
```

### Describe all variables

``` r
data %>% describe_all()
#> # A tibble: 13 × 8
#>    variable        type     na na_pct unique    min      mean    max
#>    <chr>           <chr> <int>  <dbl>  <int>  <dbl>     <dbl>  <dbl>
#>  1 period          int       0      0      1 202012 202012    202012
#>  2 buy             int       0      0      2      0      0.53      1
#>  3 age             int       0      0     41     24     52.6      92
#>  4 city_ind        int       0      0      2      0      0.49      1
#>  5 female_ind      int       0      0      2      0      0.53      1
#>  6 fixedvoice_ind  int       0      0      2      0      0.08      1
#>  7 fixeddata_ind   int       0      0      1      1      1         1
#>  8 fixedtv_ind     int       0      0      2      0      0.43      1
#>  9 mobilevoice_ind int       0      0      2      0      0.68      1
#> 10 mobiledata_prd  chr       0      0      3     NA     NA        NA
#> 11 bbi_speed_ind   int       0      0      2      0      0.6       1
#> 12 bbi_usg_gb      int       0      0     56     10   1064.   100000
#> 13 hh_single       int       0      0      2      0      0.29      1
```

``` r
data %>% 
  describe_all() %>%
  filter(unique == 1)
#> # A tibble: 2 × 8
#>   variable      type     na na_pct unique    min   mean    max
#>   <chr>         <chr> <int>  <dbl>  <int>  <dbl>  <dbl>  <dbl>
#> 1 period        int       0      0      1 202012 202012 202012
#> 2 fixeddata_ind int       0      0      1      1      1      1
```

### Describe one variable

``` r
data %>% describe(age)
#> variable = age
#> type     = integer
#> na       = 0 of 100 (0%)
#> unique   = 41
#> min|max  = 24 | 92
#> q05|q95  = 33.85 | 71
#> q25|q75  = 45 | 60
#> median   = 52.5
#> mean     = 52.55
```

``` r
data %>% describe(buy)
#> variable = buy
#> type     = integer
#> na       = 0 of 100 (0%)
#> unique   = 2
#>        0 = 47 (47%)
#>        1 = 53 (53%)
```
