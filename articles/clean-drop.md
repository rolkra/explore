# Clean / Drop

``` r
library(dplyr)
library(explore)
```

## Rename variable

``` r
data <- use_data_titanic(count = FALSE)
glimpse(data)
#> Rows: 2,201
#> Columns: 4
#> $ Class    <chr> "3rd", "3rd", "3rd", "3rd", "3rd", "3rd", "3rd", "3rd", "3rd"…
#> $ Sex      <chr> "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male…
#> $ Age      <chr> "Child", "Child", "Child", "Child", "Child", "Child", "Child"…
#> $ Survived <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "…
```

``` r
data <- data %>% clean_var(Age, name = "age")
glimpse(data)
#> Rows: 2,201
#> Columns: 4
#> $ Class    <chr> "3rd", "3rd", "3rd", "3rd", "3rd", "3rd", "3rd", "3rd", "3rd"…
#> $ Sex      <chr> "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male…
#> $ age      <chr> "Child", "Child", "Child", "Child", "Child", "Child", "Child"…
#> $ Survived <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "…
```

## Replace NA values

``` r
data <- use_data_beer()
data %>% describe(energy_kcal_100ml)
#> variable = energy_kcal_100ml
#> type     = double
#> na       = 11 of 161 (6.8%)
#> unique   = 34
#> min|max  = 20 | 62
#> q05|q95  = 24 | 56.65
#> q25|q75  = 37 | 44
#> median   = 42
#> mean     = 39.89333
```

``` r
data <- data %>% clean_var(energy_kcal_100ml, na = 42)
data %>% describe(energy_kcal_100ml)
#> variable = energy_kcal_100ml
#> type     = double
#> na       = 0 of 161 (0%)
#> unique   = 33
#> min|max  = 20 | 62
#> q05|q95  = 24 | 55
#> q25|q75  = 38 | 44
#> median   = 42
#> mean     = 40.03727
```

## Set min max values

``` r
data <- create_data_person()
data %>% describe(age)
#> variable = age
#> type     = integer
#> na       = 0 of 1 000 (0%)
#> unique   = 80
#> min|max  = 16 | 95
#> q05|q95  = 21 | 92
#> q25|q75  = 37 | 76
#> median   = 55
#> mean     = 55.845
```

``` r
data <- data %>% clean_var(age, min_val = 20, max_val = 80)
data %>% describe(age)
#> variable = age
#> type     = integer
#> na       = 0 of 1 000 (0%)
#> unique   = 61
#> min|max  = 20 | 80
#> q05|q95  = 21 | 80
#> q25|q75  = 37 | 76
#> median   = 55
#> mean     = 54.276
```

## Rescale 0 to 1

``` r
data %>% describe(income)
#> variable = income
#> type     = double
#> na       = 0 of 1 000 (0%)
#> unique   = 228
#> min|max  = 0 | 150
#> q05|q95  = 6 | 123.025
#> q25|q75  = 35 | 88.625
#> median   = 62
#> mean     = 61.5875
```

``` r
data <- data %>% clean_var(income, rescale01 = TRUE)
data %>% describe(income)
#> variable = income
#> type     = double
#> na       = 0 of 1 000 (0%)
#> unique   = 228
#> min|max  = 0 | 1
#> q05|q95  = 0.04 | 0.820167
#> q25|q75  = 0.233333 | 0.590833
#> median   = 0.4
#> mean     = 0.410583
```

## Cleaning text

``` r
data[1, "handset"] <- " android "
data[2, "handset"] <- "ANDROID"
data %>% describe(handset)
#> variable = handset
#> type     = character
#> na       = 0 of 1 000 (0%)
#> unique   = 5
#>   android  = 1 (0.1%)
#>  ANDROID   = 1 (0.1%)
#>  Android   = 471 (47.1%)
#>  Apple     = 430 (43%)
#>  Other     = 97 (9.7%)
```

``` r
data <- data %>% clean_var(handset, simplify_text = TRUE)
data %>% describe(handset)
#> variable = handset
#> type     = character
#> na       = 0 of 1 000 (0%)
#> unique   = 3
#>  ANDROID = 473 (47.3%)
#>  APPLE   = 430 (43%)
#>  OTHER   = 97 (9.7%)
```

## Drop variables

- [`drop_var_no_variance()`](../reference/drop_var_no_variance.md) Drop
  all variables with no variance
- [`drop_var_not_numeric()`](../reference/drop_var_not_numeric.md) Drop
  all not numeric variables
- [`drop_var_low_variance()`](../reference/drop_var_low_variance.md)
  Drop all variables with low variance
- [`drop_var_by_names()`](../reference/drop_var_by_names.md) Drop
  variables by name
- [`drop_var_with_na()`](../reference/drop_var_with_na.md) Drop all
  variables with NA-values

``` r
data <- use_data_beer()
data %>% describe_tbl()
#> 161 observations with 11 variables
#> 19 observations containing missings (NA)
#> 5 variables containing missings (NA)
#> 1 variables with no variance
```

``` r
data %>%
  drop_var_no_variance() %>%
  describe_tbl()
#> 161 observations with 10 variables
#> 19 observations containing missings (NA)
#> 5 variables containing missings (NA)
#> 0 variables with no variance
```

``` r
data %>%
  drop_var_with_na() %>%
  describe_tbl()
#> 161 observations with 6 variables
#> 0 observations containing missings (NA)
#> 0 variables containing missings (NA)
#> 1 variables with no variance
```

## Drop observations

- [`drop_obs_with_na()`](../reference/drop_obs_with_na.md) Drop all
  observations with NA-values

``` r
data %>%
  drop_obs_with_na() %>%
  describe_tbl()
#> 142 observations with 11 variables
#> 0 observations containing missings (NA)
#> 0 variables containing missings (NA)
#> 1 variables with no variance
```

- [`drop_obs_if()`](../reference/drop_obs_if.md) Drop all observations
  where expression is true

``` r
data %>%
  count_pct(type)
#> # A tibble: 3 × 4
#>   type            n total   pct
#>   <chr>       <int> <int> <dbl>
#> 1 Alkoholfrei    27   161 16.8 
#> 2 Bock            8   161  4.97
#> 3 Rest          126   161 78.3
```

``` r
data %>%
  drop_obs_if(type == "Alkoholfrei") %>%
  count_pct(type)
#> # A tibble: 2 × 4
#>   type      n total   pct
#>   <chr> <int> <int> <dbl>
#> 1 Bock      8   134  5.97
#> 2 Rest    126   134 94.0
```
